/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich May 30, 2006 */

using System;
using System.Threading;
using System.Collections.Generic;

namespace org.clojure.runtime
{
public class Transaction{

public const int COMMITTED = 0;
public const int WORKING = 1;
static readonly Object lockObj = new Object();

volatile static int nextSeq = 1;

static int getNextSeq(){
lock (lockObj)
{
		return nextSeq++;
	}
}

public class Info{
internal int seq;
internal int status;


internal Info(int seq,int status){
	this.seq = seq;
	this.status = status;
}
}


Info info;
int startSeq;

Dictionary<TRef,Object> sets;
Dictionary<TRef,ISeq> commutates;


static public Object runInTransaction(ThreadLocalData tld,IFn fn) {
	if(tld.transaction != null)
		return fn.invoke(tld);
	tld.transaction = new Transaction();
	try{
		return tld.transaction.run(tld, fn);
		}
	finally{
		tld.transaction = null;
		}
}

static public TRef tref(Object val) {
	Transaction trans = ThreadLocalData.get().getTransaction();
	TRef tref = new TRef();
	trans.set(tref, val);
	return tref;
}

static public Object get2(TRef tref) {
	 return ThreadLocalData.get().getTransaction().get(tref);
}

static public Object set2(TRef tref, Object val) {
	 return ThreadLocalData.get().getTransaction().set(tref,val);
}

static public void touch2(TRef tref) {
	ThreadLocalData.get().getTransaction().touch(tref);
}

static public void commutate2(TRef tref, IFn fn) {
	ThreadLocalData.get().getTransaction().commutate(tref, fn);
}


Object run(ThreadLocalData tld, IFn fn) {
	bool done = false;
	Object ret = null;
	List<TRef> locks = null;
	List<TRef> locked = null;

	while(!done){
		try
			{
			ret = fn.invoke(tld);
			if(locks == null && (sets != null || commutates != null))
				locks = new List<TRef>();
			if(sets != null)
				locks.AddRange(sets.Keys);
			if(commutates != null)
				locks.AddRange(commutates.Keys);
			if(locks != null)
				{
				if(locked == null)
					locked = new List<TRef>(locks.Count);
				//lock in order, to avoid deadlocks
				locks.Sort();
				foreach(TRef tref in locks)
					{
					//will block here
					Monitor.Enter(tref);
					locked.Add(tref);
					if(sets.ContainsKey(tref))
						{
						//try again if the thing we are trying to set has changed since we started
						TVal curr = getCurrent(tref);
						if(curr != null && curr.tinfo.seq > startSeq)
							goto loop;
						}
					}
				}

			//at this point all write targets are locked
			//turn commutates into sets
			foreach(KeyValuePair<TRef, ISeq> e in commutates)
				{
				TRef tref = e.Key;
				//note this will npe if tref has never been set, as designed
				Object val = getCurrent(tref).val;
				for(ISeq c = e.Value;c!=null;c = c.rest())
					{
					IFn f = (IFn) c.first();
					val = f.invoke(tld, val);
					}
				sets[tref] =  val;
				}

			//set the new vals
			foreach(KeyValuePair<TRef, Object> entry in sets)
				{
				TRef tref = entry.Key;
				tref.push(entry.Value, info);
				}

			//atomic commit
			lock(lockObj){
				info.seq = getNextSeq();
				info.status = COMMITTED;
			}

			done = true;
			loop:
			    ;
			}
		finally{
			if(locked != null)
				{
				foreach(TRef tref in locked)
					{
					Monitor.Exit(tref);
					}
				locked.Clear();
				}
			reset();
			if(locks != null)
				locks.Clear();
			}
		}
	return ret;
}

private void reset(){
	if(sets != null)
		sets.Clear();
	if(commutates != null)
		commutates.Clear();

}


Transaction(){
	lock(lockObj){
		int seq = getNextSeq();
		this.info = new Info(seq, WORKING);
		this.startSeq = seq;
	}
}

Object get(TRef tref) {
	if(sets != null && sets.ContainsKey(tref))
		return sets[tref];

    for(TVal ver = tref;ver != null;ver = ver.prior)
	    {
	    //note this will npe if tref has never been set, as designed
	    if(ver.tinfo.status == COMMITTED && ver.tinfo.seq <= startSeq)
		    return ver.val;
	    }

	throw new Exception("Version not found");
}

static TVal getCurrent(TRef tref) {
    for(TVal ver = tref;ver != null;ver = ver.prior)
	    {
	    if(ver.tinfo != null && ver.tinfo.status == COMMITTED)
		    return ver;
	    }
	//this return only if no value was ever successfully set
	return null;
}

Object set(TRef tref, Object val) {
	if(sets == null)
		sets = new Dictionary<TRef,Object>();
	if(commutates != null && commutates.ContainsKey(tref))
		throw new Exception("Can't commutate and set a TRef in the same transaction");

	sets[tref] =val;
	return val;
	}

void touch(TRef tref) {
	set(tref, get(tref));
	}

void commutate(TRef tref, IFn fn) {
	if(commutates == null)
		commutates = new Dictionary<TRef,ISeq>();
	if(sets != null && sets.ContainsKey(tref))
		throw new Exception("Can't commutate and set a TRef in the same transaction");
	commutates[tref] = RT.cons(fn, commutates[tref]);
	}

}
}
