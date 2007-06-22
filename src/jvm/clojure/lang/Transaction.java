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

package clojure.lang;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.ConcurrentSkipListSet;

public class Transaction{

final static ThreadLocal<Transaction> transaction = new ThreadLocal<Transaction>();

//total order on transactions
//transactions will consume a point on init, and another on commit if writing
final static AtomicInteger nextPoint = new AtomicInteger(1);

final static ConcurrentSkipListSet<Integer> completedPoints = new ConcurrentSkipListSet<Integer>();

static int completedThroughPoint(){
	Iterator<Integer> i = completedPoints.iterator();
	if(!i.hasNext())
		return 0;
	int base, tp;
	base = tp = i.next();
	while(i.hasNext() && i.next() == tp + 1)
		++tp;
	if(tp != base)
		completedPoints.removeAll(completedPoints.headSet(tp));
	return tp;
}

static Transaction getTransaction(){
	return transaction.get();
}

static void setTransaction(Transaction t){
	transaction.set(t);
}

static public Object runInTransaction(IFn fn) throws Exception{
	if(getTransaction() != null)
		return fn.invoke();

	Transaction t = new Transaction();
	setTransaction(t);
	try
		{
		return t.run(fn);
		}
	finally
		{
		if(!t.asOf)
			{
			completedPoints.add(t.readPoint);
			if(t.writePoint > 0)
				completedPoints.add(t.writePoint);
			}
		setTransaction(null);
		}
}

static public Object runInAsOfTransaction(IFn fn, int tpoint) throws Exception{
	if(getTransaction() != null)
		throw new Exception("As-of transactions cannot be nested");

	Transaction t = new Transaction(tpoint);
	setTransaction(t);
	try
		{
		return t.run(fn);
		}
	finally
		{
		setTransaction(null);
		}
}

static public Object runInAsOfTransaction(IFn fn, long msecs) throws Exception{
	if(getTransaction() != null)
		throw new Exception("As-of transactions cannot be nested");

	Transaction t = new Transaction(msecs);
	setTransaction(t);
	try
		{
		return t.run(fn);
		}
	finally
		{
		setTransaction(null);
		}
}
/*
static public Object get(TRef tref) throws Exception{
    Transaction trans = getTransaction();
    if(trans != null)
        return trans.doGet(tref);
    return getCurrent(tref).val;
}

static public Object set(TRef tref, Object val) throws Exception{
     return getTransaction().doSet(tref,val);
}

static public void touch(TRef tref) throws Exception{
    getTransaction().doTouch(tref);
}

static public void commute(TRef tref, IFn fn) throws Exception{
    getTransaction().doCommutate(tref, fn);
}
//*/

boolean asOf;
int readPoint;
long readTimeMsecs = 0;
int writePoint = 0;

IdentityHashMap<TRef, Object> sets;
IdentityHashMap<TRef, LinkedList<IFn>> commutes;

Transaction(boolean asOf, int readPoint, long readTimeMsecs){
	this.asOf = asOf;
	this.readPoint = readPoint;
	this.readTimeMsecs = readTimeMsecs;
}

Transaction(){
	this(false, nextPoint.getAndIncrement(), 0);
}

Transaction(int readPoint){
	this(true, readPoint, 0);
}

Transaction(long readTimeMsecs){
	this(true, 0, readTimeMsecs);
}

boolean casLock(TRef tref) throws Exception{
	//todo - create user-controllable policy
	for(int i = 0; i < 100; ++i)
		{
		if(tref.lockedBy.compareAndSet(0, readPoint))
			return true;
		Thread.sleep(10);
		}
	return false;
}

Object run(IFn fn) throws Exception{
	boolean done = false;
	Object ret = null;
	ArrayList<TRef> locks = null;
	ArrayList<TRef> locked = null;

	loop:
	//todo - create user-controllable policy
	for(int i=0;!done && i<100;i++)
		{
		try
			{
			ret = fn.invoke();
			//read-only trans, return right away
			if((sets == null || sets.isEmpty()) && (commutes == null || commutes.isEmpty()))
				{
				done = true;
				return ret;
				}

			if(locks == null && (sets != null || commutes != null))
				locks = new ArrayList<TRef>();
			if(sets != null)
				locks.addAll(sets.keySet());
			if(locks != null)
				{
				if(locked == null)
					locked = new ArrayList<TRef>(locks.size());
				//lock in order
				Collections.sort(locks);
				for(TRef tref : locks)
					{
					if(!casLock(tref))
						continue loop;
					locked.add(tref);
					if(!commutes.containsKey(tref))
						{
						//try again if the thing we are trying to set has changed since we started
						TVal curr = getCurrent(tref);
						if(curr != null && curr.tstamp.tpoint > readPoint)
							continue loop;
						}
					}
				}

			//at this point all write targets are locked

			//turn commutes into sets
			for(Map.Entry<TRef, LinkedList<IFn>> e : commutes.entrySet())
				{
				TRef tref = e.getKey();
				//note this will npe if tref has never been set, as designed
				Object val = getCurrent(tref).val;
				for(IFn f : e.getValue())
					{
					val = f.invoke(val);
					}
				sets.put(tref, val);
				}

			//we presume we won't throw an exception after this
			writePoint = nextPoint.getAndIncrement();
			TStamp ts = new TStamp(writePoint, System.currentTimeMillis());

			//set the new vals, unlock as we go
			for(Map.Entry<TRef, Object> entry : sets.entrySet())
				{
				TRef tref = entry.getKey();
				tref.push(new TVal(entry.getValue(), ts));
				tref.lockedBy.set(0);
				}
			done = true;
			}
		finally
			{
			if(!done)
				{
				if(locked != null)
					{
					for(TRef tref : locked)
						{
						tref.lockedBy.set(0);
						}
					locked.clear();
					}
				if(sets != null)
					sets.clear();
				if(commutes != null)
					commutes.clear();
				if(locks != null)
					locks.clear();
				}
			}
		}
	if(!done)
		throw new Exception("Transaction failed after reaching retry limit");
	return ret;
}


Object doGet(TRef tref) throws Exception{
	if(sets != null && sets.containsKey(tref))
		return sets.get(tref);

	for(TVal ver = tref.tval; ver != null; ver = ver.prior)
		{
		//note this will npe if tref has never been set, as designed
		if(readPoint > 0 && ver.tstamp.tpoint <= readPoint
		   ||
		   readTimeMsecs > 0 && ver.tstamp.msecs <= readTimeMsecs)
			return ver.val;
		}
	throw new Exception("Version not found");
}

static TVal getCurrent(TRef tref) throws Exception{
	return tref.tval;
}

Object doSet(TRef tref, Object val) throws Exception{
	if(asOf)
		throw new Exception("Can't set during as-of transaction");
	if(sets == null)
		sets = new IdentityHashMap<TRef, Object>();
	if(commutes != null && commutes.containsKey(tref))
		throw new Exception("Can't commute and set a TRef in the same transaction");

	sets.put(tref, val);
	return val;
}

void doTouch(TRef tref) throws Exception{
	doSet(tref, doGet(tref));
}

void doCommute(TRef tref, IFn fn) throws Exception{
	if(asOf)
		throw new Exception("Can't commute during as-of transaction");
	if(commutes == null)
		commutes = new IdentityHashMap<TRef, LinkedList<IFn>>();
	LinkedList<IFn> cs = commutes.get(tref);
	if(cs == null)
		{
		if(sets != null && sets.containsKey(tref))
			throw new Exception("Can't commute and set a TRef in the same transaction");
		cs = new LinkedList<IFn>();
		commutes.put(tref, cs);
		}
	cs.addLast(fn);
	sets.put(tref, fn.invoke(doGet(tref)));
}

}
