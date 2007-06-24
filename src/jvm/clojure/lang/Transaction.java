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

import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;

public class Transaction{

public static int RETRY_LIMIT = 100;
public static int LOCK_WAIT_MSECS = 100;

final static ThreadLocal<Transaction> transaction = new ThreadLocal<Transaction>();

static class RetryException extends Exception{
}

static class AbortException extends Exception{
}

//total order on transactions
//transactions will consume a point for init, for each retry, and on commit if writing
final static AtomicLong nextPoint = new AtomicLong(1);

static long getNextPoint(){
	return nextPoint.getAndIncrement();
}

static class PointNode{
	final long tpoint;
	volatile PointNode next;

	public PointNode(long tpoint, PointNode next){
		this.tpoint = tpoint;
		this.next = next;
	}
   static final AtomicReferenceFieldUpdater<PointNode, PointNode> nextUpdater =
     AtomicReferenceFieldUpdater.newUpdater(PointNode.class, PointNode.class, "next");
}

volatile static PointNode completedPoints = new PointNode(0, null);

static long completedThroughPoint(){
	return completedPoints.tpoint;
}

static void relinquish(long tpoint){
	PointNode p = completedPoints;
	//update completedThroughPoint
	while(p.next != null && p.next.tpoint == p.tpoint+1)
		p = p.next;
	completedPoints = p;

	//splice in
	PointNode n;
	do{
	  for(n=p.next;n != null && n.tpoint < tpoint;p = n, n = p.next)
		  ;
	}while(!PointNode.nextUpdater.compareAndSet(p,n,new PointNode(tpoint,n)));
}

static void statusTransition(TStamp tstamp, TStamp.Status newStatus){
	synchronized(tstamp)
		{
		tstamp.status = newStatus;
		tstamp.notifyAll();
		}
}


TStamp tstamp;

void lock(TRef tref, boolean ensurePoint) throws Exception{
	TVal head = tref.tvals.get();
	//already locked by this transaction
	if(head != null && head.tstamp == tstamp)
		return;
	boolean locked;
	if(head != null && head.tstamp.status == TStamp.Status.RUNNING)
		{
		//already locked by another transaction, block a bit
		synchronized(head.tstamp)
			{
			if(head.tstamp.status == TStamp.Status.RUNNING)
				head.tstamp.wait(LOCK_WAIT_MSECS);
			}

		locked = false;
		}
	else
		{
		TVal prior;
		if(head == null || head.tstamp.status == TStamp.Status.COMMITTED)
			prior = head;
		else  //aborted/retried at head, skip over
			prior = head.prior;

		if(ensurePoint && prior != null && prior.tstamp.tpoint > tstamp.tpoint)
			locked = false;
		else
			locked = tref.tvals.compareAndSet(head, new TVal(prior == null ? null : prior.val, tstamp, prior));
		}

	if(!locked)
		{
		statusTransition(tstamp,TStamp.Status.RETRY);
		throw new RetryException();
		}
}

void abort() throws AbortException{
	statusTransition(tstamp,TStamp.Status.ABORTED);
	throw new AbortException();
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
		setTransaction(null);
		}
}

Object run(IFn fn) throws Exception{
	boolean done = false;
	Object ret = null;

	for(int i = 0; !done && i < RETRY_LIMIT; i++)
		{
		try
			{
			tstamp = new TStamp(getNextPoint());
			ret = fn.invoke();
			done = true;
			//save the read point
			long readPoint = tstamp.tpoint;
			//get a commit point and time
			tstamp.tpoint = getNextPoint();
			tstamp.msecs = System.currentTimeMillis();
			//commit!
			statusTransition(tstamp, TStamp.Status.COMMITTED);

			relinquish(readPoint);
			relinquish(tstamp.tpoint);
			}
		catch(RetryException retry)
			{
			//eat this so we retry rather than fall out
			}
		finally
			{
			if(!done)
				{
				statusTransition(tstamp,TStamp.Status.ABORTED);
				relinquish(tstamp.tpoint);
				}
			}
		}
	if(!done)
		throw new Exception("Transaction failed after reaching retry limit");
	return ret;
}



Object doGet(TRef tref) throws Exception{
	TVal head = tref.tvals.get();
	if(head == null)
		return null;
	if(head.tstamp == tstamp)
		return head.val;
	for(TVal ver = head.tstamp.status == TStamp.Status.COMMITTED?head:head.prior; ver != null; ver = ver.prior)
		{
		if(ver.tstamp.tpoint <= tstamp.tpoint)
			return ver.val;
		}
	return null;
}

Object doSet(TRef tref, Object val) throws Exception{
	lock(tref,true);
	tref.tvals.get().val = val;
	return val;
}

void doTouch(TRef tref) throws Exception{
		lock(tref,true);
}

void doCommute(TRef tref, IFn fn) throws Exception{
	lock(tref,false);
	TVal head = tref.tvals.get();
	head.val = fn.invoke(head.val);
}


/*
static public Object runInAsOfTransaction(IFn fn, int tpoint) throws Exception{
	if(getTransaction() != null)
		throw new Exception("As-of transactions cannot be nested");

	Transaction t = new Transaction(tpoint);
	setTransaction(t);
	try
		{
		return fn.invoke();//t.run(fn);
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
		return fn.invoke();//t.run(fn);
		}
	finally
		{
		setTransaction(null);
		}
}

 */
}
