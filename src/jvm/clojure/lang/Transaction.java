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

import java.util.concurrent.atomic.AtomicReferenceFieldUpdater;
import java.util.concurrent.*;
import java.util.*;

public class Transaction{

public static int RETRY_LIMIT = 1000;
public static int LOCK_WAIT_MSECS = 100;

final static ThreadLocal<Transaction> transaction = new ThreadLocal<Transaction>();

static class RetryException extends Exception{
}

static class AbortException extends Exception{
}

//total order on transactions
//transactions will consume a point for init, for each retry, and on commit if writing
private static long nextPoint = 1;
final static PriorityQueue<Long> points = new PriorityQueue<Long>();

void getReadPoint(){
	synchronized(points)
		{
		completedPriorPoint = completedThroughPoint();
		points.add(nextPoint);
		readPoint = nextPoint++;
		}
}

static long getCommitPoint(){
	synchronized(points)
		{
		return nextPoint++;
		}
}

final static TStamp ZERO_POINT = new TStamp(TStamp.Status.COMMITTED);

static long completedThroughPoint(){
	synchronized(points)
		{
		Long p = points.peek();
		if(p != null)
			return p - 1;
		return nextPoint - 1;
		}
}

static void relinquish(long tpoint){
	synchronized(points)
		{
		points.remove(tpoint);
		}
}

static void statusTransition(TStamp tstamp, TStamp.Status newStatus){
	synchronized(tstamp)
		{
		tstamp.status = newStatus;
		tstamp.notifyAll();
		}
}


TStamp tstamp;
long completedPriorPoint;
long readPoint;

TVal lock(TRef tref, boolean ensurePoint) throws Exception{
	TVal head = (TVal) tref.tvals.get();
	//already locked by this transaction
	if(head != null && head.tstamp == tstamp)
		return head;
	if(head != null && head.tstamp.status == TStamp.Status.RUNNING)
		{
		//already locked by another transaction, block a bit
		//first drop our locks
		statusTransition(tstamp, TStamp.Status.RETRY);
		synchronized(head.tstamp)
			{
			if(head.tstamp.status == TStamp.Status.RUNNING)
				head.tstamp.wait(LOCK_WAIT_MSECS);
			}

		throw new RetryException();
		}
	else
		{
		TVal prior;
		if(head == null || head.tstamp.status == TStamp.Status.COMMITTED)
			{
			prior = head;
			}
		else  //aborted/retried at head, skip over
			prior = head.prior;
		TVal ret = null;
		if((ensurePoint && prior != null && prior.tstamp.tpoint > readPoint)
		   ||
		   !tref.tvals.compareAndSet(head, ret = new TVal(prior == null ? null : prior.val, tstamp, prior)))
			{
			statusTransition(tstamp, TStamp.Status.RETRY);
			throw new RetryException();
			}
		//auto-trim
		for(TVal tv = prior; tv != null; tv = tv.prior)
			{
			if(tv.tstamp.tpoint <= completedPriorPoint)
				tv.prior = null;
			}
		return ret;
		}
}

void abort() throws AbortException{
	statusTransition(tstamp, TStamp.Status.ABORTED);
	throw new AbortException();
}

static Transaction get(){
	return transaction.get();
}

static Transaction getEx() throws Exception{
	Transaction t = transaction.get();
	if(t == null)
		throw new Exception("No transaction running");
	return t;
}

static void setTransaction(Transaction t){
	transaction.set(t);
}

static public Object runInTransaction(IFn fn) throws Exception{
	if(get() != null)
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
			getReadPoint();
			tstamp = new TStamp(TStamp.Status.RUNNING);
			ret = fn.invoke();
			done = true;
			tstamp.msecs = System.currentTimeMillis();
			//get a commit point + alter status, atomically
			synchronized(points)
				{
				tstamp.tpoint = getCommitPoint();
				//commit!
				statusTransition(tstamp, TStamp.Status.COMMITTED);
				relinquish(readPoint);
				//relinquish(tstamp.tpoint);
				}
			}
		catch(RetryException retry)
			{
			//eat this so we retry rather than fall out
			}
		finally
			{
			if(!done)
				{
				statusTransition(tstamp, TStamp.Status.ABORTED);
				relinquish(readPoint);
				//relinquish(tstamp.tpoint);
				}
			}
		}
	if(!done)
		throw new Exception("Transaction failed after reaching retry limit");
	return ret;
}


Object doGet(TRef tref) throws Exception{
	TVal head = (TVal) tref.tvals.get();
	if(head == null)
		return null;
	if(head.tstamp == tstamp)
		return head.val;
	TVal ver;
	switch(head.tstamp.status)
		{
		case COMMITTED:
			ver = head;
			break;
		case RETRY:
		case ABORTED:
			ver = head.prior;
			break;
		default:
			//ensure a running->commit transition happens before/after our read point
			synchronized(head.tstamp)
				{
				ver = head.tstamp.status == TStamp.Status.COMMITTED ? head : head.prior;
				}
			break;
		}

	for(; ver != null; ver = ver.prior)
		{
		if(ver.tstamp.tpoint <= readPoint)
			return ver.val;
		}
	throw new IllegalStateException(tref.toString() + " is unbound.");
}

Object doSet(TRef tref, Object val) throws Exception{
	TVal head = lock(tref, true);
	head.val = val;
	return val;
}

void doTouch(TRef tref) throws Exception{
	lock(tref, true);
}

Object doCommute(TRef tref, IFn fn) throws Exception{
	TVal head = lock(tref, false);
	return head.val = fn.invoke(head.val);
}

/*
static public Object runInAsOfTransaction(IFn fn, int tpoint) throws Exception{
	if(get() != null)
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
	if(get() != null)
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

//for test
static CyclicBarrier barrier;

public static void main(String[] args){
	try
		{
		if(args.length != 4)
			System.err.println("Usage: Transaction nthreads nitems niters ninstances");
		int nthreads = Integer.parseInt(args[0]);
		int nitems = Integer.parseInt(args[1]);
		int niters = Integer.parseInt(args[2]);
		int ninstances = Integer.parseInt(args[3]);

		final ArrayList<TRef> items = new ArrayList(nitems);
		for(int i = 0; i < nitems; i++)
			items.add(new TRef(0));


		class Incr extends AFn{
			public Object invoke(Object arg1) throws Exception{
				Integer i = (Integer) arg1;
				return i + 1;
			}

			public Obj withMeta(IPersistentMap meta){
				throw new UnsupportedOperationException();

			}
		}

		class Commuter extends AFn implements Callable{
			int niters;
			List<TRef> items;
			Incr incr;


			public Commuter(int niters, List<TRef> items){
				this.niters = niters;
				this.items = items;
				this.incr = new Incr();
			}

			public Object call() throws Exception{
				long nanos = 0;
				for(int i = 0; i < niters; i++)
					{
					long start = System.nanoTime();
					Transaction.runInTransaction(this);
					long dur = System.nanoTime() - start;
					nanos += dur;
					}
				return nanos;
			}

			public Object invoke() throws Exception{
				for(TRef tref : items)
					{
					Transaction.get().doCommute(tref, incr);
					}
				return null;
			}

			public Obj withMeta(IPersistentMap meta){
				throw new UnsupportedOperationException();

			}
		}

		class Incrementer extends AFn implements Callable{
			int niters;
			List<TRef> items;


			public Incrementer(int niters, List<TRef> items){
				this.niters = niters;
				this.items = items;
			}

			public Object call() throws Exception{
				long nanos = 0;
				for(int i = 0; i < niters; i++)
					{
					long start = System.nanoTime();
					Transaction.runInTransaction(this);
					long dur = System.nanoTime() - start;
					nanos += dur;
					}
				return nanos;
			}

			public Object invoke() throws Exception{
				for(TRef tref : items)
					{
					//Transaction.get().doTouch(tref);
					Transaction t = Transaction.get();
					int val = (Integer) t.doGet(tref);
					t.doSet(tref, val + 1);
					}
				return null;
			}

			public Obj withMeta(IPersistentMap meta){
				throw new UnsupportedOperationException();

			}
		}

		ArrayList<Callable<Long>> tasks = new ArrayList(nthreads);
		for(int i = 0; i < nthreads; i++)
			{
			ArrayList<TRef> si = (ArrayList<TRef>) items.clone();
			Collections.shuffle(si);
			tasks.add(new Incrementer(niters, si));
			}
		ExecutorService e = Executors.newFixedThreadPool(nthreads);

		if(barrier == null)
			barrier = new CyclicBarrier(ninstances);
		barrier.await();
		long start = System.nanoTime();
		List<Future<Long>> results = e.invokeAll(tasks);
		long estimatedTime = System.nanoTime() - start;
		System.out.printf("nthreads: %d, nitems: %d, niters: %d, time: %d%n", nthreads, nitems, niters,
		                  estimatedTime / 1000000);
		e.shutdown();
		barrier.await();
		for(Future<Long> res : results)
			{
			System.out.printf("%d, ", res.get() / 1000000);
			}
		System.out.println();
		for(TRef item : items)
			{
			System.out.printf("%d, ", (Integer) item.currentVal());
			}
		}
	catch(Exception ex)
		{
		ex.printStackTrace();
		}
}

}
