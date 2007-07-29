/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Jul 26, 2007 */

package clojure.lang;

import java.util.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;

public class LockingTransaction{

public static int RETRY_LIMIT = 1000;
public static int LOCK_WAIT_MSECS = 100;
static final int RUNNING = 0;
static final int COMMITTED = 1;
static final int RETRY = 2;
static final int KILLED = 3;

final static ThreadLocal<LockingTransaction> transaction = new ThreadLocal<LockingTransaction>();

static class RetryException extends Exception{
}

static class AbortException extends Exception{
}

public static class Info{
	final AtomicInteger status;
	final long point;


	public Info(int status, long point){
		this.status = new AtomicInteger(status);
		this.point = point;
	}

	public boolean running(){
		return status.get() == RUNNING;
	}
}
//total order on transactions
//transactions will consume a point for init, for each retry, and on commit if writing
final private static AtomicInteger lastPoint = new AtomicInteger();
//final static PriorityQueue<Long> points = new PriorityQueue<Long>();


long getCommitPoint(){
	return lastPoint.incrementAndGet();
//	synchronized(points)
//		{
//		points.remove(readPoint);
//		completedPriorPoint = completedThroughPoint();
//		++lastPoint;
//		return lastPoint;
//		}
}

/*
static long completedThroughPoint(){
	synchronized(points)
		{
		Long p = points.peek();
		if(p != null)
			return p - 1;
		return lastPoint;
		}
}

void relinquishReadPoint(){
	synchronized(points)
		{
		points.remove(readPoint);
		}
}
*/

void stop(int status){
	if(info != null)
		{
		synchronized(info)
			{
			info.status.set(status);
			info.notifyAll();
			}
		info = null;
		vals.clear();
		sets.clear();
		commutes.clear();
		}
}


Info info;
//long completedPriorPoint;
long readPoint;
long startPoint;
RetryException retryex = new RetryException();
HashMap<Ref, Object> vals = new HashMap<Ref, Object>();
HashSet<Ref> sets = new HashSet<Ref>();
TreeMap<Ref, ArrayList<IFn>> commutes = new TreeMap<Ref, ArrayList<IFn>>();

void getReadPoint(){
	readPoint = lastPoint.incrementAndGet();
//	synchronized(points)
//		{
//		++lastPoint;
//		points.add(lastPoint);
//		readPoint = lastPoint;
//		}
}

//returns the most recent val
Object lock(Ref ref, boolean ensurePoint) throws Exception{
	boolean unlocked = false;
	try
		{
		ref.lock.writeLock().lock();
		Info refinfo = ref.tinfo;

		if(refinfo != null && refinfo != info && refinfo.running())
			{
			ref.lock.writeLock().unlock();
			unlocked = true;
			//stop prior to blocking
			stop(RETRY);
			synchronized(refinfo)
				{
				if(refinfo.running())
					refinfo.wait(LOCK_WAIT_MSECS);
				}
			throw retryex;//new RetryException();
			}
		if(ensurePoint && ref.tvals != null && ref.tvals.point > readPoint)
			{
//			stop();
			throw retryex;//throw new RetryException();
			}
		ref.tinfo = info;
		return ref.tvals == null ? null : ref.tvals.val;
		}
	finally
		{
		if(!unlocked)
			ref.lock.writeLock().unlock();
		}
}

void abort() throws AbortException{
	stop(KILLED);
	throw new AbortException();
}


static LockingTransaction getEx() throws Exception{
	LockingTransaction t = transaction.get();
	if(t.info == null)
		throw new Exception("No transaction running");
	return t;
}

static public Object runInTransaction(IFn fn) throws Exception{
	LockingTransaction t = transaction.get();
	if(t == null)
		transaction.set(t = new LockingTransaction());

	if(t.info != null)
		return fn.invoke();

	return t.run(fn);
}

Object run(IFn fn) throws Exception{
	boolean done = false;
	Object ret = null;
	ArrayList<Ref> locked = new ArrayList<Ref>();

	for(int i = 0; !done && i < RETRY_LIMIT; i++)
		{
		try
			{
			getReadPoint();
			if(i == 0)
				startPoint = readPoint;
			info = new Info(RUNNING, startPoint);
			ret = fn.invoke();
			synchronized(info)
				{
				if(info.status.get() == RUNNING)
					{
					for(Map.Entry<Ref, ArrayList<IFn>> e : commutes.entrySet())
						{
						Ref ref = e.getKey();
						ref.lock.writeLock().lock();
						locked.add(ref);
						Info refinfo = ref.tinfo;
						if(refinfo != null && refinfo != info && refinfo.running())
							throw retryex;//new RetryException();
						Object val = ref.tvals == null ? null : ref.tvals.val;
						if(!sets.contains(ref))
							vals.put(ref, val);
						for(IFn f : e.getValue())
							{
							vals.put(ref, f.invoke(vals.get(ref)));
							}
						}
					for(Ref ref : sets)
						{
						if(!commutes.containsKey(ref))
							{
							ref.lock.writeLock().lock();
							locked.add(ref);
							}
						}

					//at this point, all values calced, all refs to be written locked
					//no more client code to be called
					long commitPoint = getCommitPoint();
					long msecs = System.currentTimeMillis();
					for(Map.Entry<Ref, Object> e : vals.entrySet())
						{
						Ref ref = e.getKey();
//				ref.tvals = new Ref.TVal(e.getValue(), commitPoint, msecs, null);
						if(ref.tvals != null)
							ref.tvals.prior = null;
						ref.tvals = new Ref.TVal(e.getValue(), commitPoint, msecs, ref.tvals);
						//ref.tstatus = null;
						//auto-trim
//				for(Ref.TVal tv = ref.tvals; tv != null; tv = tv.prior)
//					{
//					if(tv.msecs <= msecs)
//						tv.prior = null;
//					}
						}
					done = true;
					}
				}
			}
		catch(RetryException retry)
			{
			//eat this so we retry rather than fall out
			}
		finally
			{
			for(int k = locked.size() - 1; k >= 0; --k)
				{
				locked.get(k).lock.writeLock().unlock();
				}
			locked.clear();
			stop(done ? COMMITTED : RETRY);
//			if(!done)
//				relinquishReadPoint();
			}
		}
	if(!done)
		throw new Exception("Transaction failed after reaching retry limit");
	return ret;
}


Object doGet(Ref ref) throws Exception{
	if(vals.containsKey(ref))
		return vals.get(ref);
	try
		{
		ref.lock.readLock().lock();
		if(ref.tvals == null)
			throw new IllegalStateException(ref.toString() + " is unbound.");
		for(Ref.TVal ver = ref.tvals; ver != null; ver = ver.prior)
			{
			if(ver.point <= readPoint)
				return ver.val;
			}
		}
	finally
		{
		ref.lock.readLock().unlock();
		}
	//no version of val precedes the read point
	throw retryex;//new RetryException();
}

Object doSet(Ref ref, Object val) throws Exception{
	if(commutes.containsKey(ref))
		throw new IllegalStateException("Can't set after commute");
	if(!sets.contains(ref))
		{
		sets.add(ref);
		lock(ref, true);
		}
	vals.put(ref, val);
	return val;
}

void doTouch(Ref ref) throws Exception{
	lock(ref, true);
}

Object doCommute(Ref ref, IFn fn) throws Exception{
	if(!vals.containsKey(ref))
		{
		Object val = null;
		try
			{
			ref.lock.readLock().lock();
			val = ref.tvals == null ? null : ref.tvals.val;
			}
		finally
			{
			ref.lock.readLock().unlock();
			}
		vals.put(ref, val);
		}
	ArrayList<IFn> fns = commutes.get(ref);
	if(fns == null)
		commutes.put(ref, fns = new ArrayList<IFn>());
	fns.add(fn);
	Object ret = fn.invoke(vals.get(ref));
	vals.put(ref, ret);
	return ret;
}


//for test
static CyclicBarrier barrier;
static ArrayList<Ref> items;

public static void main(String[] args){
	try
		{
		if(args.length != 4)
			System.err.println("Usage: LockingTransaction nthreads nitems niters ninstances");
		int nthreads = Integer.parseInt(args[0]);
		int nitems = Integer.parseInt(args[1]);
		int niters = Integer.parseInt(args[2]);
		int ninstances = Integer.parseInt(args[3]);

		if(items == null)
			{
			ArrayList<Ref> temp = new ArrayList(nitems);
			for(int i = 0; i < nitems; i++)
				temp.add(new Ref(0));
			items = temp;
			}

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
			List<Ref> items;
			Incr incr;


			public Commuter(int niters, List<Ref> items){
				this.niters = niters;
				this.items = items;
				this.incr = new Incr();
			}

			public Object call() throws Exception{
				long nanos = 0;
				for(int i = 0; i < niters; i++)
					{
					long start = System.nanoTime();
					LockingTransaction.runInTransaction(this);
					long dur = System.nanoTime() - start;
					nanos += dur;
					}
				return nanos;
			}

			public Object invoke() throws Exception{
				for(Ref tref : items)
					{
					LockingTransaction.getEx().doCommute(tref, incr);
					}
				return null;
			}

			public Obj withMeta(IPersistentMap meta){
				throw new UnsupportedOperationException();

			}
		}

		class Incrementer extends AFn implements Callable{
			int niters;
			List<Ref> items;


			public Incrementer(int niters, List<Ref> items){
				this.niters = niters;
				this.items = items;
			}

			public Object call() throws Exception{
				long nanos = 0;
				for(int i = 0; i < niters; i++)
					{
					long start = System.nanoTime();
					LockingTransaction.runInTransaction(this);
					long dur = System.nanoTime() - start;
					nanos += dur;
					}
				return nanos;
			}

			public Object invoke() throws Exception{
				for(Ref tref : items)
					{
					//Transaction.get().doTouch(tref);
					LockingTransaction t = LockingTransaction.getEx();
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
			ArrayList<Ref> si;
			synchronized(items)
				{
				si = (ArrayList<Ref>) items.clone();
				}
			Collections.shuffle(si);
			tasks.add(new Incrementer(niters, si));
			//tasks.add(new Commuter(niters, si));
			}
		ExecutorService e = Executors.newFixedThreadPool(nthreads);

		if(barrier == null)
			barrier = new CyclicBarrier(ninstances);
		System.out.println("waiting for other instances...");
		barrier.await();
		System.out.println("starting");
		long start = System.nanoTime();
		List<Future<Long>> results = e.invokeAll(tasks);
		long estimatedTime = System.nanoTime() - start;
		System.out.printf("nthreads: %d, nitems: %d, niters: %d, time: %d%n", nthreads, nitems, niters,
		                  estimatedTime / 1000000);
		e.shutdown();
		for(Future<Long> result : results)
			{
			Future<Long> res = (Future<Long>) result;
			System.out.printf("%d, ", res.get() / 1000000);
			}
		System.out.println();
		System.out.println("waiting for other instances...");
		barrier.await();
		synchronized(items)
			{
			for(Ref item : items)
				{
				System.out.printf("%d, ", (Integer) item.currentVal());
				}
			}
		System.out.println("\ndone");
		System.out.flush();
		}
	catch(Exception ex)
		{
		ex.printStackTrace();
		}
}

}
