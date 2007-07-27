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

public class LockingTransaction{

public static int RETRY_LIMIT = 1000;
public static int LOCK_WAIT_MSECS = 100;

final static ThreadLocal<LockingTransaction> transaction = new ThreadLocal<LockingTransaction>();

static class RetryException extends Exception{
}

static class AbortException extends Exception{
}

//total order on transactions
//transactions will consume a point for init, for each retry, and on commit if writing
private static long lastPoint;
final static PriorityQueue<Long> points = new PriorityQueue<Long>();


long getCommitPoint(){
	synchronized(points)
		{
		points.remove(readPoint);
		completedPriorPoint = completedThroughPoint();
		++lastPoint;
		return lastPoint;
		}
}

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

void stop(){
	if(tstatus.running)
		{
		synchronized(tstatus)
			{
			tstatus.running = false;
			tstatus.notifyAll();
			}
		vals.clear();
		sets.clear();
		commutes.clear();
		}
}


volatile Ref.TStatus tstatus;
long completedPriorPoint;
long readPoint;
HashMap<Ref, Object> vals = new HashMap<Ref, Object>();
HashSet<Ref> sets = new HashSet<Ref>();
TreeMap<Ref, ArrayList<IFn>> commutes = new TreeMap<Ref, ArrayList<IFn>>();

void getReadPoint(){
	synchronized(points)
		{
		++lastPoint;
		points.add(lastPoint);
		readPoint = lastPoint;
		}
}

//returns the most recent val
Object lock(Ref ref, boolean ensurePoint) throws Exception{
	boolean unlocked = false;
	try
		{
		ref.lock.writeLock().lock();
		Ref.TStatus status = ref.tstatus;

		if(status != null && status != tstatus && status.running)
			{
			ref.lock.writeLock().unlock();
			unlocked = true;
			stop();
			synchronized(status)
				{
				if(status.running)
					status.wait(LOCK_WAIT_MSECS);
				}
			throw new RetryException();
			}
		if(ensurePoint && ref.tvals != null && ref.tvals.point > readPoint)
			{
			stop();
			throw new RetryException();
			}
		ref.tstatus = tstatus;
		return ref.tvals == null ? null : ref.tvals.val;
		}
	finally
		{
		if(!unlocked)
			ref.lock.writeLock().unlock();
		}
}

void abort() throws AbortException{
	stop();
	throw new AbortException();
}


static LockingTransaction getEx() throws Exception{
	LockingTransaction t = transaction.get();
	if(!t.tstatus.running)
		throw new Exception("No transaction running");
	return t;
}

static public Object runInTransaction(IFn fn) throws Exception{
	LockingTransaction t = transaction.get();
	if(t == null)
		transaction.set(t = new LockingTransaction());

	if(t.tstatus != null && t.tstatus.running)
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
			tstatus = new Ref.TStatus();
			ret = fn.invoke();

			for(Map.Entry<Ref, ArrayList<IFn>> e : commutes.entrySet())
				{
				Ref ref = e.getKey();
				ref.lock.writeLock().lock();
				locked.add(ref);
				Ref.TStatus status = ref.tstatus;
				if(status != null && status != tstatus && status.running)
					throw new RetryException();
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
				ref.tvals = new Ref.TVal(e.getValue(), commitPoint, msecs, ref.tvals);
				ref.tstatus = null;
				//auto-trim
				for(Ref.TVal tv = ref.tvals; tv != null; tv = tv.prior)
					{
					if(tv.point <= completedPriorPoint)
						tv.prior = null;
					}
				}
			done = true;
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
			stop();
			if(!done)
				relinquishReadPoint();
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

	throw new IllegalStateException(ref.toString() + " is unbound.");
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
//			for(Ref.TVal ver = ref.tvals; ver != null; ver = ver.prior)
//				{
//				if(ver.point <= readPoint)
//					{
//					val = ver.val;
//					break;
//					}
//				}
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
		for(Future<Long> res : results)
			{
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
