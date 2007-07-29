/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Jul 25, 2007 */

package clojure.lang;

import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.locks.ReentrantReadWriteLock;

public class Ref implements IFn, Comparable<Ref>{

public int compareTo(Ref o){
	if(o.id == id)
		return 0;
	if(o.id > id)
		return -1;
	return 1;
}

public static class TVal{
	final Object val;
	final long point;
	long msecs;
	TVal prior;

	TVal(Object val, long point, long msecs, TVal prior){
		this.val = val;
		this.point = point;
		this.prior = prior;
		this.msecs = msecs;
	}

}


final static AtomicLong ids = new AtomicLong();
TVal tvals;
transient volatile InheritableThreadLocal<Binding> dvals;
final ReentrantReadWriteLock lock;
LockingTransaction.Info tinfo;
final long id;

public Ref(){
	this.tvals = null;
	this.dvals = null;
	this.tinfo = null;
	lock = new ReentrantReadWriteLock();
	id = ids.getAndIncrement();
}

public Ref(Object initVal){
	this();
	tvals = new TVal(initVal, 0, System.currentTimeMillis(), null);
}

//ok out of transaction
public Object currentVal(){
	Binding b = getThreadBinding();
	if(b != null)
		return b.val;
	try
		{
		lock.readLock().lock();
		if(tvals != null)
			return tvals.val;
		throw new IllegalStateException(this.toString() + " is unbound.");
		}
	finally
		{
		lock.readLock().unlock();
		}
}


final Binding getThreadBinding(){
	if(dvals != null)
		return dvals.get();
	return null;
}

public void pushThreadBinding(Object val){
	if(dvals == null)
		{
		synchronized(this)
			{
			if(dvals == null)
				dvals = new InheritableThreadLocal();
			}
		}
	dvals.set(new Binding(val, dvals.get()));
}

public void popThreadBinding() throws Exception{
	Binding b;
	if(dvals == null || (b = dvals.get()) == null)
		throw new Exception("Can't pop unbound ref");
	dvals.set(b.rest);
}

//*

//must be dynamically bound or transactional read
public Object val() throws Exception{
	Binding b = getThreadBinding();
	if(b != null)
		return b.val;
	return LockingTransaction.getEx().doGet(this);
}

public Object set(Object val) throws Exception{
	Binding b = getThreadBinding();
	if(b != null)
		return (b.val = val);
	return LockingTransaction.getEx().doSet(this, val);
}

public Object commute(IFn fn) throws Exception{
	Binding b = getThreadBinding();
	if(b != null)
		return (b.val = fn.invoke(b.val));
	return LockingTransaction.getEx().doCommute(this, fn);
}

public void touch() throws Exception{
	Binding b = getThreadBinding();
	if(b == null)
		LockingTransaction.getEx().doTouch(this);
}

//*/
boolean isBound(){
	if(dvals != null && dvals.get() != null)
		return true;
	try
		{
		lock.readLock().lock();
		return tvals != null;
		}
	finally
		{
		lock.readLock().unlock();
		}
}

void trimHistory(){
	long ctp = Transaction.completedThroughPoint();
	try
		{
		lock.writeLock().lock();
		for(TVal tv = tvals; tv != null; tv = tv.prior)
			{
			if(tv.point <= ctp)
				tv.prior = null;
			}
		}
	finally
		{
		lock.writeLock().unlock();
		}
}

final public IFn fn(){
	return (IFn) currentVal();
}

public Object invoke() throws Exception{
	return fn().invoke();
}

public Object invoke(Object arg1) throws Exception{
	return fn().invoke(arg1);
}

public Object invoke(Object arg1, Object arg2) throws Exception{
	return fn().invoke(arg1, arg2);
}

public Object invoke(Object arg1, Object arg2, Object arg3) throws Exception{
	return fn().invoke(arg1, arg2, arg3);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4) throws Exception{
	return fn().invoke(arg1, arg2, arg3, arg4);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5) throws Exception{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6) throws Exception{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7)
		throws Exception{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8) throws Exception{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9) throws Exception{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10) throws Exception{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11) throws Exception{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12) throws Exception{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13)
		throws Exception{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14)
		throws Exception{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15) throws Exception{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16) throws Exception{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15,
	                   arg16);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17) throws Exception{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15,
	                   arg16, arg17);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18) throws Exception{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15,
	                   arg16, arg17, arg18);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19) throws Exception{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15,
	                   arg16, arg17, arg18, arg19);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20)
		throws Exception{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15,
	                   arg16, arg17, arg18, arg19, arg20);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20,
                     Object... args)
		throws Exception{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15,
	                   arg16, arg17, arg18, arg19, arg20, args);
}

public Object applyTo(ISeq arglist) throws Exception{
	return AFn.applyToHelper(this, arglist);
}

}
