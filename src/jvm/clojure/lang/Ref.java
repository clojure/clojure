/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Jul 25, 2007 */

package clojure.lang;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.concurrent.locks.ReentrantReadWriteLock;

public class Ref extends ARef implements IFn, Comparable<Ref>, IRef{
    public int compareTo(Ref ref) {
        if(this.id == ref.id)
            return 0;
        else if(this.id < ref.id)
            return -1;
        else
            return 1;
    }

public int getMinHistory(){
	return minHistory;
}

public Ref setMinHistory(int minHistory){
	this.minHistory = minHistory;
	return this;
}

public int getMaxHistory(){
	return maxHistory;
}

public Ref setMaxHistory(int maxHistory){
	this.maxHistory = maxHistory;
	return this;
}

public static class TVal{
	Object val;
	long point;
	long msecs;
	TVal prior;
	TVal next;

	TVal(Object val, long point, long msecs, TVal prior){
		this.val = val;
		this.point = point;
		this.msecs = msecs;
		this.prior = prior;
		this.next = prior.next;
		this.prior.next = this;
		this.next.prior = this;
	}

	TVal(Object val, long point, long msecs){
		this.val = val;
		this.point = point;
		this.msecs = msecs;
		this.next = this;
		this.prior = this;
	}

}

TVal tvals;
final AtomicInteger faults;
final ReentrantReadWriteLock lock;
LockingTransaction.Info tinfo;
//IFn validator;
final long id;

volatile int minHistory = 0;
volatile int maxHistory = 10;

static final AtomicLong ids = new AtomicLong();

public Ref(Object initVal) {
	this(initVal, null);
}

public Ref(Object initVal,IPersistentMap meta) {
    super(meta);
    this.id = ids.getAndIncrement();
	this.faults = new AtomicInteger();
	this.lock = new ReentrantReadWriteLock();
	tvals = new TVal(initVal, 0, System.currentTimeMillis());
}

//the latest val

// ok out of transaction
Object currentVal(){
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

//*

public Object deref(){
	LockingTransaction t = LockingTransaction.getRunning();
	if(t == null)
		return currentVal();
	return t.doGet(this);
}

//void validate(IFn vf, Object val){
//	try{
//		if(vf != null && !RT.booleanCast(vf.invoke(val)))
//            throw new IllegalStateException("Invalid ref state");
//		}
//    catch(RuntimeException re)
//        {
//        throw re;
//        }
//	catch(Exception e)
//		{
//		throw new IllegalStateException("Invalid ref state", e);
//		}
//}
//
//public void setValidator(IFn vf){
//	try
//		{
//		lock.writeLock().lock();
//		validate(vf,currentVal());
//		validator = vf;
//		}
//	finally
//		{
//		lock.writeLock().unlock();
//		}
//}
//
//public IFn getValidator(){
//	try
//		{
//		lock.readLock().lock();
//		return validator;
//		}
//	finally
//		{
//		lock.readLock().unlock();
//		}
//}

public Object set(Object val){
	return LockingTransaction.getEx().doSet(this, val);
}

public Object commute(IFn fn, ISeq args) {
	return LockingTransaction.getEx().doCommute(this, fn, args);
}

public Object alter(IFn fn, ISeq args) {
	LockingTransaction t = LockingTransaction.getEx();
	return t.doSet(this, fn.applyTo(RT.cons(t.doGet(this), args)));
}

public void touch(){
	LockingTransaction.getEx().doEnsure(this);
}

//*/
boolean isBound(){
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


public void trimHistory(){
	try
		{
		lock.writeLock().lock();
		if(tvals != null)
			{
			tvals.next = tvals;
			tvals.prior = tvals;
			}
		}
	finally
		{
		lock.writeLock().unlock();
		}
}

public int getHistoryCount(){
	try
		{
		lock.writeLock().lock();
		return histCount();
		}
	finally
		{
		lock.writeLock().unlock();
		}	
}

int histCount(){
	if(tvals == null)
		return 0;
	else
		{
		int count = 0;
		for(TVal tv = tvals.next;tv != tvals;tv = tv.next)
			count++;
		return count;
		}
}

final public IFn fn(){
	return (IFn) deref();
}

public Object call() {
	return invoke();
}

public void run(){
	try
		{
		invoke();
		}
	catch(Exception e)
		{
		throw Util.sneakyThrow(e);
		}
}

public Object invoke() {
	return fn().invoke();
}

public Object invoke(Object arg1) {
	return fn().invoke(arg1);
}

public Object invoke(Object arg1, Object arg2) {
	return fn().invoke(arg1, arg2);
}

public Object invoke(Object arg1, Object arg2, Object arg3) {
	return fn().invoke(arg1, arg2, arg3);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4) {
	return fn().invoke(arg1, arg2, arg3, arg4);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5) {
	return fn().invoke(arg1, arg2, arg3, arg4, arg5);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6) {
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7)
		{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8) {
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9) {
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10) {
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11) {
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12) {
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13)
		{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14)
		{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15) {
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16) {
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15,
	                   arg16);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17) {
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15,
	                   arg16, arg17);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18) {
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15,
	                   arg16, arg17, arg18);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19) {
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15,
	                   arg16, arg17, arg18, arg19);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20)
		{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15,
	                   arg16, arg17, arg18, arg19, arg20);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20,
                     Object... args)
		{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15,
	                   arg16, arg17, arg18, arg19, arg20, args);
}

public Object applyTo(ISeq arglist) {
	return AFn.applyToHelper(this, arglist);
}

}
