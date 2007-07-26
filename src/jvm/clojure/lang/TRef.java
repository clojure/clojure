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

import java.util.concurrent.atomic.AtomicReference;

public class TRef<T> extends AFn{
//reference to a chain of TVals, only the head of which may be non-committed
final AtomicReference<TVal> tvals;
final AtomicReference<InheritableThreadLocal> dvals;


public TRef(){
	this.tvals = new AtomicReference<TVal>();
	this.dvals = new AtomicReference<InheritableThreadLocal>();
}

public Obj withMeta(IPersistentMap meta){
	return new TRef(meta, tvals, dvals);
}


private TRef(IPersistentMap meta, AtomicReference<TVal> tvals, AtomicReference<InheritableThreadLocal> dvals){
	super(meta);
	this.tvals = tvals;
	this.dvals = dvals;
}

public TRef(T initVal){
	this();
	tvals.set(new TVal(initVal, Transaction.ZERO_POINT, null));
}

public boolean equals(Object o){
	if(this == o) return true;
	if(o == null || TRef.class != o.getClass()) return false;

	TRef other = (TRef) o;

	return dvals == other.dvals && tvals == other.tvals;
}

public int hashCode(){
	return RT.hashCombine(dvals.hashCode(), tvals.hashCode());
}

public T currentVal(){
	Binding b = getThreadBinding();
	if(b != null)
		return (T) b.val;
	TVal current = getCurrentTVal();
	if(current != null)
		return (T) current.val;
	throw new IllegalStateException(this.toString() + " is unbound.");
}

public T val() throws Exception{
	Binding b = getThreadBinding();
	if(b != null)
		return (T) b.val;
	Transaction t = Transaction.get();
	if(t != null)
		return (T) t.doGet(this);
	throw new IllegalStateException(this.toString() + " is unbound.");
}

final Binding getThreadBinding(){
	InheritableThreadLocal dv = dvals.get();
	if(dv != null)
		return (Binding) dv.get();
	return null;
}

public void pushThreadBinding(T val){
	InheritableThreadLocal dv = dvals.get();
	if(dv == null)
		{
		dvals.compareAndSet(null, new InheritableThreadLocal());
		dv = dvals.get();
		}
	dv.set(new Binding(val, (Binding) dv.get()));
}

public void popThreadBinding() throws Exception{
	InheritableThreadLocal dv = dvals.get();
	Binding b;
	if(dv == null || (b = (Binding) dv.get()) == null)
		throw new Exception("Can't pop unbound ref");
	dv.set(b.rest);
}

public T set(T val) throws Exception{
	Binding b = getThreadBinding();
	if(b != null)
		return (T) (b.val = val);
	//allow out-of-transaction inits?
	if(!isBound())
		{
		tvals.set(new TVal(val, Transaction.ZERO_POINT, null));
		return val;
		}
	return (T) Transaction.getEx().doSet(this, val);
}

public T commute(IFn fn) throws Exception{
	Binding b = getThreadBinding();
	if(b != null)
		return (T) (b.val = fn.invoke(b.val));
	return (T) Transaction.getEx().doCommute(this, fn);
}

public void touch() throws Exception{
	Transaction.getEx().doTouch(this);
}

boolean isBound(){
	InheritableThreadLocal dv = dvals.get();
	return (dv != null && dv.get() != null)
	       ||
	       getCurrentTVal() != null;
}

TVal getCurrentTVal(){
	TVal head = tvals.get();
	if(head == null || head.tstamp.status == TStamp.Status.COMMITTED)
		return head;
	return head.prior;
}

TVal valAsOfPoint(TRef tref, int tpoint){
	for(TVal tv = getCurrentTVal(); tv != null; tv = tv.prior)
		{
		if(tv.tstamp.tpoint <= tpoint)
			return tv;
		}
	return null;
}

TVal valAsOfTime(TRef tref, long msecs){
	for(TVal tv = getCurrentTVal(); tv != null; tv = tv.prior)
		{
		if(tv.tstamp.msecs <= msecs)
			return tv;
		}
	return null;
}

void trimHistory(){
	long ctp = Transaction.completedThroughPoint();
	for(TVal tv = getCurrentTVal(); tv != null; tv = tv.prior)
		{
		while(tv.tstamp.tpoint > ctp)
			tv = tv.prior;
		tv.prior = null;
		}
}

void trimHistoryPriorToPoint(long tpoint){
	long ctp = Transaction.completedThroughPoint();
	for(TVal tv = getCurrentTVal(); tv != null; tv = tv.prior)
		{
		while(tv.tstamp.tpoint > tpoint || tv.tstamp.tpoint > ctp)
			tv = tv.prior;
		tv.prior = null;
		}
}

void trimHistoryPriorToTime(long msecs){
	long ctp = Transaction.completedThroughPoint();
	for(TVal tv = getCurrentTVal(); tv != null; tv = tv.prior)
		{
		while(tv.tstamp.msecs > msecs || tv.tstamp.tpoint > ctp)
			tv = tv.prior;
		tv.prior = null;
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

}
