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

public class TRef<T>{
//reference to a chain of TVals, only the head of which may be non-committed
final AtomicReference<TVal> tvals;

public TRef() {
	this.tvals = new AtomicReference<TVal>();
}

public TRef(T initVal) {
	this.tvals = new AtomicReference<TVal>();
	tvals.set(new TVal(initVal, Transaction.ZERO_POINT, null));
}

public T getCurrentVal(){
	TVal current = getCurrentTVal();
	if(current != null)
		return (T)current.val;
	return null;
}

public T get() throws Exception{
	Transaction t = Transaction.get();
	if(t != null)
		return (T) t.doGet(this);
	return getCurrentVal();
}

public T set(T val) throws Exception{
	return (T) Transaction.getEx().doSet(this,val);
}

public T commute(T val,IFn fn) throws Exception{
	return (T) Transaction.getEx().doCommute(this,fn);
}

public void touch() throws Exception{
	Transaction.getEx().doTouch(this);
}

TVal getCurrentTVal(){
	TVal head = tvals.get();
	if(head == null || head.tstamp.status == TStamp.Status.COMMITTED)
		return head;
	return head.prior;
}

TVal valAsOfPoint(TRef tref, int tpoint){
	for(TVal tv = getCurrentTVal();tv != null;tv = tv.prior)
		{
		if(tv.tstamp.tpoint <= tpoint)
			return tv;
		}
	return null;
}

TVal valAsOfTime(TRef tref,long msecs){
	for(TVal tv = getCurrentTVal();tv != null;tv = tv.prior)
		{
		if(tv.tstamp.msecs <= msecs)
			return tv;
		}
	return null;
}

void trimHistory(){
	long ctp = Transaction.completedThroughPoint();
	for(TVal tv = getCurrentTVal();tv != null;tv = tv.prior)
		{
		while(tv.tstamp.tpoint > ctp)
			tv = tv.prior;
		tv.prior = null;
		}
}

void trimHistoryPriorToPoint(int tpoint){
	long ctp = Transaction.completedThroughPoint();
	for(TVal tv = getCurrentTVal();tv != null;tv = tv.prior)
		{
		while(tv.tstamp.tpoint > tpoint || tv.tstamp.tpoint > ctp)
			tv = tv.prior;
		tv.prior = null;
		}
}

void trimHistoryPriorToTime(long msecs){
	long ctp = Transaction.completedThroughPoint();
	for(TVal tv = getCurrentTVal();tv != null;tv = tv.prior)
		{
		while(tv.tstamp.msecs > msecs || tv.tstamp.tpoint > ctp)
			tv = tv.prior;
		tv.prior = null;
		}
}

}
