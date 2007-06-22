/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Jun 22, 2007 */

package clojure.lang;

import java.util.concurrent.ConcurrentHashMap;

public class TPool{
final ConcurrentHashMap<TRef,TVal> hist;


public TPool(){
	this.hist = new ConcurrentHashMap<TRef,TVal>();
}

TRef createRef(){
	return new TRef(this);
}

void pushVal(TRef tref,TVal tval){
	//note this presumes caller has exclusive rights to tref
	tval.prior = hist.get(tref);
	hist.put(tref, tval);
}

TVal valAsOfPoint(TRef tref, int tpoint){
	for(TVal tv = hist.get(tref);tv != null;tv = tv.prior)
		{
		if(tv.tstamp.tpoint <= tpoint)
			return tv;
		}
	return null;
}


TVal valAsOfTime(TRef tref,long msecs){
	for(TVal tv = hist.get(tref);tv != null;tv = tv.prior)
		{
		if(tv.tstamp.msecs <= msecs)
			return tv;
		}
	return null;
}

void trimHistory(){
	int ctp = Transaction.completedThroughPoint();
	for(TVal tv : hist.values())
		{
		while(tv.tstamp.tpoint > ctp)
			tv = tv.prior;
		tv.prior = null;
		}
}

void trimHistoryPriorToPoint(int tpoint){
	int ctp = Transaction.completedThroughPoint();
	for(TVal tv : hist.values())
		{
		while(tv.tstamp.tpoint > tpoint || tv.tstamp.tpoint > ctp)
			tv = tv.prior;
		tv.prior = null;
		}
}

void trimHistoryPriorToTime(long msecs){
	int ctp = Transaction.completedThroughPoint();
	for(TVal tv : hist.values())
		{
		while(tv.tstamp.msecs > msecs || tv.tstamp.tpoint > ctp)
			tv = tv.prior;
		tv.prior = null;
		}
}

}
