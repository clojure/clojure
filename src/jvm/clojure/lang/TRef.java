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

import java.util.concurrent.atomic.AtomicInteger;

public class TRef implements Comparable{
final static AtomicInteger nextSeq = new AtomicInteger(1);

final int lockSeq;
final AtomicInteger lockedBy;
final TPool pool;
volatile TVal tval;

public TRef(TPool pool) {
	this.pool = pool;
	this.lockSeq = nextSeq.getAndIncrement();
	this.lockedBy = new AtomicInteger();
	this.tval = null;
}

void push(TVal tval){
	pool.pushVal(this,tval);
	this.tval = tval;
}
public int compareTo(Object o){
	return lockSeq - ((TRef) o).lockSeq;
}

public Object getLatestVal(){
	//will NPE if never been set
	return tval.val;
}

}
