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

public class TVal{

volatile Object val;
public final TStamp tstamp;
volatile TVal prior;

TVal(Object val, TStamp tstamp, TVal prior) {
    this.val = val;
	this.tstamp = tstamp;
	this.prior = prior;
}
}
