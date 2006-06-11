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

package clojure.runtime;

public class TVal{
volatile Object val;
volatile Transaction.Info tinfo;
volatile TVal prior;

TVal(){

}

TVal(Object val, Transaction.Info tinfo, TVal prior) {
    this.val = val;
    this.tinfo = tinfo;
    this.prior = prior;
}

void push(Object val,Transaction.Info tinfo) throws Exception{
	if(tinfo != null) //not newly created, clone tval part
        {
        this.prior = new TVal(this.val,this.tinfo,this.prior);
        }
    this.tinfo = tinfo;
	this.val = val;
}

}
