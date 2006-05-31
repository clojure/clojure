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

using System;

namespace org.clojure.runtime
{
public class TVal{
internal volatile Object val;
internal volatile Transaction.Info tinfo;
internal volatile TVal prior;

internal void push(Object val,Transaction.Info tinfo) {
	if(tinfo != null) //not newly created
		this.prior = (TVal) this.MemberwiseClone();
	this.tinfo = tinfo;
	this.val = val;
}

}
}
