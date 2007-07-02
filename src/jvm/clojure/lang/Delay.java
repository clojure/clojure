/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Jun 28, 2007 */

package clojure.lang;

public class Delay{
public volatile Object val;
IFn fn;


public Delay(IFn fn){
	this.fn = fn;
	this.val = this;
}

public Object force() throws Exception{
	if(val == this)
		{
		synchronized(this)
			{
			if(val == this)
				{
				val = fn.invoke();
				fn = null;
				}
			}
		}
	return val;
}
}
