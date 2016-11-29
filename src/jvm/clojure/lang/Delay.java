/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Jun 28, 2007 */

package clojure.lang;

public class Delay implements IDeref, IPending{
volatile Object val;
volatile Throwable exception;
volatile IFn fn;

public Delay(IFn fn){
	this.fn = fn;
	this.val = null;
        this.exception = null;
}

static public Object force(Object x) {
	return (x instanceof Delay) ?
	       ((Delay) x).deref()
	       : x;
}

public Object deref() {
	if(fn != null)
		{
	        synchronized(this)
	        {
	        //double check
	        if(fn!=null)
	            {
	                try
	                    {
	                    val = fn.invoke();
	                    }
	                catch(Throwable t)
	                    {
	                    exception = t;
	                    }
	                fn = null;
	            }
	        }
		}
	if(exception != null)
		throw Util.sneakyThrow(exception);
	return val;
}

synchronized public boolean isRealized(){
	return fn == null;
}
}
