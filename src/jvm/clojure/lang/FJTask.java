/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Jul 21, 2009 */

package clojure.lang;

import jsr166y.ForkJoinTask;

import java.util.concurrent.Callable;

public class FJTask extends ForkJoinTask<Object>{
final Callable f;
Object result;

public FJTask(Callable f){
	this.f = f;
}

public Object compute(){
	try
		{
		return f.call();
		}
	catch(Exception e)
		{
		throw new RuntimeException(e);
		}
}

public final Object getRawResult(){
	return result;
}

protected final void setRawResult(Object value){
	result = value;
}

protected final boolean exec(){
	result = compute();
	return true;
}

}
