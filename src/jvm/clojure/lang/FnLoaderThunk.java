/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich 2/28/11 */

package clojure.lang;

public class FnLoaderThunk extends RestFn{

final Var v;
final ClassLoader loader;
final String fnClassName;
IFn fn;

public FnLoaderThunk(Var v, String fnClassName){
	this.v = v;
	this.loader = (ClassLoader) RT.FN_LOADER_VAR.get();
	this.fnClassName = fnClassName;
	fn = null;
}

public Object invoke(Object arg1) {
	load();
	return fn.invoke(arg1);
}

public Object invoke(Object arg1, Object arg2) {
	load();
	return fn.invoke(arg1,arg2);
}

public Object invoke(Object arg1, Object arg2, Object arg3) {
	load();
	return fn.invoke(arg1,arg2,arg3);
}

protected Object doInvoke(Object args) {
	load();
	return fn.applyTo((ISeq) args);
}

private void load() {
	if(fn == null)
		{
		try
			{
			fn = (IFn) Class.forName(fnClassName,true,loader).newInstance();
			}
		catch(Exception e)
			{
			throw Util.sneakyThrow(e);
			}
		v.root = fn;
		}
}

public int getRequiredArity(){
	return 0;
}

public IObj withMeta(IPersistentMap meta){
	return this;
}

public IPersistentMap meta(){
	return null;
}
}
