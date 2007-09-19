/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 29, 2006 10:39:05 AM */

package clojure.lang;

import java.util.concurrent.ConcurrentHashMap;


public class Keyword implements IFn, Comparable{

private static ConcurrentHashMap<Symbol, Keyword> table = new ConcurrentHashMap();
public final Symbol sym;

public static Keyword intern(Symbol sym){
	Keyword k = new Keyword(sym);
	Keyword existingk = table.putIfAbsent(sym, k);
	return existingk == null ? k : existingk;
}

public static Keyword intern(String ns, String name){
	return intern(Symbol.intern(ns, name));
}

private Keyword(Symbol sym){
	this.sym = sym;
}

public String toString(){
	return ":" + sym;
}

public Object call() throws Exception{
	return AFn.throwArity();
}

public Object invoke() throws Exception{
	return AFn.throwArity();
}

public int compareTo(Object o){
	return sym.compareTo(((Keyword) o).sym);
}


/**
 * Indexer implements IFn for attr access
 *
 * @param obj - must be IPersistentMap
 * @return the value at the key or nil if not found
 * @throws Exception
 */
public Object invoke(Object obj) throws Exception{
	if(obj == null)
		return null;
	return ((IPersistentMap) obj).valAt(this);
}

public Object invoke(Object obj, Object val) throws Exception{
	return AFn.throwArity();
}

public Object invoke(Object arg1, Object arg2, Object arg3) throws Exception{
	return AFn.throwArity();
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4) throws Exception{
	return AFn.throwArity();
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5) throws Exception{
	return AFn.throwArity();
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6) throws Exception{
	return AFn.throwArity();
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7)
		throws Exception{
	return AFn.throwArity();
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8) throws Exception{
	return AFn.throwArity();
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9) throws Exception{
	return AFn.throwArity();
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10) throws Exception{
	return AFn.throwArity();
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11) throws Exception{
	return AFn.throwArity();
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12) throws Exception{
	return AFn.throwArity();
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13)
		throws Exception{
	return AFn.throwArity();
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14)
		throws Exception{
	return AFn.throwArity();
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15) throws Exception{
	return AFn.throwArity();
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16) throws Exception{
	return AFn.throwArity();
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17) throws Exception{
	return AFn.throwArity();
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18) throws Exception{
	return AFn.throwArity();
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19) throws Exception{
	return AFn.throwArity();
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20)
		throws Exception{
	return AFn.throwArity();
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20,
                     Object... args)
		throws Exception{
	return AFn.throwArity();
}


public Object applyTo(ISeq arglist) throws Exception{
	return AFn.applyToHelper(this, arglist);
}


}
