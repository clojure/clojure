/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Sep 13, 2007 */

package clojure.lang;

public class MultiFn extends AFn{
final public IFn dispatchFn;
final public Object defaultDispatchVal;
final public IPersistentMap methodTable;

public MultiFn(IFn dispatchFn, Object defaultDispatchVal){
	this.dispatchFn = dispatchFn;
	this.defaultDispatchVal = defaultDispatchVal;
	this.methodTable = PersistentHashMap.EMPTY;
}

public MultiFn assoc(Object dispatchVal, IFn method){
	return new MultiFn(meta(), dispatchFn, defaultDispatchVal, methodTable.assoc(dispatchVal, method));
}


public MultiFn dissoc(Object dispatchVal) throws Exception{
	return new MultiFn(meta(), dispatchFn,  defaultDispatchVal, methodTable.without(dispatchVal));
}

public Obj withMeta(IPersistentMap meta){
	if(meta == meta())
		return this;
	return new MultiFn(meta, dispatchFn, defaultDispatchVal, methodTable);
}

private MultiFn(IPersistentMap meta, IFn dispatchFn, Object defaultDispatchVal, IPersistentMap dispatchTable){
	super(meta);
	this.dispatchFn = dispatchFn;
	this.defaultDispatchVal = defaultDispatchVal;
	this.methodTable = dispatchTable;
}

private IFn getFn(Object dispatchVal) throws Exception{
	IFn targetFn = (IFn) methodTable.valAt(dispatchVal);
	if(targetFn == null)
	    targetFn = (IFn) methodTable.valAt(defaultDispatchVal);
	if(targetFn == null)
		throw new IllegalArgumentException(String.format("No method for dispatch value: %s", dispatchVal));
	return targetFn;
}

public Object invoke(Object arg1) throws Exception{
	return getFn(dispatchFn.invoke(arg1)).invoke(arg1);
}

public Object invoke(Object arg1, Object arg2) throws Exception{
	return getFn(dispatchFn.invoke(arg1, arg2)).invoke(arg1, arg2);
}

public Object invoke(Object arg1, Object arg2, Object arg3) throws Exception{
	return getFn(dispatchFn.invoke(arg1, arg2, arg3)).invoke(arg1, arg2, arg3);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4) throws Exception{
	return getFn(dispatchFn.invoke(arg1, arg2, arg3, arg4)).invoke(arg1, arg2, arg3, arg4);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5) throws Exception{
	return getFn(dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5)).invoke(arg1, arg2, arg3, arg4, arg5);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6) throws Exception{
	return getFn(dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6)).invoke(arg1, arg2, arg3, arg4, arg5, arg6);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7)
		throws Exception{
	return getFn(dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7))
			.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8) throws Exception{
	return getFn(dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)).
			invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9) throws Exception{
	return getFn(dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)).
			invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10) throws Exception{
	return getFn(dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)).
			invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11) throws Exception{
	return getFn(dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)).
			invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12) throws Exception{
	return getFn(dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)).
			invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13) throws Exception{
	return getFn(dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)).
			invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14)
		throws Exception{
	return getFn(
			dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14)).
			invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15) throws Exception{
	return getFn(
			dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                  arg15))
			.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16) throws Exception{
	return getFn(
			dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                  arg15, arg16))
			.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			        arg15, arg16);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17) throws Exception{
	return getFn(
			dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                  arg15, arg16, arg17))
			.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			        arg15, arg16, arg17);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18) throws Exception{
	return getFn(
			dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                  arg15, arg16, arg17, arg18)).
			invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			       arg15, arg16, arg17, arg18);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19) throws Exception{
	return getFn(
			dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                  arg15, arg16, arg17, arg18, arg19)).
			invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			       arg15, arg16, arg17, arg18, arg19);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20)
		throws Exception{
	return getFn(
			dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                  arg15, arg16, arg17, arg18, arg19, arg20)).
			invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			       arg15, arg16, arg17, arg18, arg19, arg20);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20, Object... args)
		throws Exception{
	return getFn(
			dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			                  arg15, arg16, arg17, arg18, arg19, arg20, args)).
			invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
			       arg15, arg16, arg17, arg18, arg19, arg20, args);
}

}
