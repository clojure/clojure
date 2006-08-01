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

using System;


namespace clojure.lang
{

public class Keyword : Symbol, IFn{


internal Keyword(String name):base(name)	{	}public Object invoke() /*throws Exception*/ {
    return AFn.throwArity();
}/** *  Indexer implements IFn for attr access *  This single arg version is the getter * @param tld * @param obj - must be AMap * @return the value of the attr or nil if not found */public Object invoke( Object obj) /*throws Exception*/	{
    if (obj == null)
        return null;
	return ((IPersistentMap)obj).get(this);	}/** *  Indexer implements IFn for attr access *  This two arg version is the setter * @param tld * @param obj - must be AMap * @param val * @return val */public Object invoke( Object obj, Object val) /*throws Exception*/	{
	return ((IPersistentMap)obj).put(this, val);	}

public Object invoke(Object arg1, Object arg2, Object arg3)
	{
	return AFn.throwArity();
	}
public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4)
	{
	return AFn.throwArity();
	}
public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5)
	{
	return AFn.throwArity();
	}
public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6)
	{
	return AFn.throwArity();
	}
public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7)
	{
	return AFn.throwArity();
	}
public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8)
	{
	return AFn.throwArity();
	}
public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9)
	{
	return AFn.throwArity();
	}
public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10)
	{
	return AFn.throwArity();
	}
public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11)
	{
	return AFn.throwArity();
	}
public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11, Object arg12)
	{
	return AFn.throwArity();
	}
public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13)
	{
	return AFn.throwArity();
	}
public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14)
	{
	return AFn.throwArity();
	}
public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14, Object arg15)
	{
	return AFn.throwArity();
	}
public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14, Object arg15, Object arg16)
	{
	return AFn.throwArity();
	}
public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14, Object arg15, Object arg16, Object arg17)
	{
	return AFn.throwArity();
	}
public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14, Object arg15, Object arg16, Object arg17, Object arg18)
	{
	return AFn.throwArity();
	}
public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14, Object arg15, Object arg16, Object arg17, Object arg18, Object arg19)
	{
	return AFn.throwArity();
	}
public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14, Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20)
	{
	return AFn.throwArity();
	}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14, Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20
					, params Object[] args)
	{
	return AFn.throwArity();
	}

public Object applyTo( ISeq arglist) /*throws Exception*/ {
    return AFn.applyToHelper(this,  arglist);
}
}
}