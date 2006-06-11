/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

using System;

namespace clojure.lang
{
public class Accessor :Symbol, IFn
    {
	String memberName;	internal Accessor(String name) :base(name)	{	memberName = name.Substring(1);	}

public Object invoke() /*throws Exception*/ {
    return AFn.throwArity();
    }/** *  Indexer implements IFn for attr access *  This single arg version is the getter * @param tld * @param obj - must be AMap * @return the value of the attr or nil if not found * @throws Exception */ public Object invoke( Object obj) //throws Exception	{

	return Reflector.invokeInstanceMember(memberName, obj);	}/** *  Indexer implements IFn for attr access *  This two arg version is the setter * @param tld * @param obj - must be AMap * @param val * @return val * @throws Exception */ public Object invoke( Object obj, Object val) //throws Exception	{	return Reflector.invokeInstanceMember(memberName,obj,val);	} public Object invoke( Object arg1, Object arg2, Object arg3) //throws Exception	{	return Reflector.invokeInstanceMember(memberName,arg1,arg2,arg3);	} public Object invoke( Object arg1, Object arg2, Object arg3, Object arg4) //throws Exception	{	return Reflector.invokeInstanceMember(memberName,arg1,arg2,arg3,arg4);	} public Object invoke( Object arg1, Object arg2, Object arg3, Object arg4, Object arg5)		//throws Exception	{	return Reflector.invokeInstanceMember(memberName,arg1,arg2,arg3,arg4,arg5);	} public Object invoke( Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, ISeq args)		//throws Exception	{	return Reflector.invokeInstanceMember(memberName,arg1,arg2,arg3,arg4,arg5,args);	}

public Object applyTo( ISeq arglist) /*throws Exception*/ {
    return AFn.applyToHelper(this, arglist);
    }
	    }
}
