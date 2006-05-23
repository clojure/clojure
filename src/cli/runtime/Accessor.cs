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

namespace org.clojure.runtime
{
public class Accessor :Indexer
    {
    public  String name;    public Namespace ns;internal Accessor(String name, Namespace ns)	{	this.ns = ns;	this.name = name;	}public String toString()	{	if(ns == null)		return "#:." + name;	return ns.name + ":." + name;	}/** *  Indexer implements IFn for attr access *  This single arg version is the getter * @param tld * @param obj - must be AMap * @return the value of the attr or nil if not found * @throws Exception */override public Object invoke(ThreadLocalData tld, Object obj) //throws Exception	{
    return Reflector.invokeInstanceMember(name, obj);	}/** *  Indexer implements IFn for attr access *  This two arg version is the setter * @param tld * @param obj - must be AMap * @param val * @return val * @throws Exception */override public Object invoke(ThreadLocalData tld, Object obj, Object val) //throws Exception	{	return Reflector.invokeInstanceMember(name,obj,val);	}override public Object invoke(ThreadLocalData tld, Object arg1, Object arg2, Object arg3) //throws Exception	{	return Reflector.invokeInstanceMember(name,arg1,arg2,arg3);	}override public Object invoke(ThreadLocalData tld, Object arg1, Object arg2, Object arg3, Object arg4) //throws Exception	{	return Reflector.invokeInstanceMember(name,arg1,arg2,arg3,arg4);	}override public Object invoke(ThreadLocalData tld, Object arg1, Object arg2, Object arg3, Object arg4, Object arg5)		//throws Exception	{	return Reflector.invokeInstanceMember(name,arg1,arg2,arg3,arg4,arg5);	}override public Object invoke(ThreadLocalData tld, Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Cons args)		//throws Exception	{	return Reflector.invokeInstanceMember(name,arg1,arg2,arg3,arg4,arg5,args);	}    }
}
