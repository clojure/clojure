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
public class Var :  Indexer
    {
public String name;public Namespace ns;public Cons binding;public IFn fn;  //todo, bind to throw stub?public IFn setfn;internal Var(String name, Namespace ns)	{	this.ns = ns;	this.name = name;	}public String toString()	{	if(ns == null)		return "#:" + name;	return ns.name + ":" + name;	}public Var bind(Object val)	{	if(binding == null)		binding = new Cons(val,null);	else		binding.first = val;	return this;	}public Cons getBinding(ThreadLocalData tld)	{	Cons b = getDynamicBinding(tld);	if(b != null)		return b;	return binding;	}public Object getValue(ThreadLocalData tld)	{	Cons binding = getBinding(tld);	if(binding != null)		return binding.first;	throw new InvalidOperationException(this.toString() + " is unbound.");	}public Object setValue(ThreadLocalData tld, Object val)	{	Cons b = getDynamicBinding(tld);	if(b != null)		return b.first = val;	//allow global set to create binding like this?	if(binding == null)
        throw new InvalidOperationException(this.toString() + " is unbound.");	if(val is IFn)		this.fn = (IFn) val;	else		this.fn = null; //todo, bind to throw stub?	return binding.first = val;	}public Cons getDynamicBinding(ThreadLocalData tld)	{	return (Cons) tld.dynamicBindings[this];	}public Cons pushDynamicBinding(ThreadLocalData tld, Object val)	{	Cons ret = new Cons(val, getDynamicBinding(tld));	tld.dynamicBindings[this] =  ret;	return ret;	}public Cons popDynamicBinding(ThreadLocalData tld)	{    Cons oldb = getDynamicBinding(tld).rest;	tld.dynamicBindings[this] = oldb;
    return oldb;	}override public Object invoke(ThreadLocalData tld) /*throws Exception*/	{	return fn.invoke(tld);	}override public Object invoke(ThreadLocalData tld, Object arg1) /*throws Exception*/	{	return fn.invoke(tld,arg1);	}override public Object invoke(ThreadLocalData tld, Object arg1, Object arg2) /*throws Exception*/	{	return fn.invoke(tld,arg1,arg2);	}override public Object invoke(ThreadLocalData tld, Object arg1, Object arg2, Object arg3) /*throws Exception*/	{	return fn.invoke(tld,arg1,arg2,arg3);	}override public Object invoke(ThreadLocalData tld, Object arg1, Object arg2, Object arg3, Object arg4) /*throws Exception*/	{	return fn.invoke(tld,arg1,arg2,arg3,arg4);	}override public Object invoke(ThreadLocalData tld, Object arg1, Object arg2, Object arg3, Object arg4, Object arg5)		/*throws Exception*/	{	return fn.invoke(tld,arg1,arg2,arg3,arg4,arg5);	}override public Object invoke(ThreadLocalData tld, Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Cons args)		/*throws Exception*/	{	return fn.invoke(tld,arg1,arg2,arg3,arg4,arg5,args);	}    }
}
