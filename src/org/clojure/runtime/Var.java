/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Apr 19, 2006 */

package org.clojure.runtime;

public class Var extends AFn{

public final String name;
public Namespace namespace;
public Cons binding;
public IFn fn;  //todo, bind to throw stub?
public IFn setfn;

Var(String name, Namespace ns)
	{
	this.namespace = ns;
	this.name = name;
	}

public String toString()
	{
	if(namespace == null)
		return "#:" + name;
	return namespace.name + ":" + name;
	}

public Var bind(Object val)
	{
	if(binding == null)
		binding = new Cons(val,null);
	else
		binding.first = val;

	if(val instanceof IFn)
		this.fn = (IFn) val;
	else
		this.fn = null; //todo, bind to throw stub?
        
    return this;
	}

public Cons getBinding(ThreadLocalData tld)
	{
	Cons b = getDynamicBinding(tld);
	if(b != null)
		return b;
	return binding;
	}

public Object getValue(ThreadLocalData tld)
	{
	Cons binding = getBinding(tld);
	if(binding != null)
		return binding.first;
	throw new IllegalStateException(this.toString() + " is unbound.");
	}

public Object setValue(ThreadLocalData tld, Object val)
	{
	Cons b = getDynamicBinding(tld);
	if(b != null)
		return b.first = val;
	//allow global set to create binding like this?
	if(binding == null)
		throw new IllegalStateException(this.toString() + " is unbound.");

	if(val instanceof IFn)
		this.fn = (IFn) val;
	else
		this.fn = null; //todo, bind to throw stub?

	return binding.first = val;
	}

final public Cons getDynamicBinding(ThreadLocalData tld)
	{
	return (Cons) tld.dynamicBindings.get(this);
	}

final public Cons pushDynamicBinding(ThreadLocalData tld, Object val)
	{
	Cons ret = new Cons(val, getDynamicBinding(tld));
	tld.dynamicBindings.put(this, ret);
	return ret;
	}

final public Cons popDynamicBinding(ThreadLocalData tld)
	{
	return (Cons) tld.dynamicBindings.put(this, getDynamicBinding(tld).rest);
	}

public Object invoke(ThreadLocalData tld) throws Exception
	{
	return fn.invoke(tld);
	}

public Object invoke(ThreadLocalData tld, Object arg1) throws Exception
	{
	return fn.invoke(tld,arg1);
	}

public Object invoke(ThreadLocalData tld, Object arg1, Object arg2) throws Exception
	{
	return fn.invoke(tld,arg1,arg2);
	}

public Object invoke(ThreadLocalData tld, Object arg1, Object arg2, Object arg3) throws Exception
	{
	return fn.invoke(tld,arg1,arg2,arg3);
	}

public Object invoke(ThreadLocalData tld, Object arg1, Object arg2, Object arg3, Object arg4) throws Exception
	{
	return fn.invoke(tld,arg1,arg2,arg3,arg4);
	}

public Object invoke(ThreadLocalData tld, Object arg1, Object arg2, Object arg3, Object arg4, Object arg5)
		throws Exception
	{
	return fn.invoke(tld,arg1,arg2,arg3,arg4,arg5);
	}

public Object invoke(ThreadLocalData tld, Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Cons args)
		throws Exception
	{
	return fn.invoke(tld,arg1,arg2,arg3,arg4,arg5,args);
	}

}
