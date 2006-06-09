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

public final Symbol sym;
public Namespace namespace;
public Box binding;
public IFn fn;  //todo, bind to throw stub?
public IFn setfn;

Var(Symbol sym, Namespace ns)
	{
    if(!(sym.getClass() == Symbol.class))
        throw new IllegalArgumentException("Only simple symbols can be vars");
    this.namespace = ns;
	this.sym = sym;
	}

public String toString()
	{
	if(namespace == null)
		return "#:" + sym;
	return namespace.name + ":" + sym;
	}

public Var bind(Object val)
	{
	if(binding == null)
		binding = new Box(val);
	else
		binding.val = val;

	if(val instanceof IFn)
		this.fn = (IFn) val;
	else
		this.fn = null; //todo, bind to throw stub?
        
    return this;
	}

public Box getBinding(ThreadLocalData tld)
	{
	Box b = getDynamicBinding(tld);
	if(b != null)
		return b;
	return binding;
	}

public Object getValue(ThreadLocalData tld)
	{
	Box binding = getBinding(tld);
	if(binding != null)
		return binding.val;
	throw new IllegalStateException(this.toString() + " is unbound.");
	}

public Object setValue(ThreadLocalData tld, Object val)
	{
	Box b = getDynamicBinding(tld);
	if(b != null)
		return b.val = val;
	//allow global set to create binding like this?
	if(binding == null)
		throw new IllegalStateException(this.toString() + " is unbound.");

	if(val instanceof IFn)
		this.fn = (IFn) val;
	else
		this.fn = null; //todo, bind to throw stub?

	return binding.val = val;
	}

final public Box getDynamicBinding(ThreadLocalData tld)
	{
	return (Box) tld.dynamicBindings.get(this);
	}

final public Box establishDynamicBinding(ThreadLocalData tld, Object val)
	{
	Box ret = getDynamicBinding(tld);
	tld.dynamicBindings.put(this, new Box(val));
	return ret;
	}

final public void restoreDynamicBinding(ThreadLocalData tld, Box old)
	{
	tld.dynamicBindings.put(this, old);
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

public Object invoke(ThreadLocalData tld, Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, ISeq args)
		throws Exception
	{
	return fn.invoke(tld,arg1,arg2,arg3,arg4,arg5,args);
	}

}
