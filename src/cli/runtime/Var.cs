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
public class Var :  AFn
    {
public readonly Symbol sym;public Namespace ns;public Box binding;public IFn fn;  //todo, bind to throw stub?public IFn setfn;internal Var(Symbol sym, Namespace ns)	{	if(sym.GetType() != typeof(Symbol))	    throw new ArgumentException("Only simple symbols can be vars");	this.ns = ns;	this.sym = sym;	}public String toString()	{	if(ns == null)		return "#:" + sym;	return ns.name + ":" + sym;	}public Var bind(Object val)	{	if(binding == null)		binding = new Box(val);	else		binding.val = val;

    if (val is IFn)
        this.fn = (IFn)val;
    else
        this.fn = null; //todo, bind to throw stub?	return this;	}public Box getBinding(ThreadLocalData tld)	{	Box b = getDynamicBinding(tld);	if(b != null)		return b;	return binding;	}public Object getValue(ThreadLocalData tld)	{	Box binding = getBinding(tld);	if(binding != null)		return binding.val;	throw new InvalidOperationException(this.toString() + " is unbound.");	}public Object setValue(ThreadLocalData tld, Object val)	{	Box b = getDynamicBinding(tld);	if(b != null)		return b.val = val;	//allow global set to create binding like this?	if(binding == null)
        throw new InvalidOperationException(this.toString() + " is unbound.");	if(val is IFn)		this.fn = (IFn) val;	else		this.fn = null; //todo, bind to throw stub?	return binding.val = val;	}public Box getDynamicBinding(ThreadLocalData tld)	{	return (Box) tld.dynamicBindings[this];	}public Box establishDynamicBinding(ThreadLocalData tld, Object val)	{	Box ret = getDynamicBinding(tld);	tld.dynamicBindings[this] =  new Box(val);	return ret;	}public void restoreDynamicBinding(ThreadLocalData tld, Box old)	{	tld.dynamicBindings[this] = old;
	}override public Object invoke() /*throws Exception*/	{	return fn.invoke();	}override public Object invoke( Object arg1) /*throws Exception*/	{	return fn.invoke(arg1);	}override public Object invoke( Object arg1, Object arg2) /*throws Exception*/	{	return fn.invoke(arg1,arg2);	}override public Object invoke( Object arg1, Object arg2, Object arg3) /*throws Exception*/	{	return fn.invoke(arg1,arg2,arg3);	}override public Object invoke( Object arg1, Object arg2, Object arg3, Object arg4) /*throws Exception*/	{	return fn.invoke(arg1,arg2,arg3,arg4);	}override public Object invoke( Object arg1, Object arg2, Object arg3, Object arg4, Object arg5)		/*throws Exception*/	{	return fn.invoke(arg1,arg2,arg3,arg4,arg5);	}override public Object invoke( Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, ISeq args)		/*throws Exception*/	{	return fn.invoke(arg1,arg2,arg3,arg4,arg5,args);	}    }
}
