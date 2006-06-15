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
using System.Threading;

namespace clojure.lang
{
public class Var :  AFn
    {
public readonly Symbol sym;public Namespace ns;public Binding binding;public IFn fn;  //todo, bind to throw stub?public IFn setfn;volatile IPersistentMap threadBindings = PersistentArrayIdentityMap.EMPTY;internal Var(Symbol sym, Namespace ns)	{	if(sym.GetType() != typeof(Symbol))	    throw new ArgumentException("Only simple symbols can be vars");	this.ns = ns;	this.sym = sym;	}override public String ToString()	{	if(ns == null)		return "#:" + sym;	return ns.name + ":" + sym;	}public Var bind(Object val)	{	if(binding == null)
		binding = new Binding(val);	else		binding.val = val;

    if (val is IFn)
        this.fn = (IFn)val;
    else
        this.fn = null; //todo, bind to throw stub?	return this;	}public Object getValue()	{
	Binding binding = getBinding();	if(binding != null)		return binding.val;	throw new InvalidOperationException(this.ToString() + " is unbound.");	}public Object setValue(Object val)	{
	Binding b = getThreadBinding();	if(b != null)		return b.val = val;	if(binding == null)
        throw new InvalidOperationException(this.ToString() + " is unbound.");	if(val is IFn)		this.fn = (IFn) val;	else		this.fn = null; //todo, bind to throw stub?	return binding.val = val;	}

public Binding pushThreadBinding(Object val) {
    Binding ret = new Binding(val, getThreadBinding());
    setThreadBinding(ret);
    return ret;
}

public void popThreadBinding() {
    setThreadBinding(getThreadBinding().rest);
}

Binding getBinding()	{
	Binding b = getThreadBinding();	if(b != null)		return b;	return binding;	}
Binding getThreadBinding()	{
	if (threadBindings.count() > 0)
		return (Binding)threadBindings.get(Thread.CurrentThread);	return null;	}void setThreadBinding(Binding b) {
    Thread thread = Thread.CurrentThread;
    IPersistentMap tb;
    IPersistentMap newtb;
    do
        {
        tb = threadBindings;
        if (b == null)
            newtb = tb.remove(thread);
        else
            newtb = tb.put(thread, b);
		} while (tb != Interlocked.CompareExchange(ref threadBindings, newtb, tb));
}override public Object invoke() /*throws Exception*/	{	return fn.invoke();	}override public Object invoke( Object arg1) /*throws Exception*/	{	return fn.invoke(arg1);	}override public Object invoke( Object arg1, Object arg2) /*throws Exception*/	{	return fn.invoke(arg1,arg2);	}override public Object invoke( Object arg1, Object arg2, Object arg3) /*throws Exception*/	{	return fn.invoke(arg1,arg2,arg3);	}override public Object invoke( Object arg1, Object arg2, Object arg3, Object arg4) /*throws Exception*/	{	return fn.invoke(arg1,arg2,arg3,arg4);	}override public Object invoke( Object arg1, Object arg2, Object arg3, Object arg4, Object arg5)		/*throws Exception*/	{	return fn.invoke(arg1,arg2,arg3,arg4,arg5);	}override public Object invoke( Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, ISeq args)		/*throws Exception*/	{	return fn.invoke(arg1,arg2,arg3,arg4,arg5,args);	}    }
}
