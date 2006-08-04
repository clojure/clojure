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
public readonly Symbol sym;public Namespace ns;public Binding binding;volatile IPersistentMap threadBindings = PersistentArrayMap.EMPTY;volatile int tcount = 0;internal Var(Symbol sym, Namespace ns)	{	if(sym.GetType() != typeof(Symbol))	    throw new ArgumentException("Only simple symbols can be vars");	this.ns = ns;	this.sym = sym;	}override public String ToString()	{	if(ns == null)		return "#:" + sym;	return ns.name + ":" + sym;	}public Var bind(Object val)	{	if(binding == null)
		binding = new Binding(val);	else		binding.val = val;

	return this;	}public Object getValue()	{
	Binding binding = getBinding();	if(binding != null)		return binding.val;	throw new InvalidOperationException(this.ToString() + " is unbound.");	}public Object setValue(Object val)	{
	Binding b = getThreadBinding();	if(b != null)		return b.val = val;	if(binding == null)
        throw new InvalidOperationException(this.ToString() + " is unbound.");	return binding.val = val;	}

public Binding pushThreadBinding(Object val) {
    Binding ret = new Binding(val, getThreadBinding());
    setThreadBinding(ret);
    Interlocked.Increment(ref tcount);
    return ret;
}

public void popThreadBinding() {
    setThreadBinding(getThreadBinding().rest);
    Interlocked.Decrement(ref tcount);
}

Binding getBinding()	{
	Binding b = getThreadBinding();	if(b != null)		return b;	return binding;	}
Binding getThreadBinding()	{
	if (tcount != 0)
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
            newtb = tb.assoc(thread, b);
		} while (tb != Interlocked.CompareExchange(ref threadBindings, newtb, tb));
}public IFn fn(){	return (IFn)getValue();}override public Object invoke(){
    return fn().invoke();
}
override public Object invoke(Object arg1){
    return fn().invoke(arg1);
}
override public Object invoke(Object arg1, Object arg2){
    return fn().invoke(arg1, arg2);
}
override public Object invoke(Object arg1, Object arg2, Object arg3){
    return fn().invoke(arg1, arg2, arg3);
}
override public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4){
    return fn().invoke(arg1, arg2, arg3, arg4);
}
override public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5){
    return fn().invoke(arg1, arg2, arg3, arg4, arg5);
}
override public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6){
    return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6);
}
override public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7){
    return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}
override public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8){
    return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}
override public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9){
    return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
}
override public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10){
    return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}
override public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11){
    return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
}
override public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11, Object arg12){
    return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
}
override public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13){
    return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13);
}
override public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14){
    return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14);
}
override public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14, Object arg15){
    return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15);
}
override public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14, Object arg15, Object arg16){
    return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16);
}
override public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14, Object arg15, Object arg16, Object arg17){
    return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17);
}
override public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14, Object arg15, Object arg16, Object arg17, Object arg18){
    return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18);
}
override public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14, Object arg15, Object arg16, Object arg17, Object arg18, Object arg19){
    return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19);
}
override public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14, Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20){
    return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20);
}
override public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7, Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14, Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20
							, params Object[] args){
    return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20,args);
}
    }
}
