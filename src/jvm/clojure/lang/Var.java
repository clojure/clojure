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

package clojure.lang;

import java.util.concurrent.atomic.AtomicReference;

public class Var extends AFn {

public final Symbol sym;
public Namespace namespace;
public Binding binding;
public IFn fn;  //todo, bind to throw stub?
public IFn setfn;
AtomicReference<IPersistentMap> threadBindings = new AtomicReference(PersistentArrayIdentityMap.EMPTY);

Var(Symbol sym, Namespace ns) {
    if (!(sym.getClass() == Symbol.class))
        throw new IllegalArgumentException("Only simple symbols can be vars");
    this.namespace = ns;
    this.sym = sym;
}

public String toString() {
    if (namespace == null)
        return "#:" + sym;
    return namespace.name + ":" + sym;
}

public Var bind(Object val) {
    if (binding == null)
        binding = new Binding(val);
    else
        binding.val = val;

    if (val instanceof IFn)
        this.fn = (IFn) val;
    else
        this.fn = null; //todo, bind to throw stub?

    return this;
}

public Object getValue() {
    Binding binding = getBinding();
    if (binding != null)
        return binding.val;
    throw new IllegalStateException(this.toString() + " is unbound.");
}

public Object setValue(Object val) {
    Binding b = getThreadBinding();
    if (b != null)
        return b.val = val;
    if (binding == null)
        throw new IllegalStateException(this.toString() + " is unbound.");

    if (val instanceof IFn)
        this.fn = (IFn) val;
    else
        this.fn = null; //todo, bind to throw stub?

    return binding.val = val;
}

public Binding pushThreadBinding(Object val) {
    Binding ret = new Binding(val, getThreadBinding());
    setThreadBinding(ret);
    return ret;
}

public void popThreadBinding() {
    setThreadBinding(getThreadBinding().rest);
}

private Binding getThreadBinding() {
    if (threadBindings.get().count() > 0)
        return (Binding) threadBindings.get().get(Thread.currentThread());
    return null;
}

private Binding getBinding() {
    Binding b = getThreadBinding();
    if (b != null)
        return b;
    return binding;
}

private void setThreadBinding(Binding b) {
    Thread thread = Thread.currentThread();
    IPersistentMap tb;
    IPersistentMap newtb;
    do
        {
        tb = threadBindings.get();
        if (b == null)
            newtb = tb.remove(thread);
        else
            newtb = tb.put(thread, b);
        } while (!threadBindings.compareAndSet(tb, newtb));
}

public Object invoke() throws Exception {
    return fn.invoke();
}

public Object invoke(Object arg1) throws Exception {
    return fn.invoke(arg1);
}

public Object invoke(Object arg1, Object arg2) throws Exception {
    return fn.invoke(arg1, arg2);
}

public Object invoke(Object arg1, Object arg2, Object arg3) throws Exception {
    return fn.invoke(arg1, arg2, arg3);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4) throws Exception {
    return fn.invoke(arg1, arg2, arg3, arg4);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5)
        throws Exception {
    return fn.invoke(arg1, arg2, arg3, arg4, arg5);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object... args)
        throws Exception {
    return fn.invoke(arg1, arg2, arg3, arg4, arg5, args);
}

}
