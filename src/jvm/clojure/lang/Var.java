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
import java.util.concurrent.atomic.AtomicInteger;
import java.util.Random;

public class Var extends AFn {

public final Symbol sym;
public Namespace namespace;
public Binding binding;
AtomicInteger tcount = new AtomicInteger(0);
AtomicReference<IPersistentMap> threadBindings = new AtomicReference(PersistentArrayMap.EMPTY);

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

    return binding.val = val;
}

public Binding pushThreadBinding(Object val) {
    Binding ret = new Binding(val, getThreadBinding());
    setThreadBinding(ret);
    tcount.incrementAndGet();
    return ret;
}

public void popThreadBinding() {
    setThreadBinding(getThreadBinding().rest);
    tcount.decrementAndGet();
}

private Binding getThreadBinding() {
    if (tcount.get() != 0)
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

final public IFn fn() {
    return (IFn) getValue();
}

public Object invoke() throws Exception {
    return fn().invoke();
}

public Object invoke(Object arg1) throws Exception {
    return fn().invoke(arg1);
}

public Object invoke(Object arg1, Object arg2) throws Exception {
    return fn().invoke(arg1, arg2);
}

public Object invoke(Object arg1, Object arg2, Object arg3) throws Exception {
    return fn().invoke(arg1, arg2, arg3);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4) throws Exception {
    return fn().invoke(arg1, arg2, arg3, arg4);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5) throws Exception {
    return fn().invoke(arg1, arg2, arg3, arg4, arg5);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6) throws Exception {
    return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7)
        throws Exception {
    return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8) throws Exception {
    return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9) throws Exception {
    return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10) throws Exception {
    return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11) throws Exception {
    return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12) throws Exception {
    return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13)
        throws Exception {
    return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14)
        throws Exception {
    return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15) throws Exception {
    return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16) throws Exception {
    return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15,
                       arg16);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17) throws Exception {
    return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15,
                       arg16, arg17);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18) throws Exception {
    return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15,
                       arg16, arg17, arg18);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19) throws Exception {
    return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15,
                       arg16, arg17, arg18, arg19);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20)
        throws Exception {
    return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15,
                       arg16, arg17, arg18, arg19, arg20);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20,
                     Object... args)
        throws Exception {
    return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15,
                       arg16, arg17, arg18, arg19, arg20,args);
}


static volatile Integer o = 1;

public static void main(String[] args) {

    try
        {
        int n = Integer.parseInt(args[0]);
        class Test extends AFn {
            int x = 0;

            public Object invoke(Object arg1) throws Exception {
                x += o.intValue();
                return this;
            }

            public String toString() {
                return Integer.toString(x);
            }

        }

        Var test = Namespace.intern("test", "test");

        test.bind(new Test());

        Random rand;

        Object result = 0;

        rand = new Random(42);
        long startTime = System.nanoTime();

        for (int i = 0; i < n; i++)
            result = test.invoke(result);
        long estimatedTime = System.nanoTime() - startTime;
        System.out.println("val:" + result + ", time: " + estimatedTime / 1000000);

        rand = new Random(42);
        startTime = System.nanoTime();

        for (int i = 0; i < n; i++)
            result = ((IFn) test.getValue()).invoke(result);
        estimatedTime = System.nanoTime() - startTime;
        System.out.println("val:" + result + ", time: " + estimatedTime / 1000000);

        }

    catch (Exception e)
        {
        e.printStackTrace();  //To change body of catch statement use File | Settings | File Templates.
        }

}
}
