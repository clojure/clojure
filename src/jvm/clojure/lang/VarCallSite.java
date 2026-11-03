/**
 * Copyright (c) Rich Hickey. All rights reserved.
 * The use and distribution terms for this software are covered by the
 * Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 * which can be found in the file epl-v10.html at the root of this distribution.
 * By using this software in any fashion, you are agreeing to be bound by
 * the terms of this license.
 * You must not remove this notice, or any other, from this software.
 **/

/* rich 11/2/26 */

package clojure.lang;

import java.lang.invoke.*;

public class VarCallSite extends MutableCallSite{

public VarCallSite(MethodType mt){
    super(mt);
}

public static CallSite bootstrap(MethodHandles.Lookup lookup, String instruction, MethodType type,
                                 String varName, int arity) {
    MethodType mt = MethodType.genericMethodType(arity);
    try {
        MethodHandle ifnmh = MethodHandles.lookup().findVirtual(IFn.class, "invoke", mt);
        VarCallSite vcs = new VarCallSite(mt);
        Symbol sym = Symbol.intern(varName);
        IFn var = Var.intern(Symbol.intern(sym.ns), Symbol.intern(sym.name));
        IFn rebinder = new AFn(){
            public Object invoke(Object invokeFn){
                vcs.setTarget(ifnmh.bindTo(invokeFn));
                return null;
            }
        };

        IFn init = (IFn) var.invoke(); //no-arg call returns (f rebinder)->faultFn
        rebinder.invoke(init.invoke(rebinder)); //initial invokeFn is fault
        return vcs;
        } catch(Exception e) {
            throw new RuntimeException(e);
    }

}
}
