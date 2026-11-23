/**
 * Copyright (c) Rich Hickey. All rights reserved.
 * The use and distribution terms for this software are covered by the
 * Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 * which can be found in the file epl-v10.html at the root of this distribution.
 * By using this software in any fashion, you are agreeing to be bound by
 * the terms of this license.
 * You must not remove this notice, or any other, from this software.
 **/

/* rich 11/9/26 */

package clojure.lang;

import java.lang.invoke.*;

public class VarCallSite extends MutableCallSite{

final static Keyword FN_K = Keyword.intern(null, "fn");
final static Keyword STATE_K = Keyword.intern(null, "state");

private final MethodHandle ifnmh;
private Object cache; //a map with :fn and optional :state

public VarCallSite(MethodType mt, MethodHandle ifnmh, IFn init){
    super(mt);
    this.ifnmh  = ifnmh;
    IFn cacheFn = new AFn(){
        public Object invoke() {
            return cache;
        }
        public Object invoke(Object cmap){
            var nextCache = cache;
            if(RT.booleanCast(RT.contains(cmap,STATE_K)))
                nextCache = RT.assoc(cache, STATE_K, RT.get(cmap,STATE_K));
            var tfn = RT.get(cmap,FN_K);
            if(tfn != null){
                setInvoker(tfn);
                nextCache = RT.assoc(nextCache, FN_K, tfn);
                }
            cache = nextCache;
            return null;
        }
    };
    this.cache = init.invoke(cacheFn);
    setInvoker(RT.get(cache, FN_K));
}

private void setInvoker(Object invokeFn){
    setTarget(ifnmh.bindTo(invokeFn));
}

public static CallSite bootstrap(MethodHandles.Lookup lookup, String instruction, MethodType type,
                                    String varName) {
    MethodType mt = type;
    try {
        MethodHandle ifnmh = lookup.findVirtual(IFn.class, "invoke", mt);
        Symbol sym = Symbol.intern(varName);
        IFn var = Var.find(sym);
        if(var == null)
            throw new IllegalArgumentException("Can't find var to bind callsite: " + sym.toString());
        IFn init = (IFn) var.invoke(); //no-arg call returns (f cachefn)->faultFn
        return new VarCallSite(mt,ifnmh,init);
    } catch(Exception e) {
        throw new RuntimeException(e);
    }
}
}
