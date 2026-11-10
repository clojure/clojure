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
MethodHandle ifnmh;
IFn faultFn;

static final int NSTATS = 8;
volatile IFn[] fns;
volatile long[] counts;
volatile boolean sampling = false;
volatile long faultCount = 0;
volatile long sampleWindow = 1000;

static final long MAX_RUN_WINDOW = 10000000;
volatile long runWindow = 1000; //will grow

public VarCallSite(MethodType mt, MethodHandle ifnmh, IFn init){
    super(mt);
    this.ifnmh  = ifnmh;
    IFn cacheFn = new AFn(){
        public Object invoke(Object invokeFn){
            maybeCache((IFn) invokeFn);
            return null;
        }
    };
    this.faultFn = (IFn) init.invoke(cacheFn);
    setTarget(invoker(faultFn));
}

private final MethodHandle invoker(Object invokeFn){
    return ifnmh.bindTo(invokeFn);
}

private final void maybeCache(IFn invokeFn){
    if(faultCount++ == 0){ //first fault starts as target
        setTarget(invoker(invokeFn));
    }
    else if(sampling) {
        if(fns == null){
            fns = new IFn[NSTATS];
            counts = new long[NSTATS];
        }
        //if sampling is over, pick a winner and install
        if(faultCount >= sampleWindow){
            faultCount = 0;
            sampling = false;
            int winner = 0;
            for(int i = 0;i<NSTATS;i++){
                if(fns[i] == null)
                    break;
                if(counts[i] > counts[winner])
                    winner = i;
            }
            setTarget(invoker(fns[winner]));
        }
        else { //add to count for fn
            int min = 0;
            boolean found = false;
            for(int i = 0;i<NSTATS;i++){
                if(fns[i] == null){
                    min = i;
                    break;
                }
                if(fns[i].getClass() == invokeFn.getClass()){
                    counts[i]++;
                    found = true;
                }
            }
            if(!found){
                counts[min] = 1;
                fns[min] = invokeFn;
            }
        }
    }
    else {
        //if we've seen enough faults, start sampling
        if(faultCount >= runWindow){
            if(runWindow < MAX_RUN_WINDOW)
                runWindow *= 2;
            faultCount = 0;
            sampling = true;
        }
    }
}

private static Object basis(IFn invokeFn){
    return invokeFn.getClass();
}

public static CallSite bootstrap(MethodHandles.Lookup lookup, String instruction, MethodType type,
                                    String varName, int arity) {
    MethodType mt = MethodType.genericMethodType(arity);
    try {
        MethodHandle ifnmh = lookup.findVirtual(IFn.class, "invoke", mt);
        Symbol sym = Symbol.intern(varName);
        IFn var = Var.intern(Symbol.intern(sym.ns), Symbol.intern(sym.name));
        IFn init = (IFn) var.invoke(); //no-arg call returns (f rebinder)->faultFn
        return new VarCallSite(mt,ifnmh,init);
    } catch(Exception e) {
        throw new RuntimeException(e);
    }
}
}
