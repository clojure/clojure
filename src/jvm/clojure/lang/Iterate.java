/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

package clojure.lang;

/* Alex Miller, Dec 5, 2014 */

public class Iterate extends ASeq implements IReduce {

private final IFn f;      // never null
private final Object seed;
private volatile ISeq _next;  // cached

private Iterate(IFn f, Object seed){
    this.f = f;
    this.seed = seed;
}

private Iterate(IPersistentMap meta, IFn f, Object seed){
    super(meta);
    this.f = f;
    this.seed = seed;
}

public static ISeq create(IFn f, Object seed){
    return new Iterate(f, seed);
}

public Object first(){
    return seed;
}

public ISeq next(){
    if(_next == null) {
        _next = new Iterate(f, f.invoke(seed));
    }
    return _next;
}

public Iterate withMeta(IPersistentMap meta){
    return new Iterate(meta, f, seed);
}

public Object reduce(IFn rf){
    Object ret = seed;
    Object v = f.invoke(seed);
    while(true){
        ret = rf.invoke(ret, v);
        if(RT.isReduced(ret))
            return ((IDeref)ret).deref();
        v = f.invoke(v);
    }
}

public Object reduce(IFn rf, Object start){
    Object ret = start;
    Object v = seed;
    while(true){
        ret = rf.invoke(ret, v);
        if(RT.isReduced(ret))
            return ((IDeref)ret).deref();
        v = f.invoke(v);
    }
}
}
