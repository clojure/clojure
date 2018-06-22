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

public class Iterate extends ASeq implements IReduce, IPending {

private static final Object UNREALIZED_SEED = new Object();
private final IFn f;      // never null
private final Object prevSeed;
private volatile Object _seed;  // lazily realized
private volatile ISeq _next;  // cached

private Iterate(IFn f, Object prevSeed, Object seed){
    this.f = f;
    this.prevSeed = prevSeed;
    this._seed = seed;
}

private Iterate(IPersistentMap meta, IFn f, Object prevSeed, Object seed, ISeq next){
    super(meta);
    this.f = f;
    this.prevSeed = prevSeed;
    this._seed = seed;
    this._next = next;
}

public static ISeq create(IFn f, Object seed){
    return new Iterate(f, null, seed);
}

public boolean isRealized() {
    return _seed != UNREALIZED_SEED;
}

public Object first(){
    if(_seed == UNREALIZED_SEED) {
        _seed = f.invoke(prevSeed);
    }
    return _seed;
}

public ISeq next(){
    if(_next == null) {
        _next = new Iterate(f, first(), UNREALIZED_SEED);
    }
    return _next;
}

public Iterate withMeta(IPersistentMap meta){
    if(meta() == meta)
        return this;
    return new Iterate(meta, f, prevSeed, _seed, _next);
}

public Object reduce(IFn rf){
    Object first = first();
    Object ret = first;
    Object v = f.invoke(first);
    while(true){
        ret = rf.invoke(ret, v);
        if(RT.isReduced(ret))
            return ((IDeref)ret).deref();
        v = f.invoke(v);
    }
}

public Object reduce(IFn rf, Object start){
    Object ret = start;
    Object v = first();
    while(true){
        ret = rf.invoke(ret, v);
        if(RT.isReduced(ret))
            return ((IDeref)ret).deref();
        v = f.invoke(v);
    }
}
}
