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

public class Cycle extends ASeq implements IReduce, IPending {

private final ISeq all;      // never null
private final ISeq prev;
private volatile ISeq _current; // lazily realized
private volatile ISeq _next;  // cached

private Cycle(ISeq all, ISeq prev, ISeq current){
    this.all = all;
    this.prev = prev;
    this._current = current;
}

private Cycle(IPersistentMap meta, ISeq all, ISeq prev, ISeq current, ISeq next){
    super(meta);
    this.all = all;
    this.prev = prev;
    this._current = current;
    this._next = next;
}

public static ISeq create(ISeq vals){
    if(vals == null)
        return PersistentList.EMPTY;
    return new Cycle(vals, null, vals);
}

// realization for use of current
private ISeq current() {
    if(_current == null) {
        ISeq current = prev.next();
        _current = (current == null) ? all : current;
    }
    return _current;
}

public boolean isRealized() {
    return _current != null;
}

public Object first(){
    return current().first();
}

public ISeq next(){
    if(_next == null)
        _next = new Cycle(all, current(), null);
    return _next;
}

public Cycle withMeta(IPersistentMap meta){
    if(meta() == meta)
        return this;
    return new Cycle(meta, all, prev, _current, _next);
}

public Object reduce(IFn f){
    ISeq s = current();
    Object ret = s.first();
    while(true) {
        s = s.next();
        if(s == null)
            s = all;
        ret = f.invoke(ret, s.first());
        if(RT.isReduced(ret))
            return ((IDeref)ret).deref();
    }
}

public Object reduce(IFn f, Object start){
    Object ret = start;
    ISeq s = current();
    while(true){
        ret = f.invoke(ret, s.first());
        if(RT.isReduced(ret))
            return ((IDeref)ret).deref();
        s = s.next();
        if(s == null)
            s = all;
    }
}
}
