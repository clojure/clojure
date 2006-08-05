/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

package clojure.lang;

public abstract class AnArray extends Obj implements IArray, Cloneable {

public IPersistentCollection cons(Object o) {
    PersistentArrayList ret = new PersistentArrayList(this, this.count() + 10);
    ret = ret.cons(o);
    ret._meta = _meta;
    return ret;
}

public Obj withMeta(IPersistentMap meta) {
    if(_meta == meta)
        return this;
    try{
    Obj ret = (Obj) clone();
    ret._meta = meta;
    return ret;
    }
    catch(CloneNotSupportedException ignore)
        {
        return null;
        }
}

public boolean contains(Object key) {
    if(!(key instanceof Number))
        return false;
    int i = ((Number)key).intValue();
    return i >= 0 && i < count();
}

public IMapEntry find(Object key) {
    if(key instanceof Number)
        {
        int i = ((Number)key).intValue();
        if(i >= 0 && i < count())
            return new MapEntry(key,nth(i));
        }
    return null;
}

public Associative assoc(Object key, Object val) {
    if(key instanceof Number)
        {
        int i = ((Number)key).intValue();
        return (Associative) assocN(i,val);
        }
    throw new IllegalAccessError("Key must be integer");
}

public Object get(Object key) {
    if(key instanceof Number)
        {
        int i = ((Number)key).intValue();
        if(i >= 0 && i < count())
            return nth(i);
        }
    return null;
}
}
