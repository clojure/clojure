/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

package org.clojure.runtime;

import java.util.Iterator;

public class PersistentHashMap implements IMap, Iterable{

IMap impl;
static final int CAPACITY_THRESHOLD = 10;

public PersistentHashMap(Object[] init){
    if(init.length/2 < CAPACITY_THRESHOLD)
        impl = createArrayMap(init);
    impl = createHashtableMap(init);
}

public PersistentHashMap(int initialCapacity){
    if(initialCapacity < CAPACITY_THRESHOLD)
        impl = createArrayMap();
    impl = createHashtableMap(initialCapacity);
}

PersistentHashMap(IMap impl){
    this.impl = impl;
}

public int count() {
    return impl.count();
}

public boolean contains(Object key) {
    return impl.contains(key);
}

public IMapEntry find(Object key) {
    return impl.find(key);
}

public IMap add(Object key) {
    return put(key, null);
}

public IMap put(Object key, Object val) {
    IMap newImpl = impl.put(key,val);
    if(newImpl.capacity() == CAPACITY_THRESHOLD)
        {
        newImpl = createHashtableMap(((ArrayMap)newImpl).array);
        }
    return create(newImpl);
}

public IMap remove(Object key) {
    IMap newImpl = impl.remove(key);
    if(newImpl != impl)
        return create(newImpl);
    return this;
}

public Object get(Object key) {
    return impl.get(key);
}

public int capacity() {
    return impl.capacity();
}

public Iterator iterator() {
    return ((Iterable)impl).iterator();
}

public IMap create(IMap impl) {
    return new PersistentHashMap(impl);
}

public ArrayMap createArrayMap(Object[] init) {
    return new ArrayMap(init);
}

private IMap createArrayMap() {
    return new ArrayMap();
}

private IMap createHashtableMap(Object[] init) {
    return new HashtableMap(init);
}

private IMap createHashtableMap(int initialCapacity) {
    return new HashtableMap(initialCapacity);
}

}
