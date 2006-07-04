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

import java.util.Iterator;

public class PersistentHybridMap implements IPersistentMap{

IPersistentMap impl;
static final int CAPACITY_THRESHOLD = 42;

public PersistentHybridMap(Object[] init){
    if(init.length/2 < CAPACITY_THRESHOLD)
        impl = createArrayMap(init);
    impl = createHashtableMap(init);
}

public PersistentHybridMap(int initialCapacity){
    if(initialCapacity < CAPACITY_THRESHOLD)
        impl = createArrayMap();
	else
        impl = createHashtableMap(initialCapacity);
}

PersistentHybridMap(IPersistentMap impl){
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

public IPersistentMap add(Object key, Object val) throws Exception {
    IPersistentMap newImpl = impl.add(key,val);
    if(newImpl.capacity() == CAPACITY_THRESHOLD)
        {
        newImpl = createHashtableMap(((PersistentArrayMap)newImpl).array);
        }
    return create(newImpl);
}

public IPersistentMap put(Object key, Object val) {
    IPersistentMap newImpl = impl.put(key,val);
    if(newImpl.capacity() == CAPACITY_THRESHOLD)
        {
        newImpl = createHashtableMap(((PersistentArrayMap)newImpl).array);
        }
    return create(newImpl);
}

public IPersistentMap remove(Object key) {
    IPersistentMap newImpl = impl.remove(key);
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

public IPersistentMap create(IPersistentMap impl) {
    return new PersistentHybridMap(impl);
}

public PersistentArrayMap createArrayMap(Object[] init) {
    return new PersistentArrayMap(init);
}

IPersistentMap createArrayMap() {
    return PersistentArrayMap.EMPTY;
}

IPersistentMap createHashtableMap(Object[] init) {
    return new PersistentHashtableMap(init);
}

IPersistentMap createHashtableMap(int initialCapacity) {
    return new PersistentHashtableMap(initialCapacity);
}

public ISeq seq() throws Exception {
    return impl.seq();
}
}
