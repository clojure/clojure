/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

using System;
using System.Collections;

namespace clojure.lang
{

public class PersistentHybridMap : IPersistentMap{

IPersistentMap impl;
static readonly int CAPACITY_THRESHOLD = 42;

public PersistentHybridMap(Object[] init){
    if(init.Length/2 < CAPACITY_THRESHOLD)
        impl = createArrayMap(init);
    impl = createHashtableMap(init);
}

public PersistentHybridMap(int initialCapacity){
    if(initialCapacity < CAPACITY_THRESHOLD)
        impl = createArrayMap();
	else
        impl = createHashtableMap(initialCapacity);
}

internal PersistentHybridMap(IPersistentMap impl){
    this.impl = impl;
}

public int count() {
    return impl.count();
}

public bool contains(Object key) {
    return impl.contains(key);
}

public IMapEntry find(Object key) {
    return impl.find(key);
}

public IPersistentMap add(Object key) {
    return put(key, null);
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

public IEnumerator GetEnumerator() {
    return impl.GetEnumerator();
}

virtual public IPersistentMap create(IPersistentMap impl) {
    return new PersistentHybridMap(impl);
}

virtual public PersistentArrayMap createArrayMap(Object[] init) {
    return new PersistentArrayMap(init);
}

virtual internal IPersistentMap createArrayMap() {
    return PersistentArrayMap.EMPTY;
}

virtual internal IPersistentMap createHashtableMap(Object[] init) {
    return new PersistentHashtableMap(init);
}

virtual internal IPersistentMap createHashtableMap(int initialCapacity) {
    return new PersistentHashtableMap(initialCapacity);
}


#region ISequential Members

public ISeq seq()
	{
	return impl.seq();
	}

#endregion
	}

}
