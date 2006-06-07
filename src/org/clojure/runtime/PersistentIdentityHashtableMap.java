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

public class PersistentIdentityHashtableMap extends PersistentHashtableMap {

public PersistentIdentityHashtableMap(int initialCapacity) {
    super(initialCapacity);
}

public PersistentIdentityHashtableMap(Object[] init) {
    super(init);
}

PersistentIdentityHashtableMap(int count, PersistentArray array) {
    super(count, array);
}


public Iterator<IMapEntry> iterator() {
    return new Iter(array);
}


static class Iter implements Iterator{
    PersistentArray buckets;
    int b;
    PersistentIdentityListMap e;

    Iter(PersistentArray buckets){
        this.buckets = buckets;
        this.b = -1;
        nextBucket();
    }

    private void nextBucket() {
        e = null;
        for(b = b+1;b<buckets.length();b++)
            {
            PersistentIdentityListMap a = (PersistentIdentityListMap) buckets.get(b);
            if(a != null && a != PersistentIdentityListMap.EMPTY)
                {
                e = a;
                break;
                }
            }
    }

    public boolean hasNext() {
        return e != null;
    }

    public Object next() {
        PersistentIdentityListMap ret = e;
        e = e.rest();
        if(e == PersistentIdentityListMap.EMPTY)
            nextBucket();
        return ret;
    }

    public void remove() {
        throw new UnsupportedOperationException();
    }
}

IPersistentMap create(int capacity) {
    return new PersistentIdentityHashtableMap(capacity);
}

IPersistentMap create(int count, PersistentArray array) {
    return new PersistentIdentityHashtableMap(count, array);
}

IPersistentMap createListMap(Object key, Object val){
    return PersistentIdentityListMap.create(key,val);
}

}
