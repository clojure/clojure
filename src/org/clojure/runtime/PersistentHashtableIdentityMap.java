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

public class PersistentHashtableIdentityMap extends PersistentHashtableMap {

public PersistentHashtableIdentityMap(int initialCapacity) {
    super(initialCapacity);
}

public PersistentHashtableIdentityMap(Object[] init) {
    super(init);
}

PersistentHashtableIdentityMap(int count, PersistentArray array) {
    super(count, array);
}

PersistentHashtableIdentityMap(int i, PersistentArray newArray, int growAtCount) {
    super(i, newArray, growAtCount);
}


public Iterator<IMapEntry> iterator() {
    return new Iter(array);
}


static class Iter implements Iterator{
    PersistentArray buckets;
    int b;
    PersistentListIdentityMap e;

    Iter(PersistentArray buckets){
        this.buckets = buckets;
        this.b = -1;
        nextBucket();
    }

    private void nextBucket() {
        e = null;
        for(b = b+1;b<buckets.length();b++)
            {
            PersistentListIdentityMap a = (PersistentListIdentityMap) buckets.get(b);
            if(a != null && a != PersistentListIdentityMap.EMPTY)
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
        PersistentListIdentityMap ret = e;
        e = e.rest();
        if(e == PersistentListIdentityMap.EMPTY)
            nextBucket();
        return ret;
    }

    public void remove() {
        throw new UnsupportedOperationException();
    }
}

IPersistentMap create(int capacity) {
    return new PersistentHashtableIdentityMap(capacity);
}

IPersistentMap create(int count, PersistentArray array) {
    return new PersistentHashtableIdentityMap(count, array);
}

IPersistentMap create(int i, PersistentArray newArray, int growAtCount){
	return new PersistentHashtableIdentityMap(i, newArray, growAtCount);
}

IPersistentMap createListMap(Object key, Object val){
    return PersistentListIdentityMap.create(key,val);
}

}
