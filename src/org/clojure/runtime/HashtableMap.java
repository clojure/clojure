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
import java.math.BigInteger;

public class HashtableMap implements IMap, Iterable {

static final float FILL_FACTOR = 0.75f;

final PersistentArray array;
final int _count;
final int growAtCount;

public HashtableMap(int initialCapacity) {
    array = new PersistentArray(calcPrimeCapacity(initialCapacity));
    _count = 0;
    this.growAtCount = (int) (this.array.length()*FILL_FACTOR);
}

/**
 * @param init {key1,val1,key2,val2,...}
 */
public HashtableMap(Object[] init){
    //start halfway to a rehash
    PersistentArray narray = new PersistentArray(calcPrimeCapacity(init.length));
    for(int i=0;i<init.length;i+=2)
        {
        narray = doPut(bucketFor(init[i],narray),init[i], init[i + 1],narray);
        }
    this.array = narray;
    this._count = init.length/2; //hmmm... presumes no dupe keys in init
    this.growAtCount = (int) (this.array.length()*FILL_FACTOR);
}

HashtableMap(int count,PersistentArray array) {
    this._count = count;
    this.array = array;
    this.growAtCount = (int) (this.array.length()*FILL_FACTOR);
}

int calcPrimeCapacity(int capacity) {
    return BigInteger.valueOf((long) (capacity/FILL_FACTOR)).nextProbablePrime().intValue();
}

public int count() {
    return _count;
}

public boolean contains(Object key) {
    IMap entries = entriesFor(key);
    return entries != null && entries.contains(key);
}

public IMapEntry find(Object key) {
    IMap entries = entriesFor(key);
    if(entries != null)
        return entries.find(key);
    return null;
}

public IMap add(Object key) {
    return put(key, null);
}

public IMap put(Object key, Object val) {
    if(_count > growAtCount)
        return grow().put(key, val);
    int i = bucketFor(key,array);
    int incr = 1;
    PersistentArray newArray = doPut(i, key, val, array);
    if(newArray == array)
        return this;
    if(array.get(i) != null && ((IMap)newArray.get(i)).count() == ((IMap)array.get(i)).count()) //key already there, no growth
        incr = 0;
    return create(_count + incr, newArray);
}

PersistentArray doPut(int i,Object key,Object val,PersistentArray array){
    IMap entries = (IMap) array.get(i);
    IMap newEntries;
    if (entries != null)
        {
        newEntries = entries.put(key, val);
        if(newEntries == entries) //already there with same value, no op
            return array;
        }
    else
        newEntries = createArrayMap(new Object[]{key, val});

    return array.set(i, newEntries);
}

public IMap remove(Object key) {
    int i = bucketFor(key,array);
    IMap entries = (IMap) array.get(i);
    if (entries != null)
        {
        IMap newEntries = entries.remove(key);
        if (newEntries != entries)
            return create(_count - 1, array.set(i, newEntries));
        }
    //not there, no op
    return this;
}

public Object get(Object key) {
    IMap entries = entriesFor(key);
    if(entries != null)
        return entries.get(key);
    return null;
}

public int capacity() {
    return array.length();
}

public Iterator<IMapEntry> iterator() {
    return new Iter(array);
}


IMap grow(){
    PersistentArray newArray = new PersistentArray(calcPrimeCapacity(_count * 2));
    for (Object o : this)
        {
        IMapEntry e = (IMapEntry) o;
        newArray = doPut(bucketFor(e.key(),newArray),e.key(), e.val(),newArray);
        }
    return create(_count,newArray);
}

static class Iter implements Iterator, IMapEntry{
    PersistentArray buckets;
    int b;
    Object[] nextEntries;
    int nextE;

    Object[] entries;
    int e;

    Iter(PersistentArray buckets){
        this.buckets = buckets;
        this.b = -1;
        nextBucket();
    }

    private void nextBucket() {
        nextEntries = null;
        nextE = 0;
        for(b = b+1;b<buckets.length();b++)
            {
            ArrayMap a = (ArrayMap) buckets.get(b);
            if(a != null)
                {
                nextEntries = a.array;
                break;
                }
            }
    }

    public boolean hasNext() {
        return nextEntries != null;
    }

    public Object next() {
        entries = nextEntries;
        e = nextE;
        nextE += 2;
        if(nextE >= nextEntries.length)
            nextBucket();
        return this;
    }

    public void remove() {
        throw new UnsupportedOperationException();
    }

    public Object key() {
        return entries[e];
    }

    public Object val() {
        return entries[e+1];
    }
}

IMap entriesFor(Object key){
    return (IMap) array.get(bucketFor(key,array));
}

static int bucketFor(Object key, PersistentArray array) {
    return (key.hashCode() & 0x7fffffff)%array.length();
}

IMap create(int capacity) {
    return new HashtableMap(capacity);
}

IMap create(int count,PersistentArray array) {
    return new HashtableMap(count, array);
}

IMap createArrayMap(Object[] init) {
    return new ArrayMap(init);
}

}
