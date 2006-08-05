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
import java.math.BigInteger;

public class PersistentHashtableMap extends APersistentMap {

static final float FILL_FACTOR = 0.75f;

final PersistentArray array;
final int _count;
final int growAtCount;

public PersistentHashtableMap(int initialCapacity) {
    array = new PersistentArray(calcPrimeCapacity(initialCapacity));
    _count = 0;
    this.growAtCount = (int) (this.array.length()*FILL_FACTOR);
}

/**
 * @param init {key1,val1,key2,val2,...}
 */
public PersistentHashtableMap(Object[] init){
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

PersistentHashtableMap(int count,PersistentArray array) {
    this._count = count;
    this.array = array;
    this.growAtCount = (int) (this.array.length()*FILL_FACTOR);
}

PersistentHashtableMap(int count,PersistentArray array,int growAt) {
    this._count = count;
    this.array = array;
    this.growAtCount = growAt;
}

int calcPrimeCapacity(int capacity) {
    return BigInteger.valueOf((long) (capacity/FILL_FACTOR)).nextProbablePrime().intValue();
}

public int count() {
    return _count;
}

public boolean contains(Object key) {
    IPersistentMap entries = entriesFor(key);
    return entries != null && entries.contains(key);
}

public IMapEntry find(Object key) {
    IPersistentMap entries = entriesFor(key);
    if(entries != null)
        return entries.find(key);
    return null;
}

public IPersistentMap assocEx(Object key, Object val) throws Exception {
    if(_count > growAtCount)
        return grow().assocEx(key, val);
    int i = bucketFor(key,array);
    int incr = 1;
    PersistentArray newArray = doAdd(i, key, val, array);
    return create(_count + incr, newArray, growAtCount);
}

public IPersistentMap assoc(Object key, Object val) {
    if(_count > growAtCount)
        return grow().assoc(key, val);
    int i = bucketFor(key,array);
    int incr = 1;
    PersistentArray newArray = doPut(i, key, val, array);
    if(newArray == array)
        return this;
    if(array.nth(i) != null && ((IPersistentMap)newArray.nth(i)).count() == ((IPersistentMap)array.nth(i)).count()) //key already there, no growth
        incr = 0;
    return create(_count + incr, newArray, growAtCount);
}

PersistentArray doPut(int i,Object key,Object val,PersistentArray array){
    IPersistentMap entries = (IPersistentMap) array.nth(i);
    IPersistentMap newEntries;
    if (entries != null)
        {
        newEntries = entries.assoc(key, val);
        if(newEntries == entries) //already there with same value, no op
            return array;
        }
    else
	    newEntries = createListMap(key, val);

    return array.assocN(i, newEntries);
}

PersistentArray doAdd(int i,Object key,Object val,PersistentArray array) throws Exception{
    IPersistentMap entries = (IPersistentMap) array.nth(i);
    IPersistentMap newEntries;
    if (entries != null)
        {
        newEntries = entries.assocEx(key, val);
        }
    else
	    newEntries = createListMap(key, val);

    return array.assocN(i, newEntries);
}

public IPersistentMap without(Object key) {
    int i = bucketFor(key,array);
    IPersistentMap entries = (IPersistentMap) array.nth(i);
    if (entries != null)
        {
        IPersistentMap newEntries = entries.without(key);
        if (newEntries != entries)
            return create(_count - 1, array.assocN(i, newEntries));
        }
    //not there, no op
    return this;
}

public Object get(Object key) {
    IPersistentMap entries = entriesFor(key);
    if(entries != null)
        return entries.get(key);
    return null;
}

public int capacity() {
    return array.length();
}

IPersistentMap grow(){
    PersistentArray newArray = new PersistentArray(calcPrimeCapacity(_count * 2));
    for (Object o : this)
        {
        IMapEntry e = (IMapEntry) o;
        newArray = doPut(bucketFor(e.key(),newArray),e.key(), e.val(),newArray);
        }
    return create(_count,newArray);
}

public Iterator iterator() {
    return new Iter(array);
}

public ISeq seq() {
    return Seq.create(array);
}

static class Seq extends ASeq{
    PersistentArray buckets;
    int b;
    ISeq e;


    static public Seq create(PersistentArray buckets)  {
        return next(buckets, -1, null);
    }

    static Seq next(PersistentArray buckets, int b, ISeq e) {
        if(e != null && e.rest() != null)
            return new Seq(buckets,b,e.rest());
        for(b = b+1;b<buckets.length();b++)
            {
            IPersistentCollection a = (IPersistentCollection) buckets.nth(b);
            if(a != null && a.seq() != null)
                return new Seq(buckets,b,a.seq());
            }
        return null;
    }

    Seq(PersistentArray buckets, int b, ISeq e) {
        this.buckets = buckets;
        this.b = b;
        this.e = e;
    }

    public Object first() {
        return e.first();
    }

    public ISeq rest() {
        return next(buckets,b,e);
    }
}

static class Iter implements Iterator{
	PersistentArray buckets;
	int b;
	PersistentListMap e;

	Iter(PersistentArray buckets){
        this.buckets = buckets;
        this.b = -1;
        nextBucket();
    }

	private void nextBucket() {
        e = null;
        for(b = b+1;b<buckets.length();b++)
            {
            PersistentListMap a = (PersistentListMap) buckets.nth(b);
            if(a != null && a != PersistentListMap.EMPTY)
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
		PersistentListMap ret = e;
        e = e.next();
        if(e == PersistentListMap.EMPTY)
            nextBucket();
        return ret;
    }

	public void remove() {
        throw new UnsupportedOperationException();
    }
}

final IPersistentMap entriesFor(Object key){
    return (IPersistentMap) array.nth(bucketFor(key,array));
}

static int bucketFor(Object key, PersistentArray array) {
    return (key.hashCode() & 0x7fffffff)%array.length();
}

IPersistentMap create(int capacity) {
    PersistentHashtableMap ret = new PersistentHashtableMap(capacity);
    ret._meta = _meta;
    return ret;
}

IPersistentMap create(int count,PersistentArray array) {
    PersistentHashtableMap ret =  new PersistentHashtableMap(count, array);
    ret._meta = _meta;
    return ret;
}

IPersistentMap create(int i, PersistentArray newArray, int growAtCount){
	PersistentHashtableMap ret =  new PersistentHashtableMap(i, newArray, growAtCount);
    ret._meta = _meta;
    return ret;
}


IPersistentMap createListMap(Object key, Object val){
	return PersistentListMap.create(key,val);
}

}
