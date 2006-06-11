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

public class PersistentHashtableMap implements IPersistentMap {

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

public IPersistentMap add(Object key) {
    return put(key, null);
}

public IPersistentMap put(Object key, Object val) {
    if(_count > growAtCount)
        return grow().put(key, val);
    int i = bucketFor(key,array);
    int incr = 1;
    PersistentArray newArray = doPut(i, key, val, array);
    if(newArray == array)
        return this;
    if(array.get(i) != null && ((IPersistentMap)newArray.get(i)).count() == ((IPersistentMap)array.get(i)).count()) //key already there, no growth
        incr = 0;
    return create(_count + incr, newArray, growAtCount);
}

PersistentArray doPut(int i,Object key,Object val,PersistentArray array){
    IPersistentMap entries = (IPersistentMap) array.get(i);
    IPersistentMap newEntries;
    if (entries != null)
        {
        newEntries = entries.put(key, val);
        if(newEntries == entries) //already there with same value, no op
            return array;
        }
    else
	    newEntries = createListMap(key, val);
		//newEntries = createArrayMap(new Object[]{key, val});

    return array.set(i, newEntries);
}

public IPersistentMap remove(Object key) {
    int i = bucketFor(key,array);
    IPersistentMap entries = (IPersistentMap) array.get(i);
    if (entries != null)
        {
        IPersistentMap newEntries = entries.remove(key);
        if (newEntries != entries)
            return create(_count - 1, array.set(i, newEntries));
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

public ISeq seq() throws Exception {
    return Seq.create(array);
}

static class Seq implements ISeq{
    PersistentArray buckets;
    int b;
    ISeq e;


    static public Seq create(PersistentArray buckets) throws Exception {
        return next(buckets, -1, null);
    }

    static Seq next(PersistentArray buckets, int b, ISeq e) throws Exception {
        if(e != null && e.rest() != null)
            return new Seq(buckets,b,e.rest());
        for(b = b+1;b<buckets.length();b++)
            {
            ISequential a = (ISequential) buckets.get(b);
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

    public Object first() throws Exception {
        return e.first();
    }

    public ISeq rest() throws Exception {
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
            PersistentListMap a = (PersistentListMap) buckets.get(b);
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
    return (IPersistentMap) array.get(bucketFor(key,array));
}

static int bucketFor(Object key, PersistentArray array) {
    return (key.hashCode() & 0x7fffffff)%array.length();
}

IPersistentMap create(int capacity) {
    return new PersistentHashtableMap(capacity);
}

IPersistentMap create(int count,PersistentArray array) {
    return new PersistentHashtableMap(count, array);
}

IPersistentMap create(int i, PersistentArray newArray, int growAtCount){
	return new PersistentHashtableMap(i, newArray, growAtCount);
}


IPersistentMap createListMap(Object key, Object val){
	return PersistentListMap.create(key,val);
}

}
