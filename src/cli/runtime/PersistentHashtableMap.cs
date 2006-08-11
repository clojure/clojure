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
	
	
public class PersistentHashtableMap : APersistentMap {

static readonly float FILL_FACTOR = 0.75f;

readonly internal PersistentArray array;
readonly int _count;
readonly int growAtCount;

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
    PersistentArray narray = new PersistentArray(calcPrimeCapacity(init.Length));
    for(int i=0;i<init.Length;i+=2)
        {
        narray = doPut(bucketFor(init[i],narray),init[i], init[i + 1],narray);
        }
    this.array = narray;
    this._count = init.Length/2; //hmmm... presumes no dupe keys in init
    this.growAtCount = (int) (this.array.length()*FILL_FACTOR);
}

internal PersistentHashtableMap(int count,PersistentArray array) {
    this._count = count;
    this.array = array;
    this.growAtCount = (int) (this.array.length()*FILL_FACTOR);
}

internal PersistentHashtableMap(int count,PersistentArray array,int growAt) {
    this._count = count;
    this.array = array;
    this.growAtCount = growAt;
} 
	
int calcPrimeCapacity(int capacity) {
	// No .Net equivalent
    //return BigInteger.valueOf((long) (capacity/FILL_FACTOR)).nextProbablePrime().intValue();
    int ret = (int)(capacity/FILL_FACTOR);
    if(ret%2 == 0)
		++ret;
	return ret;
}

override public int count() {
    return _count;
}

override public bool contains(Object key) {
    IPersistentMap entries = entriesFor(key);
    return entries != null && entries.contains(key);
}

override public IMapEntry find(Object key) {
    IPersistentMap entries = entriesFor(key);
    if(entries != null)
        return entries.find(key);
    return null;
}

override public IPersistentMap assocEx(Object key, Object val) {
    if(_count > growAtCount)
        return grow().assocEx(key, val);
    int i = bucketFor(key,array);
    int incr = 1;
    PersistentArray newArray = doAdd(i, key, val, array);
    return create(_count + incr, newArray, growAtCount);
}

override public Associative assoc(Object key, Object val) {
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
		newEntries = (IPersistentMap)entries.assoc(key, val);
        if(newEntries == entries) //already there with same value, no op
            return array;
        }
    else
	    newEntries = createEntryMap(key, val);
		//newEntries = createArrayMap(new Object[]{key, val});

	return (PersistentArray)array.assocN(i, newEntries);
}

PersistentArray doAdd(int i,Object key,Object val,PersistentArray array) {
    IPersistentMap entries = (IPersistentMap) array.nth(i);
    IPersistentMap newEntries;
    if (entries != null)
        {
        newEntries = entries.assocEx(key, val);
        }
    else
	    newEntries = createEntryMap(key, val);

	return (PersistentArray)array.assocN(i, newEntries);
}
override public IPersistentMap without(Object key) {
    int i = bucketFor(key,array);
    IPersistentMap entries = (IPersistentMap) array.nth(i);
    if (entries != null)
        {
        IPersistentMap newEntries = entries.without(key);
        if (newEntries != entries)
			return create(_count - 1, (PersistentArray)array.assocN(i, newEntries));
        }
    //not there, no op
    return this;
}

override public Object get(Object key) {
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
	foreach (IMapEntry e in this)
        {
        newArray = doPut(bucketFor(e.key(),newArray),e.key(), e.val(),newArray);
        }
    return create(_count,newArray);
}

override public IEnumerator GetEnumerator() {
    return new Iter(array);
}

override public ISeq seq() {
	if(count() == 0)
		return null;
    return Seq.create(array,count());
}

class Seq : ASeq{
    readonly PersistentArray buckets;
	readonly int b;
	readonly ISeq e;
	readonly int cnt;


    static public Seq create(PersistentArray buckets,int cnt) {
        return next(buckets, -1, null,cnt);
    }

	static Seq next(PersistentArray buckets, int b, ISeq e, int cnt)
		{
        if(e != null && e.rest() != null)
            return new Seq(buckets,b,e.rest(),cnt);
        for(b = b+1;b<buckets.length();b++)
            {
            IPersistentCollection a = (IPersistentCollection) buckets.nth(b);
            if(a != null && a.seq() != null)
                return new Seq(buckets,b,a.seq(),cnt);
            }
        return null;
    }

	Seq(PersistentArray buckets, int b, ISeq e, int cnt)
		{
        this.buckets = buckets;
        this.b = b;
        this.e = e;
		this.cnt = cnt;
    }

    override public Object first() {
        return e.first();
    }

    override public ISeq rest() {
        return next(buckets,b,e,cnt-1);
    }

	public override int count()
	{
		return cnt;
	}
}

internal class Iter : IEnumerator{
	PersistentArray buckets;
	int b;
	ISeq e;

	internal Iter(PersistentArray buckets){
        this.buckets = buckets;
        this.b = -1;
    }

	private void nextBucket() {
        e = null;
        for(b = b+1;b<buckets.length();b++)
            {
			IPersistentCollection a = (IPersistentCollection)buckets.nth(b);
            if(a != null && a.seq() != null)
                {
                e = a.seq();
                break;
                }
            }
    }

#region IEnumerator Members

public object Current
	{
	get { return e.first(); }
	}

public bool MoveNext()
	{
	if (e == null || (e = e.rest()) == null)
		nextBucket();
	return e != null;
	}

public void Reset()
	{
	throw new Exception("The method or operation is not implemented.");
	}

#endregion
	}

IPersistentMap entriesFor(Object key){
    return (IPersistentMap) array.nth(bucketFor(key,array));
}

static int bucketFor(Object key, PersistentArray array) {
    return (RT.hash(key) & 0x7fffffff) % array.length();
}

virtual internal IPersistentMap create(int capacity) {
    PersistentHashtableMap ret = new PersistentHashtableMap(capacity);
    ret._meta = _meta;
    return ret;
}

virtual internal IPersistentMap create(int count,PersistentArray array) {
	PersistentHashtableMap ret = new PersistentHashtableMap(count, array);
	ret._meta = _meta;
	return ret;
	}

virtual internal IPersistentMap create(int i, PersistentArray newArray, int growAtCount){
	PersistentHashtableMap ret = new PersistentHashtableMap(i, newArray, growAtCount);
	ret._meta = _meta;
	return ret;
	}


virtual internal IPersistentMap createEntryMap(Object key, Object val){
	return new MapEntry(key, val);
//return PersistentListMap.create(key, val);
}

}


}