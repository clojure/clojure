/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

package clojure.lang;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;

/**
 * <p>Simple implementation of persistent map on an array</p>
 *
 * <p>Note that instances of this class are constant values
 * i.e. add/remove etc return new values</p>
 *
 * <p>Copies array on every change, so only appropriate for _very_small_ maps</p>
 *
 * <p>null keys and values are ok, but you won't be able to distinguish a null value via valAt - use contains/entryAt</p>
 */

public class PersistentArrayMap extends APersistentMap implements IObj, IEditableCollection, IMapIterable, IKVReduce{

final Object[] array;
static final int HASHTABLE_THRESHOLD = 16;

public static final PersistentArrayMap EMPTY = new PersistentArrayMap();
private final IPersistentMap _meta;

static public IPersistentMap create(Map other){
	ITransientMap ret = EMPTY.asTransient();
	for(Object o : other.entrySet())
		{
            Map.Entry e = (Entry) o;
            ret = ret.assoc(e.getKey(), e.getValue());
		}
	return ret.persistent();
}

protected PersistentArrayMap(){
	this.array = new Object[]{};
	this._meta = null;
}

public PersistentArrayMap withMeta(IPersistentMap meta){
	if(meta() == meta)
		return this;
	return new PersistentArrayMap(meta, array);
}

PersistentArrayMap create(Object... init){
	return new PersistentArrayMap(meta(), init);
}

IPersistentMap createHT(Object[] init){
	return PersistentHashMap.create(meta(), init);
}

static public PersistentArrayMap createWithCheck(Object[] init){
	for(int i=0;i< init.length;i += 2)
		{
		for(int j=i+2;j<init.length;j += 2)
			{
			if(equalKey(init[i],init[j]))
				throw new IllegalArgumentException("Duplicate key: " + init[i]);
			}
		}
	return new PersistentArrayMap(init);
}

/**
 * <p>This method attempts to find resue the given array as the basis for an array map as quickly as possible.</p>
 *
 * <p>If a trailing element exists in the array or it contains duplicate keys then it delegates to the complex path.</p>
 **/
static public PersistentArrayMap createAsIfByAssoc(Object[] init){
	boolean complexPath, hasTrailing;
	complexPath = hasTrailing = ((init.length & 1) == 1);

	for(int i=0;((i< init.length) && !complexPath);i += 2)
		{
		for(int j=0;j<i;j += 2)
			{
			if(equalKey(init[i],init[j]))
				{
				    complexPath = true;
				    break;
				}
			}
		}

	if (complexPath) return createAsIfByAssocComplexPath(init, hasTrailing);

	return new PersistentArrayMap(init);
}

private static Object[] growSeedArray(Object[] seed, IPersistentCollection trailing){
	ISeq extraKVs = trailing.seq();
	int seedCount = seed.length - 1;
	Object[] result = Arrays.copyOf(seed, seedCount + (trailing.count() * 2));

	for(int i=seedCount; extraKVs != null; extraKVs = extraKVs.next(), i+=2)
		{
		Map.Entry e = (Entry) extraKVs.first();
		result[i] = e.getKey();
		result[i+1] = e.getValue();
		}

	return result;
}

/**
 * <p>This method handles the default case of an array containing alternating key/value pairs.</p>
 * <p>It will reallocate a smaller init array if duplicate keys are found.</p>
 *
 * <p>If a trailing element is found then will attempt to add it to the resulting map as if by conj.</p>
 * <p>No guarantees about the order of the keys in the trailing element are made.</p>
 **/
private static PersistentArrayMap createAsIfByAssocComplexPath(Object[] init, boolean hasTrailing){
	if(hasTrailing)
		{
		IPersistentCollection trailing = PersistentArrayMap.EMPTY.cons(init[init.length-1]);
		init = growSeedArray(init, trailing);
		}

	// If this looks like it is doing busy-work, it is because it
	// is achieving these goals: O(n^2) run time like
	// createWithCheck(), never modify init arg, and only
	// allocate memory if there are duplicate keys.
	int n = 0;
	for(int i=0;i< init.length;i += 2)
		{
		boolean duplicateKey = false;
		for(int j=0;j<i;j += 2)
			{
			if(equalKey(init[i],init[j]))
				{
				duplicateKey = true;
				break;
				}
			}
		if(!duplicateKey)
			n += 2;
		}
	if(n < init.length)
		{
		// Create a new shorter array with unique keys, and
		// the last value associated with each key.  To behave
		// like assoc, the first occurrence of each key must
		// be used, since its metadata may be different than
		// later equal keys.
		Object[] nodups = new Object[n];
		int m = 0;
		for(int i=0;i< init.length;i += 2)
			{
			boolean duplicateKey = false;
			for(int j=0;j<m;j += 2)
				{
				if(equalKey(init[i],nodups[j]))
					{
					duplicateKey = true;
					break;
					}
				}
			if(!duplicateKey)
				{
				int j;
				for (j=init.length-2; j>=i; j -= 2)
					{
					if(equalKey(init[i],init[j]))
						{
						break;
						}
					}
				nodups[m] = init[i];
				nodups[m+1] = init[j+1];
				m += 2;
				}
			}
		if (m != n)
			throw new IllegalArgumentException("Internal error: m=" + m);
		init = nodups;
		}
	return new PersistentArrayMap(init);
}
/**
 * This ctor captures/aliases the passed array, so do not modify later
 *
 * @param init {key1,val1,key2,val2,...}
 */
public PersistentArrayMap(Object[] init){
	this.array = init;
	this._meta = null;
}


public PersistentArrayMap(IPersistentMap meta, Object[] init){
	this._meta = meta;
	this.array = init;
}

public int count(){
	return array.length / 2;
}

public boolean containsKey(Object key){
	return indexOf(key) >= 0;
}

public IMapEntry entryAt(Object key){
	int i = indexOf(key);
	if(i >= 0)
		return (IMapEntry) MapEntry.create(array[i],array[i+1]);
	return null;
}

public IPersistentMap assocEx(Object key, Object val) {
	int i = indexOf(key);
	Object[] newArray;
	if(i >= 0)
		{
		throw Util.runtimeException("Key already present");
		}
	else //didn't have key, grow
		{
		if(array.length >= HASHTABLE_THRESHOLD)
			return createHT(array).assocEx(key, val);
		newArray = new Object[array.length + 2];
		if(array.length > 0)
			System.arraycopy(array, 0, newArray, 2, array.length);
		newArray[0] = key;
		newArray[1] = val;
		}
	return create(newArray);
}

public IPersistentMap assoc(Object key, Object val){
	int i = indexOf(key);
	Object[] newArray;
	if(i >= 0) //already have key, same-sized replacement
		{
		if(array[i + 1] == val) //no change, no op
			return this;
		newArray = array.clone();
		newArray[i + 1] = val;
		}
	else //didn't have key, grow
		{
		if(array.length >= HASHTABLE_THRESHOLD)
			return createHT(array).assoc(key, val);
		newArray = new Object[array.length + 2];
		if(array.length > 0)
			System.arraycopy(array, 0, newArray, 0, array.length);
		newArray[newArray.length-2] = key;
		newArray[newArray.length-1] = val;
		}
	return create(newArray);
}

public IPersistentMap without(Object key){
	int i = indexOf(key);
	if(i >= 0) //have key, will remove
		{
		int newlen = array.length - 2;
		if(newlen == 0)
			return empty();
		Object[] newArray = new Object[newlen];
		System.arraycopy(array, 0, newArray, 0, i);
		System.arraycopy(array, i+2, newArray, i, newlen - i);
		return create(newArray);
		}
	//don't have key, no op
	return this;
}

public IPersistentMap empty(){
	return (IPersistentMap) EMPTY.withMeta(meta());
}

final public Object valAt(Object key, Object notFound){
	int i = indexOf(key);
	if(i >= 0)
		return array[i + 1];
	return notFound;
}

public Object valAt(Object key){
	return valAt(key, null);
}

public int capacity(){
	return count();
}

private int indexOfObject(Object key){
    Util.EquivPred ep = Util.equivPred(key);
    for(int i = 0; i < array.length; i += 2)
        {
        if(ep.equiv(key, array[i]))
            return i;
        }
	return -1;
}

private int indexOf(Object key){
    if(key instanceof Keyword)
        {
        for(int i = 0; i < array.length; i += 2)
            {
            if(key == array[i])
                return i;
            }
    	return -1;
        }
    else
        return indexOfObject(key);
}

static boolean equalKey(Object k1, Object k2){
    if(k1 instanceof Keyword)
        return k1 == k2;
	return Util.equiv(k1, k2);
}

public Iterator iterator(){
	return new Iter(array,APersistentMap.MAKE_ENTRY);
}

public Iterator keyIterator(){
    return new Iter(array,APersistentMap.MAKE_KEY);
}

public Iterator valIterator() {
    return new Iter(array,APersistentMap.MAKE_VAL);
}

public ISeq seq(){
	if(array.length > 0)
		return new Seq(array, 0);
	return null;
}

public IPersistentMap meta(){
	return _meta;
}

static class Seq extends ASeq implements Counted{
	final Object[] array;
	final int i;

	Seq(Object[] array, int i){
		this.array = array;
		this.i = i;
	}

	public Seq(IPersistentMap meta, Object[] array, int i){
		super(meta);
		this.array = array;
		this.i = i;
	}

	public Object first(){
		return MapEntry.create(array[i],array[i+1]);
	}

	public ISeq next(){
		if(i + 2 < array.length)
			return new Seq(array, i + 2);
		return null;
	}

	public int count(){
		return (array.length - i) / 2;
	}

	public Obj withMeta(IPersistentMap meta){
		if(meta() == meta)
			return this;
		return new Seq(meta, array, i);
	}
}

static class Iter implements Iterator{
    IFn f;
	Object[] array;
	int i;

	//for iterator
	Iter(Object[] array, IFn f){
		this(array, -2, f);
	}

	//for entryAt
	Iter(Object[] array, int i, IFn f){
		this.array = array;
		this.i = i;
        this.f = f;
	}

	public boolean hasNext(){
		return i < array.length - 2;
	}

	public Object next(){
		try {
			i += 2;
			return f.invoke(array[i], array[i+1]);
		} catch(IndexOutOfBoundsException e) {
			throw new NoSuchElementException();
		}
	}

	public void remove(){
		throw new UnsupportedOperationException();
	}

}

public Object kvreduce(IFn f, Object init){
    for(int i=0;i < array.length;i+=2){
        init = f.invoke(init, array[i], array[i+1]);
	    if(RT.isReduced(init))
		    return ((IDeref)init).deref();
        }
    return init;
}

public ITransientMap asTransient(){
	return new TransientArrayMap(array);
}

static final class TransientArrayMap extends ATransientMap {
	volatile int len;
	final Object[] array;
	volatile Thread owner;

	public TransientArrayMap(Object[] array){
		this.owner = Thread.currentThread();
		this.array = new Object[Math.max(HASHTABLE_THRESHOLD, array.length)];
		System.arraycopy(array, 0, this.array, 0, array.length);
		this.len = array.length;
	}
	
	private int indexOf(Object key){
		for(int i = 0; i < len; i += 2)
			{
			if(equalKey(array[i], key))
				return i;
			}
		return -1;
	}

	ITransientMap doAssoc(Object key, Object val){
		int i = indexOf(key);
		if(i >= 0) //already have key,
			{
			if(array[i + 1] != val) //no change, no op
				array[i + 1] = val;
			}
		else //didn't have key, grow
			{
			if(len >= array.length)
				return PersistentHashMap.create(array).asTransient().assoc(key, val);
			array[len++] = key;
			array[len++] = val;
			}
		return this;
	}

	ITransientMap doWithout(Object key) {
		int i = indexOf(key);
		if(i >= 0) //have key, will remove
			{
			if (len >= 2)
				{
					array[i] = array[len - 2];
					array[i + 1] = array[len - 1];
				}
			len -= 2;
			}
		return this;
	}

	Object doValAt(Object key, Object notFound) {
		int i = indexOf(key);
		if (i >= 0)
			return array[i + 1];
		return notFound;
	}

	int doCount() {
		return len / 2;
	}
	
	IPersistentMap doPersistent(){
		ensureEditable();
		owner = null;
		Object[] a = new Object[len];
		System.arraycopy(array,0,a,0,len);
		return new PersistentArrayMap(a);
	}

	void ensureEditable(){
		if(owner == null)
			throw new IllegalAccessError("Transient used after persistent! call");
	}
}
}
