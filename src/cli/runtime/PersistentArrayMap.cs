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

/**
 * Simple implementation of persistent map on an array

 * Note that instances of this class are constant values
 * i.e. add/remove etc return new values
 *
 * Copies array on every change, so only appropriate for _very_small_ maps
 *
 * null keys and values are ok, but you won't be able to distinguish a null value via get - use contains/find
 */

public class PersistentArrayMap : APersistentMap {

internal readonly Object[] array;

	public static PersistentArrayMap EMPTY = new PersistentArrayMap();
	
	internal const int HASHTABLE_THRESHOLD = 42;

protected PersistentArrayMap(){
    this.array = RT.EMPTY_ARRAY;
}

virtual internal PersistentArrayMap create(params Object[] init){
	PersistentArrayMap ret = new PersistentArrayMap(init);
	ret._meta = _meta;
	return ret;
}

virtual internal IPersistentMap createHT(Object[] init){
	PersistentHashtableMap ret = new PersistentHashtableMap(init);
	ret._meta = _meta;
	return ret;
	}
	
/**
 * This ctor captures/aliases the passed array, so do not modify later
 * @param init {key1,val1,key2,val2,...}
 */
public PersistentArrayMap(params Object[] init){
    this.array = init;
}

override public int count() {
    return array.Length/2;
}

override public bool contains(Object key){
	return indexOf(key) >= 0;
}

override public IMapEntry find(Object key) {
    int i = indexOf(key);
    if(i >= 0)
        return new Iter(array,i);
    return null;
}

override public IPersistentMap assocEx(Object key, Object val) {
    int i = indexOf(key);
    Object[] newArray;
    if(i >= 0)
        {
		throw new Exception("Key already present");
        }
    else //didn't have key, grow
        {
		if (array.Length > HASHTABLE_THRESHOLD)
			return createHT(array).assocEx(key, val);
		newArray = new Object[array.Length + 2];
        if(array.Length > 0)
            Array.Copy(array,0,newArray,2,array.Length);
        newArray[0] = key;
        newArray[1] = val;
        }
    return create(newArray);
    }

override public Associative assoc(Object key, Object val) {
    int i = indexOf(key);
    Object[] newArray;
    if(i >= 0) //already have key, same-sized replacement
        {
        if(array[i+1] == val) //no change, no op
            return this;
        newArray = (Object[])array.Clone();
        newArray[i+1] = val;
        }
    else //didn't have key, grow
        {
		if (array.Length > HASHTABLE_THRESHOLD)
			return createHT(array).assoc(key, val);
		newArray = new Object[array.Length + 2];
        if(array.Length > 0)
            Array.Copy(array,0,newArray,2,array.Length);
        newArray[0] = key;
        newArray[1] = val;
        }
    return create(newArray);
}

override public IPersistentMap without(Object key) {
    int i = indexOf(key);
    if(i >= 0) //have key, will remove
        {
		int newlen = array.Length - 2;
		if (newlen == 0)
			return empty();
		Object[] newArray = new Object[newlen];
        for(int s=0,d=0;s<array.Length;s += 2)
            {
            if(!equalKey(array[s],key)) //skip removal key
                {
                newArray[d] = array[s];
                newArray[d+1] = array[s+1];
                d += 2;
                }
            }
        return create(newArray);
        }
    //don't have key, no op
    return this;
}

virtual public IPersistentMap empty() {
    if(_meta == null)
        return EMPTY;
    PersistentArrayMap ret = new PersistentArrayMap();
    ret._meta = _meta;
    return ret;
}

override public Object get(Object key) {
    int i = indexOf(key);
    if(i >= 0)
        return array[i + 1];
    return null;
}

public int capacity() {
    return count();
}

int indexOf(Object key){
    for(int i=0;i<array.Length;i+=2)
        {
        if(equalKey(array[i],key))
            return i;
        }
    return -1;
}

internal virtual bool equalKey(Object k1,Object k2){
    if(k1 == null)
        return k2 == null;
    return k1.Equals(k2);
}

override public IEnumerator GetEnumerator() {
    return new Iter(array);
}

override public ISeq seq() {
    if(array.Length > 0)
        return new Seq(array,0);
    return null;
}

internal class Seq : ISeq, IMapEntry{
    readonly Object[] array;
	readonly int i;

    internal Seq(Object[] array, int i){
        this.array = array;
        this.i = i;
    }

    public Object key() {
        return array[i];
    }

    public Object val() {
        return array[i+1];
    }

    public Object first() {
        return this;
    }

    public ISeq rest() {
        if(i+2 < array.Length)
            return new Seq(array, i + 2);
        return null;
    }
}
internal class Iter : IEnumerator,IMapEntry{
    Object[] array;
    int i;

    //for iterator
    internal Iter(Object[] array):  this(array,-2)
    {
    }

    //for find
    internal Iter(Object[] array, int i){
        this.array = array;
        this.i = i;
    }

    public Object key() {
        return array[i];
    }

    public Object val() {
        return array[i+1];
    }

#region IEnumerator Members

public object Current
	{
	get {return this;}
	}

public bool MoveNext()
	{
	i += 2;
	return i < array.Length - 2;
	}

public void Reset()
	{
	throw new Exception("The method or operation is not implemented.");
	}

#endregion
	}
}


}