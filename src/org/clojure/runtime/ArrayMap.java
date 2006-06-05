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

/**
 * Simple implementation of persistent map on an array

 * Note that instances of this class are constant values
 * i.e. add/remove etc return new values
 *
 * Copies array on every change, so only appropriate for _very_small_ maps
 *
 * null keys and values are ok, but you won't be able to distinguish a null value via get - use contains/find
 */

public class ArrayMap implements IMap, Iterable{

Object[] array;

public ArrayMap(){
    this.array = RT.EMPTY_ARRAY;
}

/**
 * This ctor captures/aliases the passed array, so do not modify later
 * @param init {key1,val1,key2,val2,...}
 */
public ArrayMap(Object[] init){
    this.array = init;
}

public int count() {
    return array.length/2;
}

public boolean contains(Object key){
	return indexOf(key) >= 0;
}

public IMapEntry find(Object key) {
    int i = indexOf(key);
    if(i >= 0)
        return new Iter(array,i);
    return null;
}

public IMap add(Object key) {

    return put(key,null);
}

public IMap put(Object key, Object val) {
    int i = indexOf(key);
    Object[] newArray;
    if(i >= 0) //already have key, same-sized replacement
        {
        if(array[i+1] == val) //no change, no op
            return this;
        newArray = array.clone();
        newArray[i+1] = val;
        }
    else //didn't have key, grow
        {
        newArray = new Object[array.length + 2];
        newArray[array.length] = key;
        newArray[array.length + 1] = val;
        }
    return new ArrayMap(newArray);
}

public IMap remove(Object key) {
    int i = indexOf(key);
    if(i >= 0) //have key, will remove
        {
        Object[] newArray = new Object[array.length - 2];
        for(int s=0,d=0;s<array.length;s += 2)
            {
            if(!equalKey(array[s],key)) //skip removal key
                {
                newArray[d] = array[s];
                newArray[d+1] = array[s+1];
                d += 2;
                }
            }
        return new ArrayMap(newArray);
        }
    //don't have key, no op
    return this;
}

public Object get(Object key) {
    int i = indexOf(key);
    if(i >= 0)
        return array[i + 1];
    return null;
}

int indexOf(Object key){
    for(int i=0;i<array.length;i+=2)
        {
        if(equalKey(array[i],key))
            return i;
        }
    return -1;
}

boolean equalKey(Object k1,Object k2){
    if(k1 == null)
        return k2 == null;
    return k1.equals(k2);
}

public Iterator iterator() {
    return new Iter(array);
}

static class Iter implements Iterator,IMapEntry{
    Object[] array;
    int i;

    //for iterator
    Iter(Object[] array){
        this(array,-2);

    }

    //for find
    Iter(Object[] array, int i){
        this.array = array;
        this.i = i;
    }

    public boolean hasNext() {
        return i < array.length - 2;
    }

    public Object next() {
        i+=2;
        return this;
    }

    public void remove() {
        throw new UnsupportedOperationException();
    }

    public Object key() {
        return array[i];
    }

    public Object val() {
        return array[i+1];
    }
}
}
