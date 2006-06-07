/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Jun 6, 2006 */

package org.clojure.runtime;

import java.util.Iterator;

/**
 * Immplementation of persistent map on a linked list

 * Note that instances of this class are constant values
 * i.e. add/remove etc return new values
 *
 * Lookups/changes are linear, so only appropriate for _very_small_ maps
 * ArrayMap is generally faster, but this class avoids the double allocation,
 * and so is better/faster as a bucket for hash tables
 *
 * null keys and values are ok, but you won't be able to distinguish a null value via get - use contains/find
 */
public class ListMap implements IMap, IMapEntry
{

static public ListMap EMPTY = new ListMap();

static public ListMap create(Object key, Object val){
	return new Tail(key, val);
}

public Object key(){
	return null;
}

public Object val(){
	return null;
}

ListMap rest(){
    return this;
    }

public int count(){
	return 0;
}

public boolean contains(Object key){
	return false;
}

public IMapEntry find(Object key){
	return null;
}

public IMap add(Object key){
	return put(key, null);
}

public ListMap put(Object key, Object val){
	return new Tail(key, val);
}

public ListMap remove(Object key){
	return this;
}

public Object get(Object key){
	return null;
}

public int capacity(){
	return 0;
}

static class Iter implements Iterator{
	ListMap e;

	Iter(ListMap e)
	{
	this.e = e;
	}

	public boolean hasNext(){
		return e != EMPTY;
	}

	public Object next(){
		ListMap ret = e;
		e = e.rest();
		return ret;
	}

	public void remove(){
		throw new UnsupportedOperationException();
	}
}

public Iterator iterator(){
	return new Iter(this);
}

static class Tail extends ListMap{
	final Object _key;
	final Object _val;

	Tail(Object key,Object val){
		this._key = key;
		this._val = val;
		}

    ListMap rest(){
        return EMPTY;
    }

	public int count(){
		return 1;
	}

	public Object get(Object key){
		if(equalKey(key,_key))
			return _val;
		return null;
	}

	public int capacity(){
		return 1;
	}

	public Object key(){
		return _key;
	}

	public Object val(){
		return _val;
	}

	public boolean contains(Object key){
		return equalKey(key,_key);
	}

	public IMapEntry find(Object key){
		if(equalKey(key,_key))
			return this;
		return null;
	}

	public ListMap put(Object key, Object val){
		if(equalKey(key,_key))  //replace
			{
			if(val == _val)
				return this;
			return new Tail(key,val);
			}
		return new Link(key,val,this);
	}

	public ListMap remove(Object key){
		if(equalKey(key,_key))
			return EMPTY;
		return this;
	}
}

static class Link extends ListMap{
	final Object _key;
	final Object _val;
	final ListMap _rest;

	Link(Object key,Object val,ListMap rest){
		this._key = key;
		this._val = val;
		this._rest = rest;
		}

	public Object key(){
		return _key;
	}

	public Object val(){
		return _val;
	}

	ListMap rest(){
        return _rest;
        }

	public int count(){
		return 1 + _rest.count();
	}

	public boolean contains(Object key){
		return find(key) != null;
	}

	public IMapEntry find(Object key){
		if(equalKey(key,_key))
			return this;
		return _rest.find(key);
	}

	public ListMap put(Object key, Object val){
		IMapEntry e = find(key);
		if(e != null)
			{
			if(e.val() == val)
				return this;
			return create(_key,_val,remove(key));
			}
		return new Link(key,val,this);
	}

	public ListMap remove(Object key){
		if(equalKey(key,_key))
			return _rest;
		ListMap r = _rest.remove(key);
		if(r == _rest)  //not there
			return this;
		return create(_key,_val,r);
	}

	public Object get(Object key){
		IMapEntry e = find(key);
		if(e != null)
			return e.val();
		return null;
	}

	public int capacity(){
		return count();
	}

	ListMap create(Object k,Object v,ListMap r){
		if(r == EMPTY)
			return new Tail(k,v);
		return new Link(k, v, r);
	}

}

boolean equalKey(Object k1,Object k2){
    if(k1 == null)
        return k2 == null;
    return k1.equals(k2);
}
}
