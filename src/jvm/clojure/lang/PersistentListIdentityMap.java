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

/**
 * Implementation of persistent map on a linked list
 * Uses identity (==) comparison, vs equals() of PersistentListMap

 * Note that instances of this class are constant values
 * i.e. add/remove etc return new values
 *
 * Lookups/changes are linear, so only appropriate for _very_small_ maps
 * PersistentArrayMap is generally faster, but this class avoids the double allocation,
 * and so is better/faster as a bucket for hash tables
 *
 * null keys and values are ok, but you won't be able to distinguish a null value via get - use contains/find
 *
 * code duplication here is kind of gross, but most efficient
 */

public class PersistentListIdentityMap implements IPersistentMap, IMapEntry, ISeq, ISequential
{

static public PersistentListIdentityMap EMPTY = new PersistentListIdentityMap();

static public PersistentListIdentityMap create(Object key, Object val){
	return new Tail(key, val);
}

public Object key(){
	return null;
}

public Object val(){
	return null;
}

PersistentListIdentityMap next(){
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

public IPersistentMap add(Object key){
	return put(key, null);
}

public PersistentListIdentityMap put(Object key, Object val){
	return new Tail(key, val);
}

public PersistentListIdentityMap remove(Object key){
	return this;
}

public Object get(Object key){
	return null;
}

public int capacity(){
	return 0;
}

public Object first() {
    return null;
}

public ISeq rest() {
    return null;
}

public ISeq seq() {
    return null;
}


static class Iter implements Iterator{
	PersistentListIdentityMap e;

	Iter(PersistentListIdentityMap e)
	{
	this.e = e;
	}

	public boolean hasNext(){
		return e != EMPTY;
	}

	public Object next(){
		PersistentListIdentityMap ret = e;
		e = e.next();
		return ret;
	}

	public void remove(){
		throw new UnsupportedOperationException();
	}
}

public Iterator iterator(){
	return new Iter(this);
}

static class Tail extends PersistentListIdentityMap {
	final Object _key;
	final Object _val;

	Tail(Object key,Object val){
		this._key = key;
		this._val = val;
		}

    PersistentListIdentityMap next(){
        return EMPTY;
    }

	public int count(){
		return 1;
	}

	public Object get(Object key){
		if(key ==_key)
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
		return key ==_key;
	}

	public IMapEntry find(Object key){
		if(key ==_key)
			return this;
		return null;
	}

	public PersistentListIdentityMap put(Object key, Object val){
		if(key == _key)  //replace
			{
			if(val == _val)
				return this;
			return new Tail(key,val);
			}
		return new Link(key,val,this);
	}

	public PersistentListIdentityMap remove(Object key){
		if(key == _key)
			return EMPTY;
		return this;
	}

    public Object first() {
        return this;
    }

    public ISeq rest() {
        return null;
    }

    public ISeq seq() {
        return this;
    }
}

static class Link extends PersistentListIdentityMap {
	final Object _key;
	final Object _val;
	final PersistentListIdentityMap _rest;

	Link(Object key,Object val,PersistentListIdentityMap next){
		this._key = key;
		this._val = val;
		this._rest = next;
		}

	public Object key(){
		return _key;
	}

	public Object val(){
		return _val;
	}

	PersistentListIdentityMap next(){
        return _rest;
        }

	public int count(){
		return 1 + _rest.count();
	}

	public boolean contains(Object key){
		return find(key) != null;
	}

	public IMapEntry find(Object key){
		if(key ==_key)
			return this;
		return _rest.find(key);
	}

	public PersistentListIdentityMap put(Object key, Object val){
		IMapEntry e = find(key);
		if(e != null)
			{
			if(e.val() == val)
				return this;
			return create(_key,_val,remove(key));
			}
		return new Link(key,val,this);
	}

	public PersistentListIdentityMap remove(Object key){
		if(key == _key)
			return _rest;
		PersistentListIdentityMap r = _rest.remove(key);
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

    public Object first() {
        return this;
    }

    public ISeq rest() {
        return _rest;
    }

    public ISeq seq() {
        return this;
    }

    PersistentListIdentityMap create(Object k,Object v,PersistentListIdentityMap r){
		if(r == EMPTY)
			return new Tail(k,v);
		return new Link(k, v, r);
	}

}

}
