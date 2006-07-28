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

package clojure.lang;

import java.util.Iterator;

/**
 * Implementation of persistent map on a linked list

 * Note that instances of this class are constant values
 * i.e. add/remove etc return new values
 *
 * Lookups/changes are linear, so only appropriate for _very_small_ maps
 * PersistentArrayMap is generally faster, but this class avoids the double allocation,
 * and so is better/faster as a bucket for hash tables
 *
 * null keys and values are ok, but you won't be able to distinguish a null value via get - use contains/find
 */
public class PersistentListMap extends Obj implements IPersistentMap, IMapEntry, ISeq, ISequential, Cloneable
{

static public PersistentListMap EMPTY = new PersistentListMap();

static public PersistentListMap create(Object key, Object val){
	return new Tail(key, val,null);
}

public Obj withMeta(IPersistentMap meta) {
    try{
    Obj ret = (Obj) clone();
    ret._meta = meta;
    return ret;
    }
    catch(CloneNotSupportedException ignore)
        {
        return null;
        }
}

public Object key(){
	return null;
}

public Object val(){
	return null;
}

PersistentListMap next(){
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

public IPersistentMap add(Object key, Object val) throws Exception {
    return put(key, val);
}

public PersistentListMap put(Object key, Object val) {
    return new Tail(key, val,_meta);
}

public PersistentListMap remove(Object key) {
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
    PersistentListMap e;

    Iter(PersistentListMap e)
    {
    this.e = e;
    }

    public boolean hasNext(){
        return e.count() != 0;
    }

    public Object next(){
        PersistentListMap ret = e;
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

static class Tail extends PersistentListMap {
	final Object _key;
	final Object _val;

	Tail(Object key,Object val, IPersistentMap meta){
		this._key = key;
		this._val = val;
        this._meta = meta;
        }

    PersistentListMap next(){
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

    public PersistentListMap add(Object key, Object val) throws Exception {
        if(equalKey(key,_key))  //replace
            {
            throw new Exception("Key already present");
            }
        return new Link(key,val,this,_meta);
    }

	public PersistentListMap put(Object key, Object val){
		if(equalKey(key,_key))  //replace
			{
			if(val == _val)
				return this;
			return new Tail(key,val,_meta);
			}
		return new Link(key,val,this,_meta);
	}

	public PersistentListMap remove(Object key) {
        if(equalKey(key,_key))
            {
            if(_meta == null)
                return EMPTY;
            return (PersistentListMap) EMPTY.withMeta(_meta);
            }
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

static class Link extends PersistentListMap {
	final Object _key;
	final Object _val;
	final PersistentListMap _rest;

	Link(Object key,Object val,PersistentListMap next,IPersistentMap meta){
		this._key = key;
		this._val = val;
		this._rest = next;
        this._meta = meta;
        }

	public Object key(){
		return _key;
	}

	public Object val(){
		return _val;
	}

	PersistentListMap next(){
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

    public PersistentListMap add(Object key, Object val) throws Exception {
        IMapEntry e = find(key);
        if(e != null)
            {
            throw new Exception("Key already present");
            }
        return new Link(key,val,this,_meta);
    }

    public PersistentListMap put(Object key, Object val) {
        IMapEntry e = find(key);
        if(e != null)
            {
            if(e.val() == val)
                return this;
            return create(_key,_val,remove(key));
            }
        return new Link(key,val,this,_meta);
    }

	public PersistentListMap remove(Object key) {
        if(equalKey(key,_key))
            {
            if(_rest._meta == _meta)
                return _rest;
            return (PersistentListMap) _rest.withMeta(_meta);
            }
        PersistentListMap r = _rest.remove(key);
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

    PersistentListMap create(Object k,Object v,PersistentListMap r){
		if(r.count() == 0)
			return new Tail(k,v,_meta);
		return new Link(k, v, r,_meta);
	}

}

boolean equalKey(Object k1,Object k2){
    if(k1 == null)
        return k2 == null;
    return k1.equals(k2);
}
}
