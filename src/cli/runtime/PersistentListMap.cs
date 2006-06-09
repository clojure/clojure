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

using System;
using System.Collections;

namespace org.clojure.runtime
{
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
public class PersistentListMap : IPersistentMap, IMapEntry, ISeq, ISequential
{

static public PersistentListMap EMPTY = new PersistentListMap();

static public PersistentListMap create(Object key, Object val){
	return new Tail(key, val);
}

public virtual Object key(){
	return null;
}

public virtual Object val(){
	return null;
}

internal virtual PersistentListMap next(){
    return this;
    }

public virtual int count(){
	return 0;
}

public virtual bool contains(Object key){
	return false;
}

public virtual IMapEntry find(Object key){
	return null;
}

public virtual IPersistentMap add(Object key){
	return put(key, null);
}

public virtual IPersistentMap put(Object key, Object val){
	return new Tail(key, val);
}

public virtual IPersistentMap remove(Object key){
	return this;
}

public virtual Object get(Object key){
	return null;
}

public virtual int capacity(){
	return 0;
}

virtual public Object first()
	{
	return null;
	}

virtual public ISeq rest()
	{
	return null;
	}

virtual public ISeq seq()
	{
	return null;
	}


internal class Iter : IEnumerator{
	PersistentListMap e;
	bool first = true;

	internal Iter(PersistentListMap e)
	{
	this.e = e;
	}

#region IEnumerator Members

public object Current
	{
	get { return e; }
	}

public bool MoveNext()
	{
	if(first)
		first = false;
	else
		e = e.next();
	return e != EMPTY;
	}

public void Reset()
	{
	throw new Exception("The method or operation is not implemented.");
	}

#endregion
	}

public IEnumerator GetEnumerator(){
	return new Iter(this);
}

internal class Tail : PersistentListMap {
	readonly Object _key;
	readonly Object _val;

	internal Tail(Object key,Object val){
		this._key = key;
		this._val = val;
		}

    override internal PersistentListMap next(){
        return EMPTY;
    }

	override public int count()
		{
		return 1;
	}

	override public Object get(Object key)
		{
		if(equalKey(key,_key))
			return _val;
		return null;
	}

	override public int capacity()
		{
		return 1;
	}

	override public Object key()
		{
		return _key;
	}

	override public Object val()
		{
		return _val;
	}

	override public bool contains(Object key)
		{
		return equalKey(key,_key);
	}

	override public IMapEntry find(Object key)
		{
		if(equalKey(key,_key))
			return this;
		return null;
	}

	override public IPersistentMap put(Object key, Object val)
		{
		if(equalKey(key,_key))  //replace
			{
			if(val == _val)
				return this;
			return new Tail(key,val);
			}
		return new Link(key,val,this);
	}

	override public IPersistentMap remove(Object key){
		if(equalKey(key,_key))
			return EMPTY;
		return this;
	}

	override public Object first()
	{
	return this;
	}

	override public ISeq rest()
	{
	return null;
	}

	override public ISeq seq()
	{
	return this;
	}
}

internal class Link : PersistentListMap {
	readonly Object _key;
	readonly Object _val;
	readonly PersistentListMap _rest;

	internal Link(Object key,Object val,PersistentListMap next){
		this._key = key;
		this._val = val;
		this._rest = next;
		}

	override public Object key(){
		return _key;
	}

	override public Object val(){
		return _val;
	}

	override internal PersistentListMap next(){
        return _rest;
        }

	override public int count(){
		return 1 + _rest.count();
	}

	override public bool contains(Object key){
		return find(key) != null;
	}

	override public IMapEntry find(Object key){
		if(equalKey(key,_key))
			return this;
		return _rest.find(key);
	}

	override public IPersistentMap put(Object key, Object val)
		{
		IMapEntry e = find(key);
		if(e != null)
			{
			if(e.val() == val)
				return this;
			return create(_key,_val,remove(key));
			}
		return new Link(key,val,this);
	}

	override public IPersistentMap remove(Object key)
		{
		if(equalKey(key,_key))
			return _rest;
		PersistentListMap r = (PersistentListMap)_rest.remove(key);
		if(r == _rest)  //not there
			return this;
		return create(_key,_val,r);
	}

	override public Object get(Object key){
		IMapEntry e = find(key);
		if(e != null)
			return e.val();
		return null;
	}

	override public int capacity(){
		return count();
	}

	override public Object first()
	{
	return this;
	}

	override public ISeq rest()
	{
	return _rest;
	}

	override public ISeq seq()
	{
	return this;
	}
	
	PersistentListMap create(Object k, Object v, IPersistentMap r)
		{
		if(r == EMPTY)
			return new Tail(k,v);
		return new Link(k, v, (PersistentListMap)r);
	}

}

bool equalKey(Object k1,Object k2){
    if(k1 == null)
        return k2 == null;
    return k1.Equals(k2);
}
}

}
