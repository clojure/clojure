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

public class ListMap implements IMap
{
final Entry entries;
final int _count;

public ListMap(){
	this.entries = null;
	this._count = 0;
}

public ListMap(Object key, Object val){
	this.entries = Entry.create(key, val, null);
	this._count = 1;
}

ListMap(int count, Entry entries){
	this._count = count;
	this.entries = entries;
}



static class Entry implements IMapEntry{
	final Object _key;
	final Object _val;

	Entry(Object key,Object val){
		this._key = key;
		this._val = val;
		}

    Entry rest(){
        return null;
    }

    static Entry create(Object key,Object val,Entry rest){
        if(rest == null)
            return new Entry(key,val);
        return new EntryLink(key, val, rest);
    }

	public Object key(){
		return _key;
	}

	public Object val(){
		return _val;
	}
}

static class EntryLink extends Entry{
	final Entry _rest;

	EntryLink(Object key,Object val,Entry rest){
        super(key,val);
		this._rest = rest;
		}

    Entry rest(){
        return _rest;
    }
}

public int count(){
	return _count;
}

public boolean contains(Object key){
	return find(key) != null;
}

public Entry find(Object key){
	for(Entry e = entries;e != null;e = e.rest())
		{
		if(equalKey(key,e._key))
			return e;
		}
	return null;
}

public IMap add(Object key){
	return put(key, null);
}

public IMap put(Object key, Object val){
	Entry remEntries = doRemove(entries, key);
	int incr = (remEntries == entries) ? 1 : 0;
	return new ListMap(_count + incr, Entry.create(key, val, remEntries));
}

Entry doRemove(Entry e, Object key){
	if(e == null)
		return null;
	else if(equalKey(key,e._key))
		{
		return e.rest();
		}
	else
		{
		Entry er = doRemove(e.rest(), key);
		if(er != e.rest()) //removed in tail
			return Entry.create(e._key, e._val, er);
		return e;
		}
}

public IMap remove(Object key){
	Entry remEntries = doRemove(entries, key);
	if(remEntries == entries) //didn't have key
		return this;
	return new ListMap(_count - 1, remEntries);
}

public Object get(Object key){
	Entry e = find(key);
	if(e != null)
		return e._val;
	return null;
}

public int capacity(){
	return count();
}

static class Iter implements Iterator{
	Entry e;

	Iter(Entry e)
	{
	this.e = e;
	}

	public boolean hasNext(){
		return e != null;
	}

	public Object next(){
		Entry ret = e;
		e = e.rest();
		return ret;
	}

	public void remove(){
		throw new UnsupportedOperationException();
	}
}

public Iterator iterator(){
	return new Iter(entries);
}

boolean equalKey(Object k1,Object k2){
    if(k1 == null)
        return k2 == null;
    return k1.equals(k2);
}
}
