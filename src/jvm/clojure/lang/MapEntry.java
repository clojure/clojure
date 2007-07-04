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

public class MapEntry extends APersistentMap implements IMapEntry{
final Object _key;
final Object _val;

public MapEntry(Object key, Object val){
	this._key = key;
	this._val = val;
}


public MapEntry(IPersistentMap meta, Object _key, Object _val){
	super(meta);
	this._key = _key;
	this._val = _val;
}

public Object key(){
	return _key;
}

public Object val(){
	return _val;
}

public boolean contains(Object key){
	return RT.equal(_key, key);
}

public IMapEntry entryAt(Object key){
	return RT.equal(_key, key) ? this : null;
}

public IPersistentMap assoc(Object key, Object val){
	if(RT.equal(_key, key))
		{
		if(_val == val)
			return this;
		return (MapEntry) new MapEntry(meta(),key, val);
		}
	return new PersistentArrayMap(meta(),_key, _val, key, val);
}

public Object valAt(Object key){
	return RT.equal(_key, key) ? _val : null;
}

public IPersistentMap assocEx(Object key, Object val) throws Exception{
	if(RT.equal(_key, key))
		throw new Exception("Key already present");
	return assoc(key, val);
}

public IPersistentMap without(Object key){
	if(RT.equal(_key, key))
		return (IPersistentMap) PersistentArrayMap.EMPTY.withMeta(meta());
	return this;
}

public int count(){
	return 1;
}

public Iterator iterator(){
	return new Iter(this);
}

static class Iter implements Iterator{
	MapEntry e;

	public Iter(MapEntry e){
		this.e = e;
	}

	public boolean hasNext(){
		return e != null;
	}

	public Object next(){
		Object ret = e;
		e = null;
		return ret;
	}

	public void remove(){
		throw new UnsupportedOperationException();
	}
}


public ISeq seq(){
	return new Seq(this);
}

static class Seq extends ASeq{
	final MapEntry e;

	public Seq(MapEntry e){
		this.e = e;
	}

	public Object first(){
		return e;
	}

	public ISeq rest(){
		return null;
	}

	public int count(){
		return 1;
	}
}
}
