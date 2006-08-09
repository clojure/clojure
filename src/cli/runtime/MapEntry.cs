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
public class MapEntry : APersistentMap , IMapEntry{
readonly Object _key;
readonly Object _val;

public MapEntry(Object key, Object val) {
    this._key = key;
    this._val = val;
}

public Object key() {
    return _key;
}

public Object val() {
    return _val;
}

override public bool contains(Object key) {
    return RT.equal(_key, key);
}

override public IMapEntry find(Object key) {
    return RT.equal(_key, key)?this:null;
}

override public Associative assoc(Object key, Object val) {
    if(RT.equal(_key, key))
        {
        if(_val == val)
            return this;
        return (MapEntry) new MapEntry(key, val).withMeta(_meta);
        }
    return (IPersistentMap) new PersistentArrayMap(_key,_val,key,val).withMeta(_meta);
}

override public Object get(Object key) {
    return RT.equal(_key, key)?_val:null;
}

override public IPersistentMap assocEx(Object key, Object val) {
    if(RT.equal(_key, key))
        throw new Exception("Key already present");
	return (IPersistentMap)assoc(key, val);
}

override public IPersistentMap without(Object key) {
    if(RT.equal(_key, key))
        return (IPersistentMap) PersistentArrayMap.EMPTY.withMeta(_meta);
    return this;
}

override public int count() {
    return 1;
}

override public IEnumerator GetEnumerator() {
    return new Iter(this);
}

class Iter : IEnumerator{
    MapEntry e;
    bool first = true;

    public Iter(MapEntry e) {
        this.e = e;
    }

#region IEnumerator Members

public object Current
	{
	get {return e;}
	}

public bool MoveNext()
	{
	if(first)
		{
		first = false;
		return true;
		}
	return false;
	}

public void Reset()
	{
	throw new Exception("The method or operation is not implemented.");
	}

#endregion
	}


override public ISeq seq()  {
    return new Seq(this);
}

class Seq : ASeq{
    readonly MapEntry e;

    public Seq(MapEntry e) {
        this.e = e;
    }

    override public Object first() {
        return e;
    }

    override public ISeq rest() {
        return null;
    }

	override public int count(){
		return 1;
	}
	
}
}

}
