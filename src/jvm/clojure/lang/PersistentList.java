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

public class PersistentList extends ASeq{

private final Object _first;
private final PersistentList _rest;
private final int _count;

final public static PersistentList EMPTY = new EmptyList(null);

public PersistentList(Object first){
	this._first = first;
	this._rest = null;

	this._count = 1;
}

PersistentList(Object first, PersistentList rest){
	this._first = first;
	this._rest = rest;

	this._count = 1 + rest.count();
}

PersistentList(IPersistentMap meta, Object _first, PersistentList _rest, int _count){
	super(meta);
	this._first = _first;
	this._rest = _rest;
	this._count = _count;
}

public Object first(){
	return _first;
}

public ISeq rest(){
	return _rest;
}

public int count(){
	return _count;
}

public ISeq cons(Object o){
	return new PersistentList(o, this);
}

public PersistentList withMeta(IPersistentMap meta){
	return new PersistentList(meta, _first, _rest, _count);
}

static class EmptyList extends PersistentList{

	EmptyList(IPersistentMap meta){
		super(meta, null, null, 0);
	}

	public ISeq cons(Object o){
		return new PersistentList(o, null);
	}

	public PersistentList withMeta(IPersistentMap meta){
		if(meta != meta())
			return new EmptyList(meta);
		return this;
	}

	public ISeq seq(){
		return null;
	}
}

}
