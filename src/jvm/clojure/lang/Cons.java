/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 25, 2006 11:01:29 AM */

package clojure.lang;

public class Cons extends ASeq{

private final Object _first;
private final ISeq _rest;

public Cons(Object first, ISeq rest){
	this._first = first;
	this._rest = rest;
}


public Cons(IPersistentMap meta, Object _first, ISeq _rest){
	super(meta);
	this._first = _first;
	this._rest = _rest;
}

public Object first(){
	return _first;
}

public ISeq rest(){
	return _rest;
}

public ISeq seq(){
	return this;
}

public Cons withMeta(IPersistentMap meta){
	return new Cons(meta, _first, _rest);
}
}
