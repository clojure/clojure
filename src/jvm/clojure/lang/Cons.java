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

import java.io.Serializable;

final public class Cons extends ASeq implements Serializable {

private final Object _first;
private final ISeq _more;

public Cons(Object first, ISeq _more){
	this._first = first;
	this._more = _more;
}


public Cons(IPersistentMap meta, Object _first, ISeq _more){
	super(meta);
	this._first = _first;
	this._more = _more;
}

public Object first(){
	return _first;
}

public ISeq next(){
	return more().seq();
}

public ISeq more(){
	if(_more == null)
		return PersistentList.EMPTY;
	return _more;
}

public int count(){
	return 1 + RT.count(_more);
}

public Cons withMeta(IPersistentMap meta){
	return new Cons(meta, _first, _more);
}
}
