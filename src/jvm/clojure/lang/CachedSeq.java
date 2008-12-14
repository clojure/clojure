/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Aug 10, 2008 */

package clojure.lang;

public class CachedSeq extends ASeq{
ISeq s;
Object _first;
ISeq _rest;

public CachedSeq(ISeq s){
	this.s = s;
	this._first = this;
	this._rest = this;
}

CachedSeq(IPersistentMap meta, Object first, ISeq rest){
	super(meta);
	this._first = first;
	this._rest = rest;
}

final
synchronized
public Object first(){
	if(_first == this)
		_first = s.first();
	return _first;
}

final
synchronized
public ISeq rest(){
	if(_rest == this)
		{
		//force sequential evaluation
		if(_first == this)
			first();
		ISeq rs = s.rest();
		if(rs == null)
			_rest = rs;
		else
			_rest = new CachedSeq(rs);
		s = null;
		}
	return _rest;
}

public CachedSeq withMeta(IPersistentMap meta){
	if(meta == meta())
		return this;
	//force before copying
	rest();
	return new CachedSeq(meta, _first, _rest);
}
}
