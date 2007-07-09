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

public class FnSeq extends ASeq{

final Object _first;
final Delay _rest;

public FnSeq(Object first, Delay rest){
	this._first = first;
	this._rest = rest;
}

public FnSeq(IPersistentMap meta, Object first, Delay rest){
	super(meta);
	this._first = first;
	this._rest = rest;
}

public Object first(){
	return _first;
}

public ISeq rest(){
	try
		{
		return (ISeq) _rest.force();
		}
	catch(Exception e)
		{
		throw new Error(e.toString());
		}
}

public FnSeq withMeta(IPersistentMap meta){
	return new FnSeq(meta, _first, _rest);
}
}
