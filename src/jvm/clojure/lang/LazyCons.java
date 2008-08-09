/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Aug 9, 2008 */

package clojure.lang;

public class LazyCons extends ASeq{
IFn _firstFn;
Object _first;
IFn _restFn;
ISeq _rest;

public LazyCons(IFn firstFn, IFn restFn){
	this._firstFn = firstFn;
	this._restFn = restFn;
	this._first = null;
	this._rest = null;
}

LazyCons(IPersistentMap meta, Object first, ISeq rest){
	super(meta);
	this._first = first;
	this._rest = rest;
}

synchronized public Object first(){
	if(_firstFn != null)
		{
		try
			{
			_first = _firstFn.invoke();
			}
		catch(Exception ex)
			{
			throw new RuntimeException(ex);
			}
		_firstFn = null;
		}
	return _first;
}

synchronized public ISeq rest(){
	//force sequential evaluation
	first();
	if(_restFn != null)
		{
		try
			{
			_rest = RT.seq(_restFn.invoke());
			}
		catch(Exception ex)
			{
			throw new RuntimeException(ex);
			}
		_restFn = null;
		}
	return _rest;
}

synchronized public LazyCons withMeta(IPersistentMap meta){
	if(meta == meta())
		return this;
	//force before copying
	rest();
	return new LazyCons(meta, _first, _rest);
}

}
