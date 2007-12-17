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
IFn _restFn;
//volatile 
ISeq _rest;

public FnSeq(Object first, IFn restFn){
	this._first = first;
	this._restFn = restFn;
	this._rest = this;
}

public FnSeq(IPersistentMap meta, Object first, IFn restFn, ISeq rest){
	super(meta);
	this._first = first;
	this._rest = rest;
	this._restFn = restFn;
}

public Object first(){
	return _first;
}

public ISeq rest(){
//	if(_restFn != null)
		synchronized(this)
			{
			if(_restFn != null)
				{
				try
					{
					_rest = (ISeq) _restFn.invoke();
					}
				catch(Exception ex)
					{
					throw new Error(ex);
					}
				_restFn = null;
				}
			return _rest;
			}
}

public FnSeq withMeta(IPersistentMap meta){
	if(meta == meta())
		return this;
	return new FnSeq(meta, _first, _restFn, _rest);
}

}
