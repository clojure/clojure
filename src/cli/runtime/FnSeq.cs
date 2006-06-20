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

namespace clojure.lang
{

public class FnSeq : ISeq{

Object _first;
IFn restFn;
volatile ISeq _rest;

public FnSeq(Object first, IFn restFn) {
    this._first = first;
    this.restFn = restFn;
	this._rest = this;
	}

public Object first() {
    return _first;
}

public ISeq rest() {
    if(_rest != this)
        return _rest;
    lock(this){
        if(_rest == this)
            {
            _rest = (ISeq) restFn.invoke();
            restFn = null;
            }
        return _rest;
		}
    }
}

}
