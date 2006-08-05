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
public class EnumeratorSeq : ASeq{
IEnumerator e;
volatile ISeq _rest;

public static EnumeratorSeq create(IEnumerator e){
    if(e.MoveNext())
        return new EnumeratorSeq(e);
    return null;
}

EnumeratorSeq(IEnumerator e){
    this.e = e;
    this._rest = this;
}

override public Object first() {
    return e.Current;
}

override public ISeq rest() {
    if(_rest == this)
        {
        lock(this){
            if(_rest == this)
                {
                _rest = create(e);
                }
            }
        }
    return _rest;
}
}

}
