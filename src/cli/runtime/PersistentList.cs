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

public class PersistentList : ASeq {

private readonly Object _first;
private readonly PersistentList _rest;
private readonly int _count;

public PersistentList(Object first) {
    this._first = first;
    this._rest = null;

    this._count = 1;
}

private PersistentList(Object first, PersistentList rest) {
    this._first = first;
    this._rest = rest;

    this._count = 1 + rest.count();
    this._meta = rest._meta;
}

override public Object first() {
    return _first;
}

override public ISeq rest() {
    return _rest;
}

override public int count() {
    return _count;
}

override public IPersistentCollection cons(Object o) {
    return new PersistentList(o,this);
}

}

}
