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

public class PersistentList extends ASeq {

private final Object _first;
private final PersistentList _rest;
private final int _count;

public PersistentList(Object first) {
    this._first = first;
    this._rest = null;

    this._count = 1;
}

PersistentList(Object first, PersistentList rest) {
    this._first = first;
    this._rest = rest;

    this._count = 1 + rest.count();
    this._meta = rest._meta;
}

public Object first() {
    return _first;
}

public ISeq rest() {
    return _rest;
}

public int count() {
    return _count;
}

public ISeq cons(Object o) {
    return new PersistentList(o,this);
}

}
