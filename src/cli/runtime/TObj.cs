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

public class TObj : IObj{
TRef _attrs;

public TObj(){
    this._attrs = Transaction.tref(PersistentArrayIdentityMap.EMPTY);
}


public Object put( Object key, Object val) {
    IPersistentMap t = (IPersistentMap) Transaction.get( _attrs);
    t = t.put(key, val);
    Transaction.set(_attrs,t);
    return val;
}

public Object get( Object key) {
    IPersistentMap t = (IPersistentMap) Transaction.get( _attrs);
    return t.get(key);
}

public bool has( Object key) {
    IPersistentMap t = (IPersistentMap) Transaction.get( _attrs);
    return t.contains(key);
}

public IPersistentMap attrs() {
    return (IPersistentMap) Transaction.get(_attrs);
}

public void remove(Object key) {
    IPersistentMap t = (IPersistentMap) Transaction.get( _attrs);
    t = t.remove(key);
    Transaction.set(_attrs,t);
}
}
}
