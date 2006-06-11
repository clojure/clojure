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

public class TObj implements IObj{
TRef _attrs;

public TObj() throws Exception{
    this._attrs = Transaction.tref(PersistentArrayIdentityMap.EMPTY);
}


public Object put( Object key, Object val) throws Exception {
    IPersistentMap t = (IPersistentMap) Transaction.get( _attrs);
    t = t.put(key, val);
    Transaction.set(_attrs,t);
    return val;
}

public Object get( Object key) throws Exception {
    IPersistentMap t = (IPersistentMap) Transaction.get( _attrs);
    return t.get(key);
}

public boolean has( Object key) throws Exception {
    IPersistentMap t = (IPersistentMap) Transaction.get( _attrs);
    return t.contains(key);
}

public IPersistentMap attrs() throws Exception {
    return (IPersistentMap) Transaction.get(_attrs);
}

public void remove(Object key) throws Exception {
    IPersistentMap t = (IPersistentMap) Transaction.get( _attrs);
    t = t.remove(key);
    Transaction.set(_attrs,t);
}
}
