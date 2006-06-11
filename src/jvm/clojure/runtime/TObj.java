/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

package org.clojure.runtime;

public class TObj implements IObj{
TRef attrs;

public TObj(ThreadLocalData tld) throws Exception{
    this.attrs = Transaction.tref(tld,new RBTree());
}

public Object put(ThreadLocalData tld, Comparable key, Object val) throws Exception {
    RBTree t = (RBTree) Transaction.get(tld, attrs);
    t = t.put(key, val);
    Transaction.set(tld,attrs,t);
    return val;
}

public Object get(ThreadLocalData tld, Comparable key) throws Exception {
    RBTree t = (RBTree) Transaction.get(tld, attrs);
    return t.get(key);
}

public boolean has(ThreadLocalData tld, Comparable key) throws Exception {
    RBTree t = (RBTree) Transaction.get(tld, attrs);
    return t.contains(key);
}
}
