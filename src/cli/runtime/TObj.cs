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
TRef attrs;

public TObj() {
    this.attrs = Transaction.tref(new PersistentTree());
}

public Object put( IComparable key, Object val)  {
    PersistentTree t = (PersistentTree) Transaction.get2( attrs);
	t = (PersistentTree) t.put(key, val);
    Transaction.set2(attrs,t);
    return val;
}

public Object get( IComparable key)  {
    PersistentTree t = (PersistentTree) Transaction.get2( attrs);
    return t.get(key);
}

public bool has( IComparable key)  {
    PersistentTree t = (PersistentTree) Transaction.get2( attrs);
    return t.contains(key);
}
}
}
