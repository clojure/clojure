/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 25, 2006 3:44:58 PM */

using System;

namespace clojure.lang
{

public class Obj{

volatile IPersistentMap _attrs = PersistentArrayIdentityMap.EMPTY;

public Object addAttr( Object key, Object val)
	{
	_attrs = _attrs.add(key, val);
	return val;
	}

public Object getAttr( Object key)
	{
	return _attrs.get(key);
	}

public bool hasAttr( Object key){
    return _attrs.contains(key);
    }


public IPersistentMap attrs() {
    return _attrs;
}

}

}