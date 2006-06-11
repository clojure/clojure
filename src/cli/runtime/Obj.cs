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
using System.Collections.Specialized;

namespace clojure.lang
{

public class Obj : IObj
{

HybridDictionary attrs;
public static int INITIAL_SIZE = 7;

public Object put( IComparable key, Object val)
	{
	if(attrs == null)
		attrs = new HybridDictionary(INITIAL_SIZE);
	attrs[key] = val;
	return val;
	}

public Object get( IComparable key)
	{
	if(attrs == null)
		return null;
	return attrs[key];
	}

public bool has( IComparable key)
    {
    if (attrs == null)
        return false;
    return attrs.Contains(key);
    }
}
}