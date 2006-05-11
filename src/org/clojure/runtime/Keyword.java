/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 29, 2006 10:39:05 AM */

package org.clojure.runtime;

import java.util.HashMap;

public class Keyword extends Indexer{


final public static HashMap table = new HashMap();

public final String name;

public static Keyword intern(String name)
	{
	synchronized(table)
		{
		Keyword sym = (Keyword) table.get(name);
		if(sym == null)
			table.put(name, sym = new Keyword(name));
		return sym;
		}
	}
/**
 * Used by Namespace.intern()
 *
 * @param name

 */
Keyword(String name)
	{
	this.name = name;
	}

public String toString()
	{
	return ":" + name;
	}

/**
 *  Indexer implements IFn for attr access
 *  This single arg version is the getter
 * @param tld
 * @param obj - must be AMap
 * @return the value of the attr or nil if not found
 * @throws Exception
 */
public Object invoke(ThreadLocalData tld, Object obj) throws Exception
	{
    if (obj == null)
        return null;
	return ((AMap)obj).get(this);
	}

/**
 *  Indexer implements IFn for attr access
 *  This two arg version is the setter
 * @param tld
 * @param obj - must be AMap
 * @param val
 * @return val
 * @throws Exception
 */
public Object invoke(ThreadLocalData tld, Object obj, Object val) throws Exception
	{
	return ((AMap)obj).put(this,val);
	}
}
