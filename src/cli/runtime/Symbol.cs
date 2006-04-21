/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 25, 2006 11:42:47 AM */

using System;
using System.Collections.Specialized;

namespace org.clojure.runtime
{
public class Symbol : AMap{

static public HybridDictionary table = new HybridDictionary();


public String name; //const is not equivalent to Java final with init elsewhere

public String toString()
	{
	return name;
	}

public static Symbol intern(String name)	{	lock(table)		{		Symbol sym = (Symbol) table[name];		if(sym == null)			table.Add(name, sym = new Symbol(name));		return sym;		}	}
/**
 * Used by Namespace.intern()
 * @param name
 * @param ns
 */
internal Symbol(String name)
	{
	this.name = name;
	}


}
}
