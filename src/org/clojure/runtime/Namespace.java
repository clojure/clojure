/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 27, 2006 1:29:39 PM */

package org.clojure.runtime;

import java.util.HashMap;

public class Namespace{

/**
 * String->Namespace
 */
static final public HashMap table = new HashMap();

/**
 * String->Symbol
 */
final public HashMap symbols = new HashMap();
final public String name;

Namespace(String name)
	{
	this.name = name;
	}

static public Namespace find(String name)
	{
	return (Namespace) table.get(name);
	}

static public Namespace findOrCreate(String name)
	{
	synchronized(table)
		{
		Namespace ns = find(name);
		if(ns == null)
			table.put(name, ns = new Namespace(name));
		return ns;
		}
	}

public Symbol intern(String name)
	{
	synchronized(symbols)
		{
	    Symbol sym = (Symbol) symbols.get(name);
		if(sym == null)
			symbols.put(name, sym = new Symbol(name, this));
		return sym;
		}
	}
}
