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
final public HashMap accessors = new HashMap();
final public HashMap vars = new HashMap();
final public String name;

Namespace(String name)
	{
	this.name = name;
	table.put(name, this);
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
			ns = new Namespace(name);
		return ns;
		}
	}

public Var internVar(String name)
	{
	synchronized(vars)
		{
		Var var = (Var) vars.get(name);
		if(var == null)
			vars.put(name, var = new Var(name, this));
		return var;
		}
	}

public Accessor internAccessor(String name)
	{
	synchronized(accessors)
		{
		Accessor acc = (Accessor) accessors.get(name);
		if(acc == null)
			accessors.put(name, acc = new Accessor(name, this));
		return acc;
		}
	}
}
