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

using System;
using System.Collections.Specialized;

namespace clojure.lang
{

public class Namespace
{

/**
 * String->Namespace
 */
static public HybridDictionary table = new HybridDictionary();

/**
 * Symbol->Var
 */
public HybridDictionary vars = new HybridDictionary();
public String name;


Namespace(String name)
	{
	this.name = name;
	table.Add(name, this);
	}

static public Namespace find(String name)
	{
	return (Namespace) table[name];
	}

static public Namespace findOrCreate(String name)
	{
	lock(table)
		{
		Namespace ns = find(name);
		if(ns == null)
			ns = new Namespace(name);
		return ns;
		}
	}

static public Var intern(String ns, String name)
    {
        return findOrCreate(ns).intern(Symbol.intern(name));
    }

public Var intern(Symbol sym)	{	lock(vars)		{		Var var = (Var) vars[sym];		if(var == null)			vars.Add(sym,var = new Var(sym, this));		return var;		}	}
}
}
