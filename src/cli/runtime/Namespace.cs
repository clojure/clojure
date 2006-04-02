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

namespace org.clojure.runtime
{

public class Namespace
{

/**
 * String->Namespace
 */
static public HybridDictionary table = new HybridDictionary();

/**
 * String->Symbol
 */
public HybridDictionary symbols = new HybridDictionary();
public String name;

static public Namespace globalNS = new Namespace("");
static public Namespace keywordNS = new KeywordNamespace("keyword");

class KeywordNamespace : Namespace{
	public KeywordNamespace(String name) : base(name)
	{
	
	}
	override public Symbol intern(String name)
		{
		Symbol sym = (Symbol) symbols[name];
		if(sym == null)
			symbols.Add(name, sym = new Keyword(name, this));
		return sym;
		}
};

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

virtual public Symbol intern(String name)
	{
	lock(symbols)
		{
		Symbol sym = (Symbol) symbols[name];
		if(sym == null)
			symbols.Add(name, sym = new Symbol(name, this));
		return sym;
		}
	}
}
}
