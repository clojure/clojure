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
using System.Collections;

namespace clojure.lang
{
public class Symbol 
//	: Obj, IComparable
{

static public readonly Hashtable table = new Hashtable(1001);
//static public readonly Hashtable hashes = new Hashtable(1001);
//static readonly Random rand = new Random(42);


public readonly String name;
//int hash = 0;

override public String ToString()
	{
	return name;
	}

public static Symbol intern(String name)	{	lock(table)		{		Symbol sym = (Symbol) table[name];
		int dot = 0;
		if (sym == null)
			{
			if (name[0] == ':')
				sym = new Keyword(name);
			else if ((dot = name.IndexOf('.')) != -1)
				{
				if (dot == 0)
					sym = new InstanceMemberSymbol(name);
				else if (name.LastIndexOf('.') == name.Length - 1)
					sym = new ClassSymbol(name);
				else
					sym = new StaticMemberSymbol(name);
				}
			else
				sym = new Symbol(name);		    if(table[name] != null) //defend against recursive static init
				return (Symbol)table[name];			table.Add(name, sym);			}		return sym;		}	}
/**
 * Used by Module.intern()
 * @param name
 * @param ns
 */
internal Symbol(String name)
	{
	this.name = name;
	}

/*
public override int GetHashCode()
    {
     if(hash == 0)
         {
         lock (hashes)
             {
             while (hash == 0)
                 {
                 int h = rand.Next();
                 if (h != 0 && !hashes.ContainsKey(h))
                     {
                     hash = h;
                     hashes.Add(h,null);
                     }
                 }
             }
         }
     return hash;
    }


#region IComparable Members

public int CompareTo(object obj)
    {
    return GetHashCode() - ((Symbol)obj).GetHashCode();
    }

#endregion

override public Obj withMeta(IPersistentMap meta) {
    this._meta = meta;
    return this;
}
*/
    }
}
