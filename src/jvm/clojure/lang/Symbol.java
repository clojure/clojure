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

package clojure.lang;


public class Symbol
{
//this must be an interned string!
public final String name;

public String toString()
    {
    return name;
    }

public Symbol(String name)
    {
    this.name = name.intern();
    }

public boolean equals(Object o){
	if(this == o)
		return true;
	if(o == null || getClass() != o.getClass())
		return false;

	Symbol symbol = (Symbol) o;

	//identity compare ok, names are interned
	return name == symbol.name;
}

public int hashCode(){
	return name.hashCode();
}

/*
public static Symbol intern(String name)
    {
    synchronized(table)
        {
        Symbol sym = (Symbol) table.get(name);
        int dot = 0;
        if(sym == null)
            {
            if(name.charAt(0) == ':')
                sym = new Keyword(name);
            else if((dot = name.indexOf('.')) != -1)
                {
                if(dot == 0)
                    sym = new InstanceMemberSymbol(name);
                else if(name.lastIndexOf('.') == name.length() - 1)
                    sym = new ClassSymbol(name);
                else
                    sym = new StaticMemberSymbol(name);
                }
            else
                sym = new Symbol(name);
            if(table.get(name) != null) //defend against recursive static init
                return (Symbol) table.get(name);
            table.put(name, sym);
            }
        return sym;
        }
    }


 */
}
