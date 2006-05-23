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

package org.clojure.runtime;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Random;

public class Symbol extends AMap implements Comparable{

final public static HashMap table = new HashMap();
final public static HashSet hashes = new HashSet();
final static Random rand = new Random(42);

public final String name;
int hash = 0;

public String toString()
    {
    return name;
    }

public static Symbol intern(String name)
    {
    synchronized(table)
        {
        Symbol sym = (Symbol) table.get(name);
        if(sym == null)
            {
            if(name.charAt(0) == ':')
                sym = new Keyword(name);
            else if(name.charAt(0) == '.')
                sym = new Accessor(name);
            else
                sym = new Symbol(name);
            table.put(name, sym);
            }
        return sym;
        }
    }

/**
 * Used by intern()
 * @param name
 */
Symbol(String name)
    {
    this.name = name;
    }

 public int hashCode(){
     if(hash == 0)
         {
         synchronized (hashes)
             {
             while (hash == 0)
                 {
                 Integer h = new Integer(rand.nextInt());
                 if (h.intValue() != 0 && !hashes.contains(h))
                     {
                     hash = h.intValue();
                     hashes.add(h);
                     }
                 }
             }
         }
     return hash;
 }


public int compareTo(Object o) {
    return hashCode() - ((Symbol)o).hashCode();
}
}
