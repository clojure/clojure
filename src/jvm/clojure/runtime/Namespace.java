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

package clojure.runtime;

import java.util.HashMap;
import java.util.IdentityHashMap;

public class Namespace{

/**
 * String->Namespace
 */
static final public HashMap table = new HashMap();

/**
 * Symbol->Var
 */
final public IdentityHashMap vars = new IdentityHashMap();
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

static public Var intern(String ns,String name)
    {
    return findOrCreate(ns).intern(Symbol.intern(name));
    }


public Var intern(Symbol sym)
    {
    synchronized(vars)
        {
        Var var = (Var) vars.get(sym);
        if(var == null)
            vars.put(sym, var = new Var(sym, this));
        return var;
        }
    }

}
