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

package clojure.lang;

import java.util.HashMap;
import java.util.IdentityHashMap;

public class Module{

/**
 * String->Module
 */
static final public TRef<IPersistentMap> table = new TRef(PersistentHashMap.EMPTY);

/**
 * Symbol->Var
 */
final public TRef<IPersistentMap> vars = new TRef(PersistentHashMap.EMPTY);
final public String name;

Module(String name){
	this.name = name;
}

static public Module find(String name) throws Exception{
	return (Module) table.get().get(name);
}

static public Module findOrCreate(String name) throws Exception{
	//must be called in transaction
	Module ns = find(name);
	if(ns == null)
		table.set(table.get().assoc(name, ns = new Module(name)));
	return ns;
}

static public Var intern(String ns, String name) throws Exception{
	return findOrCreate(ns).intern(new Symbol(name));
}

public Var find(Symbol sym) throws Exception{
	return (Var) vars.get().get(sym);
}

public Var intern(Symbol sym) throws Exception{
	//must be called in transaction
	IPersistentMap varmap = vars.get();
	Var var = (Var) varmap.get(sym);
	if(var == null)
		vars.set(varmap.assoc(sym, var = new Var(sym, this)));
	return var;
}

}
