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

import java.util.concurrent.ConcurrentHashMap;

public class Module{

/**
 * String->Module
 */
static final public ConcurrentHashMap<String, Module> table = new ConcurrentHashMap();

/**
 * Symbol->Var
 */
final public TRef<IPersistentMap> vars = new TRef(PersistentHashMap.EMPTY);
final public String name;

Module(String name){
	this.name = name;
}

static public Module findModule(String name){
	return table.get(name);
}

static public Module findOrCreateModule(String name){
	Module module = findModule(name);
	if(module == null)
		module = table.putIfAbsent(name, new Module(name));
	return module;
}

public TRef findRef(String name) throws Exception{
	return (TRef) vars.val().valAt(name);
}

public static TRef intern(String moduleName, String name) throws Exception{
	Module module = findModule(moduleName);
	if(module == null)
		throw new Exception(String.format("Module %s not found", moduleName));
	return module.intern(name);
}

public TRef intern(String name) throws Exception{
	//must be called in transaction
	IPersistentMap varmap = vars.val();
	TRef var = (TRef) varmap.valAt(name);
	if(var == null)
		vars.set(varmap.assoc(name, var = new TRef()));
	return var;
}

}
