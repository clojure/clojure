/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Jan 23, 2008 */

package clojure.lang;

import java.io.ObjectStreamException;
import java.io.Serializable;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicReference;

public class Namespace extends AReference implements Serializable {
final public Symbol name;
transient final AtomicReference<IPersistentMap> mappings = new AtomicReference<IPersistentMap>();
transient final AtomicReference<IPersistentMap> aliases = new AtomicReference<IPersistentMap>();

final static ConcurrentHashMap<Symbol, Namespace> namespaces = new ConcurrentHashMap<Symbol, Namespace>();

public String toString(){
	return name.toString();
}

Namespace(Symbol name){
	super(name.meta());
	this.name = name;
	mappings.set(RT.DEFAULT_IMPORTS);
	aliases.set(RT.map());
}

public static ISeq all(){
	return RT.seq(namespaces.values());
}

public Symbol getName(){
	return name;
}

public IPersistentMap getMappings(){
	return mappings.get();
}

public Var intern(Symbol sym){
	if(sym.ns != null)
		{
		throw new IllegalArgumentException("Can't intern namespace-qualified symbol");
		}
	IPersistentMap map = getMappings();
	Object o;
	Var v = null;
	while((o = map.valAt(sym)) == null)
		{
		if(v == null)
			v = new Var(this, sym);
		IPersistentMap newMap = map.assoc(sym, v);
		mappings.compareAndSet(map, newMap);
		map = getMappings();
		}
	if(o instanceof Var && ((Var) o).ns == this)
		return (Var) o;

	if(v == null)
		v = new Var(this, sym);

	warnOrFailOnReplace(sym, o, v);


	while(!mappings.compareAndSet(map, map.assoc(sym, v)))
		map = getMappings();

	return v;
}

private void warnOrFailOnReplace(Symbol sym, Object o, Object v){
    if (o instanceof Var)
        {
        Namespace ns = ((Var)o).ns;
        if (ns == this)
            return;
        if (ns != RT.CLOJURE_NS)
            throw new IllegalStateException(sym + " already refers to: " + o + " in namespace: " + name);
        }
	RT.errPrintWriter().println("WARNING: " + sym + " already refers to: " + o + " in namespace: " + name
		+ ", being replaced by: " + v);
}

Object reference(Symbol sym, Object val){
	if(sym.ns != null)
		{
		throw new IllegalArgumentException("Can't intern namespace-qualified symbol");
		}
	IPersistentMap map = getMappings();
	Object o;
	while((o = map.valAt(sym)) == null)
		{
		IPersistentMap newMap = map.assoc(sym, val);
		mappings.compareAndSet(map, newMap);
		map = getMappings();
		}
	if(o == val)
		return o;

	warnOrFailOnReplace(sym, o, val);

	while(!mappings.compareAndSet(map, map.assoc(sym, val)))
		map = getMappings();

	return val;

}

public static boolean areDifferentInstancesOfSameClassName(Class cls1, Class cls2) {
    return (cls1 != cls2) && (cls1.getName().equals(cls2.getName()));
}

Class referenceClass(Symbol sym, Class val){
    if(sym.ns != null)
        {
        throw new IllegalArgumentException("Can't intern namespace-qualified symbol");
        }
    IPersistentMap map = getMappings();
    Class c = (Class) map.valAt(sym);
    while((c == null) || (areDifferentInstancesOfSameClassName(c, val)))
        {
        IPersistentMap newMap = map.assoc(sym, val);
        mappings.compareAndSet(map, newMap);
        map = getMappings();
        c = (Class) map.valAt(sym);
        }
    if(c == val)
        return c;

    throw new IllegalStateException(sym + " already refers to: " + c + " in namespace: " + name);
}

public void unmap(Symbol sym) {
	if(sym.ns != null)
		{
		throw new IllegalArgumentException("Can't unintern namespace-qualified symbol");
		}
	IPersistentMap map = getMappings();
	while(map.containsKey(sym))
		{
		IPersistentMap newMap = map.without(sym);
		mappings.compareAndSet(map, newMap);
		map = getMappings();
		}
}

public Class importClass(Symbol sym, Class c){
	return referenceClass(sym, c);

}

public Class importClass(Class c){
	String n = c.getName();
	return importClass(Symbol.intern(n.substring(n.lastIndexOf('.') + 1)), c);
}

public Var refer(Symbol sym, Var var){
	return (Var) reference(sym, var);

}

public static Namespace findOrCreate(Symbol name){
	Namespace ns = namespaces.get(name);
	if(ns != null)
		return ns;
	Namespace newns = new Namespace(name);
	ns = namespaces.putIfAbsent(name, newns);
	return ns == null ? newns : ns;
}

public static Namespace remove(Symbol name){
	if(name.equals(RT.CLOJURE_NS.name))
		throw new IllegalArgumentException("Cannot remove clojure namespace");
	return namespaces.remove(name);
}

public static Namespace find(Symbol name){
	return namespaces.get(name);
}

public Object getMapping(Symbol name){
	return mappings.get().valAt(name);
}

public Var findInternedVar(Symbol symbol){
	Object o = mappings.get().valAt(symbol);
	if(o != null && o instanceof Var && ((Var) o).ns == this)
		return (Var) o;
	return null;
}


public IPersistentMap getAliases(){
	return aliases.get();
}

public Namespace lookupAlias(Symbol alias){
	IPersistentMap map = getAliases();
	return (Namespace) map.valAt(alias);
}

public void addAlias(Symbol alias, Namespace ns){
	if (alias == null || ns == null)
		throw new NullPointerException("Expecting Symbol + Namespace");
	IPersistentMap map = getAliases();
	while(!map.containsKey(alias))
		{
		IPersistentMap newMap = map.assoc(alias, ns);
		aliases.compareAndSet(map, newMap);
		map = getAliases();
		}
	// you can rebind an alias, but only to the initially-aliased namespace.
	if(!map.valAt(alias).equals(ns))
		throw new IllegalStateException("Alias " + alias + " already exists in namespace "
		                                   + name + ", aliasing " + map.valAt(alias));
}

public void removeAlias(Symbol alias) {
	IPersistentMap map = getAliases();
	while(map.containsKey(alias))
		{
		IPersistentMap newMap = map.without(alias);
		aliases.compareAndSet(map, newMap);
		map = getAliases();
		}
}

private Object readResolve() throws ObjectStreamException {
    // ensures that serialized namespaces are "deserialized" to the
    // namespace in the present runtime
    return findOrCreate(name);
}
}
