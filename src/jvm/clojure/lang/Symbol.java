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


public class Symbol extends Obj{
//these must be interned strings!
public final String ns;
public final String name;
final int hash;

public String toString(){
	if(ns != null)
		return ns + "/" + name;
	return name;
}

static public Symbol intern(String ns, String name){
	return new Symbol(ns == null ? null : ns.intern(), name.intern());
}

static public Symbol intern(String nsname){
	int i = nsname.indexOf('/');
	if(i == -1)
		return new Symbol(null, nsname.intern());
	else
		return new Symbol(nsname.substring(0, i).intern(), nsname.substring(i + 1).intern());
}

static public Symbol create(String name_interned){
	return new Symbol(null, name_interned);
}

static public Symbol create(String ns_interned, String name_interned){
	return new Symbol(ns_interned, name_interned);
}

private Symbol(String ns_interned, String name_interned){
	this.name = name_interned;
	this.ns = ns_interned;
	this.hash = RT.hashCombine(name.hashCode(), RT.hash(ns));
}

public boolean equals(Object o){
	if(this == o)
		return true;
	if(!(o instanceof Symbol))
		return false;

	Symbol symbol = (Symbol) o;

	//identity compares intended, names are interned
	return name == symbol.name && ns == symbol.ns;
}

public int hashCode(){
	return hash;
}

public Obj withMeta(IPersistentMap meta){
	return new Symbol(meta, ns, name);
}

private Symbol(IPersistentMap meta, String ns, String name){
	super(meta);
	this.name = name;
	this.ns = ns;
	this.hash = RT.hashCombine(name.hashCode(), RT.hash(ns));
}
}
