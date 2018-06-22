/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 25, 2006 11:42:47 AM */

package clojure.lang;

import java.io.Serializable;
import java.io.ObjectStreamException;


public class Symbol extends AFn implements IObj, Comparable, Named, Serializable, IHashEq{
final String ns;
final String name;
private int _hasheq;
final IPersistentMap _meta;
transient String _str;

public String toString(){
	if(_str == null){
		if(ns != null)
			_str = (ns + "/" + name);
		else
			_str = name;
	}
	return _str;
}

public String getNamespace(){
	return ns;
}

public String getName(){
	return name;
}

// the create thunks preserve binary compatibility with code compiled
// against earlier version of Clojure and can be removed (at some point).
static public Symbol create(String ns, String name) {
    return Symbol.intern(ns, name);
}

static public Symbol create(String nsname) {
    return Symbol.intern(nsname);
}
    
static public Symbol intern(String ns, String name){
	return new Symbol(ns, name);
}

static public Symbol intern(String nsname){
	int i = nsname.indexOf('/');
	if(i == -1 || nsname.equals("/"))
		return new Symbol(null, nsname);
	else
		return new Symbol(nsname.substring(0, i), nsname.substring(i + 1));
}

private Symbol(String ns_interned, String name_interned){
	this.name = name_interned;
	this.ns = ns_interned;
	this._meta = null;
}

public boolean equals(Object o){
	if(this == o)
		return true;
	if(!(o instanceof Symbol))
		return false;

	Symbol symbol = (Symbol) o;

	return Util.equals(ns,symbol.ns) && name.equals(symbol.name);
}

public int hashCode(){
	return Util.hashCombine(name.hashCode(), Util.hash(ns));
}

public int hasheq() {
	if(_hasheq == 0){
		_hasheq = Util.hashCombine(Murmur3.hashUnencodedChars(name), Util.hash(ns));
	}
	return _hasheq;
}

public IObj withMeta(IPersistentMap meta){
	if(meta() == meta)
		return this;
	return new Symbol(meta, ns, name);
}

private Symbol(IPersistentMap meta, String ns, String name){
	this.name = name;
	this.ns = ns;
	this._meta = meta;
}

public int compareTo(Object o){
	Symbol s = (Symbol) o;
	if(this.equals(o))
		return 0;
	if(this.ns == null && s.ns != null)
		return -1;
	if(this.ns != null)
		{
		if(s.ns == null)
			return 1;
		int nsc = this.ns.compareTo(s.ns);
		if(nsc != 0)
			return nsc;
		}
	return this.name.compareTo(s.name);
}

private Object readResolve() throws ObjectStreamException{
	return intern(ns, name);
}

public Object invoke(Object obj) {
	return RT.get(obj, this);
}

public Object invoke(Object obj, Object notFound) {
	return RT.get(obj, this, notFound);
}

public IPersistentMap meta(){
	return _meta;
}
}
