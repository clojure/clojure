/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Aug 19, 2007 */

package clojure.lang;

import java.util.List;

public class ArgVector extends Obj implements IPersistentArray, IPersistentList{

final PersistentVector impl;

public static final ArgVector EMPTY = new ArgVector(PersistentVector.EMPTY);

static public ArgVector create(ISeq items){
	return new ArgVector(PersistentVector.create(items));
}

static public ArgVector create(List items){
	return new ArgVector(PersistentVector.create(items));
}

static public ArgVector create(Object... items){
	return new ArgVector(PersistentVector.create(items));
}

private ArgVector(PersistentVector impl){
	this.impl = impl;
}

private ArgVector(IPersistentMap meta, PersistentVector impl){
	super(meta);
	this.impl = impl;
}

public Obj withMeta(IPersistentMap meta){
	if(meta != this.meta())
		return new ArgVector(meta, impl);
	return this;
}

public int length(){
	return impl.length();
}

public Object nth(int i){
	return impl.nth(i);
}

public IPersistentArray assocN(int i, Object val){
	return new ArgVector(meta(), impl.assocN(i, val));
}

public boolean contains(Object key){
	return impl.contains(key);
}

public IMapEntry entryAt(Object key){
	return impl.entryAt(key);
}

public Associative assoc(Object key, Object val){
	return new ArgVector(meta(), impl.assoc(key, val));
}

public Object valAt(Object key){
	return impl.valAt(key);
}

public int count(){
	return impl.count();
}

public ISeq seq(){
	return impl.seq();
}

public IPersistentCollection cons(Object o){
	return new ArgVector(meta(), impl.cons(o));
}

public Object peek(){
	return impl.peek();
}

public IPersistentList pop(){
	return new ArgVector(meta(), impl.pop());
}
}
