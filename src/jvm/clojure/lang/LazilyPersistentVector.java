/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich May 14, 2008 */

package clojure.lang;

import java.util.Collection;

public class LazilyPersistentVector extends APersistentVector{
final Object[] array;
PersistentVector v = null;

static public IPersistentVector createOwning(Object... items){
	if(items.length == 0)
		return PersistentVector.EMPTY;
	return new LazilyPersistentVector(null, items, null);
}

static public IPersistentVector create(Collection coll){
	return createOwning(coll.toArray());
}

LazilyPersistentVector(IPersistentMap meta, Object[] array, PersistentVector v){
	super(meta);
	this.array = array;
	this.v = v;
}

public Object[] toArray(){
	return array.clone();
}

public Object nth(int i){
	return array[i];
}

public IPersistentVector assocN(int i, Object val){
	return (IPersistentVector) v().assoc(i, val);
}

public int count(){
	return array.length;
}

public IPersistentVector cons(Object o){
	return v().cons(o);
}

public IPersistentCollection empty(){
	return PersistentVector.EMPTY.withMeta(meta());	
}

public IPersistentStack pop(){
	return v().pop();
}

private synchronized IPersistentVector v(){
	if(v == null)
		{
		v = PersistentVector.create(array);
		}
	return v;
}

public Obj withMeta(IPersistentMap meta){
	if(meta != _meta)
		return new LazilyPersistentVector(meta, array, v);
	return this;
}

}
