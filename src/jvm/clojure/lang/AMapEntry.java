/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 1, 2008 */

package clojure.lang;

import java.io.StringWriter;

public abstract class AMapEntry extends APersistentVector implements IMapEntry{

public Object nth(int i){
	if(i == 0)
		return key();
	else if(i == 1)
		return val();
	else
		throw new IndexOutOfBoundsException();
}

private IPersistentVector asVector(){
	return LazilyPersistentVector.createOwning(key(), val());
}

public IPersistentVector assocN(int i, Object val){
	return asVector().assocN(i, val);
}

public int count(){
	return 2;
}

public ISeq seq(){
	return asVector().seq();
}

public IPersistentVector cons(Object o){
	return asVector().cons(o);
}

public IPersistentCollection empty(){
	return null;
}

public IPersistentStack pop(){
	return LazilyPersistentVector.createOwning(key());
}

public Object setValue(Object value){
	throw new UnsupportedOperationException();
}

/*

public boolean equals(Object obj){
	return APersistentVector.doEquals(this, obj);
}

public int hashCode(){
	//must match logic in APersistentVector
	return 31 * (31 + Util.hash(key())) + Util.hash(val());
//	return Util.hashCombine(Util.hashCombine(0, Util.hash(key())), Util.hash(val()));
}

public String toString(){
	StringWriter sw = new StringWriter();
	try
		{
		RT.print(this, sw);
		}
	catch(Exception e)
		{
		//checked exceptions stink!
		throw Util.sneakyThrow(e);
		}
	return sw.toString();
}

public int length(){
	return 2;
}

public Object nth(int i){
	if(i == 0)
		return key();
	else if(i == 1)
		return val();
	else
		throw new IndexOutOfBoundsException();
}

private IPersistentVector asVector(){
	return LazilyPersistentVector.createOwning(key(), val());
}

public IPersistentVector assocN(int i, Object val){
	return asVector().assocN(i, val);
}

public int count(){
	return 2;
}

public ISeq seq(){
	return asVector().seq();
}

public IPersistentVector cons(Object o){
	return asVector().cons(o);
}

public boolean containsKey(Object key){
	return asVector().containsKey(key);
}

public IMapEntry entryAt(Object key){
	return asVector().entryAt(key);
}

public Associative assoc(Object key, Object val){
	return asVector().assoc(key, val);
}

public Object valAt(Object key){
	return asVector().valAt(key);
}

public Object valAt(Object key, Object notFound){
	return asVector().valAt(key, notFound);
}

public Object peek(){
	return val();
}


public ISeq rseq() {
	return asVector().rseq();
}
*/

}
