/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Jun 19, 2006 */

package clojure.lang;

import java.lang.reflect.Array;

public class ArraySeq extends ASeq implements IndexedSeq{
final Object array;
final int i;
//ISeq _rest;

static public ArraySeq create(){
	return null;
}

static public ArraySeq create(Object... array){
	if(array == null || array.length == 0)
		return null;
	return new ArraySeq(array, 0);
}

static ArraySeq createFromObject(Object array){
	if(array == null || Array.getLength(array) == 0)
		return null;
	return new ArraySeq(array, 0);
}

ArraySeq(Object array, int i){
	this.array = array;
	this.i = i;
//    this._rest = this;
}

ArraySeq(IPersistentMap meta, Object array, int i){
	super(meta);
	this.array = array;
	this.i = i;
}

public Object first(){
	return Array.get(array,i);
}

public ISeq rest(){
	if(i + 1 < Array.getLength(array))
		return new ArraySeq(array, i + 1);
	return null;
//    if(_rest == this)
//        {
//        if(i+1 < array.length)
//		    _rest = new ArraySeq(array, i + 1);
//	    _rest = null;
//        }
//    return _rest;
}

public int count(){
	return Array.getLength(array) - i;
}

public int index(){
	return i;
}

public ArraySeq withMeta(IPersistentMap meta){
	return new ArraySeq(meta, array, i);
}

public Object reduce(IFn f) throws Exception{
	Object ret = Array.get(array,i);
	for(int x = i+1;x < Array.getLength(array);x++)
		ret = f.invoke(ret, Array.get(array,x));
	return ret;
}

public Object reduce(IFn f, Object start) throws Exception{
	Object ret = f.invoke(start,Array.get(array,i));
	for(int x = i+1;x < Array.getLength(array);x++)
		ret = f.invoke(ret, Array.get(array,x));
	return ret;
}
}
