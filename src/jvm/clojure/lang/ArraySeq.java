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

public class ArraySeq implements IndexedSeq{
final Object[] array;
final int i;

static public ArraySeq create(Object[] array){
	if(array.length == 0)
		return null;
	return new ArraySeq(array, 0);
}

ArraySeq(Object[] array, int i){
	this.array = array;
	this.i = i;
}

public Object first() {
	return array[i];
}

public ISeq rest() {
	if(i+1 < array.length)
		return new ArraySeq(array, i + 1);
	return null;
}

public int index(){
	return i;
}
}
