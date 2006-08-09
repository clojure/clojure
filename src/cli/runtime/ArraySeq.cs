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

using System;

namespace clojure.lang
{

public class ArraySeq : ASeq, IndexedSeq{
readonly Object[] array;
readonly int i;
//ISeq _rest;

static public ArraySeq create(params Object[] array){
	if(array.Length == 0)
		return null;
	return new ArraySeq(array, 0);
}

ArraySeq(Object[] array, int i){
	this.array = array;
	this.i = i;
//    this._rest = this;
}

override public Object first() {
	return array[i];
}

override public ISeq rest() {
	if (i + 1 < array.Length)
		return new ArraySeq(array, i + 1);
	return null;

//    if(_rest == this)
//        {
//        if(i+1 < array.Length)
//		    _rest = new ArraySeq(array, i + 1);
//	    _rest = null;
//        }
//    return _rest;
}

public int index(){
	return i;
}
}

}
