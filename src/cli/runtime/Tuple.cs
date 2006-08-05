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

public class Tuple : APersistentArray{

readonly Object[] array;

readonly public static Tuple EMPTY = new Tuple();

/**
 * This ctor captures/aliases the passed array, so do not modify later !
 * @param init {key1,val1,key2,val2,...}
 */
public Tuple(params Object[] init){
	this.array = init;
}

override public int count() {
    return array.Length;
}

override public int length() {
    return array.Length;
}

override public Object nth(int i){
    return array[i];
}


override public IPersistentArray assocN(int i, Object val) {
	Object[] newArray = (Object[])array.Clone();
	newArray[i] = val;
    return new Tuple(newArray);
}

override public bool Equals(Object key){
    if(this == key) return true;
    if(key == null || !(key is IPersistentArray)) return false;

    IPersistentArray a = (IPersistentArray) key;

    if(a.length() != array.Length)
        return false;

    for(int i = 0; i < array.Length; i++)
        {
        if(!equalKey(array[i],a.nth(i)))
            return false;
        }

    return true;
}

override public int GetHashCode(){
	int ret = 0;
	for(int i = 0; i < array.Length; i++)
		{
		Object o = array[i];
		if(o != null)
			ret ^= o.GetHashCode();
		}
	return ret;
}

private bool equalKey(Object k1,Object k2){
    if(k1 == null)
        return k2 == null;
    return k1.Equals(k2);
}

override public ISeq seq() {
	return ArraySeq.create(array);
}
}

}
