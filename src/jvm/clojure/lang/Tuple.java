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

public class Tuple implements IArray{

final Object[] array;

final public static Tuple EMPTY = new Tuple();

/**
 * This ctor captures/aliases the passed array, so do not modify later !
 * @param init {key1,val1,key2,val2,...}
 */
public Tuple(Object... init){
	this.array = init;
}

public int length() {
    return array.length;
}

public Object get(int i){
    return array[i];
}


public IArray set(int i, Object val) {
    Object[] newArray = array.clone();
    newArray[i] = val;
    return new Tuple(newArray);
}

public boolean equals(Object key){
    if(this == key) return true;
    if(key == null || !(key instanceof IArray)) return false;

    final IArray a = (IArray) key;

    if(a.length() != array.length)
        return false;

    for(int i = 0; i < array.length; i++)
        {
        if(!equalKey(array[i],a.get(i)))
            return false;
        }

    return true;
}

public int hashCode(){
	int ret = 0;
	for(int i = 0; i < array.length; i++)
		{
		Object o = array[i];
		if(o != null)
			ret ^= o.hashCode();
		}
	return ret;
}

private boolean equalKey(Object k1,Object k2){
    if(k1 == null)
        return k2 == null;
    return k1.equals(k2);
}

public ISeq seq() {
	return ArraySeq.create(array);
}
}
