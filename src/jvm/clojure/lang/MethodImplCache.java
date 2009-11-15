/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Nov 8, 2009 */

package clojure.lang;

public final class MethodImplCache{
public final IPersistentMap protocol;
public final Keyword methodk;
public final int shift;
public final int mask;
public final Object[] table;    //[class, fn. class, fn ...]

//these are not volatile by design
public Object lastClass;
public IFn lastImpl;

public MethodImplCache(IPersistentMap protocol, Keyword methodk){
	this(protocol, methodk, 0, 0, RT.EMPTY_ARRAY);
}

public MethodImplCache(IPersistentMap protocol, Keyword methodk, int shift, int mask, Object[] table){
	this.protocol = protocol;
	this.methodk = methodk;
	this.shift = shift;
	this.mask = mask;
	this.table = table;
	this.lastClass = this;
}

public IFn fnFor(Class c){
	if(c == lastClass)
		return lastImpl;
	int idx = ((Util.hash(c) >> shift) & mask) << 1;
	if(idx < table.length && table[idx] == c)
		{
		lastClass = c;
		return lastImpl = 
				(IFn) table[idx + 1];
		}
	return null;
}

}
