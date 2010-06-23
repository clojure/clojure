/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Apr 19, 2008 */

package clojure.lang;

import java.math.BigInteger;

public class Util{
static public boolean equiv(Object k1, Object k2){
	if(k1 == k2)
		return true;
	if(k1 != null)
		{
		if(k1 instanceof Number && k2 instanceof Number)
			return Numbers.equal((Number)k1, (Number)k2);
		else if(k1 instanceof IPersistentCollection && k2 instanceof IPersistentCollection)
			return ((IPersistentCollection)k1).equiv(k2);
		return k1.equals(k2);
		}
	return false;
}

static public boolean equals(Object k1, Object k2){
	if(k1 == k2)
		return true;
	return k1 != null && k1.equals(k2);
}

/*
static public boolean equals(long x, long y){
	return x == y;
}

static public boolean equals(double x, double y){
	return x == y;
}

//static public boolean equals(long x, Object y){
//	return equals(Numbers.num(x),y);
//}
//
//static public boolean equals(Object x, long y){
//	return equals(x,Numbers.num(y));
//}
//
//static public boolean equals(double x, Object y){
//	return equals((Double)x,y);
//}
//
//static public boolean equals(Object x, double y){
//	return equals(x,(Double)y);
//}

static public boolean equiv(long x, long y){
	return x == y;
}

static public boolean equiv(double x, double y){
	return x == y;
}

//static public boolean equiv(long x, Object y){
//	return equiv(Numbers.num(x),y);
//}
//
//static public boolean equiv(Object x, long y){
//	return equiv(x,Numbers.num(y));
//}
//
//static public boolean equiv(double x, Object y){
//	return equiv((Double)x,y);
//}
//
//static public boolean equiv(Object x, double y){
//	return equiv(x,(Double)y);
//}
//*/

static public boolean identical(Object k1, Object k2){
	return k1 == k2;
}

static public Class classOf(Object x){
	if(x != null)
		return x.getClass();
	return null;
}

static public int compare(Object k1, Object k2){
	if(k1 == k2)
		return 0;
	if(k1 != null)
		{
		if(k2 == null)
			return 1;
		if(k1 instanceof Number)
			return Numbers.compare((Number) k1, (Number) k2);
		return ((Comparable) k1).compareTo(k2);
		}
	return -1;
}

static public int hash(Object o){
	if(o == null)
		return 0;
	return o.hashCode();
}

static public int hashCombine(int seed, int hash){
	//a la boost
	seed ^= hash + 0x9e3779b9 + (seed << 6) + (seed >> 2);
	return seed;
}

static public boolean isPrimitive(Class c){
	return c != null && c.isPrimitive() && !(c == Void.TYPE);
}

static public boolean isInteger(Object x){
	return x instanceof Integer
			|| x instanceof Long
			|| x instanceof BigInteger;
}

static public Object ret1(Object ret, Object nil){
		return ret;
}

static public ISeq ret1(ISeq ret, Object nil){
		return ret;
}

}
