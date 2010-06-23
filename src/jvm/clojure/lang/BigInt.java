/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* chouser Jun 23, 2010 */

package clojure.lang;

import java.math.BigInteger;

public class BigInt extends Number{

final long lng;
final BigInteger bint;
static final BigInteger MIN_LONG = BigInteger.valueOf(Long.MIN_VALUE);
static final BigInteger MAX_LONG = BigInteger.valueOf(Long.MAX_VALUE);

private BigInt(long lng, BigInteger bint){
	this.lng = lng;
	this.bint = bint;
}

public BigInteger getBigInteger(){
	return bint;
}

public long getLong(){
	return lng;
}

public static BigInt fromBigInteger(BigInteger bint){
	if(MIN_LONG.compareTo(bint) <= 0 && MAX_LONG.compareTo(bint) >= 0)
		return new BigInt(bint.longValue(), null);
	else
		return new BigInt(0, bint);
}

public static BigInt fromLong(long lng){
	return new BigInt(lng, null);
}

public BigInteger toBigInteger(){
	if(bint == null)
		return BigInteger.valueOf(lng);
	else
		return bint;
}

///// java.lang.Number:

public int intValue(){
	if(bint == null)
		{
		if(lng < Integer.MIN_VALUE)
			return Integer.MIN_VALUE;
		else if(lng > Integer.MAX_VALUE)
			return Integer.MAX_VALUE;
		else
			return (int)lng;
		}
	else
		return bint.intValue();
}

public long longValue(){
	if(bint == null)
		return lng;
	else
		return bint.longValue();
}

public float floatValue(){
	if(bint == null)
		{
		if(lng < Float.MIN_VALUE)
			return Float.NEGATIVE_INFINITY;
		else if(lng > Float.MAX_VALUE)
			return Float.POSITIVE_INFINITY;
		else
			return lng;
		}
	else
		return bint.floatValue();
}

public double doubleValue(){
	if(bint == null)
		{
		if(lng < Double.MIN_VALUE)
			return Double.NEGATIVE_INFINITY;
		else if(lng > Double.MAX_VALUE)
			return Double.POSITIVE_INFINITY;
		else
			return lng;
		}
	else
		return bint.doubleValue();
}

public byte byteValue(){
	if(bint == null)
		{
		if(lng < Byte.MIN_VALUE)
			return Byte.MIN_VALUE;
		else if(lng > Byte.MAX_VALUE)
			return Byte.MAX_VALUE;
		else
			return (byte)lng;
		}
	else
		return bint.byteValue();
}

public short shortValue(){
	if(bint == null)
		{
		if(lng < Short.MIN_VALUE)
			return Short.MIN_VALUE;
		else if(lng > Short.MAX_VALUE)
			return Short.MAX_VALUE;
		else
			return (short)lng;
		}
	else
		return bint.shortValue();
}

}
