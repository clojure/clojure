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

public final class BigInt extends Number{

final public long lpart;
final public BigInteger bipart;

final public static BigInt ZERO = new BigInt(0,null);
final public static BigInt ONE = new BigInt(1,null);


//must follow Long
public int hashCode(){
	if(bipart == null)
		return (int) (this.lpart ^ (this.lpart >>> 32));
	return bipart.hashCode();
}

public boolean equals(Object obj){
	if(this == obj)
		return true;
	if(obj instanceof BigInt)
		{
		BigInt o = (BigInt) obj;
		if(bipart == null)
			return o.bipart == null && this.lpart == o.lpart;
		return o.bipart != null && this.bipart.equals(o.bipart);
		}
	return false;
}

private BigInt(long lpart, BigInteger bipart){
	this.lpart = lpart;
	this.bipart = bipart;
}

public static BigInt fromBigInteger(BigInteger val){
	if(val.bitLength() < 64)
		return new BigInt(val.longValue(), null);
	else
		return new BigInt(0, val);
}

public static BigInt fromLong(long val){
	return new BigInt(val, null);
}

public BigInteger toBigInteger(){
	if(bipart == null)
		return BigInteger.valueOf(lpart);
	else
		return bipart;
}

///// java.lang.Number:

public int intValue(){
	if(bipart == null)
		return (int) lpart;
	else
		return bipart.intValue();
}

public long longValue(){
	if(bipart == null)
		return lpart;
	else
		return bipart.longValue();
}

public float floatValue(){
	if(bipart == null)
			return lpart;
	else
		return bipart.floatValue();
}

public double doubleValue(){
	if(bipart == null)
		return lpart;
	else
		return bipart.doubleValue();
}

public byte byteValue(){
	if(bipart == null)
		return (byte) lpart;
	else
		return bipart.byteValue();
}

public short shortValue(){
	if(bipart == null)
		return (short) lpart;
	else
		return bipart.shortValue();
}

public static BigInt valueOf(long val){
	return new BigInt(val, null);
}

public String toString(){
	if(bipart == null)
		return String.valueOf(lpart);
	return bipart.toString();
}

public int bitLength(){
	return toBigInteger().bitLength();
}

public BigInt add(BigInt y) {
    if ((bipart == null) && (y.bipart == null)) {
        long ret = lpart + y.lpart;
        if ((ret ^ lpart) >= 0 || (ret ^ y.lpart) >= 0)
            return BigInt.valueOf(ret);
    }
    return BigInt.fromBigInteger(this.toBigInteger().add(y.toBigInteger()));
}

public BigInt multiply(BigInt y) {
    if ((bipart == null) && (y.bipart == null)) {
        long ret = lpart * y.lpart;
            if (y.lpart == 0 || ret / y.lpart == lpart)
                return BigInt.valueOf(ret);
        }
    return BigInt.fromBigInteger(this.toBigInteger().multiply(y.toBigInteger()));
}

public BigInt quotient(BigInt y) {
    if ((bipart == null) && (y.bipart == null)) {
        return BigInt.valueOf(lpart / y.lpart);
    }
    return BigInt.fromBigInteger(this.toBigInteger().divide(y.toBigInteger()));
}

public BigInt remainder(BigInt y) {
    if ((bipart == null) && (y.bipart == null)) {
        return BigInt.valueOf(lpart % y.lpart);
    }
    return BigInt.fromBigInteger(this.toBigInteger().remainder(y.toBigInteger()));
}

public boolean lt(BigInt y) {
    if ((bipart == null) && (y.bipart == null)) {
        return lpart < y.lpart;
    }
    return this.toBigInteger().compareTo(y.toBigInteger()) < 0;
}

}
