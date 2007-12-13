/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 28, 2006 10:08:33 AM */

package clojure.lang;

import java.math.BigInteger;

public class BigNum extends IntegerNum{
public BigInteger val;

public boolean equals(Object arg0){
	return arg0 != null
	       && arg0 instanceof BigNum
	       && ((BigNum) arg0).val == val;
}

public int hashCode(){
	return val.hashCode();
}

public String toString(){
	return val.toString();
}

public BigNum(long val){
	this.val = BigInteger.valueOf(val);
}

public BigNum(BigInteger val){
	this.val = val;
}

public double doubleValue(){
	return val.doubleValue();
}

public float floatValue(){
	return val.floatValue();
}

public int intValue(){
	return val.intValue();
}

public long longValue(){
	return val.longValue();
}

public boolean equiv(Num rhs){
	return rhs.equivTo(val);
}

public boolean equivTo(BigInteger x){
	return x.equals(val);
}

public boolean equivTo(int x){
	//must be outside of range of int or would be one itself
	return false;
}

public boolean equivTo(RatioNum x){
	//wouldn't still be a RatioNum if it was an integer
	return false;
}

public boolean lt(Num rhs){
	return rhs.gt(val);
}

public boolean gt(BigInteger x){
	return x.compareTo(val) < 0;
}

public boolean gt(int x){
	return BigInteger.valueOf(x).compareTo(val) < 0;
}

public boolean gt(RatioNum x){
	return x.numerator.lt(x.denominator.multiply(val));
}

public Num add(Num rhs){
	return rhs.addTo(val);
}

public Num addTo(BigInteger x){
	return Num.from(x.add(val));
}

public Num addTo(int x){
	return Num.from(val.add(BigInteger.valueOf(x)));
}

public Num addTo(RatioNum x){
	return x.addTo(val);
}

public Num subtractFrom(Num x){
	return x.addTo(val.negate());
}

public Num multiplyBy(Num rhs){
	return rhs.multiply(val);
}

public Num multiply(BigInteger x){
	return Num.from(x.multiply(val));
}

public Num multiply(int x){
	return Num.from(val.multiply(BigInteger.valueOf(x)));
}

public Num multiply(RatioNum x){
	return x.multiply(val);
}

public Num divideBy(Num rhs){
	return rhs.divide(val);
}

public Num divide(BigInteger n){
	return Num.divide(n, val);
}

public Num divide(int n){
	return Num.divide(BigInteger.valueOf(n), val);
}

public Num divide(RatioNum x){
	return Num.divide(x.numerator, x.denominator.multiply(val));
}

public Object[] truncateDivide(Num num){
	return num.truncateBy(val);
}

public Object[] truncateBy(int div){
	return Num.truncateBigints(val, BigInteger.valueOf(div));
}

public Object[] truncateBy(BigInteger div){
	return Num.truncateBigints(val, div);
}

public Object[] truncateBy(RatioNum div){
	Num q = (Num) Num.truncate(div.denominator.multiply(val), div.numerator)[0];
	return RT.setValues(q, q.multiplyBy(div).subtractFrom(this));
}

public Num negate(){
	return Num.from(val.negate());
}

public boolean minusp(){
	return val.signum() < 0;
}

public boolean plusp(){
	return val.signum() > 0;
}

public boolean zerop(){
	return val.compareTo(BigInteger.ZERO) == 0;
}

public Num oneMinus(){
	return Num.from(val.subtract(BigInteger.ONE));
}

public Num onePlus(){
	return Num.from(val.add(BigInteger.ONE));
}

public Num bitXorBy(IntegerNum rhs){
	return rhs.bitXor(val);
}

public Num bitXor(BigInteger y){
	return Num.from(val.xor(y));
}

public Num bitXor(int y){
	return Num.from(val.xor(BigInteger.valueOf(y)));
}

public Num bitAndBy(IntegerNum rhs){
	return rhs.bitAnd(val);
}

public Num bitAnd(int y){
	return Num.from(val.and(BigInteger.valueOf(y)));
}

public Num bitAnd(BigInteger y){
	return Num.from(val.and(y));
}

public Num bitOrBy(IntegerNum rhs){
	return rhs.bitOr(val);
}

public Num bitOr(int y){
	return Num.from(val.or(BigInteger.valueOf(y)));
}

public Num bitOr(BigInteger y){
	return Num.from(val.or(y));
}

public Num bitNot(){
	return Num.from(val.not());
}

public Num shiftLeftBy(IntegerNum rhs){
	return rhs.shiftLeft(val);
}

public Num shiftLeft(BigInteger y){
	return Num.from(val.shiftLeft(y.intValue()));
}

public Num shiftLeft(int y){
	return Num.from(val.shiftLeft(y));
}

public Num shiftRightBy(IntegerNum rhs){
	return rhs.shiftRight(val);
}

public Num shiftRight(BigInteger y){
	return Num.from(val.shiftRight(y.intValue()));
}

public Num shiftRight(int y){
	return Num.from(val.shiftRight(y));
}

}

