/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 28, 2006 10:07:33 AM */

package clojure.lang;

import java.math.BigInteger;

public abstract class Num extends Number implements Comparable{

public final static Num ZERO = from(0);
public final static Num ONE = from(1);

static public Num from(int val){
	//todo - cache a bunch of small fixnums
	return new FixNum(val);
}

static public Num from(double val){
	return new DoubleNum(val);
}

static public Num from(long val){
	if(val <= Integer.MAX_VALUE && val >= Integer.MIN_VALUE)
		return from((int) val);
	else
		return new BigNum(val);
}

static public Num from(BigInteger val){
	if(val.bitLength() < 32)
		return from(val.intValue());
	else
		return new BigNum(val);
}

static public Num from(Object x){
	if(x instanceof Num)
		return (Num) x;
	else
		{
		Class c = x.getClass();
		if(c == Integer.class)
			return Num.from(((Integer) x).intValue());
		else if(c == Double.class || c == Float.class)
			return Num.from(((Number) x).doubleValue());
		else if(c == Long.class)
			return Num.from(((Long) x).longValue());
		else if(c == BigInteger.class)
			return Num.from((BigInteger) x);
		else
			return Num.from(((Number) x).intValue());
		}
}

static public Num add(Object x, Object y){
	//if(x instanceof Num && y instanceof Num)
	//return ((Num)x).add((Num) y);
	return Num.from(x).add(Num.from(y));
}

abstract public Num add(Num rhs);

abstract public Num addTo(int x);

abstract public Num addTo(BigInteger x);

abstract public Num addTo(RatioNum x);

static public Num subtract(Object x, Object y){
	return Num.from(y).subtractFrom(Num.from(x));
}

//this double-dispatches to addTo(-self)
abstract public Num subtractFrom(Num rhs);

static public Num multiply(Object x, Object y){
	return Num.from(x).multiplyBy(Num.from(y));
}

abstract public Num multiplyBy(Num rhs);

abstract public Num multiply(int x);

abstract public Num multiply(BigInteger x);

abstract public Num multiply(RatioNum x);

static public Num divide(Object x, Object y){
	return Num.from(x).divideBy(Num.from(y));
}

abstract public Num divideBy(Num rhs);

abstract public Num divide(int x);

abstract public Num divide(BigInteger x);

abstract public Num divide(RatioNum x);

static public Object truncate(Object num, Object div){
	return Num.from(div).truncateDivide(Num.from(num));
}

abstract public Object truncateDivide(Num rhs);

abstract public Object truncateBy(int x);

abstract public Object truncateBy(BigInteger x);

abstract public Object truncateBy(RatioNum x);

static public Object truncateBigints(BigInteger n, BigInteger d){
	BigInteger[] result = n.divideAndRemainder(d);
	return RT.setValues(Num.from(result[0]), Num.from(result[1]));
}

static public Num divide(BigInteger n, BigInteger d){
	BigInteger gcd = n.gcd(d);
	if(gcd.equals(BigInteger.ZERO))
		return Num.ZERO;
	n = n.divide(gcd);
	d = d.divide(gcd);
	if(d.equals(BigInteger.ONE))
		return Num.from(n);
	return new RatioNum((IntegerNum) Num.from(d.signum() < 0 ? n.negate() : n),
	                    (IntegerNum) Num.from(d.signum() < 0 ? d.negate() : d));
}

static public boolean equiv(Object x, Object y){
	return Num.from(x).equiv(Num.from(y));
}

abstract public boolean equiv(Num rhs);

abstract public boolean equivTo(int x);

abstract public boolean equivTo(BigInteger x);

abstract public boolean equivTo(RatioNum x);

static public boolean lt(Object x, Object y){
	return Num.from(x).lt(Num.from(y));
}

static public boolean lte(Object x, Object y){
	Num lx = Num.from(x);
	Num ly = Num.from(y);
	return lx.lt(ly) || lx.equiv(ly);
}

static public boolean gt(Object x, Object y){
	return Num.from(y).lt(Num.from(x));
}

static public boolean gte(Object x, Object y){
	Num lx = Num.from(x);
	Num ly = Num.from(y);
	return ly.lt(lx) || lx.equiv(ly);
}

abstract public boolean lt(Num rhs);

abstract public boolean gt(int x);

abstract public boolean gt(BigInteger x);

abstract public boolean gt(RatioNum x);

static public Num negate(Object x){
	return Num.from(x).negate();
}

static public Object negPred(Num n){
	return n.minusp() ? RT.T : null;
}

static public Object posPred(Num n){
	return n.plusp() ? RT.T : null;
}

static public Object zeroPred(Num n){
	return n.zerop() ? RT.T : null;
}

static public Num dec(Num n){
	return n.oneMinus();
}

static public Num inc(Num n){
	return n.onePlus();
}

abstract public Num negate();

abstract public boolean minusp();

abstract public boolean plusp();

abstract public boolean zerop();

abstract public Num oneMinus();

abstract public Num onePlus();

public int compareTo(Object object){
	Num other = Num.from(object);
	if(this.equiv(other))
		return 0;
	else if(this.lt(other))
		return -1;
	else
		return 1;
}
}
