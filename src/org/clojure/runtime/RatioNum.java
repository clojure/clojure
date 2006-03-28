/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 28, 2006 10:14:44 AM */

package org.clojure.runtime;

import java.math.BigInteger;

public class RatioNum extends Rational{
public boolean equals(Object arg0)
	{
	return arg0 != null
	       && arg0 instanceof RatioNum
	       && ((RatioNum) arg0).numerator.equals(numerator)
	       && ((RatioNum) arg0).denominator.equals(denominator);
	}

public int hashCode()
	{
	return numerator.hashCode() ^ denominator.hashCode();
	}

public String toString()
	{
	return numerator.toString() + "/" + denominator.toString();
	}

public IntegerNum numerator;
public IntegerNum denominator;

public RatioNum(IntegerNum n, IntegerNum d)
	{
	this.numerator = n;
	this.denominator = d;
	}

public double doubleValue()
	{
	return numerator.doubleValue() / denominator.doubleValue();
	}

public float floatValue()
	{
	return (float) doubleValue();
	}

public int intValue()
	{
	return (int) doubleValue();
	}

public long longValue()
	{
	return (long) doubleValue();
	}

public boolean equiv(Num rhs)
	{
	return rhs.equivTo(this);
	}

public boolean equivTo(BigInteger x)
	{
	return false;
	}

public boolean equivTo(int x)
	{
	return false;
	}

public boolean equivTo(RatioNum x)
	{
	return numerator.equiv(x.numerator) && denominator.equiv(x.denominator);
	}

public boolean lt(Num rhs)
	{
	return rhs.gt(this);
	}

public boolean gt(BigInteger x)
	{
	return denominator.multiply(x).lt(numerator);
	}

public boolean gt(int x)
	{
	return denominator.multiply(x).lt(numerator);
	}

public boolean gt(RatioNum x)
	{
	return x.numerator.multiplyBy(denominator).lt(numerator.multiplyBy(x.denominator));
	}

public Num add(Num rhs)
	{
	return rhs.addTo(this);
	}

public Num addTo(BigInteger x)
	{
	return Num.divide(numerator.add(denominator.multiply(x)), denominator);
	}

public Num addTo(int x)
	{
	return Num.divide(numerator.add(denominator.multiply(x)), denominator);
	}

public Num addTo(RatioNum x)
	{
	return Num.divide(numerator.multiplyBy(x.denominator)
			.add(x.numerator.multiplyBy(denominator))
			, denominator.multiplyBy(x.denominator));
	}

public Num subtractFrom(Num x)
	{
	return x.add(this.multiply(-1));
	}

public Num multiplyBy(Num rhs)
	{
	return rhs.multiply(this);
	}

public Num multiply(BigInteger x)
	{
	return Num.divide(numerator.multiply(x), denominator);
	}

public Num multiply(int x)
	{
	return Num.divide(numerator.multiply(x), denominator);
	}

public Num multiply(RatioNum x)
	{
	return Num.divide(numerator.multiplyBy(x.numerator)
			, denominator.multiplyBy(x.denominator));
	}

public Num divideBy(Num rhs)
	{
	return rhs.divide(this);
	}

public Num divide(BigInteger n)
	{
	return Num.divide(denominator.multiply(n), numerator);
	}

public Num divide(int n)
	{
	return Num.divide(denominator.multiply(n), numerator);
	}

public Num divide(RatioNum n)
	{
	return Num.divide(denominator.multiplyBy(n.numerator)
			, numerator.multiplyBy(n.denominator));
	}


public Object truncateDivide(ThreadLocalData tld, Num num)
	{
	return num.truncateBy(tld, this);
	}

public Object truncateBy(ThreadLocalData tld, int div)
	{
	Num q = (Num) Num.truncate(tld, numerator, denominator.multiply(div));
	return RT.setValues(tld, q, q.multiply(div).subtractFrom(this));
	}

public Object truncateBy(ThreadLocalData tld, BigInteger div)
	{
	Num q = (Num) Num.truncate(tld, numerator, denominator.multiply(div));
	return RT.setValues(tld, q, q.multiply(div).subtractFrom(this));
	}

public Object truncateBy(ThreadLocalData tld, RatioNum div)
	{
	Num q = (Num) Num.truncate(tld, numerator.multiplyBy(div.denominator),
	                           denominator.multiplyBy(div.numerator));
	return RT.setValues(tld, q, q.multiplyBy(div).subtractFrom(this));
	}


public Num negate()
	{
	return Num.divide(numerator.negate(), denominator);
	}

public boolean minusp()
	{
	return numerator.minusp();
	}

public boolean plusp()
	{
	return numerator.plusp();
	}

public Num oneMinus()
	{
	return addTo(-1);
	}

public Num onePlus()
	{
	return addTo(1);
	}

}

