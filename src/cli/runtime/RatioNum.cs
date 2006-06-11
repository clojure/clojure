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

using System;
using java.math;

namespace clojure.lang
{

public class RatioNum : RationalNum{
override public Boolean Equals(Object arg0)
	{
	return arg0 != null
	       && arg0 is RatioNum
	       && ((RatioNum) arg0).numerator.Equals(numerator)
	       && ((RatioNum) arg0).denominator.Equals(denominator);
	}

override public int GetHashCode()
	{
	return numerator.GetHashCode() ^ denominator.GetHashCode();
	}

override public String ToString()
	{
	return numerator.ToString() + "/" + denominator.ToString();
	}

public IntegerNum numerator;
public IntegerNum denominator;

public RatioNum(IntegerNum n, IntegerNum d)
	{
	this.numerator = n;
	this.denominator = d;
	}

override public double doubleValue()
	{
	return numerator.doubleValue() / denominator.doubleValue();
	}

override public float floatValue()
	{
	return (float) doubleValue();
	}

override public int intValue()
	{
	return (int) doubleValue();
	}

override public long longValue()
	{
	return (long) doubleValue();
	}

override public Boolean equiv(Num rhs)
	{
	return rhs.equivTo(this);
	}

override public Boolean equivTo(BigInteger x)
	{
	return false;
	}

override public Boolean equivTo(int x)
	{
	return false;
	}

override public Boolean equivTo(RatioNum x)
	{
	return numerator.equiv(x.numerator) && denominator.equiv(x.denominator);
	}

override public Boolean lt(Num rhs)
	{
	return rhs.gt(this);
	}

override public Boolean gt(BigInteger x)
	{
	return denominator.multiply(x).lt(numerator);
	}

override public Boolean gt(int x)
	{
	return denominator.multiply(x).lt(numerator);
	}

override public Boolean gt(RatioNum x)
	{
	return x.numerator.multiplyBy(denominator).lt(numerator.multiplyBy(x.denominator));
	}

override public Num add(Num rhs)
	{
	return rhs.addTo(this);
	}

override public Num addTo(BigInteger x)
	{
	return Num.divide(numerator.add(denominator.multiply(x)), denominator);
	}

override public Num addTo(int x)
	{
	return Num.divide(numerator.add(denominator.multiply(x)), denominator);
	}

override public Num addTo(RatioNum x)
	{
	return Num.divide(numerator.multiplyBy(x.denominator)
			.add(x.numerator.multiplyBy(denominator))
			, denominator.multiplyBy(x.denominator));
	}

override public Num subtractFrom(Num x)
	{
	return x.add(this.multiply(-1));
	}

override public Num multiplyBy(Num rhs)
	{
	return rhs.multiply(this);
	}

override public Num multiply(BigInteger x)
	{
	return Num.divide(numerator.multiply(x), denominator);
	}

override public Num multiply(int x)
	{
	return Num.divide(numerator.multiply(x), denominator);
	}

override public Num multiply(RatioNum x)
	{
	return Num.divide(numerator.multiplyBy(x.numerator)
			, denominator.multiplyBy(x.denominator));
	}

override public Num divideBy(Num rhs)
	{
	return rhs.divide(this);
	}

override public Num divide(BigInteger n)
	{
	return Num.divide(denominator.multiply(n), numerator);
	}

override public Num divide(int n)
	{
	return Num.divide(denominator.multiply(n), numerator);
	}

override public Num divide(RatioNum n)
	{
	return Num.divide(denominator.multiplyBy(n.numerator)
			, numerator.multiplyBy(n.denominator));
	}


override public Object truncateDivide( Num num)
	{
	return num.truncateBy( this);
	}

override public Object truncateBy( int div)
	{
	Num q = (Num) Num.truncate( numerator, denominator.multiply(div));
	return RT.setValues( q, q.multiply(div).subtractFrom(this));
	}

override public Object truncateBy( BigInteger div)
	{
	Num q = (Num) Num.truncate( numerator, denominator.multiply(div));
	return RT.setValues( q, q.multiply(div).subtractFrom(this));
	}

override public Object truncateBy( RatioNum div)
	{
	Num q = (Num) Num.truncate( numerator.multiplyBy(div.denominator),
	                           denominator.multiplyBy(div.numerator));
	return RT.setValues( q, q.multiplyBy(div).subtractFrom(this));
	}


override public Num negate()
	{
	return Num.divide(numerator.negate(), denominator);
	}

override public Boolean minusp()
	{
	return numerator.minusp();
	}

override public Boolean plusp()
	{
	return numerator.plusp();
	}

override public Num oneMinus()
	{
	return addTo(-1);
	}

override public Num onePlus()
	{
	return addTo(1);
	}

}
}

