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

using System;
using java.math;

namespace org.clojure.runtime
{


public class BigNum : IntegerNum{
public BigInteger val;

override public Boolean Equals(Object arg0)
	{
	return arg0 != null
	       && arg0 is BigNum
	       && ((BigNum) arg0).val == val;
	}

override public int GetHashCode()
	{
	return val.GetHashCode();
	}

override public String ToString()
	{
	return val.ToString();
	}

public BigNum(long val)
	{
	this.val = BigInteger.valueOf(val);
	}

public BigNum(BigInteger val)
	{
	this.val = val;
	}

override public double doubleValue()
	{
	return val.doubleValue();
	}

override public float floatValue()
	{
	return val.floatValue();
	}

override public int intValue()
	{
	return val.intValue();
	}

override public long longValue()
	{
	return val.longValue();
	}

override public Boolean equiv(Num rhs)
	{
	return rhs.equivTo(val);
	}

override public Boolean equivTo(BigInteger x)
	{
	return x.Equals(val);
	}

override public Boolean equivTo(int x)
	{
	//must be outside of range of int or would be one itself
	return false;
	}

override public Boolean equivTo(RatioNum x)
	{
	//wouldn't still be a RatioNum if it was an integer
	return false;
	}

override public Boolean lt(Num rhs)
	{
	return rhs.gt(val);
	}

override public Boolean gt(BigInteger x)
	{
	return x.compareTo(val) < 0;
	}

override public Boolean gt(int x)
	{
	return BigInteger.valueOf(x).compareTo(val) < 0;
	}

override public Boolean gt(RatioNum x)
	{
	return x.numerator.lt(x.denominator.multiply(val));
	}

override public Num add(Num rhs)
	{
	return rhs.addTo(val);
	}

override public Num addTo(BigInteger x)
	{
	return Num.from(x.add(val));
	}

override public Num addTo(int x)
	{
	return Num.from(val.add(BigInteger.valueOf(x)));
	}

override public Num addTo(RatioNum x)
	{
	return x.addTo(val);
	}

override public Num subtractFrom(Num x)
	{
	return x.addTo(val.negate());
	}

override public Num multiplyBy(Num rhs)
	{
	return rhs.multiply(val);
	}

override public Num multiply(BigInteger x)
	{
	return Num.from(x.multiply(val));
	}

override public Num multiply(int x)
	{
	return Num.from(val.multiply(BigInteger.valueOf(x)));
	}

override public Num multiply(RatioNum x)
	{
	return x.multiply(val);
	}

override public Num divideBy(Num rhs)
	{
	return rhs.divide(val);
	}

override public Num divide(BigInteger n)
	{
	return Num.divide(n, val);
	}

override public Num divide(int n)
	{
	return Num.divide(BigInteger.valueOf(n), val);
	}

override public Num divide(RatioNum x)
	{
	return Num.divide(x.numerator, x.denominator.multiply(val));
	}

override public Object truncateDivide(ThreadLocalData tld, Num num)
	{
	return num.truncateBy(tld, val);
	}

override public Object truncateBy(ThreadLocalData tld, int div)
	{
	return Num.truncateBigints(tld, val, BigInteger.valueOf(div));
	}

override public Object truncateBy(ThreadLocalData tld, BigInteger div)
	{
	return Num.truncateBigints(tld, val, div);
	}

override public Object truncateBy(ThreadLocalData tld, RatioNum div)
	{
	Num q = (Num) Num.truncate(tld, div.denominator.multiply(val), div.numerator);
	return RT.setValues(tld, q, q.multiplyBy(div).subtractFrom(this));
	}

override public Num negate()
	{
	return Num.from(val.negate());
	}

override public Boolean minusp()
	{
	return val.signum() < 0;
	}

override public Boolean plusp()
	{
	return val.signum() > 0;
	}


override public Num oneMinus()
	{
	return Num.from(val.subtract(BIG_ONE));
	}

override public Num onePlus()
	{
	return Num.from(val.add(BIG_ONE));
	}
}
}

