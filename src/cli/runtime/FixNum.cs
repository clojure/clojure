/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 28, 2006 10:09:27 AM */

using System;
using java.math;

namespace clojure.lang
{


public class FixNum : IntegerNum{
public int val;

override public Boolean Equals(Object arg0)
	{
	return arg0 != null
	       && arg0 is FixNum
	       && ((FixNum) arg0).val == val;
	}

override public int GetHashCode()
	{
	return val;
	}

override public String ToString()
	{
	return val.ToString();
	}

public FixNum(int val)
	{
	this.val = val;
	}

override public double doubleValue()
	{
	return (double) val;
	}

override public float floatValue()
	{
	return (float) val;
	}

override public int intValue()
	{
	return val;
	}

override public long longValue()
	{
	return (long) val;
	}

override public Boolean equiv(Num rhs)
	{
	return rhs.equivTo(val);
	}

override public Boolean equivTo(BigInteger x)
	{
	//wouldn't still be a BigInteger if it fit in int
	return false;
	}

override public Boolean equivTo(int x)
	{
	return x == val;
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
	return x.compareTo(BigInteger.valueOf(val)) < 0;
	}

override public Boolean gt(int x)
	{
	return x < val;
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
	return Num.from(x.add(BigInteger.valueOf(val)));
	}

override public Num addTo(int x)
	{
	return Num.from((long) x + val);
	}

override public Num addTo(RatioNum x)
	{
	return x.addTo(val);
	}

override public Num subtractFrom(Num x)
	{
	return x.addTo(-val);
	}

override public Num multiplyBy(Num rhs)
	{
	return rhs.multiply(val);
	}

override public Num multiply(BigInteger x)
	{
	return Num.from(x.multiply(BigInteger.valueOf(val)));
	}

override public Num multiply(int x)
	{
	return Num.from((long) x * val);
	}

override public Num multiply(RatioNum x)
	{
	return x.multiply(val);
	}

override public Object truncateDivide( Num num)
	{
	return num.truncateBy( val);
	}

override public Object truncateBy( int div)
	{
	return RT.setValues( Num.from(val / div), Num.from(val % div));
	}

override public Object truncateBy( BigInteger div)
	{
	return Num.truncateBigints( BigInteger.valueOf(val), div);
	}

override public Object truncateBy( RatioNum div)
	{
	Num q = (Num) Num.truncate( div.denominator.multiply(val), div.numerator);
	return RT.setValues( q, q.multiplyBy(div).subtractFrom(this));
	}

override public Num divideBy(Num rhs)
	{
	return rhs.divide(val);
	}

override public Num divide(BigInteger n)
	{
	return Num.divide(n, BigInteger.valueOf(val));
	}

static int gcdf(int u, int v)
	{
	while(v != 0)
		{
		int r = u % v;
		u = v;
		v = r;
		}
	return u;
	}

override public Num divide(int n)
	{
	int gcd = gcdf(n, val);
	if(gcd == 0)
		return Num.ZERO;

	n = n / gcd;
	int d = val / gcd;
	if(d == 1)
		return Num.from(n);
	if(d < 0)
		{
		n = -n;
		d = -d;
		}
	return new RatioNum((IntegerNum) Num.from(n), (IntegerNum) Num.from(d));
	}

override public Num divide(RatioNum x)
	{
	return Num.divide(x.numerator, x.denominator.multiply(val));
	}

override public Num negate()
	{
	return Num.from(-val);
	}

override public Boolean minusp()
	{
	return val < 0;
	}

override public Boolean plusp()
	{
	return val > 0;
	}

override public Num oneMinus()
	{
	return Num.from(val - 1);
	}

override public Num onePlus()
	{
	return Num.from(val + 1);
	}

}
}
