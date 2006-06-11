/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 28, 2006 10:13:45 AM */

using System;
using java.math;

namespace clojure.lang
{

public class DoubleNum : FloatNum{
double val;

public DoubleNum(double val)
	{
	this.val = val;
	}

override public double doubleValue()
	{
	return val;
	}

override public float floatValue()
	{
	return (float) val;
	}

override public int intValue()
	{
	return (int) val;
	}

override public long longValue()
	{
	return (long) val;
	}


public Num toRational()
	{
	BigDecimal d = new BigDecimal(val);
	return Num.divide(d.movePointRight(d.scale()).toBigInteger(), BIGTEN.pow(d.scale()));
	}

override public Boolean equiv(Num rhs)
	{
	if(rhs is RatioNum)
		return equivTo((RatioNum) rhs);
	return val == rhs.doubleValue();
	}

override public Boolean equivTo(BigInteger x)
	{
	return val == x.doubleValue();
	}

override public Boolean equivTo(int x)
	{
	return x == val;
	}

override public Boolean equivTo(RatioNum x)
	{
	return toRational().equivTo(x);
	}

override public Boolean lt(Num rhs)
	{
	if(rhs is RatioNum)
		return toRational().lt(rhs);
	return val < rhs.doubleValue();
	}

override public Boolean gt(BigInteger x)
	{
	return val > x.doubleValue();
	}

override public Boolean gt(int x)
	{
	return val > x;
	}

override public Boolean gt(RatioNum x)
	{
	return toRational().gt(x);
	}

override public Num add(Num rhs)
	{
	return Num.from(val + rhs.doubleValue());
	}

override public Num addTo(int x)
	{
	return Num.from(val + x);
	}

override public Num addTo(BigInteger x)
	{
	return Num.from(val + x.doubleValue());
	}

override public Num addTo(RatioNum x)
	{
	return Num.from(val + x.doubleValue());
	}

override public Num subtractFrom(Num x)
	{
	return Num.from(x.doubleValue() - val);
	}

override public Num multiplyBy(Num rhs)
	{
	return Num.from(val * rhs.doubleValue());
	}

override public Num multiply(int x)
	{
	return Num.from(val * x);
	}

override public Num multiply(BigInteger x)
	{
	return Num.from(val * x.doubleValue());
	}

override public Num multiply(RatioNum x)
	{
	return Num.from(val * x.doubleValue());
	}

override public Num divideBy(Num rhs)
	{
	return Num.from(val / rhs.doubleValue());
	}

override public Num divide(int x)
	{
	return Num.from(x / val);
	}

override public Num divide(BigInteger x)
	{
	return Num.from(x.doubleValue() / val);
	}

override public Num divide(RatioNum x)
	{
	return Num.from(x.doubleValue() / val);
	}

static Object truncate( double n, double d)
	{
	double q = n / d;
	if(q <= Int32.MaxValue && q >= Int32.MinValue)
		{
		return RT.setValues( Num.from((int) q),
		                    Num.from(n - ((int) q) * d));
		}
	else
		{ //bigint quotient
		Num bq = Num.from(new BigDecimal(q).toBigInteger());
		return RT.setValues( bq,
		                    Num.from(n - bq.doubleValue() * d));
		}
	}

override public Object truncateBy( BigInteger x)
	{
	return truncate( val, x.doubleValue());
	}

override public Object truncateBy( int x)
	{
	return truncate( val, x);
	}

override public Object truncateBy( RatioNum x)
	{
	return truncate( val, x.doubleValue());
	}

override public Object truncateDivide( Num num)
	{
	return truncate( num.doubleValue(), val);
	}

override public Num negate()
	{
	return Num.from(-val);
	}

override public Boolean Equals(Object arg0)
	{
	return arg0 != null
	       && arg0 is DoubleNum
	       &&((DoubleNum) arg0).val.Equals(val);
	}

override public int GetHashCode()
	{
	return val.GetHashCode();
	}

override public String ToString()
	{
	return val.ToString();
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
