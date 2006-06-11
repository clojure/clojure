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

package clojure.lang;

import java.math.BigInteger;
import java.math.BigDecimal;

public class DoubleNum extends FloatNum{
double val;

public DoubleNum(double val)
	{
	this.val = val;
	}

public double doubleValue()
	{
	return val;
	}

public float floatValue()
	{
	return (float) val;
	}

public int intValue()
	{
	return (int) val;
	}

public long longValue()
	{
	return (long) val;
	}

final static BigInteger BIGTEN = BigInteger.valueOf(10);

public Num toRational()
	{
	BigDecimal d = new BigDecimal(val);
	return Num.divide(d.unscaledValue(), BIGTEN.pow(d.scale()));
	}

public boolean equiv(Num rhs)
	{
	if(rhs instanceof RatioNum)
		return equivTo((RatioNum) rhs);
	return val == rhs.doubleValue();
	}

public boolean equivTo(BigInteger x)
	{
	return val == x.doubleValue();
	}

public boolean equivTo(int x)
	{
	return x == val;
	}

public boolean equivTo(RatioNum x)
	{
	return toRational().equivTo(x);
	}

public boolean lt(Num rhs)
	{
	if(rhs instanceof RatioNum)
		return toRational().lt(rhs);
	return val < rhs.doubleValue();
	}

public boolean gt(BigInteger x)
	{
	return val > x.doubleValue();
	}

public boolean gt(int x)
	{
	return val > x;
	}

public boolean gt(RatioNum x)
	{
	return toRational().gt(x);
	}

public Num add(Num rhs)
	{
	return Num.from(val + rhs.doubleValue());
	}

public Num addTo(int x)
	{
	return Num.from(val + x);
	}

public Num addTo(BigInteger x)
	{
	return Num.from(val + x.doubleValue());
	}

public Num addTo(RatioNum x)
	{
	return Num.from(val + x.doubleValue());
	}

public Num subtractFrom(Num x)
	{
	return Num.from(x.doubleValue() - val);
	}

public Num multiplyBy(Num rhs)
	{
	return Num.from(val * rhs.doubleValue());
	}

public Num multiply(int x)
	{
	return Num.from(val * x);
	}

public Num multiply(BigInteger x)
	{
	return Num.from(val * x.doubleValue());
	}

public Num multiply(RatioNum x)
	{
	return Num.from(val * x.doubleValue());
	}

public Num divideBy(Num rhs)
	{
	return Num.from(val / rhs.doubleValue());
	}

public Num divide(int x)
	{
	return Num.from(x / val);
	}

public Num divide(BigInteger x)
	{
	return Num.from(x.doubleValue() / val);
	}

public Num divide(RatioNum x)
	{
	return Num.from(x.doubleValue() / val);
	}

static Object truncate(double n, double d)
	{
	double q = n / d;
	if(q <= Integer.MAX_VALUE && q >= Integer.MIN_VALUE)
		{
		return RT.setValues(Num.from((int) q),
		                    Num.from(n - ((int) q) * d));
		}
	else
		{ //bigint quotient
		Num bq = Num.from(new BigDecimal(q).toBigInteger());
		return RT.setValues(bq,
		                    Num.from(n - bq.doubleValue() * d));
		}
	}

public Object truncateBy( BigInteger x)
	{
	return truncate( val, x.doubleValue());
	}

public Object truncateBy( int x)
	{
	return truncate( val, x);
	}

public Object truncateBy( RatioNum x)
	{
	return truncate( val, x.doubleValue());
	}

public Object truncateDivide( Num num)
	{
	return truncate( num.doubleValue(), val);
	}

public Num negate()
	{
	return Num.from(-val);
	}

public boolean equals(Object arg0)
	{
	return arg0 != null
	       && arg0 instanceof DoubleNum
	       && Double.doubleToLongBits(((DoubleNum) arg0).val) ==
	          Double.doubleToLongBits(val);
	}

public int hashCode()
	{
	long v = Double.doubleToLongBits(val);
	return (int) (v ^ (v >>> 32));
	}

public String toString()
	{
	return Double.toString(val);
	}

public boolean minusp()
	{
	return val < 0;
	}

public boolean plusp()
	{
	return val > 0;
	}

public Num oneMinus()
	{
	return Num.from(val - 1);
	}

public Num onePlus()
	{
	return Num.from(val + 1);
	}

}

