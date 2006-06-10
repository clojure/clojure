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

using System;
using java.math;

namespace org.clojure.runtime
{

public abstract class Num : IComparable , IConvertible
										 {

public static Num ZERO = from(0);
public static Num ONE = from(1);

static public Num from(int val)
	{
	//todo - cache a bunch of small fixnums
	return new FixNum(val);
	}

static public Num from(double val)
	{
	return new DoubleNum(val);
	}

static public Num from(long val)
	{
	if(val <= Int32.MaxValue && val >= Int32.MinValue)
		return from((int) val);
	else
		return new BigNum(val);
	}

static public Num from(BigInteger val)
	{
	if(val.bitLength() < 32)
		return from(val.intValue());
	else
		return new BigNum(val);
	}
	
	internal static BigInteger BIGTEN = BigInteger.valueOf(10);

static public Num from(Object x)
	{
	if(x is Num)
		return (Num) x;
	else
		{
		IConvertible c = x as IConvertible;
		if(c != null)
			{
			switch(c.GetTypeCode())
				{
				case TypeCode.Int32:
					return Num.from((Int32) x);
				case TypeCode.Double:
				case TypeCode.Single:
					return Num.from(Convert.ToDouble(x));
				case TypeCode.Int64:
					return Num.from((Int64) x);
				//TODO: Decimal w/o string conversion
				case TypeCode.Decimal:
					BigDecimal d = new BigDecimal(x.ToString());
					return Num.divide(d.movePointRight(d.scale()).toBigInteger(), 
										BIGTEN.pow(d.scale()));
				default:
					return Num.from(Convert.ToInt32(x));
				}
			}
		else if(x is BigInteger)
			return Num.from((BigInteger) x);
		else
			throw new ArgumentException("Cannot convert argument: " + x + " to Num");
		}
	}


    virtual public byte byteValue()
        {
        return checked((byte)intValue());
        }

    virtual public short shortValue()
        {
        return checked((short)intValue());
        }

	abstract public double doubleValue();

	abstract public float floatValue();

	abstract public int intValue();

	abstract public long longValue();
	
static public Num add(Object x, Object y)
	{
	//if(x instanceof Num && y instanceof Num)
	//return ((Num)x).add((Num) y);
	return Num.from(x).add(Num.from(y));
	}

abstract public Num add(Num rhs);

abstract public Num addTo(int x);

abstract public Num addTo(BigInteger x);

abstract public Num addTo(RatioNum x);

static public Num subtract(Object x, Object y)
	{
	return Num.from(y).subtractFrom(Num.from(x));
	}

//this double-dispatches to addTo(-self)
abstract public Num subtractFrom(Num rhs);

static public Num multiply(Object x, Object y)
	{
	return Num.from(x).multiplyBy(Num.from(y));
	}

abstract public Num multiplyBy(Num rhs);

abstract public Num multiply(int x);

abstract public Num multiply(BigInteger x);

abstract public Num multiply(RatioNum x);

static public Num divide(Object x, Object y)
	{
	return Num.from(x).divideBy(Num.from(y));
	}

abstract public Num divideBy(Num rhs);

abstract public Num divide(int x);

abstract public Num divide(BigInteger x);

abstract public Num divide(RatioNum x);

static public Object truncate( Object num, Object div)
	{
	return Num.from(div).truncateDivide( Num.from(num));
	}

abstract public Object truncateDivide( Num rhs);

abstract public Object truncateBy( int x);

abstract public Object truncateBy( BigInteger x);

abstract public Object truncateBy( RatioNum x);

static public Object truncateBigints( BigInteger n, BigInteger d)
	{
	BigInteger[] result = n.divideAndRemainder(d);
	return RT.setValues( Num.from(result[0]), Num.from(result[1]));
	}

	internal static BigInteger BIG_ONE = BigInteger.valueOf(1);
	internal static BigInteger BIG_ZERO = BigInteger.valueOf(0);

static public Num divide(BigInteger n, BigInteger d)
	{
	BigInteger gcd = n.gcd(d);
	if(gcd.Equals(BIG_ZERO))
		return Num.ZERO;
	n = n.divide(gcd);
	d = d.divide(gcd);
	if(d.Equals(BIG_ONE))
		return Num.from(n);
	return new RatioNum((IntegerNum) Num.from(d.signum() < 0 ? n.negate() : n),
	                    (IntegerNum) Num.from(d.signum() < 0 ? d.negate() : d));
	}

static public Boolean equiv(Object x, Object y)
	{
	return Num.from(x).equiv(Num.from(y));
	}

abstract public Boolean equiv(Num rhs);

abstract public Boolean equivTo(int x);

abstract public Boolean equivTo(BigInteger x);

abstract public Boolean equivTo(RatioNum x);

static public Boolean lt(Object x, Object y)
	{
	return Num.from(x).lt(Num.from(y));
	}

static public Boolean lte(Object x, Object y)
	{
	Num lx = Num.from(x);
	Num ly = Num.from(y);
	return lx.lt(ly) || lx.equiv(ly);
	}

static public Boolean gt(Object x, Object y)
	{
	return Num.from(y).lt(Num.from(x));
	}

static public Boolean gte(Object x, Object y)
	{
	Num lx = Num.from(x);
	Num ly = Num.from(y);
	return ly.lt(lx) || lx.equiv(ly);
	}

abstract public Boolean lt(Num rhs);

abstract public Boolean gt(int x);

abstract public Boolean gt(BigInteger x);

abstract public Boolean gt(RatioNum x);

static public Num negate(Object x)
	{
	return Num.from(x).negate();
	}

abstract public Num negate();

abstract public Boolean minusp();

abstract public Boolean plusp();

abstract public Num oneMinus();

abstract public Num onePlus();

public int CompareTo(Object obj)
	{
	Num other = Num.from(obj);
	if(this.equiv(other))
		return 0;
	else if(this.lt(other))
		return -1;
	else
		return 1;
	}

#region IConvertible Members

public TypeCode GetTypeCode()
    {
throw new Exception("The method or operation is not implemented.");
    }

public bool ToBoolean(IFormatProvider provider)
    {
    return true;
    }

public byte ToByte(IFormatProvider provider)
    {
    return checked((byte)intValue());
    }

public char ToChar(IFormatProvider provider)
    {
    return checked((char)intValue());
    }

public DateTime ToDateTime(IFormatProvider provider)
    {
throw new Exception("The method or operation is not implemented.");
    }

public decimal ToDecimal(IFormatProvider provider)
    {
throw new Exception("The method or operation is not implemented.");
    }

public double ToDouble(IFormatProvider provider)
    {
    return doubleValue();
    }

public short ToInt16(IFormatProvider provider)
    {
    return checked((short)intValue());
    }

public int ToInt32(IFormatProvider provider)
    {
    return intValue();
    }

public long ToInt64(IFormatProvider provider)
    {
    return longValue();
    }

public sbyte ToSByte(IFormatProvider provider)
    {
    return checked((sbyte)intValue());
    }

public float ToSingle(IFormatProvider provider)
    {
    return floatValue();
    }

public string ToString(IFormatProvider provider)
    {
    return ToString();
    }

public object ToType(Type conversionType, IFormatProvider provider)
    {
throw new Exception("The method or operation is not implemented.");
    }

public ushort ToUInt16(IFormatProvider provider)
    {
throw new Exception("The method or operation is not implemented.");
    }

public uint ToUInt32(IFormatProvider provider)
    {
throw new Exception("The method or operation is not implemented.");
    }

public ulong ToUInt64(IFormatProvider provider)
    {
throw new Exception("The method or operation is not implemented.");
    }

#endregion
    }
}
