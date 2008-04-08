/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 31, 2008 */

package clojure.lang;

import java.math.BigInteger;
import java.math.BigDecimal;

public class Numbers{

static interface Ops{
	Ops combine(Ops y);
	Ops opsWith(IntegerOps x);
	Ops opsWith(FloatOps x);
	Ops opsWith(DoubleOps x);
	Ops opsWith(RatioOps x);
	Ops opsWith(BigIntegerOps x);
	Ops opsWith(BigDecimalOps x);

	public boolean isZero(Number x);
	public boolean isPos(Number x);
	public boolean isNeg(Number x);

	public Number add(Number x, Number y);
	public Number multiply(Number x, Number y);
	public Number divide(Number x, Number y);
	public Number quotient(Number x, Number y);
	public Number remainder(Number x, Number y);

	public boolean equiv(Number x, Number y);
	public boolean lt(Number x, Number y);

	public Number negate(Number x);
	public Number inc(Number x);
	public Number dec(Number x);
}

static public boolean isZero(Number x){
	return ops(x).isZero(x);
}
static public boolean isPos(Number x){
	return ops(x).isPos(x);
}
static public boolean isNeg(Number x){
	return ops(x).isNeg(x);
}

static public Number negate(Number x){
	return ops(x).negate(x);
}
static public Number inc(Number x){
	return ops(x).inc(x);
}
static public Number dec(Number x){
	return ops(x).dec(x);		
}

static public Number add(Number x, Number y){
	return ops(x).combine(ops(y)).add(x, y);
}

static public Number subtract(Number x, Number y){
	Ops yops = ops(y);
	return ops(x).combine(yops).add(x, yops.negate(y));
}

static public Number multiply(Number x, Number y){
	return ops(x).combine(ops(y)).multiply(x, y);
}

static public Number divide(Number x, Number y){
	Ops yops = ops(y);
	if(yops.isZero(y))
		throw new ArithmeticException("Divide by zero");
	return ops(x).combine(yops).divide(x, y);
}

static public Number quotient(Number x, Number y){
	Ops yops = ops(y);
	if(yops.isZero(y))
		throw new ArithmeticException("Divide by zero");
	return ops(x).combine(yops).quotient(x, y);
}

static public Number remainder(Number x, Number y){
	Ops yops = ops(y);
	if(yops.isZero(y))
		throw new ArithmeticException("Divide by zero");
	return ops(x).combine(yops).remainder(x, y);
}

static Number quotient(double n, double d){
	double q = n / d;
	if(q <= Integer.MAX_VALUE && q >= Integer.MIN_VALUE)
		{
		return (int) q;
		}
	else
		{ //bigint quotient
		return reduce(new BigDecimal(q).toBigInteger());
		}
}

static Number remainder(double n, double d){
	double q = n / d;
	if(q <= Integer.MAX_VALUE && q >= Integer.MIN_VALUE)
		{
		return (n - ((int) q) * d);
		}
	else
		{ //bigint quotient
		Number bq = reduce(new BigDecimal(q).toBigInteger());
		return (n - bq.doubleValue() * d);
		}
}

static public boolean equiv(Object x, Object y){
	return y instanceof Number && x instanceof Number
		&& equiv((Number) x, (Number) y);
	}

static public boolean equiv(Number x, Number y){
	return ops(x).combine(ops(y)).equiv(x, y);
}

static public boolean lt(Number x, Number y){
	return ops(x).combine(ops(y)).lt(x, y);
}

static public boolean lte(Number x, Number y){
	return !ops(x).combine(ops(y)).lt(y, x);
}

static public boolean gt(Number x, Number y){
	return ops(x).combine(ops(y)).lt(y, x);
}

static public boolean gte(Number x, Number y){
	return !ops(x).combine(ops(y)).lt(x, y);
}

static public int compare(Number x, Number y){
	Ops ops = ops(x).combine(ops(y));
	if(ops.lt(x,y))
		return -1;
	else if(ops.lt(y,x))
		return 1;
	return 0;
}

static BigInteger toBigInteger(Object x){
	if(x instanceof BigInteger)
		return (BigInteger) x;
	else
		return BigInteger.valueOf(((Number) x).longValue());
}

static BigDecimal toBigDecimal(Object x){
	if(x instanceof BigDecimal)
		return (BigDecimal) x;
	else if(x instanceof BigInteger)
		return new BigDecimal((BigInteger) x);
	else
		return BigDecimal.valueOf(((Number) x).longValue());
}

static Ratio toRatio(Object x){
	if(x instanceof Ratio)
		return (Ratio) x;
	else if(x instanceof BigDecimal)
		{
		BigDecimal bx = (BigDecimal) x;
		BigInteger bv = bx.unscaledValue();
		int scale = bx.scale();
		if(scale < 0)
			return new Ratio(bv.multiply(BigInteger.TEN.pow(-scale)),BigInteger.ONE);
		else
			return new Ratio(bv,BigInteger.TEN.pow(scale));
		}
	return new Ratio(toBigInteger(x), BigInteger.ONE);
}

static public Number rationalize(Number x){
	if(x instanceof Float || x instanceof Double)
		return rationalize(BigDecimal.valueOf(x.doubleValue()));
	else if(x instanceof BigDecimal)
		{
		BigDecimal bx = (BigDecimal) x;
		BigInteger bv = bx.unscaledValue();
		int scale = bx.scale();
		if(scale < 0)
			return bv.multiply(BigInteger.TEN.pow(-scale));
		else
			return divide(bv, BigInteger.TEN.pow(scale));
		}
	return x;
}

static public Number reduce(BigInteger val){
	if(val.bitLength() < 32)
		return val.intValue();
	else
		return val;
}

static public Number divide(BigInteger n, BigInteger d){
	if(d.equals(BigInteger.ZERO))
		throw new ArithmeticException("Divide by zero");
	BigInteger gcd = n.gcd(d);
	if(gcd.equals(BigInteger.ZERO))
		return 0;
	n = n.divide(gcd);
	d = d.divide(gcd);
	if(d.equals(BigInteger.ONE))
		return reduce(n);
	return new Ratio((d.signum() < 0 ? n.negate() : n),
	                 (d.signum() < 0 ? d.negate() : d));
}


final static class IntegerOps implements Ops{
	public Ops combine(Ops y){
		return y.opsWith(this);
	}

	final public Ops opsWith(IntegerOps x){return this;}
	final public Ops opsWith(FloatOps x){return FLOAT_OPS;}
	final public Ops opsWith(DoubleOps x){return DOUBLE_OPS;}
	final public Ops opsWith(RatioOps x){return RATIO_OPS;}
	final public Ops opsWith(BigIntegerOps x){return BIGINTEGER_OPS;}
	final public Ops opsWith(BigDecimalOps x){return BIGDECIMAL_OPS;}

	public boolean isZero(Number x){
		return x.intValue() == 0;
	}

	public boolean isPos(Number x){
		return x.intValue() > 0;
	}

	public boolean isNeg(Number x){
		return x.intValue() < 0;
	}

	final public Number add(Number x, Number y){
		long ret = x.longValue() + y.longValue();
		if(ret <= Integer.MAX_VALUE && ret >= Integer.MIN_VALUE)
			return (int)ret;
		return ret;
	}

	final public Number multiply(Number x, Number y){
		long ret = x.longValue() * y.longValue();
		if(ret <= Integer.MAX_VALUE && ret >= Integer.MIN_VALUE)
			return (int)ret;
		return ret;
	}

	static int gcd(int u, int v){
		while(v != 0)
			{
			int r = u % v;
			u = v;
			v = r;
			}
		return u;
	}

	public Number divide(Number x, Number y){
		int n = x.intValue();
		int val = y.intValue();
		int gcd = gcd(n, val);
		if(gcd == 0)
			return 0;

		n = n / gcd;
		int d = val / gcd;
		if(d == 1)
			return n;
		if(d < 0)
			{
			n = -n;
			d = -d;
			}
		return new Ratio(BigInteger.valueOf(n), BigInteger.valueOf(d));
	}

	public Number quotient(Number x, Number y){
		return x.intValue() / y.intValue();
	}

	public Number remainder(Number x, Number y){
		return x.intValue() % y.intValue();
	}

	public boolean equiv(Number x, Number y){
		return x.intValue() == y.intValue();
	}

	public boolean lt(Number x, Number y){
		return x.intValue() < y.intValue();
	}

	//public Number subtract(Number x, Number y);
	final public Number negate(Number x){
		return -x.intValue();
	}

	public Number inc(Number x){
		int val = x.intValue();
		if(val < Integer.MAX_VALUE)
			return val+1;
		return BigInteger.valueOf((long)val + 1);
	}

	public Number dec(Number x){
		int val = x.intValue();
		if(val > Integer.MIN_VALUE)
			return val-1;
		return BigInteger.valueOf((long)val - 1);
	}
}

final static class FloatOps implements Ops{
	public Ops combine(Ops y){
		return y.opsWith(this);
	}

	final public Ops opsWith(IntegerOps x){return this;}
	final public Ops opsWith(FloatOps x){return this;}
	final public Ops opsWith(DoubleOps x){return DOUBLE_OPS;}
	final public Ops opsWith(RatioOps x){return this;}
	final public Ops opsWith(BigIntegerOps x){return this;}
	final public Ops opsWith(BigDecimalOps x){return this;}

	public boolean isZero(Number x){
		return x.floatValue() == 0;
	}

	public boolean isPos(Number x){
		return x.floatValue() > 0;
	}

	public boolean isNeg(Number x){
		return x.floatValue() < 0;
	}

	final public Number add(Number x, Number y){
		return x.floatValue() + y.floatValue();
	}

	final public Number multiply(Number x, Number y){
		return x.floatValue() * y.floatValue();
	}

	public Number divide(Number x, Number y){
		return x.floatValue() / y.floatValue();
	}

	public Number quotient(Number x, Number y){
		return Numbers.quotient(x.doubleValue(), y.doubleValue());
	}

	public Number remainder(Number x, Number y){
		return Numbers.remainder(x.doubleValue(), y.doubleValue());
	}

	public boolean equiv(Number x, Number y){
		return x.floatValue() == y.floatValue();
	}

	public boolean lt(Number x, Number y){
		return x.floatValue() < y.floatValue();
	}

	//public Number subtract(Number x, Number y);
	final public Number negate(Number x){
		return -x.floatValue();
	}

	public Number inc(Number x){
		return x.floatValue() + 1;
	}

	public Number dec(Number x){
		return x.floatValue() - 1;
	}
}

final static class DoubleOps implements Ops{
	public Ops combine(Ops y){
		return y.opsWith(this);
	}

	final public Ops opsWith(IntegerOps x){return this;}
	final public Ops opsWith(FloatOps x){return this;}
	final public Ops opsWith(DoubleOps x){return this;}
	final public Ops opsWith(RatioOps x){return this;}
	final public Ops opsWith(BigIntegerOps x){return this;}
	final public Ops opsWith(BigDecimalOps x){return this;}

	public boolean isZero(Number x){
		return x.doubleValue() == 0;
	}

	public boolean isPos(Number x){
		return x.doubleValue() > 0;
	}

	public boolean isNeg(Number x){
		return x.doubleValue() < 0;
	}

	final public Number add(Number x, Number y){
		return x.doubleValue() + y.doubleValue();
	}

	final public Number multiply(Number x, Number y){
		return x.doubleValue() * y.doubleValue();
	}

	public Number divide(Number x, Number y){
		return x.doubleValue() / y.doubleValue();
	}

	public Number quotient(Number x, Number y){
		return Numbers.quotient(x.doubleValue(), y.doubleValue());
	}

	public Number remainder(Number x, Number y){
		return Numbers.remainder(x.doubleValue(), y.doubleValue());
	}
	
	public boolean equiv(Number x, Number y){
		return x.doubleValue() == y.doubleValue();
	}

	public boolean lt(Number x, Number y){
		return x.doubleValue() < y.doubleValue();
	}

	//public Number subtract(Number x, Number y);
	final public Number negate(Number x){
		return -x.doubleValue();
	}

	public Number inc(Number x){
		return x.doubleValue() + 1;
	}

	public Number dec(Number x){
		return x.doubleValue() - 1;
	}
}

final static class RatioOps implements Ops{
	public Ops combine(Ops y){
		return y.opsWith(this);
	}

	final public Ops opsWith(IntegerOps x){return this;}
	final public Ops opsWith(FloatOps x){return FLOAT_OPS;}
	final public Ops opsWith(DoubleOps x){return DOUBLE_OPS;}
	final public Ops opsWith(RatioOps x){return this;}
	final public Ops opsWith(BigIntegerOps x){return this;}
	final public Ops opsWith(BigDecimalOps x){return this;}

	public boolean isZero(Number x){
		Ratio r = (Ratio) x;
		return r.numerator.signum() == 0;
	}

	public boolean isPos(Number x){
		Ratio r = (Ratio) x;
		return r.numerator.signum() > 0;
	}

	public boolean isNeg(Number x){
		Ratio r = (Ratio) x;
		return r.numerator.signum() < 0;
	}

	final public Number add(Number x, Number y){
		Ratio rx = toRatio(x);
		Ratio ry = toRatio(y);
		return divide(ry.numerator.multiply(rx.denominator)
				.add(rx.numerator.multiply(ry.denominator))
				, ry.denominator.multiply(rx.denominator));
	}

	final public Number multiply(Number x, Number y){
		Ratio rx = toRatio(x);
		Ratio ry = toRatio(y);
		return Numbers.divide(ry.numerator.multiply(rx.numerator)
						, ry.denominator.multiply(rx.denominator));
	}

	public Number divide(Number x, Number y){
		Ratio rx = toRatio(x);
		Ratio ry = toRatio(y);
		return Numbers.divide(ry.denominator.multiply(rx.numerator)
			, ry.numerator.multiply(rx.denominator));
	}

	public Number quotient(Number x, Number y){
		Ratio rx = toRatio(x);
		Ratio ry = toRatio(y);
		BigInteger q = rx.numerator.multiply(ry.denominator).divide(
	                           rx.denominator.multiply(ry.numerator));
		return reduce(q);
	}

	public Number remainder(Number x, Number y){
		Ratio rx = toRatio(x);
		Ratio ry = toRatio(y);
		BigInteger q = rx.numerator.multiply(ry.denominator).divide(
	                           rx.denominator.multiply(ry.numerator));
		return Numbers.subtract(x, Numbers.multiply(q, y));
	}

	public boolean equiv(Number x, Number y){
		Ratio rx = toRatio(x);
		Ratio ry = toRatio(y);
		return rx.numerator.equals(ry.numerator)
		       && rx.denominator.equals(ry.denominator);
	}

	public boolean lt(Number x, Number y){
		Ratio rx = toRatio(x);
		Ratio ry = toRatio(y);
		return Numbers.lt(rx.numerator.multiply(ry.denominator),ry.numerator.multiply(rx.denominator));
	}

	//public Number subtract(Number x, Number y);
	final public Number negate(Number x){
		Ratio r = (Ratio) x;
		return new Ratio(r.numerator.negate(), r.denominator);
	}

	public Number inc(Number x){
		return Numbers.add(x,1);
	}

	public Number dec(Number x){
		return Numbers.add(x,-1);
	}

}

final static class BigIntegerOps implements Ops{
	public Ops combine(Ops y){
		return y.opsWith(this);
	}

	final public Ops opsWith(IntegerOps x){return this;}
	final public Ops opsWith(FloatOps x){return FLOAT_OPS;}
	final public Ops opsWith(DoubleOps x){return DOUBLE_OPS;}
	final public Ops opsWith(RatioOps x){return RATIO_OPS;}
	final public Ops opsWith(BigIntegerOps x){return this;}
	final public Ops opsWith(BigDecimalOps x){return BIGDECIMAL_OPS;}

	public boolean isZero(Number x){
		BigInteger bx = toBigInteger(x);
		return bx.signum() == 0;
	}

	public boolean isPos(Number x){
		BigInteger bx = toBigInteger(x);
		return bx.signum() > 0;
	}

	public boolean isNeg(Number x){
		BigInteger bx = toBigInteger(x);
		return bx.signum() < 0;
	}

	final public Number add(Number x, Number y){
		return reduce(toBigInteger(x).add(toBigInteger(y)));
	}

	final public Number multiply(Number x, Number y){
		return reduce(toBigInteger(x).multiply(toBigInteger(y)));
	}

	public Number divide(Number x, Number y){
		return Numbers.divide(toBigInteger(x), toBigInteger(y));
	}

	public Number quotient(Number x, Number y){
		return toBigInteger(x).divide(toBigInteger(y));
	}

	public Number remainder(Number x, Number y){
		return toBigInteger(x).remainder(toBigInteger(y));
	}

	public boolean equiv(Number x, Number y){
		return toBigInteger(x).equals(toBigInteger(y));
	}

	public boolean lt(Number x, Number y){
		return toBigInteger(x).compareTo(toBigInteger(y)) < 0;
	}

	//public Number subtract(Number x, Number y);
	final public Number negate(Number x){
		return toBigInteger(x).negate();
	}

	public Number inc(Number x){
		BigInteger bx = toBigInteger(x);
		return reduce(bx.add(BigInteger.ONE));
	}

	public Number dec(Number x){
		BigInteger bx = toBigInteger(x);
		return reduce(bx.subtract(BigInteger.ONE));
	}
}

final static class BigDecimalOps implements Ops{
	public Ops combine(Ops y){
		return y.opsWith(this);
	}

	final public Ops opsWith(IntegerOps x){return this;}
	final public Ops opsWith(FloatOps x){return FLOAT_OPS;}
	final public Ops opsWith(DoubleOps x){return DOUBLE_OPS;}
	final public Ops opsWith(RatioOps x){return RATIO_OPS;}
	final public Ops opsWith(BigIntegerOps x){return this;}
	final public Ops opsWith(BigDecimalOps x){return this;}

	public boolean isZero(Number x){
		BigDecimal bx = (BigDecimal) x;
		return bx.signum() == 0;
	}

	public boolean isPos(Number x){
		BigDecimal bx = (BigDecimal) x;
		return bx.signum() > 0;
	}

	public boolean isNeg(Number x){
		BigDecimal bx = (BigDecimal) x;
		return bx.signum() < 0;
	}

	final public Number add(Number x, Number y){
		return toBigDecimal(x).add(toBigDecimal(y));
	}

	final public Number multiply(Number x, Number y){
		return toBigDecimal(x).multiply(toBigDecimal(y));
	}

	public Number divide(Number x, Number y){
		return toBigDecimal(x).divide(toBigDecimal(y));
	}

	public Number quotient(Number x, Number y){
		return toBigDecimal(x).divideToIntegralValue(toBigDecimal(y));
	}

	public Number remainder(Number x, Number y){
		return toBigDecimal(x).remainder(toBigDecimal(y));
	}

	public boolean equiv(Number x, Number y){
		return toBigDecimal(x).equals(toBigDecimal(y));
	}

	public boolean lt(Number x, Number y){
		return toBigDecimal(x).compareTo(toBigDecimal(y)) < 0;
	}

	//public Number subtract(Number x, Number y);
	final public Number negate(Number x){
		return ((BigDecimal)x).negate();
	}

	public Number inc(Number x){
		BigDecimal bx = (BigDecimal) x;
		return bx.add(BigDecimal.ONE);
	}

	public Number dec(Number x){
		BigDecimal bx = (BigDecimal) x;
		return bx.subtract(BigDecimal.ONE);
	}
}

static final IntegerOps INTEGER_OPS = new IntegerOps();
static final FloatOps FLOAT_OPS = new FloatOps();
static final DoubleOps DOUBLE_OPS = new DoubleOps();
static final RatioOps RATIO_OPS = new RatioOps();
static final BigIntegerOps BIGINTEGER_OPS = new BigIntegerOps();
static final BigDecimalOps BIGDECIMAL_OPS = new BigDecimalOps();

static Ops ops(Object x){
	Class xc = x.getClass();

	if(xc == Integer.class)
		return INTEGER_OPS;
	else if(xc == Double.class)
		return DOUBLE_OPS;
	else if(xc == Float.class)
		return FLOAT_OPS;
	else if(xc == BigInteger.class)
		return BIGINTEGER_OPS;
	else if(xc == Long.class)
		return BIGINTEGER_OPS;
	else if(xc == Ratio.class)
		return RATIO_OPS;
	else if(xc == BigDecimal.class)
		return BIGDECIMAL_OPS;
	else
		return INTEGER_OPS;
}

}
