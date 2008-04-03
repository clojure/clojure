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

static public Object add(Object x, Object y){
	return ops(x).combine(ops(y)).add(x, y);
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
			return new Ratio(bv, BigInteger.TEN.pow(-scale));
		else
			return new Ratio(bv.multiply(BigInteger.TEN.pow(scale)), BigInteger.ONE);
		}
	return new Ratio(toBigInteger(x), BigInteger.ONE);
}

static public Number reduce(BigInteger val){
	if(val.bitLength() < 32)
		return val.intValue();
	else if(val.bitLength() < 64)
		return val.longValue();
	else
		return val;
}

static public Object divide(BigInteger n, BigInteger d){
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

static interface Ops{
	Ops combine(Ops y);
	Ops opsWith(IntegerOps x);
	Ops opsWith(FloatOps x);
	Ops opsWith(DoubleOps x);
	Ops opsWith(RatioOps x);
	Ops opsWith(BigIntegerOps x);
	Ops opsWith(BigDecimalOps x);
	Ops opsWith(FloatArrayOps x);
	Ops opsWith(DoubleArrayOps x);

	public Object add(Object x, Object y);
}

static class IntegerOps implements Ops{
	public Ops combine(Ops y){
		return y.opsWith(this);
	}

	final public Ops opsWith(IntegerOps x){return this;}
	final public Ops opsWith(FloatOps x){return FLOAT_OPS;}
	final public Ops opsWith(DoubleOps x){return DOUBLE_OPS;}
	final public Ops opsWith(RatioOps x){return RATIO_OPS;}
	final public Ops opsWith(BigIntegerOps x){return BIGINTEGER_OPS;}
	final public Ops opsWith(BigDecimalOps x){return BIGDECIMAL_OPS;}
	final public Ops opsWith(FloatArrayOps x){return FLOATARRAY_OPS;}
	final public Ops opsWith(DoubleArrayOps x){return DOUBLEARRAY_OPS;}

	final public Object add(Object x, Object y){
		long ret = ((Number) x).longValue() + ((Number) y).longValue();
		if(ret <= Integer.MAX_VALUE && ret >= Integer.MIN_VALUE)
			return (int)ret;
		return ret;
	}
}

static class FloatOps implements Ops{
	public Ops combine(Ops y){
		return y.opsWith(this);
	}

	final public Ops opsWith(IntegerOps x){return this;}
	final public Ops opsWith(FloatOps x){return this;}
	final public Ops opsWith(DoubleOps x){return DOUBLE_OPS;}
	final public Ops opsWith(RatioOps x){return this;}
	final public Ops opsWith(BigIntegerOps x){return this;}
	final public Ops opsWith(BigDecimalOps x){return this;}
	final public Ops opsWith(FloatArrayOps x){return FLOATARRAY_OPS;}
	final public Ops opsWith(DoubleArrayOps x){return DOUBLEARRAY_OPS;}

	final public Object add(Object x, Object y){
		return ((Number) x).floatValue() + ((Number) y).floatValue();
	}
}

static class DoubleOps implements Ops{
	public Ops combine(Ops y){
		return y.opsWith(this);
	}

	final public Ops opsWith(IntegerOps x){return this;}
	final public Ops opsWith(FloatOps x){return this;}
	final public Ops opsWith(DoubleOps x){return this;}
	final public Ops opsWith(RatioOps x){return this;}
	final public Ops opsWith(BigIntegerOps x){return this;}
	final public Ops opsWith(BigDecimalOps x){return this;}
	final public Ops opsWith(FloatArrayOps x){return FLOATARRAY_OPS;}
	final public Ops opsWith(DoubleArrayOps x){return DOUBLEARRAY_OPS;}

	final public Object add(Object x, Object y){
		return ((Number) x).doubleValue() + ((Number) y).doubleValue();
	}
}

static class RatioOps implements Ops{
	public Ops combine(Ops y){
		return y.opsWith(this);
	}

	final public Ops opsWith(IntegerOps x){return this;}
	final public Ops opsWith(FloatOps x){return FLOAT_OPS;}
	final public Ops opsWith(DoubleOps x){return DOUBLE_OPS;}
	final public Ops opsWith(RatioOps x){return this;}
	final public Ops opsWith(BigIntegerOps x){return this;}
	final public Ops opsWith(BigDecimalOps x){return this;}
	final public Ops opsWith(FloatArrayOps x){return FLOATARRAY_OPS;}
	final public Ops opsWith(DoubleArrayOps x){return DOUBLEARRAY_OPS;}

	final public Object add(Object x, Object y){
		Ratio rx = toRatio(x);
		Ratio ry = toRatio(y);
		return divide(rx.numerator.multiply(rx.denominator)
				.add(rx.numerator.multiply(ry.denominator))
				, ry.denominator.multiply(rx.denominator));
	}
}
static class BigIntegerOps implements Ops{
	public Ops combine(Ops y){
		return y.opsWith(this);
	}

	final public Ops opsWith(IntegerOps x){return this;}
	final public Ops opsWith(FloatOps x){return FLOAT_OPS;}
	final public Ops opsWith(DoubleOps x){return DOUBLE_OPS;}
	final public Ops opsWith(RatioOps x){return RATIO_OPS;}
	final public Ops opsWith(BigIntegerOps x){return this;}
	final public Ops opsWith(BigDecimalOps x){return BIGDECIMAL_OPS;}
	final public Ops opsWith(FloatArrayOps x){return FLOATARRAY_OPS;}
	final public Ops opsWith(DoubleArrayOps x){return DOUBLEARRAY_OPS;}

	final public Object add(Object x, Object y){
		return reduce(toBigInteger(x).add(toBigInteger(y)));
	}
}

static class BigDecimalOps implements Ops{
	public Ops combine(Ops y){
		return y.opsWith(this);
	}

	final public Ops opsWith(IntegerOps x){return this;}
	final public Ops opsWith(FloatOps x){return FLOAT_OPS;}
	final public Ops opsWith(DoubleOps x){return DOUBLE_OPS;}
	final public Ops opsWith(RatioOps x){return RATIO_OPS;}
	final public Ops opsWith(BigIntegerOps x){return this;}
	final public Ops opsWith(BigDecimalOps x){return this;}
	final public Ops opsWith(FloatArrayOps x){return FLOATARRAY_OPS;}
	final public Ops opsWith(DoubleArrayOps x){return DOUBLEARRAY_OPS;}

	final public Object add(Object x, Object y){
		return toBigDecimal(x).add(toBigDecimal(y));
	}
}

static class FloatArrayOps implements Ops{
	public Ops combine(Ops y){
		return y.opsWith(this);
	}

	final boolean first;

	FloatArrayOps(boolean first){
		this.first = first;
	}

	final public Ops opsWith(IntegerOps x){return FLOATARRAY_OPS_REV;}
	final public Ops opsWith(FloatOps x){return FLOATARRAY_OPS_REV;}
	final public Ops opsWith(DoubleOps x){return FLOATARRAY_OPS_REV;}
	final public Ops opsWith(RatioOps x){return FLOATARRAY_OPS_REV;}
	final public Ops opsWith(BigIntegerOps x){return FLOATARRAY_OPS_REV;}
	final public Ops opsWith(BigDecimalOps x){return FLOATARRAY_OPS_REV;}
	final public Ops opsWith(FloatArrayOps x){return x;}
	final public Ops opsWith(DoubleArrayOps x){return x;}

	final public Object add(Object x, Object y){
		float[] a = (float[]) (first ? x : y);
		Object arg = first ? y : x;
		float[] ret = new float[a.length];
		if(arg instanceof Number)
			{
			float yd = ((Number) arg).floatValue();
			for(int i = 0; i < a.length; i++)
				ret[i] = a[i] + yd;
			}
		else if(arg instanceof double[])
			{
			double[] ya = (double[]) arg;
			for(int i = 0; i < a.length; i++)
				ret[i] = a[i] + (float) ya[i];
			}
		else
			{
			float[] ya = (float[]) arg;
			for(int i = 0; i < a.length; i++)
				ret[i] = a[i] + ya[i];
			}
		return ret;
	}
}

static class DoubleArrayOps implements Ops{
	public Ops combine(Ops y){
		return y.opsWith(this);
	}

	final boolean first;

	DoubleArrayOps(boolean first){
		this.first = first;
	}

	final public Ops opsWith(IntegerOps x){return DOUBLEARRAY_OPS_REV;}
	final public Ops opsWith(FloatOps x){return DOUBLEARRAY_OPS_REV;}
	final public Ops opsWith(DoubleOps x){return DOUBLEARRAY_OPS_REV;}
	final public Ops opsWith(RatioOps x){return DOUBLEARRAY_OPS_REV;}
	final public Ops opsWith(BigIntegerOps x){return DOUBLEARRAY_OPS_REV;}
	final public Ops opsWith(BigDecimalOps x){return DOUBLEARRAY_OPS_REV;}
	final public Ops opsWith(FloatArrayOps x){return DOUBLEARRAY_OPS_REV;}
	final public Ops opsWith(DoubleArrayOps x){return x;}

	final public Object add(Object x, Object y){
		double[] a = (double[]) (first ? x : y);
		Object arg = first ? y : x;
		double[] ret = new double[a.length];
		if(arg instanceof Number)
			{
			double yd = ((Number) arg).doubleValue();
			for(int i = 0; i < a.length; i++)
				ret[i] = a[i] + yd;
			}
		else if(arg instanceof double[])
			{
			double[] ya = (double[]) arg;
			for(int i = 0; i < a.length; i++)
				ret[i] = a[i] + ya[i];
			}
		else
			{
			float[] ya = (float[]) arg;
			for(int i = 0; i < a.length; i++)
				ret[i] = a[i] + ya[i];
			}
		return ret;
	}
}

static final IntegerOps INTEGER_OPS = new IntegerOps();
static final FloatOps FLOAT_OPS = new FloatOps();
static final DoubleOps DOUBLE_OPS = new DoubleOps();
static final RatioOps RATIO_OPS = new RatioOps();
static final BigIntegerOps BIGINTEGER_OPS = new BigIntegerOps();
static final BigDecimalOps BIGDECIMAL_OPS = new BigDecimalOps();
static final FloatArrayOps FLOATARRAY_OPS = new FloatArrayOps(true);
static final DoubleArrayOps DOUBLEARRAY_OPS = new DoubleArrayOps(true);
static final FloatArrayOps FLOATARRAY_OPS_REV = new FloatArrayOps(false);
static final DoubleArrayOps DOUBLEARRAY_OPS_REV = new DoubleArrayOps(false);


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
	else if(xc == Ratio.class)
		return RATIO_OPS;
	else if(xc == BigDecimal.class)
		return BIGDECIMAL_OPS;
	else if(xc == double[].class)
		return DOUBLEARRAY_OPS;
	else if(xc == float[].class)
		return FLOATARRAY_OPS;
	else
		return INTEGER_OPS;
}

}
