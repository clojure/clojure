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

//static public Object add(Object x, Object y){
//	return ops(x).add(x, y);
//}


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
//	public Object add(Object x, Object y);
}

/*

static final IntegerOps INTEGER_OPS = new IntegerOps();
//Ops(){
//	final public Object add(Object x, Object y){
//		long ret = ((Number) x).longValue() + ((Number) y).longValue();
//		if(ret <= Integer.MAX_VALUE && ret >= Integer.MIN_VALUE)
//			return (int) ret;
//		return ret;
//	}
//};
static class IntegerOps implements Ops{
	final public Ops opsWith(IntegerOps x){return INTEGER_OPS;}
	final public Ops opsWith(FloatOps x){return INTEGER_OPS;}
	final public Ops opsWith(DoubleOps x){return INTEGER_OPS;}
	final public Ops opsWith(RatioOps x){return INTEGER_OPS;}
	final public Ops opsWith(BigDecimal x){return INTEGER_OPS;}
	final public Ops opsWith(BigIntegerOps x){return INTEGER_OPS;}
	final public Ops opsWith(BigDecimalOps x){return INTEGER_OPS;}
	final public Ops opsWith(FloatArrayOps x){return INTEGER_OPS;}
	final public Ops opsWith(DoubleArrayOps x){return INTEGER_OPS;}

	final public Object add(Object x, Object y){
		long ret = ((Number) x).longValue() + ((Number) y).longValue();
		if(ret <= Integer.MAX_VALUE && ret >= Integer.MIN_VALUE)
			return (int)ret;
		return ret;
	}
}

static class FloatOps implements Ops{}
static class DoubleOps implements Ops{}
static class RatioOps implements Ops{}
static class BigIntegerOps implements Ops{}
static class BigDecimalOps implements Ops{}
static class FloatArrayOps implements Ops{}
static class DoubleArrayOps implements Ops{}

static final FloatOps FLOAT_OPS = new FloatOps();
static final DoubleOps INTEGER_OPS = new DoubleOps();
static final RatioOps INTEGER_OPS = new RatioOps();
static final BigIntegerOps INTEGER_OPS = new BigIntegerOps();
static final BigDecimalOps INTEGER_OPS = new BigDecimalOps();
static final FloatArrayOps INTEGER_OPS = new FloatArrayOps();
static final DoubleArrayOps INTEGER_OPS = new DoubleArrayOps();
  */
static final Ops doubleOps = new Ops(){
	final public Object add(Object x, Object y){
		return ((Number) x).doubleValue() + ((Number) y).doubleValue();
	}
};

static final Ops floatOps = new Ops(){
	final public Object add(Object x, Object y){
		return ((Number) x).floatValue() + ((Number) y).floatValue();
	}
};

static final Ops ratioOps = new Ops(){
	final public Object add(Object x, Object y){
		Ratio rx = toRatio(x);
		Ratio ry = toRatio(y);
		return divide(rx.numerator.multiply(rx.denominator)
				.add(rx.numerator.multiply(ry.denominator))
				, ry.denominator.multiply(rx.denominator));
	}
};

static final Ops bigdecOps = new Ops(){
	final public Object add(Object x, Object y){
		return toBigDecimal(x).add(toBigDecimal(y));
	}
};

static final Ops bigintOps = new Ops(){
	final public Object add(Object x, Object y){
		return reduce(toBigInteger(x).add(toBigInteger(y)));
	}
};
/*
static class DoubleArrayOps implements Ops{
	final boolean first;

	DoubleArrayOps(boolean first){
		this.first = first;
	}

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
static class FloatArrayOps implements Ops{
	final boolean first;

	FloatArrayOps(boolean first){
		this.first = first;
	}

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
*/

/*

static Ops ops(Object x){
	Class xc = x.getClass();

	if(xc == Integer.class)
		return INTEGER_OPS;
	else if(xc == Double.class)
		return doubleOps;
	else if(xc == Float.class)
		return floatOps;
	else if(xc == BigInteger.class)
		return bigintOps;
	else if(xc == Ratio.class)
		return ratioOps;
	else if(xc == BigDecimal.class)
		return bigdecOps;
	else if(xc == double[].class)
		return new DoubleArrayOps(true);
	else if(xc == float[].class)
		return new FloatArrayOps(true);

	return INTEGER_OPS;
}
static Ops ops(Object x, Object y){
	Class xc = x.getClass();
	Class yc = y.getClass();

	if(xc == Integer.class)// && yc == Integer.class)
		return INTEGER_OPS.ops(y);

	else if(xc.isArray() || yc.isArray())
		{
		if(xc == double[].class)
			return new DoubleArrayOps(true);
		else if(yc == double[].class)
			return new DoubleArrayOps(false);
		else if(xc == float[].class)
			return new FloatArrayOps(true);
		else if(yc == float[].class)
			return new FloatArrayOps(false);
		throw new IllegalArgumentException("Unsupported array type");
		}
	else if(xc == Double.class || yc == Double.class)
		return doubleOps;
	else if(xc == Float.class || yc == Float.class)
		return floatOps;
	else if(xc == Ratio.class || yc == Ratio.class)
		return ratioOps;
	else if(xc == BigDecimal.class || yc == BigDecimal.class)
		return bigdecOps;
	else if(xc == BigInteger.class || yc == BigInteger.class
	        || xc == Long.class || yc == Long.class)
		return bigintOps;


	return INTEGER_OPS;
}
//	*/

/*
static Object add(Object x, Object y){
	Class c = x.getClass();
	if(c == Integer.class)
		return add(((Integer) x).intValue(), y);
	else if(c == Double.class)
		return add(((Double) x).doubleValue(), y);
	else if(c == Long.class)
		return add(((Long) x).longValue(), y);
	else if(c == Float.class)
		return add(((Float) x).floatValue(), y);
	else if(c == BigInteger.class)
		return add((BigInteger) x, y);
	else if(c == BigDecimal.class)
		return add((BigDecimal) x, y);
	else if(c == Ratio.class)
		return add((Ratio) x, y);
	else if(c == double[].class)
		return add((double[]) x, y);
	else if(c == float[].class)
		return add((float[]) x, y);
	else
		return add(((Number) x).intValue(), y);
}
   */
public static Object add(int x, Object y){
	Class c = y.getClass();
	if(c == Integer.class)
		return add(((Integer) y).intValue(), x);
	else if(c == Double.class)
		return x + (Double) y;
	else if(c == Long.class)
		return add(((Long) y).longValue(), x);
	else if(c == Float.class)
		return x + (Float) y;
	else if(c == BigInteger.class)
		return BigInteger.valueOf(x).add((BigInteger) y);
	else if(c == BigDecimal.class)
		return BigDecimal.valueOf(x).add((BigDecimal) y);
	else if(c == Ratio.class)
		return add((Ratio) y, x);
	else if(c == double[].class)
		{
		return add(((double[]) y), x);
		}
	else if(c == float[].class)
		{
		return add(((float[]) y), x);
		}
	else
		return add(((Number) y).intValue(), x);
}

public static double[] add(double[] x, double y){
	double[] ret = new double[x.length];//x.clone();
	for(int i = 0; i < ret.length; i++)
		ret[i] = x[i] + y;
	return ret;
}

public static float[] add(float[] x, float y){
	float[] ret = x.clone();
	for(int i = 0; i < ret.length; i++)
		ret[i] += y;
	return ret;
}

public static Ratio add(Ratio x, int y){
	return null;
}

public static Double add(double x, double y){
	return x + y;
}

public static Float add(float x, float y){
	return x + y;
}

public static Number add(int x, int y){
	long ret = (long) x + (long) y;
	if(ret <= Integer.MAX_VALUE && ret >= Integer.MIN_VALUE)
		return (int) ret;
	return ret;
}
}
