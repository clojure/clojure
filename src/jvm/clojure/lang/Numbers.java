/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 31, 2008 */

package clojure.lang;

import java.math.BigInteger;
import java.math.BigDecimal;
import java.math.MathContext;

public class Numbers{

static interface Ops{
	Ops combine(Ops y);

	Ops opsWith(LongOps x);

	Ops opsWith(DoubleOps x);

	Ops opsWith(RatioOps x);

	Ops opsWith(BigIntOps x);

	Ops opsWith(BigDecimalOps x);

	public boolean isZero(Number x);

	public boolean isPos(Number x);

	public boolean isNeg(Number x);

	public Number add(Number x, Number y);
	public Number addP(Number x, Number y);

	public Number multiply(Number x, Number y);
	public Number multiplyP(Number x, Number y);

	public Number divide(Number x, Number y);

	public Number quotient(Number x, Number y);

	public Number remainder(Number x, Number y);

	public boolean equiv(Number x, Number y);

	public boolean lt(Number x, Number y);

	public Number negate(Number x);
	public Number negateP(Number x);

	public Number inc(Number x);
	public Number incP(Number x);

	public Number dec(Number x);
	public Number decP(Number x);
}

static abstract class OpsP implements Ops{
	public Number addP(Number x, Number y){
		return add(x, y);
	}

	public Number multiplyP(Number x, Number y){
		return multiply(x, y);
	}

	public Number negateP(Number x){
		return negate(x);
	}

	public Number incP(Number x){
		return inc(x);
	}

	public Number decP(Number x){
		return dec(x);
	}

}

static public boolean isZero(Object x){
	return ops(x).isZero((Number)x);
}

static public boolean isPos(Object x){
	return ops(x).isPos((Number)x);
}

static public boolean isNeg(Object x){
	return ops(x).isNeg((Number)x);
}

static public Number minus(Object x){
	return ops(x).negate((Number)x);
}

static public Number minusP(Object x){
	return ops(x).negateP((Number)x);
}

static public Number inc(Object x){
	return ops(x).inc((Number)x);
}

static public Number incP(Object x){
	return ops(x).incP((Number)x);
}

static public Number dec(Object x){
	return ops(x).dec((Number)x);
}

static public Number decP(Object x){
	return ops(x).decP((Number)x);
}

static public Number add(Object x, Object y){
	return ops(x).combine(ops(y)).add((Number)x, (Number)y);
}

static public Number addP(Object x, Object y){
	return ops(x).combine(ops(y)).addP((Number)x, (Number)y);
}

static public Number minus(Object x, Object y){
	Ops yops = ops(y);
	return ops(x).combine(yops).add((Number)x, yops.negate((Number)y));
}

static public Number minusP(Object x, Object y){
	Ops yops = ops(y);
	Number negativeY = yops.negateP((Number) y);
	Ops negativeYOps = ops(negativeY);
	return ops(x).combine(negativeYOps).addP((Number)x, negativeY);
}

static public Number multiply(Object x, Object y){
	return ops(x).combine(ops(y)).multiply((Number)x, (Number)y);
}

static public Number multiplyP(Object x, Object y){
	return ops(x).combine(ops(y)).multiplyP((Number)x, (Number)y);
}

static public Number divide(Object x, Object y){
	Ops yops = ops(y);
	if(yops.isZero((Number)y))
		throw new ArithmeticException("Divide by zero");
	return ops(x).combine(yops).divide((Number)x, (Number)y);
}

static public Number quotient(Object x, Object y){
	Ops yops = ops(y);
	if(yops.isZero((Number) y))
		throw new ArithmeticException("Divide by zero");
	return ops(x).combine(yops).quotient((Number)x, (Number)y);
}

static public Number remainder(Object x, Object y){
	Ops yops = ops(y);
	if(yops.isZero((Number) y))
		throw new ArithmeticException("Divide by zero");
	return ops(x).combine(yops).remainder((Number)x, (Number)y);
}

static public double quotient(double n, double d){
	if(d == 0)
		throw new ArithmeticException("Divide by zero");

	double q = n / d;
	if(q <= Long.MAX_VALUE && q >= Long.MIN_VALUE)
		{
		return (double)(long) q;
		}
	else
		{ //bigint quotient
		return new BigDecimal(q).toBigInteger().doubleValue();
		}
}

static public double remainder(double n, double d){
	if(d == 0)
		throw new ArithmeticException("Divide by zero");

	double q = n / d;
	if(q <= Long.MAX_VALUE && q >= Long.MIN_VALUE)
		{
		return (n - ((long) q) * d);
		}
	else
		{ //bigint quotient
		Number bq = new BigDecimal(q).toBigInteger();
		return (n - bq.doubleValue() * d);
		}
}

static public boolean equiv(Object x, Object y){
	return equiv((Number) x, (Number) y);
}

static public boolean equiv(Number x, Number y){
	return ops(x).combine(ops(y)).equiv(x, y);
}

static public boolean equal(Number x, Number y){
	return category(x) == category(y)
			&& ops(x).combine(ops(y)).equiv(x, y);
}

static public boolean lt(Object x, Object y){
	return ops(x).combine(ops(y)).lt((Number)x, (Number)y);
}

static public boolean lte(Object x, Object y){
	return !ops(x).combine(ops(y)).lt((Number)y, (Number)x);
}

static public boolean gt(Object x, Object y){
	return ops(x).combine(ops(y)).lt((Number)y, (Number)x);
}

static public boolean gte(Object x, Object y){
	return !ops(x).combine(ops(y)).lt((Number)x, (Number)y);
}

static public int compare(Number x, Number y){
	Ops ops = ops(x).combine(ops(y));
	if(ops.lt(x, y))
		return -1;
	else if(ops.lt(y, x))
		return 1;
	return 0;
}

static BigInt toBigInt(Object x){
	if(x instanceof BigInt)
		return (BigInt) x;
	if(x instanceof BigInteger)
		return BigInt.fromBigInteger((BigInteger) x);
	else
		return BigInt.fromLong(((Number) x).longValue());
}

static BigInteger toBigInteger(Object x){
	if(x instanceof BigInteger)
		return (BigInteger) x;
	else if(x instanceof BigInt)
		return ((BigInt) x).toBigInteger();	
	else
		return BigInteger.valueOf(((Number) x).longValue());
}

static BigDecimal toBigDecimal(Object x){
	if(x instanceof BigDecimal)
		return (BigDecimal) x;
	else if(x instanceof BigInt)
		{
		BigInt bi = (BigInt) x;
		if(bi.bipart == null)
			return BigDecimal.valueOf(bi.lpart);
		else
			return new BigDecimal(bi.bipart);
		}
	else if(x instanceof BigInteger)
		return new BigDecimal((BigInteger) x);
	else if(x instanceof Double)
		return new BigDecimal(((Number) x).doubleValue());
	else if(x instanceof Float)
		return new BigDecimal(((Number) x).doubleValue());
	else if(x instanceof Ratio)
		{
		Ratio r = (Ratio)x;
		return (BigDecimal)divide(new BigDecimal(r.numerator), r.denominator);
		}
	else
		return BigDecimal.valueOf(((Number) x).longValue());
}

static public Ratio toRatio(Object x){
	if(x instanceof Ratio)
		return (Ratio) x;
	else if(x instanceof BigDecimal)
		{
		BigDecimal bx = (BigDecimal) x;
		BigInteger bv = bx.unscaledValue();
		int scale = bx.scale();
		if(scale < 0)
			return new Ratio(bv.multiply(BigInteger.TEN.pow(-scale)), BigInteger.ONE);
		else
			return new Ratio(bv, BigInteger.TEN.pow(scale));
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
			return BigInt.fromBigInteger(bv.multiply(BigInteger.TEN.pow(-scale)));
		else
			return divide(bv, BigInteger.TEN.pow(scale));
		}
	return x;
}

//static  Number box(int val){
//		return Integer.valueOf(val);
//}

//static  Number box(long val){
//		return Long.valueOf(val);
//}
//
//static  Double box(double val){
//		return Double.valueOf(val);
//}
//
//static  Double box(float val){
//		return Double.valueOf((double) val);
//}

static public Number reduceBigInt(BigInt val){
	if(val.bipart == null)
		return num(val.lpart);
	else
		return val.bipart;
}

static public Number divide(BigInteger n, BigInteger d){
	if(d.equals(BigInteger.ZERO))
		throw new ArithmeticException("Divide by zero");
	BigInteger gcd = n.gcd(d);
	if(gcd.equals(BigInteger.ZERO))
		return BigInt.ZERO;
	n = n.divide(gcd);
	d = d.divide(gcd);
	if(d.equals(BigInteger.ONE))
		return BigInt.fromBigInteger(n);
	else if(d.equals(BigInteger.ONE.negate()))
		return BigInt.fromBigInteger(n.negate());
	return new Ratio((d.signum() < 0 ? n.negate() : n),
	                 (d.signum() < 0 ? d.negate() : d));
}

static public int shiftLeftInt(int x, int n){
	return x << n;
}

static public long shiftLeft(Object x, Object y){
    return shiftLeft(bitOpsCast(x),bitOpsCast(y));
}
static public long shiftLeft(Object x, long y){
    return shiftLeft(bitOpsCast(x),y);
}
static public long shiftLeft(long x, Object y){
    return shiftLeft(x,bitOpsCast(y));
}
static public long shiftLeft(long x, long n){
	return x << n;
}

static public int shiftRightInt(int x, int n){
	return x >> n;
}

static public long shiftRight(Object x, Object y){
    return shiftRight(bitOpsCast(x),bitOpsCast(y));
}
static public long shiftRight(Object x, long y){
    return shiftRight(bitOpsCast(x),y);
}
static public long shiftRight(long x, Object y){
    return shiftRight(x,bitOpsCast(y));
}
static public long shiftRight(long x, long n){
	return x >> n;
}

final static class LongOps implements Ops{
	public Ops combine(Ops y){
		return y.opsWith(this);
	}

	final public Ops opsWith(LongOps x){
		return this;
	}

	final public Ops opsWith(DoubleOps x){
		return DOUBLE_OPS;
	}

	final public Ops opsWith(RatioOps x){
		return RATIO_OPS;
	}

	final public Ops opsWith(BigIntOps x){
		return BIGINT_OPS;
	}

	final public Ops opsWith(BigDecimalOps x){
		return BIGDECIMAL_OPS;
	}

	public boolean isZero(Number x){
		return x.longValue() == 0;
	}

	public boolean isPos(Number x){
		return x.longValue() > 0;
	}

	public boolean isNeg(Number x){
		return x.longValue() < 0;
	}

	final public Number add(Number x, Number y){
		return num(Numbers.add(x.longValue(),y.longValue()));
	}

	final public Number addP(Number x, Number y){
		long lx = x.longValue(), ly = y.longValue();
		long ret = lx + ly;
		if ((ret ^ lx) < 0 && (ret ^ ly) < 0)
			return BIGINT_OPS.add(x, y);
		return num(ret);
	}

	final public Number multiply(Number x, Number y){
		return num(Numbers.multiply(x.longValue(), y.longValue()));
	}

	final public Number multiplyP(Number x, Number y){
		long lx = x.longValue(), ly = y.longValue();
		long ret = lx * ly;
		if (ly != 0 && ret/ly != lx)
			return BIGINT_OPS.multiply(x, y);
		return num(ret);
	}
	static long gcd(long u, long v){
		while(v != 0)
			{
			long r = u % v;
			u = v;
			v = r;
			}
		return u;
	}

	public Number divide(Number x, Number y){
		long n = x.longValue();
		long val = y.longValue();
		long gcd = gcd(n, val);
		if(gcd == 0)
			return num(0);

		n = n / gcd;
		long d = val / gcd;
		if(d == 1)
			return num(n);
		if(d < 0)
			{
			n = -n;
			d = -d;
			}
		return new Ratio(BigInteger.valueOf(n), BigInteger.valueOf(d));
	}

	public Number quotient(Number x, Number y){
		return num(x.longValue() / y.longValue());
	}

	public Number remainder(Number x, Number y){
		return num(x.longValue() % y.longValue());
	}

	public boolean equiv(Number x, Number y){
		return x.longValue() == y.longValue();
	}

	public boolean lt(Number x, Number y){
		return x.longValue() < y.longValue();
	}

	//public Number subtract(Number x, Number y);
	final public Number negate(Number x){
		long val = x.longValue();
		return num(Numbers.minus(val));
	}

	final public Number negateP(Number x){
		long val = x.longValue();
		if(val > Long.MIN_VALUE)
			return num(-val);
		return BigInt.fromBigInteger(BigInteger.valueOf(val).negate());
	}
	public Number inc(Number x){
		long val = x.longValue();
		return num(Numbers.inc(val));
	}

	public Number incP(Number x){
		long val = x.longValue();
		if(val < Long.MAX_VALUE)
			return num(val + 1);
		return BIGINT_OPS.inc(x);
	}

	public Number dec(Number x){
		long val = x.longValue();
		return num(Numbers.dec(val));
	}

	public Number decP(Number x){
		long val = x.longValue();
		if(val > Long.MIN_VALUE)
			return num(val - 1);
		return BIGINT_OPS.dec(x);
	}
}

final static class DoubleOps extends OpsP{
	public Ops combine(Ops y){
		return y.opsWith(this);
	}

	final public Ops opsWith(LongOps x){
		return this;
	}

	final public Ops opsWith(DoubleOps x){
		return this;
	}

	final public Ops opsWith(RatioOps x){
		return this;
	}

	final public Ops opsWith(BigIntOps x){
		return this;
	}

	final public Ops opsWith(BigDecimalOps x){
		return this;
	}

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
		return Double.valueOf(x.doubleValue() + y.doubleValue());
	}

	final public Number multiply(Number x, Number y){
		return Double.valueOf(x.doubleValue() * y.doubleValue());
	}

	public Number divide(Number x, Number y){
		return Double.valueOf(x.doubleValue() / y.doubleValue());
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
		return Double.valueOf(-x.doubleValue());
	}

	public Number inc(Number x){
		return Double.valueOf(x.doubleValue() + 1);
	}

	public Number dec(Number x){
		return Double.valueOf(x.doubleValue() - 1);
	}
}

final static class RatioOps extends OpsP{
	public Ops combine(Ops y){
		return y.opsWith(this);
	}

	final public Ops opsWith(LongOps x){
		return this;
	}

	final public Ops opsWith(DoubleOps x){
		return DOUBLE_OPS;
	}

	final public Ops opsWith(RatioOps x){
		return this;
	}

	final public Ops opsWith(BigIntOps x){
		return this;
	}

	final public Ops opsWith(BigDecimalOps x){
		return BIGDECIMAL_OPS;
	}

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

	static Number normalizeRet(Number ret, Number x, Number y){
//		if(ret instanceof BigInteger && !(x instanceof BigInteger || y instanceof BigInteger))
//			{
//			return reduceBigInt((BigInteger) ret);
//			}
		return ret;
	}

	final public Number add(Number x, Number y){
		Ratio rx = toRatio(x);
		Ratio ry = toRatio(y);
		Number ret = divide(ry.numerator.multiply(rx.denominator)
				.add(rx.numerator.multiply(ry.denominator))
				, ry.denominator.multiply(rx.denominator));
		return normalizeRet(ret, x, y);
	}

	final public Number multiply(Number x, Number y){
		Ratio rx = toRatio(x);
		Ratio ry = toRatio(y);
		Number ret = Numbers.divide(ry.numerator.multiply(rx.numerator)
				, ry.denominator.multiply(rx.denominator));
		return normalizeRet(ret, x, y);
	}

	public Number divide(Number x, Number y){
		Ratio rx = toRatio(x);
		Ratio ry = toRatio(y);
		Number ret = Numbers.divide(ry.denominator.multiply(rx.numerator)
				, ry.numerator.multiply(rx.denominator));
		return normalizeRet(ret, x, y);
	}

	public Number quotient(Number x, Number y){
		Ratio rx = toRatio(x);
		Ratio ry = toRatio(y);
		BigInteger q = rx.numerator.multiply(ry.denominator).divide(
				rx.denominator.multiply(ry.numerator));
		return normalizeRet(BigInt.fromBigInteger(q), x, y);
	}

	public Number remainder(Number x, Number y){
		Ratio rx = toRatio(x);
		Ratio ry = toRatio(y);
		BigInteger q = rx.numerator.multiply(ry.denominator).divide(
				rx.denominator.multiply(ry.numerator));
		Number ret = Numbers.minus(x, Numbers.multiply(q, y));
		return normalizeRet(ret, x, y);
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
		return Numbers.lt(rx.numerator.multiply(ry.denominator), ry.numerator.multiply(rx.denominator));
	}

	//public Number subtract(Number x, Number y);
	final public Number negate(Number x){
		Ratio r = (Ratio) x;
		return new Ratio(r.numerator.negate(), r.denominator);
	}

	public Number inc(Number x){
		return Numbers.add(x, 1);
	}

	public Number dec(Number x){
		return Numbers.add(x, -1);
	}

}

final static class BigIntOps extends OpsP{
	public Ops combine(Ops y){
		return y.opsWith(this);
	}

	final public Ops opsWith(LongOps x){
		return this;
	}

	final public Ops opsWith(DoubleOps x){
		return DOUBLE_OPS;
	}

	final public Ops opsWith(RatioOps x){
		return RATIO_OPS;
	}

	final public Ops opsWith(BigIntOps x){
		return this;
	}

	final public Ops opsWith(BigDecimalOps x){
		return BIGDECIMAL_OPS;
	}

	public boolean isZero(Number x){
		BigInt bx = toBigInt(x);
		if(bx.bipart == null)
			return bx.lpart == 0;
		return bx.bipart.signum() == 0;
	}

	public boolean isPos(Number x){
		BigInt bx = toBigInt(x);
		if(bx.bipart == null)
			return bx.lpart > 0;
		return bx.bipart.signum() > 0;
	}

	public boolean isNeg(Number x){
		BigInt bx = toBigInt(x);
		if(bx.bipart == null)
			return bx.lpart < 0;
		return bx.bipart.signum() < 0;
	}

	final public Number add(Number x, Number y){
        return toBigInt(x).add(toBigInt(y));
	}

	final public Number multiply(Number x, Number y){
        return toBigInt(x).multiply(toBigInt(y));
	}

	public Number divide(Number x, Number y){
		return Numbers.divide(toBigInteger(x), toBigInteger(y));
	}

	public Number quotient(Number x, Number y){
        return toBigInt(x).quotient(toBigInt(y));
	}

	public Number remainder(Number x, Number y){
        return toBigInt(x).remainder(toBigInt(y));
	}

	public boolean equiv(Number x, Number y){
		return toBigInt(x).equals(toBigInt(y));
	}

	public boolean lt(Number x, Number y){
        return toBigInt(x).lt(toBigInt(y));
	}

	//public Number subtract(Number x, Number y);
	final public Number negate(Number x){
		return BigInt.fromBigInteger(toBigInteger(x).negate());
	}

	public Number inc(Number x){
		BigInteger bx = toBigInteger(x);
		return BigInt.fromBigInteger(bx.add(BigInteger.ONE));
	}

	public Number dec(Number x){
		BigInteger bx = toBigInteger(x);
		return BigInt.fromBigInteger(bx.subtract(BigInteger.ONE));
	}
}


final static class BigDecimalOps extends OpsP{
	final static Var MATH_CONTEXT = RT.MATH_CONTEXT;

	public Ops combine(Ops y){
		return y.opsWith(this);
	}

	final public Ops opsWith(LongOps x){
		return this;
	}

	final public Ops opsWith(DoubleOps x){
		return DOUBLE_OPS;
	}

	final public Ops opsWith(RatioOps x){
		return this;
	}

	final public Ops opsWith(BigIntOps x){
		return this;
	}

	final public Ops opsWith(BigDecimalOps x){
		return this;
	}

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
		MathContext mc = (MathContext) MATH_CONTEXT.deref();
		return mc == null
		       ? toBigDecimal(x).add(toBigDecimal(y))
		       : toBigDecimal(x).add(toBigDecimal(y), mc);
	}

	final public Number multiply(Number x, Number y){
		MathContext mc = (MathContext) MATH_CONTEXT.deref();
		return mc == null
		       ? toBigDecimal(x).multiply(toBigDecimal(y))
		       : toBigDecimal(x).multiply(toBigDecimal(y), mc);
	}

	public Number divide(Number x, Number y){
		MathContext mc = (MathContext) MATH_CONTEXT.deref();
		return mc == null
		       ? toBigDecimal(x).divide(toBigDecimal(y))
		       : toBigDecimal(x).divide(toBigDecimal(y), mc);
	}

	public Number quotient(Number x, Number y){
		MathContext mc = (MathContext) MATH_CONTEXT.deref();
		return mc == null
		       ? toBigDecimal(x).divideToIntegralValue(toBigDecimal(y))
		       : toBigDecimal(x).divideToIntegralValue(toBigDecimal(y), mc);
	}

	public Number remainder(Number x, Number y){
		MathContext mc = (MathContext) MATH_CONTEXT.deref();
		return mc == null
		       ? toBigDecimal(x).remainder(toBigDecimal(y))
		       : toBigDecimal(x).remainder(toBigDecimal(y), mc);
	}

	public boolean equiv(Number x, Number y){
		return toBigDecimal(x).equals(toBigDecimal(y));
	}

	public boolean lt(Number x, Number y){
		return toBigDecimal(x).compareTo(toBigDecimal(y)) < 0;
	}

	//public Number subtract(Number x, Number y);
	final public Number negate(Number x){
		MathContext mc = (MathContext) MATH_CONTEXT.deref();
		return mc == null
		       ? ((BigDecimal) x).negate()
		       : ((BigDecimal) x).negate(mc);
	}

	public Number inc(Number x){
		MathContext mc = (MathContext) MATH_CONTEXT.deref();
		BigDecimal bx = (BigDecimal) x;
		return mc == null
		       ? bx.add(BigDecimal.ONE)
		       : bx.add(BigDecimal.ONE, mc);
	}

	public Number dec(Number x){
		MathContext mc = (MathContext) MATH_CONTEXT.deref();
		BigDecimal bx = (BigDecimal) x;
		return mc == null
		       ? bx.subtract(BigDecimal.ONE)
		       : bx.subtract(BigDecimal.ONE, mc);
	}
}

static final LongOps LONG_OPS = new LongOps();
static final DoubleOps DOUBLE_OPS = new DoubleOps();
static final RatioOps RATIO_OPS = new RatioOps();
static final BigIntOps BIGINT_OPS = new BigIntOps();
static final BigDecimalOps BIGDECIMAL_OPS = new BigDecimalOps();

static public enum Category {INTEGER, FLOATING, DECIMAL, RATIO};

static Ops ops(Object x){
	Class xc = x.getClass();

	if(xc == Long.class)
		return LONG_OPS;
	else if(xc == Double.class)
		return DOUBLE_OPS;
	else if(xc == Integer.class)
		return LONG_OPS;
	else if(xc == Float.class)
		return DOUBLE_OPS;
	else if(xc == BigInt.class)
		return BIGINT_OPS;
	else if(xc == BigInteger.class)
		return BIGINT_OPS;
	else if(xc == Ratio.class)
		return RATIO_OPS;
	else if(xc == BigDecimal.class)
		return BIGDECIMAL_OPS;
	else
		return LONG_OPS;
}

static int hasheq(Number x){
	Class xc = x.getClass();

	if(xc == Long.class
		|| xc == Integer.class
		|| xc == Short.class
		|| xc == Byte.class)
		{
		long lpart = x.longValue();
		return (int) (lpart ^ (lpart >>> 32));
		}
	return x.hashCode();
}

static Category category(Object x){
	Class xc = x.getClass();

	if(xc == Integer.class)
		return Category.INTEGER;
	else if(xc == Double.class)
		return Category.FLOATING;
	else if(xc == Long.class)
		return Category.INTEGER;
	else if(xc == Float.class)
		return Category.FLOATING;
	else if(xc == BigInt.class)
		return Category.INTEGER;
	else if(xc == Ratio.class)
		return Category.RATIO;
	else if(xc == BigDecimal.class)
		return Category.DECIMAL;
	else
		return Category.INTEGER;
}

static long bitOpsCast(Object x){
	Class xc = x.getClass();

	if(xc == Long.class
	        || xc == Integer.class
	        || xc == Short.class
	        || xc == Byte.class)
		return RT.longCast(x);
	// no bignums, no decimals
	throw new IllegalArgumentException("bit operation not supported for: " + xc);
}

	static public float[] float_array(int size, Object init){
		float[] ret = new float[size];
		if(init instanceof Number)
			{
			float f = ((Number) init).floatValue();
			for(int i = 0; i < ret.length; i++)
				ret[i] = f;
			}
		else
			{
			ISeq s = RT.seq(init);
			for(int i = 0; i < size && s != null; i++, s = s.next())
				ret[i] = ((Number) s.first()).floatValue();
			}
		return ret;
	}

	static public float[] float_array(Object sizeOrSeq){
		if(sizeOrSeq instanceof Number)
			return new float[((Number) sizeOrSeq).intValue()];
		else
			{
			ISeq s = RT.seq(sizeOrSeq);
			int size = RT.count(s);
			float[] ret = new float[size];
			for(int i = 0; i < size && s != null; i++, s = s.next())
				ret[i] = ((Number) s.first()).floatValue();
			return ret;
			}
	}

static public double[] double_array(int size, Object init){
	double[] ret = new double[size];
	if(init instanceof Number)
		{
		double f = ((Number) init).doubleValue();
		for(int i = 0; i < ret.length; i++)
			ret[i] = f;
		}
	else
		{
		ISeq s = RT.seq(init);
		for(int i = 0; i < size && s != null; i++, s = s.next())
			ret[i] = ((Number) s.first()).doubleValue();
		}
	return ret;
}

static public double[] double_array(Object sizeOrSeq){
	if(sizeOrSeq instanceof Number)
		return new double[((Number) sizeOrSeq).intValue()];
	else
		{
		ISeq s = RT.seq(sizeOrSeq);
		int size = RT.count(s);
		double[] ret = new double[size];
		for(int i = 0; i < size && s != null; i++, s = s.next())
			ret[i] = ((Number) s.first()).doubleValue();
		return ret;
		}
}

static public int[] int_array(int size, Object init){
	int[] ret = new int[size];
	if(init instanceof Number)
		{
		int f = ((Number) init).intValue();
		for(int i = 0; i < ret.length; i++)
			ret[i] = f;
		}
	else
		{
		ISeq s = RT.seq(init);
		for(int i = 0; i < size && s != null; i++, s = s.next())
			ret[i] = ((Number) s.first()).intValue();
		}
	return ret;
}

static public int[] int_array(Object sizeOrSeq){
	if(sizeOrSeq instanceof Number)
		return new int[((Number) sizeOrSeq).intValue()];
	else
		{
		ISeq s = RT.seq(sizeOrSeq);
		int size = RT.count(s);
		int[] ret = new int[size];
		for(int i = 0; i < size && s != null; i++, s = s.next())
			ret[i] = ((Number) s.first()).intValue();
		return ret;
		}
}

static public long[] long_array(int size, Object init){
	long[] ret = new long[size];
	if(init instanceof Number)
		{
		long f = ((Number) init).longValue();
		for(int i = 0; i < ret.length; i++)
			ret[i] = f;
		}
	else
		{
		ISeq s = RT.seq(init);
		for(int i = 0; i < size && s != null; i++, s = s.next())
			ret[i] = ((Number) s.first()).longValue();
		}
	return ret;
}

static public long[] long_array(Object sizeOrSeq){
	if(sizeOrSeq instanceof Number)
		return new long[((Number) sizeOrSeq).intValue()];
	else
		{
		ISeq s = RT.seq(sizeOrSeq);
		int size = RT.count(s);
		long[] ret = new long[size];
		for(int i = 0; i < size && s != null; i++, s = s.next())
			ret[i] = ((Number) s.first()).longValue();
		return ret;
		}
}

static public short[] short_array(int size, Object init){
	short[] ret = new short[size];
	if(init instanceof Short)
		{
		short s = (Short) init;
		for(int i = 0; i < ret.length; i++)
			ret[i] = s;
		}
	else
		{
		ISeq s = RT.seq(init);
		for(int i = 0; i < size && s != null; i++, s = s.next())
			ret[i] = (Short) s.first();
		}
	return ret;
}

static public short[] short_array(Object sizeOrSeq){
	if(sizeOrSeq instanceof Number)
		return new short[((Number) sizeOrSeq).intValue()];
	else
		{
		ISeq s = RT.seq(sizeOrSeq);
		int size = RT.count(s);
		short[] ret = new short[size];
		for(int i = 0; i < size && s != null; i++, s = s.next())
			ret[i] = (Short) s.first();
		return ret;
		}
}

static public char[] char_array(int size, Object init){
	char[] ret = new char[size];
	if(init instanceof Character)
		{
		char c = (Character) init;
		for(int i = 0; i < ret.length; i++)
			ret[i] = c;
		}
	else
		{
		ISeq s = RT.seq(init);
		for(int i = 0; i < size && s != null; i++, s = s.next())
			ret[i] = (Character) s.first();
		}
	return ret;
}

static public char[] char_array(Object sizeOrSeq){
	if(sizeOrSeq instanceof Number)
		return new char[((Number) sizeOrSeq).intValue()];
	else
		{
		ISeq s = RT.seq(sizeOrSeq);
		int size = RT.count(s);
		char[] ret = new char[size];
		for(int i = 0; i < size && s != null; i++, s = s.next())
			ret[i] = (Character) s.first();
		return ret;
		}
}

static public byte[] byte_array(int size, Object init){
	byte[] ret = new byte[size];
	if(init instanceof Byte)
		{
		byte b = (Byte) init;
		for(int i = 0; i < ret.length; i++)
			ret[i] = b;
		}
	else
		{
		ISeq s = RT.seq(init);
		for(int i = 0; i < size && s != null; i++, s = s.next())
			ret[i] = (Byte) s.first();
		}
	return ret;
}

static public byte[] byte_array(Object sizeOrSeq){
	if(sizeOrSeq instanceof Number)
		return new byte[((Number) sizeOrSeq).intValue()];
	else
		{
		ISeq s = RT.seq(sizeOrSeq);
		int size = RT.count(s);
		byte[] ret = new byte[size];
		for(int i = 0; i < size && s != null; i++, s = s.next())
			ret[i] = (Byte)s.first();
		return ret;
		}
}

static public boolean[] boolean_array(int size, Object init){
	boolean[] ret = new boolean[size];
	if(init instanceof Boolean)
		{
		boolean b = (Boolean) init;
		for(int i = 0; i < ret.length; i++)
			ret[i] = b;
		}
	else
		{
		ISeq s = RT.seq(init);
		for(int i = 0; i < size && s != null; i++, s = s.next())
			ret[i] = (Boolean)s.first();
		}
	return ret;
}

static public boolean[] boolean_array(Object sizeOrSeq){
	if(sizeOrSeq instanceof Number)
		return new boolean[((Number) sizeOrSeq).intValue()];
	else
		{
		ISeq s = RT.seq(sizeOrSeq);
		int size = RT.count(s);
		boolean[] ret = new boolean[size];
		for(int i = 0; i < size && s != null; i++, s = s.next())
			ret[i] = (Boolean)s.first();
		return ret;
		}
}

static public boolean[] booleans(Object array){
	return (boolean[]) array;
}

static public byte[] bytes(Object array){
	return (byte[]) array;
}

static public char[] chars(Object array){
	return (char[]) array;
}

static public short[] shorts(Object array){
	return (short[]) array;
}

static public float[] floats(Object array){
	return (float[]) array;
}

static public double[] doubles(Object array){
	return (double[]) array;
}

static public int[] ints(Object array){
	return (int[]) array;
}

static public long[] longs(Object array){
	return (long[]) array;
}

static public Number num(Object x){
	return (Number) x;
}

static public Number num(float x){
	return Float.valueOf(x);
}

static public Number num(double x){
	return Double.valueOf(x);
}

static public double add(double x, double y){
	return x + y;
}

static public double addP(double x, double y){
	return x + y;
}

static public double minus(double x, double y){
	return x - y;
}

static public double minusP(double x, double y){
	return x - y;
}

static public double minus(double x){
	return -x;
}

static public double minusP(double x){
	return -x;
}

static public double inc(double x){
	return x + 1;
}

static public double incP(double x){
	return x + 1;
}

static public double dec(double x){
	return x - 1;
}

static public double decP(double x){
	return x - 1;
}

static public double multiply(double x, double y){
	return x * y;
}

static public double multiplyP(double x, double y){
	return x * y;
}

static public double divide(double x, double y){
	return x / y;
}

static public boolean equiv(double x, double y){
	return x == y;
}

static public boolean lt(double x, double y){
	return x < y;
}

static public boolean lte(double x, double y){
	return x <= y;
}

static public boolean gt(double x, double y){
	return x > y;
}

static public boolean gte(double x, double y){
	return x >= y;
}

static public boolean isPos(double x){
	return x > 0;
}

static public boolean isNeg(double x){
	return x < 0;
}

static public boolean isZero(double x){
	return x == 0;
}

static int throwIntOverflow(){
	throw new ArithmeticException("integer overflow");
}

//static public Number num(int x){
//	return Integer.valueOf(x);
//}

static public int unchecked_int_add(int x, int y){
	return x + y;
}

static public int unchecked_int_subtract(int x, int y){
	return x - y;
}

static public int unchecked_int_negate(int x){
	return -x;
}

static public int unchecked_int_inc(int x){
	return x + 1;
}

static public int unchecked_int_dec(int x){
	return x - 1;
}

static public int unchecked_int_multiply(int x, int y){
	return x * y;
}

//static public int add(int x, int y){
//	int ret = x + y;
//	if ((ret ^ x) < 0 && (ret ^ y) < 0)
//		return throwIntOverflow();
//	return ret;
//}

//static public int not(int x){
//	return ~x;
//}

static public long not(Object x){
    return not(bitOpsCast(x));
}
static public long not(long x){
	return ~x;
}
//static public int and(int x, int y){
//	return x & y;
//}

static public long and(Object x, Object y){
    return and(bitOpsCast(x),bitOpsCast(y));
}
static public long and(Object x, long y){
    return and(bitOpsCast(x),y);
}
static public long and(long x, Object y){
    return and(x,bitOpsCast(y));
}
static public long and(long x, long y){
	return x & y;
}

//static public int or(int x, int y){
//	return x | y;
//}

static public long or(Object x, Object y){
    return or(bitOpsCast(x),bitOpsCast(y));
}
static public long or(Object x, long y){
    return or(bitOpsCast(x),y);
}
static public long or(long x, Object y){
    return or(x,bitOpsCast(y));
}
static public long or(long x, long y){
    return x | y;
}

//static public int xor(int x, int y){
//	return x ^ y;
//}

static public long xor(Object x, Object y){
    return xor(bitOpsCast(x),bitOpsCast(y));
}
static public long xor(Object x, long y){
    return xor(bitOpsCast(x),y);
}
static public long xor(long x, Object y){
    return xor(x,bitOpsCast(y));
}
static public long xor(long x, long y){
    return x ^ y;
}

static public long andNot(Object x, Object y){
    return andNot(bitOpsCast(x),bitOpsCast(y));
}
static public long andNot(Object x, long y){
    return andNot(bitOpsCast(x),y);
}
static public long andNot(long x, Object y){
    return andNot(x,bitOpsCast(y));
}
static public long andNot(long x, long y){
    return x & ~y;
}

static public long clearBit(Object x, Object y){
    return clearBit(bitOpsCast(x),bitOpsCast(y));
}
static public long clearBit(Object x, long y){
    return clearBit(bitOpsCast(x),y);
}
static public long clearBit(long x, Object y){
    return clearBit(x,bitOpsCast(y));
}
static public long clearBit(long x, long n){
    return x & ~(1L << n);
}

static public long setBit(Object x, Object y){
    return setBit(bitOpsCast(x),bitOpsCast(y));
}
static public long setBit(Object x, long y){
    return setBit(bitOpsCast(x),y);
}
static public long setBit(long x, Object y){
    return setBit(x,bitOpsCast(y));
}
static public long setBit(long x, long n){
    return x | (1L << n);
}

static public long flipBit(Object x, Object y){
    return flipBit(bitOpsCast(x),bitOpsCast(y));
}
static public long flipBit(Object x, long y){
    return flipBit(bitOpsCast(x),y);
}
static public long flipBit(long x, Object y){
    return flipBit(x,bitOpsCast(y));
}
static public long flipBit(long x, long n){
    return x ^ (1L << n);
}

static public boolean testBit(Object x, Object y){
    return testBit(bitOpsCast(x),bitOpsCast(y));
}
static public boolean testBit(Object x, long y){
    return testBit(bitOpsCast(x),y);
}
static public boolean testBit(long x, Object y){
    return testBit(x,bitOpsCast(y));
}
static public boolean testBit(long x, long n){
    return (x & (1L << n)) != 0;
}

//static public int minus(int x, int y){
//	int ret = x - y;
//	if (((ret ^ x) < 0 && (ret ^ ~y) < 0))
//		return throwIntOverflow();
//	return ret;
//}

//static public int minus(int x){
//	if(x == Integer.MIN_VALUE)
//		return throwIntOverflow();
//	return -x;
//}

//static public int inc(int x){
//	if(x == Integer.MAX_VALUE)
//		return throwIntOverflow();
//	return x + 1;
//}

//static public int dec(int x){
//	if(x == Integer.MIN_VALUE)
//		return throwIntOverflow();
//	return x - 1;
//}

//static public int multiply(int x, int y){
//	int ret = x * y;
//	if (y != 0 && ret/y != x)
//		return throwIntOverflow();
//	return ret;
//}

static public int unchecked_int_divide(int x, int y){
	return x / y;
}

static public int unchecked_int_remainder(int x, int y){
	return x % y;
}

//static public boolean equiv(int x, int y){
//	return x == y;
//}

//static public boolean lt(int x, int y){
//	return x < y;
//}

//static public boolean lte(int x, int y){
//	return x <= y;
//}

//static public boolean gt(int x, int y){
//	return x > y;
//}

//static public boolean gte(int x, int y){
//	return x >= y;
//}

//static public boolean isPos(int x){
//	return x > 0;
//}

//static public boolean isNeg(int x){
//	return x < 0;
//}

//static public boolean isZero(int x){
//	return x == 0;
//}

static public Number num(long x){
	return Long.valueOf(x);
}

static public long unchecked_add(long x, long y){return x + y;}
static public long unchecked_minus(long x, long y){return x - y;}
static public long unchecked_multiply(long x, long y){return x * y;}
static public long unchecked_minus(long x){return -x;}
static public long unchecked_inc(long x){return x + 1;}
static public long unchecked_dec(long x){return x - 1;}

static public Number unchecked_add(Object x, Object y){return add(x,y);}
static public Number unchecked_minus(Object x, Object y){return minus(x,y);}
static public Number unchecked_multiply(Object x, Object y){return multiply(x,y);}
static public Number unchecked_minus(Object x){return minus(x);}
static public Number unchecked_inc(Object x){return inc(x);}
static public Number unchecked_dec(Object x){return dec(x);}

static public double unchecked_add(double x, double y){return add(x,y);}
static public double unchecked_minus(double x, double y){return minus(x,y);}
static public double unchecked_multiply(double x, double y){return multiply(x,y);}
static public double unchecked_minus(double x){return minus(x);}
static public double unchecked_inc(double x){return inc(x);}
static public double unchecked_dec(double x){return dec(x);}

static public double unchecked_add(double x, Object y){return add(x,y);}
static public double unchecked_minus(double x, Object y){return minus(x,y);}
static public double unchecked_multiply(double x, Object y){return multiply(x,y);}
static public double unchecked_add(Object x, double y){return add(x,y);}
static public double unchecked_minus(Object x, double y){return minus(x,y);}
static public double unchecked_multiply(Object x, double y){return multiply(x,y);}

static public double unchecked_add(double x, long y){return add(x,y);}
static public double unchecked_minus(double x, long y){return minus(x,y);}
static public double unchecked_multiply(double x, long y){return multiply(x,y);}
static public double unchecked_add(long x, double y){return add(x,y);}
static public double unchecked_minus(long x, double y){return minus(x,y);}
static public double unchecked_multiply(long x, double y){return multiply(x,y);}

static public Number unchecked_add(long x, Object y){return add(x,y);}
static public Number unchecked_minus(long x, Object y){return minus(x,y);}
static public Number unchecked_multiply(long x, Object y){return multiply(x,y);}
static public Number unchecked_add(Object x, long y){return add(x,y);}
static public Number unchecked_minus(Object x, long y){return minus(x,y);}
static public Number unchecked_multiply(Object x, long y){return multiply(x,y);}

static public Number quotient(double x, Object y){return quotient((Object)x,y);}
static public Number quotient(Object x, double y){return quotient(x,(Object)y);}
static public Number quotient(long x, Object y){return quotient((Object)x,y);}
static public Number quotient(Object x, long y){return quotient(x,(Object)y);}
static public double quotient(double x, long y){return quotient(x,(double)y);}
static public double quotient(long x, double y){return quotient((double)x,y);}

static public Number remainder(double x, Object y){return remainder((Object)x,y);}
static public Number remainder(Object x, double y){return remainder(x,(Object)y);}
static public Number remainder(long x, Object y){return remainder((Object)x,y);}
static public Number remainder(Object x, long y){return remainder(x,(Object)y);}
static public double remainder(double x, long y){return remainder(x,(double)y);}
static public double remainder(long x, double y){return remainder((double)x,y);}

static public long add(long x, long y){
	long ret = x + y;
	if ((ret ^ x) < 0 && (ret ^ y) < 0)
		return throwIntOverflow();
	return ret;
}

static public Number addP(long x, long y){
	long ret = x + y;
	if ((ret ^ x) < 0 && (ret ^ y) < 0)
		return addP((Number)x,(Number)y);
	return num(ret);
}

static public long minus(long x, long y){
	long ret = x - y;
	if (((ret ^ x) < 0 && (ret ^ ~y) < 0))
		return throwIntOverflow();
	return ret;
}

static public Number minusP(long x, long y){
	long ret = x - y;
	if (((ret ^ x) < 0 && (ret ^ ~y) < 0))
		return minusP((Number)x,(Number)y);
	return num(ret);
}

static public long minus(long x){
	if(x == Long.MIN_VALUE)
		return throwIntOverflow();
	return -x;
}

static public Number minusP(long x){
	if(x == Long.MIN_VALUE)
		return BigInt.fromBigInteger(BigInteger.valueOf(x).negate());
	return num(-x);
}

static public long inc(long x){
	if(x == Long.MAX_VALUE)
		return throwIntOverflow();
	return x + 1;
}

static public Number incP(long x){
	if(x == Long.MAX_VALUE)
		return BIGINT_OPS.inc(x);
	return num(x + 1);
}

static public long dec(long x){
	if(x == Long.MIN_VALUE)
		return throwIntOverflow();
	return x - 1;
}

static public Number decP(long x){
	if(x == Long.MIN_VALUE)
		return BIGINT_OPS.dec(x);
	return num(x - 1);
}


static public long multiply(long x, long y){
	long ret = x * y;
	if (y != 0 && ret/y != x)
		return throwIntOverflow();
	return ret;
}

static public Number multiplyP(long x, long y){
	long ret = x * y;
	if (y != 0 && ret/y != x)
		return multiplyP((Number)x,(Number)y);
	return num(ret);
}

static public long quotient(long x, long y){
	return x / y;
}

static public long remainder(long x, long y){
	return x % y;
}

static public boolean equiv(long x, long y){
	return x == y;
}

static public boolean lt(long x, long y){
	return x < y;
}

static public boolean lte(long x, long y){
	return x <= y;
}

static public boolean gt(long x, long y){
	return x > y;
}

static public boolean gte(long x, long y){
	return x >= y;
}

static public boolean isPos(long x){
	return x > 0;
}

static public boolean isNeg(long x){
	return x < 0;
}

static public boolean isZero(long x){
	return x == 0;
}

/*
static public class F{
	static public float add(float x, float y){
		return x + y;
	}

	static public float subtract(float x, float y){
		return x - y;
	}

	static public float negate(float x){
		return -x;
	}

	static public float inc(float x){
		return x + 1;
	}

	static public float dec(float x){
		return x - 1;
	}

	static public float multiply(float x, float y){
		return x * y;
	}

	static public float divide(float x, float y){
		return x / y;
	}

	static public boolean equiv(float x, float y){
		return x == y;
	}

	static public boolean lt(float x, float y){
		return x < y;
	}

	static public boolean lte(float x, float y){
		return x <= y;
	}

	static public boolean gt(float x, float y){
		return x > y;
	}

	static public boolean gte(float x, float y){
		return x >= y;
	}

	static public boolean pos(float x){
		return x > 0;
	}

	static public boolean neg(float x){
		return x < 0;
	}

	static public boolean zero(float x){
		return x == 0;
	}

	static public float aget(float[] xs, int i){
		return xs[i];
	}

	static public float aset(float[] xs, int i, float v){
		xs[i] = v;
		return v;
	}

	static public int alength(float[] xs){
		return xs.length;
	}

	static public float[] aclone(float[] xs){
		return xs.clone();
	}

	static public float[] vec(int size, Object init){
		float[] ret = new float[size];
		if(init instanceof Number)
			{
			float f = ((Number) init).floatValue();
			for(int i = 0; i < ret.length; i++)
				ret[i] = f;
			}
		else
			{
			ISeq s = RT.seq(init);
			for(int i = 0; i < size && s != null; i++, s = s.rest())
				ret[i] = ((Number) s.first()).floatValue();
			}
		return ret;
	}

	static public float[] vec(Object sizeOrSeq){
		if(sizeOrSeq instanceof Number)
			return new float[((Number) sizeOrSeq).intValue()];
		else
			{
			ISeq s = RT.seq(sizeOrSeq);
			int size = s.count();
			float[] ret = new float[size];
			for(int i = 0; i < size && s != null; i++, s = s.rest())
				ret[i] = ((Number) s.first()).intValue();
			return ret;
			}
	}


	static public float[] vsadd(float[] x, float y){
		final float[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] += y;
		return xs;
	}

	static public float[] vssub(float[] x, float y){
		final float[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] -= y;
		return xs;
	}

	static public float[] vsdiv(float[] x, float y){
		final float[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] /= y;
		return xs;
	}

	static public float[] vsmul(float[] x, float y){
		final float[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] *= y;
		return xs;
	}

	static public float[] svdiv(float y, float[] x){
		final float[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = y / xs[i];
		return xs;
	}

	static public float[] vsmuladd(float[] x, float y, float[] zs){
		final float[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = xs[i] * y + zs[i];
		return xs;
	}

	static public float[] vsmulsub(float[] x, float y, float[] zs){
		final float[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = xs[i] * y - zs[i];
		return xs;
	}

	static public float[] vsmulsadd(float[] x, float y, float z){
		final float[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = xs[i] * y + z;
		return xs;
	}

	static public float[] vsmulssub(float[] x, float y, float z){
		final float[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = xs[i] * y - z;
		return xs;
	}

	static public float[] vabs(float[] x){
		final float[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = Math.abs(xs[i]);
		return xs;
	}

	static public float[] vnegabs(float[] x){
		final float[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = -Math.abs(xs[i]);
		return xs;
	}

	static public float[] vneg(float[] x){
		final float[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = -xs[i];
		return xs;
	}

	static public float[] vsqr(float[] x){
		final float[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] *= xs[i];
		return xs;
	}

	static public float[] vsignedsqr(float[] x){
		final float[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] *= Math.abs(xs[i]);
		return xs;
	}

	static public float[] vclip(float[] x, float low, float high){
		final float[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			{
			if(xs[i] < low)
				xs[i] = low;
			else if(xs[i] > high)
				xs[i] = high;
			}
		return xs;
	}

	static public IPersistentVector vclipcounts(float[] x, float low, float high){
		final float[] xs = x.clone();
		int lowc = 0;
		int highc = 0;

		for(int i = 0; i < xs.length; i++)
			{
			if(xs[i] < low)
				{
				++lowc;
				xs[i] = low;
				}
			else if(xs[i] > high)
				{
				++highc;
				xs[i] = high;
				}
			}
		return RT.vector(xs, lowc, highc);
	}

	static public float[] vthresh(float[] x, float thresh, float otherwise){
		final float[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			{
			if(xs[i] < thresh)
				xs[i] = otherwise;
			}
		return xs;
	}

	static public float[] vreverse(float[] x){
		final float[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = xs[xs.length - i - 1];
		return xs;
	}

	static public float[] vrunningsum(float[] x){
		final float[] xs = x.clone();
		for(int i = 1; i < xs.length; i++)
			xs[i] = xs[i - 1] + xs[i];
		return xs;
	}

	static public float[] vsort(float[] x){
		final float[] xs = x.clone();
		Arrays.sort(xs);
		return xs;
	}

	static public float vdot(float[] xs, float[] ys){
		float ret = 0;
		for(int i = 0; i < xs.length; i++)
			ret += xs[i] * ys[i];
		return ret;
	}

	static public float vmax(float[] xs){
		if(xs.length == 0)
			return 0;
		float ret = xs[0];
		for(int i = 0; i < xs.length; i++)
			ret = Math.max(ret, xs[i]);
		return ret;
	}

	static public float vmin(float[] xs){
		if(xs.length == 0)
			return 0;
		float ret = xs[0];
		for(int i = 0; i < xs.length; i++)
			ret = Math.min(ret, xs[i]);
		return ret;
	}

	static public float vmean(float[] xs){
		if(xs.length == 0)
			return 0;
		return vsum(xs) / xs.length;
	}

	static public double vrms(float[] xs){
		if(xs.length == 0)
			return 0;
		float ret = 0;
		for(int i = 0; i < xs.length; i++)
			ret += xs[i] * xs[i];
		return Math.sqrt(ret / xs.length);
	}

	static public float vsum(float[] xs){
		float ret = 0;
		for(int i = 0; i < xs.length; i++)
			ret += xs[i];
		return ret;
	}

	static public boolean vequiv(float[] xs, float[] ys){
		return Arrays.equals(xs, ys);
	}

	static public float[] vadd(float[] x, float[] ys){
		final float[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] += ys[i];
		return xs;
	}

	static public float[] vsub(float[] x, float[] ys){
		final float[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] -= ys[i];
		return xs;
	}

	static public float[] vaddmul(float[] x, float[] ys, float[] zs){
		final float[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = (xs[i] + ys[i]) * zs[i];
		return xs;
	}

	static public float[] vsubmul(float[] x, float[] ys, float[] zs){
		final float[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = (xs[i] - ys[i]) * zs[i];
		return xs;
	}

	static public float[] vaddsmul(float[] x, float[] ys, float z){
		final float[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = (xs[i] + ys[i]) * z;
		return xs;
	}

	static public float[] vsubsmul(float[] x, float[] ys, float z){
		final float[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = (xs[i] - ys[i]) * z;
		return xs;
	}

	static public float[] vmulsadd(float[] x, float[] ys, float z){
		final float[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = (xs[i] * ys[i]) + z;
		return xs;
	}

	static public float[] vdiv(float[] x, float[] ys){
		final float[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] /= ys[i];
		return xs;
	}

	static public float[] vmul(float[] x, float[] ys){
		final float[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] *= ys[i];
		return xs;
	}

	static public float[] vmuladd(float[] x, float[] ys, float[] zs){
		final float[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = (xs[i] * ys[i]) + zs[i];
		return xs;
	}

	static public float[] vmulsub(float[] x, float[] ys, float[] zs){
		final float[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = (xs[i] * ys[i]) - zs[i];
		return xs;
	}

	static public float[] vmax(float[] x, float[] ys){
		final float[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = Math.max(xs[i], ys[i]);
		return xs;
	}

	static public float[] vmin(float[] x, float[] ys){
		final float[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = Math.min(xs[i], ys[i]);
		return xs;
	}

	static public float[] vmap(IFn fn, float[] x) {
		float[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = ((Number) fn.invoke(xs[i])).floatValue();
		return xs;
	}

	static public float[] vmap(IFn fn, float[] x, float[] ys) {
		float[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = ((Number) fn.invoke(xs[i], ys[i])).floatValue();
		return xs;
	}

}

static public class D{
	static public double add(double x, double y){
		return x + y;
	}

	static public double subtract(double x, double y){
		return x - y;
	}

	static public double negate(double x){
		return -x;
	}

	static public double inc(double x){
		return x + 1;
	}

	static public double dec(double x){
		return x - 1;
	}

	static public double multiply(double x, double y){
		return x * y;
	}

	static public double divide(double x, double y){
		return x / y;
	}

	static public boolean equiv(double x, double y){
		return x == y;
	}

	static public boolean lt(double x, double y){
		return x < y;
	}

	static public boolean lte(double x, double y){
		return x <= y;
	}

	static public boolean gt(double x, double y){
		return x > y;
	}

	static public boolean gte(double x, double y){
		return x >= y;
	}

	static public boolean pos(double x){
		return x > 0;
	}

	static public boolean neg(double x){
		return x < 0;
	}

	static public boolean zero(double x){
		return x == 0;
	}

	static public double aget(double[] xs, int i){
		return xs[i];
	}

	static public double aset(double[] xs, int i, double v){
		xs[i] = v;
		return v;
	}

	static public int alength(double[] xs){
		return xs.length;
	}

	static public double[] aclone(double[] xs){
		return xs.clone();
	}

	static public double[] vec(int size, Object init){
		double[] ret = new double[size];
		if(init instanceof Number)
			{
			double f = ((Number) init).doubleValue();
			for(int i = 0; i < ret.length; i++)
				ret[i] = f;
			}
		else
			{
			ISeq s = RT.seq(init);
			for(int i = 0; i < size && s != null; i++, s = s.rest())
				ret[i] = ((Number) s.first()).doubleValue();
			}
		return ret;
	}

	static public double[] vec(Object sizeOrSeq){
		if(sizeOrSeq instanceof Number)
			return new double[((Number) sizeOrSeq).intValue()];
		else
			{
			ISeq s = RT.seq(sizeOrSeq);
			int size = s.count();
			double[] ret = new double[size];
			for(int i = 0; i < size && s != null; i++, s = s.rest())
				ret[i] = ((Number) s.first()).intValue();
			return ret;
			}
	}

	static public double[] vsadd(double[] x, double y){
		final double[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] += y;
		return xs;
	}

	static public double[] vssub(double[] x, double y){
		final double[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] -= y;
		return xs;
	}

	static public double[] vsdiv(double[] x, double y){
		final double[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] /= y;
		return xs;
	}

	static public double[] vsmul(double[] x, double y){
		final double[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] *= y;
		return xs;
	}

	static public double[] svdiv(double y, double[] x){
		final double[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = y / xs[i];
		return xs;
	}

	static public double[] vsmuladd(double[] x, double y, double[] zs){
		final double[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = xs[i] * y + zs[i];
		return xs;
	}

	static public double[] vsmulsub(double[] x, double y, double[] zs){
		final double[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = xs[i] * y - zs[i];
		return xs;
	}

	static public double[] vsmulsadd(double[] x, double y, double z){
		final double[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = xs[i] * y + z;
		return xs;
	}

	static public double[] vsmulssub(double[] x, double y, double z){
		final double[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = xs[i] * y - z;
		return xs;
	}

	static public double[] vabs(double[] x){
		final double[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = Math.abs(xs[i]);
		return xs;
	}

	static public double[] vnegabs(double[] x){
		final double[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = -Math.abs(xs[i]);
		return xs;
	}

	static public double[] vneg(double[] x){
		final double[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = -xs[i];
		return xs;
	}

	static public double[] vsqr(double[] x){
		final double[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] *= xs[i];
		return xs;
	}

	static public double[] vsignedsqr(double[] x){
		final double[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] *= Math.abs(xs[i]);
		return xs;
	}

	static public double[] vclip(double[] x, double low, double high){
		final double[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			{
			if(xs[i] < low)
				xs[i] = low;
			else if(xs[i] > high)
				xs[i] = high;
			}
		return xs;
	}

	static public IPersistentVector vclipcounts(double[] x, double low, double high){
		final double[] xs = x.clone();
		int lowc = 0;
		int highc = 0;

		for(int i = 0; i < xs.length; i++)
			{
			if(xs[i] < low)
				{
				++lowc;
				xs[i] = low;
				}
			else if(xs[i] > high)
				{
				++highc;
				xs[i] = high;
				}
			}
		return RT.vector(xs, lowc, highc);
	}

	static public double[] vthresh(double[] x, double thresh, double otherwise){
		final double[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			{
			if(xs[i] < thresh)
				xs[i] = otherwise;
			}
		return xs;
	}

	static public double[] vreverse(double[] x){
		final double[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = xs[xs.length - i - 1];
		return xs;
	}

	static public double[] vrunningsum(double[] x){
		final double[] xs = x.clone();
		for(int i = 1; i < xs.length; i++)
			xs[i] = xs[i - 1] + xs[i];
		return xs;
	}

	static public double[] vsort(double[] x){
		final double[] xs = x.clone();
		Arrays.sort(xs);
		return xs;
	}

	static public double vdot(double[] xs, double[] ys){
		double ret = 0;
		for(int i = 0; i < xs.length; i++)
			ret += xs[i] * ys[i];
		return ret;
	}

	static public double vmax(double[] xs){
		if(xs.length == 0)
			return 0;
		double ret = xs[0];
		for(int i = 0; i < xs.length; i++)
			ret = Math.max(ret, xs[i]);
		return ret;
	}

	static public double vmin(double[] xs){
		if(xs.length == 0)
			return 0;
		double ret = xs[0];
		for(int i = 0; i < xs.length; i++)
			ret = Math.min(ret, xs[i]);
		return ret;
	}

	static public double vmean(double[] xs){
		if(xs.length == 0)
			return 0;
		return vsum(xs) / xs.length;
	}

	static public double vrms(double[] xs){
		if(xs.length == 0)
			return 0;
		double ret = 0;
		for(int i = 0; i < xs.length; i++)
			ret += xs[i] * xs[i];
		return Math.sqrt(ret / xs.length);
	}

	static public double vsum(double[] xs){
		double ret = 0;
		for(int i = 0; i < xs.length; i++)
			ret += xs[i];
		return ret;
	}

	static public boolean vequiv(double[] xs, double[] ys){
		return Arrays.equals(xs, ys);
	}

	static public double[] vadd(double[] x, double[] ys){
		final double[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] += ys[i];
		return xs;
	}

	static public double[] vsub(double[] x, double[] ys){
		final double[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] -= ys[i];
		return xs;
	}

	static public double[] vaddmul(double[] x, double[] ys, double[] zs){
		final double[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = (xs[i] + ys[i]) * zs[i];
		return xs;
	}

	static public double[] vsubmul(double[] x, double[] ys, double[] zs){
		final double[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = (xs[i] - ys[i]) * zs[i];
		return xs;
	}

	static public double[] vaddsmul(double[] x, double[] ys, double z){
		final double[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = (xs[i] + ys[i]) * z;
		return xs;
	}

	static public double[] vsubsmul(double[] x, double[] ys, double z){
		final double[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = (xs[i] - ys[i]) * z;
		return xs;
	}

	static public double[] vmulsadd(double[] x, double[] ys, double z){
		final double[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = (xs[i] * ys[i]) + z;
		return xs;
	}

	static public double[] vdiv(double[] x, double[] ys){
		final double[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] /= ys[i];
		return xs;
	}

	static public double[] vmul(double[] x, double[] ys){
		final double[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] *= ys[i];
		return xs;
	}

	static public double[] vmuladd(double[] x, double[] ys, double[] zs){
		final double[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = (xs[i] * ys[i]) + zs[i];
		return xs;
	}

	static public double[] vmulsub(double[] x, double[] ys, double[] zs){
		final double[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = (xs[i] * ys[i]) - zs[i];
		return xs;
	}

	static public double[] vmax(double[] x, double[] ys){
		final double[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = Math.max(xs[i], ys[i]);
		return xs;
	}

	static public double[] vmin(double[] x, double[] ys){
		final double[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = Math.min(xs[i], ys[i]);
		return xs;
	}

	static public double[] vmap(IFn fn, double[] x) {
		double[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = ((Number) fn.invoke(xs[i])).doubleValue();
		return xs;
	}

	static public double[] vmap(IFn fn, double[] x, double[] ys) {
		double[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = ((Number) fn.invoke(xs[i], ys[i])).doubleValue();
		return xs;
	}
}

static public class I{
	static public int add(int x, int y){
		return x + y;
	}

	static public int subtract(int x, int y){
		return x - y;
	}

	static public int negate(int x){
		return -x;
	}

	static public int inc(int x){
		return x + 1;
	}

	static public int dec(int x){
		return x - 1;
	}

	static public int multiply(int x, int y){
		return x * y;
	}

	static public int divide(int x, int y){
		return x / y;
	}

	static public boolean equiv(int x, int y){
		return x == y;
	}

	static public boolean lt(int x, int y){
		return x < y;
	}

	static public boolean lte(int x, int y){
		return x <= y;
	}

	static public boolean gt(int x, int y){
		return x > y;
	}

	static public boolean gte(int x, int y){
		return x >= y;
	}

	static public boolean pos(int x){
		return x > 0;
	}

	static public boolean neg(int x){
		return x < 0;
	}

	static public boolean zero(int x){
		return x == 0;
	}

	static public int aget(int[] xs, int i){
		return xs[i];
	}

	static public int aset(int[] xs, int i, int v){
		xs[i] = v;
		return v;
	}

	static public int alength(int[] xs){
		return xs.length;
	}

	static public int[] aclone(int[] xs){
		return xs.clone();
	}

	static public int[] vec(int size, Object init){
		int[] ret = new int[size];
		if(init instanceof Number)
			{
			int f = ((Number) init).intValue();
			for(int i = 0; i < ret.length; i++)
				ret[i] = f;
			}
		else
			{
			ISeq s = RT.seq(init);
			for(int i = 0; i < size && s != null; i++, s = s.rest())
				ret[i] = ((Number) s.first()).intValue();
			}
		return ret;
	}

	static public int[] vec(Object sizeOrSeq){
		if(sizeOrSeq instanceof Number)
			return new int[((Number) sizeOrSeq).intValue()];
		else
			{
			ISeq s = RT.seq(sizeOrSeq);
			int size = s.count();
			int[] ret = new int[size];
			for(int i = 0; i < size && s != null; i++, s = s.rest())
				ret[i] = ((Number) s.first()).intValue();
			return ret;
			}
	}

	static public int[] vsadd(int[] x, int y){
		final int[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] += y;
		return xs;
	}

	static public int[] vssub(int[] x, int y){
		final int[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] -= y;
		return xs;
	}

	static public int[] vsdiv(int[] x, int y){
		final int[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] /= y;
		return xs;
	}

	static public int[] vsmul(int[] x, int y){
		final int[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] *= y;
		return xs;
	}

	static public int[] svdiv(int y, int[] x){
		final int[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = y / xs[i];
		return xs;
	}

	static public int[] vsmuladd(int[] x, int y, int[] zs){
		final int[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = xs[i] * y + zs[i];
		return xs;
	}

	static public int[] vsmulsub(int[] x, int y, int[] zs){
		final int[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = xs[i] * y - zs[i];
		return xs;
	}

	static public int[] vsmulsadd(int[] x, int y, int z){
		final int[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = xs[i] * y + z;
		return xs;
	}

	static public int[] vsmulssub(int[] x, int y, int z){
		final int[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = xs[i] * y - z;
		return xs;
	}

	static public int[] vabs(int[] x){
		final int[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = Math.abs(xs[i]);
		return xs;
	}

	static public int[] vnegabs(int[] x){
		final int[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = -Math.abs(xs[i]);
		return xs;
	}

	static public int[] vneg(int[] x){
		final int[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = -xs[i];
		return xs;
	}

	static public int[] vsqr(int[] x){
		final int[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] *= xs[i];
		return xs;
	}

	static public int[] vsignedsqr(int[] x){
		final int[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] *= Math.abs(xs[i]);
		return xs;
	}

	static public int[] vclip(int[] x, int low, int high){
		final int[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			{
			if(xs[i] < low)
				xs[i] = low;
			else if(xs[i] > high)
				xs[i] = high;
			}
		return xs;
	}

	static public IPersistentVector vclipcounts(int[] x, int low, int high){
		final int[] xs = x.clone();
		int lowc = 0;
		int highc = 0;

		for(int i = 0; i < xs.length; i++)
			{
			if(xs[i] < low)
				{
				++lowc;
				xs[i] = low;
				}
			else if(xs[i] > high)
				{
				++highc;
				xs[i] = high;
				}
			}
		return RT.vector(xs, lowc, highc);
	}

	static public int[] vthresh(int[] x, int thresh, int otherwise){
		final int[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			{
			if(xs[i] < thresh)
				xs[i] = otherwise;
			}
		return xs;
	}

	static public int[] vreverse(int[] x){
		final int[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = xs[xs.length - i - 1];
		return xs;
	}

	static public int[] vrunningsum(int[] x){
		final int[] xs = x.clone();
		for(int i = 1; i < xs.length; i++)
			xs[i] = xs[i - 1] + xs[i];
		return xs;
	}

	static public int[] vsort(int[] x){
		final int[] xs = x.clone();
		Arrays.sort(xs);
		return xs;
	}

	static public int vdot(int[] xs, int[] ys){
		int ret = 0;
		for(int i = 0; i < xs.length; i++)
			ret += xs[i] * ys[i];
		return ret;
	}

	static public int vmax(int[] xs){
		if(xs.length == 0)
			return 0;
		int ret = xs[0];
		for(int i = 0; i < xs.length; i++)
			ret = Math.max(ret, xs[i]);
		return ret;
	}

	static public int vmin(int[] xs){
		if(xs.length == 0)
			return 0;
		int ret = xs[0];
		for(int i = 0; i < xs.length; i++)
			ret = Math.min(ret, xs[i]);
		return ret;
	}

	static public double vmean(int[] xs){
		if(xs.length == 0)
			return 0;
		return vsum(xs) / (double) xs.length;
	}

	static public double vrms(int[] xs){
		if(xs.length == 0)
			return 0;
		int ret = 0;
		for(int i = 0; i < xs.length; i++)
			ret += xs[i] * xs[i];
		return Math.sqrt(ret / (double) xs.length);
	}

	static public int vsum(int[] xs){
		int ret = 0;
		for(int i = 0; i < xs.length; i++)
			ret += xs[i];
		return ret;
	}

	static public boolean vequiv(int[] xs, int[] ys){
		return Arrays.equals(xs, ys);
	}

	static public int[] vadd(int[] x, int[] ys){
		final int[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] += ys[i];
		return xs;
	}

	static public int[] vsub(int[] x, int[] ys){
		final int[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] -= ys[i];
		return xs;
	}

	static public int[] vaddmul(int[] x, int[] ys, int[] zs){
		final int[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = (xs[i] + ys[i]) * zs[i];
		return xs;
	}

	static public int[] vsubmul(int[] x, int[] ys, int[] zs){
		final int[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = (xs[i] - ys[i]) * zs[i];
		return xs;
	}

	static public int[] vaddsmul(int[] x, int[] ys, int z){
		final int[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = (xs[i] + ys[i]) * z;
		return xs;
	}

	static public int[] vsubsmul(int[] x, int[] ys, int z){
		final int[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = (xs[i] - ys[i]) * z;
		return xs;
	}

	static public int[] vmulsadd(int[] x, int[] ys, int z){
		final int[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = (xs[i] * ys[i]) + z;
		return xs;
	}

	static public int[] vdiv(int[] x, int[] ys){
		final int[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] /= ys[i];
		return xs;
	}

	static public int[] vmul(int[] x, int[] ys){
		final int[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] *= ys[i];
		return xs;
	}

	static public int[] vmuladd(int[] x, int[] ys, int[] zs){
		final int[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = (xs[i] * ys[i]) + zs[i];
		return xs;
	}

	static public int[] vmulsub(int[] x, int[] ys, int[] zs){
		final int[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = (xs[i] * ys[i]) - zs[i];
		return xs;
	}

	static public int[] vmax(int[] x, int[] ys){
		final int[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = Math.max(xs[i], ys[i]);
		return xs;
	}

	static public int[] vmin(int[] x, int[] ys){
		final int[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = Math.min(xs[i], ys[i]);
		return xs;
	}

	static public int[] vmap(IFn fn, int[] x) {
		int[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = ((Number) fn.invoke(xs[i])).intValue();
		return xs;
	}

	static public int[] vmap(IFn fn, int[] x, int[] ys) {
		int[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = ((Number) fn.invoke(xs[i], ys[i])).intValue();
		return xs;
	}

}

static public class L{
	static public long add(long x, long y){
		return x + y;
	}

	static public long subtract(long x, long y){
		return x - y;
	}

	static public long negate(long x){
		return -x;
	}

	static public long inc(long x){
		return x + 1;
	}

	static public long dec(long x){
		return x - 1;
	}

	static public long multiply(long x, long y){
		return x * y;
	}

	static public long divide(long x, long y){
		return x / y;
	}

	static public boolean equiv(long x, long y){
		return x == y;
	}

	static public boolean lt(long x, long y){
		return x < y;
	}

	static public boolean lte(long x, long y){
		return x <= y;
	}

	static public boolean gt(long x, long y){
		return x > y;
	}

	static public boolean gte(long x, long y){
		return x >= y;
	}

	static public boolean pos(long x){
		return x > 0;
	}

	static public boolean neg(long x){
		return x < 0;
	}

	static public boolean zero(long x){
		return x == 0;
	}

	static public long aget(long[] xs, int i){
		return xs[i];
	}

	static public long aset(long[] xs, int i, long v){
		xs[i] = v;
		return v;
	}

	static public int alength(long[] xs){
		return xs.length;
	}

	static public long[] aclone(long[] xs){
		return xs.clone();
	}

	static public long[] vec(int size, Object init){
		long[] ret = new long[size];
		if(init instanceof Number)
			{
			long f = ((Number) init).longValue();
			for(int i = 0; i < ret.length; i++)
				ret[i] = f;
			}
		else
			{
			ISeq s = RT.seq(init);
			for(int i = 0; i < size && s != null; i++, s = s.rest())
				ret[i] = ((Number) s.first()).longValue();
			}
		return ret;
	}

	static public long[] vec(Object sizeOrSeq){
		if(sizeOrSeq instanceof Number)
			return new long[((Number) sizeOrSeq).intValue()];
		else
			{
			ISeq s = RT.seq(sizeOrSeq);
			int size = s.count();
			long[] ret = new long[size];
			for(int i = 0; i < size && s != null; i++, s = s.rest())
				ret[i] = ((Number) s.first()).intValue();
			return ret;
			}
	}


	static public long[] vsadd(long[] x, long y){
		final long[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] += y;
		return xs;
	}

	static public long[] vssub(long[] x, long y){
		final long[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] -= y;
		return xs;
	}

	static public long[] vsdiv(long[] x, long y){
		final long[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] /= y;
		return xs;
	}

	static public long[] vsmul(long[] x, long y){
		final long[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] *= y;
		return xs;
	}

	static public long[] svdiv(long y, long[] x){
		final long[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = y / xs[i];
		return xs;
	}

	static public long[] vsmuladd(long[] x, long y, long[] zs){
		final long[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = xs[i] * y + zs[i];
		return xs;
	}

	static public long[] vsmulsub(long[] x, long y, long[] zs){
		final long[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = xs[i] * y - zs[i];
		return xs;
	}

	static public long[] vsmulsadd(long[] x, long y, long z){
		final long[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = xs[i] * y + z;
		return xs;
	}

	static public long[] vsmulssub(long[] x, long y, long z){
		final long[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = xs[i] * y - z;
		return xs;
	}

	static public long[] vabs(long[] x){
		final long[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = Math.abs(xs[i]);
		return xs;
	}

	static public long[] vnegabs(long[] x){
		final long[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = -Math.abs(xs[i]);
		return xs;
	}

	static public long[] vneg(long[] x){
		final long[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = -xs[i];
		return xs;
	}

	static public long[] vsqr(long[] x){
		final long[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] *= xs[i];
		return xs;
	}

	static public long[] vsignedsqr(long[] x){
		final long[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] *= Math.abs(xs[i]);
		return xs;
	}

	static public long[] vclip(long[] x, long low, long high){
		final long[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			{
			if(xs[i] < low)
				xs[i] = low;
			else if(xs[i] > high)
				xs[i] = high;
			}
		return xs;
	}

	static public IPersistentVector vclipcounts(long[] x, long low, long high){
		final long[] xs = x.clone();
		int lowc = 0;
		int highc = 0;

		for(int i = 0; i < xs.length; i++)
			{
			if(xs[i] < low)
				{
				++lowc;
				xs[i] = low;
				}
			else if(xs[i] > high)
				{
				++highc;
				xs[i] = high;
				}
			}
		return RT.vector(xs, lowc, highc);
	}

	static public long[] vthresh(long[] x, long thresh, long otherwise){
		final long[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			{
			if(xs[i] < thresh)
				xs[i] = otherwise;
			}
		return xs;
	}

	static public long[] vreverse(long[] x){
		final long[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = xs[xs.length - i - 1];
		return xs;
	}

	static public long[] vrunningsum(long[] x){
		final long[] xs = x.clone();
		for(int i = 1; i < xs.length; i++)
			xs[i] = xs[i - 1] + xs[i];
		return xs;
	}

	static public long[] vsort(long[] x){
		final long[] xs = x.clone();
		Arrays.sort(xs);
		return xs;
	}

	static public long vdot(long[] xs, long[] ys){
		long ret = 0;
		for(int i = 0; i < xs.length; i++)
			ret += xs[i] * ys[i];
		return ret;
	}

	static public long vmax(long[] xs){
		if(xs.length == 0)
			return 0;
		long ret = xs[0];
		for(int i = 0; i < xs.length; i++)
			ret = Math.max(ret, xs[i]);
		return ret;
	}

	static public long vmin(long[] xs){
		if(xs.length == 0)
			return 0;
		long ret = xs[0];
		for(int i = 0; i < xs.length; i++)
			ret = Math.min(ret, xs[i]);
		return ret;
	}

	static public double vmean(long[] xs){
		if(xs.length == 0)
			return 0;
		return vsum(xs) / (double) xs.length;
	}

	static public double vrms(long[] xs){
		if(xs.length == 0)
			return 0;
		long ret = 0;
		for(int i = 0; i < xs.length; i++)
			ret += xs[i] * xs[i];
		return Math.sqrt(ret / (double) xs.length);
	}

	static public long vsum(long[] xs){
		long ret = 0;
		for(int i = 0; i < xs.length; i++)
			ret += xs[i];
		return ret;
	}

	static public boolean vequiv(long[] xs, long[] ys){
		return Arrays.equals(xs, ys);
	}

	static public long[] vadd(long[] x, long[] ys){
		final long[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] += ys[i];
		return xs;
	}

	static public long[] vsub(long[] x, long[] ys){
		final long[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] -= ys[i];
		return xs;
	}

	static public long[] vaddmul(long[] x, long[] ys, long[] zs){
		final long[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = (xs[i] + ys[i]) * zs[i];
		return xs;
	}

	static public long[] vsubmul(long[] x, long[] ys, long[] zs){
		final long[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = (xs[i] - ys[i]) * zs[i];
		return xs;
	}

	static public long[] vaddsmul(long[] x, long[] ys, long z){
		final long[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = (xs[i] + ys[i]) * z;
		return xs;
	}

	static public long[] vsubsmul(long[] x, long[] ys, long z){
		final long[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = (xs[i] - ys[i]) * z;
		return xs;
	}

	static public long[] vmulsadd(long[] x, long[] ys, long z){
		final long[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = (xs[i] * ys[i]) + z;
		return xs;
	}

	static public long[] vdiv(long[] x, long[] ys){
		final long[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] /= ys[i];
		return xs;
	}

	static public long[] vmul(long[] x, long[] ys){
		final long[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] *= ys[i];
		return xs;
	}

	static public long[] vmuladd(long[] x, long[] ys, long[] zs){
		final long[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = (xs[i] * ys[i]) + zs[i];
		return xs;
	}

	static public long[] vmulsub(long[] x, long[] ys, long[] zs){
		final long[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = (xs[i] * ys[i]) - zs[i];
		return xs;
	}

	static public long[] vmax(long[] x, long[] ys){
		final long[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = Math.max(xs[i], ys[i]);
		return xs;
	}

	static public long[] vmin(long[] x, long[] ys){
		final long[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = Math.min(xs[i], ys[i]);
		return xs;
	}

	static public long[] vmap(IFn fn, long[] x) {
		long[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = ((Number) fn.invoke(xs[i])).longValue();
		return xs;
	}

	static public long[] vmap(IFn fn, long[] x, long[] ys) {
		long[] xs = x.clone();
		for(int i = 0; i < xs.length; i++)
			xs[i] = ((Number) fn.invoke(xs[i], ys[i])).longValue();
		return xs;
	}

}
*/


//overload resolution
//*

static public Number add(long x, Object y){
	return add((Object)x,y);
}

static public Number add(Object x, long y){
	return add(x,(Object)y);
}

static public Number addP(long x, Object y){
	return addP((Object)x,y);
}

static public Number addP(Object x, long y){
	return addP(x,(Object)y);
}

static public double add(double x, Object y){
	return add(x,((Number)y).doubleValue());
}

static public double add(Object x, double y){
	return add(((Number)x).doubleValue(),y);
}

static public double add(double x, long y){
	return x + y;
}

static public double add(long x, double y){
	return x + y;
}

static public double addP(double x, Object y){
	return addP(x,((Number)y).doubleValue());
}

static public double addP(Object x, double y){
	return addP(((Number)x).doubleValue(),y);
}

static public double addP(double x, long y){
	return x + y;
}

static public double addP(long x, double y){
	return x + y;
}

static public Number minus(long x, Object y){
	return minus((Object)x,y);
}

static public Number minus(Object x, long y){
	return minus(x,(Object)y);
}

static public Number minusP(long x, Object y){
	return minusP((Object)x,y);
}

static public Number minusP(Object x, long y){
	return minusP(x,(Object)y);
}

static public double minus(double x, Object y){
	return minus(x,((Number)y).doubleValue());
}

static public double minus(Object x, double y){
	return minus(((Number)x).doubleValue(),y);
}

static public double minus(double x, long y){
	return x - y;
}

static public double minus(long x, double y){
	return x - y;
}

static public double minusP(double x, Object y){
	return minus(x,((Number)y).doubleValue());
}

static public double minusP(Object x, double y){
	return minus(((Number)x).doubleValue(),y);
}

static public double minusP(double x, long y){
	return x - y;
}

static public double minusP(long x, double y){
	return x - y;
}

static public Number multiply(long x, Object y){
	return multiply((Object)x,y);
}

static public Number multiply(Object x, long y){
	return multiply(x,(Object)y);
}

static public Number multiplyP(long x, Object y){
	return multiplyP((Object)x,y);
}

static public Number multiplyP(Object x, long y){
	return multiplyP(x,(Object)y);
}

static public double multiply(double x, Object y){
	return multiply(x,((Number)y).doubleValue());
}

static public double multiply(Object x, double y){
	return multiply(((Number)x).doubleValue(),y);
}

static public double multiply(double x, long y){
	return x * y;
}

static public double multiply(long x, double y){
	return x * y;
}

static public double multiplyP(double x, Object y){
	return multiplyP(x,((Number)y).doubleValue());
}

static public double multiplyP(Object x, double y){
	return multiplyP(((Number)x).doubleValue(),y);
}

static public double multiplyP(double x, long y){
	return x * y;
}

static public double multiplyP(long x, double y){
	return x * y;
}

static public Number divide(long x, Object y){
	return divide((Object)x,y);
}

static public Number divide(Object x, long y){
	return divide(x,(Object)y);
}

static public double divide(double x, Object y){
	return x / ((Number)y).doubleValue();
}

static public double divide(Object x, double y){
	return ((Number)x).doubleValue() / y;
}

static public double divide(double x, long y){
	return x / y;
}

static public double divide(long x, double y){
    return x / y;
}

static public Number divide(long x, long y){
	return divide((Number)x, (Number)y);
}

static public boolean lt(long x, Object y){
	return lt((Object)x,y);
}

static public boolean lt(Object x, long y){
	return lt(x,(Object)y);
}

static public boolean lt(double x, Object y){
	return x < ((Number)y).doubleValue();
}

static public boolean lt(Object x, double y){
	return ((Number)x).doubleValue() < y;
}

static public boolean lt(double x, long y){
	return x < y;
}

static public boolean lt(long x, double y){
	return x < y;
}

static public boolean lte(long x, Object y){
	return lte((Object)x,y);
}

static public boolean lte(Object x, long y){
	return lte(x,(Object)y);
}

static public boolean lte(double x, Object y){
	return x <= ((Number)y).doubleValue();
}

static public boolean lte(Object x, double y){
	return ((Number)x).doubleValue() <= y;
}

static public boolean lte(double x, long y){
	return x <= y;
}

static public boolean lte(long x, double y){
	return x <= y;
}

static public boolean gt(long x, Object y){
	return gt((Object)x,y);
}

static public boolean gt(Object x, long y){
	return gt(x,(Object)y);
}

static public boolean gt(double x, Object y){
	return x > ((Number)y).doubleValue();
}

static public boolean gt(Object x, double y){
	return ((Number)x).doubleValue() > y;
}

static public boolean gt(double x, long y){
	return x > y;
}

static public boolean gt(long x, double y){
	return x > y;
}

static public boolean gte(long x, Object y){
	return gte((Object)x,y);
}

static public boolean gte(Object x, long y){
	return gte(x,(Object)y);
}

static public boolean gte(double x, Object y){
	return x >= ((Number)y).doubleValue();
}

static public boolean gte(Object x, double y){
	return ((Number)x).doubleValue() >= y;
}

static public boolean gte(double x, long y){
	return x >= y;
}

static public boolean gte(long x, double y){
	return x >= y;
}

static public boolean equiv(long x, Object y){
	return equiv((Object)x,y);
}

static public boolean equiv(Object x, long y){
	return equiv(x,(Object)y);
}

static public boolean equiv(double x, Object y){
	return x == ((Number)y).doubleValue();
}

static public boolean equiv(Object x, double y){
	return ((Number)x).doubleValue() == y;
}

static public boolean equiv(double x, long y){
	return x == y;
}

static public boolean equiv(long x, double y){
	return x == y;
}


static boolean isNaN(Object x){
	return (x instanceof Double) && ((Double)x).isNaN()
		|| (x instanceof Float) && ((Float)x).isNaN();
}

static public double max(double x, double y){
	return Math.max(x, y);
}

static public Object max(double x, long y){
	if(Double.isNaN(x)){
		return x;
	}
	if(x > y){
		return x;
	} else {
		return y;
	}
}

static public Object max(double x, Object y){
	if(Double.isNaN(x)){
		return x;
	} else if(isNaN(y)){
		return y;
	}
	if(x > ((Number)y).doubleValue()){
		return x;
	} else {
		return y;
	}
}

static public Object max(long x, double y){
	if(Double.isNaN(y)){
		return y;
	}
	if(x > y){
		return x;
	} else {
		return y;
	}
}


static public long max(long x, long y){
	if(x > y) {
		return x;
	} else {
		return y;
	}
}


static public Object max(long x, Object y){
	if(isNaN(y)){
		return y;
	}
	if(gt(x,y)){
		return x;
	} else {
		return y;
	}
}

static public Object max(Object x, long y){
	if(isNaN(x)){
		return x;
	}
	if(gt(x,y)){
		return x;
	} else {
		return y;
	}
}

static public Object max(Object x, double y){
	if (isNaN(x)){
		return x;
	} else if(Double.isNaN(y)){
		return y;
	}
	if(((Number)x).doubleValue() > y){
		return x;
	} else {
		return y;
	}
}

static public Object max(Object x, Object y){
	if(isNaN(x)){
		return x;
	} else if(isNaN(y)){
		return y;
	}
	if(gt(x, y)) {
		return x;
	} else {
		return y;
	}
}


static public double min(double x, double y){
	return Math.min(x, y);
}

static public Object min(double x, long y){
	if (Double.isNaN(x)){
		return x;
	}
	if(x < y){
		return x;
	} else {
		return y;
	}
}

static public Object min(double x, Object y){
	if(Double.isNaN(x)){
		return x;
	} else if(isNaN(y)){
		return y;
	}
	if(x < ((Number)y).doubleValue()){
		return x;
	} else {
		return y;
	}
}

static public Object min(long x, double y){
	if(Double.isNaN(y)){
		return y;
	}
	if(x < y){
		return x;
	} else {
		return y;
	}
}


static public long min(long x, long y){
	if(x < y) {
		return x;
	} else {
		return y;
	}
}

static public Object min(long x, Object y){
	if(isNaN(y)){
		return y;
	}
	if(lt(x,y)){
		return x;
	} else {
		return y;
	}
}

static public Object min(Object x, long y){
	if(isNaN(x)){
		return x;
	}
	if(lt(x,y)){
		return x;
	} else {
		return y;
	}
}

static public Object min(Object x, double y){
	if(isNaN(x)){
		return x;
	} else if(Double.isNaN(y)){
		return y;
	}
	if(((Number)x).doubleValue() < y){
		return x;
	} else {
		return y;
	}
}

static public Object min(Object x, Object y){
	if (isNaN(x)){
		return x;
	} else if(isNaN(y)){
		return y;
	}
	if(lt(x,y)) {
		return x;
	} else {
		return y;
	}
}

}
