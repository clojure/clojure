/**
 *   Copyright (c) David Miller. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using java.math;

namespace clojure.lang
{
    public class Numbers
    {

        interface Ops
        {
            Ops combine(Ops y);
            Ops opsWith(IntegerOps x);
            Ops opsWith(LongOps x);
            Ops opsWith(FloatOps x);
            Ops opsWith(DoubleOps x);
            Ops opsWith(RatioOps x);
            Ops opsWith(BigIntegerOps x);
            Ops opsWith(BigDecimalOps x);
            bool isZero(object x);
            bool isPos(object x);
            bool isNeg(object x);
            object add(object x, object y);
            object multiply(object x, object y);
            object divide(object x, object y);
            object quotient(object x, object y);
            object remainder(object x, object y);
            bool equiv(object x, object y);
            bool lt(object x, object y);
            object negate(object x);
            object inc(object x);
            object dec(object x);
        }

        interface BitOps
        {
            BitOps combine(BitOps y);
            BitOps bitOpsWith(IntegerBitOps x);
            BitOps bitOpsWith(LongBitOps x);
            BitOps bitOpsWith(BigIntegerBitOps x);
            object not(object x);
            object and(object x, object y);
            object or(object x, object y);
            object xor(object x, object y);
            object andNot(object x, object y);
            object clearBit(object x, int n);
            object setBit(object x, int n);
            object flipBit(object x, int n);
            bool testBit(object x, int n);
            object shiftLeft(object x, int n);
            object shiftRight(object x, int n);
        }

        #region Basic Ops operations

        public static bool isZero(object x)
        {
            return ops(x).isZero(x);
        }

        public static bool isPos(object x)
        {
            return ops(x).isPos(x);
        }

        public static bool isNeg(object x)
        {
            return ops(x).isNeg(x);
        }

        public static object minus(object x)
        {
            return ops(x).negate(x);
        }

        public static object inc(object x)
        {
            return ops(x).inc(x);
        }

        public static object dec(object x)
        {
            return ops(x).dec(x);
        }

        public static object add(object x, object y)
        {
            return ops(x).combine(ops(y)).add(x, y);
        }

        public static object minus(object x, object y)
        {
            Ops yops = ops(y);
            return ops(x).combine(yops).add(x, yops.negate(y));
        }

        public static object multiply(object x, object y)
        {
            return ops(x).combine(ops(y)).multiply(x, y);
        }

        public static object divide(object x, object y)
        {
            Ops yops = ops(y);
            if (yops.isZero(y))
                throw new ArithmeticException("Divide by zero");
            return ops(x).combine(yops).divide(x, y);
        }

        public static object quotient(object x, object y)
        {
            Ops yops = ops(y);
            if (yops.isZero(y))
                throw new ArithmeticException("Divide by zero");
            return ops(x).combine(yops).quotient(x, y);
        }

        public static object remainder(object x, object y)
        {
            Ops yops = ops(y);
            if (yops.isZero(y))
                throw new ArithmeticException("Divide by zero");
            return ops(x).combine(yops).remainder(x, y);
        }


        public static object DQuotient(double n, double d)
        {
            double q = n / d;
            if (q <= Int32.MaxValue && q >= Int32.MinValue)
                return (int)q;
            else
                // bigint quotient
                return reduce(new BigDecimal(q).toBigInteger());
        }

        public static object DRemainder(double n, double d)
        {
            double q = n / d;
            if (q <= Int32.MaxValue && q >= Int32.MinValue)
                return n - ((int)q) * d;
            else
            {
                // bigint quotient
                object bq = reduce(new BigDecimal(q).toBigInteger());
                return n - ((double)bq) * d;
            }
        }

        public static Boolean equiv(object x, object y)
        {
            return Util.IsNumeric(x)
                && Util.IsNumeric(y)
                && ops(x).combine(ops(y)).equiv(x, y);
        }

        public static bool lt(object x, object y)
        {
            return ops(x).combine(ops(y)).lt(x, y);
        }

        public static bool lte(object x, object y)
        {
            return !ops(x).combine(ops(y)).lt(y, x);
        }

        public static bool gt(object x, object y)
        {
            return ops(x).combine(ops(y)).lt(y, x);
        }

        public static bool gte(object x, object y)
        {
            return !ops(x).combine(ops(y)).lt(x, y);
        }

        public static int compare(object x, object y)
        {
            Ops ops1 = ops(x).combine(ops(y));
            if (ops1.lt(x, y))
                return -1;
            else if (ops1.lt(y, x))
                return 1;
            else
                return 0;
        }

        #endregion

        #region  utility methods

        static BigInteger toBigInteger(object x)
        {
            if (x is BigInteger)
                return (BigInteger)x;
            else
                // TODO: determine if we should just cast.
                return BigInteger.valueOf(Convert.ToInt64(x));
        }

        static BigDecimal toBigDecimal(object x)
        {
            if (x is BigDecimal)
                return (BigDecimal)x;
            else if ( x is BigInteger)
                return new BigDecimal((BigInteger)x);
            else
                // TODO: determine if we should just cast.
                return BigDecimal.valueOf(Convert.ToInt64(x));
        }

        // TODO: doublecheck toRatio
        // Java BigDecimal has unscaledValue and pow.  
        // MS implementation does not.
        static Ratio toRatio(object x)
        {
            if (x is Ratio)
                return (Ratio)x;
            else if (x is BigDecimal)
            {
                BigDecimal bx = (BigDecimal)x;
                int scale = bx.scale();
                if (scale < 0)
                    return new Ratio(bx.toBigInteger(), BigIntegerOne);
                else
                    return new Ratio(bx.movePointRight(scale).toBigInteger(), BigIntegerTen.pow(scale));
            }
            return new Ratio(toBigInteger(x), BigIntegerOne);
        }
                    
        // TODO: fix rationalize
        // Java BigDecimal has .valueOf(double)
        public static object rationalize(object x)
        {
            if ( x is float || x is double )
                return rationalize(new BigDecimal( Convert.ToDouble(x) )); // wrong, should be (double)x));
            else if ( x is BigDecimal )
            {
                BigDecimal bx= (BigDecimal)x;
                int scale = bx.scale();
                if (scale < 0)
                    return bx.toBigInteger();
                else
                    return divide(bx.movePointRight(scale).toBigInteger(), BigIntegerTen.pow(scale));
            }
            return x;
        }

        public static object reduce(BigInteger val)
        {
            int bitLength = val.bitLength();
            return (bitLength < 32)
                ? (object)val.intValue()
                : (bitLength < 64)
                    ? (object)val.longValue()
                    : val;
        }

        public static object reduce(long val)
        {
            if (val >= Int32.MinValue && val <= Int32.MaxValue)
                return (int)val;
            else
                return val;
        }

        public static object BIDivide(BigInteger n, BigInteger d)
        {
            if (d.Equals(BigIntegerZero))
                throw new ArithmeticException("Divide by zero");
            BigInteger gcd = n.gcd(d);
            if (gcd.Equals(BigIntegerZero))
                return 0;
            n = n.divide(gcd);
            d = d.divide(gcd);
            if (d.Equals(BigIntegerOne))
                return reduce(n);
            return new Ratio((d.signum() < 0 ? n.negate() : n),
                (d.signum() < 0 ? d.negate() : d));
        }

 

        public static BigInteger BigIntegerTen  = BigInteger.valueOf(10);
        public static BigInteger BigIntegerOne  = BigInteger.valueOf(1);
        public static BigInteger BigIntegerZero = BigInteger.valueOf(0);

        public static BigDecimal BigDecimalOne = BigDecimal.valueOf(1);

        #endregion

        #region Basic BitOps operations

        public static object not(object x)
        {
            return bitOps(x).not(x);
        }

        public static object and(object x, object y)
        {
            return bitOps(x).combine(bitOps(y)).and(x, y);
        }

        public static object or(object x, object y)
        {
            return bitOps(x).combine(bitOps(y)).or(x, y);
        }

        public static object xor(object x, object y)
        {
            return bitOps(x).combine(bitOps(y)).xor(x, y);
        }

        public static object andNot(object x, object y)
        {
            return bitOps(x).combine(bitOps(y)).andNot(x, y);
        }


        public static object clearBit(object x, int n)
        {
            if (n < 0)
                throw new ArithmeticException("Negative bit index");
            return bitOps(x).clearBit(x, n);
        }

        public static object setBit(object x, int n)
        {
            if (n < 0)
                throw new ArithmeticException("Negative bit index");
            return bitOps(x).setBit(x, n);
        }

        public static object flipBit(object x, int n)
        {
            if (n < 0)
                throw new ArithmeticException("Negative bit index");
            return bitOps(x).flipBit(x, n);
        }

        public static bool testBit(object x, int n)
        {
            if (n < 0)
                throw new ArithmeticException("Negative bit index");
            return bitOps(x).testBit(x, n);
        }

        public static object shiftLeft(object x, int n)
        {
            return bitOps(x).shiftLeft(x, n);
        }

        public static object shiftRight(object x, int n)
        {
            return bitOps(x).shiftRight(x, n);
        }

        #endregion

        #region Ops/BitOps dispatching

        static readonly IntegerOps INTEGER_OPS = new IntegerOps();
        static readonly LongOps LONG_OPS = new LongOps();
        static readonly FloatOps FLOAT_OPS = new FloatOps();
        static readonly DoubleOps DOUBLE_OPS = new DoubleOps();
        static readonly RatioOps RATIO_OPS = new RatioOps();
        static readonly BigIntegerOps BIGINTEGER_OPS = new BigIntegerOps();
        static readonly BigDecimalOps BIGDECIMAL_OPS = new BigDecimalOps();

        static readonly IntegerBitOps INTEGER_BITOPS = new IntegerBitOps();
        static readonly LongBitOps LONG_BITOPS = new LongBitOps();
        static readonly BigIntegerBitOps BIGINTEGER_BITOPS = new BigIntegerBitOps();

        static Ops ops(object x)
        {
            Type type = Util.GetNonNullableType(x.GetType());
            if (!type.IsEnum)
            {

                switch (Type.GetTypeCode(type))
                {
                    case TypeCode.Char:
                    case TypeCode.SByte:
                    case TypeCode.Byte:
                    case TypeCode.Int16:
                    case TypeCode.UInt16:
                    case TypeCode.Int32:
                        return INTEGER_OPS;
                    case TypeCode.Double:
                        return DOUBLE_OPS;
                    case TypeCode.Single:
                        return FLOAT_OPS;
                    case TypeCode.UInt32:
                    case TypeCode.Int64:
                        return LONG_OPS;
                    case TypeCode.UInt64:
                        return BIGINTEGER_OPS;

                    default:
                        if (type == typeof(BigInteger))
                            return BIGINTEGER_OPS;
                        else if (type == typeof(Ratio))
                            return RATIO_OPS;
                        else if (type == typeof(BigDecimal))
                            return BIGDECIMAL_OPS;
                        else
                            return INTEGER_OPS;
                }
            }
            return INTEGER_OPS;
        }


        static BitOps bitOps(object x)
        {
            Type type = Util.GetNonNullableType(x.GetType());

            if (!type.IsEnum)
            {

                switch (Type.GetTypeCode(type))
                {
                    case TypeCode.Int32:
                        return INTEGER_BITOPS;
                    case TypeCode.Int64:
                        return LONG_BITOPS;
                    default:
                        if (type == typeof(BigInteger))
                            return BIGINTEGER_BITOPS;
                        else if (Util.IsNumeric(x) || (type == typeof(BigDecimal)) || (type == typeof(Ratio)))
                            throw new ArithmeticException("bit operation on non integer type: " + type);
                        break;
                }
            }
            return INTEGER_BITOPS;
        }

        #endregion

        sealed class IntegerOps : Ops
        {
            #region Ops Members

            public Ops combine(Ops y)
            {
                return y.opsWith(this);
            }

            public Ops opsWith(IntegerOps x)
            {
                return this;
            }

            public Ops opsWith(LongOps x)
            {
                return LONG_OPS;
            }

            public Ops opsWith(FloatOps x)
            {
                return FLOAT_OPS;
            }

            public Ops opsWith(DoubleOps x)
            {
                return DOUBLE_OPS;
            }

            public Ops opsWith(RatioOps x)
            {
                return RATIO_OPS;
            }

            public Ops opsWith(BigIntegerOps x)
            {
                return BIGINTEGER_OPS;
            }

            public Ops opsWith(BigDecimalOps x)
            {
                return BIGDECIMAL_OPS;
            }

            public bool isZero(object x)
            {
                return Convert.ToInt32(x) == 0;
            }

            public bool isPos(object x)
            {
                return Convert.ToInt32(x) > 0;
            }

            public bool isNeg(object x)
            {
                return Convert.ToInt32(x) < 0;
            }

            public object add(object x, object y)
            {
                long ret = Convert.ToInt64(x) + Convert.ToInt64(y);
                if (ret <= Int32.MaxValue && ret >= Int32.MinValue)
                    return (int)ret;
                return ret;
            }

            public object multiply(object x, object y)
            {
                long ret = Convert.ToInt64(x) * Convert.ToInt64(y);
                if (ret <= Int32.MaxValue && ret >= Int32.MinValue)
                    return (int)ret;
                return ret;
            }

            static int gcd(int u, int v)
            {
                while (v != 0)
                {
                    int r = u % v;
                    u = v;
                    v = r;
                }
                return u;
            }

            public object divide(object x, object y)
            {
                int n = Convert.ToInt32(x);
                int val = Convert.ToInt32(y);
                int gcd1 = gcd(n, val);
                if (gcd1 == 0)
                    return 0;

                n = n / gcd1;
                int d = val / gcd1;
                if (d == 1)
                    return n;
                if (d < 0)
                {
                    n = -n;
                    d = -d;
                }
                return new Ratio(BigInteger.valueOf(n), BigInteger.valueOf(d));
            }

            public object quotient(object x, object y)
            {
                return Convert.ToInt32(x) / Convert.ToInt32(y);
            }

            public object remainder(object x, object y)
            {
                return Convert.ToInt32(x) % Convert.ToInt32(y);
            }

            public bool equiv(object x, object y)
            {
                return Convert.ToInt32(x) == Convert.ToInt32(y);
            }

            public bool lt(object x, object y)
            {
                return Convert.ToInt32(x) < Convert.ToInt32(y);
            }

            public object negate(object x)
            {
                int val = Convert.ToInt32(x);
                if (val > Int32.MinValue)
                    return -val;
                return -((long)val);
            }

            public object inc(object x)
            {
                int val = Convert.ToInt32(x);
                if (val < Int32.MaxValue)
                    return val + 1;
                return BigInteger.valueOf(((long)val) + 1);
            }

            public object dec(object x)
            {
                int val = Convert.ToInt32(x);
                if (val > Int32.MinValue)
                    return val - 1;
                return BigInteger.valueOf(((long)val) - 1);
            }

            #endregion
        }

        sealed class LongOps : Ops
        {
            #region Ops Members

            public Ops combine(Ops y)
            {
                return y.opsWith(this);
            }

            public Ops opsWith(IntegerOps x)
            {
                return this;
            }

            public Ops opsWith(LongOps x)
            {
                return this;
            }

            public Ops opsWith(FloatOps x)
            {
                return FLOAT_OPS;
            }

            public Ops opsWith(DoubleOps x)
            {
                return DOUBLE_OPS;
            }

            public Ops opsWith(RatioOps x)
            {
                return RATIO_OPS;
            }

            public Ops opsWith(BigIntegerOps x)
            {
                return BIGINTEGER_OPS;
            }

            public Ops opsWith(BigDecimalOps x)
            {
                return BIGDECIMAL_OPS;
            }

            public bool isZero(object x)
            {
                return Convert.ToInt64(x) == 0;
            }

            public bool isPos(object x)
            {
                return Convert.ToInt64(x) > 0;
            }

            public bool isNeg(object x)
            {
                return Convert.ToInt64(x) < 0;
            }

            public object add(object x, object y)
            {
                long lx = Convert.ToInt64(x);
                long ly = Convert.ToInt64(y);
                long ret = lx + ly;
                if ((ret ^ lx) < 0 && (ret ^ ly) < 0)
                    return BIGINTEGER_OPS.add(x, y);
                return ret;
            }

            public object multiply(object x, object y)
            {
                long lx = Convert.ToInt64(x);
                long ly = Convert.ToInt64(y);
                long ret = lx * ly;
                if (ly != 0 && ret / ly != lx)
                    return BIGINTEGER_OPS.multiply(x, y);
                return ret;
            }
            
            static long gcd(long u, long v)
            {
                while (v != 0)
                {
                    long r = u % v;
                    u = v;
                    v = r;
                }
                return u;
            }

            public object divide(object x, object y)
            {
                long n = Convert.ToInt64(x);
                long val = Convert.ToInt64(y);
                long gcd1 = gcd(n, val);
                if (gcd1 == 0)
                    return 0;

                n = n / gcd1;
                long d = val / gcd1;
                if (d == 1)
                    return n;
                if (d < 0)
                {
                    n = -n;
                    d = -d;
                }
                return new Ratio(BigInteger.valueOf(n), BigInteger.valueOf(d));
            }

            public object quotient(object x, object y)
            {
                return Convert.ToInt64(x) / Convert.ToInt64(y);
            }

            public object remainder(object x, object y)
            {
                return Convert.ToInt64(x) % Convert.ToInt64(y);
            }

            public bool equiv(object x, object y)
            {
                return Convert.ToInt64(x) == Convert.ToInt64(y);
            }

            public bool lt(object x, object y)
            {
                return Convert.ToInt64(x) < Convert.ToInt64(y);
            }

            public object negate(object x)
            {
                long val = Convert.ToInt64(x);
                if (val > Int64.MinValue)
                    return -val;
                return BigInteger.valueOf(val).negate();
            }

            public object inc(object x)
            {
                long val = Convert.ToInt64(x);
                if (val < Int64.MaxValue)
                    return val + 1;
                return BIGINTEGER_OPS.inc(x);
            }

            public object dec(object x)
            {
                long val = Convert.ToInt64(x);
                if (val > Int64.MinValue)
                    return val - 1;
                return BIGINTEGER_OPS.dec(x);
            }

            #endregion
        }

        class FloatOps : Ops
        {
            #region Ops Members

            public Ops combine(Ops y)
            {
                return y.opsWith(this);
            }

            public Ops opsWith(IntegerOps x)
            {
                return this ;
            }

            public Ops opsWith(LongOps x)
            {
                return this;
            }
            public Ops opsWith(FloatOps x)
            {
                return this;
            }

            public Ops opsWith(DoubleOps x)
            {
                return DOUBLE_OPS;
            }

            public Ops opsWith(RatioOps x)
            {
                return this;
            }

            public Ops opsWith(BigIntegerOps x)
            {
                return this;
            }

            public Ops opsWith(BigDecimalOps x)
            {
                return this;
            }

            public bool isZero(object x)
            {
                return Convert.ToSingle(x) == 0;
            }

            public bool isPos(object x)
            {
                return Convert.ToSingle(x) > 0;
            }

            public bool isNeg(object x)
            {
                return Convert.ToSingle(x) < 0;
            }

            public object add(object x, object y)
            {
                return Convert.ToSingle(x) + Convert.ToSingle(y);
            }

            public object multiply(object x, object y)
            {
                return Convert.ToSingle(x) * Convert.ToSingle(y);
            }

            public object divide(object x, object y)
            {
                return Convert.ToSingle(x) / Convert.ToSingle(y);
            }

            public object quotient(object x, object y)
            {
                return Numbers.DQuotient(Convert.ToDouble(x), Convert.ToDouble(y));
            }

            public object remainder(object x, object y)
            {
                return Numbers.DRemainder(Convert.ToDouble(x), Convert.ToDouble(y));
            }

            public bool equiv(object x, object y)
            {
                return Convert.ToSingle(x) == Convert.ToSingle(y);
            }

            public bool lt(object x, object y)
            {
                return Convert.ToSingle(x) < Convert.ToSingle(y);
            }

            public object negate(object x)
            {
                return -Convert.ToSingle(x);
            }

            public object inc(object x)
            {
                return Convert.ToSingle(x) + 1;
            }

            public object dec(object x)
            {
                return Convert.ToSingle(x) - 1;
            }

            #endregion
        }

        class DoubleOps : Ops
        {
            #region Ops Members

            public Ops combine(Ops y)
            {
                return y.opsWith(this);
            }

            public Ops opsWith(IntegerOps x)
            {
                return this;
            }
            
            public Ops opsWith(LongOps x)
            {
                return this;
            }

            public Ops opsWith(FloatOps x)
            {
                return this;
            }

            public Ops opsWith(DoubleOps x)
            {
                return this;
            }

            public Ops opsWith(RatioOps x)
            {
                return this;
            }

            public Ops opsWith(BigIntegerOps x)
            {
                return this;
            }

            public Ops opsWith(BigDecimalOps x)
            {
                return this;
            }

            public bool isZero(object x)
            {
                return Convert.ToDouble(x) == 0;
            }

            public bool isPos(object x)
            {
                return Convert.ToDouble(x) > 0;
            }

            public bool isNeg(object x)
            {
                return Convert.ToDouble(x) < 0;
            }

            public object add(object x, object y)
            {
                return Convert.ToDouble(x) + Convert.ToDouble(y);
            }

            public object multiply(object x, object y)
            {
                return Convert.ToDouble(x) * Convert.ToDouble(y);
            }

            public object divide(object x, object y)
            {
                return Convert.ToDouble(x) / Convert.ToDouble(y);
            }

            public object quotient(object x, object y)
            {
                return Numbers.DQuotient(Convert.ToDouble(x), Convert.ToDouble(y));
            }

            public object remainder(object x, object y)
            {
                return Numbers.DRemainder(Convert.ToDouble(x), Convert.ToDouble(y));
            }

            public bool equiv(object x, object y)
            {
                return Convert.ToDouble(x) == Convert.ToDouble(y);
            }

            public bool lt(object x, object y)
            {
                return Convert.ToDouble(x) < Convert.ToDouble(y);
            }

            public object negate(object x)
            {
                return -Convert.ToDouble(x);
            }

            public object inc(object x)
            {
                return Convert.ToDouble(x) + 1;
            }

            public object dec(object x)
            {
                return Convert.ToDouble(x) - 1;
            }

            #endregion
        }

        class RatioOps : Ops
        {
            #region Ops Members

            public Ops combine(Ops y)
            {
                return y.opsWith(this);
            }

            public Ops opsWith(IntegerOps x)
            {
                return this;
            }

            public Ops opsWith(LongOps x)
            {
                return this;
            }
            public Ops opsWith(FloatOps x)
            {
                return FLOAT_OPS;
            }

            public Ops opsWith(DoubleOps x)
            {
                return DOUBLE_OPS;
            }

            public Ops opsWith(RatioOps x)
            {
                return this;
            }

            public Ops opsWith(BigIntegerOps x)
            {
                return this;
            }

            public Ops opsWith(BigDecimalOps x)
            {
                return this;
            }

            public bool isZero(object x)
            {
                Ratio r =  toRatio(x);
                return r.numerator.signum()== 0;
            }

            public bool isPos(object x)
            {
                Ratio r = toRatio(x);
                return r.numerator.signum() > 0;
            }

            public bool isNeg(object x)
            {
                Ratio r = toRatio(x);
                return r.numerator.signum() < 0;
            }

            public object add(object x, object y)
            {
                Ratio rx = toRatio(x);
                Ratio ry = toRatio(y);
                return divide(ry.numerator.multiply(rx.denominator)
                    .add(rx.numerator.multiply(ry.denominator)),
                    ry.denominator.multiply(rx.denominator));
            }

            public object multiply(object x, object y)
            {
                Ratio rx = toRatio(x);
                Ratio ry = toRatio(y);
                return Numbers.divide(ry.numerator.multiply(rx.numerator),
                    ry.denominator.multiply(rx.denominator));
            }

            public object divide(object x, object y)
            {
                Ratio rx = toRatio(x);
                Ratio ry = toRatio(y);
                return Numbers.divide(ry.denominator.multiply(rx.numerator),
                    ry.numerator.multiply(rx.denominator));
            }

            public object quotient(object x, object y)
            {
                Ratio rx = toRatio(x);
                Ratio ry = toRatio(y);
                BigInteger q = rx.numerator.multiply(ry.denominator)
                    .divide(rx.denominator.multiply(ry.numerator));
                return reduce(q);
            }

            public object remainder(object x, object y)
            {
                Ratio rx = toRatio(x);
                Ratio ry = toRatio(y);
                BigInteger q = rx.numerator.multiply(ry.denominator)
                    .divide(rx.denominator.multiply(ry.numerator));
                return Numbers.minus(x, Numbers.multiply(q, y));
            }

            public bool equiv(object x, object y)
            {
                Ratio rx = toRatio(x);
                Ratio ry = toRatio(y);
                return rx.numerator.Equals(ry.numerator)
                    && rx.denominator.Equals(ry.denominator);
            }

            public bool lt(object x, object y)
            {
                Ratio rx = toRatio(x);
                Ratio ry = toRatio(y);
                return Numbers.lt(rx.numerator.multiply(ry.denominator),
                    ry.numerator.multiply(rx.denominator));
            }

            public object negate(object x)
            {
                Ratio rx = toRatio(x);
                return new Ratio(rx.numerator.negate(), rx.denominator);
            }

            public object inc(object x)
            {
                return Numbers.add(x, 1);
            }

            public object dec(object x)
            {
                return Numbers.add(x, -1);
            }

            #endregion
        }

        class BigIntegerOps : Ops
        {
            #region Ops Members

            public Ops combine(Ops y)
            {
                return y.opsWith(this);
            }

            public Ops opsWith(IntegerOps x)
            {
                return this;
            }

            public Ops opsWith(LongOps x)
            {
                return this;
            }
            public Ops opsWith(FloatOps x)
            {
                return FLOAT_OPS;
            }

            public Ops opsWith(DoubleOps x)
            {
                return DOUBLE_OPS;
            }

            public Ops opsWith(RatioOps x)
            {
                return RATIO_OPS;
            }

            public Ops opsWith(BigIntegerOps x)
            {
                return this;
            }

            public Ops opsWith(BigDecimalOps x)
            {
                return BIGDECIMAL_OPS;
            }

            public bool isZero(object x)
            {
                BigInteger bx = toBigInteger(x);
                return bx.signum() == 0;
            }

            public bool isPos(object x)
            {
                BigInteger bx = toBigInteger(x);
                return bx.signum() > 0;
            }

            public bool isNeg(object x)
            {
                BigInteger bx = toBigInteger(x);
                return bx.signum() < 0;
            }

            public object add(object x, object y)
            {
                return reduce(toBigInteger(x).add(toBigInteger(y)));
            }

            public object multiply(object x, object y)
            {
                return reduce(toBigInteger(x).multiply(toBigInteger(y)));
            }

            public object divide(object x, object y)
            {
                return BIDivide(toBigInteger(x), toBigInteger(y));
            }

            public object quotient(object x, object y)
            {
                return toBigInteger(x).divide(toBigInteger(y));
            }

            public object remainder(object x, object y)
            {
                return toBigInteger(x).remainder(toBigInteger(y));
            }

            public bool equiv(object x, object y)
            {
                return toBigInteger(x).Equals(toBigInteger(y));
            }

            public bool lt(object x, object y)
            {
                return toBigInteger(x).compareTo(toBigInteger(y)) < 0;
            }

            public object negate(object x)
            {
                return toBigInteger(x).negate();
            }

            public object inc(object x)
            {
                BigInteger bx = toBigInteger(x);
                return reduce(bx.add(BigIntegerOne));
            }

            public object dec(object x)
            {
                BigInteger bx = toBigInteger(x);
                return reduce(bx.subtract(BigIntegerOne));
            }

            #endregion
        }

        class BigDecimalOps : Ops
        {
            #region Ops Members

            public Ops combine(Ops y)
            {
                return y.opsWith(this);
            }

            public Ops opsWith(IntegerOps x)
            {
                return this;
            }

            public Ops opsWith(LongOps x)
            {
                return this;
            }

            public Ops opsWith(FloatOps x)
            {
                return FLOAT_OPS;
            }

            public Ops opsWith(DoubleOps x)
            {
                return DOUBLE_OPS;
            }

            public Ops opsWith(RatioOps x)
            {
                return RATIO_OPS;
            }

            public Ops opsWith(BigIntegerOps x)
            {
                return this;
            }

            public Ops opsWith(BigDecimalOps x)
            {
                return this;
            }

            public bool isZero(object x)
            {
                BigDecimal bx = toBigDecimal(x);
                return bx.signum() == 0;
            }

            public bool isPos(object x)
            {
                BigDecimal bx = toBigDecimal(x);
                return bx.signum() > 0;
            }

            public bool isNeg(object x)
            {
                BigDecimal bx = toBigDecimal(x);
                return bx.signum() < 0;
            }

            public object add(object x, object y)
            {
                return toBigDecimal(x).add(toBigDecimal(y));
            }

            public object multiply(object x, object y)
            {
                return toBigDecimal(x).multiply(toBigDecimal(y));
            }

            // TODO: figure out what the rounding mode should be
            public object divide(object x, object y)
            {
                return toBigDecimal(x).divide(toBigDecimal(y), BigDecimal.ROUND_HALF_EVEN);
            }

            // TODO: this is  just plain wrong;
            // Java version uses .divideToIntegralValue
            public object quotient(object x, object y)
            {
                return toBigDecimal(x).divide(toBigDecimal(y), 0);
            }

            // TODO: this is  just plain wrong;
            // Java version uses .remainder
            public object remainder(object x, object y)
            {
                return toBigDecimal(x).divide(toBigDecimal(y), 0);
            }

            public bool equiv(object x, object y)
            {
                return toBigDecimal(x).Equals(toBigDecimal(y));
            }

            public bool lt(object x, object y)
            {
                return toBigDecimal(x).compareTo(toBigDecimal(y)) < 0;
            }

            public object negate(object x)
            {
                return toBigDecimal(x).negate();
            }

            public object inc(object x)
            {
                BigDecimal bx = toBigDecimal(x);
                return bx.add(BigDecimalOne);
            }

            public object dec(object x)
            {
                BigDecimal bx = toBigDecimal(x);
                return bx.subtract(BigDecimalOne);
            }

            #endregion
        }

        class IntegerBitOps : BitOps
        {
            #region BitOps Members

            public BitOps combine(BitOps y)
            {
                return y.bitOpsWith(this); ;
            }

            public BitOps bitOpsWith(IntegerBitOps x)
            {
                return this;
            }

            public BitOps bitOpsWith(LongBitOps x)
            {
                return LONG_BITOPS;
            }

            public BitOps bitOpsWith(BigIntegerBitOps x)
            {
                return BIGINTEGER_BITOPS;
            }

            public object not(object x)
            {
                return ~Convert.ToInt32(x);
            }

            public object and(object x, object y)
            {
                return Convert.ToInt32(x) & Convert.ToInt32(y);
            }

            public object or(object x, object y)
            {
                return Convert.ToInt32(x) | Convert.ToInt32(y);
            }

            public object xor(object x, object y)
            {
                return Convert.ToInt32(x) ^ Convert.ToInt32(y);
            }

            public object andNot(object x, object y)
            {
                return Convert.ToInt32(x) & ~Convert.ToInt32(y);
            }

            public object clearBit(object x, int n)
            {
                if (n < 31)
                    return Convert.ToInt32(x) & ~(1 << n);
                else if (n < 63)
                    return Convert.ToInt64(x) & ~(1L << n);
                else
                    return toBigInteger(x).clearBit(n);
            }

            public object setBit(object x, int n)
            {
                if (n < 31)
                    return Convert.ToInt32(x) | (1 << n);
                else if (n < 63)
                    return Convert.ToInt64(x) | (1L << n);
                else
                    return toBigInteger(x).setBit(n);
            }

            public object flipBit(object x, int n)
            {
                if (n < 31)
                    return Convert.ToInt32(x) ^ (1 << n);
                else if (n < 63)
                    return Convert.ToInt64(x) ^ (1L << n);
                else
                    return toBigInteger(x).flipBit(n);
            }

            public bool testBit(object x, int n)
            {
                if (n < 31)
                    return (Convert.ToInt32(x) & (1 << n)) != 0;
                else if (n < 63)
                    return (Convert.ToInt64(x) & (1L << n)) != 0;
                else
                    return toBigInteger(x).testBit(n);
            }

            public object shiftLeft(object x, int n)
            {
                if (n < 32)
                    return (n < 0)
                        ? shiftRight(x, -n)
                        : reduce(Convert.ToInt64(x) << n);
                else
                    return reduce(toBigInteger(x).shiftLeft(n));
            }

            public object shiftRight(object x, int n)
            {
                return (n < 0)
                   ? shiftLeft(x, -n)
                   : Convert.ToInt32(x) >> n;
            }

            #endregion
        }

        class LongBitOps : BitOps
        {
            #region BitOps Members

            public BitOps combine(BitOps y)
            {
                return y.bitOpsWith(this);
            }

            public BitOps bitOpsWith(IntegerBitOps x)
            {
                return this;
            }

            public BitOps bitOpsWith(LongBitOps x)
            {
                return this;
            }

            public BitOps bitOpsWith(BigIntegerBitOps x)
            {
                return BIGINTEGER_BITOPS;
            }

            public object not(object x)
            {
                return ~Convert.ToInt64(x);
            }

            public object and(object x, object y)
            {
                return Convert.ToInt64(x) & Convert.ToInt64(y);
            }

            public object or(object x, object y)
            {
                return Convert.ToInt64(x) | Convert.ToInt64(y);
            }

            public object xor(object x, object y)
            {
                return Convert.ToInt64(x) ^ Convert.ToInt64(y);
            }

            public object andNot(object x, object y)
            {
                return Convert.ToInt64(x) & ~Convert.ToInt64(y);
            }

            public object clearBit(object x, int n)
            {
                if (n < 63)
                    return Convert.ToInt64(x) & ~(1L << n);
                else
                    return toBigInteger(x).clearBit(n);
            }

            public object setBit(object x, int n)
            {
                if (n < 63)
                    return Convert.ToInt64(x) | (1L << n);
                else
                    return toBigInteger(x).setBit(n);
            }

            public object flipBit(object x, int n)
            {
                if (n < 63)
                    return Convert.ToInt64(x) ^ (1L << n);
                else
                    return toBigInteger(x).flipBit(n);
            }

            public bool testBit(object x, int n)
            {
                if (n < 63)
                    return (Convert.ToInt64(x) & (1L << n)) != 0;
                else
                    return toBigInteger(x).testBit(n);
            }

            public object shiftLeft(object x, int n)
            {
                return n < 0
                    ? shiftRight(x,-n)
                    : reduce(toBigInteger(x).shiftLeft(n));
            }

            public object shiftRight(object x, int n)
            {
                return n < 0
                     ? shiftLeft(x, -n)
                     : Convert.ToInt64(x) >> n;
            }

            #endregion
        }

        class BigIntegerBitOps : BitOps
        {
            #region BitOps Members

            public BitOps combine(BitOps y)
            {
                return y.bitOpsWith(this);
            }

            public BitOps bitOpsWith(IntegerBitOps x)
            {
                return this;
            }

            public BitOps bitOpsWith(LongBitOps x)
            {
                return this;
            }

            public BitOps bitOpsWith(BigIntegerBitOps x)
            {
                return this;
            }

            public object not(object x)
            {
                return toBigInteger(x).not();
            }

            public object and(object x, object y)
            {
                return toBigInteger(x).and(toBigInteger(y));
            }

            public object or(object x, object y)
            {
                return toBigInteger(x).or(toBigInteger(y));
            }

            public object xor(object x, object y)
            {
                return toBigInteger(x).xor(toBigInteger(y));
            }

            public object andNot(object x, object y)
            {
                return toBigInteger(x).andNot(toBigInteger(y));
            }

            public object clearBit(object x, int n)
            {
                return toBigInteger(x).clearBit(n);
            }

            public object setBit(object x, int n)
            {
                return toBigInteger(x).setBit(n);
            }

            public object flipBit(object x, int n)
            {
                return toBigInteger(x).flipBit(n);
            }

            public bool testBit(object x, int n)
            {
                return toBigInteger(x).testBit(n);
            }

            public object shiftLeft(object x, int n)
            {
                return toBigInteger(x).shiftLeft(n);
            }

            public object shiftRight(object x, int n)
            {
                return toBigInteger(x).shiftRight(n);
            }

            #endregion
        }
        
        #region Array c-tors

        static public float[] float_array(int size, object init)
        {
            float[] ret = new float[size];
            if (Util.IsNumeric(init))
            {
                float f = Util.ConvertToFloat(init);
                for (int i = 0; i < ret.Length; i++)
                    ret[i] = f;
            }
            else
            {
                ISeq s = RT.seq(init);
                for (int i = 0; i < size && s != null; i++, s = s.next())
                    ret[i] = Util.ConvertToFloat(s.first());
            }
            return ret;
        }

        static public float[] float_array(Object sizeOrSeq)
        {
            if (Util.IsNumeric(sizeOrSeq))
                return new float[Util.ConvertToInt(sizeOrSeq)];
            else
            {
                ISeq s = RT.seq(sizeOrSeq);
                int size = s.count();
                float[] ret = new float[size];
                for (int i = 0; i < size && s != null; i++, s = s.next())
                    ret[i] = Util.ConvertToFloat(s.first());
                return ret;
            }
        }

        static public double[] double_array(int size, Object init)
        {
            double[] ret = new double[size];
            if (Util.IsNumeric(init))
            {
                double f = Util.ConvertToDouble(init);
                for (int i = 0; i < ret.Length; i++)
                    ret[i] = f;
            }
            else
            {
                ISeq s = RT.seq(init);
                for (int i = 0; i < size && s != null; i++, s = s.next())
                    ret[i] = Util.ConvertToDouble(s.first());
            }
            return ret;
        }

        static public double[] double_array(Object sizeOrSeq)
        {
            if (Util.IsNumeric(sizeOrSeq))
                return new double[Util.ConvertToInt(sizeOrSeq)];
            else
            {
                ISeq s = RT.seq(sizeOrSeq);
                int size = s.count();
                double[] ret = new double[size];
                for (int i = 0; i < size && s != null; i++, s = s.next())
                    ret[i] = Util.ConvertToDouble(s.first());
                return ret;
            }
        }

        static public int[] int_array(int size, Object init)
        {
            int[] ret = new int[size];
            if (Util.IsNumeric(init))
            {
                int f = Util.ConvertToInt(init);
                for (int i = 0; i < ret.Length; i++)
                    ret[i] = f;
            }
            else
            {
                ISeq s = RT.seq(init);
                for (int i = 0; i < size && s != null; i++, s = s.next())
                    ret[i] = Util.ConvertToInt(s.first());
            }
            return ret;
        }

        static public int[] int_array(Object sizeOrSeq)
        {
            if (Util.IsNumeric(sizeOrSeq))
                return new int[Util.ConvertToInt(sizeOrSeq)];
            else
            {
                ISeq s = RT.seq(sizeOrSeq);
                int size = s.count();
                int[] ret = new int[size];
                for (int i = 0; i < size && s != null; i++, s = s.next())
                    ret[i] = Util.ConvertToInt(s.first());
                return ret;
            }
        }

        static public long[] long_array(int size, Object init)
        {
            long[] ret = new long[size];
            if (Util.IsNumeric(init))
            {
                long f = Util.ConvertToLong(init);
                for (int i = 0; i < ret.Length; i++)
                    ret[i] = f;
            }
            else
            {
                ISeq s = RT.seq(init);
                for (int i = 0; i < size && s != null; i++, s = s.next())
                    ret[i] = Util.ConvertToLong(s.first());
            }
            return ret;
        }

        static public long[] long_array(Object sizeOrSeq)
        {
            if (Util.IsNumeric(sizeOrSeq))
                return new long[Util.ConvertToInt(sizeOrSeq)];
            else
            {
                ISeq s = RT.seq(sizeOrSeq);
                int size = s.count();
                long[] ret = new long[size];
                for (int i = 0; i < size && s != null; i++, s = s.next())
                    ret[i] = Util.ConvertToLong(s.first());
                return ret;
            }
        }

        static public float[] floats(Object array)
        {
            return (float[])array;
        }

        static public double[] doubles(Object array)
        {
            return (double[])array;
        }

        static public int[] ints(Object array)
        {
            return (int[])array;
        }

        static public long[] longs(Object array)
        {
            return (long[])array;
        }

        #endregion

        #region Float overloads for basic ops

        static public float add(float x, float y)
        {
            return x + y;
        }

        static public float minus(float x, float y)
        {
            return x - y;
        }

        static public float minus(float x)
        {
            return -x;
        }

        static public float inc(float x)
        {
            return x + 1;
        }

        static public float dec(float x)
        {
            return x - 1;
        }

        static public float multiply(float x, float y)
        {
            return x * y;
        }

        static public float divide(float x, float y)
        {
            return x / y;
        }

        static public bool equiv(float x, float y)
        {
            return x == y;
        }

        static public bool lt(float x, float y)
        {
            return x < y;
        }

        static public bool lte(float x, float y)
        {
            return x <= y;
        }

        static public bool gt(float x, float y)
        {
            return x > y;
        }

        static public bool gte(float x, float y)
        {
            return x >= y;
        }

        static public bool isPos(float x)
        {
            return x > 0;
        }

        static public bool isNeg(float x)
        {
            return x < 0;
        }

        static public bool isZero(float x)
        {
            return x == 0;
        }

        #endregion

        #region Double overloads for basic ops

        static public double add(double x, double y)
        {
            return x + y;
        }

        static public double minus(double x, double y)
        {
            return x - y;
        }

        static public double minus(double x)
        {
            return -x;
        }

        static public double inc(double x)
        {
            return x + 1;
        }

        static public double dec(double x)
        {
            return x - 1;
        }

        static public double multiply(double x, double y)
        {
            return x * y;
        }

        static public double divide(double x, double y)
        {
            return x / y;
        }

        static public bool equiv(double x, double y)
        {
            return x == y;
        }

        static public bool lt(double x, double y)
        {
            return x < y;
        }

        static public bool lte(double x, double y)
        {
            return x <= y;
        }

        static public bool gt(double x, double y)
        {
            return x > y;
        }

        static public bool gte(double x, double y)
        {
            return x >= y;
        }

        static public bool isPos(double x)
        {
            return x > 0;
        }

        static public bool isNeg(double x)
        {
            return x < 0;
        }

        static public bool isZero(double x)
        {
            return x == 0;
        }

        #endregion

        #region Int overloads for basic ops

        static int throwIntOverflow()
        {
            throw new ArithmeticException("integer overflow");
        }

        static public int unchecked_add(int x, int y)
        {
            return x + y;
        }

        static public int unchecked_subtract(int x, int y)
        {
            return x - y;
        }

        static public int unchecked_negate(int x)
        {
            return -x;
        }

        static public int unchecked_inc(int x)
        {
            return x + 1;
        }

        static public int unchecked_dec(int x)
        {
            return x - 1;
        }

        static public int unchecked_multiply(int x, int y)
        {
            return x * y;
        }

        static public int add(int x, int y)
        {
            int ret = x + y;
            if ((ret ^ x) < 0 && (ret ^ y) < 0)
                return throwIntOverflow();
            return ret;
        }

        static public int not(int x)
        {
            return ~x;
        }

        static public int and(int x, int y)
        {
            return x & y;
        }

        static public int or(int x, int y)
        {
            return x | y;
        }

        static public int xor(int x, int y)
        {
            return x ^ y;
        }

        static public int minus(int x, int y)
        {
            int ret = x - y;
            if (((ret ^ x) < 0 && (ret ^ ~y) < 0))
                return throwIntOverflow();
            return ret;
        }

        static public int minus(int x)
        {
            if (x == Int32.MinValue)
                return throwIntOverflow();
            return -x;
        }

        static public int inc(int x)
        {
            if (x == Int32.MaxValue)
                return throwIntOverflow();
            return x + 1;
        }

        static public int dec(int x)
        {
            if (x == Int32.MinValue)
                return throwIntOverflow();
            return x - 1;
        }

        static public int multiply(int x, int y)
        {
            int ret = x * y;
            if (y != 0 && ret / y != x)
                return throwIntOverflow();
            return ret;
        }

        static public int unchecked_divide(int x, int y)
        {
            return x / y;
        }

        static public int unchecked_remainder(int x, int y)
        {
            return x % y;
        }

        static public bool equiv(int x, int y)
        {
            return x == y;
        }

        static public bool lt(int x, int y)
        {
            return x < y;
        }

        static public bool lte(int x, int y)
        {
            return x <= y;
        }

        static public bool gt(int x, int y)
        {
            return x > y;
        }

        static public bool gte(int x, int y)
        {
            return x >= y;
        }

        static public bool isPos(int x)
        {
            return x > 0;
        }

        static public bool isNeg(int x)
        {
            return x < 0;
        }

        static public bool isZero(int x)
        {
            return x == 0;
        }

        #endregion

        #region Long overloads for basic ops

        static public long unchecked_add(long x, long y)
        {
            return x + y;
        }

        static public long unchecked_subtract(long x, long y)
        {
            return x - y;
        }

        static public long unchecked_negate(long x)
        {
            return -x;
        }

        static public long unchecked_inc(long x)
        {
            return x + 1;
        }

        static public long unchecked_dec(long x)
        {
            return x - 1;
        }

        static public long unchecked_multiply(long x, long y)
        {
            return x * y;
        }

        static public long add(long x, long y)
        {
            long ret = x + y;
            if ((ret ^ x) < 0 && (ret ^ y) < 0)
                return throwIntOverflow();
            return ret;
        }

        static public long minus(long x, long y)
        {
            long ret = x - y;
            if (((ret ^ x) < 0 && (ret ^ ~y) < 0))
                return throwIntOverflow();
            return ret;
        }

        static public long minus(long x)
        {
            if (x == Int64.MinValue)
                return throwIntOverflow();
            return -x;
        }

        static public long inc(long x)
        {
            if (x == Int64.MaxValue)
                return throwIntOverflow();
            return x + 1;
        }

        static public long dec(long x)
        {
            if (x == Int64.MinValue)
                return throwIntOverflow();
            return x - 1;
        }

        static public long multiply(long x, long y)
        {
            long ret = x * y;
            if (y != 0 && ret / y != x)
                return throwIntOverflow();
            return ret;
        }

        static public long unchecked_divide(long x, long y)
        {
            return x / y;
        }

        static public long unchecked_remainder(long x, long y)
        {
            return x % y;
        }

        static public bool equiv(long x, long y)
        {
            return x == y;
        }

        static public bool lt(long x, long y)
        {
            return x < y;
        }

        static public bool lte(long x, long y)
        {
            return x <= y;
        }

        static public bool gt(long x, long y)
        {
            return x > y;
        }

        static public bool gte(long x, long y)
        {
            return x >= y;
        }

        static public bool isPos(long x)
        {
            return x > 0;
        }

        static public bool isNeg(long x)
        {
            return x < 0;
        }

        static public bool isZero(long x)
        {
            return x == 0;
        }

        #endregion

        #region Overload resolution

        static public object add(int x, Object y)
        {
            return add((Object)x, y);
        }

        static public object add(Object x, int y)
        {
            return add(x, (Object)y);
        }

        static public object and(int x, Object y)
        {
            return and((Object)x, y);
        }

        static public object and(Object x, int y)
        {
            return and(x, (Object)y);
        }

        static public object or(int x, Object y)
        {
            return or((Object)x, y);
        }

        static public object or(Object x, int y)
        {
            return or(x, (Object)y);
        }

        static public object xor(int x, Object y)
        {
            return xor((Object)x, y);
        }

        static public object xor(Object x, int y)
        {
            return xor(x, (Object)y);
        }

        static public object add(float x, Object y)
        {
            return add((Object)x, y);
        }

        static public object add(Object x, float y)
        {
            return add(x, (Object)y);
        }

        static public object add(long x, Object y)
        {
            return add((Object)x, y);
        }

        static public object add(Object x, long y)
        {
            return add(x, (Object)y);
        }

        static public object add(double x, Object y)
        {
            return add((Object)x, y);
        }

        static public object add(Object x, double y)
        {
            return add(x, (Object)y);
        }

        static public object minus(int x, Object y)
        {
            return minus((Object)x, y);
        }

        static public object minus(Object x, int y)
        {
            return minus(x, (Object)y);
        }

        static public object minus(float x, Object y)
        {
            return minus((Object)x, y);
        }

        static public object minus(Object x, float y)
        {
            return minus(x, (Object)y);
        }

        static public object minus(long x, Object y)
        {
            return minus((Object)x, y);
        }

        static public object minus(Object x, long y)
        {
            return minus(x, (Object)y);
        }

        static public object minus(double x, Object y)
        {
            return minus((Object)x, y);
        }

        static public object minus(Object x, double y)
        {
            return minus(x, (Object)y);
        }

        static public object multiply(int x, Object y)
        {
            return multiply((Object)x, y);
        }

        static public object multiply(Object x, int y)
        {
            return multiply(x, (Object)y);
        }

        static public object multiply(float x, Object y)
        {
            return multiply((Object)x, y);
        }

        static public object multiply(Object x, float y)
        {
            return multiply(x, (Object)y);
        }

        static public object multiply(long x, Object y)
        {
            return multiply((Object)x, y);
        }

        static public object multiply(Object x, long y)
        {
            return multiply(x, (Object)y);
        }

        static public object multiply(double x, Object y)
        {
            return multiply((Object)x, y);
        }

        static public object multiply(Object x, double y)
        {
            return multiply(x, (Object)y);
        }

        static public object divide(int x, Object y)
        {
            return divide((Object)x, y);
        }

        static public object divide(Object x, int y)
        {
            return divide(x, (Object)y);
        }

        static public object divide(float x, Object y)
        {
            return divide((Object)x, y);
        }

        static public object divide(Object x, float y)
        {
            return divide(x, (Object)y);
        }

        static public object divide(long x, Object y)
        {
            return divide((Object)x, y);
        }

        static public object divide(Object x, long y)
        {
            return divide(x, (Object)y);
        }

        static public object divide(double x, Object y)
        {
            return divide((Object)x, y);
        }

        static public object divide(Object x, double y)
        {
            return divide(x, (Object)y);
        }

        static public bool lt(int x, Object y)
        {
            return lt((Object)x, y);
        }

        static public bool lt(Object x, int y)
        {
            return lt(x, (Object)y);
        }

        static public bool lt(float x, Object y)
        {
            return lt((Object)x, y);
        }

        static public bool lt(Object x, float y)
        {
            return lt(x, (Object)y);
        }

        static public bool lt(long x, Object y)
        {
            return lt((Object)x, y);
        }

        static public bool lt(Object x, long y)
        {
            return lt(x, (Object)y);
        }

        static public bool lt(double x, Object y)
        {
            return lt((Object)x, y);
        }

        static public bool lt(Object x, double y)
        {
            return lt(x, (Object)y);
        }

        static public bool lte(int x, Object y)
        {
            return lte((Object)x, y);
        }

        static public bool lte(Object x, int y)
        {
            return lte(x, (Object)y);
        }

        static public bool lte(float x, Object y)
        {
            return lte((Object)x, y);
        }

        static public bool lte(Object x, float y)
        {
            return lte(x, (Object)y);
        }

        static public bool lte(long x, Object y)
        {
            return lte((Object)x, y);
        }

        static public bool lte(Object x, long y)
        {
            return lte(x, (Object)y);
        }

        static public bool lte(double x, Object y)
        {
            return lte((Object)x, y);
        }

        static public bool lte(Object x, double y)
        {
            return lte(x, (Object)y);
        }

        static public bool gt(int x, Object y)
        {
            return gt((Object)x, y);
        }

        static public bool gt(Object x, int y)
        {
            return gt(x, (Object)y);
        }

        static public bool gt(float x, Object y)
        {
            return gt((Object)x, y);
        }

        static public bool gt(Object x, float y)
        {
            return gt(x, (Object)y);
        }

        static public bool gt(long x, Object y)
        {
            return gt((Object)x, y);
        }

        static public bool gt(Object x, long y)
        {
            return gt(x, (Object)y);
        }

        static public bool gt(double x, Object y)
        {
            return gt((Object)x, y);
        }

        static public bool gt(Object x, double y)
        {
            return gt(x, (Object)y);
        }

        static public bool gte(int x, Object y)
        {
            return gte((Object)x, y);
        }

        static public bool gte(Object x, int y)
        {
            return gte(x, (Object)y);
        }

        static public bool gte(float x, Object y)
        {
            return gte((Object)x, y);
        }

        static public bool gte(Object x, float y)
        {
            return gte(x, (Object)y);
        }

        static public bool gte(long x, Object y)
        {
            return gte((Object)x, y);
        }

        static public bool gte(Object x, long y)
        {
            return gte(x, (Object)y);
        }

        static public bool gte(double x, Object y)
        {
            return gte((Object)x, y);
        }

        static public bool gte(Object x, double y)
        {
            return gte(x, (Object)y);
        }


        static public bool equiv(int x, Object y)
        {
            return equiv((Object)x, y);
        }

        static public bool equiv(Object x, int y)
        {
            return equiv(x, (Object)y);
        }

        static public bool equiv(float x, Object y)
        {
            return equiv((Object)x, y);
        }

        static public bool equiv(Object x, float y)
        {
            return equiv(x, (Object)y);
        }

        static public bool equiv(long x, Object y)
        {
            return equiv((Object)x, y);
        }

        static public bool equiv(Object x, long y)
        {
            return equiv(x, (Object)y);
        }

        static public bool equiv(double x, Object y)
        {
            return equiv((Object)x, y);
        }

        static public bool equiv(Object x, double y)
        {
            return equiv(x, (Object)y);
        }


        static public float add(int x, float y)
        {
            return add((float)x, y);
        }

        static public float add(float x, int y)
        {
            return add(x, (float)y);
        }

        static public double add(int x, double y)
        {
            return add((double)x, y);
        }

        static public double add(double x, int y)
        {
            return add(x, (double)y);
        }

        static public long add(int x, long y)
        {
            return add((long)x, y);
        }

        static public long add(long x, int y)
        {
            return add(x, (long)y);
        }

        static public float add(long x, float y)
        {
            return add((float)x, y);
        }

        static public float add(float x, long y)
        {
            return add(x, (float)y);
        }

        static public double add(long x, double y)
        {
            return add((double)x, y);
        }

        static public double add(double x, long y)
        {
            return add(x, (double)y);
        }

        static public double add(float x, double y)
        {
            return add((double)x, y);
        }

        static public double add(double x, float y)
        {
            return add(x, (double)y);
        }

        static public float minus(int x, float y)
        {
            return minus((float)x, y);
        }

        static public float minus(float x, int y)
        {
            return minus(x, (float)y);
        }

        static public double minus(int x, double y)
        {
            return minus((double)x, y);
        }

        static public double minus(double x, int y)
        {
            return minus(x, (double)y);
        }

        static public long minus(int x, long y)
        {
            return minus((long)x, y);
        }

        static public long minus(long x, int y)
        {
            return minus(x, (long)y);
        }

        static public float minus(long x, float y)
        {
            return minus((float)x, y);
        }

        static public float minus(float x, long y)
        {
            return minus(x, (float)y);
        }

        static public double minus(long x, double y)
        {
            return minus((double)x, y);
        }

        static public double minus(double x, long y)
        {
            return minus(x, (double)y);
        }

        static public double minus(float x, double y)
        {
            return minus((double)x, y);
        }

        static public double minus(double x, float y)
        {
            return minus(x, (double)y);
        }

        static public float multiply(int x, float y)
        {
            return multiply((float)x, y);
        }

        static public float multiply(float x, int y)
        {
            return multiply(x, (float)y);
        }

        static public double multiply(int x, double y)
        {
            return multiply((double)x, y);
        }

        static public double multiply(double x, int y)
        {
            return multiply(x, (double)y);
        }

        static public long multiply(int x, long y)
        {
            return multiply((long)x, y);
        }

        static public long multiply(long x, int y)
        {
            return multiply(x, (long)y);
        }

        static public float multiply(long x, float y)
        {
            return multiply((float)x, y);
        }

        static public float multiply(float x, long y)
        {
            return multiply(x, (float)y);
        }

        static public double multiply(long x, double y)
        {
            return multiply((double)x, y);
        }

        static public double multiply(double x, long y)
        {
            return multiply(x, (double)y);
        }

        static public double multiply(float x, double y)
        {
            return multiply((double)x, y);
        }

        static public double multiply(double x, float y)
        {
            return multiply(x, (double)y);
        }

        static public float divide(int x, float y)
        {
            return divide((float)x, y);
        }

        static public float divide(float x, int y)
        {
            return divide(x, (float)y);
        }

        static public double divide(int x, double y)
        {
            return divide((double)x, y);
        }

        static public double divide(double x, int y)
        {
            return divide(x, (double)y);
        }

        static public float divide(long x, float y)
        {
            return divide((float)x, y);
        }

        static public float divide(float x, long y)
        {
            return divide(x, (float)y);
        }

        static public double divide(long x, double y)
        {
            return divide((double)x, y);
        }

        static public double divide(double x, long y)
        {
            return divide(x, (double)y);
        }

        static public double divide(float x, double y)
        {
            return divide((double)x, y);
        }

        static public double divide(double x, float y)
        {
            return divide(x, (double)y);
        }

        static public bool lt(int x, float y)
        {
            return lt((float)x, y);
        }

        static public bool lt(float x, int y)
        {
            return lt(x, (float)y);
        }

        static public bool lt(int x, double y)
        {
            return lt((double)x, y);
        }

        static public bool lt(double x, int y)
        {
            return lt(x, (double)y);
        }

        static public bool lt(int x, long y)
        {
            return lt((long)x, y);
        }

        static public bool lt(long x, int y)
        {
            return lt(x, (long)y);
        }

        static public bool lt(long x, float y)
        {
            return lt((float)x, y);
        }

        static public bool lt(float x, long y)
        {
            return lt(x, (float)y);
        }

        static public bool lt(long x, double y)
        {
            return lt((double)x, y);
        }

        static public bool lt(double x, long y)
        {
            return lt(x, (double)y);
        }

        static public bool lt(float x, double y)
        {
            return lt((double)x, y);
        }

        static public bool lt(double x, float y)
        {
            return lt(x, (double)y);
        }


        static public bool lte(int x, float y)
        {
            return lte((float)x, y);
        }

        static public bool lte(float x, int y)
        {
            return lte(x, (float)y);
        }

        static public bool lte(int x, double y)
        {
            return lte((double)x, y);
        }

        static public bool lte(double x, int y)
        {
            return lte(x, (double)y);
        }

        static public bool lte(int x, long y)
        {
            return lte((long)x, y);
        }

        static public bool lte(long x, int y)
        {
            return lte(x, (long)y);
        }

        static public bool lte(long x, float y)
        {
            return lte((float)x, y);
        }

        static public bool lte(float x, long y)
        {
            return lte(x, (float)y);
        }

        static public bool lte(long x, double y)
        {
            return lte((double)x, y);
        }

        static public bool lte(double x, long y)
        {
            return lte(x, (double)y);
        }

        static public bool lte(float x, double y)
        {
            return lte((double)x, y);
        }

        static public bool lte(double x, float y)
        {
            return lte(x, (double)y);
        }

        static public bool gt(int x, float y)
        {
            return gt((float)x, y);
        }

        static public bool gt(float x, int y)
        {
            return gt(x, (float)y);
        }

        static public bool gt(int x, double y)
        {
            return gt((double)x, y);
        }

        static public bool gt(double x, int y)
        {
            return gt(x, (double)y);
        }

        static public bool gt(int x, long y)
        {
            return gt((long)x, y);
        }

        static public bool gt(long x, int y)
        {
            return gt(x, (long)y);
        }

        static public bool gt(long x, float y)
        {
            return gt((float)x, y);
        }

        static public bool gt(float x, long y)
        {
            return gt(x, (float)y);
        }

        static public bool gt(long x, double y)
        {
            return gt((double)x, y);
        }

        static public bool gt(double x, long y)
        {
            return gt(x, (double)y);
        }

        static public bool gt(float x, double y)
        {
            return gt((double)x, y);
        }

        static public bool gt(double x, float y)
        {
            return gt(x, (double)y);
        }

        static public bool gte(int x, float y)
        {
            return gte((float)x, y);
        }

        static public bool gte(float x, int y)
        {
            return gte(x, (float)y);
        }

        static public bool gte(int x, double y)
        {
            return gte((double)x, y);
        }

        static public bool gte(double x, int y)
        {
            return gte(x, (double)y);
        }

        static public bool gte(int x, long y)
        {
            return gte((long)x, y);
        }

        static public bool gte(long x, int y)
        {
            return gte(x, (long)y);
        }

        static public bool gte(long x, float y)
        {
            return gte((float)x, y);
        }

        static public bool gte(float x, long y)
        {
            return gte(x, (float)y);
        }

        static public bool gte(long x, double y)
        {
            return gte((double)x, y);
        }

        static public bool gte(double x, long y)
        {
            return gte(x, (double)y);
        }

        static public bool gte(float x, double y)
        {
            return gte((double)x, y);
        }

        static public bool gte(double x, float y)
        {
            return gte(x, (double)y);
        }

        static public bool equiv(int x, float y)
        {
            return equiv((float)x, y);
        }

        static public bool equiv(float x, int y)
        {
            return equiv(x, (float)y);
        }

        static public bool equiv(int x, double y)
        {
            return equiv((double)x, y);
        }

        static public bool equiv(double x, int y)
        {
            return equiv(x, (double)y);
        }

        static public bool equiv(int x, long y)
        {
            return equiv((long)x, y);
        }

        static public bool equiv(long x, int y)
        {
            return equiv(x, (long)y);
        }

        static public bool equiv(long x, float y)
        {
            return equiv((float)x, y);
        }

        static public bool equiv(float x, long y)
        {
            return equiv(x, (float)y);
        }

        static public bool equiv(long x, double y)
        {
            return equiv((double)x, y);
        }

        static public bool equiv(double x, long y)
        {
            return equiv(x, (double)y);
        }

        static public bool equiv(float x, double y)
        {
            return equiv((double)x, y);
        }

        static public bool equiv(double x, float y)
        {
            return equiv(x, (double)y);
        }


        #endregion

    }
}
