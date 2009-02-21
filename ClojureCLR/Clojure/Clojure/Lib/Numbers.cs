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
                return BigInteger.valueOf(Convert.ToInt64(x));
        }

        static BigDecimal toBigDecimal(object x)
        {
            if (x is BigDecimal)
                return (BigDecimal)x;
            else if ( x is BigInteger)
                return new BigDecimal((BigInteger)x);
            else
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
            //return (val.bitLength() < 32) ? (object)val.intValue() : val;
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

        #region Unchecked arithmetic


        public static int unchecked_add(int x, int y)
        {
            return x + y;
        }

        public static int unchecked_subtract(int x, int y)
        {
            return x - y;
        }

        public static int unchecked_negate(int x)
        {
            return -x;
        }

        public static int unchecked_inc(int x)
        {
            return x + 1;
        }

        public static int unchecked_dec(int x)
        {
            return x - 1;
        }

        public static int unchecked_multiply(int x, int y)
        {
            return x * y;
        }

        public static int unchecked_divide(int x, int y)
        {
            return x / y;
        }

        public static int unchecked_remainder(int x, int y)
        {
            return x % y;
        }


        public static long unchecked_add(long x, long y)
        {
            return x + y;
        }

        public static long unchecked_subtract(long x, long y)
        {
            return x - y;
        }

        public static long unchecked_negate(long x)
        {
            return -x;
        }

        public static long unchecked_inc(long x)
        {
            return x + 1;
        }

        public static long unchecked_dec(long x)
        {
            return x - 1;
        }

        public static long unchecked_multiply(long x, long y)
        {
            return x * y;
        }

        public static long unchecked_divide(long x, long y)
        {
            return x / y;
        }

        public static long unchecked_remainder(long x, long y)
        {
            return x % y;
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

        class IntegerOps : Ops
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

        class LongOps : Ops
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
                return Convert.ToDouble(x) + 1;
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
                return r.Numerator.signum()== 0;
            }

            public bool isPos(object x)
            {
                Ratio r = toRatio(x);
                return r.Numerator.signum() > 0;
            }

            public bool isNeg(object x)
            {
                Ratio r = toRatio(x);
                return r.Numerator.signum() < 0;
            }

            public object add(object x, object y)
            {
                Ratio rx = toRatio(x);
                Ratio ry = toRatio(y);
                return divide(ry.Numerator.multiply(rx.Denominator)
                    .add(rx.Numerator.multiply(ry.Denominator)),
                    ry.Denominator.multiply(rx.Denominator));
            }

            public object multiply(object x, object y)
            {
                Ratio rx = toRatio(x);
                Ratio ry = toRatio(y);
                return Numbers.divide(ry.Numerator.multiply(rx.Numerator),
                    ry.Denominator.multiply(rx.Denominator));
            }

            public object divide(object x, object y)
            {
                Ratio rx = toRatio(x);
                Ratio ry = toRatio(y);
                return Numbers.divide(ry.Denominator.multiply(rx.Numerator),
                    ry.Numerator.multiply(rx.Denominator));
            }

            public object quotient(object x, object y)
            {
                Ratio rx = toRatio(x);
                Ratio ry = toRatio(y);
                BigInteger q = rx.Numerator.multiply(ry.Denominator)
                    .divide(rx.Denominator.multiply(ry.Numerator));
                return reduce(q);
            }

            public object remainder(object x, object y)
            {
                Ratio rx = toRatio(x);
                Ratio ry = toRatio(y);
                BigInteger q = rx.Numerator.multiply(ry.Denominator)
                    .divide(rx.Denominator.multiply(ry.Numerator));
                return Numbers.minus(x, Numbers.multiply(q, y));
            }

            public bool equiv(object x, object y)
            {
                Ratio rx = toRatio(x);
                Ratio ry = toRatio(y);
                return rx.Numerator.Equals(ry.Numerator)
                    && rx.Denominator.Equals(ry.Denominator);
            }

            public bool lt(object x, object y)
            {
                Ratio rx = toRatio(x);
                Ratio ry = toRatio(y);
                return Numbers.lt(rx.Numerator.multiply(ry.Denominator),
                    ry.Numerator.multiply(rx.Denominator));
            }

            public object negate(object x)
            {
                Ratio rx = toRatio(x);
                return new Ratio(rx.Numerator.negate(), rx.Denominator);
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

            // TODO: fiture out what the rounding mode should be
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
       
    }
}
