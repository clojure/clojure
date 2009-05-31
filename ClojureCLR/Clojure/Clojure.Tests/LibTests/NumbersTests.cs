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

using NUnit.Framework;
using Rhino.Mocks;

using clojure.lang;
using java.math;

namespace Clojure.Tests.LibTests
{
    [TestFixture]
    public class NumbersTests : AssertionHelper
    {
        #region Helpers

        private void ExpectInt32(object x)
        {
            Expect(x, TypeOf(typeof(Int32)));
        }

        private void ExpectSameObject(object x, object y)
        {
            Expect(x, SameAs(y));
        }

        private void ExpectEqualObject(object x, object y)
        {
            Expect(x, EqualTo(y));
        }

        #endregion

        #region reduce tests

        [Test]
        public void ReduceOnBigIntReducesSmallerValues()
        {
            BigInteger b1 = new BigInteger("123");
            BigInteger b2 = new BigInteger("0");
            BigInteger b3 = new BigInteger(Int32.MaxValue.ToString());
            BigInteger b4 = new BigInteger(Int32.MinValue.ToString());
                
            ExpectInt32(Numbers.reduce(b1));
            ExpectInt32(Numbers.reduce(b2));
            ExpectInt32(Numbers.reduce(b3));
            ExpectInt32(Numbers.reduce(b4));
        }

        [Test]
        public void ReduceOnBigIntReturnsLargerValues()
        {
            BigInteger b1 = new BigInteger("100000000000000000000", 16);
            BigInteger b2 = b1.negate();
            BigInteger b3 = new BigInteger("123456789012345678901234567890");
            BigInteger b4 = b3.negate();

            ExpectSameObject(b1, Numbers.reduce(b1));
            ExpectSameObject(b2, Numbers.reduce(b2));
            ExpectSameObject(b3, Numbers.reduce(b3));
            ExpectSameObject(b4, Numbers.reduce(b4));
        }

        [Test]
        public void ReduceOnLongReducesSmallerValues()
        {
            long b1 = 123;
            long b2 = 0;
            long b3 = Int32.MaxValue;
            long b4 = Int32.MinValue;

            ExpectInt32(Numbers.reduce(b1));
            ExpectInt32(Numbers.reduce(b2));
            ExpectInt32(Numbers.reduce(b3));
            ExpectInt32(Numbers.reduce(b4));
        }


        [Test]
        public void ReduceOnLongReturnsLargerValues()
        {
            long b1 = ((long)Int32.MaxValue) + 1;
            long b2 = ((long)Int32.MinValue) - 1;
            long b3 = 123456789000;
            long b4 = -b3;

            ExpectEqualObject(b1, Numbers.reduce(b1));
            ExpectEqualObject(b2, Numbers.reduce(b2));
            ExpectEqualObject(b3, Numbers.reduce(b3));
            ExpectEqualObject(b4, Numbers.reduce(b4));
        }

        #endregion

        #region divide tests

        [Test]
        [ExpectedException(typeof(ArithmeticException))]
        public void DivideByZeroFails()
        {
            object o = Numbers.BIDivide(Numbers.BigIntegerOne, Numbers.BigIntegerZero);
        }

        [Test]
        public void DivideReducesToIntOnDenomOne()
        {
            object o = Numbers.BIDivide(new BigInteger("75"), new BigInteger("25"));
            Expect(o, EqualTo(3));
        }

        [Test]
        public void DivideReturnsReducedRatio()
        {
            object o = Numbers.BIDivide(new BigInteger("42"), new BigInteger("30"));
            
            Expect(o, TypeOf(typeof(Ratio)));
            
            Ratio r = o as Ratio;
            Expect(r.numerator, EqualTo(new BigInteger("7")));
            Expect(r.denominator, EqualTo(new BigInteger("5")));
        }

        #endregion
    }
}
