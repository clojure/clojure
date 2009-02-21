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

using RMExpect = Rhino.Mocks.Expect;

namespace Clojure.Tests.LibTests
{
    [TestFixture]
    public class PersistentVectorTests : AssertionHelper
    {

        #region C-tor tests

        [Test]
        public void CreateOnISeqReturnsCorrectCount()
        {
            Range r = new Range(2,5);
            PersistentVector v = PersistentVector.create(r);

            Expect(v.count(),EqualTo(r.count()));
        }

        [Test]
        public void CreateOnISeqHasItems()
        {
            Range r = new Range(2, 5);
            PersistentVector v = PersistentVector.create(r);

            Expect(v.nth(0), EqualTo(2));
            Expect(v.nth(1), EqualTo(3));
            Expect(v.nth(2), EqualTo(4));
        }

        [Test]
        public void CreateOnISeqWithManyItemsWorks()
        {
            // Want to bust out of the first tail, so need to insert more than 32 elements.
            Range r = new Range(2, 1000);
            PersistentVector v = PersistentVector.create(r);

            Expect(v.count(), EqualTo(r.count()));
            for (int i = 0; i < v.count(); ++i)
                Expect(v.nth(i), EqualTo(i + 2));
        }

        [Test]
        public void CreateOnISeqWithManyManyItemsWorks()
        {
            // Want to bust out of the first tail, so need to insert more than 32 elements.
            // Let's get out of the second level, too.

            Range r = new Range(2, 100000);
            PersistentVector v = PersistentVector.create(r);

            Expect(v.count(), EqualTo(r.count()));
            for (int i = 0; i < v.count(); ++i)
                Expect(v.nth(i), EqualTo(i + 2));
        }

        [Test]
        public void CreateOnMultipleItemsWorks()
        {
            PersistentVector v = PersistentVector.create(2,3,4);

            Expect(v.count(),EqualTo(3));
            Expect(v.nth(0), EqualTo(2));
            Expect(v.nth(1), EqualTo(3));
            Expect(v.nth(2), EqualTo(4));
        }

        #endregion

        #region IPersistentVector tests


        // nth - tested in c-tor tests


        [Test]
        public void CountYieldsLength()
        {
            PersistentVector v = PersistentVector.create(1, 2, 3);

            Expect(v.length(), EqualTo(3));
        }

        [Test]
        [ExpectedException(typeof(IndexOutOfRangeException))]
        public void NthOutOfRangeLowFails()
        {
            PersistentVector v = PersistentVector.create(1, 2, 3);
            object obj = v.nth(-4);
        }

        [Test]
        [ExpectedException(typeof(IndexOutOfRangeException))]
        public void NthOutOfRangeHighFails()
        {
            PersistentVector v = PersistentVector.create(1, 2, 3);
            object obj = v.nth(4);
        }

        [Test]
        public void AssocNReplacesInRangeForSmall()
        {
            Range r = new Range(2, 5);
            PersistentVector v1 = PersistentVector.create(r);
            IPersistentVector v2 = v1.assocN(1,10);

            Expect(v1.nth(0), EqualTo(2));
            Expect(v1.nth(1), EqualTo(3));
            Expect(v1.nth(2), EqualTo(4));
            Expect(v1.count(), EqualTo(3));
            Expect(v2.nth(0), EqualTo(2));
            Expect(v2.nth(1), EqualTo(10));
            Expect(v2.nth(2), EqualTo(4));
            Expect(v2.count(), EqualTo(3));
        }

        [Test]
        public void AssocNAddsAtEndForSmall()
        {
            Range r = new Range(2, 5);
            PersistentVector v1 = PersistentVector.create(r);
            IPersistentVector v2 = v1.assocN(3, 10);

            Expect(v1.nth(0), EqualTo(2));
            Expect(v1.nth(1), EqualTo(3));
            Expect(v1.nth(2), EqualTo(4));
            Expect(v1.count(), EqualTo(3));
            Expect(v2.nth(0), EqualTo(2));
            Expect(v2.nth(1), EqualTo(3));
            Expect(v2.nth(2), EqualTo(4));
            Expect(v2.nth(3), EqualTo(10));
            Expect(v2.count(), EqualTo(4));
        }

        [Test]
        [ExpectedException(typeof(IndexOutOfRangeException))]
        public void AssocNOutOfRangeLowThrowsException()
        {
            Range r = new Range(2, 5);
            PersistentVector v1 = PersistentVector.create(r);
            IPersistentVector v2 = v1.assocN(-4, 10);
        }

        [Test]
        [ExpectedException(typeof(IndexOutOfRangeException))]
        public void AssocNOutOfRangeHighThrowsException()
        {
            Range r = new Range(2, 5);
            PersistentVector v1 = PersistentVector.create(r);
            IPersistentVector v2 = v1.assocN(4, 10);
        }

        [Test]
        public void AssocNAddsAtEndForEmpty()
        {
            PersistentVector v1 = PersistentVector.create();
            IPersistentVector v2 = v1.assocN(0, "abc");

            Expect(v1.count(), EqualTo(0));
            Expect(v2.count(), EqualTo(1));
            Expect(v2.nth(0), EqualTo("abc"));
        }

        [Test]
        public void AssocNChangesForBig()
        {
            Range r = new Range(2, 100000);
            PersistentVector v1 = PersistentVector.create(r);
            IPersistentVector v2 = v1;

            for (int i = 0; i < 110000; i++)
                v2 = v2.assocN(i, i + 20);

            for ( int i=0; i<v1.count(); ++i )
                Expect(v1.nth(i),EqualTo(i+2));

            for (int i = 0; i < v2.count(); ++i)
                Expect(v2.nth(i), EqualTo(i + 20));
        }

        [Test]
        public void ConsWorks()
        {
            PersistentVector v1 = PersistentVector.create(2,3,4);
            IPersistentVector v2 = v1;

            for (int i = 3; i < 100000; i++)
                v2 = v2.cons(i+2);

            Expect(v1.count(), EqualTo(3));
            Expect(v2.count(), EqualTo(100000));

            for (int i = 0; i < v2.count(); ++i)
                Expect(v2.nth(i), EqualTo(i + 2));
        }

        #endregion

        #region IPersistentCollection tests

        [Test]
        public void EmptyReturnsEmptyCollection()
        {
            PersistentVector v = PersistentVector.create(1, 2, 3);
            IPersistentCollection e = v.empty();

            Expect(e.count(), EqualTo(0));
        }

        [Test]
        public void EmptyCopiesMeta()
        {
            MockRepository mocks = new MockRepository();
            IPersistentMap meta = mocks.StrictMock<IPersistentMap>();
            mocks.ReplayAll();

            PersistentVector v1 = PersistentVector.create(1, 2, 3);
            IPersistentCollection e1 = v1.empty();

            PersistentVector v2 = (PersistentVector) v1.withMeta(meta);
            IPersistentCollection e2 = v2.empty();

            Expect(((IObj)e1).meta(), Null);
            Expect(((IObj)e2).meta(), SameAs(meta));

            mocks.VerifyAll();
        }


        #endregion

        #region IPersistentStack tests

        [Test]
        [ExpectedException(typeof(InvalidOperationException))]
        public void PopOnEmptyThrowsException()
        {
            PersistentVector v = PersistentVector.create();
            IPersistentStack s = v.pop();
        }

        [Test]
        public void PopOnSizeOneReturnsEmpty()
        {
            PersistentVector v = PersistentVector.create(1);
            IPersistentStack s = v.pop();

            Expect(s.count(), EqualTo(0));
        }

        [Test]
        public void PopOnSmallReturnsOneLess()
        {
            Range r = new Range(2, 20);
            PersistentVector v = PersistentVector.create(r);
            IPersistentStack s = v.pop();

            Expect(v.count(),EqualTo(r.count()));
            Expect(s.count(),EqualTo(v.count()-1));
        }

        [Test]
        public void PopOnBigWorks()
        {
            Range r = new Range(0, 100000);
            PersistentVector v = PersistentVector.create(r);
            IPersistentStack s = v;
            for (int i = 16; i < 100000; i++)
                s = s.pop();

            Expect(v.count(), EqualTo(r.count()));
            Expect(s.count(), EqualTo(16));
        }


        #endregion

        #region IFn tests

        #endregion
    }

    [TestFixture]
    public class PersistentVector_IObj_Tests : IObjTests
    {
        MockRepository _mocks;

        [SetUp]
        public void Setup()
        {
            _mocks = new MockRepository();
            IPersistentMap meta = _mocks.StrictMock<IPersistentMap>();
            _mocks.ReplayAll();

            PersistentVector v = PersistentVector.create(2, 3, 4);

            _objWithNullMeta = (IObj)v;
            _obj = _objWithNullMeta.withMeta(meta);
            _expectedType = typeof(PersistentVector);
        }

        [TearDown]
        public void Teardown()
        {
            _mocks.VerifyAll();
        }

    }

}
