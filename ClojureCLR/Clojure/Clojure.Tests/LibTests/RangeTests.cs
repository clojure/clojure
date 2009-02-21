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
    public class RangeTests : AssertionHelper
    {
        #region C-tor tests

        [Test]
        public void Basic_ctor_has_no_meta()
        {
            Range r = new Range(2, 5);
            Expect(r.meta(), Null);
        }

        [Test]
        public void Meta_ctor_has_meta()
        {
            MockRepository mocks = new MockRepository();
            IPersistentMap meta = mocks.StrictMock<IPersistentMap>();
            mocks.ReplayAll();

            Range r = new Range(meta, 2, 5);

            Expect(r.meta(), EqualTo(meta));

            mocks.VerifyAll();
        }

        #endregion

        #region IPersistentCollection tests

        [Test]
        public void Range_has_correct_count()
        {
            Range r = new Range(2, 20);

            Expect(r.count(), EqualTo(18));
        }


        #endregion

        #region IReduce tests

        [Test]
        public void ReduceWithNoStartIterates()
        {
            MockRepository mocks = new MockRepository();
            IFn fn = mocks.StrictMock<IFn>();
            RMExpect.Call(fn.invoke(2, 3)).Return(5);
            RMExpect.Call(fn.invoke(5, 4)).Return(7);
            mocks.ReplayAll();

            Range r = new Range(2, 5);
            object ret = r.reduce(fn);

            Expect(ret, EqualTo(7));

            mocks.VerifyAll();
        }

        [Test]
        public void ReduceWithStartIterates()
        {
            MockRepository mocks = new MockRepository();
            IFn fn = mocks.StrictMock<IFn>();
            RMExpect.Call(fn.invoke(20, 2)).Return(10);
            RMExpect.Call(fn.invoke(10, 3)).Return(5);
            RMExpect.Call(fn.invoke(5, 4)).Return(7);
            mocks.ReplayAll();

            Range r = new Range(2, 5);
            object ret = r.reduce(fn, 20);

            Expect(ret, EqualTo(7));

            mocks.VerifyAll();
        }

        #endregion

        // TODO: test stream capability of Range        
    }

    [TestFixture]
    public class Range_ISeq_Tests : ISeqTestHelper
    {
        Range _r;
        Range _rWithMeta;
        object[] _values;

        [SetUp]
        public void Setup()
        {
            IPersistentMap meta = PersistentHashMap.create("a", 1, "b", 2);

            _r = new Range(2, 5);
            _rWithMeta = new Range(meta, 2, 5);
            _values = new object[] { 2, 3, 4 };
        }

        [Test]
        public void Range_has_correct_values()
        {
            VerifyISeqContents(_r, _values);
        }

        [Test]
        public void Range_with_meta_has_correct_values()
        {
            VerifyISeqContents(_rWithMeta, _values);
        }

        [Test]
        public void Rest_preserves_meta()
        {
            VerifyISeqRestMaintainsMeta(_rWithMeta);
        }

        [Test]
        public void Rest_preserves_type()
        {
            VerifyISeqRestTypes(_r, typeof(Range));
        }

        [Test]
        public void Cons_works()
        {
            VerifyISeqCons(_r, 12, _values);
        }
    }

    [TestFixture]
    public class Range_IObj_Tests : IObjTests
    {
        MockRepository _mocks;

        [SetUp]
        public void Setup()
        {
            _mocks = new MockRepository();
            IPersistentMap meta = _mocks.StrictMock<IPersistentMap>();
            _mocks.ReplayAll();

            Range r = new Range(2, 5);

            _objWithNullMeta = (IObj)r;
            _obj = _objWithNullMeta.withMeta(meta);
            _expectedType = typeof(Range);
        }

        [TearDown]
        public void Teardown()
        {
            _mocks.VerifyAll();
        }

    }
}
