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
    public class LazyConsTests : AssertionHelper
    {

        #region C-tor tests

        // Couldn't think of anything except to make sure it doesn't throw an exception.
        [Test]
        public void CtorWorks()
        {
            LazyCons lc = new LazyCons(null);

            Expect(lc, Not.Null);
        }

        #endregion
    }

    [TestFixture]
    public class LazyCons_ISeq_Tests : ISeqTestHelper
    {
        MockRepository _mocks ;
        IFn _fn;
        LazyCons _lc;
        object[] _values;

        [SetUp]
        public void Setup()
        {             
            _mocks = new MockRepository();
            _fn = _mocks.StrictMock<IFn>();
            RMExpect.Call(_fn.invoke()).Return(10);
            RMExpect.Call(_fn.invoke(null)).Return(new Cons(20,null));
            _lc = new LazyCons(_fn);
            _values = new object[] { 10, 20 };
            _mocks.ReplayAll();
        }

        [TearDown]
        public void Teardown()
        {
            _mocks.VerifyAll();
        }

        [Test]
        public void ISeq_has_proper_values()
        {
            VerifyISeqContents(_lc, _values);
        }

        [Test]
        public void First_caches()
        {
            _lc.first();
            _lc.first();

            // Need to meet expectation that _rest is called.
            _lc.rest();
        }

        [Test]
        public void Rest_calcs_first()
        {
            _lc.rest();
        }

        [Test]
        public void Rest_caches()
        {
            _lc.rest();
            _lc.rest();
        }



    }

    [TestFixture]
    public class LazyCons_IObj_Tests : IObjTests
    {
        [SetUp]
        public void Setup()
        {
            MockRepository mocks = new MockRepository();
            IPersistentMap meta = mocks.StrictMock<IPersistentMap>();
            IFn fn = mocks.StrictMock<IFn>();
            RMExpect.Call(fn.invoke()).Return(10);
            RMExpect.Call(fn.invoke(null)).Return(null);
            mocks.ReplayAll();

            _objWithNullMeta = new LazyCons(fn);
            _obj = _objWithNullMeta.withMeta(meta);
            _expectedType = typeof(LazyCons);

            mocks.VerifyAll();
        }
    }
}
