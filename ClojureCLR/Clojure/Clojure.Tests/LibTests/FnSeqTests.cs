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
    public class FnSeqTests : AssertionHelper
    {
        #region C-tor tests

        // Couldn't think of anything except to make sure it doesn't throw an exception.
        [Test]
        public void CtorWorks()
        {
            FnSeq fs = new FnSeq("abc",null);

            Expect(fs, Not.Null);
        }

        #endregion
    }

    [TestFixture]
    public class FnSeq_ISeq_Tests : ISeqTestHelper
    {
        object[] _restValues;
        object[] _values;
        MockRepository _mocks;
        FnSeq _fs;

        [SetUp]
        public void Setup()
        {
            _restValues = new object[] { 2, 3, 4 };
            _values = new object[] { "abc", 2, 3, 4 };
            _mocks = new MockRepository();
            IFn _fn = _mocks.StrictMock<IFn>();
            RMExpect.Call(_fn.invoke()).Return(PersistentList.create(_restValues));

            _fs = new FnSeq("abc", _fn);

            _mocks.ReplayAll();
        }

        [TearDown]
        public void Teardown()
        {
            _mocks.VerifyAll();
        }


        [Test]
        public void FnSeq_ISeq_has_correct_values()
        {
            VerifyISeqContents(_fs, _values);
        }

        [Test]
        public void FnSeq_ISeq_conses()
        {
            VerifyISeqCons(_fs, 12, _values);
        }

        [Test]
        public void RestCachesResult()
        {
            _fs.rest();
            _fs.rest();
        }


    
    }

    [TestFixture]
    public class FnSeq_IObj_Tests : IObjTests
    {
        MockRepository _mocks;

        [SetUp]
        public void Setup()
        {
            _mocks = new MockRepository();
            IFn fn = _mocks.StrictMock<IFn>();
            RMExpect.Call(fn.invoke()).Return(null);
            _mocks.ReplayAll();

            FnSeq fs = new FnSeq("abc", fn);

            _obj = _objWithNullMeta = fs;
            _expectedType = typeof(FnSeq);
        }

        [TearDown]
        public void TearDown()
        {
            _mocks.ReplayAll();
        }

    }
}
