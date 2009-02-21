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
    public class ArraySeqTests : AssertionHelper
    {

        #region C-tor tests

        [Test]
        public void Create_on_nothing_returns_null()
        {
            ArraySeq a = ArraySeq.create();
            
            Expect(a, Null);
        }

        [Test]
        public void Create_on_array_creates()
        {
            object[] array = new object[] { 1, 2, 3 };
            ArraySeq a = ArraySeq.create(array);

            Expect(a, Not.Null);
        }

        [Test]
        public void Create_on_null_returns_null()
        {
            ArraySeq a = ArraySeq.create(null);
            
            Expect(a, Null);
        }

        [Test]
        public void Create_on_array_has_no_meta()
        {
            object[] array = new object[] { 1, 2, 3 };
            ArraySeq a = ArraySeq.create(array);

            Expect(a.meta(), Null);
        }

        [Test]
        public void Create_on_array_and_index_creates()
        {
            object[] array = new object[] { 1, 2, 3 };
            ArraySeq a = ArraySeq.create(array,0);

            Expect(a, Not.Null);
        }

        [Test]
        public void Create_on_array_and_index_with_high_index_returns_null()
        {
            object[] array = new object[] { 1, 2, 3 };
            ArraySeq a = ArraySeq.create(array, 10);

            Expect(a, Null);
        }

        [Test]
        public void Create_on_array_and_index_has_no_meta()
        {
            object[] array = new object[] { 1, 2, 3 };
            ArraySeq a = ArraySeq.create(array,0);

            Expect(a.meta(), Null);
        }  

        #endregion

        #region IPersistentCollection tests

        [Test]
        public void ArraySeq_has_correct_count_1()
        {
            object[] array = new object[] { 1, 2, 3 };
            ArraySeq a = ArraySeq.create(array);

            Expect(a.count(), EqualTo(3));
        }

        [Test]
        public void ArraySeq_has_correct_count_2()
        {
            object[] array = new object[] { 1, 2, 3 };
            ArraySeq a = ArraySeq.create(array,1);

            Expect(a.count(), EqualTo(2));
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

            object[] array = new object[] { 2, 3, 4 };
            ArraySeq a = ArraySeq.create(array);
            object ret = a.reduce(fn);

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

            object[] array = new object[] { 2, 3, 4 };
            ArraySeq a = ArraySeq.create(array);
            object ret = a.reduce(fn, 20);

            Expect(ret, EqualTo(7));

            mocks.VerifyAll();
        }
        #endregion

    }

    [TestFixture]
    public class ArraySeq_ISeq_Tests : ISeqTestHelper
    {
        object[] _array0;
        object[] _array1;
        ArraySeq _a0;
        ArraySeq _a1;

        [SetUp]
        public void Setup()
        {
            _array0 = new object[] { 1, 2, 3 };
            _array1 = new object[] {    2, 3 };
            _a0 = ArraySeq.create(_array0);
            _a1 = ArraySeq.create(_array0, 1);
        }

        [Test]
        public void ArraySeq_ISeq_std_ctor_has_correct_elements()
        {
            VerifyISeqContents(_a0, _array0);
        }

        [Test]
        public void ArraySeq_ISeq_index_ctor_has_correct_elements()
        {
            VerifyISeqContents(_a1, _array1 );
        }

        [Test]
        public void ArraySeq_ISeq_std_ctor_conses()
        {
            VerifyISeqCons(_a0, 4, _array0);
        }

        [Test]
        public void ArraySeq_ISeq_index_ctor_conses()
        {
            VerifyISeqCons(_a1, 4, _array1);
        }


    }

    [TestFixture]
    public class ArraySeq_IObj_Tests : IObjTests
    {
        [SetUp]
        public void Setup()
        {
            object[] array = new object[] { 1, 2, 3 };
            _objWithNullMeta = _obj = ArraySeq.create(array, 0);
            _expectedType = typeof(ArraySeq);
        }
    }
}
