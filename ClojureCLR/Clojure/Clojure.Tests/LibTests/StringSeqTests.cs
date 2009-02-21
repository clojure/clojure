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
    public class StringSeqTests : AssertionHelper
    {

        #region C-tor tests

        [Test]
        public void Create_on_empty_string_yields_null()
        {
            StringSeq s = StringSeq.create(String.Empty);

            Expect(s, Null);
        }

        [Test]
        public void Create_on_nonempty_string_yields_a_StringSeq()
        {
            StringSeq s = StringSeq.create("abcde");

            Expect(s, Not.Null);
        }

        #endregion

        #region IPersistentCollection tests

        [Test]
        public void Count_is_string_length()
        {
            StringSeq s = StringSeq.create("abcde");
            
            Expect(s.count(),EqualTo(5));
        }

        #endregion

        #region IndexedSeq tests

        [Test]
        public void Initial_index_is_zero()
        {
            StringSeq s = StringSeq.create("abc");

            Expect(s.index(), EqualTo(0));
        }

        [Test]
        public void Index_of_rest_is_one()
        {
            StringSeq s = StringSeq.create("abc");
            IndexedSeq i = (IndexedSeq)s.rest();

            Expect(i.index(), EqualTo(1));
        }

        #endregion

    }

    [TestFixture]
    public class StringSeq_ISeq_Tests : ISeqTestHelper
    {
        StringSeq _s;
        StringSeq _sWithMeta;
        object[] _values;

        [SetUp]
        public void Setup()
        {
            IPersistentMap meta = PersistentHashMap.create("a", 1, "b", 2);

            _s = StringSeq.create("abcde");
            _sWithMeta = (StringSeq)((IObj)StringSeq.create("abcde")).withMeta(meta);
            _values = new object[] { 'a', 'b', 'c', 'd', 'e' };
        }

        [Test]
        public void StringSeq_has_correct_ISeq_values()
        {
            VerifyISeqContents(_s, _values);
        }

        [Test]
        public void StringSeq_with_meta_has_correct_ISeq_values()
        {
            VerifyISeqContents(_sWithMeta, _values);
        }

        [Test]
        public void StringSeq_ISeq_rest_preserves_meta()
        {
            VerifyISeqRestMaintainsMeta(_sWithMeta);
        }

        [Test]
        public void StringSeq_ISeq_rest_preserves_type()
        {
            VerifyISeqRestTypes(_s,typeof(StringSeq));
        }

        [Test]
        public void StringSeq_ISeq_cons_works()
        {
            VerifyISeqCons(_s, 12, _values);
        }

    }

    [TestFixture]
    public class StringSeq_IObj_Tests : IObjTests
    {
        MockRepository _mocks;

        [SetUp]
        public void Setup()
        {
            _mocks = new MockRepository();
            IPersistentMap meta = _mocks.StrictMock<IPersistentMap>();
            _mocks.ReplayAll();

            StringSeq s = StringSeq.create("abcde");


            _objWithNullMeta = (IObj)s;
            _obj = _objWithNullMeta.withMeta(meta);
            _expectedType = typeof(StringSeq);
        }

        [TearDown]
        public void Teardown()
        {
            _mocks.VerifyAll();
        }

    }

}
