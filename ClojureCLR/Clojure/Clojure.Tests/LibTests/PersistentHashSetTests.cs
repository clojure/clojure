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
using System.Collections;

namespace Clojure.Tests.LibTests
{
    [TestFixture]
    public class PersistentHashSetTests : AssertionHelper
    {

        #region C-tor tests

        [Test]
        public void CreateOnEmptyListReturnsEmptySet()
        {
            ArrayList a = new ArrayList();
            IPersistentSet m = PersistentHashSet.create1(a);

            Expect(m.count(), EqualTo(0));
        }

        [Test]
        public void CreateOnListReturnsSet()
        {
            object[] items = new object[] { 1, "a" };
            ArrayList a = new ArrayList(items);

            IPersistentSet m = PersistentHashSet.create1(a);

            Expect(m.count(), EqualTo(2));
            Expect(m.contains(1));
            Expect(m.contains("a"));
            Expect(m.contains(3), False);
        }

        [Test]
        public void CreateOnEmptyISeqReturnsEmptySet()
        {
            object[] items = new object[] { };
            ArrayList a = new ArrayList(items);
            ISeq s = PersistentList.create(a).seq();
            IPersistentSet m = PersistentHashSet.create(s);

            Expect(m.count(), EqualTo(0));
        }

        [Test]
        public void CreateOnISeqReturnsSet()
        {
            object[] items = new object[] { 1, "a" };
            ArrayList a = new ArrayList(items);
            ISeq s = PersistentList.create(a).seq();
            IPersistentSet m = PersistentHashSet.create(s);

            Expect(m.count(), EqualTo(2));
            Expect(m.contains(1));
            Expect(m.contains("a"));
            Expect(m.contains(3), False);
        }

        [Test]
        public void CreateOnNoArgsReturnsEmptySet()
        {
            PersistentHashSet m = PersistentHashSet.create();

            Expect(m.count(), EqualTo(0));
            Expect(m.meta(), Null);
        }

        [Test]
        public void CreateOnNoArgsReturnsSet()
        {
            PersistentHashSet m = PersistentHashSet.create(1, "a");

            Expect(m.count(), EqualTo(2));
            Expect(m.contains(1));
            Expect(m.contains("a"));
            Expect(m.contains(3), False);
            Expect(m.meta(), Null);
        }



        #endregion

        #region Associative tests

        #endregion

        #region IPersistentMap tests

        #endregion

        #region IPersistentCollection tests

        #endregion

        #region Big tests

        [Test]
        public void DoSomeBigTests()
        {
            DoBigTest(100);
            DoBigTest(1000);
            DoBigTest(10000);
            DoBigTest(100000);
        }

        public void DoBigTest(int numEntries)
        {
            System.Console.WriteLine("Testing {0} items.", numEntries);

            Random rnd = new Random();
            Dictionary<object, object> dict = new Dictionary<object, object>(numEntries);
            for (int i = 0; i < numEntries; i++)
            {
                int r = rnd.Next();
                dict[r] = r;
            }

            object[] items = dict.Keys.ToArray();

            PersistentHashSet m = (PersistentHashSet)PersistentHashSet.create(items);

            Expect(m.count(), EqualTo(dict.Count));

            foreach (int key in dict.Keys)
            {
                Expect(m.contains(key));
            }

            for (ISeq s = m.seq(); s != null; s = s.rest())
                Expect(dict.ContainsKey((int)s.first()));

        }

        #endregion
    }

    [TestFixture]
    public class PersistentHashSet_IObj_Tests : IObjTests
    {
        MockRepository _mocks;

        [SetUp]
        public void Setup()
        {
            _mocks = new MockRepository();
            IPersistentMap meta = _mocks.StrictMock<IPersistentMap>();
            _mocks.ReplayAll();

            PersistentHashSet m = PersistentHashSet.create(1, "a", 2, "b");


            _objWithNullMeta = (IObj)m;
            _obj = _objWithNullMeta.withMeta(meta);
            _expectedType = typeof(PersistentHashSet);
        }

        [TearDown]
        public void Teardown()
        {
            _mocks.VerifyAll();
        }

    }

}
