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
    public class PersistentHashMapTests : AssertionHelper
    {

        #region C-tor tests

        [Test]
        public void CreateOnEmptyDictionaryReturnsEmptyMap()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            IPersistentMap m = PersistentHashMap.create(d);

            Expect(m.count(), EqualTo(0));
        }

        [Test]
        public void CreateOnDictionaryReturnsMap()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IPersistentMap m = PersistentHashMap.create(d);

            Expect(m.count(), EqualTo(2));
            Expect(m.valAt(1), EqualTo("a"));
            Expect(m.valAt(2), EqualTo("b"));
            Expect(m.containsKey(3), False);
        }

        [Test]
        public void CreateOnEmptyListReturnsEmptyMap()
        {
            ArrayList a = new ArrayList();
            IPersistentMap m = PersistentHashMap.create(a);

            Expect(m.count(), EqualTo(0));
        }

        [Test]
        public void CreateOnListReturnsMap()
        {
            object[] items = new object[] { 1, "a", 2, "b" };
            ArrayList a = new ArrayList(items);
         
            IPersistentMap m = PersistentHashMap.create(a);

            Expect(m.count(), EqualTo(2));
            Expect(m.valAt(1), EqualTo("a"));
            Expect(m.valAt(2), EqualTo("b"));
            Expect(m.containsKey(3), False);
        }

        [Test]
        public void CreateOnEmptyISeqReturnsEmptyMap()
        {
            object[] items = new object[] {};
            ArrayList a = new ArrayList(items);
            ISeq s = PersistentList.create(a).seq();
            IPersistentMap m = PersistentHashMap.create(s);

            Expect(m.count(), EqualTo(0));
        }

        [Test]
        public void CreateOnISeqReturnsMap()
        {
            object[] items = new object[] { 1, "a", 2, "b" };
            ArrayList a = new ArrayList(items);
            ISeq s = PersistentList.create(a).seq();
            IPersistentMap m = PersistentHashMap.create(s);

            Expect(m.count(), EqualTo(2));
            Expect(m.valAt(1), EqualTo("a"));
            Expect(m.valAt(2), EqualTo("b"));
            Expect(m.containsKey(3), False);
        }

        [Test]
        public void CreateOnNoArgsReturnsEmptyMap()
        {
            PersistentHashMap m = PersistentHashMap.create();

            Expect(m.count(), EqualTo(0));
            Expect(m.meta(), Null);
        }

        [Test]
        public void CreateOnNoArgsReturnsMap()
        {
            PersistentHashMap m = PersistentHashMap.create(1, "a", 2, "b");

            Expect(m.count(), EqualTo(2));
            Expect(m.valAt(1), EqualTo("a"));
            Expect(m.valAt(2), EqualTo("b"));
            Expect(m.containsKey(3), False);
            Expect(m.meta(), Null);
        }


        [Test]
        public void CreateOnMetaNoArgsReturnsEmptyMap()
        {
            MockRepository mocks = new MockRepository();
            IPersistentMap meta = mocks.StrictMock<IPersistentMap>();
            mocks.ReplayAll();

            PersistentHashMap m = PersistentHashMap.create(meta);

            Expect(m.count(), EqualTo(0));
            Expect(m.meta(), SameAs(meta));
            mocks.VerifyAll();
        }

        [Test]
        public void CreateOnMetaNoArgsReturnsMap()
        {
            MockRepository mocks = new MockRepository();
            IPersistentMap meta = mocks.StrictMock<IPersistentMap>();
            mocks.ReplayAll();

            PersistentHashMap m = PersistentHashMap.create(meta,1, "a", 2, "b");

            Expect(m.count(), EqualTo(2));
            Expect(m.valAt(1), EqualTo("a"));
            Expect(m.valAt(2), EqualTo("b"));
            Expect(m.containsKey(3), False);
            Expect(m.meta(), SameAs(meta));
            mocks.VerifyAll();
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
            Dictionary<int, int> dict = new Dictionary<int, int>(numEntries);
            for (int i = 0; i < numEntries; i++)
            {
                int r = rnd.Next();
                dict[r] = r;
            }
            PersistentHashMap m = (PersistentHashMap) PersistentHashMap.create(dict);

            Expect(m.count(),EqualTo(dict.Count));
            
            foreach ( int key in dict.Keys )
            {
                Expect(m.containsKey(key));
                Expect(m.valAt(key),EqualTo(key));
            }

            for ( ISeq s = m.seq(); s != null; s = s.rest() )
                Expect(dict.ContainsKey((int)((IMapEntry)s.first()).key()));

        }

        #endregion

    }

    [TestFixture]
    public class PersistentHashMap_IObj_Tests : IObjTests
    {
        MockRepository _mocks;

        [SetUp]
        public void Setup()
        {
            _mocks = new MockRepository();
            IPersistentMap meta = _mocks.StrictMock<IPersistentMap>();
            _mocks.ReplayAll();

            PersistentHashMap m = PersistentHashMap.create(1, "a", 2, "b");


            _objWithNullMeta = (IObj)m;
            _obj = _objWithNullMeta.withMeta(meta);
            _expectedType = typeof(PersistentHashMap);
        }

        [TearDown]
        public void Teardown()
        {
            _mocks.VerifyAll();
        }

    }

}
