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

using System.Collections;

using NUnit.Framework;
using Rhino.Mocks;

using clojure.lang;

using RMExpect = Rhino.Mocks.Expect;

namespace Clojure.Tests.LibTests
{
    [TestFixture]
    public class PersistentArrayMapTests : AssertionHelper
    {
        #region  C-tor tests

        [Test]
        public void CreateOnEmptyDictionaryReturnsEmptyMap()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            IPersistentMap m = PersistentArrayMap.create(d);

            Expect(m.count(), EqualTo(0));
        }

        [Test]
        public void CreateOnDictionaryReturnsMap()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IPersistentMap m = PersistentArrayMap.create(d);

            Expect(m.count(), EqualTo(2));
            Expect(m.valAt(1), EqualTo("a"));
            Expect(m.valAt(2), EqualTo("b"));
            Expect(m.containsKey(3), False);
        }

        // other c-tors are not public.


        #endregion
        
        #region  Associative tests

        [Test]
        public void ContainsKeyOnMissingKeyIsFalse()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IPersistentMap m = PersistentArrayMap.create(d);

            Expect(m.containsKey(3), False);
        }


        [Test]
        public void ContainsKeyOnExistingKeyIsTrue()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IPersistentMap m = PersistentArrayMap.create(d);

            Expect(m.containsKey(1));
            Expect(m.containsKey(2));
        }

        [Test]
        public void ContainsKeyNotConfusedByValue()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IPersistentMap m = PersistentArrayMap.create(d);

            Expect(m.containsKey("a"), False);
        }

        [Test]
        public void EntryAtReturnsNullOnMissingKey()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IPersistentMap m = PersistentArrayMap.create(d);

            Expect(m.entryAt(3), Null);
        }

        [Test]
        public void EntryAtReturnsEntryforKey()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IPersistentMap m = PersistentArrayMap.create(d);
            IMapEntry me = m.entryAt(1);

            Expect(me.key(), EqualTo(1));
            Expect(me.val(), EqualTo("a"));
        }

        [Test]
        public void ValAt1ReturnsNullOnMissingKey()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IPersistentMap m = PersistentArrayMap.create(d);

            Expect(m.valAt(3), Null);
        }

        [Test]
        public void ValAt1ReturnsValueforKey()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IPersistentMap m = PersistentArrayMap.create(d);

            Expect(m.valAt(1), EqualTo("a"));
        }


        [Test]
        public void ValAt2ReturnsDefaultOnMissingKey()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IPersistentMap m = PersistentArrayMap.create(d);

            Expect(m.valAt(3,99), EqualTo(99));
        }

        [Test]
        public void ValAt2ReturnsValueforKey()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IPersistentMap m = PersistentArrayMap.create(d);

            Expect(m.valAt(1,99), EqualTo("a"));
        }
        
        #endregion
        
        #region  IPersistentCollection tests

        [Test]
        public void CountOnEmptyReturns0()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            IPersistentMap m = PersistentArrayMap.create(d);

            Expect(m.count(), EqualTo(0));
        }

        [Test]
        public void CountOnNonEmptyReturnsCount()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";
            IPersistentMap m = PersistentArrayMap.create(d);

            Expect(m.count(), EqualTo(2));
        }

        [Test]
        public void EmptyReturnsEmpty()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";
            IPersistentMap m = PersistentArrayMap.create(d);
            IPersistentCollection c = m.empty();

            Expect(c.count(), EqualTo(0));
            Expect(c.seq(), Null);
        }


        [Test]
        public void SeqOnEmptyReturnNull()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            IPersistentMap m = PersistentArrayMap.create(d);
            ISeq s = m.seq();

            Expect(s, Null);
        }

        [Test]
        public void SeqOnNonEmptyIterates()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";
            IPersistentMap m = PersistentArrayMap.create(d);
            ISeq s = m.seq();
            IMapEntry me1 = (IMapEntry)s.first();
            IMapEntry me2 = (IMapEntry)s.next().first();
            ISeq end = s.next().next();

            Expect(s.count(), EqualTo(2));
            Expect(me1.key(), EqualTo(1) | EqualTo(2));
            Expect(me1.val(), EqualTo(((int)me1.key()==1 ? "a" : "b")));
            Expect(me2.key(), EqualTo(1) | EqualTo(2));
            Expect(me2.val(), EqualTo(((int)me2.key() == 1 ? "a" : "b")));
            Expect(end, Null);            
        }

        #endregion
        
        #region  IPersistentMap tests

        [Test]
        public void AssocModifiesOnExistingKey()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IPersistentMap m1 = PersistentArrayMap.create(d);
            IPersistentMap m2 = m1.assoc(2, "c");

            Expect(m1.count(), EqualTo(2));
            Expect(m1.valAt(2), EqualTo("b"));
            Expect(m2.count(), EqualTo(2));
            Expect(m2.valAt(2), EqualTo("c"));
        }


        [Test]
        public void AssocAddsOnNewKey()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IPersistentMap m1 = PersistentArrayMap.create(d);
            IPersistentMap m2 = m1.assoc(3, "c");

            Expect(m1.count(), EqualTo(2));
            Expect(m1.containsKey(3), False);
            Expect(m2.count(), EqualTo(3));
            Expect(m2.valAt(3), EqualTo("c"));
        }


        [Test]
        [ExpectedException(typeof(Exception))]
        public void AssocExFailsOnExistingKey()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IPersistentMap m1 = PersistentArrayMap.create(d);
            IPersistentMap m2 = m1.assocEx(2, "c");
        }



        [Test]
        public void AssocExModifiesOnNewKey()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IPersistentMap m1 = PersistentArrayMap.create(d);
            IPersistentMap m2 = m1.assocEx(3, "c");

            Expect(m1.count(), EqualTo(2));
            Expect(m1.containsKey(3), False);
            Expect(m2.count(), EqualTo(3));
            Expect(m2.valAt(3), EqualTo("c"));
        }

        [Test]
        public void WithoutOnExistingKeyRemovesKey()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[3] = "a";
            d[5] = "b";
            d[7] = "c";

            IPersistentMap m1 = PersistentArrayMap.create(d);
            IPersistentMap m2 = m1.without(5);

            Expect(m1.count(), EqualTo(3));
            Expect(m1.valAt(5), EqualTo("b"));
            Expect(m2.count(), EqualTo(2));
            Expect(m2.containsKey(5), False);
        }

        [Test]
        public void WithoutOnMissingKeyIsIdentity()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[3] = "a";
            d[5] = "b";
            d[7] = "c";

            IPersistentMap m1 = PersistentArrayMap.create(d);
            IPersistentMap m2 = m1.without(4);

            Expect(m2, SameAs(m1));
        }

        #endregion
        
        #region  APersistentMap tests

        [Test]
        public void EqualsOnSimilarDictionaryReturnsTrue()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IPersistentMap m = PersistentArrayMap.create(d);

            Expect(m.equiv(d));
        }

        [Test]
        public void EqualsOnDictionaryWIthDifferntValueReturnsFalse()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IPersistentMap m = PersistentArrayMap.create(d);

            d[2] = "c";

            Expect(m.Equals(d),False);
        }


        [Test]
        public void EqualsOnDictionaryWithExtraValueReturnsFalse()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IPersistentMap m = PersistentArrayMap.create(d);

            d[3] = "c";

            Expect(m.Equals(d), False);
        }

        [Test]
        public void HashCodeBasedOnValue()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IPersistentMap m1 = PersistentArrayMap.create(d);

            d[3] = "c";
            IPersistentMap m2 = PersistentArrayMap.create(d);

            Expect(m1.GetHashCode(), Not.EqualTo(m2.GetHashCode()));
        }

        [Test]
        public void AssociativeDotAssocWorks()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IPersistentMap m = PersistentArrayMap.create(d);
            Associative a = (Associative)m;

            Associative a1 = a.assoc(3, "c");
            Associative a2 = a.assoc(2, "c");

            Expect(a.count(), EqualTo(2));
            Expect(a.valAt(1), EqualTo("a"));
            Expect(a.valAt(2), EqualTo("b"));
            Expect(a.containsKey(3), False);

            Expect(a1.count(), EqualTo(3));
            Expect(a1.valAt(1), EqualTo("a"));
            Expect(a1.valAt(2), EqualTo("b"));
            Expect(a1.valAt(3), EqualTo("c"));

            Expect(a2.count(), EqualTo(2));
            Expect(a2.valAt(1), EqualTo("a"));
            Expect(a2.valAt(2), EqualTo("c"));
            Expect(a2.containsKey(3), False);
        }

        [Test]
        public void ConsOnIMapEntryAddsNew()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IPersistentMap m = PersistentArrayMap.create(d);
            IPersistentMap c = m.cons(new MapEntry(3, "c"));

            Expect(m.count(), EqualTo(2));
            Expect(m.valAt(1), EqualTo("a"));
            Expect(m.valAt(2), EqualTo("b"));

            Expect(c.count(), EqualTo(3));
            Expect(c.valAt(1), EqualTo("a"));
            Expect(c.valAt(2), EqualTo("b"));
            Expect(c.valAt(3), EqualTo("c"));
        }

        [Test]
        public void ConsOnIMapEntryReplacesExisting()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IPersistentMap m = PersistentArrayMap.create(d);
            IPersistentMap c = m.cons(new MapEntry(2, "c"));

            Expect(m.count(), EqualTo(2));
            Expect(m.valAt(1), EqualTo("a"));
            Expect(m.valAt(2), EqualTo("b"));

            Expect(c.count(), EqualTo(2));
            Expect(c.valAt(1), EqualTo("a"));
            Expect(c.valAt(2), EqualTo("c"));
        }


        [Test]
        public void ConsOnDictionaryEntryAddsNew()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IPersistentMap m = PersistentArrayMap.create(d);
            IPersistentMap c = m.cons(new DictionaryEntry(3, "c"));

            Expect(m.count(), EqualTo(2));
            Expect(m.valAt(1), EqualTo("a"));
            Expect(m.valAt(2), EqualTo("b"));

            Expect(c.count(), EqualTo(3));
            Expect(c.valAt(1), EqualTo("a"));
            Expect(c.valAt(2), EqualTo("b"));
            Expect(c.valAt(3), EqualTo("c"));
        }

        [Test]
        public void ConsOnDictionaryEntryReplacesExisting()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IPersistentMap m = PersistentArrayMap.create(d);
            IPersistentMap c = m.cons(new DictionaryEntry(2, "c"));

            Expect(m.count(), EqualTo(2));
            Expect(m.valAt(1), EqualTo("a"));
            Expect(m.valAt(2), EqualTo("b"));

            Expect(c.count(), EqualTo(2));
            Expect(c.valAt(1), EqualTo("a"));
            Expect(c.valAt(2), EqualTo("c"));
        }

        [Test]
        public void ConsOnKeyValuePairAddsNew()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IPersistentMap m = PersistentArrayMap.create(d);
            IPersistentMap c = m.cons(new KeyValuePair<int,string>(3, "c"));

            Expect(m.count(), EqualTo(2));
            Expect(m.valAt(1), EqualTo("a"));
            Expect(m.valAt(2), EqualTo("b"));

            Expect(c.count(), EqualTo(3));
            Expect(c.valAt(1), EqualTo("a"));
            Expect(c.valAt(2), EqualTo("b"));
            Expect(c.valAt(3), EqualTo("c"));
        }

        [Test]
        public void ConsOnKeyValuePairReplacesExisting()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IPersistentMap m = PersistentArrayMap.create(d);
            IPersistentMap c = m.cons(new KeyValuePair<int,string>(2, "c"));

            Expect(m.count(), EqualTo(2));
            Expect(m.valAt(1), EqualTo("a"));
            Expect(m.valAt(2), EqualTo("b"));

            Expect(c.count(), EqualTo(2));
            Expect(c.valAt(1), EqualTo("a"));
            Expect(c.valAt(2), EqualTo("c"));
        }

        [Test]
        public void ConsOnIPVAddsNew()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IPersistentMap m = PersistentArrayMap.create(d);

            IPersistentVector v = PersistentVector.create(3, "c");
            IPersistentMap c = m.cons(v);

            Expect(m.count(), EqualTo(2));
            Expect(m.valAt(1), EqualTo("a"));
            Expect(m.valAt(2), EqualTo("b"));

            Expect(c.count(), EqualTo(3));
            Expect(c.valAt(1), EqualTo("a"));
            Expect(c.valAt(2), EqualTo("b"));
            Expect(c.valAt(3), EqualTo("c"));
        }

        [Test]
        public void ConsOnIPVReplacesExisting()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IPersistentMap m = PersistentArrayMap.create(d);

            IPersistentVector v = PersistentVector.create(2, "c");
            IPersistentMap c = m.cons(v);

            Expect(m.count(), EqualTo(2));
            Expect(m.valAt(1), EqualTo("a"));
            Expect(m.valAt(2), EqualTo("b"));

            Expect(c.count(), EqualTo(2));
            Expect(c.valAt(1), EqualTo("a"));
            Expect(c.valAt(2), EqualTo("c"));
        }

        [Test]
        [ExpectedException(typeof(ArgumentException))]
        public void ConsOnNon2IPVFails()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IPersistentMap m = PersistentArrayMap.create(d);

            IPersistentVector v = PersistentVector.create(2, "c", 3, "d");
            IPersistentMap c = m.cons(v);

        }

        [Test]
        public void ConsOnIPersistentMapAddsOrReplacesMany()
        {
            Dictionary<int, string> d1 = new Dictionary<int, string>();
            d1[1] = "a";
            d1[2] = "b";

            IPersistentMap m1 = PersistentArrayMap.create(d1);


            Dictionary<int, string> d2 = new Dictionary<int, string>();
            d2[2] = "c";
            d2[3] = "d";

            IPersistentMap m2 = PersistentArrayMap.create(d2);
            IPersistentMap m3 = m1.cons(m2);


            Expect(m1.count(), EqualTo(2));
            Expect(m1.valAt(1), EqualTo("a"));
            Expect(m1.valAt(2), EqualTo("b"));

            Expect(m2.count(), EqualTo(2));
            Expect(m2.valAt(2), EqualTo("c"));
            Expect(m2.valAt(3), EqualTo("d"));

            Expect(m3.count(), EqualTo(3));
            Expect(m3.valAt(1), EqualTo("a"));
            Expect(m3.valAt(2), EqualTo("c"));
            Expect(m3.valAt(3), EqualTo("d"));
        }

        [Test]
        public void InvokeOn1ArgDoesValAt1()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IFn f = (IFn)PersistentArrayMap.create(d);

            Expect(f.invoke(1), EqualTo("a"));
            Expect(f.invoke(7),Null);

        }
        [Test]
        public void InvokeOn2ArgsDoesValAt2()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IFn f = (IFn)PersistentArrayMap.create(d);

            Expect(f.invoke(1,99), EqualTo("a"));
            Expect(f.invoke(7,99), EqualTo(99));
        }

        [Test]
        [ExpectedException(typeof(NotImplementedException))]
        public void IDictionary_Add_fails()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IDictionary id = (IDictionary)PersistentArrayMap.create(d);
            id.Add(1, "c");
        }

        [Test]
        [ExpectedException(typeof(NotImplementedException))]
        public void IDictionary_Clear_fails()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IDictionary id = (IDictionary)PersistentArrayMap.create(d);
            id.Clear();
        }

        [Test]
        [ExpectedException(typeof(NotImplementedException))]
        public void IDictionary_Remove_fails()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IDictionary id = (IDictionary)PersistentArrayMap.create(d);
            id.Remove(1);
        }

        [Test]
        public void IDictionary_Contains_finds_existing_key()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IDictionary id = (IDictionary)PersistentArrayMap.create(d);
            Expect(id.Contains(1));
        }



        [Test]
        public void IDictionary_Contains_does_not_find_existing_key()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IDictionary id = (IDictionary)PersistentArrayMap.create(d);
            
            Expect(id.Contains(3),False);
        }

        [Test]
        public void IDictionary_IsFixedSize_is_true()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IDictionary id = (IDictionary)PersistentArrayMap.create(d);

            Expect(id.IsFixedSize);
        }

        [Test]
        public void IDictionary_IsReadOnly_is_true()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IDictionary id = (IDictionary)PersistentArrayMap.create(d);

            Expect(id.IsReadOnly);
        }

        [Test]
        public void IDictionary_index_acts_like_valAt()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IDictionary id = (IDictionary)PersistentArrayMap.create(d);
            Expect(id[2], EqualTo("b"));
            Expect(id[3], Null);
        }

        [Test]
        public void IDictionary_Keys_creates_key_collection()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IDictionary id = (IDictionary)PersistentArrayMap.create(d);
            ICollection keys = id.Keys;

            Expect(keys.Count, EqualTo(2));
            int[] akeys = new int[2];
            keys.CopyTo(akeys, 0);
            Array.Sort(akeys);
            Expect(akeys[0], EqualTo(1));
            Expect(akeys[1], EqualTo(2));           
        }

        [Test]
        public void IDictionary_Values_creates_value_collection()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IDictionary id = (IDictionary)PersistentArrayMap.create(d);
            ICollection vals = id.Values;

            Expect(vals.Count, EqualTo(2));
            string[] avals = new string[2];
            vals.CopyTo(avals, 0);
            Array.Sort(avals);
            Expect(avals[0], EqualTo("a"));
            Expect(avals[1], EqualTo("b"));
        }




        [Test]
        public void IDictionary_GetEnumerator_returns_an_enumerator()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            IDictionary id = (IDictionary)PersistentArrayMap.create(d);
            IDictionaryEnumerator e = id.GetEnumerator();

            Expect(e.MoveNext());
            DictionaryEntry de1 = (DictionaryEntry)e.Current;
            Expect(e.MoveNext());
            DictionaryEntry de2 = (DictionaryEntry)e.Current;
            Expect(e.MoveNext(), False);

            Expect(de1.Key, EqualTo(1) | EqualTo(2));
            Expect(de2.Key, EqualTo(1) | EqualTo(2));
            Expect(de1.Value, EqualTo(((int)de1.Key) == 1 ? "a" : "b"));
            Expect(de2.Value, EqualTo(((int)de2.Key) == 1 ? "a" : "b"));
        }

        [Test]
        public void ICollection_CopyTo_Copies()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            ICollection c = (ICollection)PersistentArrayMap.create(d);
            IMapEntry[] a = new IMapEntry[c.Count];
            c.CopyTo(a, 0);

            int key0 = (int)a[0].key();
            int key1 = (int)a[1].key();
            string val0 = (string)a[0].val();
            string val1 = (string)a[1].val();

            Expect(key0, EqualTo(1) | EqualTo(2));
            Expect(key1, EqualTo(key0 == 1 ? 2 : 1));
            Expect(val0, EqualTo(key0 == 1 ? "a" : "b"));
            Expect(val1, EqualTo(key1 == 1 ? "a" : "b"));
        }


        [Test]
        public void ICollection_Count_returns_count_of_items()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            ICollection c = (ICollection)PersistentArrayMap.create(d);

            Expect(c.Count, EqualTo(2));
        }

        [Test]
        public void ICollection_IsSynchronized_is_true()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            ICollection c = (ICollection)PersistentArrayMap.create(d);

            Expect(c.IsSynchronized);
        }


        [Test]
        [ExpectedException(typeof(NotImplementedException))]
        public void ICollection_SyncRoot_fails()
        {
            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "a";
            d[2] = "b";

            ICollection c = (ICollection)PersistentArrayMap.create(d);

            object s = c.SyncRoot;
        }


        #endregion
    }

    [TestFixture]
    public class PersistentArrayMap_IObj_Tests : IObjTests
    {
        MockRepository _mocks;

        [SetUp]
        public void Setup()
        {
            _mocks = new MockRepository();
            IPersistentMap meta = _mocks.StrictMock<IPersistentMap>();
            _mocks.ReplayAll();

            Dictionary<int, string> d = new Dictionary<int, string>();
            d[1] = "abc";

            _objWithNullMeta = (IObj)PersistentArrayMap.create(d);
            _obj = _objWithNullMeta.withMeta(meta);
            _expectedType = typeof(PersistentArrayMap);
        }

        [TearDown]
        public void Teardown()
        {
            _mocks.VerifyAll();
        }

    }

}
