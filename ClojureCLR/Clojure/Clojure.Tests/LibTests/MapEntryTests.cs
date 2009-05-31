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
    public class MapEntryTests : AssertionHelper
    {
        #region C-tor tests

        [Test]
        public void CtorCreatesEntryWithProperKeyVal()
        {
            MapEntry me = new MapEntry(1, "abc");

            Expect(me.key(), EqualTo(1));
            Expect(me.val(), EqualTo("abc"));
        }

        #endregion
        
        #region Object override tests

        [Test]
        public void HashCodeSameAsPersistentVector()
        {
            MapEntry me = new MapEntry(1, "abc");
            PersistentVector v = PersistentVector.create(1, "abc");

            Expect(me.GetHashCode(), EqualTo(v.GetHashCode()));
        }

        [Test]
        public void HashCodeFalseOnDifferentValues()
        {
            MapEntry me = new MapEntry(1, "abc");
            PersistentVector v = PersistentVector.create(1, "abcd");

            Expect(me.GetHashCode(), Not.EqualTo(v.GetHashCode()));
        }

        [Test]
        public void EqualsWorksOnPersistentVector()
        {
            MapEntry me = new MapEntry(1, "abc");
            PersistentVector v = PersistentVector.create(1, "abc");

            Expect(me.Equals(v));
        }

        [Test]
        public void EqualsWorksFalseOnDifferentValues()
        {
            MapEntry me = new MapEntry(1, "abc");
            PersistentVector v = PersistentVector.create(1, "abcd");

            Expect(me.Equals(v),False);
        }

        
        #endregion
        
        #region IMapEntry tests
        
        #endregion
        
        #region IPersistentVector tests

        [Test]
        public void LengthIs2()
        {
            MapEntry me = new MapEntry(1, "abc");

            Expect(me.length(), EqualTo(2));
        }

        [Test]
        public void NthInRangeWorks()
        {

            MapEntry me = new MapEntry(1, "abc");

            Expect(me.nth(0), EqualTo(1));
            Expect(me.nth(1), EqualTo("abc"));
        }

        [Test]
        [ExpectedException(typeof(IndexOutOfRangeException))]
        public void NthOutOfRangeLowFails()
        {
            MapEntry me = new MapEntry(1, "abc");
            object obj = me.nth(-4);
        }

        [Test]
        [ExpectedException(typeof(IndexOutOfRangeException))]
        public void NthOutOfRangeHighFails()
        {
            MapEntry me = new MapEntry(1, "abc");
            object obj = me.nth(4);
        }

        [Test]
        public void AssocNInRangeModifies()
        {
            MapEntry me = new MapEntry(1, "abc");
            IPersistentVector v1 = me.assocN(0, 2);
            IPersistentVector v2 = me.assocN(1, "def");
            IPersistentVector v3 = me.assocN(2, "ghi");

            Expect(me.count(), EqualTo(2));
            Expect(me.key(), EqualTo(1));
            Expect(me.val(), EqualTo("abc"));

            Expect(v1.count(), EqualTo(2));
            Expect(v1.nth(0), EqualTo(2));
            Expect(v1.nth(1), EqualTo("abc"));

            Expect(v2.count(), EqualTo(2));
            Expect(v2.nth(0), EqualTo(1));
            Expect(v2.nth(1), EqualTo("def"));

            Expect(v3.count(), EqualTo(3));
            Expect(v3.nth(0), EqualTo(1));
            Expect(v3.nth(1), EqualTo("abc"));
            Expect(v3.nth(2), EqualTo("ghi"));
        }

        [Test]
        [ExpectedException(typeof(IndexOutOfRangeException))]
        public void AssocNOutOfRangeLowThrows()
        {
            MapEntry me = new MapEntry(1, "abc");
            IPersistentVector v1 = me.assocN(-4, 2);
        }

        [Test]
        [ExpectedException(typeof(IndexOutOfRangeException))]
        public void AssocNOutOfRangeHighThrows()
        {
            MapEntry me = new MapEntry(1, "abc");
            IPersistentVector v1 = me.assocN(4, 2);
        }

        [Test]
        public void ConsWorks()
        {
            MapEntry me = new MapEntry(1, "abc");
            IPersistentVector v1 = me.cons(2);

            Expect(me.count(), EqualTo(2));
            Expect(me.key(), EqualTo(1));
            Expect(me.val(), EqualTo("abc"));


            Expect(v1.count(), EqualTo(3));
            Expect(v1.nth(0), EqualTo(1));
            Expect(v1.nth(1), EqualTo("abc"));
            Expect(v1.nth(2), EqualTo(2));
        }

        #endregion
        
        #region Associative tests

        [Test]
        public void ContainsKeyOnExistingKeyWorks()
        {
            MapEntry me = new MapEntry(1, "abc");

            Expect(me.containsKey(0));
            Expect(me.containsKey(1));
        }

        [Test]
        public void ContainsKeyOutOfRangeIsFalse()
        {
            MapEntry me = new MapEntry(1, "abc");

            Expect(me.containsKey(-4),False);
            Expect(me.containsKey(4), False);
        }


        [Test]
        public void EntryAtOnExistingKeyWorks()
        {
            MapEntry me = new MapEntry(1, "abc");
            IMapEntry me1 = me.entryAt(0);
            IMapEntry me2 = me.entryAt(1);

            Expect(me1.key(), EqualTo(0));
            Expect(me1.val(), EqualTo(1));
            Expect(me2.key(), EqualTo(1));
            Expect(me2.val(), EqualTo("abc"));
        }

        [Test]
        public void EntryAtOutOfRangeLowReturnsNull()
        {
            MapEntry me = new MapEntry(1, "abc");
            IMapEntry me1 = me.entryAt(-4);

            Expect(me1,Null);
        }

        [Test]
        public void EntryAtOutOfRangeHighReturnsNull()
        {
            MapEntry me = new MapEntry(1, "abc");
            IMapEntry me1 = me.entryAt(4);

            Expect(me1, Null);
        }

        [Test]
        public void ValAtOnExistingKeyReturnsValue()
        {
            MapEntry me = new MapEntry(1, "abc");

            Expect(me.valAt(0), EqualTo(1));
            Expect(me.valAt(1), EqualTo("abc"));
        }

        [Test]
        public void ValAtOnMissingKeyReturnsNull()
        {
            MapEntry me = new MapEntry(1, "abc");

            Expect(me.valAt(-4), Null);
            Expect(me.valAt(4), Null);
        }

        [Test]
        public void ValAtWithDefaultOnExistingKeyReturnsValue()
        {
            MapEntry me = new MapEntry(1, "abc");

            Expect(me.valAt(0,7), EqualTo(1));
            Expect(me.valAt(1,7), EqualTo("abc"));
        }

        [Test]
        public void ValAtWithDefaultOnMissingKeyReturnsDefault()
        {
            MapEntry me = new MapEntry(1, "abc");

            Expect(me.valAt(-4,7), EqualTo(7));
            Expect(me.valAt(4, 7), EqualTo(7));
        }

        #endregion
        
        #region Reversible tests

        [Test]
        public void RseqReturnReverseSeq()
        {
            MapEntry me = new MapEntry(1, "abc");

            ISeq s = me.rseq();

            Expect(s.count(), EqualTo(2));
            Expect(s.first(), EqualTo("abc"));
            Expect(s.next().first(), EqualTo(1));
            Expect(s.next().next(), Null);
        }
        
        #endregion
        
        #region IPersistentCollection tests

        [Test]
        public void CountIs2()
        {
            MapEntry me = new MapEntry(1, "abc");
            Expect(me.count(), EqualTo(2));
        }

        [Test]
        public void SeqReturnsASeq()
        {
            MapEntry me = new MapEntry(1, "abc");
            ISeq s = me.seq();

            Expect(s.count(), EqualTo(2));
            Expect(s.first(), EqualTo(1));
            Expect(s.next().first(), EqualTo("abc"));
            Expect(s.next().next(), Null);
        }

        [Test]
        public void EmptyReutrnsNull()
        {
            MapEntry me = new MapEntry(1, "abc");

            Expect(me.empty(), Null);
        }


        [Test]
        public void ExplictIPersistentCollectionConsWorks()
        {
            MapEntry me = new MapEntry(1, "abc");
            IPersistentCollection c = (IPersistentCollection)me;
            ISeq s = c.cons(2).seq();

            Expect(me.count(), EqualTo(2));
            Expect(me.key(), EqualTo(1));
            Expect(me.val(), EqualTo("abc"));

            Expect(s.count(), EqualTo(3));
            Expect(s.first(), EqualTo(1));
            Expect(s.next().first(), EqualTo("abc"));
            Expect(s.next().next().first(), EqualTo(2));
            Expect(s.next().next().next(), Null);
        }
        
        #endregion
        
        #region IPersistentStack tests

        [Test]
        public void PeekReturnsVal()
        {
            MapEntry me = new MapEntry(1, "abc");

            Expect(me.peek(), EqualTo("abc"));
            Expect(me.count(), EqualTo(2));
            Expect(me.key(), EqualTo(1));
            Expect(me.val(), EqualTo("abc"));
        }

        [Test]
        public void PopLosesTheValue()
        {
            MapEntry me = new MapEntry(1, "abc");
            IPersistentVector v = (IPersistentVector)me.pop();

            Expect(v.length(), EqualTo(1));
            Expect(v.nth(0), EqualTo(1));
        }
    


        
        #endregion

    }
}
