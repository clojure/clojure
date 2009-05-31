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

    // TODO: Add tests for APersistentVector.SubVector
    // TODO: Add tests for APersistentVector.stream

    [TestFixture]
    public class APersistentVectorTests : AssertionHelper
    {

        // Usually, we test the abstract classes via the simplest concrete class that derives from it.
        // For APersistentVector, all the concrete classes are fairly complicated.
        // Hence we create a test concrete implementation class.
        // This class has no guarantees of persistence/immutability, thread-safety,
        //   or much of anything else, certainly not efficiency.
        // We determined the methods to override by trying to compile the class with no methods.
        // Thus, we have implemented only the absolute minimum.
        // We will write tests for these methods, too.
        // This class just has an underlying  List<int> to hold values.
        class CPV : APersistentVector
        {
            object[] _values;

            public CPV(object[] values)
                : base(null)
            {
                _values = values;
            }


            public CPV(IPersistentMap meta, object[] values)
                : base(meta)
            {
                _values = values;
            }

            public override IObj withMeta(IPersistentMap meta)
            {
                return meta == _meta
                    ? this
                    : new CPV(meta, _values);
            }

            //public override object applyTo(ISeq arglist)
            //{
            //    throw new NotImplementedException();
            //}

            public override IPersistentStack pop()
            {
                if (_values.Length == 0)
                    throw new InvalidOperationException("Can't pop a null stack.");

                object[] newArr = new object[_values.Length - 1];
                Array.Copy(_values, newArr, _values.Length - 1);

                return new CPV(_meta, newArr);
            }

            public override IPersistentVector cons(object o)
            {
                object[] newArr = new object[_values.Length + 1];
                _values.CopyTo(newArr, 0);
                newArr[_values.Length] = o;
                return new CPV(_meta, newArr);
            }

            public override IPersistentVector assocN(int i, object val)
            {
                if ( 0 <= i && i < _values.Length )
                {
                    object[] newArr = new object[_values.Length];
                    _values.CopyTo(newArr, 0);
                    newArr[i] = val;
                    return new CPV(_meta, newArr);
                }
                if ( i == _values.Length )
                    return cons(val);
                throw new IndexOutOfRangeException();
            }

            public override object nth(int i)
            {
                return _values[i];
            }

            public override int length()
            {
                return _values.Length;
            }

            private static CPV EMPTY = new CPV(new object[0]);
            public override IPersistentCollection empty()
            {
                return EMPTY;
            }

            public override int count()
            {
                return _values.Length;
            }
        }

        #region C-tor tests

        [Test]
        public void NoMetaCtorHasNoMeta()
        {
            CPV v = new CPV(new object[] { 1, 2, 3 });

            Expect(v.meta(),Null);
        }

        [Test]
        public void MetaCtorHasMeta()
        {
            MockRepository mocks = new MockRepository();
            IPersistentMap meta = mocks.StrictMock<IPersistentMap>();
            mocks.ReplayAll();

            CPV v = new CPV(meta,new object[] { 1, 2, 3 });

            Expect(v.meta(), SameAs(meta));
            mocks.VerifyAll();
        }

        #endregion

        #region Object tests

        [Test]
        public void ToStringMentionsTheCount()
        {
            CPV v = new CPV(new object[] { 1, 2, 3 });
            string str = v.ToString();

            Expect(str.Contains("3"));
        }

        [Test]
        public void HashCodeRepeats()
        {
            CPV v = new CPV(new object[] { 1, 2, 3 });

            Expect(v.GetHashCode(), EqualTo(v.GetHashCode()));
        }

        [Test]
        public void HashCodeDependsOnItems()
        {
            CPV v1 = new CPV(new object[] { 1, 2, 3 });
            CPV v2 = new CPV(new object[] { 1, 2, 4 });

            Expect(v1.GetHashCode(), Not.EqualTo(v2.GetHashCode()));
        }

        [Test]
        public void EqualsOnNonPersistentVectorIsFalse()
        {
            CPV v1 = new CPV(new object[] { 1, 2, 3 });

            Expect(v1.equiv(7), False);
        }

        [Test]
        public void EqualsOnPersistentVectorWithDifferentItemsIsFalse()
        {
            CPV v1 = new CPV(new object[] { 1, 2, 3 });
            CPV v2 = new CPV(new object[] { 1, 2, 4 });
            CPV v3 = new CPV(new object[] { 1, 2 });
            CPV v4 = new CPV(new object[] { 1, 2, 3, 4 });

            Expect(v1.equiv(v2), False);
            Expect(v1.equiv(v3), False);
            Expect(v1.equiv(v4), False);
        }

        [Test]
        public void EqualsOnPersistentVectorWithSameItemsIsTrue()
        {
            CPV v1 = new CPV(new object[] { 1, 2, 3 });
            CPV v2 = new CPV(new object[] { 1, 2, 3 });
            CPV v3 = new CPV(new object[] { 1 });
            CPV v4 = new CPV(new object[] { 1 });
            CPV v5 = new CPV(new object[] { });
            CPV v6 = new CPV(new object[] { });

            Expect(v1.equiv(v2));
            Expect(v3.equiv(v4));
            Expect(v5.equiv(v6));
        }

        [Test]
        public void EqualsOnSimilarISeqWorks()
        {
            CPV v1 = new CPV(new object[] { 'a', 'b', 'c' });
            StringSeq s1 = StringSeq.create("abc");

            Expect(v1.equiv(s1));
        }

        [Test]
        public void EqualsOnDissimilarISeqFails()
        {
            CPV v1 = new CPV(new object[] { 'a', 'b', 'c' });
            StringSeq s1 = StringSeq.create("ab");
            StringSeq s2 = StringSeq.create("abd");
            StringSeq s3 = StringSeq.create("abcd");

            Expect(v1.equiv(s1), False);
            Expect(v1.equiv(s2), False);
            Expect(v1.equiv(s3), False);
        }


        #endregion

        #region IFn tests

        [Test]
        public void InvokeCallsNth()
        {
            CPV v = new CPV(new object[] { 5, 6, 7 });

            Expect(v.invoke(0),EqualTo(5));
            Expect(v.invoke(1),EqualTo(6));
            Expect(v.invoke(2),EqualTo(7));
            Expect(v.invoke("1"), EqualTo(6));
            Expect(v.invoke(1.0), EqualTo(6));
            Expect(v.invoke(1.2), EqualTo(6));
            Expect(v.invoke(1.8), EqualTo(6)); // Rounds or not-- should it?
            Expect(v.invoke(1.4M), EqualTo(6));
        }


        #endregion

        #region IPersistentCollection tests

        [Test]
        public void SeqOnCount0YieldsNull()
        {
            CPV v = new CPV(new object[0]);

            Expect(v.seq(), Null);
        }

        [Test]
        public void SeqOnPositiveCountYieldsNotNull()
        {
            CPV v = new CPV(new object[]{ 1,2,3});

            Expect(v.seq(), Not.Null);
        }

        [Test]
        public void SeqOnPositiveCountYieldsValidSequence()
        {
            CPV v = new CPV(new object[] { 1, 2, 3 });
            ISeq s = v.seq();

            Expect(s.first(), EqualTo(1));
            Expect(s.next().first(), EqualTo(2));
            Expect(s.next().next().first(), EqualTo(3));
            Expect(s.next().next().next(), Null);
        }

        [Test]
        public void Explicit_IPersistentCollection_cons_works()
        {
            CPV v = new CPV(new object[] { 1, 2 });
            IPersistentCollection c = v as IPersistentCollection;

            Expect(c, Not.Null);

            IPersistentCollection c2 = c.cons(3);
            Expect(c2.count(), EqualTo(3));

            ISeq s2 = c2.seq();

            Expect(s2.first(), EqualTo(1));
            Expect(s2.next().first(), EqualTo(2));
            Expect(s2.next().next().first(), EqualTo(3));
            Expect(s2.next().next().next(), Null);
        }

        #endregion

        #region Reversible tests

        [Test]
        public void RseqOnCount0YieldsNull()
        {
            CPV v = new CPV(new object[0]);

            Expect(v.rseq(), Null);
        }

        [Test]
        public void RSeqOnPositiveCountYieldsNotNull()
        {
            CPV v = new CPV(new object[] { 1, 2, 3 });

            Expect(v.rseq(), Not.Null);
        }

        [Test]
        public void RseqOnPositiveCountYieldsValidSequence()
        {
            CPV v = new CPV(new object[] { 1, 2, 3 });
            ISeq s = v.rseq();

            Expect(s.first(), EqualTo(3));
            Expect(s.next().first(), EqualTo(2));
            Expect(s.next().next().first(), EqualTo(1));
            Expect(s.next().next().next(), Null);
        }


        #endregion

        #region Associative tests

        [Test]
        public void ContainsKeyOnNonNumericIsFalse()
        {
            CPV v = new CPV(new object[] { 4, 5, 6 });

            Expect(v.containsKey("a"), False);
        }

        [Test]
        public void ContainsKeyOnIndexInRangeIsTrue()
        {
            CPV v = new CPV(new object[] { 4, 5, 6 });

            Expect(v.containsKey(1.2));
        }


        [Test]
        public void ContainsKeyOnIndexOutOfRangeIsFalse()
        {
            CPV v = new CPV(new object[] { 4, 5, 6 });

            Expect(v.containsKey(5),False);
        }

        [Test]
        public void EntryAtOnNonNumericReturnsNull()
        {
            CPV v = new CPV(new object[] { 4, 5, 6 });

            IMapEntry me = v.entryAt("a");

            Expect(me, Null);
        }

        [Test]
        public void EntryAtOnIndexInRangeReturnsEntry()
        {
            CPV v = new CPV(new object[] { 4, 5, 6 });

            IMapEntry me = v.entryAt(1);

            Expect(me.key(), EqualTo(1));
            Expect(me.val(),EqualTo(5));
        }


        [Test]
        public void EntryAtOnIndexOutOfRangeReturnsNull()
        {
            CPV v = new CPV(new object[] { 4, 5, 6 });

            IMapEntry me = v.entryAt(5);

            Expect(me, Null);
        }

        [Test]
        [ExpectedException(typeof(ArgumentException))]
        public void AssocWithNonNumericKeyThrowsException()
        {
            CPV v = new CPV(new object[] { 4, 5, 6 });
            Associative a = v.assoc("a", 7);
        }

        [Test]
        public void AssocWithNumericKeyInRangeChangesValue()
        {
            //This just checks that APersistentVector.assoc calls CPV.assocN
            CPV v = new CPV(new object[] { 4, 5, 6 });
            Associative a = v.assoc(1, 10);

            Expect(a.valAt(0), EqualTo(4));
            Expect(a.valAt(1), EqualTo(10));
            Expect(a.valAt(2), EqualTo(6));
            Expect(a.count(), EqualTo(3));
        }

        [Test]
        public void AssocWithNumericKeyOnePastEndAddValue()
        {
            //This just checks that APersistentVector.assoc calls CPV.assocN
            CPV v = new CPV(new object[] { 4, 5, 6 });
            Associative a = v.assoc(3, 10);

            Expect(a.valAt(0), EqualTo(4));
            Expect(a.valAt(1), EqualTo(5));
            Expect(a.valAt(2), EqualTo(6));
            Expect(a.valAt(3), EqualTo(10));
            Expect(a.count(), EqualTo(4));
        }

        [Test]
        [ExpectedException(typeof(IndexOutOfRangeException))]
        public void AssocWithNumericKeyOutOfRangeHighThrowsException()
        {
            //This just checks that APersistentVector.assoc calls CPV.assocN
            CPV v = new CPV(new object[] { 4, 5, 6 });
            Associative a = v.assoc(4, 10);
        }

        [Test]
        [ExpectedException(typeof(IndexOutOfRangeException))]
        public void AssocWithNumericKeyOutOfRangeLowThrowsException()
        {
            //This just checks that APersistentVector.assoc calls CPV.assocN
            CPV v = new CPV(new object[] { 4, 5, 6 });
            Associative a = v.assoc(-1, 10);
        }

        [Test]
        public void ValAtOnNonNumericReturnsDefault()
        {
            CPV v = new CPV(new object[] { 4, 5, 6 });

            object val1 = v.valAt("a");
            object val2 = v.valAt("a", "abc");

            Expect(val1, Null);
            Expect(val2, EqualTo("abc"));
        }

        [Test]
        public void ValAtOnIndexInRangeReturnsEntry()
        {
            CPV v = new CPV(new object[] { 4, 5, 6 });

            object val1 = v.valAt(1);
            object val2 = v.valAt(1, "abc");

            Expect(val1, EqualTo(5));
            Expect(val2, EqualTo(5));
        }


        [Test]
        public void ValAtOnIndexOutOfRangeReturnsDefault()
        {
            CPV v = new CPV(new object[] { 4, 5, 6 });

            IMapEntry me = v.entryAt(5);

            object val1 = v.valAt(4);
            object val2 = v.valAt(4, "abc");

            Expect(val1, Null);
            Expect(val2, EqualTo("abc"));
        }



        #endregion

        #region IPersistentStack tests

        [Test]
        public void PeekOnCount0ReturnsNull()
        {
            CPV v = new CPV(new object[] {});

            Expect(v.peek(), Null);
        }

        [Test]
        public void PeekOnPositiveCountReturnsLastItem()
        {
            CPV v = new CPV(new object[] { 1, 2, 3 });

            Expect(v.peek(), EqualTo(3));
        }

        #endregion

        #region APersistentVector.Seq tests

        // We'll do all the tests indirectly.

        [Test]
        public void SeqFirstAndRestWork()
        {
            CPV v = new CPV(new object[] { 4, 5, 6 }); 
            ISeq s = v.seq();

            Expect(s.first(), EqualTo(4));
            Expect(s.next().first(), EqualTo(5));
            Expect(s.next().next().first(), EqualTo(6));
            Expect(s.next().next().next(), Null);
        }

        [Test]
        public void SeqIndexedWorks()
        {
            CPV v = new CPV(new object[] { 4, 5, 6 });
            ISeq s0 = v.seq();
            IndexedSeq i0 = s0 as IndexedSeq;

            ISeq s1 = s0.next();
            IndexedSeq i1 = s1 as IndexedSeq;

            Expect(i0.index(), EqualTo(0));
            Expect(i1.index(), EqualTo(1));
        }

        [Test]
        public void SeqCountWorks()
        {
            CPV v = new CPV(new object[] { 4, 5, 6 });
            ISeq s = v.seq();

            Expect(s.count(), EqualTo(3));
            Expect(s.next().count(), EqualTo(2));
            Expect(s.next().next().count(), EqualTo(1));
        }

        [Test]
        public void SeqWithMetaHasMeta()
        {
            MockRepository mocks = new MockRepository();
            IPersistentMap meta = mocks.StrictMock<IPersistentMap>();
            mocks.ReplayAll();

            CPV v = new CPV(new object[] { 4, 5, 6 });
            IObj s = (IObj)v.seq();
            IObj obj = s.withMeta(meta);

            Expect(obj.meta(), SameAs(meta));
            mocks.VerifyAll();
        }

        [Test]
        public void SeqReduceWithNoStartIterates()
        {
            MockRepository mocks = new MockRepository();
            IFn fn = mocks.StrictMock<IFn>();
            RMExpect.Call(fn.invoke(1, 2)).Return(5);
            RMExpect.Call(fn.invoke(5, 3)).Return(7);
            mocks.ReplayAll();

            CPV v = new CPV(new object[] { 1, 2, 3 });
            IReduce r = (IReduce)v.seq();
            object ret = r.reduce(fn);

            Expect(ret, EqualTo(7));
            mocks.VerifyAll();
        }

        [Test]
        public void SeqReduceWithStartIterates()
        {
            MockRepository mocks = new MockRepository();
            IFn fn = mocks.StrictMock<IFn>();
            RMExpect.Call(fn.invoke(20, 1)).Return(10);
            RMExpect.Call(fn.invoke(10, 2)).Return(5);
            RMExpect.Call(fn.invoke(5, 3)).Return(7);
            mocks.ReplayAll();

            CPV v = new CPV(new object[] { 1, 2, 3 });
            IReduce r = (IReduce)v.seq();
            object ret = r.reduce(fn, 20);

            Expect(ret, EqualTo(7));
            mocks.VerifyAll();
        }

        #endregion

        #region APersistentVector.RSeq tests

        // We'll do all the tests indirectly.

        [Test]
        public void RSeqFirstAndRestWork()
        {
            CPV v = new CPV(new object[] { 4, 5, 6 });
            ISeq s = v.rseq();

            Expect(s.first(), EqualTo(6));
            Expect(s.next().first(), EqualTo(5));
            Expect(s.next().next().first(), EqualTo(4));
            Expect(s.next().next().next(), Null);
        }

        [Test]
        public void RSeqIndexedWorks()
        {
            CPV v = new CPV(new object[] { 4, 5, 6 });
            ISeq s0 = v.rseq();
            IndexedSeq i0 = s0 as IndexedSeq;

            ISeq s1 = s0.next();
            IndexedSeq i1 = s1 as IndexedSeq;

            Expect(i0.index(), EqualTo(0));
            Expect(i1.index(), EqualTo(1));
        }

        [Test]
        public void RSeqCountWorks()
        {
            CPV v = new CPV(new object[] { 4, 5, 6 });
            ISeq s = v.rseq();

            Expect(s.count(), EqualTo(3));
            Expect(s.next().count(), EqualTo(2));
            Expect(s.next().next().count(), EqualTo(1));
        }

        [Test]
        public void RSeqWithMetaHasMeta()
        {
            MockRepository mocks = new MockRepository();
            IPersistentMap meta = mocks.StrictMock<IPersistentMap>();
            mocks.ReplayAll();

            CPV v = new CPV(new object[] { 4, 5, 6 });
            IObj s = (IObj)v.rseq();
            IObj obj = s.withMeta(meta);

            Expect(obj.meta(), SameAs(meta));
            mocks.VerifyAll();
        }

        [Test]
        public void RSeqReduceWithNoStartIterates()
        {
            MockRepository mocks = new MockRepository();
            IFn fn = mocks.StrictMock<IFn>();
            RMExpect.Call(fn.invoke(3, 2)).Return(5);
            RMExpect.Call(fn.invoke(5, 1)).Return(7);
            mocks.ReplayAll();

            CPV v = new CPV(new object[] { 1, 2, 3 });
            IReduce r = (IReduce)v.rseq();
            object ret = r.reduce(fn);

            Expect(ret, EqualTo(7));
            mocks.VerifyAll();
        }

        [Test]
        public void RSeqReduceWithStartIterates()
        {
            MockRepository mocks = new MockRepository();
            IFn fn = mocks.StrictMock<IFn>();
            RMExpect.Call(fn.invoke(20, 3)).Return(10);
            RMExpect.Call(fn.invoke(10, 2)).Return(5);
            RMExpect.Call(fn.invoke(5, 1)).Return(7);
            mocks.ReplayAll();

            CPV v = new CPV(new object[] { 1, 2, 3 });
            IReduce r = (IReduce)v.rseq();
            object ret = r.reduce(fn, 20);

            Expect(ret, EqualTo(7));
            mocks.VerifyAll();
        }

        #endregion

    }
}
