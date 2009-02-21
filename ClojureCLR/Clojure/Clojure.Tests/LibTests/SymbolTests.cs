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
    public class SymbolTests : AssertionHelper
    {

        #region c-tor tests

        [Test]
        public void Intern2CreatesSymbolWithNoNS()
        {
            Symbol sym = Symbol.intern(null, "abc");

            Expect(sym.Name, EqualTo("abc"));
            Expect(sym.Namespace, Null);
            Expect(sym.meta(), Null);
        }

        [Test]
        public void Intern2CreatesSymbolWithNS()
        {
            Symbol sym = Symbol.intern("def", "abc");

            Expect(sym.Name, EqualTo("abc"));
            Expect(sym.Namespace, EqualTo("def"));
            Expect(sym.meta(), Null);
        }

        [Test]
        public void Intern2InternsStrings()
        {
            String symname = new StringBuilder().Append("ab").Append("c").ToString();
            String nsname  = new StringBuilder().Append("ab").Append("c").ToString();

            Symbol sym1 = Symbol.intern(nsname,symname);
            Symbol sym2 = Symbol.intern(nsname,symname);

            Expect(String.IsInterned(sym1.Name),Not.Null);
            Expect(String.IsInterned(sym1.Namespace), Not.Null);
            Expect(Object.ReferenceEquals(sym1.Name, sym2.Name));
            Expect(Object.ReferenceEquals(sym1.Namespace, sym2.Namespace));
            Expect(object.ReferenceEquals(sym1.Name, symname), False);
        }



        [Test]
        public void Intern1CreatesSymbolWithNoNS()
        {
            Symbol sym = Symbol.intern("abc");

            Expect(sym.Name, EqualTo("abc"));
            Expect(sym.Namespace, Null);
            Expect(sym.meta(), Null);
        }

        [Test]
        public void Intern1CreatesSymbolWithNS()
        {
            Symbol sym = Symbol.intern("def/abc");

            Expect(sym.Name, EqualTo("abc"));
            Expect(sym.Namespace, EqualTo("def"));
            Expect(sym.meta(), Null);
        }


        [Test]
        public void Intern1CreatesSymbolWithNSFromLastSlash()
        {
            Symbol sym = Symbol.intern("ghi/def/abc");

            Expect(sym.Name, EqualTo("abc"));
            Expect(sym.Namespace, EqualTo("ghi/def"));
            Expect(sym.meta(), Null);
        }

        [Test]
        public void Intern1InternsStrings()
        {
            String name = new StringBuilder().Append("def/").Append("abc").ToString();

            Symbol sym1 = Symbol.intern(name);
            Symbol sym2 = Symbol.intern(name);

            Expect(String.IsInterned(sym1.Name), Not.Null);
            Expect(String.IsInterned(sym1.Namespace), Not.Null);
            Expect(Object.ReferenceEquals(sym1.Name, sym2.Name));
            Expect(Object.ReferenceEquals(sym1.Namespace, sym2.Namespace));
        }

        #endregion

        #region Object overrides

        [Test]
        public void SymToStringWithNoNSIsJustName()
        {
            Symbol sym = Symbol.intern("abc");
            Expect(sym.ToString(), EqualTo("abc"));
        }

        [Test]
        public void SymToStringWithNsConcatenatesNames()
        {
            Symbol sym = Symbol.intern("def", "abc");
            Expect(sym.ToString(), EqualTo("def/abc"));
        }

        [Test]
        public void EqualsOnIdentityIsTrue()
        {
            Symbol sym = Symbol.intern("abc");
            Expect(sym.Equals(sym));
        }

        [Test]
        public void EqualsOnNonSymbolIsFalse()
        {
            Symbol sym = Symbol.intern("abc");
            Expect(sym.Equals("abc"), False);
        }

        [Test]
        public void EqualsOnDissimilarSymbolIsFalse()
        {
            Symbol sym1 = Symbol.intern("abc");
            Symbol sym2 = Symbol.intern("ab");
            Symbol sym3 = Symbol.intern("def", "abc");
            Symbol sym4 = Symbol.intern("de","abc");

            Expect(sym1.Equals(sym2), False);
            Expect(sym1.Equals(sym3), False);
            Expect(sym3.Equals(sym4), False);
        }

        [Test]
        public void EqualsOnSimilarSymbolIsTrue()
        {

            Symbol sym1 = Symbol.intern("abc");
            Symbol sym2 = Symbol.intern("abc");
            Symbol sym3 = Symbol.intern("def", "abc");
            Symbol sym4 = Symbol.intern("def", "abc");

            Expect(sym1.Equals(sym2));
            Expect(sym3.Equals(sym4));
        }

        [Test]
        public void HashCodeDependsOnNames()
        {
            Symbol sym1 = Symbol.intern("abc");
            Symbol sym2 = Symbol.intern("abc");
            Symbol sym3 = Symbol.intern("def", "abc");
            Symbol sym4 = Symbol.intern("def", "abc");
            Symbol sym5 = Symbol.intern("ab");
            Symbol sym6 = Symbol.intern("de", "abc");

            Expect(sym1.GetHashCode(), EqualTo(sym2.GetHashCode()));
            Expect(sym3.GetHashCode(), EqualTo(sym4.GetHashCode()));
            Expect(sym1.GetHashCode(), Not.EqualTo(sym3.GetHashCode()));
            Expect(sym1.GetHashCode(), Not.EqualTo(sym5.GetHashCode()));
            Expect(sym3.GetHashCode(), Not.EqualTo(sym6.GetHashCode()));
        }

        #endregion

        #region Named tests

        // We've been testing these all along.

        #endregion

        #region IFn tests

        [Test]
        public void Invoke2IndexesIntoItsFirstArg()
        {
            Symbol sym1 = Symbol.intern("abc");
            Symbol sym2 = Symbol.intern("abc");
            Symbol sym3 = Symbol.intern("ab");

            IDictionary dict = new Hashtable();
            dict[sym1] = 7;
            dict["abc"] = 8;

            Expect(sym1.invoke(dict), EqualTo(7));
            Expect(sym2.invoke(dict), EqualTo(7));
            Expect(sym3.invoke(dict), Null);
        }

        [Test]
        public void Invoke3IndexesIntoItsFirstArg()
        {
            Symbol sym1 = Symbol.intern("abc");
            Symbol sym2 = Symbol.intern("abc");
            Symbol sym3 = Symbol.intern("ab");

            IDictionary dict = new Hashtable();
            dict[sym1] = 7;
            dict["abc"] = 8;

            Expect(sym1.invoke(dict,20), EqualTo(7));
            Expect(sym2.invoke(dict,20), EqualTo(7));
            Expect(sym3.invoke(dict,20), EqualTo(20));
        }

        [Test]
        [ExpectedException(typeof(ArgumentException))]
        public void InvokeOnNoArgsFails()
        {
            Symbol sym1 = Symbol.intern("abc");
            object o = sym1.invoke();
        }

        [Test]
        [ExpectedException(typeof(ArgumentException))]
        public void InvokeOnTooManyArgsFails()
        {
            Symbol sym1 = Symbol.intern("abc");
            IDictionary dict = new Hashtable();
            dict[sym1] = 7;
            dict["abc"] = 8;

            object o = sym1.invoke(dict,20,null);
        }
  
        #endregion

        #region IComparable tests

        [Test]
        [ExpectedException(typeof(InvalidOperationException))]
        public void CompareToNonSymbolFails()
        {
            Symbol sym1 = Symbol.intern("abc");
            int c = sym1.CompareTo("abc");
        }

        [Test]
        public void CompareToEqualSymbolIsZero()
        {
            Symbol sym1 = Symbol.intern("abc");
            Symbol sym2 = Symbol.intern("abc");

            Expect(sym1.CompareTo(sym2), EqualTo(0));
        }

        [Test]
        public void NullNSIsLessThanNonNullNS()
        {
            Symbol sym1 = Symbol.intern("abc");
            Symbol sym2 = Symbol.intern("a", "abc");

            Expect(sym1.CompareTo(sym2), LessThan(0));
            Expect(sym2.CompareTo(sym1), GreaterThan(0));
        }

        [Test]
        public void DissimilarNSCompareOnNS()
        {
            Symbol sym1 = Symbol.intern("a", "abc");
            Symbol sym2 = Symbol.intern("b", "abc");

            Expect(sym1.CompareTo(sym2), LessThan(0));
            Expect(sym2.CompareTo(sym1), GreaterThan(0));
        }

        #endregion

    }

    [TestFixture]
    public class Symbol_IObj_Tests : IObjTests
    {
        MockRepository _mocks;

        [SetUp]
        public void Setup()
        {
            _mocks = new MockRepository();
            IPersistentMap meta = _mocks.StrictMock<IPersistentMap>();
            _mocks.ReplayAll();

            Symbol sym1 = Symbol.intern("def", "abc");

            _objWithNullMeta = (IObj)sym1;
            _obj = _objWithNullMeta.withMeta(meta);
            _expectedType = typeof(Symbol);
        }

        [TearDown]
        public void Teardown()
       { 
            _mocks.VerifyAll();
        }

    }

}
