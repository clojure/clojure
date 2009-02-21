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
    public class KeywordTests : AssertionHelper
    {

        #region c-tor tests

        [Test]
        public void InternCreatesKeywordBasedOnSymbol()
        {
            Symbol sym = Symbol.intern("def","abc");
            Keyword k1 = Keyword.intern(sym);
            Expect(k1.Name,EqualTo(sym.Name));
            Expect(k1.Namespace,EqualTo(sym.Namespace));
        }

        [Test]
        public void InternReturnsSameKeywordOnEqualSym()
        {
            Symbol sym1 = Symbol.intern("def", "abc");
            Symbol sym2 = Symbol.intern("def", "abc");
            Keyword k1 = Keyword.intern(sym1);
            Keyword k2 = Keyword.intern(sym2);

            Expect(Object.ReferenceEquals(k1, k2));
        }

        [Test]
        public void Intern2CreatesKeywordBasedOnSymbol()
        {
            Keyword k1 = Keyword.intern("def","abc");
            Expect(k1.Name, EqualTo("abc"));
            Expect(k1.Namespace, EqualTo("def"));
        }

        [Test]
        public void Intern2ReturnsSameKeywordOnEqualSym()
        {
            Keyword k1 = Keyword.intern("def", "abc");
            Keyword k2 = Keyword.intern("def", "abc");

            Expect(Object.ReferenceEquals(k1, k2));
        }

        #endregion

        #region object override tests

        [Test]
        public void ToStringReturnsStringNameWithColonPrepended()
        {
            Symbol sym1 = Symbol.intern("abc");
            Symbol sym2 = Symbol.intern("abc/def");
            Keyword k1 = Keyword.intern(sym1);
            Keyword k2 = Keyword.intern(sym2);

            Expect(k1.ToString(), EqualTo(":abc"));
            Expect(k2.ToString(), EqualTo(":abc/def"));
        }

        [Test]
        public void EqualOnIdentityIsTrue()
        {
            Symbol sym1 = Symbol.intern("abc");
            Keyword k1 = Keyword.intern(sym1);

            Expect(k1.Equals(k1));
        }

        [Test]
        public void EqualsOnNonKeywordIsFalse()
        {
            Symbol sym1 = Symbol.intern("abc");
            Keyword k1 = Keyword.intern(sym1);

            Expect(k1.Equals(sym1), False);
        }

        //[Test]
        //public void EqualsDependsOnSym()
        //{
        //    Symbol sym1 = Symbol.intern("abc");
        //    Symbol sym2 = Symbol.intern("abc");
        //    Keyword k1 = Keyword.intern(sym1);
        //    Keyword k2 = Keyword.intern(sym2);
        //    // I don't know how we ever create two keywords that will force
        //    // the code to go into the sym.equals part of the code.
        //    // At least, not through the factory methods.
        //}

        [Test]
        public void HashCodeDependsOnValue()
        {
            Symbol sym1 = Symbol.intern("abc");
            Symbol sym2 = Symbol.intern("abc/def");
            Keyword k1 = Keyword.intern(sym1);
            Keyword k2 = Keyword.intern(sym2);

            Expect(k1.GetHashCode(), Not.EqualTo(k2.GetHashCode()));
        }


        #endregion

        #region Named tests

        public void NameAndNamespaceComeFromTheSymbol()
        {
            Symbol sym1 = Symbol.intern("def", "abc");
            Keyword k1 = Keyword.intern(sym1);
            Symbol sym2 = Symbol.intern("abc");
            Keyword k2 = Keyword.intern(sym2);
            Expect(k1.Name, EqualTo("abc"));
            Expect(k1.Namespace, EqualTo("def"));
            Expect(k2.Name, EqualTo("abc"));
            Expect(k2.Namespace, Null);
        }

        #endregion
        
        #region IFn Tests

        [Test]
        public void Invoke2IndexesIntoItsFirstArg()
        {
            Keyword k1 = Keyword.intern(Symbol.intern("abc"));
            Keyword k2 = Keyword.intern(Symbol.intern("ab"));

            IDictionary dict = new Hashtable();
            dict[k1] = 7;
            dict["abc"] = 8;

            Expect(k1.invoke(dict), EqualTo(7));
            Expect(k2.invoke(dict), Null);
        }

        [Test]
        public void Invoke3IndexesIntoItsFirstArg()
        {
            Keyword k1 = Keyword.intern(Symbol.intern("abc"));
            Keyword k2 = Keyword.intern(Symbol.intern("ab"));

            IDictionary dict = new Hashtable();
            dict[k1] = 7;
            dict["abc"] = 8;

            Expect(k1.invoke(dict, 20), EqualTo(7));
            Expect(k2.invoke(dict, 20), EqualTo(20));
        }

        [Test]
        [ExpectedException(typeof(ArgumentException))]
        public void InvokeOnNoArgsFails()
        {
            Keyword k1 = Keyword.intern(Symbol.intern("abc"));
            object o = k1.invoke();
        }

        [Test]
        [ExpectedException(typeof(ArgumentException))]
        public void InvokeOnTooManyArgsFails()
        {
            Keyword k1 = Keyword.intern(Symbol.intern("abc"));
            IDictionary dict = new Hashtable();
            dict[k1] = 7;
            dict["abc"] = 8;

            object o = k1.invoke(dict, 20, null);
        }
  
        #endregion

        #region IComparable tests

        [Test]
        [ExpectedException(typeof(InvalidCastException))]
        public void CompareToNonKeywordFails()
        {
            Symbol s1 = Symbol.intern("abc");
            Keyword k1 = Keyword.intern(s1);
            int c = k1.CompareTo(s1);
        }

        [Test]
        public void CompareToEqualKeywordIsZero()
        {
            Keyword k1 = Keyword.intern(Symbol.intern("abc"));
            Keyword k2 = Keyword.intern(Symbol.intern("abc"));

            Expect(k1.CompareTo(k2), EqualTo(0));
        }

        [Test]
        public void CompareDependsOnSymbolCompare()
        {
            Symbol sym1 = Symbol.intern("abc");
            Symbol sym2 = Symbol.intern("a", "abc");
            Symbol sym3 = Symbol.intern("b", "abc");
            Keyword k1 = Keyword.intern(sym1);
            Keyword k2 = Keyword.intern(sym2);
            Keyword k3 = Keyword.intern(sym3);

            Expect(k1.CompareTo(k2), LessThan(0));
            Expect(k2.CompareTo(k1), GreaterThan(0));
            Expect(k1.CompareTo(k3), LessThan(0));
            Expect(k3.CompareTo(k1), GreaterThan(0));
        }


        #endregion
    }

}
