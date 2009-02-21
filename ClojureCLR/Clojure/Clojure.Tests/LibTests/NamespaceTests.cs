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
 
  //TODO: NEed to fix NS tests to clear the created namespaces after each test.
    /*
    [TestFixture]
    public class NamespaceTests : AssertionHelper
    {
        #region C-tor tests

        [Test]
        public void FindOrCreateCreatesANewNamespace()
        {
            Symbol sym = Symbol.intern("abc");
            Namespace ns = Namespace.findOrCreate(sym);

            Expect(ns, Not.Null);
            Expect(ns.Name, EqualTo(sym));
        }

        [Test]
        public void FindOrCreateFindsExistingNamespace()
        {
            Symbol sym1 = Symbol.intern("abc");
            Namespace ns1 = Namespace.findOrCreate(sym1);
            Symbol sym2 = Symbol.intern("abc");
            Namespace ns2 = Namespace.findOrCreate(sym2);

            Expect(Object.ReferenceEquals(ns1, ns2));
        }

        [Test]
        public void FindGivesNullOnMissingNS()
        {
            Symbol sym = Symbol.intern("abc");
            Namespace ns = Namespace.find(sym);

            Expect(ns, Null);
        }

        [Test]
        public void FindFindsExistingNS()
        {
            Symbol sym1 = Symbol.intern("abc");
            Namespace ns1 = Namespace.findOrCreate(sym1);
            Symbol sym2 = Symbol.intern("abc");
            Namespace ns2 = Namespace.find(sym2);

            Expect(Object.ReferenceEquals(ns1, ns2));
        }

        [Test]
        public void RemoveRemovesExisingNamespace()
        {
            Symbol sym1 = Symbol.intern("abc");
            Namespace ns1 = Namespace.findOrCreate(sym1);
            Symbol sym2 = Symbol.intern("abc");
            Namespace ns2 = Namespace.remove(sym2);
            Namespace ns3 = Namespace.find(sym1);

            Expect(object.ReferenceEquals(ns1, ns2));
            Expect(ns3, Null);
        }


        [Test]
        public void RemoveReturnsNullOnNonExisingNamespace()
        {
            Symbol sym2 = Symbol.intern("abc");
            Namespace ns2 = Namespace.remove(sym2);

            Expect(ns2, Null);
        }

        [Test]
        [ExpectedException(typeof(ArgumentException))]
        public void RemoveFailsRemovingClojureCoreNS()
        {
            Symbol sym = Symbol.intern("clojure.core");
            Namespace ns = Namespace.remove(sym);
        }


        // how to test the thread-safety of findOrCreatea?

        #endregion

        #region object override tests

        [Test]
        public void ToStringWorks()
        {
            // do we care all that much?
            Namespace ns = Namespace.findOrCreate(Symbol.intern("abc"));
            Expect(ns.ToString(),EqualTo("#<Namespace: abc>"));
        }
        
        #endregion

        #region Interning symbols tests

        [Test]
        [ExpectedException(typeof(ArgumentException))]
        public void InterningSymbolWithNamespaceFails()
        {
            Symbol sym = Symbol.intern("abc", "def");
            Namespace ns = Namespace.findOrCreate(Symbol.intern("ghi"));
            Var v = ns.intern(sym);
        }

        [Test]
        public void InterningSymbolCreatesVar()
        {
            Symbol sym = Symbol.intern("def");
            Namespace ns = Namespace.findOrCreate(Symbol.intern("abc"));

            Var v = ns.intern(sym);

            Expect(v, Not.Null);
            Expect(v.Namespace, EqualTo(ns));

        }

        [Test]
        public void InterningSymbolEntersVarInMap()
        {
            Symbol sym = Symbol.intern("def");
            Namespace ns = Namespace.findOrCreate(Symbol.intern("abc"));

            Var v = ns.intern(sym);

            Expect(ns.findInternedVar(sym), SameAs(v));
        }



        [Test]
        public void InterningSymbolAgainFindsVar()
        {
            Symbol sym = Symbol.intern("def");
            Namespace ns = Namespace.findOrCreate(Symbol.intern("abc"));

            Var v1 = ns.intern(sym);

            Var v2 = ns.intern(sym);

            Expect(Object.ReferenceEquals(v1, v2));
        }

        [Test]
        [ExpectedException(typeof(InvalidOperationException))]
        public void ReferSymbolToVarInOtherAndThenInterningThrows()
        {
            // I don't know why

            Namespace ns1 = Namespace.findOrCreate(Symbol.intern("abc"));

            Symbol sym1 = Symbol.intern("def");
            Var v1 = ns1.intern(sym1);

            Namespace ns2 = Namespace.findOrCreate(Symbol.intern("d"));
            Symbol sym2 = Symbol.intern("g");

            ns2.refer(sym2, v1);
            ns2.intern(sym2);
        }



        [Test]
        [ExpectedException(typeof(ArgumentException))]
        public void ReferOfSymbolWithNamespaceFails()
        {
            Symbol sym = Symbol.intern("abc", "def");
            Namespace ns = Namespace.findOrCreate(Symbol.intern("ghi"));
            ns.refer(sym, Var.create());
        }

        [Test]
        public void ReferEntersVar()
        {
            Namespace ns = Namespace.findOrCreate(Symbol.intern("abc"));

            Symbol sym = Symbol.intern("def");
            Var v = Var.create();
            ns.refer(sym, v);

            Expect(ns.getMapping(sym), SameAs(v));
        }

        [Test]
        [ExpectedException(typeof(ArgumentException))]
        public void ImportTypeOnSymbolWithNamespaceFails()
        {
            Symbol sym = Symbol.intern("abc", "def");
            Namespace ns = Namespace.findOrCreate(Symbol.intern("ghi"));
            ns.importType(sym, typeof(Int32)); 
        }

        [Test]
        public void ImportTypeEntersType()
        {
            Namespace ns = Namespace.findOrCreate(Symbol.intern("abc"));

            Symbol sym = Symbol.intern("def");
            ns.importType(sym, typeof(Int32));

            Expect(ns.getMapping(sym), SameAs(typeof(Int32)));
        }


        [Test]
        public void FindInternedVarFailsIfNonVarValueInMap()
        {
            Namespace ns = Namespace.findOrCreate(Symbol.intern("abc"));

            Symbol sym = Symbol.intern("def");
            ns.importType(sym, typeof(Int32));

            Var v = ns.findInternedVar(sym);

            Expect(v, Null);
        }
   

        // Don't know how to test the race condition in the loops for
        //  intern(Symbol), reference(Symbol), unmap(Symbol)

        #endregion
    }     
     */

}
