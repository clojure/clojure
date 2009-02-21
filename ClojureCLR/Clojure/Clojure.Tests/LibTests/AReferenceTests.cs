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
    public class AReferenceTests : AssertionHelper
    {
        // AReference is abstract.  We need a class to instantiate.

        class ConcreteAReference : AReference
        {
            public ConcreteAReference() : base() { }
            public ConcreteAReference(IPersistentMap meta) : base(meta) { }
        }


        #region C-tor tests

        [Test]
        public void Default_ctor_creates_with_null_metadata()
        {
            ConcreteAReference c = new ConcreteAReference();
            Expect(c.meta(), Null);
        }

        [Test]
        public void Map_ctor_creates_with_given_metadata()
        {
            MockRepository mocks = new MockRepository();
            IPersistentMap meta = mocks.StrictMock<IPersistentMap>();
            mocks.ReplayAll();

            ConcreteAReference c = new ConcreteAReference(meta);
            Expect(c.meta(), EqualTo(meta));

            mocks.VerifyAll();
        }

        #endregion


        #region IReference tests

        [Test]
        public void AlterMeta_changes_meta()
        {
            MockRepository mocks = new MockRepository();
            IPersistentMap meta = mocks.StrictMock<IPersistentMap>();
            IFn fn = mocks.StrictMock<IFn>();
            RMExpect.Call(fn.applyTo(null)).IgnoreArguments().Return(meta);
            mocks.ReplayAll();

            ConcreteAReference c = new ConcreteAReference();
            c.alterMeta(fn, null);

            Expect(c.meta(), EqualTo(meta));
            mocks.VerifyAll();
        }

        [Test]
        public void ResetMeta_sets_meta()
        {
            MockRepository mocks = new MockRepository();
            IPersistentMap meta = mocks.StrictMock<IPersistentMap>();
            mocks.ReplayAll();

            ConcreteAReference c = new ConcreteAReference();
            c.resetMeta(meta);

            Expect(c.meta(), EqualTo(meta));
            mocks.VerifyAll();
        }


        #endregion
    }
}
