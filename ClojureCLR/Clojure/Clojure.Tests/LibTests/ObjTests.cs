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


namespace Clojure.Tests.LibTests
{
    [TestFixture]
    public class ObjTests : IObjTests
    {

        class MockObj : Obj
        {
            public MockObj()
            {
            }

            public MockObj(IPersistentMap meta)
                : base(meta)
            {
            }

            public override IObj withMeta(IPersistentMap meta)
            {
                return meta == _meta
                    ? this
                    : new MockObj(meta);
            }
        }

        MockRepository _mocks;

        [SetUp]
        public void Setup()
        {
            _mocks = new MockRepository();
            IPersistentMap meta = _mocks.StrictMock<IPersistentMap>();
            _mocks.ReplayAll();

            _objWithNullMeta = new MockObj();
            _obj = new MockObj(meta);
            _expectedType = typeof(MockObj);
        }
        
        [TearDown]
        public void Teardown()
        {
            _mocks.VerifyAll();
        }
    }
}

