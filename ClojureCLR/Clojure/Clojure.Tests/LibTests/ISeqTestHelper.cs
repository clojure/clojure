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
using clojure.lang;

namespace Clojure.Tests.LibTests
{
    public class ISeqTestHelper : AssertionHelper
    {
        public void VerifyISeqContents(ISeq s, IList<object> values)
        {
            int i=0;

            for (; s != null; s = s.rest(), i++)
                Expect(s.first(), EqualTo(values[i]));

            Expect(i, EqualTo(values.Count));
        }

        public void VerifyISeqCons(ISeq s, object newVal, IList<object> values)
        {
            ISeq newSeq = s.cons(newVal);

            Expect(newSeq.first(), EqualTo(newVal));
            VerifyISeqContents(newSeq.rest(), values);
        }

        public void VerifyISeqRestTypes(ISeq s, Type type)
        {
            for ( ; s.rest() != null; s = s.rest())
                Expect(s.rest(), InstanceOfType(type));
        }

        public void VerifyISeqRestMaintainsMeta(ISeq s)
        {
            IPersistentMap meta = ((IMeta)s).meta();

            for (; s.rest() != null; s = s.rest())
                Expect(((IMeta)s.rest()).meta(), EqualTo(meta));
        }
    }
}
