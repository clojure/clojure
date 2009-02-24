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

namespace clojure.lang
{
    // TODO: ArrayStream needs some thought.
    public class ArrayStream : IStream
    {
        #region Data

        readonly AtomicInteger _idx = new AtomicInteger(0);
        readonly Array _array;

        #endregion

        #region C-tors & factory methods

        public ArrayStream(Array array)
        {
            _array = array;
        }

        public static IStream createFromObject(object array)
        {
            if (array.GetType().IsArray)
                return new ArrayStream((Array)array);
            // TODO: Decide if we want all the specialized types.
            // TODO: make sure we don't have a multi-dim array

            throw new ArgumentException(String.Format("Unsupported array type %s", array.GetType()));
        }


        #endregion

        #region IStream Members

        public object next()
        {
            int i = _idx.getAndIncrement();
            if (i < _array.Length)
                return _array.GetValue(i);
            return RT.eos();
        }

        #endregion
    }
}
