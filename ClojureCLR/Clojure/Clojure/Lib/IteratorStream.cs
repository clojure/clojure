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
using System.Runtime.CompilerServices;

namespace clojure.lang
{

    /// <summary>
    /// Implements a stream running over an IEnumerator.
    /// </summary>
    public class IteratorStream : IStream
    {
        #region Data

        /// <summary>
        /// The IEnumerator being streamed.
        /// </summary>
        private readonly IEnumerator _iter;

        #endregion

        #region Ctors and factory methods

        /// <summary>
        /// Constructs an <see cref="IteratorStream">IteratorStream</see> over an IEnumerator.
        /// </summary>
        /// <param name="iter">The IEnumerator to stream over.</param>
        public IteratorStream(IEnumerator iter)
        {
            _iter = iter;
        }

        #endregion

        #region IStream Members

        /// <summary>
        /// Get the next value in the stream.
        /// </summary>
        /// <returns>The next value.</returns>
        [MethodImpl(MethodImplOptions.Synchronized)]
        public object next()
        {
            if (_iter.MoveNext())
                return _iter.Current;
            return RT.eos();
        }

       

        #endregion
    }
}
