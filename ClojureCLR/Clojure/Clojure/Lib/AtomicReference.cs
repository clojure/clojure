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
using System.Threading;

namespace clojure.lang
{
    /// <summary>
    /// Implements the Java java.util.concurrent.atomic.AtomicReference&lt;V&gt; class
    /// </summary>
    /// <remarks>I hope.  Someone with more knowledge of these things should check this out.
    /// <para>Not implemented: weakCompareAndSet</para></remarks>
    [Serializable]
    public sealed class AtomicReference<T> where T : class
    {
        #region Data
        
        /// <summary>
        /// The current value.
        /// </summary>
        private T _ref;
        
        #endregion

        #region C-tors

        /// <summary>
        /// Intializes an <see cref="AtomicReference">AtomicReference</see> to null.
        /// </summary>
        public AtomicReference()
            : this(null)
        {
        }

        /// <summary>
        /// Intializes an <see cref="AtomicReference">AtomicReference</see> to hold a given reference.
        /// </summary>
        /// <param name="initVal">The initial value.</param>
        public AtomicReference(T initVal)
        {
            _ref = initVal;
        }

        #endregion

        #region Methods

        /// <summary>
        /// Sets the reference if the expected reference is current.
        /// </summary>
        /// <param name="expect">The expected value.</param>
        /// <param name="update">The new value.</param>
        /// <returns><value>true</value> if the value is changed; <value>false</value> otherwise.</returns>
        /// <remarks>The Java version returns a boolean.  The BCL version returns the old value.  
        /// Is ReferenceEquals the correct comparison?  (That is what CompareExchange uses, so I'm guessing so.)</remarks>
        public bool CompareAndSet(T expect, T update)
        {
            T oldVal = Interlocked.CompareExchange<T>(ref _ref, update, expect);
            return Object.ReferenceEquals(oldVal,expect);  // TODO: (Or does it use Equals?)
        }

        /// <summary>
        /// Get the current value.
        /// </summary>
        /// <returns>The current value.</returns>
        public T Get()
        {
            return _ref;
        }

        /// <summary>
        /// Sets a new value and returns the original value.
        /// </summary>
        /// <param name="update">The new value.</param>
        /// <returns>The original value.</returns>
        public T GetAndSet(T update)
        {
            return Interlocked.Exchange(ref _ref, update);
        }

        /// <summary>
        /// Sets the value.
        /// </summary>
        /// <param name="update">The new value.</param>
        public void Set(T update)
        {
            Interlocked.Exchange(ref _ref, update);
        }

        #endregion

        #region object overrides

        /// <summary>
        /// Returns string representing the value.
        /// </summary>
        /// <returns>A string representing the value.</returns>
        public override string ToString()
        {
            return _ref.ToString();
        }

        #endregion
    }
}


