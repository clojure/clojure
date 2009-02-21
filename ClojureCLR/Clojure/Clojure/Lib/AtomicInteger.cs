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
    /// Implements the Java <c>java.util.concurrent.atomic.AtomicInteger</c> class.  
    /// </summary>
    /// <remarks>I hope.  Someone with more knowledge of these things should check this out.</remarks>
    public sealed class AtomicInteger
    {
        #region Data

        /// <summary>
        /// The current <see cref="Int32">integer</see> value.
        /// </summary>
        int _val;

        #endregion

        #region C-tors

        /// <summary>
        /// Initializes an <see cref="AtomicInteger">AtomicInteger</see> with value zero.
        /// </summary>
        public AtomicInteger()
        {
            _val = 0;
        }

        /// <summary>
        /// Initializes an <see cref="AtomicInteger">AtomicInteger</see> with a given value.
        /// </summary>
        /// <param name="initVal">The initial value.</param>
        public AtomicInteger(int initVal)
        {
            _val = initVal;
        }

        #endregion

        #region Value access

        /// <summary>
        /// Gets the current value.
        /// </summary>
        /// <returns>The current value.</returns>
        public int get() 
        {
            return _val;
        }

        /// <summary>
        /// Increments the value and returns the new value.
        /// </summary>
        /// <returns>The new value.</returns>
        public int incrementAndGet()
        {
            return Interlocked.Increment(ref _val);
        }

        /// <summary>
        /// Increments the value and returns the original value.
        /// </summary>
        /// <returns>The original value.</returns>
        public int getAndIncrement()
        {
            return Interlocked.Increment(ref _val)-1;
        }

        /// <summary>
        /// Decrements the value and returns the new value.
        /// </summary>
        /// <returns>The new value.</returns>
        public int decrementAndGet()
        {
            return Interlocked.Decrement(ref _val);
        }

        /// <summary>
        /// Decrements the value and returns the original value.
        /// </summary>
        /// <returns>The original value.</returns>
        public int getAndDecrement()
        {
            return Interlocked.Decrement(ref _val) - 1;
        }
        /// <summary>
        /// Sets the value if the expected value is current.
        /// </summary>
        /// <param name="oldVal">The expected value.</param>
        /// <param name="newVal">The new value.</param>
        /// <returns><value>true</value> if the value was set; <value>false</value> otherwise.</returns>
        public bool compareAndSet(int oldVal, int newVal)
        {
            int origVal = Interlocked.CompareExchange(ref _val, newVal, oldVal);
            return origVal == oldVal;
        }

        /// <summary>
        /// Sets the value.
        /// </summary>
        /// <param name="newVal">The new value.</param>
        /// <returns>The new value.</returns>
        public int set(int newVal)
        {
            return Interlocked.Exchange(ref _val,newVal);
        }

        #endregion
    }
}
