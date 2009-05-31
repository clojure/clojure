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
    /// <summary>
    /// Represents a (contiguous) range of integers.
    /// </summary>
    public class Range: ASeq, IReduce, Streamable, Counted
    {
        #region Data

        /// <summary>
        /// Final value in the range.
        /// </summary>
        private readonly int _end;

        /// <summary>
        /// First value in the range.
        /// </summary>
        private readonly int _n;

        #endregion

        #region C-tors and factory methods

        /// <summary>
        /// Initialize a range.
        /// </summary>
        /// <param name="start">Start value</param>
        /// <param name="end">End value</param>
        /// <remarks>Needed to interface with core.clj</remarks>
        public Range(object start, object end)
            : this((int)start, (int)end)
        {
        }

        /// <summary>
        /// Initialize a range.
        /// </summary>
        /// <param name="start">Start value</param>
        /// <param name="end">End value</param>
        public Range(int start, int end)
        {
            this._end = end;
            this._n = start;
        }

        /// <summary>
        /// Initialize a range and attach metadata.
        /// </summary>
        /// <param name="meta">The metadata to attach.</param>
        /// <param name="start">Start value</param>
        /// <param name="end">End value</param>
        public Range(IPersistentMap meta, int start, int end)
            : base (meta)
        {
            this._end = end;
            this._n = start;
        }

        #endregion

        #region IPersistentCollection members

        /// <summary>
        /// Gets the number of items in the collection.
        /// </summary>
        /// <returns>The number of items in the collection.</returns>
        public override int count()
        {
            return _n < _end ? _end - _n : 0;
        }

        #endregion

        #region ISeq members

        /// <summary>
        /// Gets the first item.
        /// </summary>
        /// <returns>The first item.</returns>
        public override object first()
        {
            return _n;
        }

        /// <summary>
        /// Return a seq of the items after the first.  Calls <c>seq</c> on its argument.  If there are no more items, returns nil."
        /// </summary>
        /// <returns>A seq of the items after the first, or <c>nil</c> if there are no more items.</returns>
        public override ISeq next()
        {
            return (_n < _end - 1)
                ? new Range(_meta, _n + 1, _end)
                : null;
        }

        #endregion

        #region IObj members

        /// <summary>
        /// Create a copy with new metadata.
        /// </summary>
        /// <param name="meta">The new metadata.</param>
        /// <returns>A copy of the object with new metadata attached.</returns>
        public override IObj withMeta(IPersistentMap meta)
        {
            return meta == this.meta()
                 ? this
                 : new Range(meta, _end, _n);
        }

        #endregion
        
        #region IReduce Members

        /// <summary>
        /// Reduce the collection using a function.
        /// </summary>
        /// <param name="f">The function to apply.</param>
        /// <returns>The reduced value</returns>
        /// <remarks>Computes f(...f(f(f(i0,i1),i2),i3),...).</remarks>
        public object reduce(IFn f)
        {
            object ret = _n;
            for (int x = _n + 1; x < _end; x++)
                ret = f.invoke(ret, x);
            return ret;
        }

        /// <summary>
        /// Reduce the collection using a function.
        /// </summary>
        /// <param name="f">The function to apply.</param>
        /// <param name="start">An initial value to get started.</param>
        /// <returns>The reduced value</returns>
        /// <remarks>Computes f(...f(f(f(start,i0),i1),i2),...).</remarks>
        public object reduce(IFn f, object start)
        {
            object ret = f.invoke(start, _n);
            for (int x = _n + 1; x < _end; x++)
                ret = f.invoke(ret, x);
            return ret;
        }

        #endregion

        #region Streamable Members

        /// <summary>
        /// Implements a stream over a <see cref="Range">Range</see>.
        /// </summary>
        private new class Src : AFn
        {
            /// <summary>
            /// Current position.
            /// </summary>
            private int _n;

            /// <summary>
            /// Final position.
            /// </summary>
            private readonly long _end;


            public Src(int n, int end)
            {
                _n = n;
                _end = end;
            }

            public override object invoke()
            {
                return (_n < _end)
                    ? _n++
                    : RT.EOS;
            }
        }

        /// <summary>
        /// Gets a <see cref="Stream"/> for this object.
        /// </summary>
        /// <returns>The <see cref="Stream"/>.</returns>
        public override Stream stream()
        {
            return new Stream(new Src(_n, _end));
        }

        #endregion
    }
}
