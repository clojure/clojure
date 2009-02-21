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
using System.Runtime.CompilerServices;

namespace clojure.lang
{
    /// <summary>
    /// Represents a sequence with cached values.
    /// </summary>
    /// <remarks>Built on top of another sequence that might be infinite or otherwise lazy.  
    /// Caches the first/rest values if they are computed.</remarks>
    public class CachedSeq : ASeq
    {
        #region Data

        /// <summary>
        /// The underlying sequence.
        /// </summary>
        ISeq _s;

        /// <summary>
        /// The first item in the sequence.
        /// </summary>
        /// <remarks>Initialized to this.  Holds cached value after it is computed.</remarks>
        object _first;

        /// <summary>
        /// The rest of the sequence.
        /// </summary>
        /// <remarks>Initialized to this.  Holds cached value after it is computed.</remarks>
        ISeq _rest;

        #endregion

        #region C-tors and factory methods

        // TODO: fix parameter type when we have type inferencing
        //public CachedSeq(ISeq s)
        /// <summary>
        /// Initialize from an arbitray object (must be seq-able).
        /// </summary>
        /// <param name="s">The object to cache.</param>
        public CachedSeq(object s)
        {
            _s = RT.seq(s);
            _first = this;
            _rest = this;
        }

        /// <summary>
        /// Initialize from first/rest, with metadata.
        /// </summary>
        /// <param name="meta">The metadata to attach.</param>
        /// <param name="first">The first element.</param>
        /// <param name="rest">The rest of the sequence.</param>
        CachedSeq(IPersistentMap meta, object first, ISeq rest)
            : base(meta)
        {
            _first = first;
            _rest = rest;
        }


        #endregion

        #region IObject members

        /// <summary>
        /// Create a copy with new metadata.
        /// </summary>
        /// <param name="meta">The new metadata.</param>
        /// <returns>A copy of the object with new metadata attached.</returns>
        public override IObj withMeta(IPersistentMap m)
        {
            if (m == meta())
                return this;
            // force before copying
            rest();
            return new CachedSeq(m, _first, _rest);
        }

        #endregion

        #region ISeq members

        /// <summary>
        /// Gets the first item.
        /// </summary>
        /// <returns>The first item.</returns>
        [MethodImpl(MethodImplOptions.Synchronized)]
        public override object first()
        {
            if (_first == this)
                _first = _s.first();
            return _first;
        }

        /// <summary>
        /// Gets the rest of the sequence.
        /// </summary>
        /// <returns>The rest of the sequence, or <c>null</c> if no more elements.</returns>
        public override ISeq rest()
        {
            if (_rest == this)
            {
                // force sequential evaluation
                if (_first == this)
                    first();
                ISeq rs = _s.rest();
                if (rs == null)
                    _rest = rs;
                else
                    _rest = new CachedSeq(rs);
                _s = null;
            }
            return _rest;
        }

        #endregion

    }
}
