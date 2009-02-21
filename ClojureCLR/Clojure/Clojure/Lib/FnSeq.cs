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
    /// A sequence that computes its first/rest from functional calls.  Caches.
    /// </summary>
    public class FnSeq: ASeq
    {
        #region Data

        /// <summary>
        /// The first item.
        /// </summary>
        protected readonly object _first;

        /// <summary>
        /// The rest of the sequence.
        /// </summary>
        protected ISeq _rest;

        /// <summary>
        /// Function to compute first/rest.
        /// </summary>
        protected IFn _restFn;

        #endregion

        #region C-tors and factory methods

        // TODO: When we get typing, fix the parameter type
        //public FnSeq(object first, IFn restFn)
        /// <summary>
        /// Initialize from given first and a restFn.
        /// </summary>
        /// <param name="first"></param>
        /// <param name="restFn"></param>
        public FnSeq(object first, object restFn)
        {
            _first = first;
            _restFn = (IFn)restFn;
            _rest = this;
        }

        /// <summary>
        /// Initialize from given metatadata, plus first, restFn, rest.
        /// </summary>
        /// <param name="meta">The metadata to attach</param>
        /// <param name="first">The first of the sequence.</param>
        /// <param name="restFn">The function to generate the next value.</param>
        /// <param name="rest">The rest of the sequence..</param>
        FnSeq(IPersistentMap meta, object first, IFn restFn, ISeq rest)
            : base(meta)
        {
            _first = first;
            _restFn = restFn;
            _rest = rest; 
        }

        #endregion

        #region ISeq methods

        /// <summary>
        /// Gets the first item.
        /// </summary>
        /// <returns>The first item.</returns>
        public override object first()
        {
            return _first;
        }

        /// <summary>
        /// Gets the rest of the sequence.
        /// </summary>
        /// <returns>The rest of the sequence, or <c>null</c> if no more elements.</returns>
        [MethodImpl(MethodImplOptions.Synchronized)]
        public override ISeq rest()
        {
            if ( _restFn != null )
            {
                try
                {
                    _rest = (ISeq)_restFn.invoke();
                }
                catch ( Exception ex )
                {
                    throw new InvalidOperationException("Error invoking rest function in FnSeq: " + ex.Message, ex);
                }
                _restFn = null;
            }
            return _rest;
        }

        #endregion

        #region IObj methods

        /// <summary>
        /// Create a copy with new metadata.
        /// </summary>
        /// <param name="meta">The new metadata.</param>
        /// <returns>A copy of the object with new metadata attached.</returns>
        public override IObj withMeta(IPersistentMap meta)
        {
            if (meta == _meta)
                return this;
            //force eval of restFn before copying
            rest();
            return new FnSeq(meta, _first, _restFn, _rest);
        }
        
        #endregion
    }
}
