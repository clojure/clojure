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
    /// A cons cell constructed lazily by a call to an <see cref="Ifn">IFn</see>.
    /// </summary>
    /// <remarks>
    /// The 'first' is computed by calling <c>f()</c>.  The 'rest' is computed by calling <c>f(null)</c>.
    /// </remarks>
    public class LazyCons: ASeq
    {
        #region Data

        /// <summary>
        /// A special value indicating that first/rest have not been initialized.
        /// </summary>
        private static readonly ISeq sentinel = new Cons(null, null);

        /// <summary>
        /// The function generating the first and rest.
        /// </summary>
        protected IFn _f;

        /// <summary>
        /// The first item of the sequence, when computed.
        /// </summary>
        protected object _first;

        /// <summary>
        /// The rest of the sequence, when computed.
        /// </summary>
        protected ISeq _rest;

        #endregion

        #region C-tors and factory methods

        /// <summary>
        /// Construct a lazy cons on a function.
        /// </summary>
        /// <param name="f">The function that will create first/rest.</param>
        public LazyCons(IFn f)
        {
            this._f = f;
            this._first = sentinel;
            this._rest = sentinel;
        }

        /// <summary>
        /// Construct a lazy cons on a function, with attached metadata.
        /// </summary>
        /// <param name="meta">The metadata to attach.</param>
        /// <param name="first">The first of the sequence.</param>
        /// <param name="rest">The rest of the sequence.</param>
        private  LazyCons(IPersistentMap meta, object first, ISeq rest)
            : base(meta)
        {
            this._first = first;
            this._rest = rest;
        }

        #endregion        
        
        #region ISeq methods

        /// <summary>
        /// Gets the first item.
        /// </summary>
        /// <returns>The first item.</returns>
        [MethodImpl(MethodImplOptions.Synchronized)]
        public override object first()
        {
            if (_first == sentinel)
            {
                // Java code wraps this in a try/catch, where the catch wraps the Exception with a RuntimeException.
                // We can just let it pass through.

                _first = _f.invoke();

            }
            return _first;
        }


        /// <summary>
        /// Gets the rest of the sequence.
        /// </summary>
        /// <returns>The rest of the sequence, or <c>null</c> if no more elements.</returns>
        [MethodImpl(MethodImplOptions.Synchronized)]
        public override ISeq rest()
        {
            if (_rest == sentinel)
            {
                // Java code wraps this in a try/catch, where the catch wraps the Exception with a RuntimeException.
                // We can just let it pass through.

                // force sequential evaluation
                if (_first == sentinel)
                    first();
                _rest = RT.seq(_f.invoke(null));
                _f = null;
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
            //force eval before copying
            rest();
            return new LazyCons(meta, _first, _rest);
        }

        #endregion
    }
}
