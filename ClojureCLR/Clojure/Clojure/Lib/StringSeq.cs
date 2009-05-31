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
    /// A sequence of characters from a string.
    /// </summary>
    public class StringSeq: ASeq, IndexedSeq
    {
        #region Data

        /// <summary>
        /// The string providing the characters.
        /// </summary>
        private readonly string _s;

        /// <summary>
        /// Current position in the string.
        /// </summary>
        private readonly int _i;

        #endregion

        #region C-tors and factory methods

        /// <summary>
        /// Create a <see cref="StringSeq">StringSeq</see> from a String.
        /// </summary>
        /// <param name="s"></param>
        /// <returns></returns>
        static public StringSeq create(string s)
        {
            return s.Length == 0
                ? null
                : new StringSeq(null, s, 0);
        }

        /// <summary>
        /// Construct a <see cref="StringSeq">StringSeq</see> from given metadata, string, position.
        /// </summary>
        /// <param name="meta">The metadata to attach.</param>
        /// <param name="s">The string.</param>
        /// <param name="i">The current position.</param>
        StringSeq(IPersistentMap meta, string s, int i)
            : base(meta)
        {
            this._s = s;
            this._i = i;
        }

        #endregion

        #region ISeq members

        /// <summary>
        /// Gets the first item.
        /// </summary>
        /// <returns>The first item.</returns>
        public override object first()
        {
            return _s[_i];
        }

        /// <summary>
        /// Return a seq of the items after the first.  Calls <c>seq</c> on its argument.  If there are no more items, returns nil."
        /// </summary>
        /// <returns>A seq of the items after the first, or <c>nil</c> if there are no more items.</returns>
        public override ISeq next()
        {
            return _i + 1 < _s.Length
                ? new StringSeq(_meta, _s, _i + 1)
                : null;
         }

        #endregion

        #region IPersistentCollection members

        //  The Java version does not define this.
        //  Defaults to ASeq's iteration method.
        //  Seems wasteful.
        /// <summary>
        /// Gets the number of items in the collection.
        /// </summary>
        /// <returns>The number of items in the collection.</returns>
        /// <remarks>The Java version does not define this.
        /// Defaults to <see cref="ASeq">ASeq</see>'s iteration method.
        /// Seems wasteful</remarks>
         public override int count()
        {
            return _i < _s.Length ? _s.Length - _i : 0;
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
            return meta == _meta
                ? this
                : new StringSeq(meta, _s, _i);
        }

        #endregion

        #region IndexedSeq Members

         /// <summary>
         /// Gets the index associated with this sequence.
         /// </summary>
         /// <returns>The index associated with this sequence.</returns>
         public int index()
        {
            return _i;
        }

        #endregion
    }
}
