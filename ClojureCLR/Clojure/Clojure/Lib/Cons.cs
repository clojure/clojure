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
    /// Implements an immutable cons cell.
    /// </summary>
    public sealed class Cons: ASeq
    {
        // Any reason not to seal this class?

        #region Data

        /// <summary>
        /// Holds the first value.  (= CAR)
        /// </summary>
        private readonly object _first;

        /// <summary>
        /// Holds the rest value. (= CDR)
        /// </summary>
        private readonly ISeq _more;

        #endregion

        #region C-tors

        /// <summary>
        /// Initializes a <see cref="Cons">Cons</see> with the given metadata and first/rest.
        /// </summary>
        /// <param name="meta">The metadata to attach.</param>
        /// <param name="first">The first value.</param>
        /// <param name="more">The rest of the sequence.</param>
        public Cons(IPersistentMap meta, object first, ISeq more)
            : base(meta)
        {
            _first = first;
            _more = more;
        }

        /// <summary>
        /// Initializes a <see cref="Cons">Cons</see> with null metadata and given first/rest.
        /// </summary>
        /// <param name="first">The first value.</param>
        /// <param name="more">The rest of the sequence.</param>
        public Cons(object first, ISeq more)
        {
            _first = first;
            _more = more;
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
            // Java doesn't make the identity test: return new Cons(meta, _first, _rest);
            return (meta == _meta)
                ? this
                : new Cons(meta, _first, _more);
        }

        #endregion

        #region ISeq members

        /// <summary>
        /// Gets the first item.
        /// </summary>
        /// <returns>The first item.</returns>
         public override Object first()
        {
            return _first;
        }


         /// <summary>
         /// Return a seq of the items after the first.  Calls <c>seq</c> on its argument.  If there are no more items, returns nil."
         /// </summary>
         /// <returns>A seq of the items after the first, or <c>nil</c> if there are no more items.</returns>
         public override ISeq next()
         {
             return more().seq();
         }


         public override ISeq more()
        {
            return (_more == null )
             ? PersistentList.EMPTY
             : _more;
        }

        #endregion

        #region IPersistentCollection members

         /// <summary>
         /// Gets the number of items in the collection.
         /// </summary>
         /// <returns>The number of items in the collection.</returns>
         public override int count()
         {
             return 1 + RT.count(_more);
         }

        #endregion
    }
}
