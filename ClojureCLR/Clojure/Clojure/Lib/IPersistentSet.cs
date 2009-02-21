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
    /// Represents an immutable set (collection of unique elements).
    /// </summary>
    public interface IPersistentSet: IPersistentCollection, Counted
    {
        /// <summary>
        /// Get a set with the given item removed.
        /// </summary>
        /// <param name="key">The item to remove.</param>
        /// <returns>A new set with the item removed.</returns>
        IPersistentSet disjoin(object key);

        /// <summary>
        /// Test if the set contains the key.
        /// </summary>
        /// <param name="key">The value to test for membership in the set.</param>
        /// <returns>True if the item is in the collection; false, otherwise.</returns>
        bool contains(object key);

        /// <summary>
        /// Get the value for the key (= the key itself, or null if not present).
        /// </summary>
        /// <param name="key">The value to test for membership in the set.</param>
        /// <returns>the key if the key is in the set, else null.</returns>
        object get(object key);

        /// <summary>
        /// Gets the number of items in the collection.
        /// </summary>
        /// <returns>The number of items in the collection.</returns>
        /// <remarks>Overrides <c>count()</c> in both <see cref="IPersistentCollection">IPersistentCollection</see> 
        /// and <see cref="Counted">Counted</see> to resolve ambiguity for callers.</remarks>
        new int count();
    }
}
