using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace clojure.lang
{
    /// <summary>
    /// Represents an immutable key/value mapping.
    /// </summary>
    public interface Associative: IPersistentCollection
    {
        /// <summary>
        /// Test if the map contains a key.
        /// </summary>
        /// <param name="key">The key to test for membership</param>
        /// <returns>True if the key is in this map.</returns>
        bool containsKey(object key);

        /// <summary>
        /// Returns the key/value pair for this key.
        /// </summary>
        /// <param name="key">The key to retrieve</param>
        /// <returns>The key/value pair for the key, or null if the key is not in the map.</returns>
        IMapEntry entryAt(object key);

        /// <summary>
        /// Add a new key/value pair.
        /// </summary>
        /// <param name="key">The key</param>
        /// <param name="val">The value</param>
        /// <returns>A new map with the key/value added.</returns>
        Associative assoc(object key, object val);

        /// <summary>
        /// Gets the value associated with a key.
        /// </summary>
        /// <param name="key">The key to look up.</param>
        /// <returns>The associated value. (Throws an exception if key is not present.)</returns>
        object valAt(object key);

        /// <summary>
        /// Gets the value associated with a key.
        /// </summary>
        /// <param name="key">The key to look up.</param>
        /// <param name="notFound">The value to return if the key is not present.</param>
        /// <returns>The associated value (or <c>notFound</c> if the key is not present.</returns>
        object valAt(object key, object notFound);
    }
}
