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
    /// Implements an (immutable) entry in a persistent map.
    /// </summary>
    /// <remarks>
    /// <para>In built-in BCL collections, this is a struct.  We cannot do this here 
    /// because of all the other baggage this carries 
    /// (see <see cref="AMapEntry">AMapEntry</see>.</para>
    /// <para>Provides storage for a key and a value.  What more can be said?</para></remarks>
    public class MapEntry: AMapEntry
    {
        //Should we make this a sealed class? Any reason we need to derive from this?

        #region Data

        /// <summary>
        /// The key.
        /// </summary>
        protected readonly object _key;

        /// <summary>
        /// The value.
        /// </summary>
        protected readonly object _val;

        #endregion

        #region C-tors and factory methods

        /// <summary>
        /// Initialize a <see cref="MapEntry">MapEntry</see> with a key and a value.
        /// </summary>
        /// <param name="key">The key.</param>
        /// <param name="val">The value.</param>
        public MapEntry(object key, object val)
        {
            this._key = key;
            this._val = val;
        }

        #endregion

        #region IMapEntry members

        /// <summary>
        /// Get the key in a key/value pair.
        /// </summary>
        /// <returns>The key.</returns>
        public override object key()
        {
            return _key;
        }

        /// <summary>
        /// Get the value in a key/value pair.
        /// </summary>
        /// <returns>The value.</returns>
        public override object val()
        {
            return _val;
        }

        #endregion
    }
}
