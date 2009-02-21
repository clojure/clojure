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
    /// Represents an object with settable metadata.
    /// </summary>
    public interface IReference : IMeta
    {
        /// <summary>
        /// Alter the metadata on the object.
        /// </summary>
        /// <param name="alter">A function to apply to generate the new metadata</param>
        /// <param name="args">Arguments to apply the function to.</param>
        /// <returns>The new metadata map.</returns>
        /// <remarks>The new value will be the result of <c>(apply alter (cons currentMeta args))</c>.</remarks>
        IPersistentMap alterMeta(IFn alter, ISeq args);

        /// <summary>
        /// Set the metadata of the object.
        /// </summary>
        /// <param name="m">The new metadata map</param>
        /// <returns>The new metadata map.</returns>
        IPersistentMap resetMeta(IPersistentMap m);
    }
}
