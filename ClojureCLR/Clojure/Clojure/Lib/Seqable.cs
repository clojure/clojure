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
    /// Represents an object that can produce an <see cref="ISeq"/>.
    /// </summary>
    public interface Seqable
    {
        /// <summary>
        /// Gets an <see cref="ISeq"/>to allow first/rest/next iteration through the collection.
        /// </summary>
        /// <returns>An <see cref="ISeq"/> for iteration.</returns>
        ISeq seq();
    }
}
