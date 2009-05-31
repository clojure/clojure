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
    /// Represents an object that has an <see cref="IStream">IStream</see>.
    /// </summary>
    public interface Streamable
    {
        /// <summary>
        /// Gets a <see cref="Stream">Stream/see> for this object.
        /// </summary>
        /// <returns>The <see cref="Stream">Stream</see>.</returns>
        Stream stream();
    }
}
