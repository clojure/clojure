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
    /// Represents an object that has a namespace/name.
    /// </summary>
    /// <remarks>Lowercase-named methods for compatibility with the JVM implementation.</remarks>
    public interface Named
    {
        /// <summary>
        /// Gets the namespace name for the object.
        /// </summary>
        /// <returns>The namespace name.</returns>
        string getNamespace();

        /// <summary>
        /// Gets the name of the object
        /// </summary>
        /// <returns>The name.</returns>
        string getName();
    }
}
