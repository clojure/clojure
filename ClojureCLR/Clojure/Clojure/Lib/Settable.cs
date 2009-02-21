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
    ///  Represents an object with a value that can be set.
    /// </summary>
    public interface Settable
    {
        /// <summary>
        /// Sets the value.
        /// </summary>
        /// <param name="val">The new value</param>
        /// <returns>The new value.</returns>
        /// <remarks>Can only be called in a transaction or with a binding on the stack, else throws an exception.</remarks>
        object doSet(object val);

        /// <summary>
        /// Sets the root value.
        /// </summary>
        /// <param name="val">The new value</param>
        /// <returns>The new value.</returns>
        object doReset(object val);
    }
}
