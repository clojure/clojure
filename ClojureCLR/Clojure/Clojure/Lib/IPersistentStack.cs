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
    /// Represents an immutable collection with stack operations.
    /// </summary>
    public interface IPersistentStack: IPersistentCollection
    {
        /// <summary>
        /// Peek at the top (first) element in the stack.
        /// </summary>
        /// <returns>The top (first) element.</returns>
        object peek();

        /// <summary>
        /// Returns a new stack with the top element popped.
        /// </summary>
        /// <returns>The new stack.</returns>
        IPersistentStack pop();
    }
}
