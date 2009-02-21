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

    // TODO: Replace Box:  mostly this is used in the Java version in lieu of ref/out parameters.

    /// <summary>
    /// Boxes any value or reference.
    /// </summary>
    public class Box
    {
        /// <summary>
        /// The value being boxed.
        /// </summary>
        private object _val;

        /// <summary>
        /// Gets the boxed value.
        /// </summary>
        public object Val
        {
            get { return _val; }
            set { _val = value; }
        }

        /// <summary>
        /// Initializes a <see cref="Box">Box</see> to the given value.
        /// </summary>
        /// <param name="val"></param>
        public Box(object val)
        {
            _val = val;
        }
            

    }
}
