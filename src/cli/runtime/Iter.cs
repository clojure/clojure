/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

using System;

namespace org.clojure.runtime
    {
    public interface Iter
        {
/**
 *  * Multiple calls to get() are allowed prior to calling iterate()
 *  * @return  the currently referenced item/element/value
 *  */
        Object get();

        /**
         *  * This may destroy or otherwise invalidate the object it is called upon
         *  * so always capture and use the return value (even though sometimes you may find it is the same object)
         *  * @return The next iter to use, or null if at end of sequence
         *  */
        Iter iterate();
        }
    }
