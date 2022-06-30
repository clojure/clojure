/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

package clojure.lang;

/**
 * Persistent or algorithmically defined collections can implement IDrop to provide
 * a means of dropping N items that is more efficient than sequential walking.
 */
public interface IDrop{
    /**
     * Returns a collection that is Sequential, ISeq, and IReduceInit. It is also
     * useful if the returned coll implements IDrop for subsequent use in a
     * partition-like scenario.
     *
     * If n is &lt;= 0, return this.
     * If n drops to or past the end of the collection, return null.
     *
     * @param n Items to drop
     * @return Collection that is Sequential, ISeq, and IReduceInit
     */
    Sequential drop(int n);
}
