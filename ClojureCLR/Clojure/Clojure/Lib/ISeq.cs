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
    /// A persistent, functional, sequence interface.  Immutable values, not changed by any method in the interface.
    /// </summary>
    /// <remarks><para>Being a non-null ISeq implies that there is at least one element.  
    /// A null value for <c>rest()</c> implies the end of the sequence.</para>
    /// A standard iteration is of the form:
    /// <code>
    /// for ( ISeq s = init;  s != null; s = s.rest() )
    /// {
    ///   ... s.first() ...
    /// }
    /// </code>
    /// </remarks>
    public interface ISeq: IPersistentCollection, Sequential
    {
        /// <summary>
        /// Gets the first item.
        /// </summary>
        /// <returns>The first item.</returns>
        object first();

        ///// <summary>
        ///// Gets the rest of the sequence.
        ///// </summary>
        ///// <returns>The rest of the sequence, or <c>null</c> if no more elements.</returns>
        //ISeq rest();

        /// <summary>
        /// Return a seq of the items after the first.  Calls <c>seq</c> on its argument.  If there are no more items, returns nil."
        /// </summary>
        /// <returns>A seq of the items after the first, or <c>nil</c> if there are no more items.</returns>
        ISeq next();

        ISeq more();

        /// <summary>
        /// Adds an item to the beginning of the sequence.
        /// </summary>
        /// <param name="o">The item to add.</param>
        /// <returns>A new sequence containing the new item in front of the items already in the sequence.</returns>
        /// <remarks>This overrides the <c>cons</c> method in <see cref="IPersistentCollection">IPersistentCollection</see>
        /// by giving an <see cref="ISeq">ISeq</see> in return.</remarks>
        new ISeq cons(object o);
    }
}
