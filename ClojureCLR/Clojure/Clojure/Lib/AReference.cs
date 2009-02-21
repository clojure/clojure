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
using System.Runtime.CompilerServices;

namespace clojure.lang
{
    /// <summary>
    /// Provides a basic implementation of IReference functionality.
    /// </summary>
    /// <remarks>The JVM implementation does not make this abstract, but I see no reason to every create one of these standalone.</remarks>
    public abstract class AReference : IReference
    {
        #region Data

        /// <summary>
        /// The metatdata for the object.
        /// </summary>
        IPersistentMap _meta;

        #endregion

        #region Ctors and factory methods

        /// <summary>
        /// Initializes a new instance of <see cref="AReference">AReference</see> that has null metadata.
        /// </summary>
        public AReference()
            : this(null)
        {
        }

        /// <summary>
        /// Initializes a new instance of <see cref="AReference">AReference</see> that has 
        /// the given <see cref="IPersistentMap">IPersistentMap</see> as its metadata.
        /// </summary>
        /// <param name="meta">The map used to initialize the metadata.</param>
        public AReference(IPersistentMap meta)
        {
            _meta = meta;
        }



        #endregion

        #region IReference Members

        /// <summary>
        /// Alter the metadata on the object.
        /// </summary>
        /// <param name="alter">A function to apply to generate the new metadata</param>
        /// <param name="args">Arguments to apply the function to.</param>
        /// <returns>The new metadata map.</returns>
        /// <remarks>The new value will be the result of <c>(apply alter (cons currentMeta args))</c>.</remarks>
        [MethodImpl(MethodImplOptions.Synchronized)]
        public IPersistentMap alterMeta(IFn alter, ISeq args)
        {
            _meta = (IPersistentMap)alter.applyTo(new Cons(_meta, args));
            return _meta;
        }

        /// <summary>
        /// Set the metadata of the object.
        /// </summary>
        /// <param name="m">The new metadata map</param>
        /// <returns>The new metadata map.</returns>
        [MethodImpl(MethodImplOptions.Synchronized)]
        public IPersistentMap resetMeta(IPersistentMap m)
        {
            _meta = m;
            return m;
        }

        #endregion

        #region IMeta Members

        /// <summary>
        /// Gets the metadata attached to the object.
        /// </summary>
        /// <returns>An immutable map representing the object's metadata.</returns>
        [MethodImpl(MethodImplOptions.Synchronized)]
        public IPersistentMap meta()
        {
            return _meta;
        }

        #endregion
    }
}
