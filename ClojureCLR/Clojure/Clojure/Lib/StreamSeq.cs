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
    /// Implements a persistent sequence over a stream.
    /// </summary>
    /// <remarks>Caches the stream values as retrieved.</remarks>
    public class StreamSeq : ASeq
    {
        #region Data

        /// <summary>
        /// The stream whose values are being sequenced.
        /// </summary>
        IStream _stream;

        /// <summary>
        /// The first item in the stream.
        /// </summary>
        readonly object _first;

        /// <summary>
        /// The rest of the stream.
        /// </summary>
        ISeq _rest;

        #endregion

        #region Ctors and factory methods

        /// <summary>
        /// Create a sequence from a stream.
        /// </summary>
        /// <param name="stream">The stream to sequence.</param>
        /// <returns>The persistent sequence.</returns>
        /// <remarks>Requires looking at the first element of the stream.  
        /// This is so that we know if the stream has any elements.  
        /// This is so because the sequence must have at least element or be null.</remarks>
        public static StreamSeq create(IStream stream)
        {
            object x = stream.next();
            return (RT.isEOS(x))
                ? null
                : new StreamSeq(x, stream);
        }

        /// <summary>
        /// Construct a <see cref="StreamSeq">StreamSeq</see> from metadata and first/rest.
        /// </summary>
        /// <param name="meta">The metadata to attach</param>
        /// <param name="first">The first item.</param>
        /// <param name="rest">The rest of the sequence.</param>
        StreamSeq(IPersistentMap meta, Object first, ISeq rest)
            : base(meta)
        {
            _first = first;
            _rest = rest;
            _stream = null;
        }
        
        /// <summary>
        /// Construct a <see cref="StreamSeq">StreamSeq</see> from a first item and a stream.
        /// </summary>
        /// <param name="first">The first item.</param>
        /// <param name="stream">The stream of remaining items.</param>
        StreamSeq(object first, IStream stream)
        {
            _first = first;
            _stream = stream;
        }

        #endregion

        #region IObj members


        /// <summary>
        /// Create a copy with new metadata.
        /// </summary>
        /// <param name="meta">The new metadata.</param>
        /// <returns>A copy of the object with new metadata attached.</returns>
        public override IObj withMeta(IPersistentMap meta)
        {
            if (meta != _meta)
            {
                rest();
                return new StreamSeq(meta, _first, _rest);
            }
            return this;
        }

        #endregion

        #region ISeq members

        /// <summary>
        /// Gets the first item.
        /// </summary>
        /// <returns>The first item.</returns>
        public override object first()
        {
            return _first;
        }

        /// <summary>
        /// Gets the rest of the sequence.
        /// </summary>
        /// <returns>The rest of the sequence, or <c>null</c> if no more elements.</returns>
        public override ISeq rest()
        {
            if (_stream != null)
            {
                _rest = create(_stream);
                _stream = null;
            }
            return _rest;
        }

        #endregion
    }
}
