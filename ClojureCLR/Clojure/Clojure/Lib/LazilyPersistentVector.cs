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
    /// A persistent vector based on an array.  Holds a lazily-allocated <see cref="PersistentVector">PersistentVector</see>
    /// if operations such as <see cref="LazilyPersistentVector.assoc()">assoc()</see> 
    /// are called that require a true persistent collection.
    /// </summary>
    public class LazilyPersistentVector: APersistentVector
    {
        #region Data

        /// <summary>
        /// The array with the items.
        /// </summary>
        protected readonly object[] _array;

        /// <summary>
        /// The lazily-allocated persistent vector for generative operations.
        /// </summary>
        PersistentVector _v = null;

        #endregion

        #region C-tors and factory methods

        /// <summary>
        /// Create a <see cref="LazilyPersistentVector">LazilyPersistentVector</see> for an array of items.
        /// </summary>
        /// <param name="items">An array of items</param>
        /// <returns>A <see cref="LazilyPersistentVector">LazilyPersistentVector</see>.</returns>
        static public IPersistentVector createOwning(params object[] items)
        {
            return items.Length == 0
                ? (IPersistentVector) PersistentVector.EMPTY
                : new LazilyPersistentVector(null, items, null);
        }

        /// <summary>
        /// Create a <see cref="LazilyPersistentVector">LazilyPersistentVector</see> from an ICollection of items.
        /// </summary>
        /// <param name="coll">The collection of items.</param>
        /// <returns>A <see cref="LazilyPersistentVector">LazilyPersistentVector</see>.</returns>
        static public IPersistentVector create(System.Collections.ICollection coll)
        {
            object[] array = new object[coll.Count];
            coll.CopyTo(array, 0);
            return createOwning(array);
        }

        /// <summary>
        /// Constructs a lazily persistent vector.
        /// </summary>
        /// <param name="meta">The metadata to attach.</param>
        /// <param name="array">The array of values.</param>
        /// <param name="v">The accompanying persistent vector.</param>
        LazilyPersistentVector(IPersistentMap meta, object[] array, PersistentVector v)
            : base(meta)
        {
            _array = array;
            _v = v;
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
            return (meta == _meta)
                ? this
                : new LazilyPersistentVector(meta, _array, _v);
        }

        #endregion

        #region IPersistentVector members

        /// <summary>
        /// Get the i-th item in the vector.
        /// </summary>
        /// <param name="i">The index of the item to retrieve/</param>
        /// <returns>The i-th item</returns>
        /// <remarks>Throws an exception if the index <c>i</c> is not in the range of the vector's elements.</remarks>
        public override object nth(int i)
        {
            return _array[i];
        }

        /// <summary>
        /// Return a new vector with the i-th value set to <c>val</c>.
        /// </summary>
        /// <param name="i">The index of the item to set.</param>
        /// <param name="val">The new value</param>
        /// <returns>A new (immutable) vector v with v[i] == val.</returns>
        public override IPersistentVector assocN(int i, object val)
        {
            return (IPersistentVector)Vec.assoc(i, val);
        }


        /// <summary>
        /// Creates a new vector with a new item at the end.
        /// </summary>
        /// <param name="o">The item to add to the vector.</param>
        /// <returns>A new (immutable) vector with the objected added at the end.</returns>
        /// <remarks>Overrides <c>cons</c> in <see cref="IPersistentCollection">IPersistentCollection</see> to specialize the return value.</remarks>
        public override IPersistentVector cons(object o)
        {
            return Vec.cons(o);
        }

        /// <summary>
        /// Gets the number of items in the vector.
        /// </summary>
        /// <returns>The number of items.</returns>
        /// <remarks>Not sure why you wouldn't use <c>count()</c> intead.</remarks>
        public override int length()
        {
           return count();
        }

        #endregion

        #region IPersistentCollection members

        /// <summary>
        /// Gets the number of items in the collection.
        /// </summary>
        /// <returns>The number of items in the collection.</returns>
        public override int count()
        {
            return _array.Length;
        }

        /// <summary>
        /// Gets an empty collection of the same type.
        /// </summary>
        /// <returns>An emtpy collection.</returns>
        public override IPersistentCollection empty()
        {
            return (IPersistentCollection)PersistentVector.EMPTY.withMeta(meta());
        }

        #endregion

        #region IPersistentStack members

        /// <summary>
        /// Peek at the top (first) element in the stack.
        /// </summary>
        /// <returns>The top (first) element.</returns>
        public override IPersistentStack pop()
        {
            return Vec.pop();
        }

        #endregion

        #region Internals

        /// <summary>
        /// Gets the lazy vector, creating it if necessary.
        /// </summary>
        private IPersistentVector Vec
        {
            [MethodImpl(MethodImplOptions.Synchronized)]
            get
            {
                if (_v == null)
                    _v = PersistentVector.create(_array);
                return _v;
            }
        }

        #endregion

    }
}
