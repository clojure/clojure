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

    // TODO: Find out why this is an APersistentVector.  

    /// <summary>
    /// Provides a basic implementation of <see cref="IMapEntry">IMapEntry</see>.
    /// </summary>
    /// <remarks>This also provides <see cref="IPersistentVector">IPersistentVector</see> functionality.  
    /// I'm not sure why.</remarks>
    public abstract class AMapEntry: APersistentVector, IMapEntry
    {

        #region C-tors

        /// <summary>
        /// Initialize an <see cref="AMapEntry">AMapEntry</see> with null metadata.
        /// </summary>
        public AMapEntry()
            : base(null)
        {
        }

        #endregion

        #region Helpers

        /// <summary>
        /// Convert to an actual <see cref="IPersistentVector">IPersistentVector</see> with two elements.
        /// </summary>
        /// <returns>An <see cref="IPersistentVector">IPersistentVector</see> with two elements.</returns>
        private IPersistentVector AsVector()
        {
            return LazilyPersistentVector.createOwning(key(), val());
        }

        #endregion

        #region Object members

        // they changed from implementing IPersistentVector to deriving from APersistentVector.

        ///// <summary>
        ///// Determines whether the specified Object is equal to the current one.
        ///// </summary>
        ///// <param name="obj">The Object to compare with the current one.</param>
        ///// <returns><value>true</value> if the specified Object is equal to the current one; 
        ///// otherwise <value>false</value>.</returns>
        ///// <remarks>Acts like a value type.</remarks>
        //public override bool Equals(object obj)
        //{
        //    return APersistentVector.doEquals(this, obj);
        //}

        ///// <summary>
        ///// Computes the hash code for the object.
        ///// </summary>
        ///// <returns>A hash code for the current object.</returns>
        //public override int GetHashCode()
        //{
        //    // must match logic in APersistentVector
        //    return Util.HashCombine(Util.HashCombine(0, Util.Hash(key())), Util.Hash(val()));
        //}

        ///// <summary>
        ///// Returns a string that represents the current object.
        ///// </summary>
        ///// <returns></returns>
        //public override string ToString()
        //{
        //    StringBuilder sb = new StringBuilder();
        //    RT.print(this, sb);
        //    return sb.ToString();
        //}

        #endregion

        #region IMapEntry Members

        /// <summary>
        /// Get the key in a key/value pair.
        /// </summary>
        /// <returns>The key.</returns>
        public abstract object key();

        /// <summary>
        /// Get the value in a key/value pair.
        /// </summary>
        /// <returns>The value.</returns>
        public abstract object val();

        #endregion

        #region IPersistentVector Members

        /// <summary>
        /// Gets the number of items in the vector.
        /// </summary>
        /// <returns>The number of items.</returns>
        /// <remarks>Not sure why you wouldn't use <c>count()</c> intead.</remarks>
        public override int length()
        {
            return 2;
        }

        /// <summary>
        /// Get the i-th item in the vector.
        /// </summary>
        /// <param name="i">The index of the item to retrieve/</param>
        /// <returns>The i-th item</returns>
        /// <remarks>Throws an exception if the index <c>i</c> is not in the range of the vector's elements.</remarks>
        public override object nth(int i)
        {
            switch (i)
            {
                case 0:
                    return key();
                case 1:
                    return val();
                default:
                    throw new IndexOutOfRangeException();
            }
        }

        /// <summary>
        /// Return a new vector with the i-th value set to <c>val</c>.
        /// </summary>
        /// <param name="i">The index of the item to set.</param>
        /// <param name="val">The new value</param>
        /// <returns>A new (immutable) vector v with v[i] == val.</returns>
        public override IPersistentVector assocN(int i, object val)
        {
            return AsVector().assocN(i, val);
        }

        /// <summary>
        /// Creates a new vector with a new item at the end.
        /// </summary>
        /// <param name="o">The item to add to the vector.</param>
        /// <returns>A new (immutable) vector with the objected added at the end.</returns>
        public override IPersistentVector cons(object o)
        {
            return AsVector().cons(o);
        }

        #endregion

        #region Associative Members

        /// <summary>
        /// Test if the map contains a key.
        /// </summary>
        /// <param name="key">The key to test for membership</param>
        /// <returns>True if the key is in this map.</returns>
        public override bool containsKey(object key)
        {
            return AsVector().containsKey(key);
        }

        /// <summary>
        /// Returns the key/value pair for this key.
        /// </summary>
        /// <param name="key">The key to retrieve</param>
        /// <returns>The key/value pair for the key, or null if the key is not in the map.</returns>
        public override IMapEntry entryAt(object key)
        {
            return AsVector().entryAt(key);
        }

        /// <summary>
        /// Add a new key/value pair.
        /// </summary>
        /// <param name="key">The key</param>
        /// <param name="val">The value</param>
        /// <returns>A new map with the key/value added.</returns>
        public override Associative assoc(object key, object val)
        {
            return AsVector().assoc(key, val);
        }

        /// <summary>
        /// Gets the value associated with a key.
        /// </summary>
        /// <param name="key">The key to look up.</param>
        /// <returns>The associated value. (Throws an exception if key is not present.)</returns>
        public override object valAt(object key)
        {
            return AsVector().valAt(key);
        }

        /// <summary>
        /// Gets the value associated with a key.
        /// </summary>
        /// <param name="key">The key to look up.</param>
        /// <param name="notFound">The value to return if the key is not present.</param>
        /// <returns>The associated value (or <c>notFound</c> if the key is not present.</returns>
        public override object valAt(object key, object notFound)
        {
            return AsVector().valAt(key,notFound);
        }

        #endregion

        #region Reversible members

        //public ISeq rseq()
        //{
        //    return asVector().rseq();
        //}

        #endregion

        #region IPersistentCollection Members

        /// <summary>
        /// Gets the number of items in the collection.
        /// </summary>
        /// <returns>The number of items in the collection.</returns>
        public override int count()
        {
            return 2;
        }

        /// <summary>
        /// Gets an ISeq to allow first/rest iteration through the collection.
        /// </summary>
        /// <returns>An ISeq for iteration.</returns>
        public override ISeq seq()
        {
            return AsVector().seq();
        }

        /// <summary>
        /// Returns a new collection that has the given element cons'd on front of the eixsting collection.
        /// </summary>
        /// <param name="o">An item to put at the front of the collection.</param>
        /// <returns>A new immutable collection with the item added.</returns>
        public override IPersistentCollection empty()
        {
            return null;
        }

        //IPersistentCollection IPersistentCollection.cons(object o)
        //{
        //    return cons(o);
        //}

        #endregion

        #region IPersistentStack Members

        /// <summary>
        /// Peek at the top (first) element in the stack.
        /// </summary>
        /// <returns>The top (first) element.</returns>
        public override object peek()
        {
            return val();
        }

        /// <summary>
        /// Returns a new stack with the top element popped.
        /// </summary>
        /// <returns>The new stack</returns>
        public override IPersistentStack pop()
        {
            return LazilyPersistentVector.createOwning(key());
        }

        #endregion
    }
}
