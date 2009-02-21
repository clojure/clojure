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

using System.Collections;

namespace clojure.lang
{

    /// <summary>
    /// Implements a persistent map as an array of alternating keys/values(suitable for small maps only).
    /// </summary>
    /// <remarks>
    /// <para>Note that instances of this class are constant values, i.e., add/remove etc return new values.</para>
    /// <para>Copies the array on every change, so only appropriate for <i>very small</i> maps</para>
    /// <para><value>null</value> keys and values are okay, 
    /// but you won't be able to distinguish a <value>null</value> value via <see cref="valAt">valAt</see> --
    /// use <see cref="contains">contains</see> or <see cref="entryAt">entryAt</see>.</para>
    /// </remarks>
    public class PersistentArrayMap : APersistentMap
    {
        // any reason not to seal this class?

        #region Data

        /// <summary>
        /// The maximum number of entries to hold using this implementation.
        /// </summary>
        /// <remarks>
        /// <para>Operations adding more than this number of entries should switch to another implementation.</para>
        /// <para>The value was changed from 8 to 16 in Java Rev 1159 to improve proxy perf -- we don't have proxy yet,
        /// but I changed it here anyway.</para>
        /// </remarks>
        internal const int HASHTABLE_THRESHOLD = 16;

        /// <summary>
        /// The array holding the key/value pairs.
        /// </summary>
        /// <remarks>The i-th pair is in _array[2*i] and _array[2*i+1].</remarks>
        protected readonly object[] _array;

        /// <summary>
        /// An empty <see cref="PersistentArrayMap">PersistentArrayMap</see>. Constant.
        /// </summary>
        public static readonly PersistentArrayMap EMPTY = new PersistentArrayMap();

        #endregion

        #region C-tors and factory methods

        /// <summary>
        /// Create a <see cref="PersistentArrayMap">PersistentArrayMap</see> (if small enough, else create a <see cref="PersistentHashMap">PersistentHashMap</see>.
        /// </summary>
        /// <param name="other">The BCL map to initialize from</param>
        /// <returns>A new persistent map.</returns>
        public static IPersistentMap create(IDictionary other)
        {
            // Java version has this.  Seems wasteful.
            //IPersistentMap ret = EMPTY;
            //foreach (DictionaryEntry e in other)
            //{
            //    ret = ret.assoc(e.Key, e.Value);
            //}
            //return ret;
            if (other.Count > HASHTABLE_THRESHOLD / 2)
                return PersistentHashMap.create(other);

            object[] array = new object[other.Count * 2];
            int i=0;
            foreach (DictionaryEntry e in other)
            {
                array[2 * i] = e.Key;
                array[2 * i + 1] = e.Value;
                i++;
            }
            return new PersistentArrayMap(array);
        }

        /// <summary>
        /// Create a <see cref="PersistentArrayMap">PersistentArrayMap</see> with new data but same metadata as the current object.
        /// </summary>
        /// <param name="init">The new key/value array</param>
        /// <returns>A new <see cref="PersistentArrayMap">PersistentArrayMap</see>.</returns>
        /// <remarks>The array is used directly.  Do not modify externally or immutability is sacrificed.</remarks>
        PersistentArrayMap create(params object[] init)
        {
            return new PersistentArrayMap(meta(), init);
        }


        /// <summary>
        /// Create an empty <see cref="PersistentArrayMap">PersistentArrayMap</see>.
        /// </summary>
        protected PersistentArrayMap()
        {
            _array = new object[] { };
        }

        /// <summary>
        /// Initializes a <see cref="PersistentArrayMap">PersistentArrayMap</see> to use the supplied key/value array.
        /// </summary>
        /// <param name="init">An array with alternating keys and values.</param>
        /// <remarks>The array is used directly.  Do not modify externally or immutability is sacrificed.</remarks>
        public  PersistentArrayMap(object[] init)
        {
            if (init.Length % 2 != 0)
                throw new ArgumentException("Key/value array must have an even number of elements.");
            _array = init;
        }

        /// <summary>
        /// Initializes a <see cref="PersistentArrayMap">PersistentArrayMap</see> to use the supplied key/value array and metadata.
        /// </summary>
        /// <param name="meta">The metadata to attach.</param>
        /// <param name="init">An array with alternating keys and values.</param>
        /// <remarks>The array is used directly.  Do not modify externally or immutability is sacrificed.</remarks>
        protected PersistentArrayMap(IPersistentMap meta, object[] init)
            : base(meta)
        {
            if (init.Length % 2 != 0)
                throw new ArgumentException("Key/value array must have an even number of elements.");

            _array = init;
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
            // Java version as follows
            //return new PersistentArrayMap(meta, _array);
            // But the usual pattern is this:

            return meta == _meta 
                ? this
                : new PersistentArrayMap(meta, _array);
        }


        #endregion

        #region Associative members

        /// <summary>
        /// Gets the index of the key in the array.
        /// </summary>
        /// <param name="key">The key to search for.</param>
        /// <returns>The index of the key if found; -1 otherwise.</returns>
        private int IndexOfKey(object key)
        {
            for (int i = 0; i < _array.Length; i += 2)
                if (EqualKey(_array[i],key))
                    return i;
            return -1;
        }

        /// <summary>
        /// Compare two keys for equality.
        /// </summary>
        /// <param name="k1">The first key to compare.</param>
        /// <param name="k2">The second key to compare.</param>
        /// <returns></returns>
        /// <remarks>Handles nulls properly.</remarks>
        private bool EqualKey(object k1, object k2)
        {
            if (k1 == null)
                return k2 == null;
            return k1.Equals(k2);
        }
        /// <summary>
        /// Test if the map contains a key.
        /// </summary>
        /// <param name="key">The key to test for membership</param>
        /// <returns>True if the key is in this map.</returns>
        public override bool containsKey(object key)
        {
            return IndexOfKey(key) >= 0;
        }

        /// <summary>
        /// Returns the key/value pair for this key.
        /// </summary>
        /// <param name="key">The key to retrieve</param>
        /// <returns>The key/value pair for the key, or null if the key is not in the map.</returns>
        public override IMapEntry entryAt(object key)
        {
            int i = IndexOfKey(key);
            return i >= 0
                ? new MapEntry(_array[i], _array[i + 1])
                : null;
        }

        /// <summary>
        /// Gets the value associated with a key.
        /// </summary>
        /// <param name="key">The key to look up.</param>
        /// <returns>The associated value. (Throws an exception if key is not present.)</returns>
        public override object valAt(object key)
        {
            return valAt(key, null);
        }

        /// <summary>
        /// Gets the value associated with a key.
        /// </summary>
        /// <param name="key">The key to look up.</param>
        /// <param name="notFound">The value to return if the key is not present.</param>
        /// <returns>The associated value (or <c>notFound</c> if the key is not present.</returns>
        public override object valAt(object key, object notFound)
        {
            int i = IndexOfKey(key);
            return i >= 0
                ? _array[i + 1]
                : notFound;
        }

        #endregion

        #region IPersistentCollection members

        /// <summary>
        /// Gets the number of items in the collection.
        /// </summary>
        /// <returns>The number of items in the collection.</returns>
        public override int count()
        {
            return _array.Length / 2;
        }

        /// <summary>
        /// Gets an ISeq to allow first/rest iteration through the collection.
        /// </summary>
        /// <returns>An ISeq for iteration.</returns>
        public override ISeq seq()
        {
            return _array.Length > 0
                ? new Seq(_array, 0)
                : null;
        }

        /// <summary>
        /// Gets an empty collection of the same type.
        /// </summary>
        /// <returns>An emtpy collection.</returns>
        public override IPersistentCollection empty()
        {
            return (IPersistentCollection)EMPTY.withMeta(meta());
        }

        #endregion

        #region IPersistentMap members

        /// <summary>
        /// Add a new key/value pair.
        /// </summary>
        /// <param name="key">The key</param>
        /// <param name="val">The value</param>
        /// <returns>A new map with key+value added.</returns>
        /// <remarks>Overwrites an exising value for the <paramref name="key"/>, if present.</remarks>
        public override IPersistentMap assoc(object key, object val)
        {
            int i = IndexOfKey(key);
            object[] newArray;
            if (i >= 0)
            {
                // already have key, same sized replacement
                if (_array[i + 1] == val) // no change, no-op
                    return this;
                newArray = (object[]) _array.Clone();
                newArray[i + 1] = val;
            }
            else
            {
                // new key, grow
                if (_array.Length > HASHTABLE_THRESHOLD)
                    return createHT(_array).assoc(key, val);
                newArray = new object[_array.Length + 2];
                if (_array.Length > 0)
                    Array.Copy(_array, 0, newArray, 2, _array.Length);
                newArray[0] = key;
                newArray[1] = val;
            }
            return create(newArray);
        }

        /// <summary>
        /// Create an <see cref="IPersistentMap">IPersistentMap</see> to hold the data when 
        /// an operation causes the threshhold size to be exceeded.
        /// </summary>
        /// <param name="init">The array of key/value pairs.</param>
        /// <returns>A new <see cref="IPersistentMap">IPersistentMap</see>.</returns>
        private IPersistentMap createHT(object[] init)
        {
            return PersistentHashMap.create(meta(), init);
        }

        /// <summary>
        /// Add a new key/value pair.
        /// </summary>
        /// <param name="key">The key</param>
        /// <param name="val">The value</param>
        /// <returns>A new map with key+value added.</returns>
        /// <remarks>Throws an exception if <paramref name="key"/> has a value already.</remarks>
        public override IPersistentMap assocEx(object key, object val)
        {
            int i = IndexOfKey(key);
            if (i >= 0)
                throw new Exception("Key already present.");
            return assoc(key, val);
        }

        /// <summary>
        /// Remove a key entry.
        /// </summary>
        /// <param name="key">The key to remove</param>
        /// <returns>A new map with the key removed (or the same map if the key is not contained).</returns>
        public override IPersistentMap without(object key)
        {
            int i = IndexOfKey(key);
            if (i >= 0)
            {
                // key exists, remove
                int newlen = _array.Length - 2;
                if (newlen == 0)
                    return (IPersistentMap)empty();
                object[] newArray = new object[newlen];
                for (int s = 0, d = 0; s < _array.Length; s += 2)
                    if (s != i) // skip key to be removed
                    {
                        newArray[d] = _array[s];
                        newArray[d + 1] = _array[s + 1];
                        d += 2;
                    }
                return create(newArray);
            }
            else
                return this;             
        }

        #endregion

       
        /// <summary>
        /// Internal class providing an <see cref="ISeq">ISeq</see> 
        /// for <see cref="PersistentArrayMap">PersistentArrayMap</see>s.
        /// </summary>
        protected sealed class Seq : ASeq, Counted
        {
            #region Data

            /// <summary>
            /// The array to iterate over.
            /// </summary>
            private readonly object[] _array;

            /// <summary>
            /// Current index position in the array.
            /// </summary>
            private readonly int _i;

            #endregion

            #region C-tors & factory methods

            /// <summary>
            /// Initialize the sequence to a given array and index.
            /// </summary>
            /// <param name="array">The array being sequenced over.</param>
            /// <param name="i">The current index.</param>
            public Seq(object[] array, int i)
            {
                _array = array;
                _i = i;
            }

            /// <summary>
            /// Initialize the sequence with given metatdata and array/index.
            /// </summary>
            /// <param name="meta">The metadata to attach.</param>
            /// <param name="array">The array being sequenced over.</param>
            /// <param name="i">The current index.</param>
            public Seq(IPersistentMap meta, object[] array, int i)
                : base(meta)
            {
                _array = array;
                _i = i;
            }

            #endregion

            #region ISeq members

            /// <summary>
            /// Gets the first item.
            /// </summary>
            /// <returns>The first item.</returns>
            public override object first()
            {
                return new MapEntry(_array[_i], _array[_i + 1]);
            }

            /// <summary>
            /// Gets the rest of the sequence.
            /// </summary>
            /// <returns>The rest of the sequence, or <c>null</c> if no more elements.</returns>
            public override ISeq rest()
            {
                return _i + 2 < _array.Length
                    ? new Seq(_array, _i + 2)
                    : null;
            }

            #endregion

            #region IPersistentCollection members
            /// <summary>
            /// Gets the number of items in the collection.
            /// </summary>
            /// <returns>The number of items in the collection.</returns>
            public override int count()
            {
                return (_array.Length - _i) / 2;
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
                return new Seq(meta, _array, _i);
            }

            #endregion

        }
    }
}
