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
    /// Represents a structure map (map with fixed set of quickly accessible keys).
    /// </summary>
    /// <remarks>See the Clojure API for more information.</remarks>
    public class PersistentStructMap : APersistentMap
    {

        #region Internal classes

        /// <summary>
        /// Holds the fixed keys and map of keys to indexes.
        /// </summary>
        [Serializable]
        public sealed class Def
        {
            #region Data

            /// <summary>
            /// The fixed keys.
            /// </summary>
            readonly ISeq _keys;

            /// <summary>
            /// Get the fixed keys.
            /// </summary>
            public ISeq Keys
            {
                get { return _keys; }
            } 

            /// <summary>
            /// The map from (fixed) keys to indexes.
            /// </summary>
            readonly IPersistentMap _keyslots;

            /// <summary>
            /// Get the map from (fixed) keys to indexes.
            /// </summary>
            public IPersistentMap Keyslots
            {
              get { return _keyslots; }  
            } 

            #endregion

            #region C-tors

            /// <summary>
            /// Initialize a <see cref="Def">Def</see>.
            /// </summary>
            /// <param name="keys">The fixed keys.</param>
            /// <param name="keyslots">The map of keys/values for the fixed keys.</param>
            public Def(ISeq keys, IPersistentMap keyslots)
            {
                _keys = keys;
                _keyslots = keyslots;
            }

            #endregion
        }

        #endregion

        #region Data

        /// <summary>
        /// The <see cref="Def">Def</see> holding the fixed key definitions.
        /// </summary>
        readonly Def _def;

        /// <summary>
        /// Holds the values for the fixed keys.
        /// </summary>
        readonly object[] _vals;

        /// <summary>
        /// The map for non-fixed keys.
        /// </summary>
        readonly IPersistentMap _ext;

        #endregion

        #region C-tors and factory methods

        /// <summary>
        /// Creates a struct definition.
        /// </summary>
        /// <param name="keys">The set of fixed keys.</param>
        /// <returns>A struct definition.</returns>
        public static Def createSlotMap(ISeq keys)
        {
            if (keys == null)
                throw new ArgumentException("Must supply keys");
            IPersistentMap map = PersistentHashMap.EMPTY;
            int i = 0;
            for (ISeq s = keys; s != null; s = s.next(), i++)
                map = map.assoc(s.first(), i);
            return new Def(keys, map);
        }

        /// <summary>
        /// Create a struct from a struct definition and a sequence of alternating keys and values.
        /// </summary>
        /// <param name="def">The struct definition</param>
        /// <param name="keyvals">A sequence of alternating keys and values.</param>
        /// <returns>A <see cref="PersistentStructMap">PersistentStructMap</see>.</returns>
        public static PersistentStructMap create(Def def, ISeq keyvals)
        {
            object[] vals = new object[def.Keyslots.count()];
            IPersistentMap ext = PersistentHashMap.EMPTY;
            for (; keyvals != null; keyvals = keyvals.next().next())
            {
                if (keyvals.next() == null)
                    throw new ArgumentException(String.Format("No value supplied for key: {0}", keyvals.first()));
                object k = keyvals.first();
                object v = RT.second(keyvals);
                IMapEntry me = def.Keyslots.entryAt(k);
                if (me != null)
                    vals[Util.ConvertToInt(me.val())] = v;
                else
                    ext = ext.assoc(k, v);
            }
            return new PersistentStructMap(null, def, vals, ext);
        }

        /// <summary>
        /// Create a struct from a struct definition and values (in order) for the fixed keys.
        /// </summary>
        /// <param name="def">A struct definition</param>
        /// <param name="valseq">A sequence of values for the fixed keys (in definition order).</param>
        /// <returns>A <see cref="PersistentStructMap">PersistentStructMap</see>.</returns>
        public static PersistentStructMap construct(Def def, ISeq valseq)
        {
            object[] vals = new object[def.Keyslots.count()];
            IPersistentMap ext = PersistentHashMap.EMPTY;
            for (int i = 0; i < vals.Length && valseq != null; valseq = valseq.next(), i++)
            {
                vals[i] = valseq.first();
            }
            if (valseq != null)
                throw new ArgumentException("Too many arguments to struct constructor");
            return new PersistentStructMap(null, def, vals, ext);
        }

        /// <summary>
        /// Initialize a struct from given data.
        /// </summary>
        /// <param name="meta">The metadata to attach.</param>
        /// <param name="def">The structure definition.</param>
        /// <param name="vals">Values for the fixed keys.</param>
        /// <param name="ext">Additional keys/values.</param>
        protected PersistentStructMap(IPersistentMap meta, Def def, Object[] vals, IPersistentMap ext)
            : base(meta)
        {
            _ext = ext;
            _def = def;
            _vals = vals;
        }

        /// <summary>
        /// Create a structure from the given data.
        /// </summary>
        /// <param name="meta"></param>
        /// <param name="def"></param>
        /// <param name="vals"></param>
        /// <param name="ext"></param>
        /// <returns></returns>
        /// <remarks>
        /// This method is used instead of the PersistentStructMap constructor by
        /// all methods that return a new PersistentStructMap.  This is done
        /// to allow subclasses to override this method to return instances of their own class from 
        /// all PersistentStructMap methods.
        /// </remarks>
        protected virtual PersistentStructMap makeNew(IPersistentMap meta, Def def, object[] vals, IPersistentMap ext)
        {
            return new PersistentStructMap(meta, def, vals, ext);
        }

        #endregion

        #region Fast key access

        /// <summary>
        /// A function providing quick access to given fixed key of a struct.
        /// </summary>
        sealed class AccessorFn : AFn
        {
            #region Data

            /// <summary>
            /// The struct definition.
            /// </summary>
            private readonly Def _def;

            /// <summary>
            /// The index of the key to access.
            /// </summary>
            private readonly int _index;

            #endregion

            #region Ctors

            /// <summary>
            /// Initialize.
            /// </summary>
            /// <param name="def">The struct definition.</param>
            /// <param name="index">The index of the key to access.</param>
            public AccessorFn(Def def, int index)
            {
                _def = def;
                _index = index;
            }

            #endregion

            #region IFn methods

            public override object invoke(object arg1)
            {
                PersistentStructMap m = (PersistentStructMap)arg1;
                if (m._def != _def)
                    throw new Exception("Accessor/struct mismatch");
                return m._vals[_index];
            }

            #endregion
        }

        /// <summary>
        /// Get a fast accessor for a fixed key.
        /// </summary>
        /// <param name="def">The struct definition.</param>
        /// <param name="key">The fixed key to access.</param>
        /// <returns>An accessor function.</returns>
        public static IFn getAccessor(Def def, object key)
        {
            IMapEntry e = def.Keyslots.entryAt(key);
            if (e == null)
                throw new ArgumentException("Not a key of struct");

            int i = (int)e.val();

            return new AccessorFn(def, i);
        }


        #endregion

        #region IObj methods

        /// <summary>
        /// Create a copy with new metadata.
        /// </summary>
        /// <param name="meta">The new metadata.</param>
        /// <returns>A copy of the object with new metadata attached.</returns>
        public override IObj withMeta(IPersistentMap meta)
        {
            return (meta == _meta)
                ? this
                : makeNew(meta, _def, _vals, _ext);
        }

        #endregion

        #region IPersistentCollection members

        /// <summary>
        /// Gets the number of items in the collection.
        /// </summary>
        /// <returns>The number of items in the collection.</returns>
        public override int count()
        {
            return _vals.Length + RT.count(_ext);
        }

        /// <summary>
        /// Gets an <see cref="ISeq">ISeq</see> to allow first/rest iteration through the collection.
        /// </summary>
        /// <returns>An <see cref="ISeq">ISeq</see> for iteration.</returns>
        public override ISeq seq()
        {
            return new Seq(null, _def.Keys, _vals, 0, _ext);
        }


        /// <summary>
        /// Gets an empty collection of the same type.
        /// </summary>
        /// <returns>An emtpy collection.</returns>
        public override IPersistentCollection empty()
        {
            return construct(_def,null);
        }

        #endregion

        #region Associative members

        /// <summary>
        /// Test if the map contains a key.
        /// </summary>
        /// <param name="key">The key to test for membership</param>
        /// <returns>True if the key is in this map.</returns>
        public override bool containsKey(object key)
        {
            return _def.Keyslots.containsKey(key) || _ext.containsKey(key);
        }

        /// <summary>
        /// Returns the key/value pair for this key.
        /// </summary>
        /// <param name="key">The key to retrieve</param>
        /// <returns>The key/value pair for the key, or null if the key is not in the map.</returns>
        public override IMapEntry entryAt(object key)
        {
            IMapEntry me = _def.Keyslots.entryAt(key);
            return me == null
                ? _ext.entryAt(key)
                : new MapEntry(me.key(), _vals[Util.ConvertToInt(me.val())]);
        }

        /// <summary>
        /// Gets the value associated with a key.
        /// </summary>
        /// <param name="key">The key to look up.</param>
        /// <returns>The associated value. (Throws an exception if key is not present.)</returns>
        public override object valAt(object key)
        {
            IMapEntry me = _def.Keyslots.entryAt(key);
            return me == null
                ? _ext.valAt(key)
                : _vals[Util.ConvertToInt(me.val())];
        }

        /// <summary>
        /// Gets the value associated with a key.
        /// </summary>
        /// <param name="key">The key to look up.</param>
        /// <param name="notFound">The value to return if the key is not present.</param>
        /// <returns>The associated value (or <c>notFound</c> if the key is not present.</returns>
        public override object valAt(object key, object notFound)
        {
            IMapEntry me = _def.Keyslots.entryAt(key);
            return me == null
                ? _ext.valAt(key,notFound)
                : _vals[Util.ConvertToInt(me.val())];
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
            IMapEntry me = _def.Keyslots.entryAt(key);
            if (me != null)
            {
                int i = Util.ConvertToInt(me.val());
                object[] newVals = (object[])_vals.Clone();
                newVals[i] = val;
                return makeNew(_meta, _def, newVals, _ext);
            }
            return makeNew(_meta, _def, _vals, _ext.assoc(key, val));
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
            if (containsKey(key))
                throw new Exception("Key already present");
            return assoc(key, val);
        }

        /// <summary>
        /// Remove a key entry.
        /// </summary>
        /// <param name="key">The key to remove</param>
        /// <returns>A new map with the key removed (or the same map if the key is not contained).</returns>
        public override IPersistentMap without(object key)
        {
            IMapEntry me = _def.Keyslots.entryAt(key);
            if (me != null)
                throw new Exception("Can't remove struct key");
            IPersistentMap newExt = _ext.without(key);
            return newExt == _ext
                ? this
                : makeNew(_meta, _def, _vals, newExt);
         }

        #endregion

        /// <summary>
        /// Implements an <see cref="ISeq">ISeq</see> iterating over a <see cref="PersistentStructMap">PersistentStructMap</see>.
        /// </summary>
        /// <remarks>Combines an index-iteration over the array of fixed keys, 
        /// followed by a regular iteration of the map of non-fixed keys.
        /// </remarks>
        sealed class Seq : ASeq
        {
            #region Data

            /// <summary>
            /// Index into the fixed keys.
            /// </summary>
            readonly int _i;

            /// <summary>
            /// The (remaining) fixed keys.
            /// </summary>
            readonly ISeq _keys;

            /// <summary>
            /// The values for the fixed keys.
            /// </summary>
            readonly object[] _vals;

            /// <summary>
            /// The map of non-fixed keys and their values.
            /// </summary>
            readonly IPersistentMap _ext;

            #endregion

            #region C-tors and factory methods

            /// <summary>
            /// Initialize a <see cref="Seq">PersistentStuctMap.Seq</see>.
            /// </summary>
            /// <param name="meta">The metadata to attach.</param>
            /// <param name="keys">The remaining fixed keys.</param>
            /// <param name="vals">The values for the fixed keys.</param>
            /// <param name="i">The index of the first fixed key.</param>
            /// <param name="ext">The non-fixed keys and their values.</param>
            public Seq(IPersistentMap meta, ISeq keys, object[] vals, int i, IPersistentMap ext)
                : base(meta)
            {
                _i = i;
                _keys = keys;
                _vals = vals;
                _ext = ext;
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
                    : new Seq(meta, _keys, _vals, _i, _ext);
            }
             

            #endregion

            #region ISeq members

            /// <summary>
            /// Gets the first item.
            /// </summary>
            /// <returns>The first item.</returns>
            public override object first()
            {
                return new MapEntry(_keys.first(),_vals[_i]);
            }

            /// <summary>
            /// Return a seq of the items after the first.  Calls <c>seq</c> on its argument.  If there are no more items, returns nil."
            /// </summary>
            /// <returns>A seq of the items after the first, or <c>nil</c> if there are no more items.</returns>
            public override ISeq next()
            {
                return (_i + 1 < _vals.Length)
                    ? new Seq(_meta, _keys.next(), _vals, _i + 1, _ext)
                    : _ext.seq();
            }

            #endregion
        }      
    }
}
