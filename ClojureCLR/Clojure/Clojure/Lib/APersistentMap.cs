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
using System.Reflection;

using System.Collections;

namespace clojure.lang
{
    /// <summary>
    /// Provides a basic impelmentation of <see cref="IPersistentMap">IPersistentMap</see> functionality.
    /// </summary>
    public abstract class APersistentMap: AFn, IPersistentMap, IDictionary, IEnumerable<IMapEntry>
    {
        #region  Data
        
        /// <summary>
        /// Caches the hash code, when computed.
        /// </summary>
        /// <remarks>The value <value>-1</value> indicates that the hash code has not been computed yet.</remarks>
        int _hash = -1;

        #endregion

        #region C-tors and factory methods

        /// <summary>
        /// Initializes a <see cref="APersistnetMap">APersistantMap</see> to have the given metadata.
        /// </summary>
        /// <param name="meta">The metatdata to attach.</param>
        protected APersistentMap(IPersistentMap meta)
            : base(meta)
        {
        }

        /// <summary>
        /// Initializes a <see cref="APersistnetMap">APersistantMap</see> to have null metadata.
        /// </summary>
        protected APersistentMap()
        {
        }

        #endregion

        #region object overrides

        /// <summary>
        /// Returns a string that represents the current object.
        /// </summary>
        /// <returns>A string that represents the current object.</returns>
        public override string ToString()
        {
            return RT.printString(this);
        }

        /// <summary>
        /// Determines whether the specified Object is equal to the current Object.
        /// </summary>
        /// <param name="obj">The Object to compare with the current Object. </param>
        /// <returns>true if the specified Object is equal to the current Object; 
        /// otherwise, false.</returns>
        public override bool Equals(object obj)
        {
            //if(!(obj instanceof Map))
            //    return false;
            //Map m = (Map) obj;

            IDictionary d = obj as IDictionary;
            if (d == null)
                return false;

            // Java had the following.
            // This works on other APersistentMap implementations, but not on
            //  arbitrary dictionaries.
            if (d.Count != this.Count || d.GetHashCode() != this.GetHashCode())
                return false;

            //if (d.Count != this.Count)
            //    return false;

            for (ISeq s = seq(); s != null; s = s.next())
            {
                IMapEntry me = (IMapEntry)s.first();
                bool found = d.Contains(me.key());
                if (!found || !Util.equals(me.val(), d[me.key()]))
                    return false;
            }
            return true;
        }

 
        /// <summary>
        /// Gets a hash code for the current object.
        /// </summary>
        /// <returns>A hash code for the current object.</returns>
        /// <remarks>Valud-based = relies on all entries.  Once computed, it is cached.</remarks>
        public override int GetHashCode()
        {
            if (_hash == -1)
            {
                int hash = 0;
                for (ISeq s = seq(); s != null; s = s.next())
                {
                    IMapEntry me = (IMapEntry)s.first();
                    hash += (me.key() == null ? 0 : me.key().GetHashCode())
                        ^ (me.val() == null ? 0 : me.val().GetHashCode());
                }
                _hash = hash;
            }
            return _hash;
        }


        #endregion

        #region Associative methods

        abstract public bool containsKey(object key);
        abstract public IMapEntry entryAt(object key);
        Associative Associative.assoc(object key, object val)
        {
            return assoc(key, val);
        }
        abstract public object valAt(object key);
        abstract public object valAt(object key, object notFound);

        #endregion

        #region Seqable members

        abstract public ISeq seq();

        #endregion

        #region IPersistentCollection Members

        /// <summary>
        /// Returns a new collection that has the given element cons'd on front of the eixsting collection.
        /// </summary>
        /// <param name="o">An item to put at the front of the collection.</param>
        /// <returns>A new immutable collection with the item added.</returns>
        IPersistentCollection IPersistentCollection.cons(object o)
        {
            return cons(o);
        }

        abstract public int count();
        abstract public IPersistentCollection empty();

        /// <summary>
        /// Determine if an object is equivalent to this (handles all collections).
        /// </summary>
        /// <param name="o">The object to compare.</param>
        /// <returns><c>true</c> if the object is equivalent; <c>false</c> otherwise.</returns>
        /// <remarks>
        /// In Java Rev 1215, Added equiv.  Same as the definition in Equals, as in they took out the hashcode comparison.
        /// Different, as in Util.Equal above became Util.equals. and below it is Util.equiv.
        /// </remarks> 
        public bool equiv(object obj)
        {
            //if(!(obj instanceof Map))
            //    return false;
            //Map m = (Map) obj;

            IDictionary d = obj as IDictionary;
            if (d == null)
                return false;

            // Java had the following.
            // This works on other APersistentMap implementations, but not on
            //  arbitrary dictionaries.
            //if (d.Count != this.Count || d.GetHashCode() != this.GetHashCode())
            //    return false;

            if (d.Count != this.Count)
                return false;

            for (ISeq s = seq(); s != null; s = s.next())
            {
                IMapEntry me = (IMapEntry)s.first();
                bool found = d.Contains(me.key());
                if (!found || !Util.equiv(me.val(), d[me.key()]))
                    return false;
            }
            return true;
        }



        #endregion

        #region IObj members

        abstract public override IObj withMeta(IPersistentMap meta);

        #endregion

        #region IPersistentMap members

        abstract public IPersistentMap assoc(object key, object val);
        abstract public IPersistentMap assocEx(object key, object val);
        abstract public IPersistentMap without(object key);

        /// <summary>
        /// Add a new key/value pair.
        /// </summary>
        /// <param name="o">The key/value pair to add.</param>
        /// <returns>A new map with key+value pair added.</returns>
        public IPersistentMap cons(object o)
        {
            IMapEntry e = o as IMapEntry;
            if (e != null)
                return assoc(e.key(), e.val());

            if (o is DictionaryEntry)
            {
                DictionaryEntry de = (DictionaryEntry)o;
                return assoc(de.Key, de.Value);
            }
            Type t = o.GetType();
            if (t.IsGenericType && t.Name == "KeyValuePair`2")
            {
                object key = t.InvokeMember("Key", BindingFlags.GetProperty, null, o, null);
                object val = t.InvokeMember("Value", BindingFlags.GetProperty, null, o, null);
                return assoc(key, val);
            }

            IPersistentVector v = o as IPersistentVector;
            if (v != null)
            {
                if (v.count() != 2)
                    throw new ArgumentException("Vector arg to map cons must be a pair");
                return assoc(v.nth(0), v.nth(1));
            }

            IPersistentMap ret = this;
            for (ISeq s = RT.seq(o); s != null; s = s.next())
            {
                IMapEntry me = (IMapEntry)s.first();
                ret = ret.assoc(me.key(), me.val());
            }
            return ret;
        }

        #endregion

        #region IFn members

        public override object invoke(object arg1)
        {
            return valAt(arg1);
        }

        public override object invoke(object arg1, object arg2)
        {
            return valAt(arg1, arg2);
        }

        #endregion

        #region IDictionary Members

        public void Add(object key, object value)
        {
            throw new NotImplementedException();
        }

        public void Clear()
        {
            throw new NotImplementedException();
        }

        public bool Contains(object key)
        {
            return this.containsKey(key);
        }

        public virtual IDictionaryEnumerator GetEnumerator()
        {
            return new MapEnumerator(this);
        }

        public bool IsFixedSize
        {
            get { return true; }
        }

        public bool IsReadOnly
        {
            get { return true; }
        }

        public ICollection Keys
        {
            get { return KeySeq.create(seq()); }
        }

        public void Remove(object key)
        {
            throw new NotImplementedException();
        }

        public ICollection Values
        {
            get { return ValSeq.create(seq()); }
        }

        public object this[object key]
        {
            get
            {
                return valAt(key);
            }
            set
            {
                throw new NotImplementedException();
            }
        }

        #endregion

        #region ICollection Members

        public void CopyTo(Array array, int index)
        {
            ((ICollection)seq()).CopyTo(array, index);
        }

        public int Count
        {
            get { return count(); }
        }

        public bool IsSynchronized
        {
            get { return true; }
        }

        public object SyncRoot
        {
            get { throw new NotImplementedException(); }
        }

        #endregion

        #region IEnumerable Members

        IEnumerator IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }

        #endregion

        /// <summary>
        /// Implements a sequence across the keys of map.
        /// </summary>
        public sealed class KeySeq : ASeq
        {
            #region Data

            ISeq _seq;

            #endregion

            #region C-tors & factory methods

            static public KeySeq create(ISeq seq)
            {
                if (seq == null)
                    return null;
                return new KeySeq(seq);
            }

            private KeySeq(ISeq seq)
            {
                _seq = seq;
            }

            private KeySeq(IPersistentMap meta, ISeq seq)
                : base(meta)
            {
                _seq = seq;
            }

            #endregion

            #region ISeq members

            public override object first()
            {
                return ((IMapEntry)_seq.first()).key();
            }

            public override ISeq next()
            {
                return create(_seq.next());
            }

            #endregion

            #region IObj methods

            public override IObj withMeta(IPersistentMap meta)
            {
                return new KeySeq(meta, _seq);
            }

            #endregion
        }

        /// <summary>
        /// Implements a sequence across the values of a map.
        /// </summary>
        public sealed class ValSeq : ASeq
        {
            #region Data

            ISeq _seq;

            #endregion

            #region C-tors & factory methods

            static public ValSeq create(ISeq seq)
            {
                if (seq == null)
                    return null;
                return new ValSeq(seq);
            }

            private ValSeq(ISeq seq)
            {
                _seq = seq;
            }

            private ValSeq(IPersistentMap meta, ISeq seq)
                : base(meta)
            {
                _seq = seq;
            }

            #endregion

            #region ISeq members

            public override object first()
            {
                return ((IMapEntry)_seq.first()).val();
            }

            public override ISeq next()
            {
                return create(_seq.next());
            }

            #endregion

            #region IObj methods

            public override IObj withMeta(IPersistentMap meta)
            {
                return new ValSeq(meta, _seq);
            }

            #endregion
        }


        #region IEnumerable<IMapEntry> Members

        IEnumerator<IMapEntry> IEnumerable<IMapEntry>.GetEnumerator()
        {
            for (ISeq s = seq(); s != null; s = s.next())
                yield return (IMapEntry)s.first();
        }

        #endregion
    }


}
