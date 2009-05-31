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
using System.Runtime.CompilerServices;

namespace clojure.lang
{
    /// <summary>
    /// Provides basic implementation of <see cref="ISeq"/> functionality.
    /// </summary>
    public abstract class ASeq: Obj, ISeq, IList, Streamable
    {
        #region Data

        /// <summary>
        /// Holds the hash code.
        /// </summary>
        [NonSerialized]
        protected int _hash = -1;

        #endregion

        #region C-tors and factory methods

        /// <summary>
        /// Initializes an <see cref="ASeq">ASeq</see> with given metadata.
        /// </summary>
        /// <param name="meta"></param>
        protected ASeq(IPersistentMap meta)
            : base(meta)
        {
        }

        /// <summary>
        /// Initializes an <see cref="ASeq">ASeq</see> with null metadata.
        /// </summary>
        protected ASeq()
        {
        }

        #endregion

        #region object overrides

        /// <summary>
        /// Returns a String that represents the current object.
        /// </summary>
        /// <returns>A String that represents the current object.</returns>
        public override string ToString()
        {
            return RT.printString(this);
        }

        /// <summary>
        /// Determines whether the specified Object is equal to the current object.
        /// </summary>
        /// <param name="obj">The Object to compare to the current object.</param>
        /// <returns><c>true</c> if the specified Object is equal to the current object; otherwise <c>false</c></returns>
        /// <remarks>Equality is value-based, ie.e. depends on the sequence of items.</remarks>
        public override bool Equals(object obj)
        {
            if (!(obj is Sequential || obj is IList))
                return false;

            ISeq ms = RT.seq(obj);

            for (ISeq s = seq(); s != null; s = s.next(), ms = ms.next())
            {
                if ( ms == null || !Util.equals(s.first(),ms.first())) 
                    return false;
            }

            return ms == null; // hit end of sequence on both sequences
        }

        /// <summary>
        /// Computes a hash code for the current object.
        /// </summary>
        /// <returns>A hash code for the current object.</returns>
        /// <remarks>The hash code is cached after it is computed the first time.  The hash code depends on the value (sequenc of items).</remarks>
        public override int GetHashCode()
        { 
            if ( _hash == -1 )
            {
                int h = 0;
                for (ISeq s = seq(); s != null; s = s.next())
                    h = 31 * h + (s.first() == null ? 0 : s.first().GetHashCode());
                _hash = h;
            }
            return _hash;
        }

        #endregion

        #region ISeq Members

        /// <summary>
        /// Gets the first item.
        /// </summary>
        /// <returns>The first item.</returns>
        public abstract object first();


        ///// <summary>
        ///// Gets the rest of the sequence.
        ///// </summary>
        ///// <returns>The rest of the sequence, or <c>null</c> if no more elements.</returns>
        //public abstract ISeq rest();

        /// <summary>
        /// Return a seq of the items after the first.  Calls <c>seq</c> on its argument.  If there are no more items, returns nil."
        /// </summary>
        /// <returns>A seq of the items after the first, or <c>nil</c> if there are no more items.</returns>
        public abstract ISeq next();


        public virtual ISeq more()
        {
            ISeq s = next();
            if (s == null)
                return PersistentList.EMPTY;
            return s;
        }

        /// <summary>
        /// Adds an item to the beginning of the sequence.
        /// </summary>
        /// <param name="o">The item to add.</param>
        /// <returns>A new sequence containing the new item in front of the items already in the sequence.</returns>
        /// <remarks>This overrides the <c>cons</c> method in <see cref="IPersistentCollection">IPersistentCollection</see>
        /// by giving an <see cref="ISeq">ISeq</see> in return.</remarks>
        public virtual ISeq cons(object o)
        {
            return new Cons(o, this);
        }
 
        #endregion

        #region IPersistentCollection Members

        /// <summary>
        /// Adds an item to the beginning of the sequence.
        /// </summary>
        /// <param name="o">The item to add.</param>
        /// <returns>A new sequence containing the new item in front of the items already in the sequence.</returns>
        /// <remarks>This overrides the <c>cons</c> method in <see cref="IPersistentCollection">IPersistentCollection</see>
        /// by giving an <see cref="ISeq">ISeq</see> in return.</remarks>
        /// <remarks>Explicit implementation defers to the implicit implementation for <see cref="ISeq">ISeq</see>.</remarks>
        IPersistentCollection IPersistentCollection.cons(object o)
        {
            return cons(o);
        }


        /// <summary>
        /// Gets the number of items in the collection.
        /// </summary>
        /// <returns>The number of items in the collection.</returns>
        /// <remarks>This implementation is has time linear in the number of items.  
        /// Derived classes will want to override to cache this value.  
        /// (Easy because the collections are immutable.)</remarks>
        public virtual int count()
        {
            int i = 1;  // if it is here, it is non-empty.
            for (ISeq s = next(); s != null; s = s.next(), i++)
                if (s is Counted)
                    return i + s.count();

            return i;
        }

        /// <summary>
        /// Gets an ISeq to allow first/rest iteration through the collection.
        /// </summary>
        /// <returns>This item itself.</returns>
        public ISeq seq()
        {
            return this;
        }

        /// <summary>
        /// Gets an empty collection of the same type.
        /// </summary>
        /// <returns>An emtpy collection.</returns>
        /// <remarks>An empty sequence must be null.</remarks>
        virtual public IPersistentCollection empty()
        {
            return PersistentList.EMPTY;
        }


        /// <summary>
        /// Determine if an object is equivalent to this (handles all collections).
        /// </summary>
        /// <param name="o">The object to compare.</param>
        /// <returns><c>true</c> if the object is equivalent; <c>false</c> otherwise.</returns>
        public bool equiv(object obj)
        {
            if (!(obj is Sequential || obj is IList))
                return false;

            ISeq ms = RT.seq(obj);

            for (ISeq s = seq(); s != null; s = s.next(), ms = ms.next())
            {
                if (ms == null || !Util.equiv(s.first(), ms.first()))
                    return false;
            }

            return ms == null; // hit end of sequence on both sequences
        }

        #endregion

        #region ICollection Members

        /// <summary>
        /// Copies the elements of the sequence to an Array, starting at a particular index.
        /// </summary>
        /// <param name="array">The Array that is the destination of the copy.</param>
        /// <param name="index">The zero-based index in <paramref name="array"/>array</param> at which copying begins.
        public void CopyTo(Array array, int index)
        {
            if (array == null)
                throw new ArgumentException("Array cannot be null.");

            if (array.Length - index < count())
                throw new ArgumentException("The number of elements in source is greater than the available space in the array.)");

            if ( array.Rank != 1 )
                throw new ArgumentException("Array must be 1-dimensional.");

            if (index < 0)
                throw new ArgumentOutOfRangeException("Index cannot be negative.");

            ISeq s = seq();
            for (int i = index; i < array.Length && s != null; ++i, s = s.next())
                array.SetValue(s.first(), i);
        }

        /// <summary>
        /// Gets the number of elements in the sequence.
        /// </summary>
        public int Count
        {
            get { return count(); }
        }
        
        /// <summary>
        /// Gets a value indicating whether access to the collection is thread-safe.
        /// </summary>
        public bool IsSynchronized
        {
            get { return true; }
        }

        public object SyncRoot
        {
            get { throw new NotImplementedException("No need for explicit locking on immutable sequence."); }
        }

        #endregion

        #region IEnumerable Members

        /// <summary>
        /// Returns an enumerator that iterates through a collection.
        /// </summary>
        /// <returns>A <see cref="SeqEnumerator">SeqEnumerator</see> that iterates through the sequence.</returns>
        public IEnumerator GetEnumerator()
        {
            return new SeqEnumerator(seq());
        }

        #endregion

        #region Streamable Members

        /// <summary>
        /// Internal class that implements Stream source for ASeq objects.
        /// </summary>
        internal class Src : AFn
        {
            ISeq _s;

            public Src(ISeq s)
            {
                _s = s;
            }

            public override object invoke()
            {
                ISeq sq = RT.seq(_s);
                if (sq != null)
                {
                    object ret = sq.first();
                    _s = sq.more();
                    return ret;
                }
                return RT.EOS;
            }
        }

        /// <summary>
        /// Gets a <see cref="IStream">Stream</see> for this object.
        /// </summary>
        /// <returns>The <see cref="Stream">Stream</see>.</returns>
        public virtual Stream stream()
        {
            return new Stream(new Src(this));
        }

        #endregion

        #region IList Members

        public int Add(object value)
        {
            throw new InvalidOperationException();
        }

        public void Clear()
        {
            throw new InvalidOperationException();
        }

        public bool Contains(object value)
        {
            for (ISeq s = seq(); s != null; s = s.next())
                if (Util.equiv(s.first(), value))
                    return true;

            return false;
        }

        public int IndexOf(object value)
        {
            int i = 0;
            for (ISeq s = seq(); s != null; s = s.next(),i++)
                if (Util.equiv(s.first(), value))
                    return i;

            return -1;
        }

        public void Insert(int index, object value)
        {
            throw new InvalidOperationException();
        }

        public bool IsFixedSize
        {
            get { throw new NotImplementedException(); }
        }

        public bool IsReadOnly
        {
            get { return true; }
        }

        public void Remove(object value)
        {
            throw new InvalidOperationException();
        }

        public void RemoveAt(int index)
        {
            throw new InvalidOperationException();
        }

        public object this[int index]
        {
            get
            {
                //Java has this: return RT.nth(this, index);
                // THis causes an infinite loop in my code.  
                // When this was introduces, a change was made in RT.nth that changed the List test in its type dispatch to RandomAccess.
                // CLR does not have the equivalent notion, so I just left it at IList.  BOOM!
                // So, I have to do a sequential search, duplicating some of the code in RT.nth.
                ISeq seq = this;
                for (int i = 0; i <= index && seq != null; ++i, seq = seq.next())
                {
                    if (i == index)
                        return seq.first();
                }
                throw new IndexOutOfRangeException();
            }
            set
            {
                throw new InvalidOperationException();
            }
        }

        #endregion
    }
}
