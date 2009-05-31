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
    public sealed class LazySeq : Obj, ISeq, ICollection, IList  // Should we do IList -- has index accessor
    {
        #region Data

        private IFn _fn;
        private ISeq _s;

        #endregion

        #region C-tors & factory methods

        public LazySeq(IFn fn)
        {
            _fn = fn;
        }

        private LazySeq(IPersistentMap meta, ISeq s)
            : base(meta)
        {
            _fn = null;
            _s = s;
        }

        #endregion

        #region Object overrides

        public override int GetHashCode()
        {
            return Util.hash(seq());
        }

        public override bool Equals(object obj)
        {
            ISeq s = seq();
            if (s != null)
                return s.equiv(obj);
            else
                return (obj is Sequential || obj is IList) && RT.seq(obj) == null;
        }

        #endregion

        #region IObj members

        public override IObj withMeta(IPersistentMap meta)
        {
           return new LazySeq(meta,seq());
        }

        #endregion

        #region Seqable Members

        /// <summary>
        /// Gets an <see cref="ISeq"/>to allow first/rest/next iteration through the collection.
        /// </summary>
        /// <returns>An <see cref="ISeq"/> for iteration.</returns>
        [MethodImpl(MethodImplOptions.Synchronized)]
        public ISeq seq()
        {
            if (_fn != null)
            {
                _s = RT.seq(_fn.invoke());
                _fn = null;
            }
            return _s;
        }

        #endregion
        
        #region IPersistentCollection Members

        public int count()
        {
            int c = 0;
            for (ISeq s = seq(); s != null; s = s.next())
                ++c;
            return c;
        }

        IPersistentCollection IPersistentCollection.cons(object o)
        {
            return cons(o);
        }

        public IPersistentCollection empty()
        {
            return PersistentList.EMPTY;
        }

        public bool equiv(object o)
        {
            return Equals(o);
        }

        #endregion
        
        #region ISeq Members

        public object first()
        {
            seq();
            if (_s == null)
                return null;
            return _s.first();
        }

        public ISeq next()
        {
            seq();
            if (_s == null)
                return null;
            return _s.next();
        }

        public ISeq more()
        {
            seq();
            if (_s == null)
                return PersistentList.EMPTY;
            return _s.more();
        }

        public ISeq cons(object o)
        {
            return RT.cons(o, seq());
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
            ISeq s = seq();
            for (int i=0; s != null; s = s.next(), i++)
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
            get { return true; }
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
                if ( index < 0 )
                    throw new ArgumentOutOfRangeException("Index must be non-negative.");

                ISeq s = seq();
                for (int i = 0; s != null; s = s.next(), i++)
                    if (i == index)
                        return s.first();
                throw new ArgumentOutOfRangeException("Index past end of sequence.");
            }
            set
            {
                throw new InvalidOperationException();
            }
        }

        #endregion

        #region ICollection Members

        public void CopyTo(Array array, int index)
        {
            if (array == null)
                throw new ArgumentNullException("Array must not be null");
            if (index < 0)
                throw new ArgumentOutOfRangeException("Index must be non-negative.");
            if (array.Rank > 1)
                throw new ArgumentException("Array must not be multidimensional.");
            if (index >= array.Length)
                throw new ArgumentException("Index must be less than the length of the array.");
            if (count() > array.Length - index)
                throw new ArgumentException("Not enough available space from index to end of the array.");

            ISeq s = seq();
            for (int i = index; s != null; ++i, s = s.next())
                array.SetValue(s.first(), i);
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
            get { return this; }
        }

        #endregion

        #region IEnumerable Members

        public IEnumerator GetEnumerator()
        {
            return new SeqEnumerator(seq());
        }

        #endregion
    }
}
