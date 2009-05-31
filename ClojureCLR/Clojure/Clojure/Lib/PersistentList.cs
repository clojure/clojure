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
using System.Collections;
using System.Linq;
using System.Text;

namespace clojure.lang
{
    /// <summary>
    /// Represents a persistent list.
    /// </summary>
    public class PersistentList: ASeq, IPersistentList, IReduce, IList, Counted
    {

        #region Data

        /// <summary>
        /// The first item in the list.
        /// </summary>
        private readonly object _first;

        /// <summary>
        /// The rest of the list.
        /// </summary>
        private readonly IPersistentList _rest;

        /// <summary>
        /// The number of items in the list.
        /// </summary>
        private readonly int _count;

        /// <summary>
        /// An empty <see cref="IPersistentList">IPersistentList</see>.
        /// </summary>
        public static readonly EmptyList EMPTY = new EmptyList(null);

        #endregion

        #region C-tors 

        /// <summary>
        /// Initializes a list of one member.
        /// </summary>
        /// <param name="first">The one member.</param>
        public PersistentList(object first)
        {
            this._first = first;
            this._rest = null;
            this._count = 1;
        }
        
        /// <summary>
        /// Create a list initialized from a given IList.
        /// </summary>
        /// <param name="init">The list to initialize from.</param>
        /// <returns>A list.</returns>
        public static IPersistentList create(IList init)
        {
            IPersistentList ret = EMPTY;
            for (int i = init.Count - 1; i >= 0; --i)
                ret = (IPersistentList) ret.cons(init[i]);
            return ret;
        }

        /// <summary>
        /// Initialize a list with given metadata, first element and rest of list.
        /// </summary>
        /// <param name="meta">The metadata to attach.</param>
        /// <param name="first">The first element in the list.</param>
        /// <param name="rest">The rest of the list.</param>
        /// <param name="count">The number of elements in the list.</param>
        PersistentList(IPersistentMap meta, Object first, IPersistentList rest, int count)
            : base(meta)
        {
            this._first = first;
            this._rest = rest;
            this._count = count;
        }

        /// <summary>
        /// Provides a function to create a list from a sequence of arguments. (Internal use only.)
        /// </summary>
        /// <remarks>Internal use only.  Used to interface with core.clj.</remarks>
        sealed class PLCreator : RestFn
        {
            /// <summary>
            ///  Create the function with requiredArity=0.
            /// </summary>
            public PLCreator()
                : base(0)
            {
            }

            /// <summary>
            /// The creator method.
            /// </summary>
            /// <param name="args">A sequence of elements.</param>
            /// <returns>A new list.</returns>
            protected override object doInvoke(object args)
            {
                if (args is ArraySeq)
                {
                    object[] argsarray = (object[])((ArraySeq)args).ToArray();
                    IPersistentList ret = EMPTY;
                    for (int i = argsarray.Length - 1; i >= 0; i--)
                        ret = (IPersistentList)ret.cons(argsarray[i]);
                    return ret;
                }

                List<object> list = new List<object>();
                for (ISeq s = RT.seq(args); s != null; s = s.next())
                    list.Add(s.first());
                return create(list);
            }
        }


        /// <summary>
        /// An <see cref="IFn">IFn</see> to create a list from a sequence of items.
        /// </summary>
        /// <remarks>The name is without our usual leading underscore for compatiblity with core.clj.</remarks>
        public static IFn creator = new PLCreator();


        #endregion

        #region IObj members

        public override IObj withMeta(IPersistentMap meta)
        {
            return meta == _meta
                ? this
                : new PersistentList(meta, _first, _rest, _count);        
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
        /// Return a seq of the items after the first.  Calls <c>seq</c> on its argument.  If there are no more items, returns nil."
        /// </summary>
        /// <returns>A seq of the items after the first, or <c>nil</c> if there are no more items.</returns>
        public override ISeq next()
        {
            if ( _count == 1 )
                return null;
            return _rest.seq();
        }

        /// <summary>
        /// Adds an item to the beginning of the list.
        /// </summary>
        /// <param name="o">The item to add.</param>
        /// <returns>A new list containing the new item in front of the items already in the sequence.</returns>
        public override ISeq cons(Object o)
        {
            return new PersistentList(meta(), o, this, _count + 1);
        }

        #endregion

        #region IPersistentStack Members

        /// <summary>
        /// Peek at the top (first) element in the stack.
        /// </summary>
        /// <returns>The top (first) element.</returns>
        public object peek()
        {
            return _first;
        }

        /// <summary>
        /// Returns a new stack with the top element popped.
        /// </summary>
        /// <returns>The new stack</returns>
        public IPersistentStack pop()
        {
            return _rest == null
                ? (IPersistentList)EMPTY.withMeta(_meta)
                : _rest;
        }

        #endregion

        #region IPersistentCollection members

        /// <summary>
        /// Gets the number of items in the collection.
        /// </summary>
        /// <returns>The number of items in the collection.</returns>
        public override int count()
        {
            return _count;
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

        #region IReduce Members

        /// <summary>
        /// Reduce the collection using a function.
        /// </summary>
        /// <param name="f">The function to apply.</param>
        /// <returns>The reduced value</returns>
        public object reduce(IFn f)
        {
            object ret = first();
            for (ISeq s = next(); s != null; s = s.next())
                ret = f.invoke(ret, s.first());
            return ret;
        }

        /// <summary>
        /// Reduce the collection using a function.
        /// </summary>
        /// <param name="f">The function to apply.</param>
        /// <param name="start">An initial value to get started.</param>
        /// <returns>The reduced value</returns>
        public object reduce(IFn f, object start)
        {
            object ret = f.invoke(start, first());
            for (ISeq s = next(); s != null; s = s.next())
                ret = f.invoke(ret, s.first());
            return ret;
        }

        #endregion

        /// <summary>
        /// Represents an empty <see cref="IPersistentList">IPersistentList</see>.
        /// </summary>
        public class EmptyList : Obj, IPersistentList, IList, ISeq, Counted
        {
            #region C-tors

            /// <summary>
            /// Initialize an <see cref="EmptyList">PersistentList.EmptyList</see> with given metadata.
            /// </summary>
            /// <param name="meta">The metadata to attach.</param>
            public EmptyList(IPersistentMap meta)
                : base(meta)
            {
            }

            /// <summary>
            /// Initialize an <see cref="EmptyList">PersistentList.EmptyList</see> with null metadata.
            /// </summary>
            EmptyList()
            {
            }


            #endregion

            #region Object overrides

            /// <summary>
            /// Return the hash code for the object.
            /// </summary>
            /// <returns>The hash code</returns>
            public override int GetHashCode()
            {
                return -1;
            }

            /// <summary>
            /// Determines if an object is equal to the current object.
            /// </summary>
            /// <param name="obj">The object to compare to.</param>
            /// <returns><value>true</value> if the object is the same; <value>false</value> otherwise.</returns>
            /// <remarks>
            /// Equality is value-based.  Any empty sequence will do.
            /// </remarks>
            public override bool Equals(object obj)
            {
                return (obj is Sequential || obj is IList) && RT.seq(obj) == null;
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
                return meta == _meta
                    ? this
                    : new EmptyList(meta);
            }

            #endregion

            #region ISeq Members

            public object first()
            {
                return null;
            }

            public ISeq next()
            {
                return null;
            }

            public ISeq more()
            {
                return this;
            }

            public ISeq cons(object o)
            {
                return new PersistentList(meta(), o, null, 1);
            }

            #endregion
            
            #region IPersistentStack Members

            /// <summary>
            /// Peek at the top (first) element in the stack.
            /// </summary>
            /// <returns>The top (first) element.  )(Always null.)</returns>
            public object peek()
            {
                return null;
            }

            /// <summary>
            /// Returns a new stack with the top element popped.
            /// </summary>
            /// <returns>The new stack.  Always throws an exception.</returns>
            public IPersistentStack pop()
            {
                throw new InvalidOperationException("Can't pop empty list");
            }

            #endregion

            #region IPersistentCollection Members

            /// <summary>
            /// Gets the number of items in the collection.
            /// </summary>
            /// <returns>The number of items in the collection.  Always zero.</returns>
            public int count()
            {
                return 0;
            }

            /// <summary>
            /// Gets an ISeq to allow first/rest iteration through the collection.
            /// </summary>
            /// <returns>An ISeq for iteration.  The sequence is empty, so always null.</returns>
            public ISeq seq()
            {
                return null;
            }

            IPersistentCollection IPersistentCollection.cons(object o)
            {
                return cons(o);
            }

            /// <summary>
            /// Gets an empty collection of the same type.
            /// </summary>
            /// <returns>An emtpy collection.  Always returns itself.</returns>
            public IPersistentCollection empty()
            {
                return this;
            }

            /// <summary>
            /// Determine if an object is equivalent to this (handles all collections).
            /// </summary>
            /// <param name="o">The object to compare.</param>
            /// <returns><c>true</c> if the object is equivalent; <c>false</c> otherwise.</returns>
            public bool equiv(object o)
            {
                return Equals(o);
            }

            #endregion

            #region ICollection Members

            public void CopyTo(Array array, int index)
            {
                // no-op: no items to copy.
            }

            public int Count
            {
                get { return 0; }
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

            public IEnumerator GetEnumerator()
            {
                yield break;
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
                for (int i = 0; s != null; s = s.next(), i++)
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
                    return RT.nth(this,index);
                }
                set
                {
                    throw new InvalidOperationException();
                }
            }

            #endregion
        }
    }
}
