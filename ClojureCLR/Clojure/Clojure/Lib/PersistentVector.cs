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
    /// Implements a persistent vector using a specialized form of array-mapped hash trie.
    /// </summary>
    public class PersistentVector: APersistentVector
    {
        #region Data

        protected readonly int _cnt;
        protected readonly int _shift;
        protected readonly object[] _root;
        protected readonly object[] _tail;

        /// <summary>
        /// An empty <see cref="PersistentVector">PersistentVector</see>.
        /// </summary>
        static public readonly PersistentVector EMPTY = new PersistentVector(0,5,RT.EMPTY_ARRAY,RT.EMPTY_ARRAY);

        #endregion

        #region C-tors and factory methods

        /// <summary>
        /// Create a <see cref="PersistentVector">PersistentVector</see> from an <see cref="ISeq">ISeq</see>.
        /// </summary>
        /// <param name="items">A sequence of items.</param>
        /// <returns>An initialized vector.</returns>
        static public PersistentVector create(ISeq items)
        {
            IPersistentVector ret = EMPTY;
            for (; items != null; items = items.rest())
                ret = ret.cons(items.first());
            return (PersistentVector)ret;
        }

        /// <summary>
        /// Create a <see cref="PersistentVector">PersistentVector</see> from an array of items.
        /// </summary>
        /// <param name="items"></param>
        /// <returns></returns>
        static public PersistentVector create(params object[] items)
        {
            IPersistentVector ret = EMPTY;
            foreach (object item in items)
                ret = ret.cons(item);
            return (PersistentVector)ret;
        }

        /// <summary>
        /// Initialize a <see cref="PersistentVector">PersistentVector</see> from basic components.
        /// </summary>
        /// <param name="cnt"></param>
        /// <param name="shift"></param>
        /// <param name="root"></param>
        /// <param name="tail"></param>
        PersistentVector(int cnt, int shift, object[] root, object[] tail)
            : base(null)
        {
            _cnt = cnt;
            _shift = shift;
            _root = root;
            _tail = tail;
        }


        /// <summary>
        /// Initialize a <see cref="PersistentVector">PersistentVector</see> from given metadata and basic components.
        /// </summary>
        /// <param name="meta"></param>
        /// <param name="cnt"></param>
        /// <param name="shift"></param>
        /// <param name="root"></param>
        /// <param name="tail"></param>
        PersistentVector(IPersistentMap meta, int cnt, int shift, object[] root, object[] tail)
            : base(meta)
        {
            _cnt = cnt;
            _shift = shift;
            _root = root;
            _tail = tail;
        }

        #endregion

        #region IObj members

        public override IObj withMeta(IPersistentMap meta)
        {
            // Java version does not do identity check
            return (meta == _meta)
                ? this
                : new PersistentVector(meta, _cnt, _shift, _root, _tail);
        }

        #endregion

        #region IPersistentVector members

        /// <summary>
        /// Gets the number of items in the vector.
        /// </summary>
        /// <returns>The number of items.</returns>
        /// <remarks>Not sure why you wouldn't use <c>count()</c> intead.</remarks>
        public override int length()
        {
            return count();
        }


        int tailoff()
        {
            return _cnt - _tail.Length;
        }


        /// <summary>
        /// Get the i-th item in the vector.
        /// </summary>
        /// <param name="i">The index of the item to retrieve/</param>
        /// <returns>The i-th item</returns>
        /// <remarks>Throws an exception if the index <c>i</c> is not in the range of the vector's elements.</remarks>
        public override object nth(int i)
        {
            if ( i >= 0 && i < _cnt )
            {
                if ( i >= tailoff() )
                    return _tail[i & 0x01f];
                object[] arr = _root;
                for ( int level = _shift; level > 0; level -= 5)
                    arr = (object[]) arr[ (i >> level) & 0x01f];
                return arr[i & 0x01f];
            }
            throw new IndexOutOfRangeException();
        }

        /// <summary>
        /// Return a new vector with the i-th value set to <c>val</c>.
        /// </summary>
        /// <param name="i">The index of the item to set.</param>
        /// <param name="val">The new value</param>
        /// <returns>A new (immutable) vector v with v[i] == val.</returns>
        public override IPersistentVector assocN(int i, Object val)
        {
            if (i >= 0 && i < _cnt)
            {
                if (i >= tailoff())
                {
                    object[] newTail = new object[_tail.Length];
                    Array.Copy(_tail, newTail, _tail.Length);
                    newTail[i & 0x01f] = val;

                    return new PersistentVector(meta(), _cnt, _shift, _root, newTail);
                }

                return new PersistentVector(meta(), _cnt, _shift, doAssoc(_shift, _root, i, val), _tail);
            }
            if (i == _cnt)
                return cons(val);
            throw new IndexOutOfRangeException();
        }

        static private object[] doAssoc(int level, object[] arr, int i, object val)
        {
            object[] ret = (object[]) arr.Clone();
            if (level == 0)
                ret[i & 0x01f] = val;
            else
            {
                int subidx = ( i >> level ) & 0x01f;
                ret[subidx] = doAssoc(level-5,(object[]) arr[subidx], i, val);
            }
            return ret;
        }

        /// <summary>
        /// Creates a new vector with a new item at the end.
        /// </summary>
        /// <param name="o">The item to add to the vector.</param>
        /// <returns>A new (immutable) vector with the objected added at the end.</returns>
        /// <remarks>Overrides <c>cons</c> in <see cref="IPersistentCollection">IPersistentCollection</see> to specialize the return value.</remarks>
        public override IPersistentVector cons(object val)
        {
            if (_tail.Length < 32)
            {
                object[] newTail = new object[_tail.Length + 1];
                Array.Copy(_tail, newTail, _tail.Length);
                newTail[_tail.Length] = val;
                return new PersistentVector(meta(), _cnt + 1, _shift, _root, newTail);
            }
            Box expansion = new Box(null);

            object[] newroot = pushTail(_shift - 5, _root, _tail, expansion);
            int newshift = _shift;
            if (expansion.Val != null)
            {
                newroot = new object[] { newroot, expansion.Val };
                newshift += 5;
            }

            return new PersistentVector(meta(), _cnt + 1, newshift, newroot, new object[] { val });
        }

        private object[] pushTail(int level, object[] arr, object[] tailNode, Box expansion)
        {
            object newchild;
            if (level == 0)
                newchild = tailNode;
            else
            {
                newchild = pushTail(level - 5, (object[])arr[arr.Length - 1], tailNode, expansion);
                if (expansion.Val == null)
                {
                    object[] ret1 = (object[])arr.Clone();
                    ret1[arr.Length - 1] = newchild;
                    return ret1;
                }
                else
                    newchild = expansion.Val;
            }
            //expansion
            if ( arr.Length == 32 )
            {
                expansion.Val = new object[]{newchild};
                return arr;
            }
            object[] ret = new object[arr.Length + 1];
            Array.Copy(arr, ret, arr.Length);
            ret[arr.Length] = newchild;
            expansion.Val = null;
            return ret;
        }


        
        #endregion

        #region IPersistentCollection members


        /// <summary>
        /// Gets the number of items in the collection.
        /// </summary>
        /// <returns>The number of items in the collection.</returns>
        public override int count()
        {
            return _cnt;
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

        #region IPersistentStack members

        /// <summary>
        /// Returns a new stack with the top element popped.
        /// </summary>
        /// <returns>The new stack.</returns>
        public override IPersistentStack pop()
        {
            if ( _cnt == 0 )
                throw new InvalidOperationException("Can't pop empty vector");
            if ( _cnt == 1)
                return (IPersistentStack)EMPTY.withMeta(meta());
            if ( _tail.Length > 1 )
            {
                object[] newTail = new object[_tail.Length-1];
                Array.Copy(_tail,newTail,newTail.Length);
                return new PersistentVector(meta(),_cnt-1,_shift,_root,newTail);
            }
            Box ptail = new Box(null);
            object[] newroot = popTail(_shift-5,_root,ptail);
            int newshift = _shift;
            if ( newroot == null )
                newroot = RT.EMPTY_ARRAY;
            if ( _shift > 5 && newroot.Length == 1 )
            {
                newroot = (Object[])newroot[0];
                newshift -= 5;
            }
            return new PersistentVector(meta(),_cnt-1,newshift,newroot,(object[])ptail.Val);
        }

        private object[] popTail(int shift, object[] arr, Box ptail)
        {
            if ( shift > 0 )
            {
                object[] newchild = popTail(shift-5,(object[])arr[arr.Length-1],ptail);
                if ( newchild != null )
                {
                    object[] ret1 = (object[])arr.Clone();
                    ret1[arr.Length-1] = newchild;
                    return ret1;
                }
            }
            if ( shift == 0 )
                ptail.Val = arr[arr.Length-1];
            //contaction
            if( arr.Length == 1 )
                return null;

            object[] ret = new Object[arr.Length-1];
            Array.Copy(arr,ret,ret.Length);
            return ret;
        }

        #endregion

        #region IFn members



        #endregion
    }
}
