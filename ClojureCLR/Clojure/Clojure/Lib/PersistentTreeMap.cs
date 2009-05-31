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
    /// Implements a persistent Red-Black Tree.
    /// </summary>
    /// <remarks>
    /// <para>>Note that instances of this class are constant values
    /// i.e., add/remove etc return new values.</para
    /// <para>See Okasaki, Kahrs, Larsen et al</para>
    /// </remarks>
    public class PersistentTreeMap : APersistentMap, Reversible, Sorted
    {
        #region Data

        /// <summary>
        /// The method used to compare elements for sorting.
        /// </summary>
        protected readonly IComparer _comp;

        /// <summary>
        /// Root node of the Red-Black tree.
        /// </summary>
        internal readonly Node _tree;

        /// <summary>
        /// Number of items in the tree.
        /// </summary>
        protected readonly int _count;

        /// <summary>
        /// An empty <see cref="PersistentTreeMap">PersistentTreeMap</see>.
        /// </summary>
        public static readonly PersistentTreeMap EMPTY = new PersistentTreeMap();

        #endregion

        #region Ctors & factory methods

        /// <summary>
        /// Create a <see cref="PersistentTreeMap">PersistentTreeMap</see> from a dictionary.
        /// </summary>
        /// <param name="other">The dictionary to initialize from.</param>
        /// <returns>A <see cref="PersistentTreeMap">PersistentTreeMap</see>.</returns>
        public static IPersistentMap create(IDictionary other)
        {
            IPersistentMap ret = EMPTY;
            foreach (DictionaryEntry e in other)
                ret = ret.assoc(e.Key, e.Value);
            return ret;
        }

        /// <summary>
        /// Create a <see cref="PersistentTreeMap">PersistentTreeMap</see> from 
        /// an <see cref="ISeq">ISeq</see> of alternating keys and values.
        /// </summary>
        /// <param name="items">The <see cref="ISeq">ISeq</see>  of alternating keys and values.</param>
        /// <returns>A <see cref="PersistentTreeMap">PersistentTreeMap</see>.</returns>
        public static PersistentTreeMap create(ISeq items)
        {
            IPersistentMap ret = EMPTY;
            for (; items != null; items = items.next().next())
            {
                if (items.next() == null)
                    throw new ArgumentException(string.Format("No value supplied for key: %s", items.first()));
                ret = ret.assoc(items.first(), items.next().first());
            }
            return (PersistentTreeMap)ret;
        }

        /// <summary>
        /// Create a <see cref="PersistentTreeMap">PersistentTreeMap</see> from a comparison method
        /// an <see cref="ISeq">ISeq</see> of alternating keys and values.
        /// </summary>
        /// <param name="comp">A comparison method.</param>
        /// <param name="items">The <see cref="ISeq">ISeq</see>  of alternating keys and values.</param>
        /// <returns>A <see cref="PersistentTreeMap">PersistentTreeMap</see>.</returns>
        public static PersistentTreeMap create(IComparer comp, ISeq items)
        {
            IPersistentMap ret = new PersistentTreeMap(comp);
            for (; items != null; items = items.next().next())
            {
                if (items.next() == null)
                    throw new ArgumentException(string.Format("No value supplied for key: %s", items.first()));
                ret = ret.assoc(items.first(), RT.second(items));
            }
            return (PersistentTreeMap)ret;
        }

        /// <summary>
        /// Initialize a <see cref="PersistentTreeMap">PersistentTreeMap</see> using a default comparer.
        /// </summary>
        public PersistentTreeMap()
            : this(RT.DEFAULT_COMPARER)
        {
        }

        /// <summary>
        /// Initialize a <see cref="PersistentTreeMap">PersistentTreeMap</see> using a given comparer.
        /// </summary>
        /// <param name="comp"></param>
        private PersistentTreeMap(IComparer comp)
            : this(null, comp)
        {
        }

        /// <summary>
        /// Initialize a <see cref="PersistentTreeMap">PersistentTreeMap</see> using  given metadata and comparer.
        /// </summary>
        /// <param name="meta"></param>
        /// <param name="comp"></param>
        public PersistentTreeMap(IPersistentMap meta, IComparer comp)
            : base(meta)
        {
            _comp = comp;
            _tree = null;
            _count = 0;
        }

        /// <summary>
        /// Initialize a <see cref="PersistentTreeMap">PersistentTreeMap</see> using given internal data. (Internal use only.)
        /// </summary>
        /// <param name="comp">The comparer for sorting.</param>
        /// <param name="tree">The root node of the RB tree.</param>
        /// <param name="count">The number of elements in the tree.</param>
        /// <param name="meta">The metadata to attach.</param>
        PersistentTreeMap(IComparer comp, Node tree, int count, IPersistentMap meta)
            : base(meta)
        {
            _comp = comp;
            _tree = tree;
            _count = count;
        }

        // Why we have this and the previous, I don't know.
        PersistentTreeMap(IPersistentMap meta, IComparer comp, Node tree, int count)
            : base(meta)
        {
            _comp = comp;
            _tree = tree;
            _count = count;
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
            return meta == _meta
                ? this
                : new PersistentTreeMap(meta, _comp, _tree, _count);
            // Java: return new PersistentTreeMap(meta, _comp, _tree, _count);
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
            return NodeAt(key) != null;
        }

        /// <summary>
        /// Returns the key/value pair for this key.
        /// </summary>
        /// <param name="key">The key to retrieve</param>
        /// <returns>The key/value pair for the key, or null if the key is not in the map.</returns>
        public override IMapEntry entryAt(object key)
        {
            return NodeAt(key);
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
            Node n = NodeAt(key);
            return (n != null) ? n.Val : notFound;
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
        /// Gets an <see cref="ISeq">ISeq</see> to allow first/rest iteration through the collection.
        /// </summary>
        /// <returns>An <see cref="ISeq">ISeq</see> for iteration.</returns>
        public override ISeq seq()
        {
            return (_count > 0)
                ? Seq.create(_tree, true, _count)
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
            Box found = new Box(null);
            Node t = Add(_tree, key, val, found);
            if (t == null)
            {
                Node foundNode = (Node)found.Val;
                if (foundNode.Val == val)
                    return this;
                return new PersistentTreeMap(_comp, Replace(_tree, key, val), _count, meta());
            }

            return new PersistentTreeMap(_comp, t.Blacken(), _count + 1, meta());
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
            Box found = new Box(null);
            Node t = Add(_tree, key, val, found);
            if (t == null)
                throw new Exception("Key already present");
            return new PersistentTreeMap(_comp, t.Blacken(), _count + 1, meta());
        }

        /// <summary>
        /// Remove a key entry.
        /// </summary>
        /// <param name="key">The key to remove</param>
        /// <returns>A new map with the key removed (or the same map if the key is not contained).</returns>
        public override IPersistentMap without(object key)
        {
            Box found = new Box(null);
            Node t = Remove(_tree, key, found);
            if (t == null)
            {
                if (found.Val == null)
                    return this;
                return new PersistentTreeMap(meta(), _comp);
            }
            return new PersistentTreeMap(_comp, t.Blacken(), _count - 1, meta());
        }

        #endregion

        #region Reversible Members

        /// <summary>
        /// Gets an <see cref="ISeq">ISeq</see> to travers the sequence in reverse.
        /// </summary>
        /// <returns>An <see cref="ISeq">ISeq</see> .</returns>
        public ISeq rseq()
        {
            return (_count > 0)
                ? Seq.create(_tree, false, _count)
                : null;
        }

        #endregion

        #region Sorted Members

        /// <summary>
        /// Returns the comparer used to sort the elements in the collection.
        /// </summary>
        /// <returns>The <c>IComparer</c> used to sort the items.</returns>
        /// <remarks>Would be called <c>Comparer</c> except we need to match the JVM name.</remarks>
        public System.Collections.IComparer comparator()
        {
            return _comp;
        }

        /// <summary>
        /// Returns the key to be passed to the comparator to sort the element.
        /// </summary>
        /// <param name="entry">An element in the collection.</param>
        /// <returns>The key used to sort the element.</returns>
        public object entryKey(object entry)
        {
            return ((IMapEntry)entry).key();
        }

        /// <summary>
        /// Returns an <see cref="ISeq">ISeq</see> to iterate through the collection in the designated direction. 
        /// </summary>
        /// <param name="ascending">A flag indicating if the iteration is ascending or descending.</param>
        /// <returns>A sequence for first/rest iteration.</returns>
        public ISeq seq(bool ascending)
        {
            return (_count > 0)
                ? Seq.create(_tree, ascending, _count)
                : null;
        }

        /// <summary>
        /// Returns an <see cref="ISeq">ISeq</see> to iterate through the collection in the designated direction starting from a particular key. 
        /// </summary>
        /// <param name="key">The key at which to start the iteration.</param>
        /// <param name="ascending">A flag indicating if the iteration is ascending or descending.</param>
        /// <returns>A sequence for first/rest iteration.</returns>
        /// <remarks>The key need not be in the collection.  If not present, the iteration will start with 
        /// the first element with a key greater than (if asscending) or less than (if descending) the given key.</remarks>
        public ISeq seqFrom(object key, bool ascending)
        {
            if (_count > 0)
            {
                ISeq stack = null;
                Node t = _tree;
                while (t != null)
                {
                    int c = DoCompare(key, t.Key);
                    if (c == 0)
                    {
                        stack = RT.cons(t, stack);
                        return new Seq(stack, ascending);
                    }
                    else if (ascending)
                    {
                        if (c < 0)
                        {
                            stack = RT.cons(t, stack);
                            t = t.Left;
                        }
                        else
                            t = t.Right;
                    }
                    else
                    {
                        if (c > 0)
                        {
                            stack = RT.cons(t, stack);
                            t = t.Right;
                        }
                        else
                            t = t.Left;
                    }
                }
                if (stack != null)
                    return new Seq(stack, ascending);
            }
            return null;
        }

        #endregion

        #region IDictionary Members

        public override IDictionaryEnumerator GetEnumerator()
        {
            return new NodeEnumerator(_tree, true);
        }

        #endregion


        // TODO: finish commenting RB tree operations.

        #region tree operations

        /// <summary>
        /// Get the Node containing a key, or null if key not in tree.
        /// </summary>
        /// <param name="key">The key to find.</param>
        /// <returns>The node containing the key, or null if key not found.</returns>
        Node NodeAt(object key)
        {
            Node t = _tree;
            while (t != null)
            {
                int c = DoCompare(key, t.Key);
                if (c == 0)
                    return t;
                else if (c < 0)
                    t = t.Left;
                else
                    t = t.Right;
            }
            return t;
        }

        /// <summary>
        /// Compare two keys.
        /// </summary>
        /// <param name="k1">The first key.</param>
        /// <param name="k2">The second key.</param>
        /// <returns>negative, zero, positive</returns>
        public int DoCompare(object k1, object k2)
        {
            return _comp.Compare(k1, k2);
        }

        /// <summary>
        /// Add a node for a key
        /// </summary>
        /// <param name="t"></param>
        /// <param name="key"></param>
        /// <param name="val"></param>
        /// <param name="found"></param>
        /// <returns></returns>
        Node Add(Node t, object key, object val, Box found)
        {
            if (t == null)
                return val == null
                    ? new Red(key)
                    : new RedVal(key, val);
            int c = DoCompare(key, t.Key);
            if (c == 0)
            {
                found.Val = t;
                return null;
            }
            Node ins = c < 0 ? Add(t.Left, key, val, found) : Add(t.Right, key, val, found);
            if (ins == null)
                return null;
            return c < 0
                ? t.AddLeft(ins)
                : t.AddRight(ins);
        }

        Node Remove(Node t, object key, Box found)
        {
            if (t == null)
                return null;
            int c = DoCompare(key, t.Key);
            if (c == 0)
            {
                found.Val = t;
                return Append(t.Left, t.Right);
            }
            Node del = c < 0 ? Remove(t.Left, key, found) : Remove(t.Right, key, found);
            if (del == null && found.Val == null)
                return null;
            if (c < 0)
                return (t.Left is Black)
                    ? BalanceLeftDel(t.Key, t.Val, del, t.Right)
                    : MakeRed(t.Key, t.Val, del, t.Right);
            return (t.Right is Black)
                ? BalanceRightDel(t.Key, t.Val, t.Left, del)
                : MakeRed(t.Key, t.Val, t.Left, del);
        }

        static Node Append(Node left, Node right)
        {
            if (left == null)
                return right;
            else if (right == null)
                return left;
            else if (left is Red)
            {
                if (right is Red)
                {
                    Node app = Append(left.Right, right.Left);
                    return app is Red
                        ? MakeRed(app.Key, app.Val,
                              MakeRed(left.Key, left.Val, left.Left, app.Left),
                              MakeRed(right.Key, right.Val, app.Right, right.Right))
                        : MakeRed(left.Key, left.Val, left.Left, MakeRed(right.Key, right.Val, app, right.Right));
                }
                else
                    return MakeRed(left.Key, left.Val, left.Left, Append(left.Right, right));
            }
            else if (right is Red)
                return MakeRed(right.Key, right.Val, Append(left, right.Left), right.Right);
            else
            {
                Node app = Append(left.Right, right.Left);
                return (app is Red)
                    ? MakeRed(app.Key, app.Val,
                                  MakeBlack(left.Key, left.Val, left.Left, app.Left),
                                  MakeBlack(right.Key, right.Val, app.Right, right.Right))
                                  : BalanceLeftDel(left.Key, left.Val, left.Left, MakeBlack(right.Key, right.Val, app, right.Right));
            }
        }

        static Node BalanceLeftDel(object key, object val, Node del, Node right)
        {
            if (del is Red)
                return MakeRed(key, val, del.Blacken(), right);
            else if (right is Black)
                return RightBalance(key, val, del, right.Redden());
            else if (right is Red && right.Left is Black)
                return MakeRed(right.Left.Key, right.Left.Val,
                           MakeBlack(key, val, del, right.Left.Left),
                           RightBalance(right.Key, right.Val, right.Left.Right, right.Right.Redden()));
            else
                throw new InvalidOperationException("Invariant violation");
        }


        static Node BalanceRightDel(object key, object val, Node left, Node del)
        {
            if (del is Red)
                return MakeRed(key, val, left, del.Blacken());
            else if (left is Black)
                return LeftBalance(key, val, left.Redden(), del);
            else if (left is Red && left.Right is Black)
                return MakeRed(left.Right.Key, left.Right.Val,
                           LeftBalance(left.Key, left.Val, left.Left.Redden(), left.Right.Left),
                           MakeBlack(key, val, left.Right.Right, del));
            else
                throw new InvalidOperationException("Invariant violation");
        }

        static Node LeftBalance(object key, object val, Node ins, Node right)
        {
            if (ins is Red && ins.Left is Red)
                return MakeRed(ins.Key, ins.Val, ins.Left.Blacken(), MakeBlack(key, val, ins.Right, right));
            else if (ins is Red && ins.Right is Red)
                return MakeRed(ins.Right.Key, ins.Right.Val,
                            MakeBlack(ins.Key, ins.Val, ins.Left, ins.Right.Left),
                            MakeBlack(key, val, ins.Right.Right, right));
            else
                return MakeBlack(key, val, ins, right);
        }

        static Node RightBalance(object key, object val, Node left, Node ins)
        {
            if (ins is Red && ins.Right is Red)
                return MakeRed(ins.Key, ins.Val, MakeBlack(key, val, left, ins.Left), ins.Right.Blacken());
            else if (ins is Red && ins.Left is Red)
                return MakeRed(ins.Left.Key, ins.Left.Val,
                           MakeBlack(key, val, left, ins.Left.Left),
                           MakeBlack(ins.Key, ins.Val, ins.Left.Right, ins.Right));
            else
                return MakeBlack(key, val, left, ins);
        }


        Node Replace(Node t, object key, object val)
        {
            int c = DoCompare(key, t.Key);
            return t.Replace(t.Key,
                             c == 0 ? val : t.Val,
                             c < 0 ? Replace(t.Left, key, val) : t.Left,
                             c > 0 ? Replace(t.Right, key, val) : t.Right);
        }


        static Red MakeRed(object key, object val, Node left, Node right)
        {
            if (left == null && right == null)
            {
                if (val == null)
                    return new Red(key);
                return new RedVal(key, val);
            }
            if (val == null)
                return new RedBranch(key, left, right);
            return new RedBranchVal(key, val, left, right);
        }


        static Black MakeBlack(object key, object val, Node left, Node right)
        {
            if (left == null && right == null)
            {
                if (val == null)
                    return new Black(key);
                return new BlackVal(key, val);
            }
            if (val == null)
                return new BlackBranch(key, left, right);
            return new BlackBranchVal(key, val, left, right);
        }


        #endregion

        abstract internal class Node : AMapEntry
        {
            #region Data

            protected readonly object _key;

            #endregion

            #region C-tors

            internal Node(object key)
            {
                _key = key;
            }

            #endregion

            #region IMapEntry members

            public override object key()
            {
                return _key;
            }

            public override object val()
            {
                return null;
            }


            #endregion


            protected internal object Key { get { return _key; } }
            protected internal virtual object Val { get { return null; } }

            protected internal virtual Node Left { get { return null; } }
            protected internal virtual Node Right { get { return null; } }

            abstract protected internal Node AddLeft(Node ins);
            abstract protected internal Node AddRight(Node ins);
            abstract protected internal Node RemoveLeft(Node del);
            abstract protected internal Node RemoveRight(Node del);
            abstract protected internal Node Blacken();
            abstract protected internal Node Redden();

            protected internal virtual Node BalanceLeft(Node parent)
            {
                return MakeBlack(parent.Key, parent.Val, this, parent.Right);
            }

            protected internal virtual Node BalanceRight(Node parent)
            {
                return MakeBlack(parent.Key, parent.Val, parent.Left, this);
            }

            abstract protected internal Node Replace(object key, object val, Node left, Node right);


        }  // end class Node

        class Black : Node
        {
            public Black(object key)
                : base(key)
            {
            }

            protected internal override Node AddLeft(Node ins)
            {
                return ins.BalanceLeft(this);
            }

            protected internal override Node AddRight(Node ins)
            {
                return ins.BalanceRight(this);
            }

            protected internal override Node RemoveLeft(Node del)
            {
                return BalanceLeftDel(_key, val(), del, Right);
            }

            protected internal override Node RemoveRight(Node del)
            {
                return BalanceRightDel(_key, val(), Left, del);
            }

            protected internal override Node Blacken()
            {
                return this;
            }

            protected internal override Node Redden()
            {
                return new Red(_key);
            }

            protected internal override Node Replace(object key, object val, Node left, Node right)
            {
                return MakeBlack(key, val, left, right);
            }
        }

        class BlackVal : Black
        {
            protected readonly object _val;

            public BlackVal(object key, object val)
                : base(key)
            {
                _val = val;
            }

            public override object val()
            {
                return _val;
            }


            override protected internal object Val { get { return _val; } }


            protected internal override Node Redden()
            {
                return new RedVal(_key, _val);
            }
        }

        class BlackBranch : Black
        {

            protected readonly Node _left;
            protected readonly Node _right;

            public BlackBranch(object key, Node left, Node right)
                : base(key)
            {
                this._left = left;
                this._right = right;
            }

            protected internal override Node Left { get { return _left; } }
            protected internal override Node Right { get { return _right; } }

            protected internal override Node Redden()
            {
                return new RedBranch(_key, _left, _right);
            }
        }

        class BlackBranchVal : BlackBranch
        {
            readonly object _val;

            public BlackBranchVal(object key, object val, Node left, Node right)
                : base(key, left, right)
            {
                _val = val;
            }

            public override object val()
            {
                return _val;
            }

            override protected internal object Val { get { return _val; } }

            protected internal override Node Redden()
            {
                return new RedBranchVal(_key, _val, _left, _right);
            }
        }

        class Red : Node
        {
            public Red(object key)
                : base(key)
            {

            }

            protected internal override Node AddLeft(Node ins)
            {
                return MakeRed(_key, val(), ins, Right);
            }

            protected internal override Node AddRight(Node ins)
            {
                return MakeRed(_key, val(), Left, ins);
            }

            protected internal override Node RemoveLeft(Node del)
            {
                return MakeRed(_key, val(), del, Right);
            }

            protected internal override Node RemoveRight(Node del)
            {
                return MakeRed(_key, val(), Left, del);
            }

            protected internal override Node Blacken()
            {
                return new Black(_key);
            }

            protected internal override Node Redden()
            {
                throw new InvalidOperationException("Invariant violation");
            }

            protected internal override Node Replace(object key, object val, Node left, Node right)
            {
                return MakeRed(key, val, left, right);
            }

        }

        class RedVal : Red
        {
            protected readonly object _val;

            public RedVal(object key, object val)
                : base(key)
            {
                this._val = val;
            }

            override public object val()
            {
                return _val;
            }

            override protected internal object Val { get { return _val; } }

            protected internal override Node Blacken()
            {
                return new BlackVal(_key, _val);
            }

        }

        class RedBranch : Red
        {

            protected readonly Node _left;
            protected readonly Node _right;

            public RedBranch(object key, Node left, Node right)
                : base(key)
            {
                this._left = left;
                this._right = right;
            }

            protected internal override Node Left { get { return _left; } }
            protected internal override Node Right { get { return _right; } }

            protected internal override Node BalanceLeft(Node parent)
            {
                if (_left is Red)
                    return MakeRed(_key, val(), _left.Blacken(), MakeBlack(parent.Key, parent.Val, _right, parent.Right));
                else if (_right is Red)
                    return MakeRed(_right.Key, _right.Val, MakeBlack(_key, val(), _left, _right.Left),
                               MakeBlack(parent.Key, parent.Val, _right.Right, parent.Right));
                else
                    return base.BalanceLeft(parent);

            }

            protected internal override Node BalanceRight(Node parent)
            {
                if (_right is Red)
                    return MakeRed(_key, val(), MakeBlack(parent.Key, parent.Val, parent.Left, _left), _right.Blacken());
                else if (_left is Red)
                    return MakeRed(_left.Key, _left.Val, MakeBlack(parent.Key, parent.Val, parent.Left, _left.Left),
                               MakeBlack(_key, val(), _left.Right, _right));
                else
                    return base.BalanceRight(parent);
            }

            protected internal override Node Blacken()
            {
                return new BlackBranch(_key, _left, _right);
            }

        }

        class RedBranchVal : RedBranch
        {
            readonly object _val;

            public RedBranchVal(object key, object val, Node left, Node right)
                : base(key, left, right)
            {
                this._val = val;
            }

            override public object val()
            {
                return _val;
            }

            override protected internal object Val { get { return _val; } }


            protected internal override Node Blacken()
            {
                return new BlackBranchVal(_key, _val, _left, _right);
            }
        }

        class Seq : ASeq
        {
            #region Data

            readonly ISeq _stack;
            readonly bool _asc;
            readonly int _cnt;

            #endregion

            #region C-tors & factory methods

            public Seq(ISeq stack, bool asc)
            {
                _stack = stack;
                _asc = asc;
                _cnt = -1;
            }

            public Seq(ISeq stack, bool asc, int cnt)
            {
                _stack = stack;
                _asc = asc;
                _cnt = cnt;
            }

            Seq(IPersistentMap meta, ISeq stack, bool asc, int cnt)
                : base(meta)
            {
                _stack = stack;
                _asc = asc;
                _cnt = cnt;
            }

            internal static Seq create(Node t, bool asc, int cnt)
            {
                return new Seq(push(t, null, asc), asc, cnt);
            }


            #endregion

            #region IObj members

            public override IObj withMeta(IPersistentMap meta)
            {
                return new Seq(meta, _stack, _asc, _cnt);
            }

            #endregion

            #region details

            static ISeq push(Node t, ISeq stack, bool asc)
            {
                while (t != null)
                {
                    stack = RT.cons(t, stack);
                    t = asc ? t.Left : t.Right;
                }
                return stack;
            }

            #endregion

            #region ISeq members

            public override object first()
            {
                return _stack.first();
            }

            public override ISeq next()
            {
                Node t = (Node)_stack.first();
                ISeq nextStack = push(_asc ? t.Right : t.Left, _stack.next(), _asc);
                return nextStack != null
                    ? new Seq(nextStack, _asc, _cnt - 1)
                    : null;
            }

            #endregion

            #region IPersistentCollection members

            public override int count()
            {
                return (_cnt < 0)
                    ? base.count()
                    : _cnt;
            }

            #endregion

        }

        class NodeEnumerator : IDictionaryEnumerator
        {
            #region Data

            Stack<Node> _stack = new Stack<Node>();
            bool _asc;
            Node _startNode;

            #endregion

            #region C-tors

            internal NodeEnumerator(Node t, bool asc)
            {
                _asc = asc;
                _startNode = t;
                push(t);
            }

            #endregion

            #region details

            void push(Node t)
            {
                while (t != null)
                {
                    _stack.Push(t);
                    t = _asc ? t.Left : t.Right;
                }
            }

            #endregion

            #region IDictionaryEnumerator Members

            public DictionaryEntry Entry
            {
                get { return (DictionaryEntry)Current; }
            }

            public object Key
            {
                get { return Entry.Key; }
            }

            public object Value
            {
                get { return Entry.Value; }
            }

            #endregion


            #region IEnumerator Members

            public object Current
            {
                get { return _stack.Peek(); }
            }

            public bool MoveNext()
            {
                if (_stack.Count == 0)
                    throw new InvalidOperationException("Enumerator at end.");
                Node t = _stack.Pop();
                push(_asc ? t.Right : t.Left);
                return _stack.Count > 0;
            }

            public void Reset()
            {
                _stack.Clear();
                push(_startNode);
            }

            #endregion
        }

    }
}

#region Java code not implemented

        //public NodeIterator iterator(){
        //    return new NodeIterator(tree, true);
        //}

        //public NodeIterator reverseIterator(){
        //    return new NodeIterator(tree, false);
        //}

        //public Iterator keys(){
        //    return keys(iterator());
        //}

        //public Iterator vals(){
        //    return vals(iterator());
        //}

        //public Iterator keys(NodeIterator it){
        //    return new KeyIterator(it);
        //}

        //public Iterator vals(NodeIterator it){
        //    return new ValIterator(it);
        //}

        //public Object minKey(){
        //    Node t = min();
        //    return t != null ? t.key : null;
        //}

        //public Node min(){
        //    Node t = tree;
        //    if(t != null)
        //        {
        //        while(t.left() != null)
        //            t = t.left();
        //        }
        //    return t;
        //}

        //public Object maxKey(){
        //    Node t = max();
        //    return t != null ? t.key : null;
        //}

        //public Node max(){
        //    Node t = tree;
        //    if(t != null)
        //        {
        //        while(t.right() != null)
        //            t = t.right();
        //        }
        //    return t;
        //}

        //public int depth(){
        //    return depth(tree);
        //}

        //int depth(Node t){
        //    if(t == null)
        //        return 0;
        //    return 1 + Math.max(depth(t.left()), depth(t.right()));
        //}

        //static class KeyIterator implements Iterator{
        //    NodeIterator it;

        //    KeyIterator(NodeIterator it){
        //        this.it = it;
        //    }

        //    public boolean hasNext(){
        //        return it.hasNext();
        //    }

        //    public Object next(){
        //        return ((Node) it.next()).key;
        //    }

        //    public void remove(){
        //        throw new UnsupportedOperationException();
        //    }
        //}

        //static class ValIterator implements Iterator{
        //    NodeIterator it;

        //    ValIterator(NodeIterator it){
        //        this.it = it;
        //    }

        //    public boolean hasNext(){
        //        return it.hasNext();
        //    }

        //    public Object next(){
        //        return ((Node) it.next()).val();
        //    }

        //    public void remove(){
        //        throw new UnsupportedOperationException();
        //    }
        //}



#endregion

