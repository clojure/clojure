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
    /// A persistent rendition of Phil Bagwell's Hash Array Mapped Trie
    /// <para>Uses path copying for persistence.</para>
    /// <para>HashCollision leaves vs extended hashing</para>
    /// <para>Node polymorphism vs conditionals</para>
    /// <para>No sub-tree pools or root-resizing</para>
    /// <para>Any errors are Rich Hickey's, except those that are mine.</para>
    /// </summary>
    /// <remarks>
    /// <para>Uses path copying for persistence.</para>
    /// <para>HashCollision leaves vs extended hashing</para>
    /// <para>Node polymorphism vs conditionals</para>
    /// <para>No sub-tree pools or root-resizing</para>
    /// <para>Any errors are Rich Hickey's (so he says), except those that I introduced.</para>
    /// </remarks>
    public class PersistentHashMap: APersistentMap
    {
        #region Data

        /// <summary>
        /// The number of entries in the map.
        /// </summary>
        protected readonly int _count;

        /// <summary>
        /// The root of the trie.
        /// </summary>
        protected readonly INode _root;

        /// <summary>
        /// An empty <see cref="PersistentHashMap">PersistentHashMap</see>.
        /// </summary>
        public static readonly PersistentHashMap EMPTY = new PersistentHashMap(0, new EmptyNode());

        #endregion

        #region C-tors & factory methods

        // TODO: Is it worth speeding up the initializers that create so many unnecessary nodes?

        /// <summary>
        /// Create a <see cref="PersistentHashMap">PersistentHashMap</see> initialized from a CLR dictionary.
        /// </summary>
        /// <param name="other">The dictionary to copy from.</param>
        /// <returns>A <see cref="PersistentHashMap">PersistentHashMap</see>.</returns>
        public static IPersistentMap create(IDictionary other)
        {
            IPersistentMap ret = EMPTY;
            foreach (DictionaryEntry e in other)
                ret = ret.assoc(e.Key, e.Value);
            return ret;
        }

        /// <summary>
        /// Create a <see cref="PersistentHashMap">PersistentHashMap</see> initialized from an array of alternating keys and values.
        /// </summary>
        /// <param name="init">An array of alternating keys and values.</param>
        /// <returns>A <see cref="PersistentHashMap">PersistentHashMap</see>.</returns>
        public static PersistentHashMap create(params object[] init)
        {
            IPersistentMap ret = EMPTY;
            for (int i = 0; i < init.Length; i += 2)
                ret = ret.assoc(init[i], init[i + 1]);
            return (PersistentHashMap)ret;
        }

        /// <summary>
        /// Create a <see cref="PersistentHashMap">PersistentHashMap</see> initialized from an IList of alternating keys and values.
        /// </summary>
        /// <param name="init">An IList of alternating keys and values.</param>
        /// <returns>A <see cref="PersistentHashMap">PersistentHashMap</see>.</returns>
        public static PersistentHashMap create1(IList init)
        {
            IPersistentMap ret = EMPTY;
            for (IEnumerator i = init.GetEnumerator(); i.MoveNext(); )
            {
                object key = i.Current;
                if (!i.MoveNext())
                    throw new ArgumentException(String.Format("No value supplied for key: {0}", key));
                object val = i.Current;
                ret = ret.assoc(key, val);
            }
            return (PersistentHashMap)ret;
        }

        /// <summary>
        /// Create a <see cref="PersistentHashMap">PersistentHashMap</see> initialized from 
        /// an <see cref="ISeq">ISeq</see> of alternating keys and values.
        /// </summary>
        /// <param name="items">An <see cref="ISeq">ISeq</see> of alternating keys and values.</param>
        /// <returns>A <see cref="PersistentHashMap">PersistentHashMap</see>.</returns>
        public static PersistentHashMap create(ISeq items)
        {
            IPersistentMap ret = EMPTY;
            for ( ; items != null; items = items.next().next() )
            {
                if ( items.next() == null )
                    throw new ArgumentException(String.Format("No value supplied for key: {0}", items.first()));
                ret = ret.assoc(items.first(), RT.second(items) );
            }
            return (PersistentHashMap)ret;
        }


        /// <summary>
        /// Create a <see cref="PersistentHashMap">PersistentHashMap</see> with given metadata initialized from an array of alternating keys and values.
        /// </summary>
        /// <param name="meta">The metadata to attach.</param>
        /// <param name="init">An array of alternating keys and values.</param>
        /// <returns>A <see cref="PersistentHashMap">PersistentHashMap</see>.</returns>
        public static PersistentHashMap create(IPersistentMap meta, params object[] init)
        {
            IPersistentMap ret = (IPersistentMap)EMPTY.withMeta(meta);
            for (int i = 0; i < init.Length; i += 2)
                ret = ret.assoc(init[i], init[i + 1]);
            return (PersistentHashMap)ret;
        }

        /// <summary>
        /// Initialize a <see cref="PersistentHashMap">PersistentHashMap</see> with a given count and root node.
        /// </summary>
        /// <param name="count">The count.</param>
        /// <param name="root">The root node.</param>
        PersistentHashMap(int count, INode root)
        {
            _count = count;
            _root = root;
        }

        /// <summary>
        /// Initialize a <see cref="PersistentHashMap">PersistentHashMap</see> with given metadata, count and root node.
        /// </summary>
        /// <param name="meta">The metadata to attach</param>
        /// <param name="count">The count.</param>
        /// <param name="root">The root node.</param>
        PersistentHashMap(IPersistentMap meta, int count, INode root)
            : base(meta)
        {
            _count = count;
            _root = root;
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
             // Java does not include change test
            return meta == _meta
             ? this
             : new PersistentHashMap(meta, _count, _root);
        }

        #endregion

        #region Associative

         /// <summary>
         /// Test if the map contains a key.
         /// </summary>
         /// <param name="key">The key to test for membership</param>
         /// <returns>True if the key is in this map.</returns>
         public override bool containsKey(object key)
        {
            return entryAt(key) != null;
        }

        /// <summary>
        /// Returns the key/value pair for this key.
        /// </summary>
        /// <param name="key">The key to retrieve</param>
        /// <returns>The key/value pair for the key, or null if the key is not in the map.</returns>
        public override IMapEntry entryAt(object key)
        {
            return (IMapEntry)_root.find(Util.Hash(key),key);
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
            IMapEntry e = entryAt(key);
            return e != null
                ? e.val()
                : notFound;
        }

        #endregion

        #region IPersistentMap

        /// <summary>
        /// Add a new key/value pair.
        /// </summary>
        /// <param name="key">The key</param>
        /// <param name="val">The value</param>
        /// <returns>A new map with key+value added.</returns>
        /// <remarks>Overwrites an exising value for the <paramref name="key"/>, if present.</remarks>
        public override IPersistentMap assoc(object key, object val)
        {
            Box addedLeaf = new Box(null);
            INode newroot = _root.assoc(0, Util.Hash(key), key, val, addedLeaf);
            return newroot == _root
                ? this
                : new PersistentHashMap(meta(), addedLeaf.Val == null ? _count : _count + 1, newroot);
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
            INode newroot = _root.without(Util.Hash(key), key);

            return newroot == _root
                ? this
                : (newroot == null
                    ? (IPersistentMap) EMPTY.withMeta(meta())
                    : new PersistentHashMap(meta(), _count - 1, newroot));

        }

        #endregion

        #region IPersistentCollection

        /// <summary>
        /// Gets the number of items in the collection.
        /// </summary>
        /// <returns>The number of items in the collection.</returns>
        public override int count()
        {
            return _count;
        }

        /// <summary>
        /// Gets an ISeq to allow first/rest iteration through the collection.
        /// </summary>
        /// <returns>An ISeq for iteration.</returns>
        public override ISeq seq()
        {
            return _root.nodeSeq();
        }

        /// <summary>
        /// Gets an empty collection of the same type.
        /// </summary>
        /// <returns>An emtpy collection.</returns>
        public override IPersistentCollection empty()
        {
            return (IPersistentCollection) EMPTY.withMeta(meta());
        }

        #endregion

        /// <summary>
        /// Interface for all nodes in the trie.
        /// </summary>
        public interface INode
        {
            /// <summary>
            /// Return a trie with a new key/value pair.
            /// </summary>
            /// <param name="shift"></param>
            /// <param name="hash"></param>
            /// <param name="key"></param>
            /// <param name="val"></param>
            /// <param name="addedLeaf"></param>
            /// <returns></returns>
            INode assoc(int shift, int hash, object key, object val, Box addedLeaf);

            /// <summary>
            /// Return a trie with the given key removed.
            /// </summary>
            /// <param name="hash"></param>
            /// <param name="key"></param>
            /// <returns></returns>
            INode without(int hash, object key);

            /// <summary>
            /// Gets the node containing a given key.
            /// </summary>
            /// <param name="hash"></param>
            /// <param name="key"></param>
            /// <returns></returns>
            INode find(int hash, object key);

            /// <summary>
            /// Return an <see cref="ISeq">ISeq</see> with iterating the tree defined by the current node.
            /// </summary>
            /// <returns>An <see cref="ISeq">ISeq</see> </returns>
            ISeq nodeSeq();

            /// <summary>
            /// Get the hash for the current ndoe.
            /// </summary>
            /// <returns></returns>
            int getHash();
        }

        /// <summary>
        /// A node with no keys.  Represents the empty map.
        /// </summary>
        sealed class EmptyNode : INode
        {
            #region INode Members

            public INode assoc(int shift, int hash, object key, object val, Box addedLeaf)
            {
                INode ret = new LeafNode(hash, key, val);
                addedLeaf.Val = ret;
                return ret;
            }

            public INode without(int hash, object key)
            {
                return this;
            }

            public INode find(int hash, object key)
            {
                return null;
            }

            public ISeq nodeSeq()
            {
                return null;
            }

            public int getHash()
            {
                return 0;
            }

            #endregion
        }

        /// <summary>
        ///  An internal node in the trie with all branches filled.
        /// </summary>
        sealed class FullNode : INode
        {
            #region Data

            readonly INode[] _nodes;
            readonly int _shift;
            readonly int _hash;

            #endregion

            #region C-tors

            internal FullNode(INode[] nodes, int shift)
            {
                _nodes = nodes;
                _shift = shift;
                _hash = nodes[0].getHash();
            }

            #endregion

            #region Calculations

            static int bitpos(int hash, int shift)
            {
                return 1 << Util.Mask(hash, shift);
            }

            #endregion

            #region INode Members

            public INode assoc(int shift, int hash, object key, object val, Box addedLeaf)
            {
                int idx = Util.Mask(hash, shift);

                INode n = _nodes[idx].assoc(shift + 5, hash, key, val, addedLeaf);
                if (n == _nodes[idx])
                    return this;
                else
                {
                    INode[] newNodes = (INode[])_nodes.Clone();
                    newNodes[idx] = n;
                    return new FullNode(newNodes, shift);
                }
            }


            public INode without(int hash, object key)
            {
                int idx = Util.Mask(hash, _shift);
                INode n = _nodes[idx].without(hash, key);
                if (n != _nodes[idx])
                {
                    if (n == null)
                    {
                        INode[] newnodes1 = new INode[_nodes.Length - 1];
                        Array.Copy(_nodes, 0, newnodes1, 0, idx);
                        Array.Copy(_nodes, idx + 1, newnodes1, idx, _nodes.Length - (idx + 1));
                        return new BitmapIndexedNode(~bitpos(hash, _shift), newnodes1, _shift);
                    }
                    INode[] newnodes = (INode[])_nodes.Clone();
                    newnodes[idx] = n;
                    return new FullNode(newnodes, _shift);
                }
                return this;
            }

            public INode find(int hash, object key)
            {
                return _nodes[Util.Mask(hash, _shift)].find(hash, key);
            }

            public ISeq nodeSeq()
            {
                return Seq.create(this, 0);
            }

            public int getHash()
            {
                return _hash;
            }

            #endregion

            private sealed class Seq : ASeq
            {
                #region Data

                readonly ISeq _s;
                readonly int _i;
                readonly FullNode _node;

                #endregion

                #region C-tors & factory methods

                Seq(ISeq s, int i, FullNode node)
                {
                    _s = s;
                    _i = i;
                    _node = node;
                }

                Seq(IPersistentMap meta, ISeq s, int i, FullNode node)
                    :base(meta)
                {
                    _s = s;
                    _i = i;
                    _node = node;
                }

                public static ISeq create(FullNode node, int i)
                {
                    return i >= node._nodes.Length
                        ? null
                        : new Seq(node._nodes[i].nodeSeq(), i, node);
                }

                #endregion

                #region ISeq members

                public override object first()
                {
                    return _s.first();
                }

                public override ISeq next()
                {
                    ISeq nexts = _s.next();
                    return nexts != null
                        ? new Seq(nexts, _i, _node)
                        : create(_node, _i + 1);
                }
             

                #endregion

                #region IObj members

                public override IObj withMeta(IPersistentMap meta)
                {
                    return new Seq(meta, _s, _i, _node);
                }

                #endregion
            }
        }

        /// <summary>
        /// Represents a leaf node in the tree, corresponding to single map entry (key/value).
        /// </summary>
        sealed class LeafNode : AMapEntry, INode
        {
            #region Data

            readonly int _hash;
            readonly object _key;
            readonly object _val;
            
            #endregion

            #region C-tors

            public LeafNode(int hash, object key, object val)
            {
                _hash = hash;
                _key = key;
                _val = val;
            }

            #endregion

            #region IMapEntry members

            public override object key()
            {
                return _key;
            }

            public override object val()
            {
                return _val;
            }

            #endregion

            #region INode Members

            public INode assoc(int shift, int hash, object key, object val, Box addedLeaf)
            {
                if (hash == _hash)
                {
                    if (Util.equals(key, _key))
                    {
                        if (val == _val)
                            return this;
                        // note - do not set AddedLeaf, since we are replacing
                        else
                            return new LeafNode(hash, key, val);
                    }
                    else
                    {
                        // hash collision, same hash, different keys
                        LeafNode newLeaf = new LeafNode(hash, key, val);
                        addedLeaf.Val = newLeaf;
                        return new HashCollisionNode(hash, this, newLeaf);
                    }
                }
                else
                    return BitmapIndexedNode.create(shift, this, hash, key, val, addedLeaf);
            }

            public INode without(int hash, object key)
            {
                return (hash == _hash && Util.equals(key, _key))
                   ? null
                   : this;
            }

            public INode find(int hash, object key)
            {
                return (hash == _hash && Util.equals(key, _key))
                    ? this
                    : null;
            }

            public ISeq nodeSeq()
            {
                return (ISeq)RT.cons(this,null);
            }

            public int getHash()
            {
                return _hash;
            }

            #endregion
        }

        /// <summary>
        ///  Represents an internal node in the trie, not full.
        /// </summary>
        sealed class BitmapIndexedNode : INode
        {
            #region Data

            readonly int _bitmap;
            readonly INode[] _nodes;
            readonly int _shift;
            readonly int _hash;

            #endregion

            #region Calculations

            static int bitpos(int hash, int shift)
            {
                return 1 << Util.Mask(hash, shift);
            }

            int index(int bit)
            {
                return Util.BitCount(_bitmap & (bit - 1));
            }

            #endregion

            #region C-tors & factory methods

            internal BitmapIndexedNode(int bitmap, INode[] nodes, int shift)
            {
                _bitmap = bitmap;
                _nodes = nodes;
                _shift = shift;
                _hash = nodes[0].getHash();
            }

            internal static INode create(int bitmap, INode[] nodes, int shift)
            {
                return bitmap == -1
                    ? (INode)new FullNode(nodes, shift)
                    : (INode)new BitmapIndexedNode(bitmap, nodes, shift);
            }

            internal static INode create(int shift, INode branch, int hash, object key, object val, Box addedLeaf)
            {
                return (new BitmapIndexedNode(bitpos(branch.getHash(), shift), new INode[] { branch }, shift))
                        .assoc(shift, hash, key, val, addedLeaf);
            }

            #endregion

            #region INode Members

            public INode assoc(int shift, int hash, object key, object val, Box addedLeaf)
            {
                int bit = bitpos(hash, shift);
                int idx = index(bit);
                if ((_bitmap & bit) != 0)
                {
                    INode n = _nodes[idx].assoc(shift + 5, hash, key, val, addedLeaf);
                    if (n == _nodes[idx])
                        return this;
                    else
                    {
                        INode[] newnodes = (INode[])_nodes.Clone();
                        newnodes[idx] = n;
                        return new BitmapIndexedNode(_bitmap, newnodes, shift);
                    }
                }
                else
                {
                    INode[] newnodes = new INode[_nodes.Length + 1];
                    Array.Copy(_nodes, 0, newnodes, 0, idx);
                    addedLeaf.Val = newnodes[idx] = new LeafNode(hash, key, val);
                    Array.Copy(_nodes, idx, newnodes, idx + 1, _nodes.Length - idx);
                    return create(_bitmap | bit, newnodes, shift);
                }           
            }

            public INode without(int hash, object key)
            {
                int bit = bitpos(hash, _shift);
                if ((_bitmap & bit) != 0)
                {
                    int idx = index(bit);
                    INode n = _nodes[idx].without(hash, key);
                    if (n != _nodes[idx])
                    {
                        if (n == null)
                        {
                            if (_bitmap == bit)
                                return null;
                            INode[] newnodes1 = new INode[_nodes.Length - 1];
                            Array.Copy(_nodes, 0, newnodes1, 0, idx);
                            Array.Copy(_nodes, idx + 1, newnodes1, idx, _nodes.Length - (idx + 1));
                            return new BitmapIndexedNode(_bitmap & ~bit, newnodes1, _shift);
                        }
                        INode[] newnodes = (INode[])_nodes.Clone();
                        newnodes[idx] = n;
                        return new BitmapIndexedNode(_bitmap, newnodes, _shift);
                    }
                }
                return this;
            }


            public INode find(int hash, object key)
            {
                int bit = bitpos(hash, _shift);
                return ((_bitmap & bit) != 0)
                    ? _nodes[index(bit)].find(hash, key)
                    : null;
            }

            public ISeq nodeSeq()
            {
                return Seq.create(this,0);
            }

            public int getHash()
            {
                return _hash;
            }

            #endregion

            sealed class Seq : ASeq
            {
                #region Data

                readonly ISeq _s;
                readonly int _i;
                readonly BitmapIndexedNode _node;

                #endregion

                #region C-tors & factory methods

                Seq(ISeq s, int i, BitmapIndexedNode node)
                {
                    _s = s;
                    _i = i;
                    _node = node;
                }

                Seq(IPersistentMap meta, ISeq s, int i, BitmapIndexedNode node)
                    :base(meta)
                {
                    _s = s;
                    _i = i;
                    _node = node;
                }

                public static ISeq create(BitmapIndexedNode node, int i)
                {
                    return i >= node._nodes.Length
                        ? null
                        : new Seq(node._nodes[i].nodeSeq(), i, node);
                }


                #endregion

                #region ISeq members

                public override object first()
                {
                    return _s.first();
                }

                public override ISeq next()
                {
                    ISeq nexts = _s.next();
                    return ( nexts != null )
                        ? new Seq(nexts,_i,_node)
                        : create(_node, _i+1);
                }

                #endregion

                #region IObj members

                public override IObj withMeta(IPersistentMap meta)
                {
                    return new Seq(meta, _s, _i, _node);
                }

                #endregion
            }
        }

        /// <summary>
        /// Represents a leaf node corresponding to multiple map entries, all with keys that have the same hash value.
        /// </summary>
        sealed class HashCollisionNode : INode
        {
            #region Data

            readonly int _hash;
            readonly LeafNode[] _leaves;

            #endregion

            #region C-tors

            public HashCollisionNode(int hash, params LeafNode[] leaves)
            {
                _hash = hash;
                _leaves = leaves;
            }

            #endregion

            #region details

            int findIndex(int hash, object key)
            {
                for (int i = 0; i < _leaves.Length; i++)
                {
                    if (_leaves[i].find(hash, key) != null)
                        return i;
                }
                return -1;
            }

            #endregion

            #region INode Members

            public INode assoc(int shift, int hash, object key, object val, Box addedLeaf)
            {
                if (_hash == hash)
                {
                    int idx = findIndex(hash, key);
                    if (idx != -1)
                    {
                        if (_leaves[idx].val() == val)
                            return this;
                        LeafNode[] newLeaves1 = (LeafNode[])_leaves.Clone();
                        // Note: do not set addedLeaf, since we are replacing
                        newLeaves1[idx] = new LeafNode(hash, key, val);
                        return new HashCollisionNode(hash, newLeaves1);
                    }
                    LeafNode[] newLeaves = new LeafNode[_leaves.Length + 1];
                    Array.Copy(_leaves, 0, newLeaves, 0, _leaves.Length);
                    addedLeaf.Val = newLeaves[_leaves.Length] = new LeafNode(hash, key, val);
                    return new HashCollisionNode(hash, newLeaves);
                }
                return BitmapIndexedNode.create(shift, this, hash, key, val, addedLeaf);
            }

            public INode without(int hash, object key)
            {
                int idx = findIndex(hash, key);
                if (idx == -1)
                    return this;
                if (_leaves.Length == 2)
                    return idx == 0 ? _leaves[1] : _leaves[0];
                LeafNode[] newLeaves = new LeafNode[_leaves.Length - 1];
                Array.Copy(_leaves, 0, newLeaves, 0, idx);
                Array.Copy(_leaves, idx + 1, newLeaves, idx, _leaves.Length - (idx + 1));
                return new HashCollisionNode(hash, newLeaves);
            }

            public INode find(int hash, object key)
            {
                int idx = findIndex(hash, key);
                return idx != -1
                    ? _leaves[idx]
                    : null;
            }

            public ISeq nodeSeq()
            {
                return ArraySeq.create((object[])_leaves);
            }

            public int getHash()
            {
                return _hash;
            }

            #endregion
        }
        
    }
}
