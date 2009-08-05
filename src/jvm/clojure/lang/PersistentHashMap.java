/**
 Copyright (c) 2007-2008, Rich Hickey
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:

 * Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above
   copyright notice, this list of conditions and the following
   disclaimer in the documentation and/or other materials provided
   with the distribution.

 * Neither the name of Clojure nor the names of its contributors
   may be used to endorse or promote products derived from this
   software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 POSSIBILITY OF SUCH DAMAGE.
 **/

/* rich Jun 18, 2007 */

package clojure.lang;

import java.util.*;
//this stuff is just for the test main()
import java.util.concurrent.atomic.AtomicReference;

import clojure.lang.PersistentVector.Node;

/*
 A persistent rendition of Phil Bagwell's Hash Array Mapped Trie

 Uses path copying for persistence
 HashCollision leaves vs. extended hashing
 Node polymorphism vs. conditionals
 No sub-tree pools or root-resizing
 Any errors are my own
 */

public class PersistentHashMap extends APersistentMap implements IEditableCollection {

final int count;
final INode root;

final public static PersistentHashMap EMPTY = new PersistentHashMap(0, new EmptyNode());

static public IPersistentMap create(Map other){
	ITransientMap ret = EMPTY.asTransient();
	for(Object o : other.entrySet())
		{
		Map.Entry e = (Entry) o;
		ret = ret.assoc(e.getKey(), e.getValue());
		}
	return ret.persistent();
}

/*
 * @param init {key1,val1,key2,val2,...}
 */
public static PersistentHashMap create(Object... init){
	ITransientMap ret = EMPTY.asTransient();
	for(int i = 0; i < init.length; i += 2)
		{
		ret = ret.assoc(init[i], init[i + 1]);
		}
	return (PersistentHashMap) ret.persistent();
}

public static PersistentHashMap create(List init){
	ITransientMap ret = EMPTY.asTransient();
	for(Iterator i = init.iterator(); i.hasNext();)
		{
		Object key = i.next();
		if(!i.hasNext())
			throw new IllegalArgumentException(String.format("No value supplied for key: %s", key));
		Object val = i.next();
		ret = ret.assoc(key, val);
		}
	return (PersistentHashMap) ret.persistent();
}

static public PersistentHashMap create(ISeq items){
	ITransientMap ret = EMPTY.asTransient();
	for(; items != null; items = items.next().next())
		{
		if(items.next() == null)
			throw new IllegalArgumentException(String.format("No value supplied for key: %s", items.first()));
		ret = ret.assoc(items.first(), RT.second(items));
		}
	return (PersistentHashMap) ret.persistent();
}

/*
 * @param init {key1,val1,key2,val2,...}
 */
public static PersistentHashMap create(IPersistentMap meta, Object... init){
	return create(init).withMeta(meta);
}

PersistentHashMap(int count, INode root){
	this.count = count;
	this.root = root;
}


public PersistentHashMap(IPersistentMap meta, int count, INode root){
	super(meta);
	this.count = count;
	this.root = root;
}

public boolean containsKey(Object key){
	return entryAt(key) != null;
}

public IMapEntry entryAt(Object key){
	return root.find(Util.hash(key), key);
}

public IPersistentMap assoc(Object key, Object val){
	Box addedLeaf = new Box(null);
	INode newroot = root.assoc(0, Util.hash(key), key, val, addedLeaf);
	if(newroot == root)
		return this;
	return new PersistentHashMap(meta(), addedLeaf.val == null ? count : count + 1, newroot);
}

public Object valAt(Object key, Object notFound){
	IMapEntry e = entryAt(key);
	if(e != null)
		return e.val();
	return notFound;
}

public Object valAt(Object key){
	return valAt(key, null);
}

public IPersistentMap assocEx(Object key, Object val) throws Exception{
	if(containsKey(key))
		throw new Exception("Key already present");
	return assoc(key, val);
}

public IPersistentMap without(Object key){
	INode newroot = root.without(Util.hash(key), key);
	if(newroot == root)
		return this;
	if(newroot == null)
		return EMPTY.withMeta(meta());
	return new PersistentHashMap(meta(), count - 1, newroot);
}

public Iterator iterator(){
	return new SeqIterator(seq());
}

public int count(){
	return count;
}

public ISeq seq(){
	return root.nodeSeq();
}

public IPersistentCollection empty(){
	return EMPTY.withMeta(meta());	
}

static int mask(int hash, int shift){
	//return ((hash << shift) >>> 27);// & 0x01f;
	return (hash >>> shift) & 0x01f;
}

public PersistentHashMap withMeta(IPersistentMap meta){
	return new PersistentHashMap(meta, count, root);
}

public TransientHashMap asTransient() {
	return new TransientHashMap(this);
}

static final class TransientHashMap extends AFn implements ITransientMap {
	AtomicReference<Thread> edit;
	INode root;
	int count;
	
	TransientHashMap(PersistentHashMap m) {
		this(new AtomicReference<Thread>(Thread.currentThread()), m.root, m.count);
	}
	
	TransientHashMap(AtomicReference<Thread> edit, INode root, int count) {
		this.edit = edit;
		this.root = root; 
		this.count = count; 
	}

	public ITransientMap assoc(Object key, Object val) {
		ensureEditable();
		Box addedLeaf = new Box(null);
		this.root = root.assoc(edit, 0, Util.hash(key), key, val, addedLeaf);
		if (addedLeaf.val != null) this.count++;
		return this;
	}

	public ITransientMap without(Object key) {
		ensureEditable();
		Box removedLeaf = new Box(null);
		INode newroot = root.without(edit, Util.hash(key), key, removedLeaf);
		this.root = newroot == null ? EMPTY.root : newroot;
		if (removedLeaf != null) this.count--;
		return this;
	}

	public ITransientMap conj(Object o) {
		ensureEditable();
		if(o instanceof Map.Entry)
			{
			Map.Entry e = (Map.Entry) o;
		
			return assoc(e.getKey(), e.getValue());
			}
		else if(o instanceof IPersistentVector)
			{
			IPersistentVector v = (IPersistentVector) o;
			if(v.count() != 2)
				throw new IllegalArgumentException("Vector arg to map conj must be a pair");
			return assoc(v.nth(0), v.nth(1));
			}
		
		ITransientMap ret = this;
		for(ISeq es = RT.seq(o); es != null; es = es.next())
			{
			Map.Entry e = (Map.Entry) es.first();
			ret = ret.assoc(e.getKey(), e.getValue());
			}
		return ret;
	}

	public IPersistentMap persistent() {
		ensureEditable();
		edit.set(null);
		return new PersistentHashMap(count, root);
	}

	public IMapEntry entryAt(Object key){
		return root.find(Util.hash(key), key);
	}

	public Object valAt(Object key) {
		return valAt(key, null);
	}

	public Object valAt(Object key, Object notFound) {
		ensureEditable();
		IMapEntry e = entryAt(key);
		if(e != null)
			return e.val();
		return notFound;
	}

	public Object invoke(Object arg1) throws Exception{
		return valAt(arg1);
	}

	public Object invoke(Object arg1, Object notFound) throws Exception{
		return valAt(arg1, notFound);
	}

	public int count() {
		ensureEditable();
		return count;
	}
	
	void ensureEditable(){
		Thread owner = edit.get();
		if(owner == Thread.currentThread())
			return;
		if(owner != null)
			throw new IllegalAccessError("Mutable used by non-owner thread");
		throw new IllegalAccessError("Mutable used after immutable call");
	}
}

/*
final static int[] pathmasks = new int[32];
static{
	pathmasks[0] = 0;
	for(int i=1;i<32;i++)
		pathmasks[i] = 0x80000000 >> (i - 1);
}
//final static int pathmask(int hash, int shift){
//	//return hash & (0x80000000 >> (shift - 1));
//	return hash & pathmasks[shift];
//	}

static boolean diffPath(int shift, int hash1, int hash2){
	return shift > 0 && ((hash1^hash2) & pathmasks[shift]) != 0;
	//return shift > 0 && pathmask(hash1^hash2,shift) != 0;
}
*/
static interface INode{
	INode assoc(int shift, int hash, Object key, Object val, Box addedLeaf);

	INode without(int hash, Object key);

	LeafNode find(int hash, Object key);

	ISeq nodeSeq();

	int getHash();

	INode assoc(AtomicReference<Thread> edit, int shift, int hash, Object key, Object val, Box addedLeaf);

	INode without(AtomicReference<Thread> edit, int hash, Object key, Box removedLeaf);
}

/*
static interface ILeaf extends INode{
	int getHash();
}
*/

final static class EmptyNode implements INode{

	public INode assoc(int shift, int hash, Object key, Object val, Box addedLeaf){
		INode ret = new LeafNode(null, hash, key, val);
		addedLeaf.val = ret;
		return ret;
	}

	public INode without(int hash, Object key){
		return this;
	}

	public LeafNode find(int hash, Object key){
		return null;
	}

	public ISeq nodeSeq(){
		return null;
	}

	public int getHash(){
		return 0;
	}
	
	public INode assoc(AtomicReference<Thread> edit, int shift, int hash, Object key, Object val, Box addedLeaf){
		return assoc(shift, hash, key, val, addedLeaf);
	}	

	public INode without(AtomicReference<Thread> edit, int hash, Object key, Box removedLeaf){
		return this;
	}
}

final static class FullNode implements INode{
	final INode[] nodes;
	final int shift;
	final int _hash;
	final AtomicReference<Thread> edit;

	static int bitpos(int hash, int shift){
		return 1 << mask(hash, shift);
	}

	FullNode(AtomicReference<Thread> edit, INode[] nodes, int shift){
		this.nodes = nodes;
		this.shift = shift;
		this._hash = nodes[0].getHash();
		this.edit = edit;
	}

	public INode assoc(int levelShift, int hash, Object key, Object val, Box addedLeaf){
//		if(levelShift < shift && diffPath(shift,hash,_hash))
//			return BitmapIndexedNode.create(levelShift, this, hash, key, val, addedLeaf);
		int idx = mask(hash, shift);

		INode n = nodes[idx].assoc(shift + 5, hash, key, val, addedLeaf);
		if(n == nodes[idx])
			return this;
		else
			{
			INode[] newnodes = nodes.clone();
			newnodes[idx] = n;
			return new FullNode(null, newnodes, shift);
			}
	}

	public INode without(int hash, Object key){
//		if(diffPath(shift,hash,_hash))
//			return this;
		int idx = mask(hash, shift);
		INode n = nodes[idx].without(hash, key);
		if(n != nodes[idx])
			{
			if(n == null)
				{
				INode[] newnodes = new INode[nodes.length - 1];
				System.arraycopy(nodes, 0, newnodes, 0, idx);
				System.arraycopy(nodes, idx + 1, newnodes, idx, nodes.length - (idx + 1));
				return new BitmapIndexedNode(null, ~bitpos(hash, shift), newnodes, shift);
				}
			INode[] newnodes = nodes.clone();
			newnodes[idx] = n;
			return new FullNode(null, newnodes, shift);
			}
		return this;
	}

	public LeafNode find(int hash, Object key){
//		if(diffPath(shift,hash,_hash))
//			return null;
		return nodes[mask(hash, shift)].find(hash, key);
	}

	public ISeq nodeSeq(){
		return Seq.create(this, 0);
	}

	public int getHash(){
		return _hash;
	}

	static class Seq extends ASeq{
		final ISeq s;
		final int i;
		final FullNode node;

		Seq(ISeq s, int i, FullNode node){
			this.s = s;
			this.i = i;
			this.node = node;
		}

		Seq(IPersistentMap meta, ISeq s, int i, FullNode node){
			super(meta);
			this.s = s;
			this.i = i;
			this.node = node;
		}

		static ISeq create(FullNode node, int i){
			if(i >= node.nodes.length)
				return null;
			return new Seq(node.nodes[i].nodeSeq(), i, node);
		}

		public Object first(){
			return s.first();
		}

		public ISeq next(){
			ISeq nexts = s.next();
			if(nexts != null)
				return new Seq(nexts, i, node);
			return create(node, i + 1);
		}

		public Seq withMeta(IPersistentMap meta){
			return new Seq(meta, s, i, node);
		}
    }

	FullNode ensureEditable(AtomicReference<Thread> edit){
		if(this.edit == edit)
			return this;
		return new FullNode(edit, this.nodes.clone(), shift);
	}

	public INode assoc(AtomicReference<Thread> edit, int shift, int hash, Object key, Object val, Box addedLeaf){
		int idx = mask(hash, shift);

		INode n = nodes[idx].assoc(edit, shift + 5, hash, key, val, addedLeaf);
		if(n == nodes[idx])
			return this;
		else
			{
			FullNode node = this.ensureEditable(edit);
			node.nodes[idx] = n;
			return node;
			}
	}	

	public INode without(AtomicReference<Thread> edit, int hash, Object key, Box removedLeaf){
		int idx = mask(hash, shift);
		INode n = nodes[idx].without(edit, hash, key, removedLeaf);
		if(n != nodes[idx])
			{
			if(n == null)
				{
				INode[] newnodes = new INode[nodes.length - 1];
				System.arraycopy(nodes, 0, newnodes, 0, idx);
				System.arraycopy(nodes, idx + 1, newnodes, idx, nodes.length - (idx + 1));
				return new BitmapIndexedNode(edit, ~bitpos(hash, shift), newnodes, shift);
				}
			FullNode node = ensureEditable(edit);
			node.nodes[idx] = n;
			return node;
			}
		return this;
	}
}

final static class BitmapIndexedNode implements INode{
	final int bitmap;
	final INode[] nodes;
	final int shift;
	final int _hash;
	final AtomicReference<Thread> edit;

	static int bitpos(int hash, int shift){
		return 1 << mask(hash, shift);
	}

	final int index(int bit){
		return Integer.bitCount(bitmap & (bit - 1));
	}

	BitmapIndexedNode(AtomicReference<Thread> edit, int bitmap, INode[] nodes, int shift){
		this.bitmap = bitmap;
		this.nodes = nodes;
		this.shift = shift;
		this._hash = nodes[0].getHash();
		this.edit = edit;
	}

	static INode create(AtomicReference<Thread> edit, int bitmap, INode[] nodes, int shift){
		if(bitmap == -1)
			return new FullNode(edit, nodes, shift);
		return new BitmapIndexedNode(edit, bitmap, nodes, shift);
	}

	static INode create(AtomicReference<Thread> edit, int shift, INode branch, int hash, Object key, Object val, Box addedLeaf){
//		int hx = branch.getHash()^hash;
//		while(mask(hx,shift) == 0)
//			shift += 5;
//		if(mask(branch.getHash(),shift) == mask(hash,shift))
//			return create(shift+5,branch,hash,key,val,addedLeaf);
		BitmapIndexedNode node = new BitmapIndexedNode(edit, bitpos(branch.getHash(), shift), new INode[]{branch}, shift); 
		return edit == null ? node.assoc(shift, hash, key, val, addedLeaf) : node.assoc(edit, shift, hash, key, val, addedLeaf);
	}

	public INode assoc(int levelShift, int hash, Object key, Object val, Box addedLeaf){
//		if(levelShift < shift && diffPath(shift,hash,_hash))
//			return create(levelShift, this, hash, key, val, addedLeaf);
		int bit = bitpos(hash, shift);
		int idx = index(bit);
		if((bitmap & bit) != 0)
			{
			INode n = nodes[idx].assoc(shift + 5, hash, key, val, addedLeaf);
			if(n == nodes[idx])
				return this;
			else
				{
				INode[] newnodes = nodes.clone();
				newnodes[idx] = n;
				return new BitmapIndexedNode(null, bitmap, newnodes, shift);
				}
			}
		else
			{
			INode[] newnodes = new INode[nodes.length + 1];
			System.arraycopy(nodes, 0, newnodes, 0, idx);
			addedLeaf.val = newnodes[idx] = new LeafNode(null, hash, key, val);
			System.arraycopy(nodes, idx, newnodes, idx + 1, nodes.length - idx);
			return create(null, bitmap | bit, newnodes, shift);
			}
	}

	public INode without(int hash, Object key){
//		if(diffPath(shift,hash,_hash))
//			return this;
		int bit = bitpos(hash, shift);
		if((bitmap & bit) != 0)
			{
			int idx = index(bit);
			INode n = nodes[idx].without(hash, key);
			if(n != nodes[idx])
				{
				if(n == null)
					{
					if(bitmap == bit)
						return null;
//					if(nodes.length == 2)
//						return nodes[idx == 0?1:0];
					INode[] newnodes = new INode[nodes.length - 1];
					System.arraycopy(nodes, 0, newnodes, 0, idx);
					System.arraycopy(nodes, idx + 1, newnodes, idx, nodes.length - (idx + 1));
					return new BitmapIndexedNode(null, bitmap & ~bit, newnodes, shift);
					}
				INode[] newnodes = nodes.clone();
				newnodes[idx] = n;
				return new BitmapIndexedNode(null, bitmap, newnodes, shift);
				}
			}
		return this;
	}

	public LeafNode find(int hash, Object key){
//		if(diffPath(shift,hash,_hash))
//			return null;
		int bit = bitpos(hash, shift);
		if((bitmap & bit) != 0)
			{
			return nodes[index(bit)].find(hash, key);
			}
		else
			return null;
	}

	public int getHash(){
		return _hash;
	}

	public ISeq nodeSeq(){
		return Seq.create(this, 0);
	}

	static class Seq extends ASeq{
		final ISeq s;
		final int i;
		final BitmapIndexedNode node;


		Seq(ISeq s, int i, BitmapIndexedNode node){
			this.s = s;
			this.i = i;
			this.node = node;
		}

		Seq(IPersistentMap meta, ISeq s, int i, BitmapIndexedNode node){
			super(meta);
			this.s = s;
			this.i = i;
			this.node = node;
		}

		static ISeq create(BitmapIndexedNode node, int i){
			if(i >= node.nodes.length)
				return null;
			return new Seq(node.nodes[i].nodeSeq(), i, node);
		}

		public Object first(){
			return s.first();
		}

		public ISeq next(){
			ISeq nexts = s.next();
			if(nexts != null)
				return new Seq(nexts, i, node);
			return create(node, i + 1);
		}

		public Seq withMeta(IPersistentMap meta){
			return new Seq(meta, s, i, node);
		}
    }
	
	BitmapIndexedNode ensureEditable(AtomicReference<Thread> edit){
		if(this.edit == edit)
			return this;
		return new BitmapIndexedNode(edit, bitmap, nodes.clone(), shift);
	}

	public INode assoc(AtomicReference<Thread> edit, int shift, int hash, Object key, Object val, Box addedLeaf){
		int bit = bitpos(hash, shift);
		int idx = index(bit);
		if((bitmap & bit) != 0)
			{
			INode n = nodes[idx].assoc(shift + 5, hash, key, val, addedLeaf);
			if(n == nodes[idx])
				return this;
			else
				{
				BitmapIndexedNode node = ensureEditable(edit);
				node.nodes[idx] = n;
				return node;
				}
			}
		else
			{
			// TODO can do better
			INode[] newnodes = new INode[nodes.length + 1];
			System.arraycopy(nodes, 0, newnodes, 0, idx);
			addedLeaf.val = newnodes[idx] = new LeafNode(null, hash, key, val);
			System.arraycopy(nodes, idx, newnodes, idx + 1, nodes.length - idx);
			return create(edit, bitmap | bit, newnodes, shift);
			}
	}

	public INode without(AtomicReference<Thread> edit, int hash, Object key, Box removedLeaf){
		int bit = bitpos(hash, shift);
		if((bitmap & bit) != 0)
			{
			int idx = index(bit);
			INode n = nodes[idx].without(edit, hash, key, removedLeaf);
			if(n != nodes[idx])
				{
				if(n == null)
					{
					if(bitmap == bit)
						return null;
					INode[] newnodes = new INode[nodes.length - 1];
					System.arraycopy(nodes, 0, newnodes, 0, idx);
					System.arraycopy(nodes, idx + 1, newnodes, idx, nodes.length - (idx + 1));
					return new BitmapIndexedNode(edit, bitmap & ~bit, newnodes, shift);
					}
				BitmapIndexedNode node = ensureEditable(edit);
				node.nodes[idx] = n;
				return node;
				}
			}
		return this;
	}
}

final static class LeafNode extends AMapEntry implements INode{
	final int hash;
	final Object key;
	Object val;
	final AtomicReference<Thread> edit; 

	public LeafNode(AtomicReference<Thread> edit, int hash, Object key, Object val){
		this.edit = edit;
		this.hash = hash;
		this.key = key;
		this.val = val;
	}

	public INode assoc(int shift, int hash, Object key, Object val, Box addedLeaf){
		if(hash == this.hash)
			{
			if(Util.equals(key, this.key))
				{
				if(val == this.val)
					return this;
				//note  - do not set addedLeaf, since we are replacing
				return new LeafNode(null, hash, key, val);
				}
			//hash collision - same hash, different keys
			LeafNode newLeaf = new LeafNode(null, hash, key, val);
			addedLeaf.val = newLeaf;
			return new HashCollisionNode(null, hash, this, newLeaf);
			}
		return BitmapIndexedNode.create(null, shift, this, hash, key, val, addedLeaf);
	}

	public INode without(int hash, Object key){
		if(hash == this.hash && Util.equals(key, this.key))
			return null;
		return this;
	}

	public LeafNode find(int hash, Object key){
		if(hash == this.hash && Util.equals(key, this.key))
			return this;
		return null;
	}

	public ISeq nodeSeq(){
		return RT.cons(this, null);
	}

	public int getHash(){
		return hash;
	}

	public Object key(){
		return this.key;
	}

	public Object val(){
		return this.val;
	}

	public Object getKey(){
		return this.key;
	}

	public Object getValue(){
		return this.val;
	}
	
	LeafNode ensureEditable(AtomicReference<Thread> edit){
		if(this.edit == edit)
			return this;
		return new LeafNode(edit, this.hash, this.key, this.val);
	}

	public INode assoc(AtomicReference<Thread> edit, int shift, int hash, Object key, Object val, Box addedLeaf){
		if(hash == this.hash)
			{
			if(Util.equals(key, this.key))
				{
				if(val == this.val)
					return this;
				LeafNode node = ensureEditable(edit);
				node.val = val;
				//note  - do not set addedLeaf, since we are replacing
				return node;
				}
			//hash collision - same hash, different keys
			LeafNode newLeaf = new LeafNode(edit, hash, key, val);
			addedLeaf.val = newLeaf;
			return new HashCollisionNode(edit, hash, this, newLeaf);
			}
		return BitmapIndexedNode.create(edit, shift, this, hash, key, val, addedLeaf);
	}	

	public INode without(AtomicReference<Thread> edit, int hash, Object key, Box removedLeaf){
		if(hash == this.hash && Util.equals(key, this.key)) {
			removedLeaf.val = this;
			return null;
		}
		return this;
	}
}

final static class HashCollisionNode implements INode{

	final int hash;
	final LeafNode[] leaves;
	final AtomicReference<Thread> edit;

	public HashCollisionNode(AtomicReference<Thread> edit, int hash, LeafNode... leaves){
		this.edit = edit;
		this.hash = hash;
		this.leaves = leaves;
	}

	public INode assoc(int shift, int hash, Object key, Object val, Box addedLeaf){
		if(hash == this.hash)
			{
			int idx = findIndex(hash, key);
			if(idx != -1)
				{
				if(leaves[idx].val == val)
					return this;
				LeafNode[] newLeaves = leaves.clone();
				//note  - do not set addedLeaf, since we are replacing
				newLeaves[idx] = new LeafNode(null, hash, key, val);
				return new HashCollisionNode(null, hash, newLeaves);
				}
			LeafNode[] newLeaves = new LeafNode[leaves.length + 1];
			System.arraycopy(leaves, 0, newLeaves, 0, leaves.length);
			addedLeaf.val = newLeaves[leaves.length] = new LeafNode(null, hash, key, val);
			return new HashCollisionNode(null, hash, newLeaves);
			}
		return BitmapIndexedNode.create(null, shift, this, hash, key, val, addedLeaf);
	}

	public INode without(int hash, Object key){
		int idx = findIndex(hash, key);
		if(idx == -1)
			return this;
		if(leaves.length == 2)
			return idx == 0 ? leaves[1] : leaves[0];
		LeafNode[] newLeaves = new LeafNode[leaves.length - 1];
		System.arraycopy(leaves, 0, newLeaves, 0, idx);
		System.arraycopy(leaves, idx + 1, newLeaves, idx, leaves.length - (idx + 1));
		return new HashCollisionNode(null, hash, newLeaves);
	}

	public LeafNode find(int hash, Object key){
		int idx = findIndex(hash, key);
		if(idx != -1)
			return leaves[idx];
		return null;
	}

	public ISeq nodeSeq(){
		return ArraySeq.create((Object[]) leaves);
	}

	int findIndex(int hash, Object key){
		for(int i = 0; i < leaves.length; i++)
			{
			if(leaves[i].find(hash, key) != null)
				return i;
			}
		return -1;
	}

	public int getHash(){
		return hash;
	}

	HashCollisionNode ensureEditable(AtomicReference<Thread> edit){
		if(this.edit == edit)
			return this;
		return new HashCollisionNode(edit, hash, leaves);
	}

	public INode assoc(AtomicReference<Thread> edit, int shift, int hash, Object key, Object val, Box addedLeaf){
		if(hash == this.hash)
			{
			int idx = findIndex(hash, key);
			if(idx != -1)
				{
				if(leaves[idx].val == val)
					return this;
				LeafNode leaf = leaves[idx].ensureEditable(edit);
				leaf.val = val;
				if (leaves[idx] == leaf)
					return this;
				HashCollisionNode node = ensureEditable(edit);
				node.leaves[idx] = leaf;
				return node;
				}
			LeafNode[] newLeaves = new LeafNode[leaves.length + 1];
			System.arraycopy(leaves, 0, newLeaves, 0, leaves.length);
			addedLeaf.val = newLeaves[leaves.length] = new LeafNode(null, hash, key, val);
			return new HashCollisionNode(edit, hash, newLeaves);
			}
		return BitmapIndexedNode.create(edit, shift, this, hash, key, val, addedLeaf);
	}	

	public INode without(AtomicReference<Thread> edit, int hash, Object key, Box removedLeaf){
		int idx = findIndex(hash, key);
		if(idx == -1)
			return this;
		removedLeaf.val = leaves[idx];
		if(leaves.length == 2)
			return idx == 0 ? leaves[1] : leaves[0];
		LeafNode[] newLeaves = new LeafNode[leaves.length - 1];
		System.arraycopy(leaves, 0, newLeaves, 0, idx);
		System.arraycopy(leaves, idx + 1, newLeaves, idx, leaves.length - (idx + 1));
		return new HashCollisionNode(edit, hash, newLeaves);
	}
}

/*
public static void main(String[] args){
	try
		{
		ArrayList words = new ArrayList();
		Scanner s = new Scanner(new File(args[0]));
		s.useDelimiter(Pattern.compile("\\W"));
		while(s.hasNext())
			{
			String word = s.next();
			words.add(word);
			}
		System.out.println("words: " + words.size());
		IPersistentMap map = PersistentHashMap.EMPTY;
		//IPersistentMap map = new PersistentTreeMap();
		//Map ht = new Hashtable();
		Map ht = new HashMap();
		Random rand;

		System.out.println("Building map");
		long startTime = System.nanoTime();
		for(Object word5 : words)
			{
			map = map.assoc(word5, word5);
			}
		rand = new Random(42);
		IPersistentMap snapshotMap = map;
		for(int i = 0; i < words.size() / 200; i++)
			{
			map = map.without(words.get(rand.nextInt(words.size() / 2)));
			}
		long estimatedTime = System.nanoTime() - startTime;
		System.out.println("count = " + map.count() + ", time: " + estimatedTime / 1000000);

		System.out.println("Building ht");
		startTime = System.nanoTime();
		for(Object word1 : words)
			{
			ht.put(word1, word1);
			}
		rand = new Random(42);
		for(int i = 0; i < words.size() / 200; i++)
			{
			ht.remove(words.get(rand.nextInt(words.size() / 2)));
			}
		estimatedTime = System.nanoTime() - startTime;
		System.out.println("count = " + ht.size() + ", time: " + estimatedTime / 1000000);

		System.out.println("map lookup");
		startTime = System.nanoTime();
		int c = 0;
		for(Object word2 : words)
			{
			if(!map.contains(word2))
				++c;
			}
		estimatedTime = System.nanoTime() - startTime;
		System.out.println("notfound = " + c + ", time: " + estimatedTime / 1000000);
		System.out.println("ht lookup");
		startTime = System.nanoTime();
		c = 0;
		for(Object word3 : words)
			{
			if(!ht.containsKey(word3))
				++c;
			}
		estimatedTime = System.nanoTime() - startTime;
		System.out.println("notfound = " + c + ", time: " + estimatedTime / 1000000);
		System.out.println("snapshotMap lookup");
		startTime = System.nanoTime();
		c = 0;
		for(Object word4 : words)
			{
			if(!snapshotMap.contains(word4))
				++c;
			}
		estimatedTime = System.nanoTime() - startTime;
		System.out.println("notfound = " + c + ", time: " + estimatedTime / 1000000);
		}
	catch(FileNotFoundException e)
		{
		e.printStackTrace();
		}

}
*/
}

