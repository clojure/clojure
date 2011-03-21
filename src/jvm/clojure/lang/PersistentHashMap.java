/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

package clojure.lang;

import java.io.Serializable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;

/*
 A persistent rendition of Phil Bagwell's Hash Array Mapped Trie

 Uses path copying for persistence
 HashCollision leaves vs. extended hashing
 Node polymorphism vs. conditionals
 No sub-tree pools or root-resizing
 Any errors are my own
 */

public class PersistentHashMap extends APersistentMap implements IEditableCollection, IObj {

final int count;
final INode root;
final boolean hasNull;
final Object nullValue;
final IPersistentMap _meta;

final public static PersistentHashMap EMPTY = new PersistentHashMap(0, null, false, null);
final private static Object NOT_FOUND = new Object();

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

public static PersistentHashMap createWithCheck(Object... init){
	ITransientMap ret = EMPTY.asTransient();
	for(int i = 0; i < init.length; i += 2)
		{
		ret = ret.assoc(init[i], init[i + 1]);
		if(ret.count() != i/2 + 1)
			throw new IllegalArgumentException("Duplicate key: " + init[i]);
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

static public PersistentHashMap createWithCheck(ISeq items){
	ITransientMap ret = EMPTY.asTransient();
	for(int i=0; items != null; items = items.next().next(), ++i)
		{
		if(items.next() == null)
			throw new IllegalArgumentException(String.format("No value supplied for key: %s", items.first()));
		ret = ret.assoc(items.first(), RT.second(items));
		if(ret.count() != i + 1)
			throw new IllegalArgumentException("Duplicate key: " + items.first());
		}
	return (PersistentHashMap) ret.persistent();
}

/*
 * @param init {key1,val1,key2,val2,...}
 */
public static PersistentHashMap create(IPersistentMap meta, Object... init){
	return create(init).withMeta(meta);
}

PersistentHashMap(int count, INode root, boolean hasNull, Object nullValue){
	this.count = count;
	this.root = root;
	this.hasNull = hasNull;
	this.nullValue = nullValue;
	this._meta = null;
}

public PersistentHashMap(IPersistentMap meta, int count, INode root, boolean hasNull, Object nullValue){
	this._meta = meta;
	this.count = count;
	this.root = root;
	this.hasNull = hasNull;
	this.nullValue = nullValue;
}

public boolean containsKey(Object key){
	if(key == null)
		return hasNull;
	return (root != null) ? root.find(0, Util.hash(key), key, NOT_FOUND) != NOT_FOUND : false;
}

public IMapEntry entryAt(Object key){
	if(key == null)
		return hasNull ? new MapEntry(null, nullValue) : null;
	return (root != null) ? root.find(0, Util.hash(key), key) : null;
}

public IPersistentMap assoc(Object key, Object val){
	if(key == null) {
		if(hasNull && val == nullValue)
			return this;
		return new PersistentHashMap(meta(), hasNull ? count : count + 1, root, true, val);
	}
	Box addedLeaf = new Box(null);
	INode newroot = (root == null ? BitmapIndexedNode.EMPTY : root) 
			.assoc(0, Util.hash(key), key, val, addedLeaf);
	if(newroot == root)
		return this;
	return new PersistentHashMap(meta(), addedLeaf.val == null ? count : count + 1, newroot, hasNull, nullValue);
}

public Object valAt(Object key, Object notFound){
	if(key == null)
		return hasNull ? nullValue : notFound;
	return root != null ? root.find(0, Util.hash(key), key, notFound) : notFound;
}

public Object valAt(Object key){
	return valAt(key, null);
}

public IPersistentMap assocEx(Object key, Object val) {
	if(containsKey(key))
		throw Util.runtimeException("Key already present");
	return assoc(key, val);
}

public IPersistentMap without(Object key){
	if(key == null)
		return hasNull ? new PersistentHashMap(meta(), count - 1, root, false, null) : this;
	if(root == null)
		return this;
	INode newroot = root.without(0, Util.hash(key), key);
	if(newroot == root)
		return this;
	return new PersistentHashMap(meta(), count - 1, newroot, hasNull, nullValue); 
}

public Iterator iterator(){
	return new SeqIterator(seq());
}

public int count(){
	return count;
}

public ISeq seq(){
	ISeq s = root != null ? root.nodeSeq() : null; 
	return hasNull ? new Cons(new MapEntry(null, nullValue), s) : s;
}

public IPersistentCollection empty(){
	return EMPTY.withMeta(meta());	
}

static int mask(int hash, int shift){
	//return ((hash << shift) >>> 27);// & 0x01f;
	return (hash >>> shift) & 0x01f;
}

public PersistentHashMap withMeta(IPersistentMap meta){
	return new PersistentHashMap(meta, count, root, hasNull, nullValue);
}

public TransientHashMap asTransient() {
	return new TransientHashMap(this);
}

public IPersistentMap meta(){
	return _meta;
}

static final class TransientHashMap extends ATransientMap {
	AtomicReference<Thread> edit;
	INode root;
	int count;
	boolean hasNull;
	Object nullValue;
	final Box leafFlag = new Box(null);


	TransientHashMap(PersistentHashMap m) {
		this(new AtomicReference<Thread>(Thread.currentThread()), m.root, m.count, m.hasNull, m.nullValue);
	}
	
	TransientHashMap(AtomicReference<Thread> edit, INode root, int count, boolean hasNull, Object nullValue) {
		this.edit = edit;
		this.root = root; 
		this.count = count; 
		this.hasNull = hasNull;
		this.nullValue = nullValue;
	}

	ITransientMap doAssoc(Object key, Object val) {
		if (key == null) {
			if (this.nullValue != val)
				this.nullValue = val;
			if (!hasNull) {
				this.count++;
				this.hasNull = true;
			}
			return this;
		}
//		Box leafFlag = new Box(null);
		leafFlag.val = null;
		INode n = (root == null ? BitmapIndexedNode.EMPTY : root)
			.assoc(edit, 0, Util.hash(key), key, val, leafFlag);
		if (n != this.root)
			this.root = n; 
		if(leafFlag.val != null) this.count++;
		return this;
	}

	ITransientMap doWithout(Object key) {
		if (key == null) {
			if (!hasNull) return this;
			hasNull = false;
			nullValue = null;
			this.count--;
			return this;
		}
		if (root == null) return this;
//		Box leafFlag = new Box(null);
		leafFlag.val = null;
		INode n = root.without(edit, 0, Util.hash(key), key, leafFlag);
		if (n != root)
			this.root = n;
		if(leafFlag.val != null) this.count--;
		return this;
	}

	IPersistentMap doPersistent() {
		edit.set(null);
		return new PersistentHashMap(count, root, hasNull, nullValue);
	}

	Object doValAt(Object key, Object notFound) {
		if (key == null)
			if (hasNull)
				return nullValue;
			else
				return notFound;
		if (root == null)
			return null;
		return root.find(0, Util.hash(key), key, notFound);
	}

	int doCount() {
		return count;
	}
	
	void ensureEditable(){
		Thread owner = edit.get();
		if(owner == Thread.currentThread())
			return;
		if(owner != null)
			throw new IllegalAccessError("Transient used by non-owner thread");
		throw new IllegalAccessError("Transient used after persistent! call");
	}
}

static interface INode extends Serializable {
	INode assoc(int shift, int hash, Object key, Object val, Box addedLeaf);

	INode without(int shift, int hash, Object key);

	IMapEntry find(int shift, int hash, Object key);

	Object find(int shift, int hash, Object key, Object notFound);

	ISeq nodeSeq();

	INode assoc(AtomicReference<Thread> edit, int shift, int hash, Object key, Object val, Box addedLeaf);

	INode without(AtomicReference<Thread> edit, int shift, int hash, Object key, Box removedLeaf);
}

final static class ArrayNode implements INode{
	int count;
	final INode[] array;
	final AtomicReference<Thread> edit;

	ArrayNode(AtomicReference<Thread> edit, int count, INode[] array){
		this.array = array;
		this.edit = edit;
		this.count = count;
	}

	public INode assoc(int shift, int hash, Object key, Object val, Box addedLeaf){
		int idx = mask(hash, shift);
		INode node = array[idx];
		if(node == null)
			return new ArrayNode(null, count + 1, cloneAndSet(array, idx, BitmapIndexedNode.EMPTY.assoc(shift + 5, hash, key, val, addedLeaf)));			
		INode n = node.assoc(shift + 5, hash, key, val, addedLeaf);
		if(n == node)
			return this;
		return new ArrayNode(null, count, cloneAndSet(array, idx, n));
	}

	public INode without(int shift, int hash, Object key){
		int idx = mask(hash, shift);
		INode node = array[idx];
		if(node == null)
			return this;
		INode n = node.without(shift + 5, hash, key);
		if(n == node)
			return this;
		if (n == null) {
			if (count <= 8) // shrink
				return pack(null, idx);
			return new ArrayNode(null, count - 1, cloneAndSet(array, idx, n));
		} else 
			return new ArrayNode(null, count, cloneAndSet(array, idx, n));
	}

	public IMapEntry find(int shift, int hash, Object key){
		int idx = mask(hash, shift);
		INode node = array[idx];
		if(node == null)
			return null;
		return node.find(shift + 5, hash, key); 
	}

	public Object find(int shift, int hash, Object key, Object notFound){
		int idx = mask(hash, shift);
		INode node = array[idx];
		if(node == null)
			return notFound;
		return node.find(shift + 5, hash, key, notFound); 
	}
	
	public ISeq nodeSeq(){
		return Seq.create(array);
	}

	private ArrayNode ensureEditable(AtomicReference<Thread> edit){
		if(this.edit == edit)
			return this;
		return new ArrayNode(edit, count, this.array.clone());
	}
	
	private ArrayNode editAndSet(AtomicReference<Thread> edit, int i, INode n){
		ArrayNode editable = ensureEditable(edit);
		editable.array[i] = n;
		return editable;
	}


	private INode pack(AtomicReference<Thread> edit, int idx) {
		Object[] newArray = new Object[2*(count - 1)];
		int j = 1;
		int bitmap = 0;
		for(int i = 0; i < idx; i++)
			if (array[i] != null) {
				newArray[j] = array[i];
				bitmap |= 1 << i;
				j += 2;
			}
		for(int i = idx + 1; i < array.length; i++)
			if (array[i] != null) {
				newArray[j] = array[i];
				bitmap |= 1 << i;
				j += 2;
			}
		return new BitmapIndexedNode(edit, bitmap, newArray);
	}

	public INode assoc(AtomicReference<Thread> edit, int shift, int hash, Object key, Object val, Box addedLeaf){
		int idx = mask(hash, shift);
		INode node = array[idx];
		if(node == null) {
			ArrayNode editable = editAndSet(edit, idx, BitmapIndexedNode.EMPTY.assoc(edit, shift + 5, hash, key, val, addedLeaf));
			editable.count++;
			return editable;			
		}
		INode n = node.assoc(edit, shift + 5, hash, key, val, addedLeaf);
		if(n == node)
			return this;
		return editAndSet(edit, idx, n);
	}	

	public INode without(AtomicReference<Thread> edit, int shift, int hash, Object key, Box removedLeaf){
		int idx = mask(hash, shift);
		INode node = array[idx];
		if(node == null)
			return this;
		INode n = node.without(edit, shift + 5, hash, key, removedLeaf);
		if(n == node)
			return this;
		if(n == null) {
			if (count <= 8) // shrink
				return pack(edit, idx);
			ArrayNode editable = editAndSet(edit, idx, n);
			editable.count--;
			return editable;
		}
		return editAndSet(edit, idx, n);
	}
	
	static class Seq extends ASeq {
		final INode[] nodes;
		final int i;
		final ISeq s; 
		
		static ISeq create(INode[] nodes) {
			return create(null, nodes, 0, null);
		}
		
		private static ISeq create(IPersistentMap meta, INode[] nodes, int i, ISeq s) {
			if (s != null)
				return new Seq(meta, nodes, i, s);
			for(int j = i; j < nodes.length; j++)
				if (nodes[j] != null) {
					ISeq ns = nodes[j].nodeSeq();
					if (ns != null)
						return new Seq(meta, nodes, j + 1, ns);
				}
			return null;
		}
		
		private Seq(IPersistentMap meta, INode[] nodes, int i, ISeq s) {
			super(meta);
			this.nodes = nodes;
			this.i = i;
			this.s = s;
		}

		public Obj withMeta(IPersistentMap meta) {
			return new Seq(meta, nodes, i, s);
		}

		public Object first() {
			return s.first();
		}

		public ISeq next() {
			return create(null, nodes, i, s.next());
		}
		
	}
}

final static class BitmapIndexedNode implements INode{
	static final BitmapIndexedNode EMPTY = new BitmapIndexedNode(null, 0, new Object[0]);
	
	int bitmap;
	Object[] array;
	final AtomicReference<Thread> edit;

	final int index(int bit){
		return Integer.bitCount(bitmap & (bit - 1));
	}

	BitmapIndexedNode(AtomicReference<Thread> edit, int bitmap, Object[] array){
		this.bitmap = bitmap;
		this.array = array;
		this.edit = edit;
	}

	public INode assoc(int shift, int hash, Object key, Object val, Box addedLeaf){
		int bit = bitpos(hash, shift);
		int idx = index(bit);
		if((bitmap & bit) != 0) {
			Object keyOrNull = array[2*idx];
			Object valOrNode = array[2*idx+1];
			if(keyOrNull == null) {
				INode n = ((INode) valOrNode).assoc(shift + 5, hash, key, val, addedLeaf);
				if(n == valOrNode)
					return this;
				return new BitmapIndexedNode(null, bitmap, cloneAndSet(array, 2*idx+1, n));
			} 
			if(Util.equiv(key, keyOrNull)) {
				if(val == valOrNode)
					return this;
				return new BitmapIndexedNode(null, bitmap, cloneAndSet(array, 2*idx+1, val));
			} 
			addedLeaf.val = addedLeaf;
			return new BitmapIndexedNode(null, bitmap, 
					cloneAndSet(array, 
							2*idx, null, 
							2*idx+1, createNode(shift + 5, keyOrNull, valOrNode, hash, key, val)));
		} else {
			int n = Integer.bitCount(bitmap);
			if(n >= 16) {
				INode[] nodes = new INode[32];
				int jdx = mask(hash, shift);
				nodes[jdx] = EMPTY.assoc(shift + 5, hash, key, val, addedLeaf);  
				int j = 0;
				for(int i = 0; i < 32; i++)
					if(((bitmap >>> i) & 1) != 0) {
						if (array[j] == null)
							nodes[i] = (INode) array[j+1];
						else
							nodes[i] = EMPTY.assoc(shift + 5, Util.hash(array[j]), array[j], array[j+1], addedLeaf);
						j += 2;
					}
				return new ArrayNode(null, n + 1, nodes);
			} else {
				Object[] newArray = new Object[2*(n+1)];
				System.arraycopy(array, 0, newArray, 0, 2*idx);
				newArray[2*idx] = key;
				addedLeaf.val = addedLeaf; 
				newArray[2*idx+1] = val;
				System.arraycopy(array, 2*idx, newArray, 2*(idx+1), 2*(n-idx));
				return new BitmapIndexedNode(null, bitmap | bit, newArray);
			}
		}
	}

	public INode without(int shift, int hash, Object key){
		int bit = bitpos(hash, shift);
		if((bitmap & bit) == 0)
			return this;
		int idx = index(bit);
		Object keyOrNull = array[2*idx];
		Object valOrNode = array[2*idx+1];
		if(keyOrNull == null) {
			INode n = ((INode) valOrNode).without(shift + 5, hash, key);
			if (n == valOrNode)
				return this;
			if (n != null)
				return new BitmapIndexedNode(null, bitmap, cloneAndSet(array, 2*idx+1, n));
			if (bitmap == bit) 
				return null;
			return new BitmapIndexedNode(null, bitmap ^ bit, removePair(array, idx));
		}
		if(Util.equiv(key, keyOrNull))
			// TODO: collapse
			return new BitmapIndexedNode(null, bitmap ^ bit, removePair(array, idx));
		return this;
	}
	
	public IMapEntry find(int shift, int hash, Object key){
		int bit = bitpos(hash, shift);
		if((bitmap & bit) == 0)
			return null;
		int idx = index(bit);
		Object keyOrNull = array[2*idx];
		Object valOrNode = array[2*idx+1];
		if(keyOrNull == null)
			return ((INode) valOrNode).find(shift + 5, hash, key);
		if(Util.equiv(key, keyOrNull))
			return new MapEntry(keyOrNull, valOrNode);
		return null;
	}

	public Object find(int shift, int hash, Object key, Object notFound){
		int bit = bitpos(hash, shift);
		if((bitmap & bit) == 0)
			return notFound;
		int idx = index(bit);
		Object keyOrNull = array[2*idx];
		Object valOrNode = array[2*idx+1];
		if(keyOrNull == null)
			return ((INode) valOrNode).find(shift + 5, hash, key, notFound);
		if(Util.equiv(key, keyOrNull))
			return valOrNode;
		return notFound;
	}

	public ISeq nodeSeq(){
		return NodeSeq.create(array);
	}

	private BitmapIndexedNode ensureEditable(AtomicReference<Thread> edit){
		if(this.edit == edit)
			return this;
		int n = Integer.bitCount(bitmap);
		Object[] newArray = new Object[n >= 0 ? 2*(n+1) : 4]; // make room for next assoc
		System.arraycopy(array, 0, newArray, 0, 2*n);
		return new BitmapIndexedNode(edit, bitmap, newArray);
	}
	
	private BitmapIndexedNode editAndSet(AtomicReference<Thread> edit, int i, Object a) {
		BitmapIndexedNode editable = ensureEditable(edit);
		editable.array[i] = a;
		return editable;
	}

	private BitmapIndexedNode editAndSet(AtomicReference<Thread> edit, int i, Object a, int j, Object b) {
		BitmapIndexedNode editable = ensureEditable(edit);
		editable.array[i] = a;
		editable.array[j] = b;
		return editable;
	}

	private BitmapIndexedNode editAndRemovePair(AtomicReference<Thread> edit, int bit, int i) {
		if (bitmap == bit) 
			return null;
		BitmapIndexedNode editable = ensureEditable(edit);
		editable.bitmap ^= bit;
		System.arraycopy(editable.array, 2*(i+1), editable.array, 2*i, editable.array.length - 2*(i+1));
		editable.array[editable.array.length - 2] = null;
		editable.array[editable.array.length - 1] = null;
		return editable;
	}

	public INode assoc(AtomicReference<Thread> edit, int shift, int hash, Object key, Object val, Box addedLeaf){
		int bit = bitpos(hash, shift);
		int idx = index(bit);
		if((bitmap & bit) != 0) {
			Object keyOrNull = array[2*idx];
			Object valOrNode = array[2*idx+1];
			if(keyOrNull == null) {
				INode n = ((INode) valOrNode).assoc(edit, shift + 5, hash, key, val, addedLeaf);
				if(n == valOrNode)
					return this;
				return editAndSet(edit, 2*idx+1, n);
			} 
			if(Util.equiv(key, keyOrNull)) {
				if(val == valOrNode)
					return this;
				return editAndSet(edit, 2*idx+1, val);
			} 
			addedLeaf.val = addedLeaf;
			return editAndSet(edit, 2*idx, null, 2*idx+1, 
					createNode(edit, shift + 5, keyOrNull, valOrNode, hash, key, val)); 
		} else {
			int n = Integer.bitCount(bitmap);
			if(n*2 < array.length) {
				addedLeaf.val = addedLeaf;
				BitmapIndexedNode editable = ensureEditable(edit);
				System.arraycopy(editable.array, 2*idx, editable.array, 2*(idx+1), 2*(n-idx));
				editable.array[2*idx] = key;
				editable.array[2*idx+1] = val;
				editable.bitmap |= bit;
				return editable;
			}
			if(n >= 16) {
				INode[] nodes = new INode[32];
				int jdx = mask(hash, shift);
				nodes[jdx] = EMPTY.assoc(edit, shift + 5, hash, key, val, addedLeaf);  
				int j = 0;
				for(int i = 0; i < 32; i++)
					if(((bitmap >>> i) & 1) != 0) {
						if (array[j] == null)
							nodes[i] = (INode) array[j+1];
						else
							nodes[i] = EMPTY.assoc(edit, shift + 5, Util.hash(array[j]), array[j], array[j+1], addedLeaf);
						j += 2;
					}
				return new ArrayNode(edit, n + 1, nodes);
			} else {
				Object[] newArray = new Object[2*(n+4)];
				System.arraycopy(array, 0, newArray, 0, 2*idx);
				newArray[2*idx] = key;
				addedLeaf.val = addedLeaf; 
				newArray[2*idx+1] = val;
				System.arraycopy(array, 2*idx, newArray, 2*(idx+1), 2*(n-idx));
				BitmapIndexedNode editable = ensureEditable(edit);
				editable.array = newArray;
				editable.bitmap |= bit;
				return editable;
			}
		}
	}

	public INode without(AtomicReference<Thread> edit, int shift, int hash, Object key, Box removedLeaf){
		int bit = bitpos(hash, shift);
		if((bitmap & bit) == 0)
			return this;
		int idx = index(bit);
		Object keyOrNull = array[2*idx];
		Object valOrNode = array[2*idx+1];
		if(keyOrNull == null) {
			INode n = ((INode) valOrNode).without(edit, shift + 5, hash, key, removedLeaf);
			if (n == valOrNode)
				return this;
			if (n != null)
				return editAndSet(edit, 2*idx+1, n); 
			if (bitmap == bit) 
				return null;
			removedLeaf.val = removedLeaf;
			return editAndRemovePair(edit, bit, idx); 
		}
		if(Util.equiv(key, keyOrNull)) {
			removedLeaf.val = removedLeaf;
			// TODO: collapse
			return editAndRemovePair(edit, bit, idx); 			
		}
		return this;
	}
}

final static class HashCollisionNode implements INode{

	final int hash;
	int count;
	Object[] array;
	final AtomicReference<Thread> edit;

	HashCollisionNode(AtomicReference<Thread> edit, int hash, int count, Object... array){
		this.edit = edit;
		this.hash = hash;
		this.count = count;
		this.array = array;
	}

	public INode assoc(int shift, int hash, Object key, Object val, Box addedLeaf){
		if(hash == this.hash) {
			int idx = findIndex(key);
			if(idx != -1) {
				if(array[idx + 1] == val)
					return this;
				return new HashCollisionNode(null, hash, count, cloneAndSet(array, idx + 1, val));
			}
			Object[] newArray = new Object[array.length + 2];
			System.arraycopy(array, 0, newArray, 0, array.length);
			newArray[array.length] = key;
			newArray[array.length + 1] = val;
			addedLeaf.val = addedLeaf;
			return new HashCollisionNode(edit, hash, count + 1, newArray);
		}
		// nest it in a bitmap node
		return new BitmapIndexedNode(null, bitpos(this.hash, shift), new Object[] {null, this})
			.assoc(shift, hash, key, val, addedLeaf);
	}

	public INode without(int shift, int hash, Object key){
		int idx = findIndex(key);
		if(idx == -1)
			return this;
		if(count == 1)
			return null;
		return new HashCollisionNode(null, hash, count - 1, removePair(array, idx/2));
	}

	public IMapEntry find(int shift, int hash, Object key){
		int idx = findIndex(key);
		if(idx < 0)
			return null;
		if(Util.equiv(key, array[idx]))
			return new MapEntry(array[idx], array[idx+1]);
		return null;
	}

	public Object find(int shift, int hash, Object key, Object notFound){
		int idx = findIndex(key);
		if(idx < 0)
			return notFound;
		if(Util.equiv(key, array[idx]))
			return array[idx+1];
		return notFound;
	}

	public ISeq nodeSeq(){
		return NodeSeq.create(array);
	}

	public int findIndex(Object key){
		for(int i = 0; i < 2*count; i+=2)
			{
			if(Util.equiv(key, array[i]))
				return i;
			}
		return -1;
	}

	private HashCollisionNode ensureEditable(AtomicReference<Thread> edit){
		if(this.edit == edit)
			return this;
		return new HashCollisionNode(edit, hash, count, array);
	}

	private HashCollisionNode ensureEditable(AtomicReference<Thread> edit, int count, Object[] array){
		if(this.edit == edit) {
			this.array = array;
			this.count = count;
			return this;
		}
		return new HashCollisionNode(edit, hash, count, array);
	}

	private HashCollisionNode editAndSet(AtomicReference<Thread> edit, int i, Object a) {
		HashCollisionNode editable = ensureEditable(edit);
		editable.array[i] = a;
		return editable;
	}

	private HashCollisionNode editAndSet(AtomicReference<Thread> edit, int i, Object a, int j, Object b) {
		HashCollisionNode editable = ensureEditable(edit);
		editable.array[i] = a;
		editable.array[j] = b;
		return editable;
	}


	public INode assoc(AtomicReference<Thread> edit, int shift, int hash, Object key, Object val, Box addedLeaf){
		if(hash == this.hash) {
			int idx = findIndex(key);
			if(idx != -1) {
				if(array[idx + 1] == val)
					return this;
				return editAndSet(edit, idx+1, val); 
			}
			if (array.length > 2*count) {
				addedLeaf.val = addedLeaf;
				HashCollisionNode editable = editAndSet(edit, 2*count, key, 2*count+1, val);
				editable.count++;
				return editable;
			}
			Object[] newArray = new Object[array.length + 2];
			System.arraycopy(array, 0, newArray, 0, array.length);
			newArray[array.length] = key;
			newArray[array.length + 1] = val;
			addedLeaf.val = addedLeaf;
			return ensureEditable(edit, count + 1, newArray);
		}
		// nest it in a bitmap node
		return new BitmapIndexedNode(edit, bitpos(this.hash, shift), new Object[] {null, this, null, null})
			.assoc(edit, shift, hash, key, val, addedLeaf);
	}	

	public INode without(AtomicReference<Thread> edit, int shift, int hash, Object key, Box removedLeaf){
		int idx = findIndex(key);
		if(idx == -1)
			return this;
		if(count == 1)
			return null;
		HashCollisionNode editable = ensureEditable(edit);
		editable.array[idx] = editable.array[2*count-2];
		editable.array[idx+1] = editable.array[2*count-1];
		editable.array[2*count-2] = editable.array[2*count-1] = null;
		editable.count--;
		return editable;
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

private static INode[] cloneAndSet(INode[] array, int i, INode a) {
	INode[] clone = array.clone();
	clone[i] = a;
	return clone;
}

private static Object[] cloneAndSet(Object[] array, int i, Object a) {
	Object[] clone = array.clone();
	clone[i] = a;
	return clone;
}

private static Object[] cloneAndSet(Object[] array, int i, Object a, int j, Object b) {
	Object[] clone = array.clone();
	clone[i] = a;
	clone[j] = b;
	return clone;
}

private static Object[] removePair(Object[] array, int i) {
	Object[] newArray = new Object[array.length - 2];
	System.arraycopy(array, 0, newArray, 0, 2*i);
	System.arraycopy(array, 2*(i+1), newArray, 2*i, newArray.length - 2*i);
	return newArray;
}

private static INode createNode(int shift, Object key1, Object val1, int key2hash, Object key2, Object val2) {
	int key1hash = Util.hash(key1);
	if(key1hash == key2hash)
		return new HashCollisionNode(null, key1hash, 2, new Object[] {key1, val1, key2, val2});
	Box _ = new Box(null);
	AtomicReference<Thread> edit = new AtomicReference<Thread>();
	return BitmapIndexedNode.EMPTY
		.assoc(edit, shift, key1hash, key1, val1, _)
		.assoc(edit, shift, key2hash, key2, val2, _);
}

private static INode createNode(AtomicReference<Thread> edit, int shift, Object key1, Object val1, int key2hash, Object key2, Object val2) {
	int key1hash = Util.hash(key1);
	if(key1hash == key2hash)
		return new HashCollisionNode(null, key1hash, 2, new Object[] {key1, val1, key2, val2});
	Box _ = new Box(null);
	return BitmapIndexedNode.EMPTY
		.assoc(edit, shift, key1hash, key1, val1, _)
		.assoc(edit, shift, key2hash, key2, val2, _);
}

private static int bitpos(int hash, int shift){
	return 1 << mask(hash, shift);
}

static final class NodeSeq extends ASeq {
	final Object[] array;
	final int i;
	final ISeq s;
	
	NodeSeq(Object[] array, int i) {
		this(null, array, i, null);
	}

	static ISeq create(Object[] array) {
		return create(array, 0, null);
	}

	private static ISeq create(Object[] array, int i, ISeq s) {
		if(s != null)
			return new NodeSeq(null, array, i, s);
		for(int j = i; j < array.length; j+=2) {
			if(array[j] != null)
				return new NodeSeq(null, array, j, null);
			INode node = (INode) array[j+1];
			if (node != null) {
				ISeq nodeSeq = node.nodeSeq();
				if(nodeSeq != null)
					return new NodeSeq(null, array, j + 2, nodeSeq);
			}
		}
		return null;
	}
	
	NodeSeq(IPersistentMap meta, Object[] array, int i, ISeq s) {
		super(meta);
		this.array = array;
		this.i = i;
		this.s = s;
	}

	public Obj withMeta(IPersistentMap meta) {
		return new NodeSeq(meta, array, i, s);
	}

	public Object first() {
		if(s != null)
			return s.first();
		return new MapEntry(array[i], array[i+1]);
	}

	public ISeq next() {
		if(s != null)
			return create(array, i, s.next());
		return create(array, i + 2, null);
	}
}

}