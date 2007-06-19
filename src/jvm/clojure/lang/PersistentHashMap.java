/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Jun 18, 2007 */

package clojure.lang;

public class PersistentHashMap {

final int count;
final INode root;

static interface INode{
	INode assoc(int level, int hash, Object key, Object val);
	INode without(int hash, Object key);
	Leaf find(int hash, Object key);
}

static interface ILeaf extends INode{
	int getHash();
}

final static class Node implements INode{
	final int bitmap;
	final INode[] nodes;
	final int level;

	public Node(ILeaf leaf1, ILeaf leaf2){
		//presumes will have different hashes
		//but not necessarily at this level
	}

	final int mask(int hash)
	{
		return (hash >>> (5 * level))&0x01f;
	}

	final int index(int bit)
	{
		return Integer.bitCount(bitmap & (bit-1));
	}


	Node(int bitmap, INode[] nodes, int level){
		this.bitmap = bitmap;
		this.nodes = nodes;
		this.level = level;
	}

	public INode assoc(int level, int hash, Object key, Object val){
		int bit = 1 << mask(hash);
		int idx = index(bit);
		if((bitmap & bit) != 0)
			{
			INode n = nodes[idx].assoc(level+1, hash, key, val);
			if(n == nodes[idx])
				return this;
			else
				{
				INode[] newnodes = nodes.clone();
				newnodes[idx] = n;
				return new Node(bitmap, newnodes, level);
				}
			}
		else
			{
			INode[] newnodes = new INode[nodes.length + 1];
			for(int i=0;i<idx;++i)
				newnodes[i] = nodes[i];
			newnodes[idx] =	new Leaf(hash, key, val);
			for(int i=idx;i<nodes.length;++i)
				newnodes[i+1] = nodes[i];
			return new Node(bitmap | bit, newnodes, level);
			}
	}

	public INode without(int hash, Object key){
		int bit = 1 << mask(hash);
		if((bitmap & bit) != 0)
			{
			int idx = index(bit);
			INode n = nodes[idx].without(hash, key);
			if(n != nodes[idx])
				{
				if(n == null && bitmap == bit)
					{
					return null;
					}
				}
			}
		return this;
	}

	public Leaf find(int hash, Object key){
		int bit = 1 << mask(hash);
		if((bitmap & bit) != 0)
			{
			return nodes[index(bit)].find(hash,key);
			}
		else
			return null;
	}
}

final static class Leaf implements ILeaf{
	final int hash;
	final Object key;
	final Object val;

	public Leaf(int hash, Object key, Object val){
		this.hash = hash;
		this.key = key;
		this.val = val;
	}

	public INode assoc(int level, int hash, Object key, Object val){
		if(hash == this.hash)
			{
			if(key.equals(this.key))
				return this;
			//hash collision
			return new MultiLeaf(hash, this, new Leaf(hash, key, val));
			}
	}

	public INode without(int hash, Object key){
		if(hash == this.hash && key.equals(this.key))
			return null;
		return this;
	}

	public Leaf find(int hash, Object key){
		if(hash == this.hash && key.equals(this.key))
			return this;
		return null;
	}

	public int getHash(){
		return hash;
	}
}

final static class MultiLeaf implements ILeaf{

	final int hash;
	final Leaf[] leaves;

	public MultiLeaf(int hash, Leaf... leaves) {
		this.hash = hash;
		this.leaves = leaves;
	}

	public INode assoc(int level, int hash, Object key, Object val){
		if(hash == this.hash)
			{
			int idx = findIndex(hash, key);
			if(idx != -1)
				return this;
			Leaf[] newLeaves = new Leaf[leaves.length + 1];
			for(int i=0;i<leaves.length;i++)
				newLeaves[i] = leaves[i];
			newLeaves[leaves.length]  = new Leaf(hash,key,val);
			return new MultiLeaf(hash, newLeaves);
			}
		return new Node(this,new Leaf(hash,key,val));
	}

	public INode without(int hash, Object key){
		int idx = findIndex(hash, key);
		if(idx == -1)
			return this;
		if(leaves.length == 2)
			return idx == 0?leaves[1]:leaves[0];
		Leaf[] newLeaves = new Leaf[leaves.length - 1];
		for(int i=0;i<idx;i++)
			newLeaves[i] = leaves[i];
		for(int i=idx+1;i<leaves.length;i++)
			newLeaves[i-1] = leaves[i];
		return new MultiLeaf(hash, newLeaves);
	}

	public Leaf find(int hash, Object key){
		int idx = findIndex(hash, key);
		if(idx != -1)
			return leaves[idx];
		return null;
	}

	int findIndex(int hash, Object key){
		for(int i=0;i<leaves.length;i++)
			{
			if(leaves[i].find(hash,key) != null)
				return i;
			}
		return -1;
	}

	public int getHash(){
		return hash;
	}
}

}

