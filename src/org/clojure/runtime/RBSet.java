/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich May 20, 2006 */

package org.clojure.runtime;

import java.util.*;

public class RBSet{

public final Comparator comp;
public final Node tree;
public final int count;

public RBSet(){
	this(null);
}

public RBSet(Comparator comp){
	this.comp = comp;
	tree = null;
	count = 0;
}

public boolean contains(Object key){
	return find(key) != null;
}

public RBSet add(Object key){
	return put(key, null);
}

public RBSet put(Object key,Object val){
	Box found = new Box(null);
	Node t = add(tree, key, val, found);
	if(t == null)   //null == already contains key
		{
		Node foundNode =  (Node) found.val;
		if(foundNode.val() == val)  //note only get same collection on identity of val, not equals()
			return this;
		return new RBSet(comp, replace(tree,key,val), count);
		}
	return new RBSet(comp, t.blacken(), count + 1);
}


/*
public RBSet remove(Object key){
	Node t = remove(tree, key);
	if(t == null)   //null == doesn't contain key
		return this;
	if(t instanceof Red)
		t = blacken(t);
	return new RBSet(comp, t, count - 1);
}
*/

public NodeIterator iterator(){
	return new NodeIterator(tree, true);
}

public NodeIterator reverseIterator(){
	return new NodeIterator(tree, false);
}

public Iterator keys(){
	return keys(iterator());
}

public Iterator vals(){
	return vals(iterator());
}

public Iterator keys(NodeIterator it){
	return new KeyIterator(it);
}

public Iterator vals(NodeIterator it){
	return new ValIterator(it);
}

public Object min(){
	Node t = tree;
	if(t != null)
		{
		while(t.left() != null)
			t = t.left();
		}
	return t != null ? t.key : null;
}

public Object max(){
	Node t = tree;
	if(t != null)
		{
		while(t.right() != null)
			t = t.right();
		}
	return t != null ? t.key : null;
}

public int depth(){
	return depth(tree);
}

int depth(Node t){
	if(t == null)
		return 0;
	return 1 + Math.max(depth(t.left()), depth(t.right()));
}

Node find(Object key){
	Node t = tree;
	while(t != null)
		{
		int c = compare(key, t.key);
		if(c == 0)
			return t;
		else if(c < 0)
			t = t.left();
		else
			t = t.right();
		}
	return t;
}

int compare(Object k1, Object k2){
	if(comp != null)
		return comp.compare(k1, k2);
	return ((Comparable) k1).compareTo(k2);
}

Node add(Node t, Object key, Object val, Box found){
	if(t == null)
		{
		if(val == null)
			return new Red(key);
		return new RedVal(key,val);
		}
	int c = compare(key, t.key);
	if(c == 0)
		{
		found.val = t;
		return null;
		}
	Node ins = c < 0 ? add(t.left(), key, val, found) : add(t.right(), key, val, found);
	if(ins == null) //found below
		return null;
	if(c < 0)
		return t.addLeft(ins);
	return t.addRight(ins);
}

Node replace(Node t, Object key, Object val){
	int c = compare(key, t.key);
	return t.replace(t.key,
	                 c==0?val:t.val(),
	                 c<0?replace(t.left(),key,val):t.left(),
	                 c>0?replace(t.right(),key,val):t.right());
}

RBSet(Comparator comp, Node tree, int count){
	this.comp = comp;
	this.tree = tree;
	this.count = count;
}

static Red red(Object key, Object val, Node left, Node right){
	if(left == null && right == null)
		{
		if(val == null)
			return new Red(key);
		return new RedVal(key, val);
		}
	if(val == null)
		return new RedBranch(key, left, right);
	return new RedBranchVal(key, val, left, right);
}

static Black black(Object key, Object val, Node left, Node right){
	if(left == null && right == null)
		{
		if(val == null)
			return new Black(key);
		return new BlackVal(key, val);
		}
	if(val == null)
		return new BlackBranch(key, left, right);
	return new BlackBranchVal(key, val, left, right);
}


static abstract class Node{
	final Object key;

	Node(Object key){
		this.key = key;
	}

	public Object key(){
		return key;
	}

	public Object val(){
		return null;
	}

	Node left(){
		return null;
	}

	Node right(){
		return null;
	}

	abstract Node addLeft(Node ins);

	abstract Node addRight(Node ins);

	abstract Node blacken();

	Node balanceLeft(Node parent){
		return black(parent.key, parent.val(), this, parent.right());
	}

	Node balanceRight(Node parent){
		return black(parent.key, parent.val(), parent.left(), this);
	}

	abstract Node replace(Object key, Object val, Node left, Node right);
}

static class Black extends Node{
	public Black(Object key){
		super(key);
	}

	Node addLeft(Node ins){
		return ins.balanceLeft(this);
	}

	Node addRight(Node ins){
		return ins.balanceRight(this);
	}

	Node blacken(){
		return this;
	}

	Node replace(Object key, Object val, Node left, Node right){
		return black(key, val, left, right);
	}
}

static class BlackVal extends Black{
	final Object val;
	public BlackVal(Object key,Object val){
		super(key);
		this.val = val;
	}

	public Object val(){
		return val;
	}
}

static class BlackBranch extends Black{
	final Node left;
	final Node right;
	public BlackBranch(Object key,Node left,Node right){
		super(key);
		this.left = left;
		this.right = right;
		}

	public Node left(){
		return left;
	}

	public Node right(){
		return right;
	}
}

static class BlackBranchVal extends BlackBranch{
	final Object val;
	public BlackBranchVal(Object key,Object val,Node left,Node right){
		super(key, left, right);
		this.val = val;
	}
	public Object val(){
		return val;
	}
}

static class Red extends Node{
	public Red(Object key){
		super(key);
	}

	Node addLeft(Node ins){
		return red(key, val(), ins, right());
	}

	Node addRight(Node ins){
		return red(key, val(), left(), ins);
	}

	Node blacken(){
		return new Black(key);
	}

	Node replace(Object key, Object val, Node left, Node right){
		return red(key, val, left, right);
	}
}

static class RedVal extends Red{
	final Object val;
	public RedVal(Object key,Object val){
		super(key);
		this.val = val;
	}

	public Object val(){
		return val;
	}

	Node blacken(){
		return new BlackVal(key, val);
	}
}

static class RedBranch extends Red{
	final Node left;
	final Node right;
	public RedBranch(Object key,Node left,Node right){
		super(key);
		this.left = left;
		this.right = right;
		}

	public Node left(){
		return left;
	}

	public Node right(){
		return right;
	}

	Node balanceLeft(Node parent){
		if(left instanceof Red)
			return red(key, val(), left.blacken(), black(parent.key, parent.val(), right, parent.right()));
		else if(right instanceof Red)
			return red(right.key, right.val(),black(key, val(), left, right.left()),
			           black(parent.key, parent.val(), right.right(), parent.right()));
		else
			return super.balanceLeft(parent);

	}

	Node balanceRight(Node parent){
		if(right instanceof Red)
			return red(key, val(), black(parent.key, parent.val(), parent.left(), left), right.blacken());
		else if(left instanceof Red)
			return red(left.key, left.val(), black(parent.key, parent.val(), parent.left(), left.left()),
			           black(key, val(), left.right(), right));
		else
			return super.balanceRight(parent);
	}

	Node blacken(){
		return new BlackBranch(key, left, right);
	}
}

static class RedBranchVal extends RedBranch{
	final Object val;
	public RedBranchVal(Object key,Object val,Node left,Node right){
		super(key, left, right);
		this.val = val;
	}
	public Object val(){
		return val;
	}

	Node blacken(){
		return new BlackBranchVal(key, val, left, right);
	}
}

static public class NodeIterator implements Iterator{
	Stack stack = new Stack();
	boolean asc;

	NodeIterator(Node t, boolean asc){
		this.asc = asc;
		push(t);
	}

	void push(Node t){
		while(t != null)
			{
			stack.push(t);
			t = asc ? t.left() : t.right();
			}
	}

	public boolean hasNext(){
		return !stack.isEmpty();
	}

	public Object next(){
		Node t = (Node) stack.pop();
		push(asc ? t.right() : t.left());
		return t;
	}

	public void remove(){
		throw new UnsupportedOperationException();
	}
}

static class KeyIterator implements Iterator{
	NodeIterator it;
	KeyIterator(NodeIterator it){
		this.it = it;
	}

	public boolean hasNext(){
		return it.hasNext();
	}

	public Object next(){
		return ((Node)it.next()).key;
	}

	public void remove(){
		throw new UnsupportedOperationException();
	}
}

static class ValIterator implements Iterator{
	NodeIterator it;
	ValIterator(NodeIterator it){
		this.it = it;
	}

	public boolean hasNext(){
		return it.hasNext();
	}

	public Object next(){
		return ((Node)it.next()).val();
	}

	public void remove(){
		throw new UnsupportedOperationException();
	}
}

static public void main(String args[]){
	if(args.length != 1)
		System.err.println("Usage: RBSet n");
	int n = Integer.parseInt(args[0]);
	Integer[] ints = new Integer[n];
	for(int i = 0; i < ints.length; i++)
		{
		ints[i] = new Integer(i);
		}
	Collections.shuffle(Arrays.asList(ints));
	System.out.println("Building set");
	RBSet set = new RBSet();
	for(int i = 0; i < ints.length; i++)
		{
		Integer anInt = ints[i];
		set = set.add(anInt);
		}
	for(int i = 0; i < ints.length; i++)
		{
		Integer anInt = ints[i];
		set = set.put(anInt,anInt);
		}
	System.out.println("count = " + set.count + ", min: " + set.min() + ", max: " + set.max()
	                   + ", depth: " + set.depth());
	Iterator it = set.keys();
	while(it.hasNext())
		{
		Object o = it.next();
		if(!set.contains(o))
			System.err.println("Can't find: " + o);
		else if(n < 2000)
			System.out.print(o.toString() + ",");
		}
}
}
