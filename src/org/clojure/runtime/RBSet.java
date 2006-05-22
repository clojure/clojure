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

public RBSet()
	{
	this(null);
	}

public RBSet(Comparator comp)
	{
	this.comp = comp;
	tree = null;
	count = 0;
	}

public boolean contains(Object key)
	{
	return find(key) != null;
	}

public RBSet add(Object key)
	{
	Node t = add(tree,key);
	if(t == null)   //null == already contains key
		return this;
	if(t instanceof Red)
		t = blacken(t);
	return new RBSet(comp, t, count + 1);
	}



public Iterator iterator()
	{
	return new NodeIterator(tree, true);
	}

public Iterator reverseIterator()
	{
	return new NodeIterator(tree, false);
	}

public Object min()
	{
	Node t = tree;
	if(t != null)
		{
		while(t.left != null)
			t = t.left;
		}
	return t != null ? t.key : null;
	}

public Object max()
	{
	Node t = tree;
	if(t != null)
		{
		while(t.right != null)
			t = t.right;
		}
	return t != null ? t.key : null;
	}

public int depth()
	{
	return depth(tree);
	}

int depth(Node t)
	{
	if(t == null)
		return 0;
	return 1 + Math.max(depth(t.left), depth(t.right));
	}

Node find(Object key)
	{
	Node t = tree;
	while(t != null)
		{
		int c = compare(key,t.key);
		if(c == 0)
			return t;
		else if(c < 0)
			t = t.left;
		else
			t = t.right;
		}
	return t;
	}

int compare(Object k1,Object k2)
	{
	if(comp != null)
		return comp.compare(k1, k2);
	return ((Comparable) k1).compareTo(k2);
	}

Node add(Node t, Object key)
	{
	if(t == null)
		return red(key, null, null);
	int c = compare(key,t.key);
	if(c == 0)
		return null;
	Node ins = c < 0 ? add(t.left, key) : add(t.right, key);
	if(ins == null) //found below
		return null;
	if(t instanceof Black)
		{
		if(c < 0)
			return leftBalance(t.key, ins, t.right);
		return rightBalance(t.key, t.left, ins);
		}
	else //Red
		{
		if(c < 0)
			return red(t.key, ins, t.right);
		return red(t.key, t.left, ins);
		}
	}

Node leftBalance(Object key, Node ins, Node right)
	{
	if(ins instanceof Red && ins.left instanceof Red)
		return red(ins.key, blacken(ins.left), black(key, ins.right, right));
	else if(ins instanceof Red && ins.right instanceof Red)
		return red(ins.right.key, black(ins.key,ins.left, ins.right.left), black(key, ins.right.right, right));
	else
		return black(key, ins, right);
	}

Node rightBalance(Object key, Node left, Node ins)
	{
	if(ins instanceof Red && ins.right instanceof Red)
		return red(ins.key, black(key, left, ins.left), blacken(ins.right));
	else if(ins instanceof Red && ins.left instanceof Red)
		return red(ins.left.key, black(key, left, ins.left.left), black(ins.key, ins.left.right, ins.right));
	else
		return black(key, left, ins);
	}

Node blacken(Node n)
	{
	return black(n.key, n.left, n.right);
	}

RBSet(Comparator comp, Node tree, int count)
	{
	this.comp = comp;
	this.tree = tree;
	this.count = count;
	}

static Red red(Object key,Node left,Node right)
	{
	return new Red(key, left, right);
	}

static Black black(Object key,Node left,Node right)
	{
	return new Black(key, left, right);
	}

static class Node{
	public final Object key;
	public final Node left;
	public final Node right;

	public Node(Object key,Node left,Node right)
		{
		this.key = key;
		this.left = left;
		this.right = right;
		}
}

static class Red extends Node{
	public Red(Object key,Node left,Node right)
		{
		super(key, left, right);
		}
}

static class Black extends Node{
	public Black(Object key,Node left,Node right)
		{
		super(key, left, right);
		}
}


static class NodeIterator implements Iterator{
	Stack stack = new Stack();
	boolean asc;

	NodeIterator(Node t,boolean asc)
		{
		this.asc = asc;
		push(t);
		}

	void push(Node t)
		{
		while(t != null)
			{
			stack.push(t);
			t = asc ? t.left:t.right;
			}
		}

	public boolean hasNext()
		{
		return !stack.isEmpty();
		}

	public Object next()
		{
		Node t = (Node) stack.pop();
		push(asc ? t.right:t.left);
		return t.key;
		}

	public void remove()
		{
		throw new UnsupportedOperationException();
		}
}

static public void main(String args[])
	{
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
	System.out.println("count = " + set.count + ", min: " + set.min() + ", max: " + set.max()
	                   + ", depth: " + set.depth());
	Iterator it = set.iterator();
	while(it.hasNext())
		{
		Object o =  it.next();
		if(!set.contains(o))
			System.err.println("Can't find: " + o);
		else
			System.out.print(o.toString()+",");
		}
	}
}
