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

import java.util.Comparator;

public class RBSet{

public final Comparator comp;
public final Node tree;
public final int count;

public RBSet(Comparator comp)
	{
	this.comp = comp;
	tree = null;
	count = 0;
	}

public boolean contains(Object x)
	{
	return treeContains(tree,x);
	}

public RBSet add(Object x)
	{
	Node t = treeInsert(tree,x);
	if(t == null)   //null == already contains x
		return this;
	return new RBSet(comp, t, count + 1);
	}

Node treeInsert(Node t, Object x)
	{
	if(t == null)
		return red(x, null, null);
	int c = comp.compare(t.key,x);
	if(c == 0)
		return null;
	Node ins = c < 0 ? treeInsert(t.left, x) : treeInsert(t.right, x);
	if(ins == null) //found below
		return null;
	if(t instanceof Black)
		{
		if(c < 0)
			return leftBalance(x, ins, t.right);
		return rightBalance(x, t.left, ins);
		}
	else //Red
		{
		if(c < 0)
			return red(x, ins, t.right);
		return red(x, t.left, ins);
		}
	}

Node leftBalance(Object x, Node left, Node right)
	{
	if(left instanceof Red && left.left instanceof Red)
		return red(left.key, blacken(left.left), black(x, left.right, right));
	else if(left instanceof Red && left.right instanceof Red)
		return red(left.right.key, black(left.key,left.left, left.right.left), black(x, left.right.right, right));
	else
		return black(x, left, right);
	}

Node rightBalance(Object x, Node left, Node right)
	{
	if(right instanceof Red && right.right instanceof Red)
		return red(right.key, black(x, left, right.left), blacken(right.right));
	else if(right instanceof Red && right.left instanceof Red)
		return red(right.right.key, black(x, left, right.left.left), black(right.key, right.left.right, right));
	else
		return black(x, left, right);
	}


Node blacken(Node n)
	{
	return black(n.key, n.left, n.right);
	}

boolean treeContains(Node t,Object x)
	{
	if(t == null)
		return false;
	int c = comp.compare(t.key,x);
	if(c == 0)
		return true;
	else if(c < 0)
		return treeContains(t.left, x);
	return treeContains(t.right, x);
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

static public class Node{
	public Object key;
	public Node left;
	public Node right;
	public Node(Object key,Node left,Node right)
		{
		this.key = key;
		this.left = left;
		this.right = right;
		}
}

static public class Red extends Node{
	public Red(Object key,Node left,Node right)
		{
		super(key, left, right);
		}

}

static public class Black extends Node{
	public Black(Object key,Node left,Node right)
		{
		super(key, left, right);
		}

}


}
