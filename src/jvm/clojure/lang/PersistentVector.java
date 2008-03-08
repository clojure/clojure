/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Jul 5, 2007 */

package clojure.lang;

import java.util.List;

public class PersistentVector extends APersistentVector{
final int cnt;
final int shift;
final Object[] root;
final Object[] tail;

public final static PersistentVector EMPTY = new PersistentVector(0, 5, RT.EMPTY_ARRAY, RT.EMPTY_ARRAY);


static public PersistentVector create(ISeq items){
	PersistentVector ret = EMPTY;
	for(; items != null; items = items.rest())
		ret = ret.cons(items.first());
	return ret;
}

static public PersistentVector create(List items){
	PersistentVector ret = EMPTY;
	for(Object item : items)
		ret = ret.cons(item);
	return ret;
}

static public PersistentVector create(Object... items){
	PersistentVector ret = EMPTY;
	for(Object item : items)
		ret = ret.cons(item);
	return ret;
}

PersistentVector(int cnt, int shift, Object[] root, Object[] tail){
	super(null);
	this.cnt = cnt;
	this.shift = shift;
	this.root = root;
	this.tail = tail;
}


PersistentVector(IPersistentMap meta, int cnt, int shift, Object[] root, Object[] tail){
	super(meta);
	this.cnt = cnt;
	this.shift = shift;
	this.root = root;
	this.tail = tail;
}

final int tailoff(){
	return cnt - tail.length;
}

public Object nth(int i){
	if(i >= 0 && i < cnt)
		{
		if(i >= tailoff())
			return tail[i & 0x01f];
		Object[] arr = root;
		for(int level = shift; level > 0; level -= 5)
			arr = (Object[]) arr[(i >>> level) & 0x01f];
		return arr[i & 0x01f];
		}
	throw new IndexOutOfBoundsException();
}

public PersistentVector assocN(int i, Object val){
	if(i >= 0 && i < cnt)
		{
		if(i >= tailoff())
			{
			Object[] newTail = new Object[tail.length];
			System.arraycopy(tail, 0, newTail, 0, tail.length);
			newTail[i & 0x01f] = val;

			return new PersistentVector(meta(), cnt, shift, root, newTail);
			}

		return new PersistentVector(meta(), cnt, shift, doAssoc(shift, root, i, val), tail);
		}
	if(i == cnt)
		return cons(val);
	throw new IndexOutOfBoundsException();
}

private static Object[] doAssoc(int level, Object[] arr, int i, Object val){
	Object[] ret = arr.clone();
	if(level == 0)
		{
		ret[i & 0x01f] = val;
		}
	else
		{
		int subidx = (i >>> level) & 0x01f;
		ret[subidx] = doAssoc(level - 5, (Object[]) arr[subidx], i, val);
		}
	return ret;
}

public int count(){
	return cnt;
}

public PersistentVector withMeta(IPersistentMap meta){
	return new PersistentVector(meta, cnt, shift, root, tail);
}


public PersistentVector cons(Object val){
	if(tail.length < 32)
		{
		Object[] newTail = new Object[tail.length + 1];
		System.arraycopy(tail, 0, newTail, 0, tail.length);
		newTail[tail.length] = val;
		return new PersistentVector(meta(), cnt + 1, shift, root, newTail);
		}
	Box expansion = new Box(null);
	Object[] newroot = pushTail(shift - 5, root, tail, expansion);
	int newshift = shift;
	if(expansion.val != null)
		{
		newroot = new Object[]{newroot, expansion.val};
		newshift += 5;
		}
	return new PersistentVector(meta(), cnt + 1, newshift, newroot, new Object[]{val});
}

private Object[] pushTail(int level, Object[] arr, Object[] tailNode, Box expansion){
	Object newchild;
	if(level == 0)
		{
		newchild = tailNode;
		}
	else
		{
		newchild = pushTail(level - 5, (Object[]) arr[arr.length - 1], tailNode, expansion);
		if(expansion.val == null)
			{
			Object[] ret = arr.clone();
			ret[arr.length - 1] = newchild;
			return ret;
			}
		else
			newchild = expansion.val;
		}
	//expansion
	if(arr.length == 32)
		{
		expansion.val = new Object[]{newchild};
		return arr;
		}
	Object[] ret = new Object[arr.length + 1];
	System.arraycopy(arr, 0, ret, 0, arr.length);
	ret[arr.length] = newchild;
	expansion.val = null;
	return ret;
}

public PersistentVector pop(){
	if(cnt == 0)
		throw new IllegalAccessError("Can't pop empty vector");
	if(cnt == 1)
		return EMPTY.withMeta(meta());
	if(tail.length > 1)
		{
		Object[] newTail = new Object[tail.length - 1];
		System.arraycopy(tail, 0, newTail, 0, newTail.length);
		return new PersistentVector(meta(), cnt - 1, shift, root, newTail);
		}
	Box ptail = new Box(null);
	Object[] newroot = popTail(shift - 5, root, ptail);
	int newshift = shift;
	if(newroot == null)
		{
		newroot = RT.EMPTY_ARRAY;
		}
	if(shift > 5 && newroot.length == 1)
		{
		newroot = (Object[]) newroot[0];
		newshift -= 5;
		}
	return new PersistentVector(meta(), cnt - 1, newshift, newroot, (Object[]) ptail.val);
}

private Object[] popTail(int shift, Object[] arr, Box ptail){
	if(shift > 0)
		{
		Object[] newchild = popTail(shift - 5, (Object[]) arr[arr.length - 1], ptail);
		if(newchild != null)
			{
			Object[] ret = arr.clone();
			ret[arr.length - 1] = newchild;
			return ret;
			}
		}
	if(shift == 0)
		ptail.val = arr[arr.length - 1];
	//contraction
	if(arr.length == 1)
		return null;
	Object[] ret = new Object[arr.length - 1];
	System.arraycopy(arr, 0, ret, 0, ret.length);
	return ret;
}

/*
static public void main(String[] args){
	if(args.length != 3)
		{
		System.err.println("Usage: PersistentVector size writes reads");
		return;
		}
	int size = Integer.parseInt(args[0]);
	int writes = Integer.parseInt(args[1]);
	int reads = Integer.parseInt(args[2]);
	Vector v = new Vector(size);
	v.setSize(size);
	//PersistentArray p = new PersistentArray(size);
	PersistentVector p = PersistentVector.EMPTY;

	for(int i = 0; i < size; i++)
		{
		v.set(i, i);
		//p = p.set(i, 0);
		p = p.cons(i);
		}

	Random rand;

	rand = new Random(42);
	long tv = 0;
	System.out.println("Vector");
	long startTime = System.nanoTime();
	for(int i = 0; i < writes; i++)
		{
		v.set(rand.nextInt(size), i);
		}
	for(int i = 0; i < reads; i++)
		{
		tv += (Integer) v.get(rand.nextInt(size));
		}
	long estimatedTime = System.nanoTime() - startTime;
	System.out.println("time: " + estimatedTime / 1000000);
	System.out.println("PersistentVector");
	rand = new Random(42);
	startTime = System.nanoTime();
	long tp = 0;

//	PersistentVector oldp = p;
	//Random rand2 = new Random(42);

	for(int i = 0; i < writes; i++)
		{
		p = p.assocN(rand.nextInt(size), i);
		//dummy set to force perverse branching
		//oldp =	oldp.assocN(rand2.nextInt(size), i);
		}
	for(int i = 0; i < reads; i++)
		{
		tp += (Integer) p.nth(rand.nextInt(size));
		}
	estimatedTime = System.nanoTime() - startTime;
	System.out.println("time: " + estimatedTime / 1000000);
	for(int i = 0; i < size / 2; i++)
		{
		p = p.pop();
		v.remove(v.size() - 1);
		}
	for(int i = 0; i < size / 2; i++)
		{
		tp += (Integer) p.nth(i);
		tv += (Integer) v.get(i);
		}
	System.out.println("Done: " + tv + ", " + tp);

}
  */
}
