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

import java.util.*;

public class PersistentVector extends AFn implements IPersistentVector, Iterable{
final int cnt;
final int shift;
final Object[] root;
int _hash = -1;

public final static PersistentVector EMPTY = new PersistentVector(0, 0, RT.EMPTY_ARRAY);


static public PersistentVector create(ISeq items){
	//todo - consider building tree directly
	PersistentVector ret = EMPTY;
	for(; items != null; items = items.rest())
		ret = ret.cons(items.first());
	return ret;
}

static public PersistentVector create(List items){
	//todo - consider building tree directly
	PersistentVector ret = EMPTY;
	for(Object item : items)
		ret = ret.cons(item);
	return ret;
}

/**
 * This ctor may capture/alias the passed array, so do not modify later !
 */
static public PersistentVector create(Object... items){
	//todo - consider building tree directly
	if(items.length <= 32)
		return new PersistentVector(items.length, 0, items);
	return create(ArraySeq.create((Object[]) items));
}

PersistentVector(int cnt, int shift, Object[] root){
	this.cnt = cnt;
	this.shift = shift;
	this.root = root;
}


PersistentVector(IPersistentMap meta, int cnt, int shift, Object[] root){
	super(meta);
	this.cnt = cnt;
	this.shift = shift;
	this.root = root;
}

public int length(){
	return cnt;
}

public Object nth(int i){
	if(i >= 0 && i < cnt)
		{
		Object[] arr = root;
		for(int level = shift; level > 0; level -= 5)
			arr = (Object[]) arr[(i >>> level) & 0x01f];
		return arr[i & 0x01f];
		}
	throw new IndexOutOfBoundsException();
}

public PersistentVector assocN(int i, Object val){
	if(i >= 0 && i < cnt)
		return new PersistentVector(meta(), cnt, shift, doAssoc(shift, root, i, val));
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

public ISeq seq(){
	if(cnt > 0)
		return new Seq(this, 0);
	return null;
}

public ISeq rseq(){
	if(cnt > 0)
		return new RSeq(this, count() - 1);
	return null;
}

public boolean equals(Object obj){
	if(obj instanceof IPersistentVector)
		{
		IPersistentVector ma = (IPersistentVector) obj;
		if(ma.count() != count() || ma.hashCode() != hashCode())
			return false;
		for(int i = 0; i < count(); i++)
			{
			if(!RT.equal(nth(i), ma.nth(i)))
				return false;
			}
		}
	else
		{
		if(!(obj instanceof Sequential))
			return false;
		ISeq ms = ((IPersistentCollection) obj).seq();
		for(int i = 0; i < count(); i++, ms = ms.rest())
			{
			if(ms == null || !RT.equal(nth(i), ms.first()))
				return false;
			}
		if(ms.rest() != null)
			return false;
		}

	return true;
}

public int hashCode(){
	if(_hash == -1)
		{
		int hash = 0;
		for(int i = 0; i < cnt; i++)
			{
			hash = RT.hashCombine(hash, RT.hash(nth(i)));
			}
		this._hash = hash;
		}
	return _hash;
}

public PersistentVector withMeta(IPersistentMap meta){
	return new PersistentVector(meta, cnt, shift, root);
}

public Object invoke(Object arg1) throws Exception{
	return nth(((Number) arg1).intValue());
}

public Iterator iterator(){
	//todo - something more efficient
	return new Iterator(){
		int i = 0;

		public boolean hasNext(){
			return i < cnt;
		}

		public Object next(){
			return nth(i++);
		}

		public void remove(){
			throw new UnsupportedOperationException();
		}
	};
}

static class Seq extends ASeq implements IndexedSeq{
	//todo - something more efficient
	final PersistentVector v;
	final int i;


	public Seq(PersistentVector v, int i){
		this.v = v;
		this.i = i;
	}

	Seq(IPersistentMap meta, PersistentVector v, int i){
		super(meta);
		this.v = v;
		this.i = i;
	}

	public Object first(){
		return v.nth(i);
	}

	public ISeq rest(){
		if(i + 1 < v.cnt)
			return new Seq(v, i + 1);
		return null;
	}

	public int index(){
		return i;
	}

	public int count(){
		return v.cnt - i;
	}

	public Seq withMeta(IPersistentMap meta){
		return new Seq(meta, v, i);
	}
}

static class RSeq extends ASeq implements IndexedSeq{
	final PersistentVector v;
	final int i;

	RSeq(PersistentVector vector, int i){
		this.v = vector;
		this.i = i;
	}

	RSeq(IPersistentMap meta, PersistentVector v, int i){
		super(meta);
		this.v = v;
		this.i = i;
	}

	public Object first(){
		return v.nth(i);
	}

	public ISeq rest(){
		if(i > 0)
			return new RSeq(v, i - 1);
		return null;
	}

	public int index(){
		return i;
	}

	public int count(){
		return i + 1;
	}

	public RSeq withMeta(IPersistentMap meta){
		return new RSeq(meta, v, i);
	}
}

public PersistentVector cons(Object val){
	Box expansion = new Box(null);
	Object[] newroot = doCons(shift, root, val, expansion);
	int newshift = shift;
	if(expansion.val != null)
		{
		newroot = new Object[]{newroot, expansion.val};
		newshift += 5;
		}
	return new PersistentVector(meta(), cnt + 1, newshift, newroot);
}

private Object[] doCons(int level, Object[] arr, Object val, Box expansion){
	Object newchild;
	if(level == 0)
		{
		newchild = val;
		}
	else
		{
		newchild = doCons(level - 5, (Object[]) arr[arr.length - 1], val, expansion);
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

public Object peek(){
	if(cnt > 0)
		return nth(cnt - 1);
	return null;
}

public PersistentVector pop(){
	if(cnt == 0)
		throw new IllegalAccessError("Can't pop empty vector");
	Object[] newroot = doPop(shift, root);
	int newshift = shift;
	if(newroot == null)
		{
		return (PersistentVector) EMPTY.withMeta(meta());
		}
	if(newroot.length == 1)
		{
		newroot = (Object[]) newroot[0];
		newshift -= 5;
		}
	return new PersistentVector(meta(), cnt - 1, newshift, newroot);
}

private Object[] doPop(int shift, Object[] arr){
	if(shift > 0)
		{
		Object[] newchild = doPop(shift - 5, (Object[]) arr[arr.length - 1]);
		if(newchild != null)
			{
			Object[] ret = arr.clone();
			ret[arr.length - 1] = newchild;
			return ret;
			}
		}
	//contraction
	if(arr.length == 1)
		return null;
	Object[] ret = new Object[arr.length - 1];
	System.arraycopy(arr, 0, ret, 0, ret.length);
	return ret;
}

public boolean contains(Object key){
	if(!(key instanceof Number))
		return false;
	int i = ((Number) key).intValue();
	return i >= 0 && i < count();
}

public IMapEntry entryAt(Object key){
	if(key instanceof Number)
		{
		int i = ((Number) key).intValue();
		if(i >= 0 && i < count())
			return new MapEntry(key, nth(i));
		}
	return null;
}

public PersistentVector assoc(Object key, Object val){
	if(key instanceof Number)
		{
		int i = ((Number) key).intValue();
		return assocN(i, val);
		}
	throw new IllegalAccessError("Key must be integer");
}

public Object valAt(Object key){
	if(key instanceof Number)
		{
		int i = ((Number) key).intValue();
		if(i >= 0 && i < count())
			return nth(i);
		}
	return null;
}

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

}
