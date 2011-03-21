/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Dec 18, 2007 */

package clojure.lang;

import java.io.Serializable;
import java.util.*;

public abstract class APersistentVector extends AFn implements IPersistentVector, Iterable,
                                                               List,
                                                               RandomAccess, Comparable,
                                                               Serializable {
int _hash = -1;

public String toString(){
	return RT.printString(this);
}

public ISeq seq(){
	if(count() > 0)
		return new Seq(this, 0);
	return null;
}

public ISeq rseq(){
	if(count() > 0)
		return new RSeq(this, count() - 1);
	return null;
}

static boolean doEquals(IPersistentVector v, Object obj){
	if(v == obj) return true;
	if(obj instanceof List || obj instanceof IPersistentVector)
		{
		Collection ma = (Collection) obj;
		if(ma.size() != v.count() || ma.hashCode() != v.hashCode())
			return false;
		for(Iterator i1 = ((List) v).iterator(), i2 = ma.iterator();
		    i1.hasNext();)
			{
			if(!Util.equals(i1.next(), i2.next()))
				return false;
			}
		return true;
		}
//	if(obj instanceof IPersistentVector)
//		{
//		IPersistentVector ma = (IPersistentVector) obj;
//		if(ma.count() != v.count() || ma.hashCode() != v.hashCode())
//			return false;
//		for(int i = 0; i < v.count(); i++)
//			{
//			if(!Util.equal(v.nth(i), ma.nth(i)))
//				return false;
//			}
//		}
	else
		{
		if(!(obj instanceof Sequential))
			return false;
		ISeq ms = RT.seq(obj);
		for(int i = 0; i < v.count(); i++, ms = ms.next())
			{
			if(ms == null || !Util.equals(v.nth(i), ms.first()))
				return false;
			}
		if(ms != null)
			return false;
		}

	return true;

}

static boolean doEquiv(IPersistentVector v, Object obj){
	if(obj instanceof List || obj instanceof IPersistentVector)
		{
		Collection ma = (Collection) obj;
		if(ma.size() != v.count())
			return false;
		for(Iterator i1 = ((List) v).iterator(), i2 = ma.iterator();
		    i1.hasNext();)
			{
			if(!Util.equiv(i1.next(), i2.next()))
				return false;
			}
		return true;
		}
//	if(obj instanceof IPersistentVector)
//		{
//		IPersistentVector ma = (IPersistentVector) obj;
//		if(ma.count() != v.count() || ma.hashCode() != v.hashCode())
//			return false;
//		for(int i = 0; i < v.count(); i++)
//			{
//			if(!Util.equal(v.nth(i), ma.nth(i)))
//				return false;
//			}
//		}
	else
		{
		if(!(obj instanceof Sequential))
			return false;
		ISeq ms = RT.seq(obj);
		for(int i = 0; i < v.count(); i++, ms = ms.next())
			{
			if(ms == null || !Util.equiv(v.nth(i), ms.first()))
				return false;
			}
		if(ms != null)
			return false;
		}

	return true;

}

public boolean equals(Object obj){
	return doEquals(this, obj);
}

public boolean equiv(Object obj){
	return doEquiv(this, obj);
}

public int hashCode(){
	if(_hash == -1)
		{
		int hash = 1;
		Iterator i = iterator();
		while(i.hasNext())
			{
			Object obj = i.next();
			hash = 31 * hash + (obj == null ? 0 : obj.hashCode());
			}
//		int hash = 0;
//		for(int i = 0; i < count(); i++)
//			{
//			hash = Util.hashCombine(hash, Util.hash(nth(i)));
//			}
		this._hash = hash;
		}
	return _hash;
}

public Object get(int index){
	return nth(index);
}

public Object nth(int i, Object notFound){
	if(i >= 0 && i < count())
		return nth(i);
	return notFound;
}

public Object remove(int i){
	throw new UnsupportedOperationException();
}

public int indexOf(Object o){
	for(int i = 0; i < count(); i++)
		if(Util.equiv(nth(i), o))
			return i;
	return -1;
}

public int lastIndexOf(Object o){
	for(int i = count() - 1; i >= 0; i--)
		if(Util.equiv(nth(i), o))
			return i;
	return -1;
}

public ListIterator listIterator(){
	return listIterator(0);
}

public ListIterator listIterator(final int index){
	return new ListIterator(){
		int nexti = index;

		public boolean hasNext(){
			return nexti < count();
		}

		public Object next(){
			return nth(nexti++);
		}

		public boolean hasPrevious(){
			return nexti > 0;
		}

		public Object previous(){
			return nth(--nexti);
		}

		public int nextIndex(){
			return nexti;
		}

		public int previousIndex(){
			return nexti - 1;
		}

		public void remove(){
			throw new UnsupportedOperationException();
		}

		public void set(Object o){
			throw new UnsupportedOperationException();
		}

		public void add(Object o){
			throw new UnsupportedOperationException();
		}
	};
}

public List subList(int fromIndex, int toIndex){
	return (List) RT.subvec(this, fromIndex, toIndex);
}


public Object set(int i, Object o){
	throw new UnsupportedOperationException();
}

public void add(int i, Object o){
	throw new UnsupportedOperationException();
}

public boolean addAll(int i, Collection c){
	throw new UnsupportedOperationException();
}


public Object invoke(Object arg1) {
	if(Util.isInteger(arg1))
		return nth(((Number) arg1).intValue());
	throw new IllegalArgumentException("Key must be integer");
}

public Iterator iterator(){
	//todo - something more efficient
	return new Iterator(){
		int i = 0;

		public boolean hasNext(){
			return i < count();
		}

		public Object next(){
			return nth(i++);
		}

		public void remove(){
			throw new UnsupportedOperationException();
		}
	};
}

public Object peek(){
	if(count() > 0)
		return nth(count() - 1);
	return null;
}

public boolean containsKey(Object key){
	if(!(Util.isInteger(key)))
		return false;
	int i = ((Number) key).intValue();
	return i >= 0 && i < count();
}

public IMapEntry entryAt(Object key){
	if(Util.isInteger(key))
		{
		int i = ((Number) key).intValue();
		if(i >= 0 && i < count())
			return new MapEntry(key, nth(i));
		}
	return null;
}

public IPersistentVector assoc(Object key, Object val){
	if(Util.isInteger(key))
		{
		int i = ((Number) key).intValue();
		return assocN(i, val);
		}
	throw new IllegalArgumentException("Key must be integer");
}

public Object valAt(Object key, Object notFound){
	if(Util.isInteger(key))
		{
		int i = ((Number) key).intValue();
		if(i >= 0 && i < count())
			return nth(i);
		}
	return notFound;
}

public Object valAt(Object key){
	return valAt(key, null);
}

// java.util.Collection implementation

public Object[] toArray(){
	return RT.seqToArray(seq());
}

public boolean add(Object o){
	throw new UnsupportedOperationException();
}

public boolean remove(Object o){
	throw new UnsupportedOperationException();
}

public boolean addAll(Collection c){
	throw new UnsupportedOperationException();
}

public void clear(){
	throw new UnsupportedOperationException();
}

public boolean retainAll(Collection c){
	throw new UnsupportedOperationException();
}

public boolean removeAll(Collection c){
	throw new UnsupportedOperationException();
}

public boolean containsAll(Collection c){
	for(Object o : c)
		{
		if(!contains(o))
			return false;
		}
	return true;
}

public Object[] toArray(Object[] a){
	if(a.length >= count())
		{
		ISeq s = seq();
		for(int i = 0; s != null; ++i, s = s.next())
			{
			a[i] = s.first();
			}
		if(a.length > count())
			a[count()] = null;
		return a;
		}
	else
		return toArray();
}

public int size(){
	return count();
}

public boolean isEmpty(){
	return count() == 0;
}

public boolean contains(Object o){
	for(ISeq s = seq(); s != null; s = s.next())
		{
		if(Util.equiv(s.first(), o))
			return true;
		}
	return false;
}

public int length(){
	return count();
}

public int compareTo(Object o){
	IPersistentVector v = (IPersistentVector) o;
	if(count() < v.count())
		return -1;
	else if(count() > v.count())
		return 1;
	for(int i = 0; i < count(); i++)
		{
		int c = Util.compare(nth(i),v.nth(i));
		if(c != 0)
			return c;
		}
	return 0;
}

    static class Seq extends ASeq implements IndexedSeq, IReduce{
	//todo - something more efficient
	final IPersistentVector v;
	final int i;


	public Seq(IPersistentVector v, int i){
		this.v = v;
		this.i = i;
	}

	Seq(IPersistentMap meta, IPersistentVector v, int i){
		super(meta);
		this.v = v;
		this.i = i;
	}

	public Object first(){
		return v.nth(i);
	}

	public ISeq next(){
		if(i + 1 < v.count())
			return new APersistentVector.Seq(v, i + 1);
		return null;
	}

	public int index(){
		return i;
	}

	public int count(){
		return v.count() - i;
	}

	public APersistentVector.Seq withMeta(IPersistentMap meta){
		return new APersistentVector.Seq(meta, v, i);
	}

	public Object reduce(IFn f) {
		Object ret = v.nth(i);
		for(int x = i + 1; x < v.count(); x++)
			ret = f.invoke(ret, v.nth(x));
		return ret;
	}

	public Object reduce(IFn f, Object start) {
		Object ret = f.invoke(start, v.nth(i));
		for(int x = i + 1; x < v.count(); x++)
			ret = f.invoke(ret, v.nth(x));
		return ret;
	}
    }

public static class RSeq extends ASeq implements IndexedSeq, Counted{
	final IPersistentVector v;
	final int i;

	public RSeq(IPersistentVector vector, int i){
		this.v = vector;
		this.i = i;
	}

	RSeq(IPersistentMap meta, IPersistentVector v, int i){
		super(meta);
		this.v = v;
		this.i = i;
	}

	public Object first(){
		return v.nth(i);
	}

	public ISeq next(){
		if(i > 0)
			return new APersistentVector.RSeq(v, i - 1);
		return null;
	}

	public int index(){
		return i;
	}

	public int count(){
		return i + 1;
	}

	public APersistentVector.RSeq withMeta(IPersistentMap meta){
		return new APersistentVector.RSeq(meta, v, i);
	}
}

static class SubVector extends APersistentVector implements IObj{
	final IPersistentVector v;
	final int start;
	final int end;
	final IPersistentMap _meta;



	public SubVector(IPersistentMap meta, IPersistentVector v, int start, int end){
		this._meta = meta;

		if(v instanceof APersistentVector.SubVector)
			{
			APersistentVector.SubVector sv = (APersistentVector.SubVector) v;
			start += sv.start;
			end += sv.start;
			v = sv.v;
			}
		this.v = v;
		this.start = start;
		this.end = end;
	}

	public Object nth(int i){
		if(start + i >= end)
			throw new IndexOutOfBoundsException();
		return v.nth(start + i);
	}

	public IPersistentVector assocN(int i, Object val){
		if(start + i > end)
			throw new IndexOutOfBoundsException();
		else if(start + i == end)
			return cons(val);
		return new SubVector(_meta, v.assocN(start + i, val), start, end);
	}

	public int count(){
		return end - start;
	}

	public IPersistentVector cons(Object o){
		return new SubVector(_meta, v.assocN(end, o), start, end + 1);
	}

	public IPersistentCollection empty(){
		return PersistentVector.EMPTY.withMeta(meta());
	}

	public IPersistentStack pop(){
		if(end - 1 == start)
			{
			return PersistentVector.EMPTY;
			}
		return new SubVector(_meta, v, start, end - 1);
	}

	public SubVector withMeta(IPersistentMap meta){
		if(meta == _meta)
			return this;
		return new SubVector(meta, v, start, end);
	}

	public IPersistentMap meta(){
		return _meta;
	}
}
}
