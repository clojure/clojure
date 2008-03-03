/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

package clojure.lang;

import java.util.Collection;

public abstract class APersistentMap extends AFn implements IPersistentMap, Collection{
int _hash = -1;


protected APersistentMap(IPersistentMap meta){
	super(meta);
}


protected APersistentMap(){
}

public String toString(){
	return "<map: - " + count() + " items>";
}

public IPersistentCollection cons(Object o){
	if(o instanceof IPersistentVector)
		{
		IPersistentVector v = (IPersistentVector) o;
		if(v.count() != 2)
			throw new IllegalArgumentException("Vector arg to map conj must be a pair");
		return assoc(v.nth(0), v.nth(1));
		}

	IPersistentMap ret = this;
	for(ISeq es = RT.seq(o); es != null; es = es.rest())
		{
		IMapEntry e = (IMapEntry) es.first();
		ret = ret.assoc(e.key(), e.val());
		}
	return ret;
}

public boolean equals(Object obj){
	if(!(obj instanceof IPersistentMap))
		return false;
	IPersistentMap m = (IPersistentMap) obj;

	if(m.count() != count() || m.hashCode() != hashCode())
		return false;

	for(ISeq s = seq(); s != null; s = s.rest())
		{
		IMapEntry e = (IMapEntry) s.first();
		IMapEntry me = m.entryAt(e.key());

		if(me == null || !RT.equal(e.val(), me.val()))
			return false;
		}

	return true;
}

public int hashCode(){
	if(_hash == -1)
		{
		int hash = count();
		for(ISeq s = seq(); s != null; s = s.rest())
			{
			IMapEntry e = (IMapEntry) s.first();
			hash ^= RT.hashCombine(RT.hash(e.key()), RT.hash(e.val()));
			}
		this._hash = hash;
		}
	return _hash;
}

static public class KeySeq extends ASeq{
	ISeq seq;

	static public KeySeq create(ISeq seq){
		if(seq == null)
			return null;
		return new KeySeq(seq);
	}

	private KeySeq(ISeq seq){
		this.seq = seq;
	}

	private KeySeq(IPersistentMap meta, ISeq seq){
		super(meta);
		this.seq = seq;
	}

	public Object first(){
		return ((IMapEntry) seq.first()).key();
	}

	public ISeq rest(){
		return create(seq.rest());
	}

	public KeySeq withMeta(IPersistentMap meta){
		return new KeySeq(meta, seq);
	}
}

static public class ValSeq extends ASeq{
	ISeq seq;

	static public ValSeq create(ISeq seq){
		if(seq == null)
			return null;
		return new ValSeq(seq);
	}

	private ValSeq(ISeq seq){
		this.seq = seq;
	}

	private ValSeq(IPersistentMap meta, ISeq seq){
		super(meta);
		this.seq = seq;
	}

	public Object first(){
		return ((IMapEntry) seq.first()).val();
	}

	public ISeq rest(){
		return create(seq.rest());
	}

	public ValSeq withMeta(IPersistentMap meta){
		return new ValSeq(meta, seq);
	}
}


public Object invoke(Object arg1) throws Exception{
	return valAt(arg1);
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
		for(int i = 0; s != null; ++i, s = s.rest())
			{
			a[i] = s.first();
			}
		if(a.length >= count())
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
	if(o instanceof IMapEntry)
		{
		IMapEntry e = (IMapEntry) o;
		IMapEntry v = entryAt(e.key());
		return (v != null && RT.equal(v.val(), e.val()));
		}
	return false;
}

}
