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
import java.util.*;

public abstract class APersistentMap extends AFn implements IPersistentMap, Map, Iterable, Serializable, MapEquivalence, IHashEq {
int _hash;
int _hasheq;

public String toString(){
	return RT.printString(this);
}

public IPersistentCollection cons(Object o){
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

	IPersistentMap ret = this;
	for(ISeq es = RT.seq(o); es != null; es = es.next())
		{
		Map.Entry e = (Map.Entry) es.first();
		ret = ret.assoc(e.getKey(), e.getValue());
		}
	return ret;
}

public boolean equals(Object obj){
	return mapEquals(this, obj);
}

static public boolean mapEquals(IPersistentMap m1, Object obj){
	if(m1 == obj) return true;
	if(!(obj instanceof Map))
		return false;
	Map m = (Map) obj;

	if(m.size() != m1.count())
		return false;

	for(ISeq s = m1.seq(); s != null; s = s.next())
		{
		Map.Entry e = (Map.Entry) s.first();
		boolean found = m.containsKey(e.getKey());

		if(!found || !Util.equals(e.getValue(), m.get(e.getKey())))
			return false;
		}

	return true;
}

public boolean equiv(Object obj){
	if(!(obj instanceof Map))
		return false;
	if(obj instanceof IPersistentMap && !(obj instanceof MapEquivalence))
		return false;
	
	Map m = (Map) obj;

	if(m.size() != size())
		return false;

	for(ISeq s = seq(); s != null; s = s.next())
		{
		Map.Entry e = (Map.Entry) s.first();
		boolean found = m.containsKey(e.getKey());

		if(!found || !Util.equiv(e.getValue(), m.get(e.getKey())))
			return false;
		}

	return true;
}
public int hashCode(){
    int cached = this._hash;
	if(cached == 0)
		{
		this._hash = cached = mapHash(this);
		}
	return cached;
}

static public int mapHash(IPersistentMap m){
	int hash = 0;
	for(ISeq s = m.seq(); s != null; s = s.next())
		{
		Map.Entry e = (Map.Entry) s.first();
		hash += (e.getKey() == null ? 0 : e.getKey().hashCode()) ^
				(e.getValue() == null ? 0 : e.getValue().hashCode());
		}
	return hash;
}

public int hasheq(){
    int cached = this._hasheq;
	if(cached == 0)
		{
		//this._hasheq = mapHasheq(this);
		this._hasheq = cached = Murmur3.hashUnordered(this);
		}
	return cached;
}

static public int mapHasheq(IPersistentMap m) {
	return Murmur3.hashUnordered(m);
//	int hash = 0;
//	for(ISeq s = m.seq(); s != null; s = s.next())
//		{
//		Map.Entry e = (Map.Entry) s.first();
//		hash += Util.hasheq(e.getKey()) ^
//				Util.hasheq(e.getValue());
//		}
//	return hash;
}

static public class KeySeq extends ASeq{
	final ISeq seq;
	final Iterable iterable;

	static public KeySeq create(ISeq seq){
		if(seq == null)
			return null;
		return new KeySeq(seq, null);
	}

	static public KeySeq createFromMap(IPersistentMap map){
		if(map == null)
			return null;
		ISeq seq = map.seq();
		if(seq == null)
			return null;
		return new KeySeq(seq, map);
	}

	private KeySeq(ISeq seq, Iterable iterable){
		this.seq = seq;
		this.iterable = iterable;
	}

	private KeySeq(IPersistentMap meta, ISeq seq, Iterable iterable){
		super(meta);
		this.seq = seq;
		this.iterable = iterable;
	}

	public Object first(){
		return ((Map.Entry) seq.first()).getKey();
	}

	public ISeq next(){
		return create(seq.next());
	}

	public KeySeq withMeta(IPersistentMap meta){
		if(meta() == meta)
			return this;
		return new KeySeq(meta, seq, iterable);
	}

	public Iterator iterator(){
		if(iterable == null)
			return super.iterator();

		if(iterable instanceof IMapIterable)
			return ((IMapIterable)iterable).keyIterator();

		final Iterator mapIter = iterable.iterator();
		return new Iterator() {
			public boolean hasNext() {
				return mapIter.hasNext();
			}

			public Object next() {
				return ((Map.Entry)mapIter.next()).getKey();
			}

			public void remove() {
				throw new UnsupportedOperationException();
			}
		};
	}
}

static public class ValSeq extends ASeq{
	final ISeq seq;
	final Iterable iterable;

	static public ValSeq create(ISeq seq){
		if(seq == null)
			return null;
		return new ValSeq(seq, null);
	}

	static public ValSeq createFromMap(IPersistentMap map) {
		if(map == null)
			return null;
		ISeq seq = map.seq();
		if(seq == null)
			return null;
		return new ValSeq(seq, map);
	}

	private ValSeq(ISeq seq, Iterable iterable){
		this.seq = seq;
		this.iterable = iterable;
	}

	private ValSeq(IPersistentMap meta, ISeq seq, Iterable iterable){
		super(meta);
		this.seq = seq;
		this.iterable = iterable;
	}

	public Object first(){
		return ((Map.Entry) seq.first()).getValue();
	}

	public ISeq next(){
		return create(seq.next());
	}

	public ValSeq withMeta(IPersistentMap meta){
		if(meta() == meta)
			return this;
		return new ValSeq(meta, seq, iterable);
	}

	public Iterator iterator(){
		if(iterable == null)
			return super.iterator();

		if(iterable instanceof IMapIterable)
			return ((IMapIterable)iterable).valIterator();

		final Iterator mapIter = iterable.iterator();
		return new Iterator() {
			public boolean hasNext() {
				return mapIter.hasNext();
			}

			public Object next() {
				return ((Map.Entry)mapIter.next()).getValue();
			}

			public void remove() {
				throw new UnsupportedOperationException();
			}
		};
	}
}

static final IFn MAKE_ENTRY = new AFn() {
    public Object invoke(Object key, Object val) {
        return MapEntry.create(key, val);
    }
};

static final IFn MAKE_KEY = new AFn() {
    public Object invoke(Object key, Object val) {
        return key;
    }
};

static final IFn MAKE_VAL = new AFn() {
    public Object invoke(Object key, Object val) {
        return val;
    }
};

public Object invoke(Object arg1) {
	return valAt(arg1);
}

public Object invoke(Object arg1, Object notFound) {
	return valAt(arg1, notFound);
}

// java.util.Map implementation

public void clear(){
	throw new UnsupportedOperationException();
}

public boolean containsValue(Object value){
	return values().contains(value);
}

public Set entrySet(){
	return new AbstractSet(){

		public Iterator iterator(){
			return APersistentMap.this.iterator();
		}

		public int size(){
			return count();
		}

		public int hashCode(){
			return APersistentMap.this.hashCode();
		}

		public boolean contains(Object o){
			if(o instanceof Entry)
				{
				Entry e = (Entry) o;
				Entry found = entryAt(e.getKey());
				if(found != null && Util.equals(found.getValue(), e.getValue()))
					return true;
				}
			return false;
		}
	};
}

public Object get(Object key){
	return valAt(key);
}

public boolean isEmpty(){
	return count() == 0;
}

public Set keySet(){
	return new AbstractSet(){

		public Iterator iterator(){
			final Iterator mi = APersistentMap.this.iterator();

			return new Iterator(){


				public boolean hasNext(){
					return mi.hasNext();
				}

				public Object next(){
					Entry e = (Entry) mi.next();
					return e.getKey();
				}

				public void remove(){
					throw new UnsupportedOperationException();
				}
			};
		}

		public int size(){
			return count();
		}

		public boolean contains(Object o){
			return APersistentMap.this.containsKey(o);
		}
	};
}

public Object put(Object key, Object value){
	throw new UnsupportedOperationException();
}

public void putAll(Map t){
	throw new UnsupportedOperationException();
}

public Object remove(Object key){
	throw new UnsupportedOperationException();
}

public int size(){
	return count();
}

public Collection values(){
	return new AbstractCollection(){

		public Iterator iterator(){
			final Iterator mi = APersistentMap.this.iterator();

			return new Iterator(){


				public boolean hasNext(){
					return mi.hasNext();
				}

				public Object next(){
					Entry e = (Entry) mi.next();
					return e.getValue();
				}

				public void remove(){
					throw new UnsupportedOperationException();
				}
			};
		}

		public int size(){
			return count();
		}
	};
}

/*
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
	if(o instanceof Map.Entry)
		{
		Map.Entry e = (Map.Entry) o;
		Map.Entry v = entryAt(e.getKey());
		return (v != null && Util.equal(v.getValue(), e.getValue()));
		}
	return false;
}
*/
}
