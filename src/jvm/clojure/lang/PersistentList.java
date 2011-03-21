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

public class PersistentList extends ASeq implements IPersistentList, IReduce, List, Counted {

private final Object _first;
private final IPersistentList _rest;
private final int _count;

public static IFn creator = new RestFn(){
	final public int getRequiredArity(){
		return 0;
	}

	final protected Object doInvoke(Object args) {
		if(args instanceof ArraySeq)
			{
			Object[] argsarray = (Object[]) ((ArraySeq) args).array;
			IPersistentList ret = EMPTY;
			for(int i = argsarray.length - 1; i >= 0; --i)
				ret = (IPersistentList) ret.cons(argsarray[i]);
			return ret;
			}
		LinkedList list = new LinkedList();
		for(ISeq s = RT.seq(args); s != null; s = s.next())
			list.add(s.first());
		return create(list);
	}

	public IObj withMeta(IPersistentMap meta){
		throw new UnsupportedOperationException();
	}

	public IPersistentMap meta(){
		return null;
	}
};

final public static EmptyList EMPTY = new EmptyList(null);

public PersistentList(Object first){
	this._first = first;
	this._rest = null;

	this._count = 1;
}

PersistentList(IPersistentMap meta, Object _first, IPersistentList _rest, int _count){
	super(meta);
	this._first = _first;
	this._rest = _rest;
	this._count = _count;
}

public static IPersistentList create(List init){
	IPersistentList ret = EMPTY;
	for(ListIterator i = init.listIterator(init.size()); i.hasPrevious();)
		{
		ret = (IPersistentList) ret.cons(i.previous());
		}
	return ret;
}

public Object first(){
	return _first;
}

public ISeq next(){
	if(_count == 1)
		return null;
	return (ISeq) _rest;
}

public Object peek(){
	return first();
}

public IPersistentList pop(){
	if(_rest == null)
		return EMPTY.withMeta(_meta);
	return _rest;
}

public int count(){
	return _count;
}

public PersistentList cons(Object o){
	return new PersistentList(meta(), o, this, _count + 1);
}

public IPersistentCollection empty(){
	return EMPTY.withMeta(meta());
}

public PersistentList withMeta(IPersistentMap meta){
	if(meta != _meta)
		return new PersistentList(meta, _first, _rest, _count);
	return this;
}

public Object reduce(IFn f) {
	Object ret = first();
	for(ISeq s = next(); s != null; s = s.next())
		ret = f.invoke(ret, s.first());
	return ret;
}

public Object reduce(IFn f, Object start) {
	Object ret = f.invoke(start, first());
	for(ISeq s = next(); s != null; s = s.next())
		ret = f.invoke(ret, s.first());
	return ret;
}


    static class EmptyList extends Obj implements IPersistentList, List, ISeq, Counted{

	public int hashCode(){
		return 1;
	}

    public boolean equals(Object o) {
        return (o instanceof Sequential || o instanceof List) && RT.seq(o) == null;
    }

	public boolean equiv(Object o){
		return equals(o);
	}
	
    EmptyList(IPersistentMap meta){
		super(meta);
	}

        public Object first() {
            return null;
        }

        public ISeq next() {
            return null;
        }

        public ISeq more() {
            return this;
        }

        public PersistentList cons(Object o){
		return new PersistentList(meta(), o, null, 1);
	}

	public IPersistentCollection empty(){
		return this;
	}

	public EmptyList withMeta(IPersistentMap meta){
		if(meta != meta())
			return new EmptyList(meta);
		return this;
	}

	public Object peek(){
		return null;
	}

	public IPersistentList pop(){
		throw new IllegalStateException("Can't pop empty list");
	}

	public int count(){
		return 0;
	}

	public ISeq seq(){
		return null;
	}


	public int size(){
		return 0;
	}

	public boolean isEmpty(){
		return true;
	}

	public boolean contains(Object o){
		return false;
	}

	public Iterator iterator(){
		return new Iterator(){

			public boolean hasNext(){
				return false;
			}

			public Object next(){
				throw new NoSuchElementException();
			}

			public void remove(){
				throw new UnsupportedOperationException();
			}
		};
	}

	public Object[] toArray(){
		return RT.EMPTY_ARRAY;
	}

	public boolean add(Object o){
		throw new UnsupportedOperationException();
	}

	public boolean remove(Object o){
		throw new UnsupportedOperationException();
	}

	public boolean addAll(Collection collection){
		throw new UnsupportedOperationException();
	}

	public void clear(){
		throw new UnsupportedOperationException();
	}

	public boolean retainAll(Collection collection){
		throw new UnsupportedOperationException();
	}

	public boolean removeAll(Collection collection){
		throw new UnsupportedOperationException();
	}

	public boolean containsAll(Collection collection){
		return collection.isEmpty();
	}

	public Object[] toArray(Object[] objects){
		if(objects.length > 0)
			objects[0] = null;
		return objects;
	}

	//////////// List stuff /////////////////
	private List reify(){
		return Collections.unmodifiableList(new ArrayList(this));
	}

	public List subList(int fromIndex, int toIndex){
		return reify().subList(fromIndex, toIndex);
	}

	public Object set(int index, Object element){
		throw new UnsupportedOperationException();
	}

	public Object remove(int index){
		throw new UnsupportedOperationException();
	}

	public int indexOf(Object o){
		ISeq s = seq();
		for(int i = 0; s != null; s = s.next(), i++)
			{
			if(Util.equiv(s.first(), o))
				return i;
			}
		return -1;
	}

	public int lastIndexOf(Object o){
		return reify().lastIndexOf(o);
	}

	public ListIterator listIterator(){
		return reify().listIterator();
	}

	public ListIterator listIterator(int index){
		return reify().listIterator(index);
	}

	public Object get(int index){
		return RT.nth(this, index);
	}

	public void add(int index, Object element){
		throw new UnsupportedOperationException();
	}

	public boolean addAll(int index, Collection c){
		throw new UnsupportedOperationException();
	}


}

}
