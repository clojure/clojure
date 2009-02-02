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

import java.util.*;

public abstract class ASeq extends Obj implements ISeq, List, Streamable{
transient int _hash = -1;

public String toString(){
	return RT.printString(this);
}

public IPersistentCollection empty(){
	return null;
}

protected ASeq(IPersistentMap meta){
	super(meta);
}


protected ASeq(){
}

public boolean equiv(Object obj){

	if(!(obj instanceof Sequential || obj instanceof List))
		return false;
	ISeq ms = RT.seq(obj);
	for(ISeq s = seq(); s != null; s = s.rest(), ms = ms.rest())
		{
		if(ms == null || !Util.equiv(s.first(), ms.first()))
			return false;
		}
	return ms == null;

}

public boolean equals(Object obj){

	if(!(obj instanceof Sequential || obj instanceof List))
		return false;
	ISeq ms = RT.seq(obj);
	for(ISeq s = seq(); s != null; s = s.rest(), ms = ms.rest())
		{
		if(ms == null || !Util.equals(s.first(), ms.first()))
			return false;
		}
	return ms == null;

}

public int hashCode(){
	if(_hash == -1)
		{
		int hash = 1;
		for(ISeq s = seq(); s != null; s = s.rest())
			{
			hash = 31 * hash + (s.first() == null ? 0 : s.first().hashCode());
			}
		this._hash = hash;
		}
	return _hash;
}


//public Object reduce(IFn f) throws Exception{
//	Object ret = first();
//	for(ISeq s = rest(); s != null; s = s.rest())
//		ret = f.invoke(ret, s.first());
//	return ret;
//}
//
//public Object reduce(IFn f, Object start) throws Exception{
//	Object ret = f.invoke(start, first());
//	for(ISeq s = rest(); s != null; s = s.rest())
//		ret = f.invoke(ret, s.first());
//	return ret;
//}

//public Object peek(){
//	return first();
//}
//
//public IPersistentList pop(){
//	return rest();
//}

public int count(){
	int i = 1;
	for(ISeq s = rest(); s != null; s = s.rest(), i++)
		;
	return i;
}

public ISeq seq(){
	return this;
}

public ISeq cons(Object o){
	return new Cons(o, this);
}

final public ISeq rest(){
    Seqable m = more();
    if(m == null)
        return null;
    return m.seq();
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
	for(ISeq s = seq(); s != null; s = s.rest())
		{
		if(Util.equiv(s.first(), o))
			return true;
		}
	return false;
}


public Iterator iterator(){
	return new SeqIterator(this);
}



public IStream stream() throws Exception {
    return new Stream(this);
}

    static class Stream implements IStream{
    ISeq s;

    public Stream(ISeq s) {
        this.s = s;
    }

    synchronized public Object next() throws Exception {
        if(s != null)
            {
            Object ret = s.first();
            s = s.rest();
            return ret;
            }
        return RT.eos();
    }
}


//////////// List stuff /////////////////
private List reify(){
	return new ArrayList(this);
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
	for(int i = 0; s != null; s = s.rest(), i++)
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
