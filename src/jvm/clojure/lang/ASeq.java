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
import java.util.Iterator;

public abstract class ASeq extends Obj implements ISeq, Collection, Sequential{
transient int _hash = -1;


public IPersistentCollection empty(){
	return null;
}

protected ASeq(IPersistentMap meta){
	super(meta);
}


protected ASeq(){
}

public boolean equals(Object obj){

	if(!(obj instanceof Sequential))
		return false;
	ISeq ms = ((IPersistentCollection) obj).seq();
	for(ISeq s = seq(); s != null; s = s.rest(), ms = ms.rest())
		{
		if(ms == null || !Util.equal(s.first(), ms.first()))
			return false;
		}
	if(ms != null)
		return false;

	return true;
}

public int hashCode(){
	if(_hash == -1)
		{
		int hash = 0;
		for(ISeq s = seq(); s != null; s = s.rest())
			{
			hash = Util.hashCombine(hash, Util.hash(s.first()));
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
		if(contains(o))
			return true;
		}
	return false;
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
		if(Util.equal(s.first(), o))
			return true;
		}
	return false;
}


public Iterator iterator(){
	return new SeqIterator(this);
}

}
