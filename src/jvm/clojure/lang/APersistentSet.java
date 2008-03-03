/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 3, 2008 */

package clojure.lang;

import java.util.Collection;
import java.util.Iterator;

public abstract class APersistentSet extends AFn implements IPersistentSet, Collection{
int _hash = -1;
final IPersistentMap impl;

protected APersistentSet(IPersistentMap meta, IPersistentMap impl){
	super(meta);
	this.impl = impl;
}

public boolean contains(Object key){
	return impl.containsKey(key);
}

public int count(){
	return impl.count();
}

public ISeq seq(){
	return RT.keys(impl);
}

public Object invoke(Object arg1) throws Exception{
	return contains(arg1)?arg1:null;
}

public boolean equals(Object obj){
	if(!(obj instanceof IPersistentSet))
		return false;
	IPersistentSet m = (IPersistentSet) obj;

	if(m.count() != count() || m.hashCode() != hashCode())
		return false;

	for(ISeq s = seq(); s != null; s = s.rest())
		{
		if(!m.contains(s.first()))
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
			Object e =  s.first();
			hash = RT.hashCombine(hash,RT.hash(e));
			}
		this._hash = hash;
		}
	return _hash;
}

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

public Iterator iterator(){
	return new SeqIterator(seq());
}

}
