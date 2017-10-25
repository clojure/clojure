/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 3, 2008 */

package clojure.lang;

import java.io.Serializable;
import java.util.Collection;
import java.util.Iterator;
import java.util.Set;

public abstract class APersistentSet extends AFn implements IPersistentSet, Collection, Set, Serializable, IHashEq {
int _hash = -1;
int _hasheq = -1;
final IPersistentMap impl;

protected APersistentSet(IPersistentMap impl){
	this.impl = impl;
}

public String toString(){
	return RT.printString(this);
}

public boolean contains(Object key){
	return impl.containsKey(key);
}

public Object get(Object key){
	return impl.valAt(key);
}

public int count(){
	return impl.count();
}

public ISeq seq(){
	return RT.keys(impl);
}

public Object invoke(Object arg1) {
	return get(arg1);
}

public boolean equals(Object obj){
    return setEquals(this, obj);
}

static public boolean setEquals(IPersistentSet s1, Object obj) {
    if(s1 == obj) return true;
    if(!(obj instanceof Set))
        return false;
    Set m = (Set) obj;

    if(m.size() != s1.count())
        return false;

    for(Object aM : m)
    {
        if(!s1.contains(aM))
            return false;
    }

    return true;
}

public boolean equiv(Object obj){
	if (!(obj instanceof Set))
        return false;

    Set m = (Set) obj;

    if (m.size() != size())
        return false;

    for(Object aM : m)
    {
        if(!contains(aM))
            return false;
    }

    return true;
}

public int hashCode(){
	if(_hash == -1)
		{
		//int hash = count();
		int hash = 0;
		for(ISeq s = seq(); s != null; s = s.next())
			{
			Object e = s.first();
//			hash = Util.hashCombine(hash, Util.hash(e));
			hash +=  Util.hash(e);
			}
		this._hash = hash;
		}
	return _hash;
}

public int hasheq(){
	if(_hasheq == -1){
//		int hash = 0;
//		for(ISeq s = seq(); s != null; s = s.next())
//			{
//			Object e = s.first();
//			hash +=  Util.hasheq(e);
//			}
//		this._hasheq = hash;
		_hasheq = Murmur3.hashUnordered(this);
	}
	return _hasheq;		
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
    return RT.seqToPassedArray(seq(), a);
}

public int size(){
	return count();
}

public boolean isEmpty(){
	return count() == 0;
}

public Iterator iterator(){
    if(impl instanceof IMapIterable)
        return ((IMapIterable)impl).keyIterator();
    else return new Iterator() {
        private final Iterator iter = impl.iterator();

        public boolean hasNext() {
            return iter.hasNext();
        }

        public Object next() {
            return ((IMapEntry)iter.next()).key();
        }

        public void remove() {
            throw new UnsupportedOperationException();
        }
    };
}

}
