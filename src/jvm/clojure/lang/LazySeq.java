/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Jan 31, 2009 */

package clojure.lang;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.util.*;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public final class LazySeq extends Obj implements ISeq, Sequential, List, IPending, IHashEq{

private static final long serialVersionUID = -7531333024710395876L;

private transient IFn fn;
private Object sv;
private ISeq s;
private volatile Lock lock;

public LazySeq(IFn f){
	fn = f;
	lock = new ReentrantLock();
}

private LazySeq(IPersistentMap meta, ISeq seq){
	super(meta);
	fn = null;
	s = seq;
}

public Obj withMeta(IPersistentMap meta){
	if(meta() == meta)
		return this;
	return new LazySeq(meta, seq());
}

// MUST be locked when called!
final private void force() {
	if (fn != null) {
		sv = fn.invoke();
		fn = null;
	}
}

final private Object sval() {
    Lock l = lock;
    if(l != null) {
        l.lock();
        try {
            //must re-examine under lock
            if(lock != null) { //unrealized
                force();
                return sv;
            }
        } finally {
            l.unlock();
        }
    }
    // realized, read of lock above guarantees visibility of s
    return s;
}

final private Object unwrap(Object ls){
    while(ls instanceof LazySeq) {
        ls = ((LazySeq) ls).sval();
        }
    return ls;
}

final private void realize() {
	Lock l = lock;
	if(l != null) {
		l.lock();
		try {
            //must re-examine under lock
            if(lock != null) {
                force();
                Object ls = sv;
                sv = null;
                if(ls instanceof LazySeq)
                    ls = unwrap(ls);
                s = RT.seq(ls);
                lock = null;
                }
		    }
        finally {
			l.unlock();
		}
	}
}

public final ISeq seq(){
    if(lock != null)
        realize();
	return s;
}

public int count(){
	int c = 0;
	for(ISeq s = seq(); s != null; s = s.next())
		++c;                                                                                
	return c;
}

public Object first(){
	seq();
	if(s == null)
		return null;
	return s.first();
}

public ISeq next(){
	seq();
	if(s == null)
		return null;
	return s.next();	
}

public ISeq more(){
	seq();
	if(s == null)
		return PersistentList.EMPTY;
	return s.more();
}

public ISeq cons(Object o){
	return RT.cons(o, seq());
}

public IPersistentCollection empty(){
	return PersistentList.EMPTY;
}

public boolean equiv(Object o){
	ISeq s = seq();
	if(s != null)
		return s.equiv(o);
	else
		return (o instanceof Sequential || o instanceof List) && RT.seq(o) == null;
}

public int hashCode(){
	ISeq s = seq();
	if(s == null)
		return 1;
	return Util.hash(s);
}

public int hasheq(){
	return Murmur3.hashOrdered(this);
}

public boolean equals(Object o){
	ISeq s = seq();
	if(s != null)
		return s.equals(o);
	else
		return (o instanceof Sequential || o instanceof List) && RT.seq(o) == null;
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
    return RT.seqToPassedArray(seq(), a);
}

public int size(){
	return count();
}

public boolean isEmpty(){
	return seq() == null;
}

public boolean contains(Object o){
	for(ISeq s = seq(); s != null; s = s.next())
		{
		if(Util.equiv(s.first(), o))
			return true;
		}
	return false;
}

public Iterator iterator(){
	return new SeqIterator(this);
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

public boolean isRealized(){
    return lock == null;
}

// custom Serializable implementation - ensure seq is fully-realized before writing
private void writeObject(java.io.ObjectOutputStream out) throws IOException {
	ISeq s = this;
	while(s != null) {
		s = s.next();
	}
	out.defaultWriteObject();
}

}

