/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/
 
/* rich 7/5/14 */

package clojure.lang;

import java.util.*;

public final class LazyTransformer extends Obj implements ISeq, Sequential, List, IPending, IHashEq{

IStepper stepper;
Object first = null;
LazyTransformer rest = null;

public LazyTransformer(IStepper stepper){
	this.stepper = stepper;
}

public static LazyTransformer create(IFn xform, Object coll){
	return new LazyTransformer(new Stepper(xform, RT.iter(coll)));
}

public static LazyTransformer createMulti(IFn xform, Object[] colls){
	Iterator[] iters = new Iterator[colls.length];
	for(int i = 0;i< colls.length;i++)
		iters[i] = RT.iter(colls[i]);
	return new LazyTransformer(new MultiStepper(xform, iters));
}
private LazyTransformer(IPersistentMap meta, Object first, LazyTransformer rest){
	super(meta);
	this.stepper = null;
	this.first = first;
	this.rest = rest;
}

public Obj withMeta(IPersistentMap meta){
	seq();
	return new LazyTransformer(meta, first, rest);
}


final synchronized public ISeq seq(){
	if(stepper != null)
		stepper.step(this);
	if(rest == null)
		return null;
	return this;
}

public Object first(){
	if(stepper != null)
		seq();
	if(rest == null)
		return null;
	return first;
}

public ISeq next(){
	if(stepper != null)
		seq();
	if(rest == null)
		return null;
	return rest.seq();
}

public ISeq more(){
	if(stepper != null)
		seq();
	if(rest == null)
		return PersistentList.EMPTY;
	return rest;
}

public int count(){
	int c = 0;
	for(ISeq s = seq(); s != null; s = s.next())
		++c;
	return c;
}

static interface IStepper{
	void step(LazyTransformer lt);
}

static class Stepper implements IStepper{
	Iterator iter;
	IFn xform;
	static IFn stepfn = new AFn(){
		public Object invoke(Object result){
			LazyTransformer lt = (LazyTransformer) (RT.isReduced(result)?
			                                        ((Reduced)result).deref():result);
			lt.stepper = null;
			return result;
			}
		public Object invoke(Object result, Object input){
			LazyTransformer lt = (LazyTransformer)result;
			lt.first = input;
			lt.rest = new LazyTransformer(lt.stepper);
			lt.stepper = null;
			return lt.rest;
			}
		};

	Stepper(IFn xform, Iterator iter){
		this.iter = iter;
		this.xform = (IFn) xform.invoke(stepfn);
		}

	public void step(LazyTransformer lt){
		while(lt.stepper != null && iter.hasNext()){
			if(RT.isReduced(xform.invoke(lt, iter.next())))
				{
				if(lt.rest != null)
					lt.rest.stepper = null;
				break;
				}
			}
		if(lt.stepper != null)
			xform.invoke(lt);
		}
}

static class MultiStepper implements IStepper{
	Iterator[] iters;
	Object[] nexts;
	IFn xform;
	static IFn stepfn = new AFn(){
		public Object invoke(Object result){
			LazyTransformer lt = (LazyTransformer) (RT.isReduced(result)?
			                                        ((Reduced)result).deref():result);
			lt.stepper = null;
			return lt;
			}
		public Object invoke(Object result, Object input){
			LazyTransformer lt = (LazyTransformer)result;
			lt.first = input;
			lt.rest = new LazyTransformer(lt.stepper);
			lt.stepper = null;
			return lt.rest;
			}
		};

	MultiStepper(IFn xform, Iterator[] iters){
		this.iters = iters;
		this.nexts = new Object[iters.length];
		this.xform = (IFn) xform.invoke(stepfn);
		}

	boolean hasNext(){
		for(Iterator iter : iters)
			if(!iter.hasNext())
				return false;
		return true;
		}

	ISeq next(){
		for(int i = 0;i<iters.length;i++)
			nexts[i] = iters[i].next();
		return new ArraySeq(nexts,0);
	}

	public void step(LazyTransformer lt){
		while(lt.stepper != null && hasNext()){
			if(RT.isReduced(xform.applyTo(RT.cons(lt, next()))))
				{
				if(lt.rest != null)
					lt.rest.stepper = null;
				break;
				}
			}
		if(lt.stepper != null)
			xform.invoke(lt);
		}
}

public ISeq cons(Object o){
	return RT.cons(o, seq());
}

public IPersistentCollection empty(){
	return PersistentList.EMPTY;
}

public boolean equiv(Object o){
	if(this == o)
        return true;
	if(!(o instanceof Sequential || o instanceof List))
		return false;
	ISeq ms = RT.seq(o);
	for(ISeq s = seq(); s != null; s = s.next(), ms = ms.next())
		{
            if(ms == null || !Util.equiv(s.first(), ms.first()))
                return false;
		}
	return ms == null;}

public int hashCode(){
	ISeq s = seq();
	if(s == null)
		return 1;
	return Util.hash(seq());
}

public int hasheq(){
	return Murmur3.hashOrdered(this);
}

public boolean equals(Object o){
	if(this == o)
        return true;
	if(!(o instanceof Sequential || o instanceof List))
		return false;
	ISeq ms = RT.seq(o);
	for(ISeq s = seq(); s != null; s = s.next(), ms = ms.next())
		{
            if(ms == null || !Util.equals(s.first(), ms.first()))
                return false;
		}
	return ms == null;
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


synchronized public boolean isRealized(){
	return stepper == null;
}

}
