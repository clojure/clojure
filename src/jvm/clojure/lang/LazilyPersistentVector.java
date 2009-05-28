/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich May 14, 2008 */

package clojure.lang;

import java.util.Collection;

public class LazilyPersistentVector extends APersistentVector{
final Object[] array;
PersistentVector v = null;

static public IPersistentVector createOwning(Object... items){
	if(items.length == 0)
		return PersistentVector.EMPTY;
	return new LazilyPersistentVector(null, items, null);
}

static public IPersistentVector create(Collection coll){
	return createOwning(coll.toArray());
}

LazilyPersistentVector(IPersistentMap meta, Object[] array, PersistentVector v){
	super(meta);
	this.array = array;
	this.v = v;
}

public ISeq seq(){
	if(array.length == 0)
		return null;
	return new ChunkedSeq(array);
}

static class ChunkedSeq extends ASeq implements IChunkedSeq, IndexedSeq{
	final Object[] array;
	final int offset;
	final int end;
	static final int BLOCK = 32;

	ChunkedSeq(IPersistentMap meta, Object[] array, int offset, int end){
		super(meta);
		this.array = array;
		this.offset = offset;
		this.end = end;
	}

	ChunkedSeq(Object[] array){
		this(array, 0);
	}

	ChunkedSeq(Object[] array, int offset){
		this(array,offset,Math.min(offset + BLOCK, array.length));
	}

	ChunkedSeq(Object[] array, int offset, int end){
		this.array = array;
		this.offset = offset;
		this.end = end;
	}

	public Obj withMeta(IPersistentMap meta){
		if(meta != _meta)
			return new ChunkedSeq(meta, array,offset,end);
		return this;
	}

	public Object first(){
		return array[offset];
	}

	public ISeq next(){
		if(offset + 1 < end)
			return new ChunkedSeq(array,offset + 1,end);
		return chunkedNext();
	}

	public Indexed chunkedFirst(){
		return new ArrayChunk(array, offset, end);
	}

	public ISeq chunkedNext(){
		if(end < array.length)
			return new ChunkedSeq(array,end);
		return null;
		}

	public ISeq chunkedMore(){
		ISeq s = chunkedNext();
		if(s == null)
			return PersistentList.EMPTY;
		return s;
	}

	public int index(){
		return offset;
	}

	public int count(){
		return array.length - offset;
	}
}

public Object[] toArray(){
	return array.clone();
}

public Object nth(int i){
	return array[i];
}

public IPersistentVector assocN(int i, Object val){
	return (IPersistentVector) v().assoc(i, val);
}

public int count(){
	return array.length;
}

public IPersistentVector cons(Object o){
	return v().cons(o);
}

public IPersistentCollection empty(){
	return PersistentVector.EMPTY.withMeta(meta());	
}

public IPersistentStack pop(){
	return v().pop();
}

private synchronized IPersistentVector v(){
	if(v == null)
		{
		v = PersistentVector.create(array);
		}
	return v;
}

public Obj withMeta(IPersistentMap meta){
	if(meta != _meta)
		return new LazilyPersistentVector(meta, array, v);
	return this;
}

}
