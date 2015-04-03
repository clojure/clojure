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

/**
 * Implements generic numeric (potentially infinite) range.
 */
public class Range extends ASeq implements IChunkedSeq, IReduce {

private static final int CHUNK_SIZE = 32;

// Invariants guarantee this is never an "empty" seq
//   assert(start != end && step != 0)
final Object end;
final Object start;
final Object step;
final BoundsCheck boundsCheck;
private volatile IChunk _chunk;      // lazy
private volatile ISeq _chunkNext;        // lazy
private volatile ISeq _next;             // cached

private static interface BoundsCheck extends Serializable {
    boolean exceededBounds(Object val);
}

private static BoundsCheck positiveStep(final Object end) {
    return new BoundsCheck() {
        public boolean exceededBounds(Object val){
            return Numbers.gte(val, end);
        }
    };
}

private static BoundsCheck negativeStep(final Object end) {
    return new BoundsCheck() {
        public boolean exceededBounds(Object val){
            return Numbers.lte(val, end);
        }
    };
}

private Range(Object start, Object end, Object step, BoundsCheck boundsCheck){
	this.end = end;
	this.start = start;
    this.step = step;
    this.boundsCheck = boundsCheck;
}

private Range(Object start, Object end, Object step, BoundsCheck boundsCheck, IChunk chunk, ISeq chunkNext){
    this.end = end;
    this.start = start;
    this.step = step;
    this.boundsCheck = boundsCheck;
    this._chunk = chunk;
    this._chunkNext = chunkNext;
}

private Range(IPersistentMap meta, Object start, Object end, Object step, BoundsCheck boundsCheck, IChunk chunk, ISeq chunkNext){
    super(meta);
	this.end = end;
	this.start = start;
    this.step = step;
    this.boundsCheck = boundsCheck;
    this._chunk = chunk;
    this._chunkNext = chunkNext;
}

public static ISeq create(Object end) {
    if(Numbers.isPos(end))
        return new Range(0L, end, 1L, positiveStep(end));
    return PersistentList.EMPTY;
}

public static ISeq create(Object start, Object end) {
    return create(start, end, 1L);
}

public static ISeq create(final Object start, Object end, Object step) {
    if((Numbers.isPos(step) && Numbers.gt(start, end)) ||
       (Numbers.isNeg(step) && Numbers.gt(end, start)) ||
       Numbers.equiv(start, end))
        return PersistentList.EMPTY;
    if(Numbers.isZero(step))
        return Repeat.create(start);
    return new Range(start, end, step, Numbers.isPos(step)?positiveStep(end):negativeStep(end));
}

public Obj withMeta(IPersistentMap meta){
	if(meta == _meta)
		return this;
	return new Range(meta, end, start, step, boundsCheck, _chunk, _chunkNext);
}

public Object first(){
    return start;
}

public void forceChunk() {
    if(_chunk != null) return;

    Object[] arr = new Object[CHUNK_SIZE];
    int n = 0;
    Object val = start;
    while(n < CHUNK_SIZE) {
        arr[n++] = val;
        val = Numbers.addP(val, step);
        if(boundsCheck.exceededBounds(val)) {
            //partial last chunk
            _chunk = new ArrayChunk(arr, 0, n);
            return;
        }
    }

    // full last chunk
    if(boundsCheck.exceededBounds(val)) {
        _chunk = new ArrayChunk(arr, 0, CHUNK_SIZE);
        return;
    }

    // full intermediate chunk
    _chunk = new ArrayChunk(arr, 0, CHUNK_SIZE);
    _chunkNext = new Range(val, end, step, boundsCheck);
}

public ISeq next() {
    if(_next != null)
        return _next;

    forceChunk();
    if(_chunk.count() > 1) {
        IChunk smallerChunk = _chunk.dropFirst();
        _next = new Range(smallerChunk.nth(0), end, step, boundsCheck, smallerChunk, _chunkNext);
        return _next;
    }
    return chunkedNext();
}

public IChunk chunkedFirst() {
    forceChunk();
    return _chunk;
}

public ISeq chunkedNext() {
    return chunkedMore().seq();
}

public ISeq chunkedMore() {
    forceChunk();
    if(_chunkNext == null)
        return PersistentList.EMPTY;
    return _chunkNext;
}

public Object reduce(IFn f) {
    Object acc = start;
    Number i = Numbers.addP(start, step);
    while(! boundsCheck.exceededBounds(i)) {
        acc = f.invoke(acc, i);
        if (RT.isReduced(acc)) return ((Reduced)acc).deref();
        i = Numbers.addP(i, step);
    }
    return acc;
}

public Object reduce(IFn f, Object val) {
    Object acc = val;
    Object i = start;
    while(! boundsCheck.exceededBounds(i)) {
        acc = f.invoke(acc, i);
        if (RT.isReduced(acc)) return ((Reduced)acc).deref();
        i = Numbers.addP(i, step);
    }
    return acc;
}

public Iterator iterator() {
    return new RangeIterator();
}

private class RangeIterator implements Iterator {
    private Object next;

    public RangeIterator() {
        this.next = start;
    }

    public boolean hasNext() {
        return(! boundsCheck.exceededBounds(next));
    }

    public Object next() {
        if (hasNext()) {
            Object ret = next;
            next = Numbers.addP(next, step);
            return ret;
        } else {
            throw new NoSuchElementException();
        }
    }

    public void remove() {
        throw new UnsupportedOperationException();
    }
}

}
