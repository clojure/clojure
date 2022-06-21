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

import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * Implements the special common case of a finite range based on long start, end, and step,
 * with no more than Integer.MAX_VALUE items.
 */
public class LongRange extends ASeq implements Counted, IChunkedSeq, IReduce, IDrop {

private static final long serialVersionUID = -1467242400566893909L;

// Invariants guarantee this is never an empty or infinite seq
//   assert(start != end && step != 0)
final long start;
final long end;
final long step;
final int count;

private LongRange(long start, long end, long step, int count){
    this.start = start;
    this.end = end;
    this.step = step;
    this.count = count;
}

private LongRange(IPersistentMap meta, long start, long end, long step, int count){
    super(meta);
    this.start = start;
    this.end = end;
    this.step = step;
    this.count = count;
}

// returns exact size of remaining items OR throws ArithmeticException for overflow case
static long rangeCount(long start, long end, long step) {
    // (1) count = ceiling ( (end - start) / step )
    // (2) ceiling(a/b) = (a+b+o)/b where o=-1 for positive stepping and +1 for negative stepping
    // thus: count = end - start + step + o / step
    return Numbers.add(Numbers.add(Numbers.minus(end, start), step), step > 0 ? -1 : 1) / step;
}

public static ISeq create(long end) {
    if(end > 0) {
        try {
            return new LongRange(0L, end, 1L, Math.toIntExact(rangeCount(0L, end, 1L)));
        } catch(ArithmeticException e) {
            return Range.create(end);  // count > Integer.MAX_VALUE
        }
    } else {
        return PersistentList.EMPTY;
    }
}

public static ISeq create(long start, long end) {
    if(start >= end) {
        return PersistentList.EMPTY;
    } else {
        try {
            return new LongRange(start, end, 1L, Math.toIntExact(rangeCount(start, end, 1L)));
        } catch(ArithmeticException e) {
            return Range.create(start, end);
        }
    }
}

public static ISeq create(final long start, long end, long step) {
    if(step > 0) {
        if(end <= start) return PersistentList.EMPTY;
        try {
            return new LongRange(start, end, step, Math.toIntExact(rangeCount(start, end, step)));
        } catch(ArithmeticException e) {
            return Range.create(start, end, step);
        }
    } else if(step < 0) {
        if(end >= start) return PersistentList.EMPTY;
        try {
            return new LongRange(start, end, step, Math.toIntExact(rangeCount(start, end, step)));
        } catch(ArithmeticException e) {
            return Range.create(start, end, step);
        }
    } else {
        if(end == start) return PersistentList.EMPTY;
        return Repeat.create(start);
    }
}

public Obj withMeta(IPersistentMap meta){
    if(meta == _meta)
        return this;
    return new LongRange(meta, start, end, step, count);
}

public Object first() {
    return start;
}

public ISeq next() {
    if(count > 1) {
        return new LongRange(start + step, end, step, count-1);
    } else {
        return null;
    }
}

public IChunk chunkedFirst() {
    return new LongChunk(start, step, count);
}

public ISeq chunkedNext() {
    return null;
}

public ISeq chunkedMore() {
    return PersistentList.EMPTY;
}

public Sequential drop(int n) {
    if(n <= 0) {
        return this;
    } else if(n < count) {
        return new LongRange(start+(step*n), end, step, count - n);
    } else {
        return null;
    }
}

public int count() {
    return count;
}

public Object reduce(IFn f) {
    Object acc = start;
    long i = start + step;
    int n = count;
    while(n > 1) {
        acc = f.invoke(acc, i);
        if (acc instanceof Reduced) return ((Reduced)acc).deref();
        i += step;
        n--;
    }
    return acc;
}

public Object reduce(IFn f, Object val) {
    Object acc = val;
    int n = count;
    long i = start;
    do {
        acc = f.invoke(acc, i);
        if (RT.isReduced(acc)) return ((Reduced)acc).deref();
        i += step;
        n--;
    } while(n > 0);
    return acc;
}

public Iterator iterator() {
    return new LongRangeIterator();
}

class LongRangeIterator implements Iterator {
    private long next;
    private int remaining;

    public LongRangeIterator() {
        this.next = start;
        this.remaining = count;
    }

    public boolean hasNext() {
        return remaining > 0;
    }

    public Object next() {
        if (remaining > 0) {
            long ret = next;
            next = next + step;
            remaining = remaining - 1;
            return ret;
        } else {
            throw new NoSuchElementException();
        }
    }

    public void remove() {
        throw new UnsupportedOperationException();
    }
}

private static class LongChunk implements IChunk, Serializable {
    final long start;
    final long step;
    final int count;

    public LongChunk(long start, long step, int count) {
        this.start = start;
        this.step = step;
        this.count = count;
    }

    public long first(){
        return start;
    }

    public Object nth(int i){
        return start + (i * step);
    }

    public Object nth(int i, Object notFound){
        if(i >= 0 && i < count)
            return start + (i * step);
        return notFound;
    }

    public int count(){
        return count;
    }

    public LongChunk dropFirst(){
        if(count <= 1)
            throw new IllegalStateException("dropFirst of empty chunk");
        return new LongChunk(start+step, step, count-1);
    }

    public Object reduce(IFn f, Object init) {
        long x = start;
        Object ret = init;
        for(int i=0; i<count; i++) {
            ret = f.invoke(ret, x);
            if(RT.isReduced(ret))
                return ret;
            x += step;
        }
        return ret;
    }

}
}
