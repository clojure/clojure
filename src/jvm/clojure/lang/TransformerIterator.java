/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* Alex Miller 3/3/15 */

package clojure.lang;

import java.util.Iterator;
import java.util.List;
import java.util.NoSuchElementException;
import java.util.Queue;
import java.util.LinkedList;

public class TransformerIterator implements Iterator {

private static final Buffer EMPTY = new Empty();
private static final Object NONE = new Object();

// Source
private final Iterator sourceIter;
private final IFn xf;
private final boolean multi;

// Iteration state
private volatile Buffer buffer = EMPTY;
private volatile Object next = NONE;
private volatile boolean completed = false;

private TransformerIterator(IFn xform, Iterator sourceIter, boolean multi) {
    this.sourceIter = sourceIter;
    this.xf = (IFn) xform.invoke(new AFn() {
        public Object invoke() {
            return null;
        }

        public Object invoke(Object acc) {
            return acc;
        }

        public Object invoke(Object acc, Object o) {
            buffer = buffer.add(o);
            return acc;
        }
    });
    this.multi = multi;
}

public static Iterator create(IFn xform, Iterator source) {
    return new TransformerIterator(xform, source, false);
}

public static Iterator createMulti(IFn xform, List sources) {
    Iterator[] iters = new Iterator[sources.size()];
    for(int i=0; i<sources.size(); i++)
        iters[i] = (Iterator)sources.get(i);
    return new TransformerIterator(xform, new MultiIterator(iters), true);
}

private boolean step() {
    if(next != NONE)
        return true;

    while (next == NONE) {
        if(buffer.isEmpty()) {
            if(completed) {
                return false;
            } else if (sourceIter.hasNext()) {
                Object iter = null;
                if(multi)
                    iter = xf.applyTo(RT.cons(null, sourceIter.next()));
                else
                    iter = xf.invoke(null, sourceIter.next());

                if(RT.isReduced(iter)) {
                    xf.invoke(null);
                    completed = true;
                }
            } else {
                xf.invoke(null);
                completed = true;
            }
        } else {
            next = buffer.remove();
        }
    }
    return true;
}

public boolean hasNext() {
    return step();
}

public Object next() {
    if(hasNext()) {
        Object ret = next;
        next = NONE;
        return ret;
    }
    throw new NoSuchElementException();
}

public void remove() {
    throw new UnsupportedOperationException();
}

private static interface Buffer {
    Buffer add(Object o);
    Object remove();
    boolean isEmpty();
}

private static class Empty implements Buffer {
    public Buffer add(Object o) {
        return new Single(o);
    }

    public Object remove() {
        throw new IllegalStateException("Removing object from empty buffer");
    }

    public boolean isEmpty() {
        return true;
    }

    public String toString() {
        return "Empty";
    }
}

private static class Single implements Buffer {
    private volatile Object val;

    public Single(Object o) {
        this.val = o;
    }

    public Buffer add(Object o) {
        if (val == NONE) {
            val = o;
            return this;
        } else {
            return new Many(val, o);
        }
    }

    public Object remove() {
        if(val == NONE) {
            throw new IllegalStateException("Removing object from empty buffer");
        }
        Object ret = val;
        val = NONE;
        return ret;
    }

    public boolean isEmpty() {
        return val == NONE;
    }

    public String toString() {
        return "Single: " + val;
    }
}

private static class Many implements Buffer {
    private final Queue vals = new LinkedList();

    public Many(Object o1, Object o2) {
        vals.add(o1);
        vals.add(o2);
    }

    public Buffer add(Object o) {
        vals.add(o);
        return this;
    }

    public Object remove() {
        return vals.remove();
    }

    public boolean isEmpty() {
        return vals.isEmpty();
    }

    public String toString() {
        return "Many: " + vals.toString();
    }
}

private static class MultiIterator implements Iterator {
    private final Iterator[] iters;

    public MultiIterator(Iterator[] iters) {
        this.iters = iters;
    }

    public boolean hasNext(){
        for(Iterator iter : iters)
            if(!iter.hasNext())
                return false;
        return true;
    }

    public Object next() {
        Object[] nexts = new Object[iters.length];
        for(int i = 0;i<iters.length;i++)
            nexts[i] = iters[i].next();
        return new ArraySeq(nexts,0);
    }

    public void remove() {
        throw new UnsupportedOperationException();
    }
}

}
