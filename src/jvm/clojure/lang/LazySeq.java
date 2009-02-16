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

import java.util.*;

public class LazySeq extends AFn implements ISeq, List {
    static final ISeq DUMMY = new Cons(null, null);
    static final LazySeq EMPTY = new LazySeq(null);

    private ISeq s;

    public LazySeq() {
        this(DUMMY);
    }

    LazySeq(ISeq s) {
        this.s = s;
    }

    final synchronized public ISeq seq() {
        if(s == DUMMY)
            {
            try
                {
                s = RT.seq(invoke());
                }
            catch (Exception e)
                {
                throw new RuntimeException(e);
                }
            }
        return s;
    }

    public int count() {
        int c = 0;
        for (ISeq s = seq(); s != null; s = s.next())
            ++c;
        return c;
    }

    public Object first() {
        return RT.first(seq());
    }

    public ISeq next() {
        return RT.next(seq());
    }

    public ISeq more() {
        return RT.more(seq());
    }

    public ISeq cons(Object o) {
        return RT.cons(o, seq());
    }

    public IPersistentCollection empty() {
        return null;
    }

    public boolean equiv(Object o) {
        ISeq s = seq();
        return s == o || (s != null && s.equiv(o));
    }

    public int hashCode() {
        return Util.hash(seq());
    }

    public boolean equals(Object o) {
        ISeq s = seq();
        return s == o || (s != null && s.equals(o));
    }


// java.util.Collection implementation

    public Object[] toArray() {
        return RT.seqToArray(seq());
    }

    public boolean add(Object o) {
        throw new UnsupportedOperationException();
    }

    public boolean remove(Object o) {
        throw new UnsupportedOperationException();
    }

    public boolean addAll(Collection c) {
        throw new UnsupportedOperationException();
    }

    public void clear() {
        throw new UnsupportedOperationException();
    }

    public boolean retainAll(Collection c) {
        throw new UnsupportedOperationException();
    }

    public boolean removeAll(Collection c) {
        throw new UnsupportedOperationException();
    }

    public boolean containsAll(Collection c) {
        for (Object o : c)
            {
            if (!contains(o))
                return false;
            }
        return true;
    }

    public Object[] toArray(Object[] a) {
        if (a.length >= count())
            {
            ISeq s = seq();
            for (int i = 0; s != null; ++i, s = s.next())
                {
                a[i] = s.first();
                }
            if (a.length > count())
                a[count()] = null;
            return a;
            }
        else
            return toArray();
    }

    public int size() {
        return count();
    }

    public boolean isEmpty() {
        return count() == 0;
    }

    public boolean contains(Object o) {
        for (ISeq s = seq(); s != null; s = s.next())
            {
            if (Util.equiv(s.first(), o))
                return true;
            }
        return false;
    }

    public Iterator iterator() {
        return new SeqIterator(seq());
    }

    //////////// List stuff /////////////////
    private List reify() {
        return new ArrayList(this);
    }

    public List subList(int fromIndex, int toIndex) {
        return reify().subList(fromIndex, toIndex);
    }

    public Object set(int index, Object element) {
        throw new UnsupportedOperationException();
    }

    public Object remove(int index) {
        throw new UnsupportedOperationException();
    }

    public int indexOf(Object o) {
        ISeq s = seq();
        for (int i = 0; s != null; s = s.next(), i++)
            {
            if (Util.equiv(s.first(), o))
                return i;
            }
        return -1;
    }

    public int lastIndexOf(Object o) {
        return reify().lastIndexOf(o);
    }

    public ListIterator listIterator() {
        return reify().listIterator();
    }

    public ListIterator listIterator(int index) {
        return reify().listIterator(index);
    }

    public Object get(int index) {
        return RT.nth(this, index);
    }

    public void add(int index, Object element) {
        throw new UnsupportedOperationException();
    }

    public boolean addAll(int index, Collection c) {
        throw new UnsupportedOperationException();
    }

}
