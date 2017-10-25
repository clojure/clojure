/**
 * Copyright (c) Rich Hickey. All rights reserved.
 * The use and distribution terms for this software are covered by the
 * Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 * which can be found in the file epl-v10.html at the root of this distribution.
 * By using this software in any fashion, you are agreeing to be bound by
 * the terms of this license.
 * You must not remove this notice, or any other, from this software.
 */

/* ghadi shayban Sep 24, 2014 */

package clojure.lang;

import java.util.Iterator;

public final class RecordIterator implements Iterator {

    int i = 0;
    final int basecnt;
    final ILookup rec;
    final IPersistentVector basefields;
    final Iterator extmap;

    public RecordIterator (ILookup rec, IPersistentVector basefields, Iterator extmap) {
        this.rec = rec;
        this.basefields = basefields;
        this.basecnt = basefields.count();
        this.extmap = extmap;
    }

    public boolean hasNext() {
        if (i < basecnt) {
            return true;
        } else {
            return extmap.hasNext();
        }
    }

    public Object next() {
        if (i < basecnt) {
            Object k = basefields.nth(i);
            i++;
            return MapEntry.create(k, rec.valAt(k));
        } else  {
            return extmap.next();
        }
    }

    public void remove() {
        throw new UnsupportedOperationException();
    }
}
