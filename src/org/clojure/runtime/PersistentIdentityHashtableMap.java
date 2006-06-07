/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

package org.clojure.runtime;

public class PersistentIdentityHashtableMap extends PersistentHashtableMap {

public PersistentIdentityHashtableMap(int initialCapacity) {
    super(initialCapacity);
}

public PersistentIdentityHashtableMap(Object[] init) {
    super(init);
}

PersistentIdentityHashtableMap(int count, PersistentArray array) {
    super(count, array);
}

IPersistentMap create(int capacity) {
    return new PersistentIdentityHashtableMap(capacity);
}

IPersistentMap create(int count, PersistentArray array) {
    return new PersistentIdentityHashtableMap(count, array);
}

IPersistentMap createArrayMap(Object[] init) {
    return new PersistentIdentityArrayMap(init);
}
}
