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

public class IdentityHashtableMap extends HashtableMap{

public IdentityHashtableMap(int initialCapacity) {
    super(initialCapacity);
}

public IdentityHashtableMap(Object[] init) {
    super(init);
}

IdentityHashtableMap(int count, PersistentArray array) {
    super(count, array);
}

IMap create(int capacity) {
    return new IdentityHashtableMap(capacity);
}

IMap create(int count, PersistentArray array) {
    return new IdentityHashtableMap(count, array);
}

IMap createArrayMap(Object[] init) {
    return new IdentityArrayMap(init);
}
}
