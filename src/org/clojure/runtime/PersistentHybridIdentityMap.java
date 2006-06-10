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

public class PersistentHybridIdentityMap extends PersistentHybridMap{

public PersistentHybridIdentityMap(Object[] init) {
    super(init);
}

public PersistentHybridIdentityMap(int initialCapacity) {
    super(initialCapacity);
}

PersistentHybridIdentityMap(IPersistentMap impl) {
    super(impl);
}

public IPersistentMap create(IPersistentMap impl) {
    return new PersistentHybridIdentityMap(impl);
}

public PersistentArrayMap createArrayMap(Object[] init) {
    return new PersistentArrayIdentityMap(init);
}

IPersistentMap createArrayMap() {
    return PersistentArrayIdentityMap.EMPTY;
}

IPersistentMap createHashtableMap(Object[] init) {
    return new PersistentHashtableIdentityMap(init);
}

IPersistentMap createHashtableMap(int initialCapacity) {
    return new PersistentHashtableIdentityMap(initialCapacity);
}

}
