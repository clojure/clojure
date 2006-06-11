/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

using System;
using System.Collections;

namespace clojure.lang
{

public class PersistentHybridIdentityMap : PersistentHybridMap{

public PersistentHybridIdentityMap(Object[] init) :base(init) {
}

public PersistentHybridIdentityMap(int initialCapacity) :base(initialCapacity) {
}

PersistentHybridIdentityMap(IPersistentMap impl) :base(impl) {
}

override public IPersistentMap create(IPersistentMap impl) {
    return new PersistentHybridIdentityMap(impl);
}

override public PersistentArrayMap createArrayMap(Object[] init) {
    return new PersistentArrayIdentityMap(init);
}

override internal IPersistentMap createArrayMap() {
    return PersistentArrayIdentityMap.EMPTY;
}

override internal IPersistentMap createHashtableMap(Object[] init) {
    return new PersistentHashtableIdentityMap(init);
}

override internal IPersistentMap createHashtableMap(int initialCapacity) {
    return new PersistentHashtableIdentityMap(initialCapacity);
}

}

}
