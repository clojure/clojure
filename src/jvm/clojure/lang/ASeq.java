/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

package clojure.lang;

public abstract class ASeq implements ISeq{

public Object peek() {
    return first();
}

public IPersistentList pop() {
    return rest();
}

public int count() {
    return 1 + RT.count(rest());
}

public ISeq seq() {
    return this;
}

public IPersistentCollection cons(Object o) {
    return new Cons(o, this);
}
}
