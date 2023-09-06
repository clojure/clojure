/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Feb 9, 2009 */

package clojure.lang;

import java.util.function.BooleanSupplier;
import java.util.function.IntSupplier;
import java.util.function.LongSupplier;
import java.util.function.Supplier;

public interface IDeref extends Supplier, BooleanSupplier, IntSupplier, LongSupplier {
Object deref() ;

@Override
default Object get() {
    return deref();
}

@Override
default boolean getAsBoolean() {
    return RT.booleanCast(deref());
}

@Override
default int getAsInt() {
    return RT.intCast(deref());
}

@Override
default long getAsLong() {
    return RT.longCast(deref());
}

}
