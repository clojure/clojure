/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Aug 2, 2009 */

package clojure.lang;

public interface IAtom{
Object swap(IFn f);

Object swap(IFn f, Object arg);

Object swap(IFn f, Object arg1, Object arg2);

Object swap(IFn f, Object x, Object y, ISeq args);

boolean compareAndSet(Object oldv, Object newv);

Object reset(Object newval);
}
