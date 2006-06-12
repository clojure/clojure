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


public interface IObj {
Object putAttr( Object key, Object val) throws Exception;

Object getAttr( Object key) throws Exception;

boolean hasAttr( Object key) throws Exception;

IPersistentMap attrs() throws Exception;

void removeAttr(Object key) throws Exception;
}
