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

/**
 * Created by IntelliJ IDEA.
 * User: rich
 * Date: May 31, 2006
 * Time: 3:41:10 PM
 * To change this template use File | Settings | File Templates.
 */
public interface IObj {
Object put(ThreadLocalData tld, Comparable key, Object val) throws Exception;

Object get(ThreadLocalData tld, Comparable key) throws Exception;

boolean has(ThreadLocalData tld, Comparable key) throws Exception;
}
