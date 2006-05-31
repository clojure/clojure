package org.clojure.runtime;

/**
 * Created by IntelliJ IDEA.
 * User: rich
 * Date: May 31, 2006
 * Time: 3:41:10 PM
 * To change this template use File | Settings | File Templates.
 */
public interface IObj {
Object put(Comparable key, Object val);

Object get(Comparable key);

boolean has(Comparable key);
}
