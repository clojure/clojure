/**
 *   Copyright (c) Rich Hickey and Contributors. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

package clojure.java.api;

import clojure.lang.IFn;
import clojure.lang.Symbol;
import clojure.lang.Var;
import clojure.lang.RT;

/**
 * <p>The Clojure class provides a minimal interface to bootstrap Clojure access
 * from other JVM languages. It provides:</p>
 *
 * <ol>
 * <li>The ability to use Clojure's namespaces to locate an arbitrary
 * <a href="http://clojure.org/vars">var</a>, returning the
 * var's {@link clojure.lang.IFn} interface.</li>
 * <li>A convenience method <code>read</code> for reading data using
 * Clojure's edn reader</li>
 * </ol>
 *
 * <p>To lookup and call a Clojure function:</p>
 *
 * <pre>
 * IFn plus = Clojure.var("clojure.core", "+");
 * plus.invoke(1, 2);</pre>
 *
 * <p>Functions in <code>clojure.core</code> are automatically loaded. Other
 * namespaces can be loaded via <code>require</code>:</p>
 *
 * <pre>
 * IFn require = Clojure.var("clojure.core", "require");
 * require.invoke(Clojure.read("clojure.set"));</pre>
 *
 * <p><code>IFn</code>s can be passed to higher order functions, e.g. the
 * example below passes <code>plus</code> to <code>read</code>:</p>
 *
 * <pre>
 * IFn map = Clojure.var("clojure.core", "map");
 * IFn inc = Clojure.var("clojure.core", "inc");
 * map.invoke(inc, Clojure.read("[1 2 3]"));</pre>
 */
public class Clojure {
    private Clojure() {}

    private static Symbol asSym(Object o) {
        Symbol s;
        if (o instanceof String) {
            s = Symbol.intern((String) o);
        }  else {
            s = (Symbol) o;
        }
        return s;
    }

    /**
     * Returns the var associated with qualifiedName.
     *
     * @param qualifiedName  a String or clojure.lang.Symbol
     * @return               a clojure.lang.IFn
     */
    public static IFn var(Object qualifiedName) {
        Symbol s = asSym(qualifiedName);
        return var(s.getNamespace(), s.getName());
    }

    /**
     * Returns an IFn associated with the namespace and name.
     *
     * @param ns        a String or clojure.lang.Symbol
     * @param name      a String or clojure.lang.Symbol
     * @return          a clojure.lang.IFn
     */
    public static IFn var(Object ns, Object name) {
        return Var.intern(asSym(ns), asSym(name));
    }

    /**
     * Read one object from the String s.  Reads data in the
     * <a href="http://edn-format.org">edn format</a>.
     * @param s   a String
     * @return    an Object, or nil.
     */
    public static Object read(String s) {
        return EDN_READ_STRING.invoke(s);
    }

    static {
        RT.init();
        Symbol edn = (Symbol) var("clojure.core", "symbol").invoke("clojure.edn");
        var("clojure.core", "require").invoke(edn);
    }
    private static final IFn EDN_READ_STRING = var("clojure.edn", "read-string");
}
