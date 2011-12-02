/**
 * Copyright (c) Rich Hickey. All rights reserved.
 * The use and distribution terms for this software are covered by the
 * Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 * which can be found in the file epl-v10.html at the root of this distribution.
 * By using this software in any fashion, you are agreeing to be bound by
 * the terms of this license.
 * You must not remove this notice, or any other, from this software.
 */

package clojure.lang;

/**
 * Exception that carries data (a map) as additional payload. Clojure programs that need
 * richer semantics for exceptions should use this in lieu of defining project-specific
 * exception classes.
 */
public class ExceptionInfo extends RuntimeException{
    public final IPersistentMap data;

    public ExceptionInfo(String s, IPersistentMap data) {
        super(s);
        if (data instanceof IPersistentMap) {
            this.data = data;
        }  else {
            throw new IllegalArgumentException("Additional data must be a persistent map: " + data);
        }
    }

    public ExceptionInfo(String s, IPersistentMap data, Throwable throwable) {
        super(s, throwable);
        this.data = data;
    }

    public IPersistentMap getData() {
        return data;
    }

    public String toString() {
        return "clojure.lang.ExceptionInfo: " + getMessage() + " " + data.toString();
    }
}
