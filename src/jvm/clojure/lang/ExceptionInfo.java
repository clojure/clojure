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
public class ExceptionInfo extends RuntimeException implements IExceptionInfo {
    public final IPersistentMap data;

    public ExceptionInfo(String s, IPersistentMap data) {
        this(s, data, null);
    }

    public ExceptionInfo(String s, IPersistentMap data, Throwable throwable) {
        // null cause is equivalent to not passing a cause
        super(s, throwable);
        if (data != null) {
            this.data = data;
        }  else {
            throw new IllegalArgumentException("Additional data must be non-nil.");
        }
    }

    public IPersistentMap getData() {
        return data;
    }

    public String toString() {
        return "clojure.lang.ExceptionInfo: " + getMessage() + " " + data.toString();
    }
}
