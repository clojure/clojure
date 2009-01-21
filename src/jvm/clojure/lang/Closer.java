/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Jan 10, 2009 */

package clojure.lang;

import java.io.Closeable;
import java.io.IOException;

public class Closer implements Closeable{
    ISeq closes;


    public void close() throws IOException {
        for(ISeq s = closes;s!=null;s = s.rest())
            {
            ((Closeable)s.first()).close();
            }
    }

    public Closer register(Closeable c) {
        closes = new Cons(c, closes);
        return this;
    }
}
