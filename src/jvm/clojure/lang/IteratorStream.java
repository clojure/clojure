/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Dec 7, 2008 */

package clojure.lang;

import java.util.Iterator;

public class IteratorStream implements IStream{
    final Iterator iter;

    public IteratorStream(Iterator iter) {
        this.iter = iter;
    }

    synchronized public Object next() throws Exception {
        if(iter.hasNext())
            return iter.next();
        return RT.eos();
    }
}
