/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Dec 8, 2008 */

package clojure.lang;

public class StreamSeq extends ASeq {
    IStream stream;
    final Object _first;
    ISeq _rest;

    static public StreamSeq create(IStream stream) throws Exception {
        Object x = stream.next();
        if (RT.isEOS(x))
            return null;
        return new StreamSeq(x, stream);
    }

    StreamSeq(IPersistentMap meta, Object _first, ISeq _rest) {
        super(meta);
        this._first = _first;
        this._rest = _rest;
        this.stream = null;
    }

    StreamSeq(Object first, IStream stream) {
        this._first = first;
        this.stream = stream;
    }


    public Object first() {
        return _first;
    }

    synchronized public ISeq next() {
        if (stream != null) {
            try {
                _rest = create(stream);
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
            stream = null;
        }
        return _rest;
    }

    public Obj withMeta(IPersistentMap meta) {
        if(meta != this.meta())
            {
            next();
                return new StreamSeq(meta, _first, _rest);
            }
        return this;
    }
}
