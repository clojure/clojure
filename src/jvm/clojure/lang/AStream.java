/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Dec 14, 2008 */

package clojure.lang;

import java.util.concurrent.Callable;

final public class AStream implements Seqable {

    ISeq seq = null;
    Callable src;

    public AStream(Callable src){
	    this.src = src;
    }

    final synchronized public ISeq seq(){
        if (src != null)
            {
            seq = Seq.create(src);
            src = null;
            }
        return seq;
    }

    final synchronized public Object next() throws Exception {
        if (src == null)
            return RT.eos();
        return src.call();
    }

    static class Seq extends ASeq {
        Callable src;
        final Object _first;
        ISeq _rest;

        static Seq create(Callable src) {
            Object x;
            try
                {
                x = src.call();
                }
            catch (Exception e)
                {
                throw new RuntimeException(e);
                }
            if (RT.isEOS(x))
                return null;
	        else
                 return new Seq(x, src);
        }

        Seq(IPersistentMap meta, Object _first, ISeq _rest) {
            super(meta);
            this._first = _first;
            this._rest = _rest;
            this.src = null;
        }

        Seq(Object first, Callable src) {
            this._first = first;
            this.src = src;
        }


        public Object first() {
            return _first;
        }

        synchronized public ISeq rest() {
            if (src != null)
                {
                _rest = create(src);
                src = null;
                }
            return _rest;
        }

        synchronized public Obj withMeta(IPersistentMap meta) {
            if (meta != this.meta())
                {
                rest();
                return new Seq(meta, _first, _rest);
                }
            return this;
        }
    }

}
