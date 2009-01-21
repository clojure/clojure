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

final public class AStream implements Seqable, Streamable, Sequential {

    static final ISeq NO_SEQ = new Cons(null, null);

    ISeq seq = NO_SEQ;
    final IFn src;
    Cons pushed = null;
    Iter iter = null;

    public AStream(IFn src) {
        this.src = src;
    }

    final synchronized public ISeq seq() {
        if (seq == NO_SEQ)
            {
            iter();
            seq = Seq.create(pushed,src);
            }
        return seq;
    }

    final synchronized public AStream stream() throws Exception {
        if (seq == NO_SEQ)
            return this;
        return RT.stream(seq);
    }

    final synchronized public Iter iter() {
        if (iter != null)
            throw new IllegalStateException("Already iterating");

        return iter = new Iter(this);
    }

    static public class Iter {
        final AStream s;

        Iter(AStream s) {
            this.s = s;
        }

        final public Iter pushBack(Object x) throws Exception {
            synchronized (s)
                {
                if (s.iter != this)
                    throw new IllegalAccessError("Invalid iterator");
                s.pushed = new Cons(x,s.pushed);
                return this;
                }
        }

        final public AStream detach() {
            synchronized (s)
                {
                if (s.iter != this)
                    throw new IllegalAccessError("Invalid iterator");
                s.iter = null;
                return s;
                }
        }

        final public Object next(Object eos) {
            synchronized (s)
                {
                if (s.iter != this)
                    throw new IllegalAccessError("Invalid iterator");
                if (s.pushed != null)
                    {
                    Object ret = s.pushed.first();
                    s.pushed = (Cons) s.pushed.rest();
                    return ret;
                    }
                try
                    {
                    return s.src.invoke(eos);
                    }
                catch (Exception e)
                    {
                    throw new RuntimeException(e);
                    }
                }
        }

    }

    static class Seq extends ASeq {
        static final Object EOS = new Object();

        final Object _first;
        ISeq _rest = NO_SEQ;
        final ISeq pushed;
        final IFn src;

        Seq(Object first, ISeq pushed, IFn src) {
            _first = first;
            this.pushed = pushed;
            this.src = src;
        }

        Seq(IPersistentMap meta, Object _first, ISeq _rest) {
            super(meta);
            this._first = _first;
            this._rest = _rest;
            this.pushed = null;
            this.src = null;
        }

        final public Object first() {
            return _first;
        }

        final synchronized public ISeq rest() {
            if (_rest == NO_SEQ)
                {
                _rest = create(pushed, src);
                }
            return _rest;
        }

        static Seq create(ISeq pushed, IFn src) {
            if(pushed != null)
                return new Seq(pushed.first(),pushed.rest(),src);
            try
                {
                Object x = src.invoke(EOS);
                if (x == EOS)
                    return null;
                return new Seq(x, null, src);
                }
            catch (Exception e)
                {
                throw new RuntimeException(e);
                }
        }

        public Obj withMeta(IPersistentMap meta) {
            rest();
            return new Seq(meta, _first, _rest);
        }

    }
}
