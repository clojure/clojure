/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Jan 1, 2009 */

package clojure.lang;

import java.util.concurrent.atomic.AtomicReference;

public class Atom extends ARef{
    final AtomicReference state;

    public Atom(Object state) {
        this.state = new AtomicReference(state);
    }

    public Atom(Object state, IPersistentMap meta) {
        super(meta);
        this.state = new AtomicReference(state);
    }

    public Object get() {
        return state.get();
    }

    public Object swap(IFn f, ISeq args) throws Exception {
        for(;;)
            {
            Object v = get();
            Object newv = f.applyTo(new Cons(v, args));
            validate(newv);
            if(state.compareAndSet(v,newv))
                return newv;
            }
    }

    public boolean compareAndSet(Object oldv, Object newv){
        validate(newv);
        return state.compareAndSet(oldv, newv);
    }

    public Object reset(Object newval){
        validate(newval);
        state.set(newval);
        return newval;
    }
}
