/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 5, 2009 */

package clojure.lang;

final public class Stream implements Seqable, Streamable, Sequential {

    static final ISeq NO_SEQ = new Cons(null, null);

    ISeq seq = NO_SEQ;
    final IFn src;
	final IFn xform;
    Cons pushed = null;
    IFn tap = null;

	public Stream(IFn src){
		this.src = src;
		this.xform = null;
	}

	public Stream(IFn xform, Stream src) {
        this.src = src.tap();
		this.xform = xform;
    }

	final synchronized public ISeq seq(){
		if(seq == NO_SEQ)
			{
			tap();
			seq = makeSeq(tap);
			}
		return seq;
	}

	static ISeq makeSeq(final IFn tap){
		return RT.seq(new LazySeq(new AFn(){
				public Object invoke() throws Exception{
					Object v;
					do {
						v = tap.invoke();
					} while(v == RT.SKIP);
					if(v == RT.EOS)
						return null;
					return new Cons(v, new LazySeq(this));
				}
			}));
	}

    final synchronized public Stream stream() throws Exception {
            return this;
    }

    final synchronized public IFn tap() {
        if (tap != null)
            throw new IllegalStateException("Stream already tapped");

        return tap = makeTap(xform, src);
    }

	static IFn makeTap(final IFn xform, final IFn src){
		return new AFn(){
			public Object invoke() throws Exception{
				Object v;
				do {
					v = src.invoke();
				} while(v == RT.SKIP);
				if(xform == null || v == RT.EOS)
					return v;
				return xform.invoke(v);
			}
		};
	}

}
