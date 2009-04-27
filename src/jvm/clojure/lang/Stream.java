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

    ISeq sequence = NO_SEQ;
    final IFn src;
	final IFn xform;
    IFn tap = null;

	public Stream(IFn src){
		this.src = src;
		this.xform = null;
	}

	public Stream(IFn xform, Stream src) {
        this.src = src.tap();
		this.xform = xform;
    }

	final public ISeq seq(){
		return sequence().seq();
	}

	final synchronized public ISeq sequence(){
		if(sequence == NO_SEQ)
			{
			tap();
			sequence = makeSequence(tap);
			}
		return sequence;
	}

	static ISeq makeSequence(final IFn tap){
		return new LazySeq(new AFn(){
				public Object invoke() throws Exception{
					Object v = tap.invoke();
					if(v == RT.EOS)
						return null;
					return new Cons(v, new LazySeq(this));
				}
			});
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
			final synchronized public Object invoke() throws Exception{
				if(xform == null)
					return src.invoke();
				Object v;
				Object xv;
				do {
					v = src.invoke();
					if(v == RT.EOS)
						return v;
					xv = xform.invoke(v);
				} while(xv == RT.SKIP);
				return xv;
			}
		};
	}


}
