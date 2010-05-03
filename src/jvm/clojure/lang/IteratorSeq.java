/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

package clojure.lang;

import java.io.IOException;
import java.io.NotSerializableException;
import java.util.Iterator;

public class IteratorSeq extends ASeq{
final Iterator iter;
final State state;

    static class State{
	volatile Object val;
	volatile Object _rest;
}

public static IteratorSeq create(Iterator iter){
	if(iter.hasNext())
		return new IteratorSeq(iter);
	return null;
}

IteratorSeq(Iterator iter){
	this.iter = iter;
	state = new State();
	this.state.val = state;
	this.state._rest = state;
}

IteratorSeq(IPersistentMap meta, Iterator iter, State state){
	super(meta);
	this.iter = iter;
	this.state = state;
}

public Object first(){
	if(state.val == state)
		synchronized(state)
			{
			if(state.val == state)
				state.val = iter.next();
			}
	return state.val;
}

public ISeq next(){
	if(state._rest == state)
		synchronized(state)
			{
			if(state._rest == state)
				{
				first();
				state._rest = create(iter);
				}
			}
	return (ISeq) state._rest;
}

public IteratorSeq withMeta(IPersistentMap meta){
	return new IteratorSeq(meta, iter, state);
}

private void writeObject (java.io.ObjectOutputStream out) throws IOException {
    throw new NotSerializableException(getClass().getName());
}
}
