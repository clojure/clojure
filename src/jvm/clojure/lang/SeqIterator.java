/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Jun 19, 2007 */

package clojure.lang;

import java.util.Iterator;
import java.util.NoSuchElementException;

public class SeqIterator implements Iterator{

static final Object START = new Object();
Object seq;
Object next;

public SeqIterator(Object o){
	seq = START;
	next = o;
}

//preserved for binary compatibility
public SeqIterator(ISeq o){
	seq = START;
	next = o;
}

public boolean hasNext(){
	if(seq == START){
		seq = null;
		next = RT.seq(next);
		}
	else if(seq == next)
		next = RT.next(seq);
	return next != null;
}

public Object next() throws NoSuchElementException {
	if(!hasNext())
		throw new NoSuchElementException();
	seq = next;
	return RT.first(next);
}

public void remove(){
throw new UnsupportedOperationException();
}
}
