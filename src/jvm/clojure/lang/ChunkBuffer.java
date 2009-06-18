/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich May 26, 2009 */

package clojure.lang;

final public class ChunkBuffer implements Counted{
	Object[] buffer;
	int end;

public ChunkBuffer(int capacity){
	buffer = new Object[capacity];
	end = 0;
}

public void add(Object o){
	buffer[end++] = o;
}

public IChunk chunk(){
	ArrayChunk ret = new ArrayChunk(buffer, 0, end);
	buffer = null;
	return ret;
}

public int count(){
	return end;
}
}
