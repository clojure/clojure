/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Apr 1, 2008 */

package clojure.lang;

public class Range extends ASeq implements IReduce, Counted{
final int end;
final int n;

public Range(int start, int end){
	this.end = end;
	this.n = start;
}

public Range(IPersistentMap meta, int start, int end){
	super(meta);
	this.end = end;
	this.n = start;
}

public Obj withMeta(IPersistentMap meta){
	if(meta == meta())
		return this;
	return new Range(meta(), end, n);
}

public Object first(){
	return n;
}

public ISeq next(){
	if(n < end-1)
		return new Range(_meta, n + 1, end);
	return null;
}

public Object reduce(IFn f) {
	Object ret = n;
	for(int x = n+1;x < end;x++)
		ret = f.invoke(ret, x);
	return ret;
}

public Object reduce(IFn f, Object start) {
	Object ret = f.invoke(start,n);
	for(int x = n+1;x < end;x++)
		ret = f.invoke(ret, x);
	return ret;
}

public int count() {
    return end - n;
    }

}
