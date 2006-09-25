/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

package clojure.lang;

import java.util.Iterator;

public class IteratorSeq extends ASeq{
Iterator iter;
Object val;
ISeq _rest;

public static IteratorSeq create(Iterator iter){
	if(iter.hasNext())
		return new IteratorSeq(iter);
	return null;
}

IteratorSeq(Iterator iter){
	this.iter = iter;
	this.val = this;
	this._rest = this;
}

public Object first(){
	synchronized(this)
		{
		if(val == this)
			val = iter.next();
		return val;
		}
}

public ISeq rest(){
	synchronized(this)
		{
		if(_rest == this)
			{
			first();
			_rest = create(iter);
			}
		return _rest;
		}
}
}
