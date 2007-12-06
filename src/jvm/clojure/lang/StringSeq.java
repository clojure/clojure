/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Dec 6, 2007 */

package clojure.lang;

public class StringSeq extends ASeq implements IndexedSeq{
final String s;
final int i;

static public StringSeq create(String s){
	if(s.length() == 0)
		return null;
	return new StringSeq(null, s, 0);
}

StringSeq(IPersistentMap meta, String s, int i){
	super(meta);
	this.s = s;
	this.i = i;
}

public Obj withMeta(IPersistentMap meta){
	if(meta == meta())
		return this;
	return new StringSeq(meta, s, i);
}

public Object first(){
	return Character.valueOf(s.charAt(i));
}

public ISeq rest(){
	if(i + 1 < s.length())
		return new StringSeq(_meta, s, i + 1);
	return null;
}

public int index(){
	return i;
}
}
