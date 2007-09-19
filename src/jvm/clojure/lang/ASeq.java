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

public abstract class ASeq extends Obj implements ISeq{
transient int _hash = -1;


protected ASeq(IPersistentMap meta){
	super(meta);
}


protected ASeq(){
}

public boolean equals(Object obj){

	if(!(obj instanceof Sequential))
		return false;
	ISeq ms = ((IPersistentCollection) obj).seq();
	for(ISeq s = seq(); s != null; s = s.rest(), ms = ms.rest())
		{
		if(ms == null || !RT.equal(s.first(), ms.first()))
			return false;
		}
	if(ms != null)
		return false;

	return true;
}

public int hashCode(){
	if(_hash == -1)
		{
		int hash = 0;
		for(ISeq s = seq(); s != null; s = s.rest())
			{
			hash = RT.hashCombine(hash, RT.hash(s.first()));
			}
		this._hash = hash;
		}
	return _hash;
}

//public Object peek(){
//	return first();
//}
//
//public IPersistentList pop(){
//	return rest();
//}

public int count(){
	return 1 + RT.count(rest());
}

public ISeq seq(){
	return this;
}

public ISeq cons(Object o){
	return new Cons(o, this);
}

}
