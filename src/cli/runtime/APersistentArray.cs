/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

using System;
using System.Collections;

namespace clojure.lang
{
public abstract class APersistentArray : Obj, IPersistentArray {

public virtual IPersistentCollection cons(Object o) {
    PersistentArrayList ret = new PersistentArrayList(this, this.count() + 10);
    ret = (PersistentArrayList)ret.cons(o);
    ret._meta = _meta;
    return ret;
}

	public override Obj withMeta(IPersistentMap meta)
		{
		if(_meta == meta)
			return this;
		Obj ret = (Obj)MemberwiseClone();
		ret._meta = meta;
		return ret;
		}


	#region IArray Members

	abstract public int length();

	abstract public object nth(int i);

	abstract public IPersistentArray assocN(int i, object val);

	#endregion

	#region IPersistentCollection Members

	abstract public int count();

	abstract public ISeq seq();

	#endregion
	
public bool contains(Object key) {
     try{
		int i = Convert.ToInt32(key);
		return i >= 0 && i < count();
		}
	catch(Exception)
		{
		return false;
		}
}

public IMapEntry find(Object key) {
    try
        {
		int i = Convert.ToInt32(key);
        if(i >= 0 && i < count())
            return new MapEntry(key,nth(i));
        }
	catch(Exception)
		{
		}
    return null;
}

public Associative assoc(Object key, Object val) {
		int i = Convert.ToInt32(key);
		return (Associative)assocN(i, val);
}

public Object get(Object key) {
    try
        {
		int i = Convert.ToInt32(key);
        if(i >= 0 && i < count())
            return nth(i);
        }
	catch (Exception)
		{
		} 
	return null;
	}

}

}
