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
int _hash = -1;

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

override public bool Equals(Object obj) {
    if(obj is IPersistentArray)
        {
        IPersistentArray ma = (IPersistentArray) obj;
		if (ma.count() != count() || ma.GetHashCode() != GetHashCode())
            return false;
        for(int i=0;i<count();i++)
            {
            if(!RT.equal(nth(i),ma.nth(i)))
                return false;
            }
        }
    else
        {
        if(!(obj is Sequential))
            return false;
        ISeq ms  = ((IPersistentCollection)obj).seq();
		for (int i = 0; i < count(); i++, ms = ms.rest())
			{
			if (ms == null || !RT.equal(nth(i), ms.first()))
				return false;
			}
        if(ms.rest() != null)
            return false;
        }

    return true;
}

override public int GetHashCode() {
    if(_hash == -1)
        {
        int hash = 0;
        for(int i=0;i<count();i++)
            {
            hash = RT.hashCombine(hash, RT.hash(nth(i)));
            }
        this._hash = hash;
        }
    return _hash;
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
