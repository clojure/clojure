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

namespace clojure.lang
	{
	
public abstract class ASeq : Obj, ISeq{
int _hash = -1;
	
	public override Obj withMeta(IPersistentMap meta)
		{
		if(_meta == meta)
			return this;
		Obj ret = (Obj)MemberwiseClone();
		ret._meta = meta;
		return ret;
		}

override public bool Equals(Object obj) {
    if(!(obj is Sequential))
        return false;
    for(ISeq s = seq(), ms = ((IPersistentCollection)obj).seq();s!=null;s = s.rest(), ms = ms.rest())
        {
        if(ms == null || !RT.equal(s.first(),ms.first()))
            return false;
        }

    return true;
}

override public int GetHashCode() {
    if(_hash == -1)
        {
        int hash = 0;
        for(ISeq s = seq();s!=null;s = s.rest())
            {
            hash = RT.hashCombine(hash, RT.hash(s.first()));
            }
        this._hash = hash;
        }
    return _hash;
}
		
public virtual Object peek() {
    return first();
}

public virtual IPersistentList pop() {
    return rest();
}

public virtual int count() {
    return 1 + RT.count(rest());
}

public virtual ISeq seq() {
    return this;
}

public virtual IPersistentCollection cons(Object o) {
    return new Cons(o, this);
}

#region ISeq Members

abstract public object first();

abstract public ISeq rest();

#endregion
	}

}
