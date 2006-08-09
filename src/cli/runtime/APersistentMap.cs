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
using System.Threading;
using System.Collections;

namespace clojure.lang
{
public abstract class APersistentMap : Obj, IPersistentMap{
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
    IPersistentMap m = obj as IPersistentMap;
    if(obj == null)
        return false;
        
    if(m.count() != count() || m.GetHashCode() != GetHashCode())
        return false;

    for(ISeq s = seq();s!=null;s = s.rest())
        {
        IMapEntry e = (IMapEntry) s.first();
        IMapEntry me = m.find(e.key());

        if(me == null || !RT.equal(e.val(),me.val()))
            return false;
        }

    return true;
}

override public int GetHashCode() {
    if(_hash == -1)
        {
        int hash = count();
        for(ISeq s = seq();s!=null;s = s.rest())
            {
            IMapEntry e = (IMapEntry) s.first();
            hash ^= RT.hashCombine(RT.hash(e.key()), RT.hash(e.val()));
            }
        this._hash = hash;
        }
    return _hash;
}
	#region IPersistentMap Members

	abstract public IPersistentMap assocEx(object key, object val);


	abstract public IPersistentMap without(object key);



	#endregion

	#region Associative Members

	abstract public bool contains(object key);

	abstract public IMapEntry find(object key);

	abstract public object get(object key);

	abstract public Associative assoc(object key, object val);

	#endregion

	#region IEnumerable Members

	abstract public IEnumerator GetEnumerator();

	#endregion

	#region IPersistentCollection Members

	abstract public int count();

	abstract public ISeq seq();

	public IPersistentCollection cons(Object o)
		{
		IMapEntry e = (IMapEntry)o;
		return (IPersistentCollection)assoc(e.key(), e.val());
		}
		
	#endregion
	}

}
