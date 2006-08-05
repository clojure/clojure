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

public abstract class APersistentMap extends Obj implements IPersistentMap, Cloneable{
int _hash = -1;

public Obj withMeta(IPersistentMap meta) {
    if(_meta == meta)
        return this;
    try{
    Obj ret = (Obj) clone();
    ret._meta = meta;
    return ret;
    }
    catch(CloneNotSupportedException ignore)
        {
        return null;
        }
}

public IPersistentCollection cons(Object o) {
    IMapEntry e = (IMapEntry)o;
    return assoc(e.key(), e.val());
}

public boolean equals(Object obj) {
    if(!(obj instanceof IPersistentMap))
        return false;
    IPersistentMap m = (IPersistentMap)obj;

    if(m.count() != count())
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

public int hashCode() {
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

}
