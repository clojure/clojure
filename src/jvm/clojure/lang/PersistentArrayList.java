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

import java.util.BitSet;

public class PersistentArrayList extends PersistentArray{

int _count;

public PersistentArrayList(int initialCapacity){
    super(initialCapacity);
    _count = 0;
}

PersistentArrayList(Master master,int rev,int baseline, BitSet history, int count){
    super(master,rev,baseline,history);
    this._count = count;
}

PersistentArrayList(int size, Object defaultVal, float loadFactor, int count) {
    super(size, defaultVal, loadFactor);
    this._count = count;
}

public Object nth(int i) {
    if(i >= _count)
        throw new IndexOutOfBoundsException();

    return super.nth(i);
}

public PersistentArrayList assocN(int i,Object val) {
    if(i >= _count)
        throw new IndexOutOfBoundsException();

    return (PersistentArrayList) super.assocN(i, val);
}

public int length(){
    return _count;
}

public int count(){
    return _count;
}

public int capacity(){
	return data.master.array.length;
}

public PersistentArrayList add(Object val) {
    if(_count == data.master.array.length) //full
        {
        synchronized(data.master){
            if(_count == data.master.array.length) //still full
                grow();
            }
        }
    PersistentArrayList ret =  (PersistentArrayList) super.assocN(_count, val);
    ret._count = _count + 1;
    return ret;
}

public PersistentArrayList remove() {
    if(_count == 0)
        throw new IllegalAccessError();
    PersistentArrayList ret = new PersistentArrayList(data.master, data.rev, data.baseline, data.history, _count - 1);
	ret._meta = _meta;
	return ret;
}


private void grow() {
    //must be called inside lock of master
    if(data.master.next != null) //this master has been trimmed, but this rev is not yet propagated
        trim();

    Master newMaster = new Master(data.master.array.length * 2, data.master.defaultVal, data.master.loadFactor);
    newMaster.rev = data.master.rev;
    newMaster.load = data.master.load;
    newMaster.basis = data.master.basis;
    for(int i=0;i<data.master.array.length;i++)
        newMaster.array[i] = data.master.array[i];
    data.master = newMaster;
}

protected PersistentArray create(Master master,int rev,int baseline, BitSet history){
    PersistentArray ret = new PersistentArrayList(data.master, rev, baseline, history,_count);
    ret._meta = _meta;
    return ret;
}

protected PersistentArray create(int size, Object defaultVal, float loadFactor) {
    PersistentArray ret =  new PersistentArrayList(size, defaultVal, loadFactor,_count);
    ret._meta = _meta;
    return ret;
}

}
