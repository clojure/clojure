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
public class PersistentArrayList : PersistentArray, IPersistentList{

int _count;

public PersistentArrayList(int initialCapacity) : base(initialCapacity){
    
    _count = 0;
}

PersistentArrayList(Master master,int rev,int baseline, BitArray history, int count):base(master,rev,baseline,history){
    
    this._count = count;
}

PersistentArrayList(int size, Object defaultVal, float loadFactor, int count):base(size, defaultVal, loadFactor) {
    
    this._count = count;
}

public PersistentArrayList(IPersistentArray init, int initialCapacity):base(init,initialCapacity){
    _count = Math.Min(init.count(),initialCapacity);
}

override public Object nth(int i) {
    if(i >= _count)
        throw new IndexOutOfRangeException();

    return base.nth(i);
}

override public IPersistentArray assocN(int i,Object val) {
    if(i >= _count)
		throw new IndexOutOfRangeException();

    return base.assocN(i, val);
}

override public int length(){
    return _count;
}

override public int count(){
    return _count;
}

override public IPersistentCollection cons(Object val) {
    if(_count == data.master.array.Length) //full
        {
        lock(data.master){
            if(_count == data.master.array.Length) //still full
                grow();
            }
        }
    PersistentArrayList ret =  (PersistentArrayList) base.assocN(_count, val);
    ret._count = _count + 1;
    return ret;
}

public Object peek(){
    if(_count > 0)
        return nth(_count - 1);
    return null;
}

public IPersistentList pop() {
    if(_count == 0)
        throw new InvalidOperationException();
    PersistentArrayList ret = new PersistentArrayList(data.master, data.rev, data.baseline, data.history, _count - 1);
	ret._meta = _meta;
	return ret;
}


private void grow() {
    //must be called inside lock of master
    if(data.master.next != null) //this master has been trimmed, but this rev is not yet propagated
        trim();

	Master newMaster = new Master(data.master.array.Length * 2, data.master.defaultVal, data.master.loadFactor, data.master.basis);
    newMaster.rev = data.master.rev;
    newMaster.load = data.master.load;
    for(int i=0;i<data.master.array.Length;i++)
        newMaster.array[i] = data.master.array[i];
	this.data = new Data(newMaster, data.rev, data.baseline, data.history);
}

override internal PersistentArray create(Master master,int rev,int baseline, BitArray history){
    PersistentArray ret = new PersistentArrayList(data.master, rev, baseline, history,_count);
    ret._meta = _meta;
    return ret;
    }

override internal PersistentArray create(int size, Object defaultVal, float loadFactor) {
    PersistentArray ret =  new PersistentArrayList(size, defaultVal, loadFactor,_count);
    ret._meta = _meta;
    return ret;
    }

}
}
