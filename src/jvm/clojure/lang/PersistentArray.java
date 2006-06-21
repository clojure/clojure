/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Jun 2, 2006 */

package clojure.lang;

//import java.util.concurrent.atomic.AtomicInteger;
//import java.util.concurrent.atomic.AtomicReferenceArray;
import java.util.BitSet;
import java.util.Iterator;
import java.util.Vector;
import java.util.Random;

/**
 * Note that instances of this class are constant values
 * i.e. set() returns a new array, old one is intact
 *
 * Multiple revisions (thread-safely) share the same master array
 *
 * Constant time most-recent-revision lookups
 * Amortized constant-time sequential revisions (when loadFactor > 1)
 * where a sequential revision is a revision of the most recent revision
 *
 * Non-sequential revisions are O(length), but with a small constant multiplier of 1/32
 * Worst-case O(r) lookups for oldest revs where r is number of revisions
 * at index i since last (automatic or manual) isolate. If set()s are roughly evenly
 * distributed, r should be approximately == loadFactor, i.e. constant
 * In pathological case (all mods to same index), r == (loadFactor * length)
 *
 * (loadFactor * length) old values are retained, even if the array revisions aren't
 * Default loadFactor is 2.1
 * When the load exceeds (loadFactor * length) the next revision is automatically isolated
 * You can determine how many values are in the shared master by calling load()
 * and can trim them by calling isolate() or resize(), which yield a new array with no
 * sharing and no old values
 *
 * See Cohen for basic idea
 * I added hybrid most-recent-sequential-range + shared-bitset idea, multi-thread-safety
 */

public class PersistentArray implements Iterable, IArray {

public Iterator iterator(){
	return new ValIter(this);
}

public ISeq seq() {
    if(length() > 0)
        return new Seq(this, 0);
    return null;
}

static class Master{
    final Entry[] array;
    final Object defaultVal;
    int rev;
    int load;
    final int maxLoad;
    final float loadFactor;
    final int[] basis;
    volatile Master next;

    Master(int size,Object defaultVal, float loadFactor){
        this.array = new Entry[size];//new AtomicReferenceArray(size);
        this.defaultVal = defaultVal;
        this.rev = 0;//new AtomicInteger(0);
        this.load = 0;//new AtomicInteger(0);
        this.maxLoad = (int) (size * loadFactor);
        this.loadFactor = loadFactor;
        basis = null;
        next = null;
        }

    Master(Master parent){
        this.array = new Entry[parent.array.length];
        this.defaultVal = parent.defaultVal;
        this.rev = 0;
        this.load = 0;
        this.maxLoad = parent.maxLoad;
        this.loadFactor = parent.loadFactor;
        this.next = null;

        this.basis = new int[parent.array.length];
        }
}

static class Entry{
	final int rev;
	final Object val;

	Entry(int rev,Object val){
		this.rev = rev;
		this.val = val;
		}

    Entry rest(){
        return null;
    }

    static Entry create(int rev,Object val,Entry rest){
        if(rest == null)
            return new Entry(rev,val);
        return new EntryLink(rev, val, rest);
    }
}

static class EntryLink extends Entry{
	final Entry _rest;

	EntryLink(int rev,Object val,Entry rest){
        super(rev,val);
		this._rest = rest;
		}

    Entry rest(){
        return _rest;
    }
}

static class Seq implements IndexedSeq{
	final PersistentArray p;
	final int i;

	Seq(PersistentArray p, int i){
		this.p = p;
		this.i = i;
	}

    public Object first() {
        return p.get(i);
    }

    public ISeq rest() {
        if(i+1 < p.length())
            return new Seq(p, i + 1);
        return null;
    }

    public int index() {
        return i;
    }
}

static class ValIter implements Iterator{
	PersistentArray p;
	int i;

	ValIter(PersistentArray p){
		this.p = p;
		this.i = 0;
	}

	public boolean hasNext(){
		return i < p.length();
	}

	public Object next(){
		return p.get(i++);
	}

	public void remove(){
		throw new UnsupportedOperationException();
	}
}

static class Data{
final Master master;
final int rev;
final int baseline;
final BitSet history;

    public Data(Master master, int rev, int baseline, BitSet history) {
        this.master = master;
        this.rev = rev;
        this.baseline = baseline;
        this.history = history;
    }
}

volatile Data data;

public PersistentArray(int size){
	this(size, (Object)null);
}

public PersistentArray(int size, Object defaultVal){
	this(size,defaultVal,2.1f);
}

public PersistentArray(int size, Object defaultVal, float loadFactor){
	this.data = new Data(new Master(size, defaultVal,loadFactor),0,0,null);
}

PersistentArray(Master master,int rev,int baseline, BitSet history){
    this.data = new Data(master,rev,baseline,history);
}

public PersistentArray(int size, ISeq seq) throws Exception {
    this(size);
    int load = 0;
    for(int i=0;seq != null && i < size;i++, seq=seq.rest())
        {
        data.master.array[i] = new Entry(0,seq.first());
        ++load;
        }

    data.master.load = load;
}

public PersistentArray(IArray init)  {
    this(init.length());
    int load = 0;
    for(int i=0;i < init.length();i++)
        {
        data.master.array[i] = new Entry(0,init.get(i));
        ++load;
        }

    data.master.load = load;
}

final public int length(){
	return data.master.array.length;
}

final public Object get(int i) {
    Entry e = getEntry(i);
    if(e != null)
        return e.val;
    return data.master.defaultVal;
}

final public boolean has(int i){
    return getEntry(i) != null;
}

final public PersistentArray resize(int newLength) {
    PersistentArray ret = new PersistentArray(newLength, data.master.defaultVal, data.master.loadFactor);
    int load = 0;
    for(int i=0;i< Math.min(length(),newLength);i++)
        {
        Entry e = getEntry(i);
        if(e != null)
            {
            ret.data.master.array[i] = new Entry(0,e.val);
            ++load;
            }
        }

	ret.data.master.load = load;

    return ret;
}

/**
 *
 * @return number of values (of all revisions) stored in shared array
 */
final public int load(){
	return data.master.load;
}

final public void isolate() {
    synchronized(data.master)
        {
        Master nextMaster = new Master(data.master);
        int load = 0;
        for(int i=0;i<length();i++)
            {
            Entry entry = getEntry(i);
            if(entry != null)
                {
                nextMaster.array[i] = new Entry(0,entry.val);
                ++load;
                }
            }
        nextMaster.load = load;
        this.data = new Data(nextMaster, 0, 0, null);
        }
}

private Entry getEntry(int i){
	for(Entry e = data.master.array[i];e != null;e = e.rest())
		{
		if(e.rev <= data.rev)
			{
			if(e.rev >= data.baseline
			   || (data.history != null && data.history.get(e.rev)))
				return e;
			}
		}
    return null;
}

final public PersistentArray set(int i,Object val) {
    synchronized(data.master){
        if(data.master.load >= data.master.maxLoad)
            trim();
		PersistentArray ret = getSetArray();
		ret.doSet(i, val);
		return ret;
	}
}

private void trim(){
    //must be called inside lock of master
    if(data.master.next == null) //this master has never been trimmed
        {
        Master nextMaster = new Master(data.master);
        int load = 0;
        for(int i=0;i<length();i++)
            {
            Entry entry = getEntry(i);
            if(entry != null)
                {
                nextMaster.array[i] = new Entry(0,entry.val);
                nextMaster.basis[i] = entry.rev;
                ++load;
                }
            }
        nextMaster.load = load;
        Data nextData = new Data(nextMaster, 0, 0, null);
        data.master.next = nextMaster;
        data = nextData;
        }
    else //this master has been trimmed, but this rev is not yet propagated
        {
        Master nextMaster = data.master.next;
        int diff = 0;
        for(int i=0;i<length()/2;i++)
            {
            Entry e = getEntry(i);
            if(e != null && e.rev != nextMaster.basis[i])
                ++diff;
            }
        if(diff >= length()/2 || nextMaster.load + diff > nextMaster.maxLoad)
            isolate();
        else
            {
            Data nextData;
            synchronized(nextMaster){
                int rev = ++nextMaster.rev;
                for(int i=0;i<length();i++)
                    {
                    Entry e = getEntry(i);
                    if(e != null && e.rev != nextMaster.basis[i])
                        {
                        nextMaster.array[i] = Entry.create(rev, e.val, nextMaster.array[i]);
                        ++nextMaster.load;
                        }
                    }
                BitSet history = new BitSet(rev);
                history.set(0);
                nextData = new Data(nextMaster,rev,rev,history);
                }
            this.data = nextData;
            }
        }
}

public boolean equals(Object key){
    if(this == key) return true;
    if(key == null || !(key instanceof IArray)) return false;

    final IArray a = (IArray) key;

    if(a.length() != length())
        return false;

    for(int i = 0; i < length(); i++)
        {
        if(!equalKey(get(i),a.get(i)))
            return false;
        }

    return true;
}

public int hashCode(){
	int ret = 0;
	for(int i = 0; i < length(); i++)
		{
		Object o = get(i);
		if(o != null)
			ret ^= o.hashCode();
		}
	return ret;
}

private boolean equalKey(Object k1,Object k2){
    if(k1 == null)
        return k2 == null;
    return k1.equals(k2);
}

private  void doSet(int i, Object val){
	//must be called inside lock of master
	data.master.array[i] = Entry.create(data.rev, val, data.master.array[i]);
	++data.master.load;
}

private PersistentArray getSetArray(){
	//must be called inside lock of master
	//is this a sequential update?
	if(data.master.rev == data.rev)
		{
		return new PersistentArray(data.master, ++data.master.rev, data.baseline, data.history);
		}
	else //gap
		{
		int nextRev = ++data.master.rev;
		BitSet nextHistory;
		if(data.history != null)
			nextHistory = (BitSet) data.history.clone();
		else
			nextHistory = new BitSet(data.rev+1);
		nextHistory.set(data.baseline,data.rev+1);
		return new PersistentArray(data.master, nextRev, nextRev, nextHistory);
		}

}


static public void main(String[] args){
	if(args.length != 3)
		{
		System.err.println("Usage: PersistentArray size writes reads");
		return;
		}
	int size = Integer.parseInt(args[0]);
	int writes = Integer.parseInt(args[1]);
	int reads = Integer.parseInt(args[2]);
	Vector v = new Vector(size);
	v.setSize(size);
	PersistentArray p = new PersistentArray(size);

	for(int i = 0; i < size; i++)
		{
		v.set(i, 0);
		p = p.set(i, 0);
		}

	Random rand;

	rand = new Random(42);
	long tv = 0;
	System.out.println("Vector");
	long startTime = System.nanoTime();
	for(int i = 0; i < writes; i++)
		{
		v.set(rand.nextInt(size), i);
		}
	for(int i = 0; i < reads; i++)
		{
		tv += (Integer)v.get(rand.nextInt(size));
		}
	long estimatedTime = System.nanoTime() - startTime;
	System.out.println("time: " + estimatedTime/1000000);
	System.out.println("PersistentArray");
	rand = new Random(42);
	startTime = System.nanoTime();
	long tp = 0;

    PersistentArray oldp = p;

    for(int i = 0; i < writes; i++)
		{
		p =	p.set(rand.nextInt(size), i);
		//dummy set to force perverse branching
        //oldp =	oldp.set(rand.nextInt(size), i);
		}
	for(int i = 0; i < reads; i++)
		{
		tp += (Integer)p.get(rand.nextInt(size));
		}
	estimatedTime = System.nanoTime() - startTime;
	System.out.println("time: " + estimatedTime/1000000);
	System.out.println("Done: " + tv + ", " + tp);


}
}
