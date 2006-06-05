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

package org.clojure.runtime;

import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReferenceArray;
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
 * Java implementation is lock-free
 */

public class PersistentArray implements Iterable{

public Iterator iterator(){
	return new ValIter(this);
}

static class Master{
	final AtomicReferenceArray array;
	final Object defaultVal;
    final AtomicInteger rev;
    final AtomicInteger load;
	final int maxLoad;
    final float loadFactor;

    Master(int size,Object defaultVal, float loadFactor){
		this.array = new AtomicReferenceArray(size);
		this.defaultVal = defaultVal;
        this.rev = new AtomicInteger(0);
        this.load = new AtomicInteger(0);
		this.maxLoad = (int) (size * loadFactor);
        this.loadFactor = loadFactor;
        }
}

static class Entry{
	final int rev;
	final Object val;
	final Entry rest;

	Entry(int rev,Object val,Entry rest){
		this.rev = rev;
		this.val = val;
		this.rest = rest;
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

final Master master;
final int rev;
final int baseline;
final BitSet history;

public PersistentArray(int size){
	this(size, null);
}

public PersistentArray(int size, Object defaultVal){
	this(size,defaultVal,2.1f);
}

public PersistentArray(int size, Object defaultVal, float loadFactor){
	this.master = new Master(size, defaultVal,loadFactor);
	this.rev = 0;
	this.baseline = 0;
	this.history = null;
}

PersistentArray(Master master,int rev,int baseline, BitSet history){
	this.master = master;
	this.rev = rev;
	this.baseline = baseline;
	this.history = history;
}



public int length(){
	return master.array.length();
}

public Object get(int i) {
    Entry e = getEntry(i);
    if(e != null)
        return e.val;
    return master.defaultVal;
}

public boolean has(int i){
    return getEntry(i) != null;
}

public PersistentArray resize(int newLength) {
    PersistentArray ret = new PersistentArray(newLength, master.defaultVal, master.loadFactor);
    int load = 0;
    for(int i=0;i< Math.min(length(),newLength);i++)
        {
        Entry e = getEntry(i);
        if(e != null)
            {
            ret.master.array.set(i,new Entry(0,e.val, null));
            ++load;
            }
        }

    ret.master.load.set(load);

    return ret;
}

/**
 *
 * @return number of values (of all revisions) stored in shared array
 */
public int load(){
    return master.load.get();
}

public PersistentArray isolate() {
    return resize(length());
}

Entry getEntry(int i){
	for(Entry e = (Entry) master.array.get(i);e != null;e = e.rest)
		{
		if(e.rev <= rev)
			{
			if(e.rev >= baseline
			   || (history != null && history.get(e.rev)))
				return e;
			}
		}
    return null;
}

public PersistentArray set(int i,Object val) {
	if(master.load.get() >= master.maxLoad)
		return isolate().set(i,val);
	PersistentArray ret = getSetArray();
	ret.doSet(i, val);
	return ret;
}

void doSet(int i, Object val){
	Entry oldEntry, newEntry;
	do
		{
		oldEntry = (Entry) master.array.get(i);
		newEntry = new Entry(rev, val, oldEntry);
		} while(!master.array.compareAndSet(i, oldEntry, newEntry));
    master.load.incrementAndGet();
}

PersistentArray getSetArray(){
	int nextRev;
	int nextBaseline;
	BitSet nextHistory;
	//is this a sequential update?
	if(master.rev.compareAndSet(rev, rev + 1))
		{
		nextRev = rev + 1;
		nextBaseline = baseline;
		nextHistory = history;
		}
	else //gap
		{
		nextRev = master.rev.incrementAndGet();
		nextBaseline = nextRev;
		if(history != null)
			nextHistory = (BitSet) history.clone();
		else
			nextHistory = new BitSet(rev+1);
		nextHistory.set(baseline,rev+1);
		}

	return new PersistentArray(master, nextRev, nextBaseline, nextHistory);
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
	for(int i = 0; i < writes; i++)
		{
		v.set(rand.nextInt(size), i);
		}
	for(int i = 0; i < reads; i++)
		{
		tv += (Integer)v.get(rand.nextInt(size));
		}
	System.out.println("PersistentArray");
	rand = new Random(42);
	long tp = 0;
	for(int i = 0; i < writes; i++)
		{
		p =	p.set(rand.nextInt(size), i);
		//dummy set to force perverse branching
		//p.set(i%size, i);
		}
	for(int i = 0; i < reads; i++)
		{
		tp += (Integer)p.get(rand.nextInt(size));
		}
	System.out.println("Done: " + tv + ", " + tp);


}
}
