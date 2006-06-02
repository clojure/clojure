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
 * Hybrid range/bitset, multi-thread-safe
 * 
 * See Cohen for basic idea
 */

public class PersistentArray implements Iterable{

public Iterator iterator(){
	return new ValIter(this);
}

static class Master{
	final AtomicReferenceArray array;
	final Object defaultVal;
	final AtomicInteger rev;

	Master(int size,Object defaultVal){
		this.array = new AtomicReferenceArray(size);
		this.defaultVal = defaultVal;
		this.rev = new AtomicInteger(0);
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

public PersistentArray(int size,Object defaultVal){
	this.master = new Master(size, defaultVal);
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

public PersistentArray(int size){
	this(size, null);
}

public int length(){
	return master.array.length();
}

public Object get(int i){
	for(Entry e = (Entry) master.array.get(i);e != null;e = e.rest)
		{
		if(e.rev <= rev)
			{
			if(e.rev >= baseline
			   || (history != null && history.get(e.rev)))
				return e.val;
			}
		}
	return master.defaultVal;
}

public PersistentArray set(int i,Object val) {
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
			nextHistory = new BitSet(rev);
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
		p.set(i%size, i);
		}
	for(int i = 0; i < reads; i++)
		{
		tp += (Integer)p.get(rand.nextInt(size));
		}
	System.out.println("Done: " + tv + ", " + tp);


}
}
