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

using System;
using System.Threading;
using System.Collections;

namespace clojure.lang
{
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

public class PersistentArray : IEnumerable, ISequential{

	#region IEnumerable Members

	public IEnumerator GetEnumerator()
		{
		return new ValIter(this);
		}

	#endregion

	public ISeq seq()
		{
		if (length() > 0)
			return new Seq(this, 0);
		return null;
		}
		
internal class Master{
	internal readonly Entry[] array;
	internal readonly Object defaultVal;
	internal int rev;
	internal int load;
	internal readonly int maxLoad;
	internal readonly float loadFactor;

	internal Master(int size,Object defaultVal, float loadFactor){
		this.array = new Entry[size];
		this.defaultVal = defaultVal;
		this.rev = 0;
		this.load = 0;
		this.maxLoad = (int)(size * loadFactor);
		this.loadFactor = loadFactor;
		}
}

internal class Entry
	{
	internal readonly int rev;
	internal readonly Object val;

	internal Entry(int rev, Object val)
		{
		this.rev = rev;
		this.val = val;
		}

	internal virtual Entry rest()
		{
		return null;
		}

	internal static Entry create(int rev, Object val, Entry rest)
		{
		if (rest == null)
			return new Entry(rev, val);
		return new EntryLink(rev, val, rest);
		}
	}

internal class EntryLink : Entry
	{
	internal readonly Entry _rest;

	internal EntryLink(int rev, Object val, Entry rest) :base(rev,val)
		{
		this._rest = rest;
		}
		
	override internal Entry rest(){
		return _rest;
	}
}

internal class Seq : ISeq{
	readonly PersistentArray p;
	readonly int i;

	internal Seq(PersistentArray p, int i){
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
}

internal class ValIter : IEnumerator
	{
	internal PersistentArray p;
	internal int i;

	internal ValIter(PersistentArray p)
		{
	this.p = p;
	this.i = -1;
}

#region IEnumerator Members

public object Current
	{
	get { return p.get(i); }
	}

public bool MoveNext()
	{
	++i;
	return i < p.length();
	}

public void Reset()
	{
	throw new Exception("The method or operation is not implemented.");
	}

#endregion
	}

	internal readonly Master master;
	internal readonly int rev;
	internal readonly int baseline;
	internal readonly BitArray history;

public PersistentArray(int size)
		: this(size, null)
		{
		}
		
public PersistentArray(int size, Object defaultVal)
	:this(size,defaultVal,2.1f)
	{
	}

public PersistentArray(int size, Object defaultVal, float loadFactor){
	this.master = new Master(size, defaultVal, loadFactor);
	this.rev = 0;
	this.baseline = 0;
	this.history = null;
}

	internal PersistentArray(Master master, int rev, int baseline, BitArray history)
		{
	this.master = master;
	this.rev = rev;
	this.baseline = baseline;
	this.history = history;
}



public int length(){
	return master.array.Length;
}

public Object get(int i){
	Entry e = getEntry(i);
		if(e != null)
				return e.val;
	return master.defaultVal;
}

public bool has(int i){
    return getEntry(i) != null;
}

public PersistentArray resize(int newLength)
	{
	PersistentArray ret = new PersistentArray(newLength, master.defaultVal, master.loadFactor);
	for (int i = 0; i < Math.Min(length(), newLength); i++)
		{
		Entry e = getEntry(i);
		if (e != null)
			{
			ret.master.array[i] = Entry.create(0, e.val, null);
			++ret.master.load;
			}
		}
	return ret;
	}

public int load(){
    return master.load;
}

public PersistentArray isolate()
	{
	return resize(length());
	}
	
Entry getEntry(int i){
	for(Entry e = (Entry) master.array[i];e != null;e = e.rest())
		{
		if(e.rev <= rev)
			{
			if(e.rev >= baseline
			   || (history != null && e.rev < history.Length && history.Get(e.rev)))
				return e;
			}
		}
	return null;
}

public PersistentArray set(int i,Object val) {
	if(master.load >= master.maxLoad)
		return isolate().set(i,val);
	lock(master){
		PersistentArray ret = getSetArray();
		ret.doSet(i, val);
		return ret;
	}
}

void doSet(int i, Object val){
	//must now be called inside lock of master
	master.array[i] = Entry.create(rev, val, master.array[i]);
	++master.load;
}

PersistentArray getSetArray(){
	//must now be called inside lock of master
	//is this a sequential update?
	if (master.rev == rev)
		{
		return new PersistentArray(master, ++master.rev, baseline, history);
		}
	else //gap
		{

		int nextRev = ++master.rev;
		BitArray nextHistory;
		if (history != null)
			{
			nextHistory = (BitArray) history.Clone();
			nextHistory.Length = rev+1;
			}
		else
			nextHistory = new BitArray(rev+1);
		for(int i=baseline;i<=rev;i++)
			nextHistory.Set(i,true);
		return new PersistentArray(master, nextRev, nextRev, nextHistory);
		}
}

/*
[STAThread] 
static public void Main(String[] args){
	if(args.Length != 3)
		{
		Console.Error.WriteLine("Usage: PersistentArray size writes reads");
		return;
		}
	int size = Int32.Parse(args[0]);
	int writes = Int32.Parse(args[1]);
	int reads = Int32.Parse(args[2]);
	ArrayList v = ArrayList.Synchronized(new ArrayList(size));
	//v.setSize(size);
	PersistentArray p = new PersistentArray(size);

	for(int i = 0; i < size; i++)
		{
		v.Add(0);
		p = p.set(i, 0);
		}

	Random rand;

	rand = new Random(42);
	long tv = 0;
	Console.WriteLine("ArrayList");
	DateTime start = DateTime.Now;
	for(int i = 0; i < writes; i++)
		{
		v[rand.Next(size)] = i;
		}
	for(int i = 0; i < reads; i++)
		{
		tv += (int)v[rand.Next(size)];
		}

	Console.WriteLine("Time: " + (DateTime.Now - start));
	
	Console.WriteLine("PersistentArray");
	rand = new Random(42);
	long tp = 0;
	start = DateTime.Now;
	for (int i = 0; i < writes; i++)
		{
		p =	p.set(rand.Next(size), i);
		//dummy set to force perverse branching
		//p.set(i%size, i);
		}
	for(int i = 0; i < reads; i++)
		{
		tp += (int)p.get(rand.Next(size));
		}
	Console.WriteLine("Time: " + (DateTime.Now - start));
	Console.WriteLine("Done: " + tv + ", " + tp);


}
 //*/


}

}
