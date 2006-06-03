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

namespace org.clojure.runtime
{
/**
 * Hybrid range/BitArray, multi-thread-safe
 * 
 * See Cohen for basic idea
 */

public class PersistentArray : IEnumerable{

	#region IEnumerable Members

	public IEnumerator GetEnumerator()
		{
		return new ValIter(this);
		}

	#endregion
	
internal class Master{
	internal readonly Entry[] array;
	internal readonly Object defaultVal;
	internal int rev;

	internal Master(int size, Object defaultVal)
		{
		this.array = new Entry[size];
		this.defaultVal = defaultVal;
		this.rev = 0;
		}
}

	internal class Entry
		{
		internal readonly int rev;
		internal readonly Object val;
		internal readonly Entry rest;

		internal Entry(int rev, Object val, Entry rest)
			{
			this.rev = rev;
			this.val = val;
			this.rest = rest;
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

public PersistentArray(int size,Object defaultVal){
	this.master = new Master(size, defaultVal);
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

	public PersistentArray(int size)
		: this(size, null)
		{
		}

public int length(){
	return master.array.Length;
}

public Object get(int i){
	for(Entry e = (Entry) master.array[i];e != null;e = e.rest)
		{
		if(e.rev <= rev)
			{
			if(e.rev >= baseline
			   || (history != null && e.rev < history.Length && history.Get(e.rev)))
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
	lock(master.array)
		{
		oldEntry = (Entry) master.array[i];
		newEntry = new Entry(rev, val, oldEntry);
		master.array[i] = newEntry;
		}
}

PersistentArray getSetArray(){
	int nextRev;
	int nextBaseline;
	BitArray nextHistory;
	//is this a sequential update?
	if(Interlocked.CompareExchange(ref master.rev,rev + 1,rev) == rev)
		{
		nextRev = rev + 1;
		nextBaseline = baseline;
		nextHistory = history;
		}
	else //gap
		{
		nextRev = Interlocked.Increment(ref master.rev);
		nextBaseline = nextRev;
		if(history != null)
			{
			nextHistory = (BitArray) history.Clone();
			nextHistory.Length = rev+1;
			}
		else
			nextHistory = new BitArray(rev+1);
		for(int i=baseline;i<=rev;i++)
			nextHistory.Set(i,true);
		}

	return new PersistentArray(master, nextRev, nextBaseline, nextHistory);
}

//*
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
	ArrayList v = new ArrayList(size);
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
	for(int i = 0; i < writes; i++)
		{
		v[rand.Next(size)] = i;
		}
	for(int i = 0; i < reads; i++)
		{
		tv += (int)v[rand.Next(size)];
		}
	Console.WriteLine("PersistentArray");
	rand = new Random(42);
	long tp = 0;
	for(int i = 0; i < writes; i++)
		{
		p =	p.set(rand.Next(size), i);
		//dummy set to force perverse branching
		p.set(i%size, i);
		}
	for(int i = 0; i < reads; i++)
		{
		tp += (int)p.get(rand.Next(size));
		}
	Console.WriteLine("Done: " + tv + ", " + tp);


}
 //*/


}

}
