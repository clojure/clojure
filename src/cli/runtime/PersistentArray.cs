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

	public class PersistentArray : AnArray, IEnumerable
		{

	#region IEnumerable Members

	public IEnumerator GetEnumerator()
		{
		return new ValIter(this);
		}

	#endregion

	override public ISeq seq()
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
    internal int[] basis;
    internal volatile Master next;
    
	internal Master(int size,Object defaultVal, float loadFactor){
		this.array = new Entry[size];
		this.defaultVal = defaultVal;
		this.rev = 0;
		this.load = 0;
		this.maxLoad = (int)(size * loadFactor);
		this.loadFactor = loadFactor;
		}
	internal Master(Master parent)
		{
		this.array = new Entry[parent.array.Length];
		this.defaultVal = parent.defaultVal;
		this.rev = 0;
		this.load = 0;
		this.maxLoad = parent.maxLoad;
		this.loadFactor = parent.loadFactor;
		this.next = null;

		this.basis = new int[parent.array.Length];
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

internal class Seq : IndexedSeq{
	readonly PersistentArray p;
	readonly int i;

	internal Seq(PersistentArray p, int i){
		this.p = p;
		this.i = i;
	}

    public Object first() {
        return p.nth(i);
    }

    public ISeq rest() {
        if(i+1 < p.length())
            return new Seq(p, i + 1);
        return null;
    }

#region IndexedSeq Members

public int index()
	{
	return i;
	}

#endregion
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
	get { return p.nth(i); }
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

internal class Data{
	internal Master master;
	internal readonly int rev;
	internal readonly int baseline;
	internal readonly BitArray history;
	public Data(Master master, int rev, int baseline, BitArray history)
		{
		this.master = master;
		this.rev = rev;
		this.baseline = baseline;
		this.history = history;
		}	
	}

internal volatile Data data;

public PersistentArray(int size)
		: this(size, (Object)null)
		{
		}
		
public PersistentArray(int size, Object defaultVal)
	:this(size,defaultVal,2.1f)
	{
	}

public PersistentArray(int size, Object defaultVal, float loadFactor){
	this.data = new Data(new Master(size, defaultVal, loadFactor), 0, 0, null);
}

	internal PersistentArray(Master master, int rev, int baseline, BitArray history)
		{
		this.data = new Data(master, rev, baseline, history);
}

public PersistentArray(int size, ISeq seq) : this(size){
    int load = 0;
    for(int i=0;seq != null && i < size;i++, seq=seq.rest())
        {
        data.master.array[i] = new Entry(0,seq.first());
        ++load;
        }

	data.master.load = load;
}

public PersistentArray(IArray init) :this(init.length()) {
    int load = 0;
    for(int i=0;i < init.length();i++)
        {
		data.master.array[i] = new Entry(0, init.nth(i));
        ++load;
        }

	data.master.load = load;
}

override public int count(){
return data.master.array.Length;
}

override public int length(){
return data.master.array.Length;
}

override public Object nth(int i){
	Entry e = getEntry(i);
		if(e != null)
				return e.val;
			return data.master.defaultVal;
}

public bool has(int i){
    return getEntry(i) != null;
}

public PersistentArray resize(int newLength)
	{
	PersistentArray ret = create(newLength, data.master.defaultVal, data.master.loadFactor);
	for (int i = 0; i < Math.Min(length(), newLength); i++)
		{
		Entry e = getEntry(i);
		if (e != null)
			{
			ret.data.master.array[i] = Entry.create(0, e.val, null);
			++ret.data.master.load;
			}
		}
	return ret;
	}

public int load(){
return data.master.load;
}

public void isolate()
	{
    lock(data.master)
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
	
Entry getEntry(int i){
for (Entry e = (Entry)data.master.array[i]; e != null; e = e.rest())
		{
		if (e.rev <= data.rev)
			{
			if (e.rev >= data.baseline
			   || (data.history != null && e.rev < data.history.Length && data.history.Get(e.rev)))
				return e;
			}
		}
	return null;
}

override public IArray assocN(int i,Object val) {
//if (data.master.load >= data.master.maxLoad)
//		{
//		isolate();
//		//set(i,val);
//		}
	lock (data.master)
		{
		if (data.master.load >= data.master.maxLoad)
			//isolate();
			trim();
		PersistentArray ret = getSetArray();
		ret.doSet(i, val);
		return ret;
	}
}

protected void trim(){
    //must be called inside lock of master
	if (data.master.next == null) //this master has never been trimmed
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
        for(int i=0;i<length();i++)
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
            lock(nextMaster){
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
                BitArray history = new BitArray(rev);
                history.Set(0,true);
                nextData = new Data(nextMaster,rev,rev,history);
                }
            this.data = nextData;
            }
        }
}

override public bool Equals(Object key){
    if(this == key) return true;
    if(key == null || !(key is IArray)) return false;

    IArray a = (IArray) key;

    if(a.length() != length())
        return false;

    for(int i = 0; i < length(); i++)
        {
        if(!equalKey(nth(i),a.nth(i)))
            return false;
        }

    return true;
}

override public int GetHashCode()
	{
	int ret = 0;
	for (int i = 0; i < length(); i++)
		{
		Object o = nth(i);
		if (o != null)
			ret ^= o.GetHashCode();
		}
	return ret;
	}

private bool equalKey(Object k1, Object k2)
	{
	if (k1 == null)
		return k2 == null;
	return k1.Equals(k2);
	}
	
void doSet(int i, Object val){
	//must now be called inside lock of master
data.master.array[i] = Entry.create(data.rev, val, data.master.array[i]);
++data.master.load;
}

PersistentArray getSetArray(){
	//must now be called inside lock of master
	//is this a sequential update?
if (data.master.rev == data.rev)
		{
		return create(data.master, ++data.master.rev, data.baseline, data.history);
		}
	else //gap
		{

		int nextRev = ++data.master.rev;
		BitArray nextHistory;
		if (data.history != null)
			{
			nextHistory = (BitArray)data.history.Clone();
			nextHistory.Length = data.rev + 1;
			}
		else
			nextHistory = new BitArray(data.rev + 1);
		for (int i = data.baseline; i <= data.rev; i++)
			nextHistory.Set(i,true);
		return create(data.master, nextRev, nextRev, nextHistory);
		}
}

internal  virtual PersistentArray create(Master master, int rev, int baseline, BitArray history)
	{
	PersistentArray ret = new PersistentArray(data.master, rev, baseline, history);
	ret._meta = _meta;
	return ret;
	}

internal virtual PersistentArray create(int size, Object defaultVal, float loadFactor)
	{
	PersistentArray ret = new PersistentArray(size, defaultVal, loadFactor);
	ret._meta = _meta;
	return ret;
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
	ArrayList v = ArrayList.Synchronized(new ArrayList(size));
	//v.setSize(size);
	//IArray p = new PersistentArray(size);
	IArray p = new PersistentArrayList(size);

	for(int i = 0; i < size; i++)
		{
		v.Add(0);
		//p = p.set(i, 0);
		p = ((PersistentArrayList)p).add(0);
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
	IArray oldp = p;
	for (int i = 0; i < writes; i++)
		{
		p =	p.assocN(rand.Next(size), i);
		//dummy set to force perverse branching
		oldp = oldp.assocN(i%size, i);
		//p.set(i%size, i);
		}
	for(int i = 0; i < reads; i++)
		{
		tp += (int)p.nth(rand.Next(size));
		}
	Console.WriteLine("Time: " + (DateTime.Now - start));
	Console.WriteLine("Done: " + tv + ", " + tp);


}
 //*/


}

}
