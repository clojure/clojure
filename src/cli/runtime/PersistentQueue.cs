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
	
/**
 * conses onto rear, peeks/pops from front
 * See Okasaki's Batched Queues
 * This differs in that it uses a PersistentArrayList as the rear, which is in-order,
 * so no reversing or suspensions required for persistent use
 */

public class PersistentQueue : Obj, IPersistentList {

readonly public static PersistentQueue EMPTY = new PersistentQueue(null,null,null);

readonly ISeq f;
readonly PersistentArrayList r;
static readonly int INITIAL_REAR_SIZE = 4;
int _hash = -1;


PersistentQueue(ISeq f, PersistentArrayList r, IPersistentMap meta) {
    this.f = f;
    this.r = r;
	this._meta = meta;
}

override public bool Equals(Object obj)
	{
	if (!(obj is Sequential))
		return false;
	ISeq ms = ((IPersistentCollection)obj).seq();
	for (ISeq s = seq(); s != null; s = s.rest(), ms = ms.rest())
		{
		if (ms == null || !RT.equal(s.first(), ms.first()))
			return false;
		}
	return ms.rest() == null;
	}

override public int GetHashCode()
	{
	if (_hash == -1)
		{
		int hash = 0;
		for (ISeq s = seq(); s != null; s = s.rest())
			{
			hash = RT.hashCombine(hash, RT.hash(s.first()));
			}
		this._hash = hash;
		}
	return _hash;
	}
	
public Object peek() {
    return RT.first(f);
}

public IPersistentList pop() {
    if(f == null)  //hmmm... pop of empty queue -> empty queue?
        return this;
        //throw new IllegalStateException("popping empty queue");
    ISeq f1 = f.rest();
    PersistentArrayList r1 = r;
    if(f1 == null)
        {
        f1 = RT.seq(r);
        r1 = null;
        }
    return new PersistentQueue(f1, r1,_meta);
}

public int count() {
    return RT.count(f) + RT.count(r);
}

public ISeq seq() {
    if(f == null)
        return null;
    return new Seq(f, RT.seq(r));
}

public IPersistentCollection cons(Object o) {
    if(f == null)     //empty
        return new PersistentQueue(RT.list(o), null,_meta);
    else
        return new PersistentQueue(f,
                (PersistentArrayList) (r != null ? r : new PersistentArrayList(INITIAL_REAR_SIZE)).cons(o),
        		_meta);
}

public override Obj withMeta(IPersistentMap meta)
	{
	if(_meta == meta)
		return this;
	Obj ret = (Obj)MemberwiseClone();
	ret._meta = meta;
	return ret;
	}

class Seq : ASeq {
    readonly ISeq f;
    readonly ISeq rseq;

    internal Seq(ISeq f, ISeq rseq) {
        this.f = f;
        this.rseq = rseq;
    }

    public override Object first() {
        return f.first();
    }

    public override ISeq rest() {
        ISeq f1 = f.rest();
        ISeq r1 = rseq;
        if (f1 == null)
            {
            if (rseq == null)
                return null;
            f1 = rseq;
            r1 = null;
            }
        return new Seq(f1, r1);
    }
}

//*
public static void Main(String[] args) {
    if (args.Length != 1)
        {
        Console.Error.WriteLine("Usage: PersistentQueue n");
        return;
        }
	int n = Int32.Parse(args[0]);


    Random rand;

    rand = new Random(42);

    DateTime startTime;
    TimeSpan estimatedTime;

    //Queue list = new LinkedList();
    Queue list = Queue.Synchronized(new Queue());
    Console.WriteLine("Queue");
    startTime = DateTime.Now;
    for (int i = 0; i < n; i++)
        {
        list.Enqueue(i);
		list.Enqueue(i);
        list.Dequeue();
        }
    for (int i = 0; i < n - 10; i++)
        {
        list.Dequeue();
        }
	estimatedTime = DateTime.Now - startTime;
    Console.WriteLine("time: " + estimatedTime.Ticks / 10000);
    Console.WriteLine("peek: " + list.Peek());


    PersistentQueue q = PersistentQueue.EMPTY;
    Console.WriteLine("PersistentQueue");
	startTime = DateTime.Now;
	for (int i = 0; i < n; i++)
        {
        q = (PersistentQueue) q.cons(i);
        q = (PersistentQueue) q.cons(i);
        q = (PersistentQueue) q.pop();
        }
    IPersistentList lastq = null;
    IPersistentList lastq2;
    for (int i = 0; i < n - 10; i++)
        {
        //lastq2 = lastq;
        //lastq = q;
        q = (PersistentQueue) q.pop();
        }
	estimatedTime = DateTime.Now - startTime;
	Console.WriteLine("time: " + estimatedTime.Ticks / 10000);
    Console.WriteLine("peek: " + q.peek());

    IPersistentList q2 = q;
    for (int i = 0; i < 10; i++)
        {
        q2 = (IPersistentList) q2.cons(i);
        }
//    for(ISeq s = q.seq();s != null;s = s.rest())
//        Console.WriteLine("q: " + s.first());
//    for(ISeq s = q2.seq();s != null;s = s.rest())
//        Console.WriteLine("q2: " + s.first());
}
//*/

}
		
		}
