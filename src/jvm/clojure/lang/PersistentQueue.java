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

import java.util.Queue;
import java.util.LinkedList;
import java.util.concurrent.ConcurrentLinkedQueue;

/**
 * conses onto rear, peeks/pops from front
 * See Okasaki's Batched Queues
 * This differs in that it uses a PersistentArrayList as the rear, which is in-order,
 * so no reversing or suspensions required for persistent use
 */

public class PersistentQueue extends Obj implements IPersistentList {

final public static PersistentQueue EMPTY = new PersistentQueue(null,null,null);

//*
final ISeq f;
final PersistentArrayList r;
static final int INITIAL_REAR_SIZE = 4;
int _hash = -1;

PersistentQueue(ISeq f, PersistentArrayList r, IPersistentMap meta) {
    this.f = f;
    this.r = r;
	this._meta = meta;
}

public boolean equals(Object obj) {

        if(!(obj instanceof Sequential))
            return false;
        ISeq ms  = ((IPersistentCollection)obj).seq();
        for(ISeq s = seq();s!=null;s = s.rest(), ms = ms.rest())
            {
            if(ms == null || !RT.equal(s.first(),ms.first()))
                return false;
            }
        return ms.rest() == null;

}

public int hashCode() {
    if(_hash == -1)
        {
        int hash = 0;
        for(ISeq s = seq();s!=null;s = s.rest())
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

public PersistentQueue pop() {
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

public PersistentQueue cons(Object o) {
    if(f == null)     //empty
        return new PersistentQueue(RT.list(o), null,_meta);
    else
        return new PersistentQueue(f,
                (PersistentArrayList) (r != null ? r : new PersistentArrayList(INITIAL_REAR_SIZE)).cons(o),_meta);
}

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

static class Seq extends ASeq {
    final ISeq f;
    final ISeq rseq;

    Seq(ISeq f, ISeq rseq) {
        this.f = f;
        this.rseq = rseq;
    }

    public Object first() {
        return f.first();
    }

    public ISeq rest() {
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

    public int count() {
        return RT.count(f) + RT.count(rseq);
    }

}

public static void main(String[] args) {
    if (args.length != 1)
        {
        System.err.println("Usage: PersistentQueue n");
        return;
        }
    int n = Integer.parseInt(args[0]);


    long startTime, estimatedTime;

    //*
    //Queue list = new LinkedList();
    Queue list = new ConcurrentLinkedQueue();
    System.out.println("Queue");
    startTime = System.nanoTime();
    for (int i = 0; i < n; i++)
        {
        list.add(i);
        list.add(i);
        list.remove();
        }
    for (int i = 0; i < n - 10; i++)
        {
        list.remove();
        }
    estimatedTime = System.nanoTime() - startTime;
    System.out.println("time: " + estimatedTime / 1000000);
    System.out.println("peek: " + list.peek());

//*/

//*
    PersistentQueue q = PersistentQueue.EMPTY;
    System.out.println("PersistentQueue");
    startTime = System.nanoTime();
    for (int i = 0; i < n; i++)
        {
        q = q.cons(i);
        q = q.cons(i);
        q = q.pop();
        }
    IPersistentList lastq = null;
    IPersistentList lastq2;
    for (int i = 0; i < n - 10; i++)
        {
        //lastq2 = lastq;
        //lastq = q;
        q = q.pop();
        }
    estimatedTime = System.nanoTime() - startTime;
    System.out.println("time: " + estimatedTime / 1000000);
    System.out.println("peek: " + q.peek());
    //*/

    IPersistentList q2 = q;
    for (int i = 0; i < 10; i++)
        {
        q2 = (IPersistentList) q2.cons(i);
        }
/*
    for(ISeq s = q.seq();s != null;s = s.rest())
        System.out.println("q: " + s.first().toString());
    for(ISeq s = q2.seq();s != null;s = s.rest())
        System.out.println("q2: " + s.first().toString());
//*/
}

}
