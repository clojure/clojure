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

import java.util.Random;
import java.util.LinkedList;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;

/**
 * conses onto rear, peeks/pops from front
 * See Okasaki's Bootstrapped Queue
 */

public class PersistentQueue implements IPersistentList {

static class Empty implements IPersistentList{

    public Object peek() {
        return null;
    }

    public IPersistentList pop() {
        throw new IllegalStateException("popping empty queue");
    }

    public int count() {
        return 0;
    }

    public ISeq seq() {
        return null;
    }

    public IPersistentCollection cons(Object o) {
        return new PersistentQueue(1,RT.list(o),EMPTY,null);
    }
}

final public static Empty EMPTY = new Empty();

final int lenfm;
final ISeq f;
final IPersistentList m;    //queue of suspended seqs
final ISeq r;



public PersistentQueue(int lenfm, ISeq f, IPersistentList m, ISeq r) {
    this.lenfm = lenfm;
    this.f = f;
    this.m = m;
    this.r = r;
}

public Object peek() {
    return RT.first(f);
}

public IPersistentList pop() {
    return checkQ(lenfm - 1, RT.rest(f), m, r);
}

public int count() {
    return lenfm + RT.count(r);
}

public ISeq seq() {
    ISeq mseq = (r == null) ? m.seq():m.cons(new SuspReverse(r)).seq();
    return new Seq(f, mseq);
}

public IPersistentCollection cons(Object o) {
    return checkQ(lenfm, f, m,  RT.cons(o, r));
}


static class Seq extends ASeq{
    final ISeq f;
    final ISeq mseq;    //seq of suspended seqs

    Seq(ISeq f, ISeq mseq) {
        this.f = f;
        this.mseq = mseq;
    }

    public Object first() {
        return f.first();
    }

    public ISeq rest() {
        ISeq f1 = f.rest();
        ISeq m1 = mseq;
        if(f1 == null)
            {
            if(mseq == null)
                return null;
            f1 = ((SuspReverse) mseq.first()).force();
            m1 = mseq.rest();
            }
        return new Seq(f1, m1);
    }
}

//////////// implementation ///////////////
static  IPersistentList checkQ(int lenfm, ISeq f, IPersistentList m, ISeq r) {
    if(RT.count(r) <= lenfm)
        return checkF(lenfm,f,m,r);
    return checkF(lenfm + RT.count(r), f, (IPersistentList) m.cons(new SuspReverse(r)), null);
}

static  IPersistentList checkF(int lenfm, ISeq f, IPersistentList m, ISeq r) {
    if(f == null)
        {
        if(m.count() == 0)
            return EMPTY;
        return new PersistentQueue(lenfm,((SuspReverse)m.peek()).force(),m.pop(),r);
        }
    return new PersistentQueue(lenfm,f,m,r);
}

static ISeq reverse(ISeq r){
    Object[] rev = new Object[RT.count(r)];

    ISeq s;
    int i;
    for (s = r, i=0; s != null; s = s.rest(), ++i)
        rev[i] = s.first();
    ISeq ret = null;
    for (i = 0; i < rev.length; i++)
        ret = RT.cons(rev[i], ret);

    return ret;
}

static class SuspReverse {
    volatile Object result;
    final ISeq s;

    public SuspReverse(ISeq s) {
        this.s = s;
        this.result = this;
    }

    public ISeq force() {
        if(result == this)
            result = reverse(s);
        return (ISeq) result;
    }
}

public static void main(String[] args) {
    if(args.length != 1)
        {
        System.err.println("Usage: PersistentQueue n");
        return;
        }
    int n = Integer.parseInt(args[0]);

    IPersistentList q = PersistentQueue.EMPTY;

    Random rand;

    rand = new Random(42);

    System.out.println("PersistentQueue");
    long startTime = System.nanoTime();
    for(int i=0;i<n;i++)
        {
        q = (IPersistentList) q.cons(i);
        }
    for(int i=0;i<n-10;i++)
        {
        q = (IPersistentList) q.pop();
        }
    long estimatedTime = System.nanoTime() - startTime;
    System.out.println("time: " + estimatedTime/1000000);
    System.out.println("peek: " + q.peek());

    //Queue list = new LinkedList();
    Queue list = new ConcurrentLinkedQueue();
    System.out.println("LinkedList");
    startTime = System.nanoTime();
    for(int i=0;i<n;i++)
        {
        list.add(i);
        }
    for(int i=0;i<n-10;i++)
        {
        list.remove();
        }
    estimatedTime = System.nanoTime() - startTime;
    System.out.println("time: " + estimatedTime/1000000);
    System.out.println("peek: " + list.peek());

    IPersistentList q2 = q;
    for(int i=0;i<10;i++)
        {
        q2 = (IPersistentList) q2.cons(i);
        }

    for(ISeq s = q.seq();s != null;s = s.rest())
        System.out.println(s.first().toString());
    for(ISeq s = q2.seq();s != null;s = s.rest())
        System.out.println(s.first().toString());
}

}
