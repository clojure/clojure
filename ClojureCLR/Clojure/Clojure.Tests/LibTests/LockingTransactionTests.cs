/**
 *   Copyright (c) David Miller. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;


using NUnit.Framework;
using Rhino.Mocks;

using clojure.lang;

using RMExpect = Rhino.Mocks.Expect;
using System.Threading;

namespace Clojure.Tests.LibTests
{
    // TODO: Add tests for LockingTransaction
    [TestFixture]
    public class LockingTransactionTests : AssertionHelper
    {


        //// TODO: Make this work.

        //// This test is taken from the Java code.
        //[Test]
        //public void BigTest()
        //{
        //    // We have to start the main work unit on its own thread because
        //    // the main during testing is STA and so does not support waiting on multiple handles.

        //    BigTester<Incrementer> tester = new BigTester<Incrementer>(5, 10, 2000);
        //    EventWaitHandle h = new EventWaitHandle(false, EventResetMode.ManualReset);
        //    Thread t = new Thread(tester.Work);
        //    t.Start(h);
        //    h.WaitOne();
        //    Console.WriteLine("Done");
        //}

        //class BigTester<T> where T : LockingTransactionTests.RefTester
        //{
        //    int _nthreads;
        //    int _niters;
        //    int _nitems;

        //    public BigTester(int nthreads, int nitems, int niters)
        //    {
        //        _nthreads = nthreads;
        //        _nitems = nitems;
        //        _niters = niters;
        //    }

        //    public void Work(object o)
        //    {
        //        List<Ref> refs = new List<Ref>();

        //        for (int i = 0; i < _nitems; i++)
        //            refs.Add(new Ref(0));

        //        List<Thread> threads = new List<Thread>(_nthreads);
        //        List<EventWaitHandle> handles = new List<EventWaitHandle>(_nthreads);
        //        List<Incrementer> tasks = new List<Incrementer>(_nthreads);

        //        for (int i = 0; i < _nthreads; i++)
        //        {
        //            List<Ref> copy = refs.GetRange(0, refs.Count);
        //            Shuffle(copy);
        //            tasks.Add(new Incrementer(i, _niters, copy));
        //            threads.Add(new Thread(tasks[i].Work));
        //            handles.Add(new EventWaitHandle(false, EventResetMode.ManualReset));
        //        }


        //        for (int i = 0; i < _nthreads; i++)
        //        {
        //            threads[i].Name = "Thr " + i;
        //            threads[i].Start(handles[i]);
        //        }

        //        EventWaitHandle.WaitAll(handles.ToArray());

        //        foreach (Incrementer task in tasks)
        //            Console.WriteLine("Task {0}: {1} millisecs", task.Id, task.Nanos / 10000.0);

        //        foreach (Ref r in refs)
        //            Console.WriteLine("Ref is {0}", r.get());

        //        EventWaitHandle ewh = (EventWaitHandle)o;
        //        ewh.Set();

        //    }


        //    void Shuffle(List<Ref> refs)
        //    {
        //    }
        //}

        //public abstract class RefTester
        //{
        //    readonly int _id;
        //    public int Id
        //    {
        //      get { return _id; }  
        //    } 

        //    protected readonly int _niters;
        //    protected readonly List<Ref> _items;

        //    long _nanos = 0;
        //    public long Nanos
        //    {
        //      get { return _nanos; }
        //    }


        //    public RefTester(int id, int niters, List<Ref> items)
        //    {
        //        _id = id;
        //        _niters = niters;
        //        _items = items;
        //    }
            
        //    public void Work(object o)
        //    {
        //        for (int i = 0; i < _niters; i++)
        //        {
        //            long startTime = DateTime.Now.Ticks;
        //            LockingTransaction.runInTransaction(this.TxUnit);
        //            long finishTime = DateTime.Now.Ticks;
        //            _nanos += finishTime - startTime;
        //        }

        //        EventWaitHandle h = (EventWaitHandle)o;
        //        h.Set();
        //    }

        //    public abstract object TxUnit(object[] args);
        //}

        //class Incrementer : RefTester
        //{
        //    public Incrementer(int id, int niters, List<Ref> items)
        //        : base(id, niters, items)
        //    {
        //    }

        //    public override object TxUnit(object[] args)
        //    {
        //        foreach (Ref r in _items)
        //        {
        //            int val = (int)r.get();
        //            r.set(val + 1);
        //        }
        //        return null;
        //    }
        //}


        //class Commuter : RefTester
        //{
        //    public Commuter(int id, int niters, List<Ref> items)
        //        : base(id, niters, items)
        //    {
        //    }

        //    public override object TxUnit(object[] args)
        //    {
        //        foreach (Ref r in _items)
        //        {
        //            r.commute(Commuter.Incr,null);
        //        }
        //        return null;
        //    }

        //    static object Incr(object[] args)
        //    {
        //        int val = (int)args[0];
        //        return val + 1;
        //    }
        //}

    }
}
