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
using System.Threading;

namespace clojure.lang
{
    /// <summary>
    /// Provides transaction semantics for <see cref="Agent">Agent</see>s, <see cref="Ref">Ref</see>s, etc.
    /// </summary>
    public class LockingTransaction
    {
        #region Constants & enums

        /// <summary>
        /// The number of times to retry a transaction in case of a conflict.
        /// </summary>
        public const int RETRY_LIMIT = 10000;

        /// <summary>
        /// How long to wait for a lock.
        /// </summary>
        public const int LOCK_WAIT_MSECS = 100;

        /// <summary>
        /// How old another transaction must be before we 'barge' it.
        /// </summary>
        public const long BARGE_WAIT_TICKS = 100000;


        // State constants
        // Should be an enum, but we want Interlocked capability

        /// <summary>
        /// Value: The transaction is running.
        /// </summary>
        const int RUNNING = 0;

        /// <summary>
        /// Value: The transaction is committing.
        /// </summary>
        const int COMMITTING = 1;

        /// <summary>
        /// Value: the transaction is getting ready to retry.
        /// </summary>
        const int RETRY = 2;

        /// <summary>
        /// The transaction has been killed.
        /// </summary>
        const int KILLED = 3;

        /// <summary>
        /// The transaction has been committed.
        /// </summary>
        const int COMMITTED = 4;
        
        #endregion

        #region supporting classes

        /// <summary>
        /// Exception thrown when a retry is necessary.
        /// </summary>
        public class RetryEx : Exception
        {
        }

        /// <summary>
        /// The transaction has been aborted.
        /// </summary>
        public class AbortException : Exception
        {
        }

        /// <summary>
        /// The current state of a transaction.
        /// </summary>
        public class Info
        {
            #region Data

            /// <summary>
            /// The status of the transaction.
            /// </summary>
            readonly AtomicInteger _status;

            /// <summary>
            /// The status of the transaction.
            /// </summary>
            internal AtomicInteger Status
            {
                get { return _status; }
            }

            /// <summary>
            /// The start point of the transaction.
            /// </summary>
            readonly long _startPoint;

            /// <summary>
            /// The start point of the transaction.
            /// </summary>
            public long StartPoint
            {
                get { return _startPoint; }
            }

            #endregion

            #region C-tors

            /// <summary>
            /// Construct an info.
            /// </summary>
            /// <param name="status">Current status.</param>
            /// <param name="startPoint">Start point.</param>
            public Info(int status, long startPoint)
            {
                _status = new AtomicInteger(status);
                _startPoint = startPoint;
            }

            #endregion

            #region Other

            /// <summary>
            /// Is the transaction running?
            /// </summary>
            public bool IsRunning
            {
                get
                {
                    long s = _status.get();
                    return s == RUNNING || s == COMMITTING;
                }
            }

            #endregion
        }

        /// <summary>
        /// Pending call of a function on arguments.
        /// </summary>
        class CFn
        {
            #region Data

            /// <summary>
            ///  The function to be called.
            /// </summary>
            readonly IFn _fn;

            /// <summary>
            ///  The function to be called.
            /// </summary>
            public IFn Fn
            {
                get { return _fn; }
            }

            /// <summary>
            /// The arguments to the function.
            /// </summary>
            readonly ISeq _args;

            /// <summary>
            /// The arguments to the function.
            /// </summary>
            public ISeq Args
            {
                get { return _args; }
            }

            #endregion

            #region C-tors

            /// <summary>
            /// Construct one.
            /// </summary>
            /// <param name="fn">The function to invoke.</param>
            /// <param name="args">The arguments to invoke the function on.</param>
            public CFn(IFn fn, ISeq args)
            {
                _fn = fn;
                _args = args;
            }

            #endregion
        }

        #endregion

        #region Data

        /// <summary>
        /// The transaction running on the current thread.  (Thread-local.)
        /// </summary>
        [ThreadStatic]
        private static LockingTransaction _transaction;

        /// <summary>
        /// The current point.
        /// </summary>
        /// <remarks>
        /// <para>Used to provide a total ordering on transactions 
        /// for the purpose of determining preference on transactions 
        /// when there are conflicts.  
        /// Transactions consume a point for init, for each retry, 
        /// and on commit if writing.</para>
        /// </remarks>
        private static readonly AtomicLong _lastPoint = new AtomicLong();

        /// <summary>
        ///  The state of the transaction.
        /// </summary>
        /// <remarks>Encapsulated so things like Refs can look.</remarks>
        Info _info;

        /// <summary>
        /// The point at the start of the current retry (or first try).
        /// </summary>
        long _readPoint;

        /// <summary>
        /// The point at the start of the transaction.
        /// </summary>
        long _startPoint;

        /// <summary>
        /// The system ticks at the start of the transaction.
        /// </summary>
        long _startTime;

        /// <summary>
        /// Cached retry exception.
        /// </summary>
        readonly RetryEx _retryex = new RetryEx();

        /// <summary>
        /// Agent actions pending on this thread.
        /// </summary>
        readonly List<Agent.Action> _actions = new List<Agent.Action>();

        /// <summary>
        /// Ref assignments made in this transaction (both sets and commutes).
        /// </summary>
        readonly Dictionary<Ref, Object> _vals = new Dictionary<Ref, Object>();

        /// <summary>
        /// Refs that have been set in this transaction.
        /// </summary>
        readonly HashSet<Ref> _sets = new HashSet<Ref>();

        /// <summary>
        /// Ref commutes that have been made in this transaction.
        /// </summary>
        readonly SortedDictionary<Ref, List<CFn>> _commutes = new SortedDictionary<Ref, List<CFn>>();

        #endregion

        #region Debugging

        //string TId() 
        //{ 
        //    return String.Format("<{0}:{1}>", Thread.CurrentThread.ManagedThreadId, _readPoint);
        //}

        #endregion

        #region  Point manipulation

        /// <summary>
        /// Get a new read point value.
        /// </summary>
        void getReadPoint()
        {
            _readPoint = _lastPoint.incrementAndGet();
        }

        /// <summary>
        /// Get a commit point value.
        /// </summary>
        /// <returns></returns>
        long getCommitpoint()
        {
            return _lastPoint.incrementAndGet();
        }

        #endregion

        #region Actions

        /// <summary>
        /// Stop this transaction.
        /// </summary>
        /// <param name="status">The new status.</param>
        void stop(int status)
        {

            if (_info != null)
            {
                lock (_info)
                {
                    _info.Status.set(status);
                    Monitor.PulseAll(_info);
                }
                _info = null;
                _vals.Clear();
                _sets.Clear();
                _commutes.Clear();
                // Java commented out: _actions.Clear();
            }
        }

        /// <summary>
        /// Lock a ref.
        /// </summary>
        /// <param name="r">The ref to lock.</param>
        /// <returns>The current value of the ref.</returns>
        object Lock(Ref r)
        {
            bool unlocked = false;
            try
            {
                r.EnterWriteLock();
                if (r.CurrentValPoint() > _readPoint)
                    throw _retryex;

                Info refinfo = r.TInfo;

                // write lock conflict
                if (refinfo != null && refinfo != _info && refinfo.IsRunning)
                {
                    if (!barge(refinfo))
                    {
                        r.ExitWriteLock();
                        unlocked = true;
                        // stop prior to blocking
                        stop(RETRY);
                        lock (refinfo)
                        {
                            if (refinfo.IsRunning)
                            {
                                try
                                {
                                    Monitor.Wait(refinfo, LOCK_WAIT_MSECS);
                                }
                                catch (ThreadInterruptedException)
                                {
                                }
                            }
                        }
                        throw _retryex;
                    }
                }

                r.TInfo = _info;
                return r.TryGetVal();
            }
            finally
            {
                if (!unlocked)
                {
                    r.ExitWriteLock();
                }
            }
        }

        /// <summary>
        /// Kill this transaction.
        /// </summary>
        void abort()
        {
            stop(KILLED);
            throw new AbortException();
        }

        /// <summary>
        /// Determine if sufficient clock time has elapsed to barge another transaction.
        /// </summary>
        /// <returns><value>true</value> if enough time has elapsed; <value>false</value> otherwise.</returns>
        private bool bargeTimeElapsed()
        {
            return DateTime.Now.Ticks - _startTime > BARGE_WAIT_TICKS;
        }

        /// <summary>
        /// Try to barge a conflicting transaction.
        /// </summary>
        /// <param name="refinfo">The info on the other transaction.</param>
        /// <returns><value>true</value> if we killed the other transaction; <value>false</value> otherwise.</returns>
        private bool barge(Info refinfo)
        {
            bool barged = false;
            // if this transaction is older
            //   try to abort the other
            if (bargeTimeElapsed() && _startPoint < refinfo.StartPoint)
            {
                lock (refinfo)
                {
                    barged = refinfo.Status.compareAndSet(RUNNING, KILLED);
                    if (barged)
                        Monitor.PulseAll(refinfo);
                }
            }
            return barged;
        }

        /// <summary>
        /// Get the transaction running on this thread (throw exception if no transaction). 
        /// </summary>
        /// <returns>The running transaction.</returns>
        public static LockingTransaction getEx()
        {
            LockingTransaction t = _transaction;
            if (t == null || t._info == null)
                throw new InvalidOperationException("No transaction running");
            return t;
        }

        /// <summary>
        /// Get the transaction running on this thread (or null if no transaction).
        /// </summary>
        /// <returns>The running transaction if there is one, else <value>null</value>.</returns>
        public static LockingTransaction getRunning()
        {
            LockingTransaction t = _transaction;
            if (t == null || t._info == null)
                return null;
            return t;
        }

        /// <summary>
        /// Is there a transaction running on this thread?
        /// </summary>
        /// <returns><value>true</value> if there is a transaction running on this thread; <value>false</value> otherwise.</returns>
        public static bool isRunning()
        {
            return getRunning() != null;
        }

        /// <summary>
        /// Invoke a function in a transaction
        /// </summary>
        /// <param name="fn">The function to invoke.</param>
        /// <returns>The value computed by the function.</returns>
        public static object runInTransaction(IFn fn)
        {
            LockingTransaction t = _transaction;
            if (t == null)
                _transaction = t = new LockingTransaction();

            if (t._info != null)
                return fn.invoke();

            return t.run(fn);
        }

        /// <summary>
        /// Start a transaction and invoke a function.
        /// </summary>
        /// <param name="fn">The fucntion to invoke.</param>
        /// <returns>The value computed by the function.</returns>
        object run(IFn fn)
        {
            bool done = false;
            object ret = null;
            List<Ref> locked = new List<Ref>();

            for (int i = 0; !done && i < RETRY_LIMIT; i++)
            {
                try
                {
                    getReadPoint();
                    if (i == 0)
                    {
                        _startPoint = _readPoint;
                        _startTime = DateTime.Now.Ticks;
                    }

                    _info = new Info(RUNNING, _startPoint);
                    ret = fn.invoke();

                    // make sure no one has killed us before this point,
                    // and can't from now on
                    if (_info.Status.compareAndSet(RUNNING, COMMITTING))
                    {
                        foreach (KeyValuePair<Ref, List<CFn>> pair in _commutes)
                        {
                            Ref r = pair.Key;
                            r.EnterWriteLock();
                            locked.Add(r);
                            Info refinfo = r.TInfo;
                            if (refinfo != null && refinfo != _info && refinfo.IsRunning)
                            {
                                if (!barge(refinfo))
                                {
                                    throw _retryex;
                                }
                            }
                            object val = r.TryGetVal();
                            if (!_sets.Contains(r))
                                _vals[r] = val;
                            foreach (CFn f in pair.Value)
                                _vals[r] = f.Fn.applyTo(RT.cons(_vals[r], f.Args));
                        }
                        foreach (Ref r in _sets)
                        {
                            if (!_commutes.ContainsKey(r))
                            {
                                r.EnterWriteLock();
                                locked.Add(r);
                            }
                        }
                        // validate and enqueue notifications
                        foreach (KeyValuePair<Ref, object> pair in _vals)
                        {
                            Ref r = pair.Key;
                            r.Validate(pair.Value);
                            r.notifyWatches();
                        }

                        // at this point, all values calced, all refs to be written locked
                        // no more client code to be called
                        int msecs = System.Environment.TickCount;
                        long commitPoint = getCommitpoint();
                        foreach (KeyValuePair<Ref, object> pair in _vals)
                        {
                            Ref r = pair.Key;
                            object val = pair.Value;
                            r.SetValue(val, commitPoint, msecs);
                        }

                        done = true;
                        _info.Status.set(COMMITTED);
                    }
                }
                catch (RetryEx)
                {
                    // eat this so we retry rather than fall out
                }
                catch (Exception ex)
                {
                    if (ContainsNestedRetryEx(ex))
                    {
                        // Wrapped exception, eat it.
                    }
                    else
                    {
                        throw ex;
                    }
                }
                finally
                {
                    for (int k = locked.Count - 1; k >= 0; --k)
                    {
                        locked[k].ExitWriteLock();
                    }
                    locked.Clear();
                    stop(done ? COMMITTED : RETRY);
                    if (done) // re-dispatch out of transaction
                    {
                        foreach (Agent.Action action in _actions)
                        {
                            Agent.DispatchAction(action);
                        }
                    }
                    _actions.Clear();
                }
            }
            if (!done)
                throw new Exception("Transaction failed after reaching retry limit");
            return ret;
        }

        /// <summary>
        /// Determine if the exception wraps a <see cref="RetryEx">RetryEx</see> at some level.
        /// </summary>
        /// <param name="ex">The exception to test.</param>
        /// <returns><value>true</value> if there is a nested  <see cref="RetryEx">RetryEx</see>; <value>false</value> otherwise.</returns>
        /// <remarks>Needed because sometimes our retry exceptions get wrapped.  You do not want to know how long it took to track down this problem.</remarks>
        private static bool ContainsNestedRetryEx(Exception ex)
        {
            for (Exception e = ex; e != null; e = e.InnerException)
                if (e is RetryEx)
                    return true;
            return false;
        }

        /// <summary>
        /// Add an agent action sent during the transaction to a queue.
        /// </summary>
        /// <param name="action">The action that was sent.</param>
        internal void enqueue(Agent.Action action)
        {
            _actions.Add(action);
        }

        /// <summary>
        /// Get the value of a ref most recently set in this transaction (or prior to entering).
        /// </summary>
        /// <param name="r"></param>
        /// <param name="tvals"></param>
        /// <returns>The value.</returns>
        internal object doGet(Ref r, Ref.TVal tvals)
        {
            if (!_info.IsRunning)
                throw _retryex;
            if (_vals.ContainsKey(r))
            {
                return _vals[r];
            }
            try
            {
                r.EnterReadLock();
                if (tvals == null)
                    throw new InvalidOperationException(r.ToString() + " is not bound.");
                Ref.TVal ver = tvals;
                do
                {
                    if (ver.Point <= _readPoint)
                    {
                        return ver.Val;
                    }
                } while ((ver = ver.Prior) != tvals);
            }
            finally
            {
                r.ExitReadLock();
            }
            // no version of val precedes the read point
            r.AddFault();
            throw _retryex;
        }

        /// <summary>
        /// Set the value of a ref inside the transaction.
        /// </summary>
        /// <param name="r">The ref to set.</param>
        /// <param name="val">The value.</param>
        /// <returns>The value.</returns>
        internal object doSet(Ref r, object val)
        {
            if (!_info.IsRunning)
                throw _retryex;
            if (_commutes.ContainsKey(r))
                throw new InvalidOperationException("Can't set after commute");
            if (!_sets.Contains(r))
            {
                _sets.Add(r);
                Lock(r);
            }
            _vals[r] = val;
            return val;
        }

        /// <summary>
        /// Touch a ref.  (Lock it.)
        /// </summary>
        /// <param name="r">The ref to touch.</param>
        internal void doTouch(Ref r)
        {
            if (!_info.IsRunning)
                throw _retryex;
            Lock(r);
        }

        /// <summary>
        /// Post a commute on a ref in this transaction.
        /// </summary>
        /// <param name="r">The ref.</param>
        /// <param name="fn">The commuting function.</param>
        /// <param name="args">Additional arguments to the function.</param>
        /// <returns>The computed value.</returns>
        internal object doCommute(Ref r, IFn fn, ISeq args)
        {
            if (!_info.IsRunning)
                throw _retryex;
            if (!_vals.ContainsKey(r))
            {
                object val = null;
                try
                {
                    r.EnterReadLock();
                    val = r.TryGetVal();
                }
                finally
                {
                    r.ExitReadLock();
                }
                _vals[r] = val;
            }
            List<CFn> fns = _commutes[r];
            if (fns == null)
                _commutes[r] = fns = new List<CFn>();
            fns.Add(new CFn(fn, args));
            object ret = fn.applyTo(RT.cons(_vals[r], args));
            _vals[r] = ret;

            return ret;
        }

        #endregion
    }
}
