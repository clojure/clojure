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
    /// Represents a reference.
    /// </summary>
    public class Ref : ARef, IFn, IComparable<Ref>, IRef
    {
        #region Nested classes

        /// <summary>
        /// Represents the value of reference on a thread at particular point in time.
        /// </summary>
        public sealed class TVal
        {
            #region Data

            /// <summary>
            /// The value.
            /// </summary>
            object _val;

            /// <summary>
            /// The value.
            /// </summary>
            public object Val
            {
                get { return _val; }
                set { _val = value; }
            }

            /// <summary>
            /// The transaction commit/read point at which this value was set.
            /// </summary>
            long _point;

            /// <summary>
            /// The transaction commit/read point at which this value was set.
            /// </summary>
            public long Point
            {
                get { return _point; }
            }

            /// <summary>
            /// The clock time. (not used?)
            /// </summary>
            int _msecs;

            /// <summary>
            /// The prior <see cref="TVal">TVal</see>.
            /// </summary>
            /// <remarks>Implements a doubly-linked circular list.</remarks>
            TVal _prior;

            /// <summary>
            /// The prior <see cref="TVal">TVal</see>.
            /// </summary>
            /// <remarks>Implements a doubly-linked circular list.</remarks>
            public TVal Prior
            {
                get { return _prior; }
                //set { _prior = value; }
            }

            /// <summary>
            /// The next  <see cref="TVal">TVal</see>.
            /// </summary>
            /// <remarks>Implements a doubly-linked circular list.</remarks>
            TVal _next;

            /// <summary>
            /// The next  <see cref="TVal">TVal</see>.
            /// </summary>
            /// <remarks>Implements a doubly-linked circular list.</remarks>
            public TVal Next
            {
                get { return _next; }
                //set { _next = value; }
            }

            #endregion

            #region Ctors

            /// <summary>
            /// Construct a TVal, linked to a previous TVal.
            /// </summary>
            /// <param name="val"></param>
            /// <param name="point"></param>
            /// <param name="msecs"></param>
            /// <param name="prior"></param>
            public TVal(object val, long point, int msecs, TVal prior)
            {
                _val = val;
                _point = point;
                _msecs = msecs;
                _prior = prior;
                _next = _prior._next;
                _prior._next = this;
                _next._prior = this;
            }

            /// <summary>
            /// Construct a TVal, linked to itself.
            /// </summary>
            /// <param name="val"></param>
            /// <param name="point"></param>
            /// <param name="msecs"></param>
            public TVal(object val, long point, int msecs)
            {
                _val = val;
                _point = point;
                _msecs = msecs;
                _prior = this;
                _next = this;
            }

            #endregion

            #region other

            /// <summary>
            /// Set the value/point.
            /// </summary>
            /// <param name="val"></param>
            /// <param name="point"></param>
            /// <param name="msecs"></param>
            public void SetValue(object val, long point, int msecs)
            {
                _val = val;
                _point = point;
                _msecs = msecs;
            }

            #endregion
        }

        #endregion

        #region Data

        /// <summary>
        /// Values for this reference.
        /// </summary>
        TVal _tvals;

        /// <summary>
        /// Number of faults for the reference.
        /// </summary>
        readonly AtomicInteger _faults;

        /// <summary>
        /// Reader/writer lock for the reference.
        /// </summary>
        readonly ReaderWriterLockSlim _lock;

        /// <summary>
        /// Info on the transaction locking this ref.
        /// </summary>
        LockingTransaction.Info _tinfo;

        /// <summary>
        /// Info on the transaction locking this ref.
        /// </summary>
        public LockingTransaction.Info TInfo
        {
            get { return _tinfo; }
            set { _tinfo = value; }
        }

        /// <summary>
        /// An id uniquely identifying this reference.
        /// </summary>
        readonly long _id;


        /// <summary>
        /// An id uniquely identifying this reference.
        /// </summary>
        public long Id
        {
            get { return _id; }
        }

        /// <summary>
        /// Used to generate unique ids.
        /// </summary>
        static readonly AtomicLong _ids = new AtomicLong();

        #endregion

        #region C-tors & factory methods

        /// <summary>
        ///  Construct a ref with given initial value.
        /// </summary>
        /// <param name="initVal">The initial value.</param>
        public Ref(object initVal)
            : this(initVal, null)
        {
        }


        /// <summary>
        ///  Construct a ref with given initial value and metadata.
        /// </summary>
        /// <param name="initVal">The initial value.</param>
        /// <param name="meta">The metadat to attach.</param>
        public Ref(object initval, IPersistentMap meta)
            : base(meta)
        {
            _id = _ids.getAndIncrement();
            _faults = new AtomicInteger();
            _lock = new ReaderWriterLockSlim(LockRecursionPolicy.SupportsRecursion);
            _tvals = new TVal(initval, 0, System.Environment.TickCount);
        }

        #endregion

        #region Debugging

        ///// <summary>
        ///// I was having a hard day.
        ///// </summary>
        ///// <returns></returns>
        //public string DebugStr()
        //{
        //    StringBuilder sb = new StringBuilder();
        //    sb.Append("<Ref ");
        //    sb.Append(Id);
        //    sb.Append(", ");
        //    if (_tinfo == null)
        //        sb.Append("NO");
        //    else
        //        sb.AppendFormat("{0} {1}", _tinfo.Status.get(), _tinfo.StartPoint);
        //    sb.Append(", ");
        //    if (_tvals == null)
        //        sb.Append("TVals: NO");
        //    else
        //    {
        //        sb.Append("TVals: ");
        //        TVal t = _tvals;
        //        do
        //        {
        //            sb.Append(t.Point);
        //            sb.Append(" ");
        //        } while ((t = t.Prior) != _tvals);
        //    }
        //    sb.Append(">");
        //    return sb.ToString();
        //}

        #endregion

        #region IDeref Members

        /// <summary>
        /// Gets the (immutable) value the reference is holding.
        /// </summary>
        /// <returns>The value</returns>
        public override object deref()
        {
            LockingTransaction t = LockingTransaction.getRunning();
            if (t == null)
            {
                object ret = currentVal();
                //Console.WriteLine("Thr {0}, {1}: No-trans get => {2}", Thread.CurrentThread.ManagedThreadId,DebugStr(), ret);
                return ret;
            }
            return t.doGet(this, _tvals);
        }

        object currentVal()
        {
            try
            {
                _lock.EnterReadLock();
                if (_tvals != null)
                    return _tvals.Val;
                throw new InvalidOperationException(String.Format("{0} is unbound.", ToString()));
            }
            finally
            {
                _lock.ExitReadLock();
            }
        }

        #endregion

        #region  Interface for LockingTransaction

        /// <summary>
        /// Get the read lock.
        /// </summary>
        public void EnterReadLock()
        {
            _lock.EnterReadLock();
        }

        /// <summary>
        /// Release the read lock.
        /// </summary>
        public void ExitReadLock()
        {
            _lock.ExitReadLock();
        }

        /// <summary>
        /// Get the write lock.
        /// </summary>
        public void EnterWriteLock()
        {
            _lock.EnterWriteLock();
        }

        /// <summary>
        /// Release the write lock.
        /// </summary>
        public void ExitWriteLock()
        {
            _lock.ExitWriteLock();
        }

        /// <summary>
        /// Add to the fault count.
        /// </summary>
        public void AddFault()
        {
            _faults.incrementAndGet();
        }

        /// <summary>
        /// Get the read/commit point associated with the current value.
        /// </summary>
        /// <returns></returns>
        public long CurrentValPoint()
        {
            return _tvals != null ? _tvals.Point : -1;
        }

        /// <summary>
        /// Try to get the value (else null).
        /// </summary>
        /// <returns>The value if it has been set; <value>null</value> otherwise.</returns>
        public object TryGetVal()
        {
            return _tvals == null ? null : _tvals.Val;
        }

        /// <summary>
        /// Set the value.
        /// </summary>
        /// <param name="val">The new value.</param>
        /// <param name="commitPoint">The transaction's commit point.</param>
        /// <param name="msecs">The clock time.</param>
        internal void SetValue(object val, long commitPoint, int msecs)
        {
            if (_tvals == null)
                _tvals = new TVal(val, commitPoint, msecs);
            else if (_faults.get() > 0)
            {
                _tvals = new TVal(val, commitPoint, msecs, _tvals);
                _faults.set(0);
            }
            else
            {
                _tvals = _tvals.Next;
                _tvals.SetValue(val, commitPoint, msecs);
            }
        }

        #endregion

        #region Ref operations

        /// <summary>
        /// Set the value (must be in a transaction).
        /// </summary>
        /// <param name="val">The new value.</param>
        /// <returns>The new value.</returns>
        public object set(object val)
        {
            return LockingTransaction.getEx().doSet(this, val);
        }

        /// <summary>
        /// Apply a commute to the reference. (Must be in a transaction.)
        /// </summary>
        /// <param name="fn">The function to apply to the current state and additional arguments.</param>
        /// <param name="args">Additional arguments.</param>
        /// <returns>The computed value.</returns>
        public object commute(IFn fn, ISeq args)
        {
            return LockingTransaction.getEx().doCommute(this, fn, args);
        }

        /// <summary>
        /// Change to a computed value.
        /// </summary>
        /// <param name="fn">The function to apply to the current state and additional arguments.</param>
        /// <param name="args">Additional arguments.</param>
        /// <returns>The computed value.</returns>
        public object alter(IFn fn, ISeq args)
        {
            LockingTransaction t = LockingTransaction.getEx();
            return t.doSet(this, fn.applyTo(RT.cons(t.doGet(this, _tvals), args)));
        }

        /// <summary>
        /// Touch the reference.  (Add to the tracking list in the current transaction.)
        /// </summary>
        public void touch()
        {
            LockingTransaction.getEx().doTouch(this);
        }

        #endregion

        #region IFn Members


        public IFn fn()
        {
            return (IFn)deref();
        }

        public object invoke()
        {
            return fn().invoke();
        }

        public object invoke(object arg1)
        {
            return fn().invoke(arg1);
        }

        public object invoke(object arg1, object arg2)
        {
            return fn().invoke(arg1, arg2);
        }

        public object invoke(object arg1, object arg2, object arg3)
        {
            return fn().invoke(arg1, arg2, arg3);
        }

        public object invoke(object arg1, object arg2, object arg3, object arg4)
        {
            return fn().invoke(arg1, arg2, arg3, arg4);
        }

        public object invoke(object arg1, object arg2, object arg3, object arg4, object arg5)
        {
            return fn().invoke(arg1, arg2, arg3, arg4, arg5);
        }

        public object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6)
        {
            return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6);
        }

        public object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7)
        {
            return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7);
        }

        public object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8)
        {
            return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
        }

        public object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9)
        {
            return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
        }

        public object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10)
        {
            return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
        }

        public object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11)
        {
            return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
        }

        public object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11, object arg12)
        {
            return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
        }

        public object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11, object arg12, object arg13)
        {
            return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13);
        }

        public object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14)
        {
            return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14);
        }

        public object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14,
                             object arg15)
        {
            return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15);
        }

        public object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14,
                             object arg15, object arg16)
        {
            return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15,
                               arg16);
        }

        public object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14,
                             object arg15, object arg16, object arg17)
        {
            return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15,
                               arg16, arg17);
        }

        public object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14,
                             object arg15, object arg16, object arg17, object arg18)
        {
            return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15,
                               arg16, arg17, arg18);
        }

        public object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14,
                             object arg15, object arg16, object arg17, object arg18, object arg19)
        {
            return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15,
                               arg16, arg17, arg18, arg19);
        }

        public object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14,
                             object arg15, object arg16, object arg17, object arg18, object arg19, object arg20)
        {
            return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15,
                               arg16, arg17, arg18, arg19, arg20);
        }

        public object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14,
                             object arg15, object arg16, object arg17, object arg18, object arg19, object arg20,
                             params object[] args)
        {
            return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15,
                               arg16, arg17, arg18, arg19, arg20, args);
        }

        public object applyTo(ISeq arglist)
        {
            return AFn.ApplyToHelper(this, arglist);
        }

        #endregion

        #region IComparable<Ref> Members

        /// <summary>
        /// Compare to another ref.
        /// </summary>
        /// <param name="other">The other ref.</param>
        /// <returns><value>true</value> if they are identical; <value>false</value> otherwise.</returns>
        public int CompareTo(Ref other)
        {
            return _id.CompareTo(other._id);
        }

        #endregion

    }
}
