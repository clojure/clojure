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

namespace clojure.lang
{
    /// <summary>
    /// Provides spin-loop synchronized access to a value.  One of the reference types.
    /// </summary>
    public class Atom : ARef
    {
        #region Data

        /// <summary>
        /// The atom's value.
        /// </summary>
        readonly AtomicReference<object> _state;

        #endregion

        #region Ctors and factory methods

        /// <summary>
        /// Construct an atom with given intiial value.
        /// </summary>
        /// <param name="state">The initial value</param>
        public Atom(object state)
        {
            _state = new AtomicReference<object>(state);
        }

        /// <summary>
        /// Construct an atom with given initial value and metadata.
        /// </summary>
        /// <param name="state">The initial value.</param>
        /// <param name="meta">The metadata to attach.</param>
        public Atom(object state, IPersistentMap meta)
            : base(meta)
        {
            _state = new AtomicReference<object>(state);
        }



        #endregion

        #region IDeref methods

        /// <summary>
        /// Gets the (immutable) value the reference is holding.
        /// </summary>
        /// <returns>The value</returns>
        public override object deref()
        {
            return _state.Get();
        }

        #endregion

        #region State manipulation

        /// <summary>
        /// Compute and set a new value.  Spin loop for coordination.
        /// </summary>
        /// <param name="f">The function to apply to the current state.</param>
        /// <returns>The new value.</returns>
        /// <remarks>Lowercase name for core.clj compatability.</remarks>
        public object swap(IFn f)
        {
            for (; ; )
            {
                object v = deref();
                object newv = f.invoke(v);
                Validate(newv);
                if (_state.CompareAndSet(v, newv))
                {
                    if (v != newv)
                        notifyWatches();
                    return newv;
                }
            }
        }

        /// <summary>
        /// Compute and set a new value.  Spin loop for coordination.
        /// </summary>
        /// <param name="f">The function to apply to current state and one additional argument.</param>
        /// <param name="arg">Additional argument.</param>
        /// <returns>The new value.</returns>
        /// <remarks>Lowercase name for core.clj compatability.</remarks>
        public object swap(IFn f, Object arg)
        {
            for (; ; )
            {
                object v = deref();
                object newv = f.invoke(v, arg);
                Validate(newv);
                if (_state.CompareAndSet(v, newv))
                {
                    if (v != newv)
                        notifyWatches();
                    return newv;
                }
            }
        }

        /// <summary>
        /// Compute and set a new value.  Spin loop for coordination.
        /// </summary>
        /// <param name="f">The function to apply to current state and additional arguments.</param>
        /// <param name="arg1">First additional argument.</param>
        /// <param name="arg2">Second additional argument.</param>
        /// <returns>The new value.</returns>
        /// <remarks>Lowercase name for core.clj compatability.</remarks>
        public object swap(IFn f, Object arg1, Object arg2)
        {
            for (; ; )
            {
                object v = deref();
                object newv = f.invoke(v, arg1, arg2);
                Validate(newv);
                if (_state.CompareAndSet(v, newv))
                {
                    if (v != newv)
                        notifyWatches();
                    return newv;
                }
            }
        }

        /// <summary>
        /// Compute and set a new value.  Spin loop for coordination.
        /// </summary>
        /// <param name="f">The function to apply to current state and additional arguments.</param>
        /// <param name="x">First additional argument.</param>
        /// <param name="y">Second additional argument.</param>
        /// <param name="args">Sequence of additional arguments.</param>
        /// <returns>The new value.</returns>
        /// <remarks>Lowercase name for core.clj compatability.</remarks>
        public object swap(IFn f, Object x, Object y, ISeq args)
        {
            for (; ; )
            {
                object v = deref();
                object newv = f.applyTo(RT.listStar(v, x, y, args));
                Validate(newv);
                if (_state.CompareAndSet(v, newv))
                {
                    if (v != newv)
                        notifyWatches();
                    return newv;
                }
            }
        }

        /// <summary>
        /// Compare/exchange the value.
        /// </summary>
        /// <param name="oldv">The expected value.</param>
        /// <param name="newv">The new value.</param>
        /// <returns><value>true</value> if the value was set; <value>false</value> otherwise.</returns>
        public bool compareAndSet(object oldv, object newv)
        {
            Validate(newv);
            bool ret =  _state.CompareAndSet(oldv, newv);
            if (ret && oldv != newv)
                notifyWatches();
            return ret;
        }


        /// <summary>
        /// Set the value.
        /// </summary>
        /// <param name="newv">The new value.</param>
        /// <returns>The new value.</returns>
        public object reset(object newv)
        {
            Validate(newv);
            _state.Set(newv);
            notifyWatches();
            return newv;
        }

        #endregion
    }
}
