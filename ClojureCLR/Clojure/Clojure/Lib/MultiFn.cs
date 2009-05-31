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
using System.Runtime.CompilerServices;

namespace clojure.lang
{
    /// <summary>
    /// Represents a multifunction.
    /// </summary>
    /// <remarks>See the Clojure documentation for more details.</remarks>
    public class MultiFn : AFn
    {
        #region Data

        /// <summary>
        /// The function that dispatches calls to the correct method.
        /// </summary>
        readonly IFn _dispatchFn;

        /// <summary>
        /// The default dispatch value.
        /// </summary>
        readonly object _defaultDispatchVal;

        /// <summary>
        /// The hierarchy for this defmulti.
        /// </summary>
        readonly IRef _hierarchy;

        /// <summary>
        /// The name of this multifunction.
        /// </summary>
        readonly string _name;

        /// <summary>
        /// The methods defined for this multifunction.
        /// </summary>
        IPersistentMap _methodTable;

        /// <summary>
        /// The methods defined for this multifunction.
        /// </summary>
        public IPersistentMap MethodTable
        {
            get { return _methodTable; }
        }

        /// <summary>
        /// Method preferences.
        /// </summary>
        IPersistentMap _preferTable;

        /// <summary>
        /// Method preferences.
        /// </summary>
        public IPersistentMap PreferTable
        {
            get { return _preferTable; }
        }
        
        /// <summary>
        /// Cache of previously encountered dispatch-value to method mappings.
        /// </summary>
        IPersistentMap _methodCache;

        /// <summary>
        /// Hierarchy on which cached computations are based.
        /// </summary>
        object _cachedHierarchy;

        static readonly Var _assoc = RT.var("clojure.core", "assoc");
        static readonly Var _dissoc = RT.var("clojure.core", "dissoc");
        static readonly Var _isa = RT.var("clojure.core", "isa?", null);
        static readonly Var _parents = RT.var("clojure.core", "parents");
        //static readonly Var _hierarchy = RT.var("clojure.core", "global-hierarchy", null);

        #endregion

        #region C-tors & factory methods

        /// Construct a multifunction.
        /// </summary>
        /// <param name="name">The name</param>
        /// <param name="dispatchFn">The dispatch function.</param>
        /// <param name="defaultDispatchVal">The default dispatch value.</param>
        /// <param name="hierarchy">The hierarchy for this multifunction</param>
        public MultiFn(string name, IFn dispatchFn, object defaultDispatchVal, IRef hierarchy)
        {
            _name = name;
            _dispatchFn = dispatchFn;
            _defaultDispatchVal = defaultDispatchVal;
            _methodTable = PersistentHashMap.EMPTY;
            _methodCache = MethodTable;
            _preferTable = PersistentHashMap.EMPTY;
            _hierarchy = hierarchy;
            _cachedHierarchy = null;
        }

        #endregion

        #region External interface

        /// <summary>
        /// Add a new method to this multimethod.
        /// </summary>
        /// <param name="dispatchVal">The discriminator value for this method.</param>
        /// <param name="method">The method code.</param>
        /// <returns>This multifunction.</returns>
        [MethodImpl(MethodImplOptions.Synchronized)]
        public MultiFn addMethod(object dispatchVal, IFn method)
        {
            return addMethodImpl(dispatchVal, method);
        }


        /// <summary>
        /// Implements adding a new method to this multimethod
        /// </summary>
        /// <param name="dispatchVal">The discriminator value for this method.</param>
        /// <param name="method">The method code.</param>
        /// <returns>This multifunction.</returns>
        MultiFn addMethodImpl(object dispatchVal, IFn method)
        {
            _methodTable = MethodTable.assoc(dispatchVal, method);
            ResetCache();
            return this;
        }

        /// <summary>
        /// Remove a method.
        /// </summary>
        /// <param name="dispatchVal">The dispatch value for the multimethod.</param>
        /// <returns>This multifunction.</returns>
        [MethodImpl(MethodImplOptions.Synchronized)]
        public MultiFn removeMethod(object dispatchVal)
        {
            _methodTable = MethodTable.without(dispatchVal);
            ResetCache();
            return this;
        }

        /// <summary>
        /// Add a preference for one method over another.
        /// </summary>
        /// <param name="dispatchValX">The more preferred dispatch value.</param>
        /// <param name="dispatchValY">The less preferred dispatch value.</param>
        /// <returns>This multifunction.</returns>
        [MethodImpl(MethodImplOptions.Synchronized)]
        public MultiFn preferMethod(object dispatchValX, object dispatchValY)
        {
            if (Prefers(dispatchValY, dispatchValX))
                throw new InvalidOperationException(String.Format("Preference conflict in multimethod {0}: {1} is already preferred to {2}", _name,dispatchValY, dispatchValX));
            _preferTable = PreferTable.assoc(dispatchValX,
                RT.conj((IPersistentCollection)RT.get(_preferTable, dispatchValX, PersistentHashSet.EMPTY),
                        dispatchValY));
            ResetCache();
            return this;
        }

        #endregion

        #region Implementation details

        /// <summary>
        /// Is one value preferred over another?
        /// </summary>
        /// <param name="x">The first dispatch value.</param>
        /// <param name="y">The second dispatch value.</param>
        /// <returns><value>true</value> if <paramref name="x"/> is preferred over <paramref name="y"/></returns>
        private bool Prefers(object x, object y)
        {
            IPersistentSet xprefs = (IPersistentSet)PreferTable.valAt(x);
            if (xprefs != null && xprefs.contains(y))
                return true;
            for (ISeq ps = RT.seq(_parents.invoke(y)); ps != null; ps = ps.next())
                if (Prefers(x, ps.first()))
                    return true;
            for (ISeq ps = RT.seq(_parents.invoke(x)); ps != null; ps = ps.next())
                if (Prefers(ps.first(), y))
                    return true;
            return false;
        }

        /// <summary>
        /// Check the hierarchy.
        /// </summary>
        /// <param name="x"></param>
        /// <param name="y"></param>
        /// <returns></returns>
        private bool IsA(object x, object y)
        {
            return RT.booleanCast(_isa.invoke(_hierarchy.deref(),x, y));
        }

        /// <summary>
        /// Determine if one dispatch is preferred over another.
        /// </summary>
        /// <param name="x"></param>
        /// <param name="y"></param>
        /// <returns></returns>
        private bool Dominates(object x, object y)
        {
            return Prefers(x, y) || IsA(x, y);
        }


        /// <summary>
        /// Reset the method cache.
        /// </summary>
        /// <returns></returns>
        private IPersistentMap ResetCache()
        {
            _methodCache = MethodTable;
            _cachedHierarchy = _hierarchy.deref();
            return _methodCache;
        }

        /// <summary>
        /// Get the method for a dispatch value.
        /// </summary>
        /// <param name="dispatchVal">The dispatch value.</param>
        /// <returns>The preferred method for the value.</returns>
        /// <remarks>lower initial letter for core.clj compatibility</remarks>
        [MethodImpl(MethodImplOptions.Synchronized)]
        public IFn getMethod(object dispatchVal)
        {
            if (_cachedHierarchy != _hierarchy.deref())
                ResetCache();

            IFn targetFn = (IFn)_methodCache.valAt(dispatchVal);
            if (targetFn != null)
                return targetFn;

            targetFn = FindAndCacheBestMethod(dispatchVal);
            if (targetFn != null)
                return targetFn;

            targetFn = (IFn)MethodTable.valAt(_defaultDispatchVal);
            return targetFn;
        }

        private IFn GetFn(object dispatchVal)
        {
            IFn targetFn = getMethod(dispatchVal);
            if (targetFn == null)
                throw new ArgumentException(String.Format("No method for dispatch value: {0}", dispatchVal));
            return targetFn;
        }

        /// <summary>
        /// Get the method for a dispatch value and cache it.
        /// </summary>
        /// <param name="dispatchVal">The disaptch value.</param>
        /// <returns>The mest method.</returns>
        private IFn FindAndCacheBestMethod(object dispatchVal)
        {
            IMapEntry bestEntry = null;
            foreach (IMapEntry me in MethodTable)
            {
                if (IsA(dispatchVal, me.key()))
                {
                    if (bestEntry == null || Dominates(me.key(), bestEntry.key()))
                        bestEntry = me;
                    if (!Dominates(bestEntry.key(), me.key()))
                        throw new ArgumentException(String.Format("Multiple methods in multimethod {0} match dispatch value: {1} -> {2} and {3}, and neither is preferred",
                            _name,dispatchVal, me.key(), bestEntry.key()));
                }
            }
            if (bestEntry == null)
                return null;

            // ensure basis has stayed stable throughout, else redo
            if (_cachedHierarchy == _hierarchy.deref())
            {
                // place in cache
                _methodCache = _methodCache.assoc(dispatchVal, bestEntry.val());
                return (IFn)bestEntry.val();
            }
            else
            {
                ResetCache();
                return FindAndCacheBestMethod(dispatchVal);
            }
        }

        #endregion

        #region core.clj compatibility

        /// <summary>
        /// Get the map of dispatch values to dispatch fns.
        /// </summary>
        /// <returns>The map of dispatch values to dispatch fns.</returns>
        public IPersistentMap getMethodTable()
        {
            return MethodTable;
        }


        /// <summary>
        /// Get the map of preferred value to set of other values.
        /// </summary>
        /// <returns>The map of preferred value to set of other values.</returns>
        public IPersistentMap getPreferTable()
        {
            return PreferTable;
        }

        #endregion

        #region IFn members


        public override object invoke()
        {
            return GetFn(_dispatchFn.invoke()).invoke();
        }

        public override object invoke(object arg1)
        {
            return GetFn(_dispatchFn.invoke(arg1)).invoke(arg1);
        }

        public override object invoke(object arg1, object arg2)
        {
            return GetFn(_dispatchFn.invoke(arg1, arg2)).invoke(arg1, arg2);
        }

        public override object invoke(object arg1, object arg2, object arg3)
        {
            return GetFn(_dispatchFn.invoke(arg1, arg2, arg3)).invoke(arg1, arg2, arg3);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4)
        {
            return GetFn(_dispatchFn.invoke(arg1, arg2, arg3, arg4)).invoke(arg1, arg2, arg3, arg4);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5)
        {
            return GetFn(_dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5)).invoke(arg1, arg2, arg3, arg4, arg5);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6)
        {
            return GetFn(_dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6)).invoke(arg1, arg2, arg3, arg4, arg5, arg6);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7)
        {
            return GetFn(_dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7))
                    .invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8)
        {
            return GetFn(_dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)).
                    invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9)
        {
            return GetFn(_dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)).
                    invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10)
        {
            return GetFn(_dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)).
                    invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11)
        {
            return GetFn(_dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)).
                    invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11, object arg12)
        {
            return GetFn(_dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)).
                    invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11, object arg12, object arg13)
        {
            return GetFn(_dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13)).
                    invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14)
        {
            return GetFn(
                    _dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14)).
                    invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14,
                             object arg15)
        {
            return GetFn(
                    _dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
                                      arg15))
                    .invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14,
                             object arg15, object arg16)
        {
            return GetFn(
                    _dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
                                      arg15, arg16))
                    .invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
                            arg15, arg16);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14,
                             object arg15, object arg16, object arg17)
        {
            return GetFn(
                    _dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
                                      arg15, arg16, arg17))
                    .invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
                            arg15, arg16, arg17);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14,
                             object arg15, object arg16, object arg17, object arg18)
        {
            return GetFn(
                    _dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
                                      arg15, arg16, arg17, arg18)).
                    invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
                           arg15, arg16, arg17, arg18);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14,
                             object arg15, object arg16, object arg17, object arg18, object arg19)
        {
            return GetFn(
                    _dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
                                      arg15, arg16, arg17, arg18, arg19)).
                    invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
                           arg15, arg16, arg17, arg18, arg19);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14,
                             object arg15, object arg16, object arg17, object arg18, object arg19, object arg20)
        {
            return GetFn(
                    _dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
                                      arg15, arg16, arg17, arg18, arg19, arg20)).
                    invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
                           arg15, arg16, arg17, arg18, arg19, arg20);
        }

        public override object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14,
                             object arg15, object arg16, object arg17, object arg18, object arg19, object arg20, params object[] args)
        {
            return GetFn(
                    _dispatchFn.invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
                                      arg15, arg16, arg17, arg18, arg19, arg20, args)).
                    invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14,
                           arg15, arg16, arg17, arg18, arg19, arg20, args);
        }


        #endregion

    }
}
