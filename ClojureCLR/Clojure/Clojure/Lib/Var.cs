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
using System.Runtime.CompilerServices;

namespace clojure.lang
{

    /// <summary>
    /// Represents a Var.
    /// </summary>
    /// <remarks>
    /// <para>From the Clojure documentation:</para>
    /// <blockquote>"Vars provide a mechanism to refer to a mutable storage location 
    /// that can be dynamically rebound (to a new storage location) on a per-thread basis. 
    /// Every Var can (but needn't) have a root binding, 
    /// which is a binding that is shared by all threads that do not have a per-thread binding."</blockquote>
    /// </remarks>
    public sealed class Var : ARef, IFn, IRef, Settable
    {
        /// <summary>
        /// Represents a set of Var bindings established at a particular point in the call stack.
        /// </summary>
        class Frame
        {
            #region Data

            /// <summary>
            /// A mapping from <see cref="Var">Var</see>s to <see cref="Box">Box</see>es holding their values.
            /// </summary>
            readonly Associative _bindings;

            /// <summary>
            /// Get mapping from <see cref="Var">Var</see>s to <see cref="Box">Box</see>es holding their values.
            /// </summary>
            public Associative Bindings
            {
                get { return _bindings; }
            }

            /// <summary>
            /// The bindings of this frame only.
            /// </summary>
            /// <remarks>Used only to know which Vars to decrement the counts of 
            /// during an unwind/pop, i.e., only the keys matter.</remarks>
            readonly Associative _frameBindings;

            /// <summary>
            /// Get the bindings of this frame only.
            /// </summary>
            public Associative FrameBindings
            {
                get { return _frameBindings; }
            }

            /// <summary>
            /// The previous <see cref="Frame">Frame</see> on the stack.
            /// </summary>
            readonly Frame _prev;

            /// <summary>
            /// Get the previous <see cref="Frame">Frame</see> on the stack.
            /// </summary>
            public Frame Prev
            {
                get { return _prev; }
            }

            #endregion

            #region Ctors

            /// <summary>
            /// Construct an empty frame.
            /// </summary>
            public Frame()
                : this(PersistentHashMap.EMPTY, PersistentHashMap.EMPTY, null)
            {
            }

            /// <summary>
            /// Construct a frame on the stack.
            /// </summary>
            /// <param name="frameBindings">The bindings for this frame only.</param>
            /// <param name="bindings">Bindings all the way down the stack.</param>
            /// <param name="prev">The previous frame.</param>
            public Frame(Associative frameBindings, Associative bindings, Frame prev)
            {
                _frameBindings = frameBindings;
                _bindings = bindings;
                _prev = prev;
            }

            #endregion
        }

        #region Data

        /// <summary>
        /// The current frame.  Thread-local.
        /// </summary>
        [ThreadStatic]
        private static Frame _currentFrame;

        /// <summary>
        /// Get/set the current frame.
        /// </summary>
        /// <remarks>Best to make all access to _currentFrame through this accessor.</remarks>
        private static Frame CurrentFrame
        {
            get
            {
                if (_currentFrame == null)
                    _currentFrame = new Frame();
                return _currentFrame;
            }
            set
            {
                _currentFrame = value;
            }
        }

        /// <summary>
        /// Special value for the root to indicate the root is unbound.
        /// </summary>
        static object _rootUnboundValue = new object();

        /// <summary>
        /// The root value.
        /// </summary>
        volatile object _root;

        static Keyword _privateKey = Keyword.intern(null, "private");
        static IPersistentMap _privateMeta = new PersistentArrayMap(new object[] { _privateKey, true });
        static Keyword _macroKey = Keyword.intern(null, "macro");
        static Keyword _nameKey = Keyword.intern(null, "name");
        static Keyword _nsKey = Keyword.intern(null, "ns");

        /// <summary>
        /// The number of bindings for this var on the binding stack.
        /// </summary>
        [NonSerialized]
        AtomicInteger _count;        
        
        /// <summary>
        /// The symbol naming this var, if named.
        /// </summary>
        readonly Symbol _sym;

        /// <summary>
        /// Get the symbol naming this var, if named.
        /// </summary>
        internal Symbol Symbol
        {
            get { return _sym; }
        }

        /// <summary>
        /// The namespace holding this var.
        /// </summary>
        readonly Namespace _ns;

        /// <summary>
        /// Get the namespace holding this var.
        /// </summary>
        public Namespace Namespace
        {
            get { return _ns; }
        }

        #endregion

        #region C-tors & factory methods

        /// <summary>
        /// Intern a named var in a namespace, with given value.
        /// </summary>
        /// <param name="ns">The namespace.</param>
        /// <param name="sym">The name.</param>
        /// <param name="root">The root value.</param>
        /// <returns>The var that was found or created.</returns>
        public static Var intern(Namespace ns, Symbol sym, object root)
        {
            return intern(ns, sym, root, true);
        }

        /// <summary>
        /// Intern a named var in a namespace, with given value (if has a root value already, then change only if replaceRoot is true).
        /// </summary>
        /// <param name="ns">The namespace.</param>
        /// <param name="sym">The name.</param>
        /// <param name="root">The root value.</param>
        /// <param name="replaceRoot">Replace an existing root value if <value>true</value>.</param>
        /// <returns>The var that was found or created.</returns>
        public static Var intern(Namespace ns, Symbol sym, object root, bool replaceRoot)
        {
            Var dvout = ns.intern(sym);
            if (!dvout.hasRoot() || replaceRoot)
                dvout.BindRoot(root);
            return dvout;
        }

        /// <summary>
        /// Intern a named var in a namespace (creating the namespece if necessary).
        /// </summary>
        /// <param name="nsName">The name of the namespace.  (A namespace with this name will be created if not existing already.)</param>
        /// <param name="sym">The name of the var.</param>
        /// <returns>The var that was found or created.</returns>
        public static Var intern(Symbol nsName, Symbol sym)
        {
            Namespace ns = Namespace.findOrCreate(nsName);
            return intern(ns, sym);
        }

        /// <summary>
        /// Intern a named var (flagged private) in a namespace (creating the namespece if necessary).
        /// </summary>
        /// <param name="nsName">The name of the namespace.  (A namespace with this name will be created if not existing already.)</param>
        /// <param name="sym">The name of the var.</param>
        /// <returns>The var that was found or created.</returns>
        /// <remarks>Added in Java Rev 1110.</remarks>
        public static Var internPrivate(string nsName, String sym)
        {
            Namespace ns = Namespace.findOrCreate(Symbol.intern(nsName));
            Var ret = intern(ns, Symbol.intern(sym));
            ret.SetMeta(_privateMeta);
            return ret;
        }

        /// <summary>
        /// Intern a named var in a namespace.
        /// </summary>
        /// <param name="ns">The namespace.</param>
        /// <param name="sym">The name.</param>
        /// <returns></returns>
        public static Var intern(Namespace ns, Symbol sym)
        {
            return ns.intern(sym);
        }

        /// <summary>
        /// Create an uninterned var.
        /// </summary>
        /// <returns>An uninterned var.</returns>
        public static Var create()
        {
            return new Var(null, null);
        }

        /// <summary>
        /// Create an uninterned var with a root value.
        /// </summary>
        /// <param name="root">The root value.</param>
        /// <returns>An uninterned var.</returns>
        public static Var create(object root)
        {
            return new Var(null, null, root);
        }

        /// <summary>
        /// Construct a var in a given namespace with a given name.
        /// </summary>
        /// <param name="ns">The namespace.</param>
        /// <param name="sym">The var.</param>
        internal Var(Namespace ns, Symbol sym)
        {
            _ns = ns;
            _sym = sym;
            _count = new AtomicInteger();
            _root = _rootUnboundValue;
            SetMeta(PersistentHashMap.EMPTY);
        }


        /// <summary>
        /// Construct a var in a given namespace with a given name and root value.
        /// </summary>
        /// <param name="ns">The namespace.</param>
        /// <param name="sym">The var.</param>
        /// <param name="root">The root value.</param>
        Var(Namespace ns, Symbol sym, object root)
            : this(ns, sym)
        {
            _root = root;
        }

        #endregion

        #region object overrides

        /// <summary>
        /// Return a string representing this var.
        /// </summary>
        /// <returns>A string representing this var.</returns>
        public override string ToString()
        {
            return (_ns != null)
                ? "#'" + _ns.Name + "/" + _sym
                : "#<Var: " + (_sym != null ? _sym.ToString() : "--unnamed--") + ">";
        }

        #endregion

        #region Flag management

        /// <summary>
        /// Set the metadata attached to this var.
        /// </summary>
        /// <param name="m">The metadata to attach.</param>
        /// <remarks>The metadata must contain entries for the namespace and name.</remarks>
        public void SetMeta(IPersistentMap m)
        { 
            // ensure these basis keys
            resetMeta(m.assoc(_nameKey, _sym).assoc(_nsKey, _ns));
        }

        /// <summary>
        /// Add a macro=true flag to the metadata.
        /// </summary>
        /// <remarks>Lowercase name for core.clj compatability.</remarks>
        public void setMacro()
        {
            alterMeta(_assoc,RT.list(_macroKey, RT.T));
        }

        /// <summary>
        /// Is the var a macro?
        /// </summary>
        public bool IsMacro
        {
            get { return RT.booleanCast(meta().valAt(_macroKey)); }
        }

        /// <summary>
        /// Is the var public?
        /// </summary>
        public bool IsPublic
        {
            get { return !RT.booleanCast(meta().valAt(_privateKey)); }
        }

        /// <summary>
        /// Get the tag on the var.
        /// </summary>
        /// <remarks>In Java code, setTag takes only Symbols.  Don't know why.  I ran into a problem when I changed the type to Symbol.</remarks>
        public object Tag
        {
            get { return meta().valAt(RT.TAG_KEY); }
            set { alterMeta(_assoc,RT.list(RT.TAG_KEY, value)); }
        }

        #endregion

        #region Value management

        /// <summary>
        /// Does the var have value?
        /// </summary>
        public bool IsBound
        {
            get { return hasRoot() || (_count.get() > 0 && CurrentFrame.Bindings.containsKey(this)); }
        }

       
        /// <summary>
        /// Does the var have a root value?
        /// </summary>
        /// <returns></returns>
        /// <remarks>core.clj compatibility (initial lowercase/ public /method-instead-of-property)</remarks>
        public bool hasRoot()
        {
            return _root != _rootUnboundValue;
        }

        /// <summary>
        /// 
        /// </summary>
        /// <returns>The root value.</returns>
        object getRoot()
        {
            return _root;
        }

        // In the Java version, haven't missed it yet.
        //public object alter(IFn fn, ISeq args)
        //{
        //    set(fn.applyTo(RT.cons(deref(), args)));
        //    return this;
        //}

        /// <summary>
        /// Set the value of the var.
        /// </summary>
        /// <param name="val">The new value.</param>
        /// <returns>the new value.</returns>
        /// <remarks>It is an error to set the root binding with this method.</remarks>
        public object set(object val)
        {
            Validate(getValidator(), val);
            Box b = GetThreadBinding();
            if (b != null)
                return (b.Val = val);
            throw new InvalidOperationException(String.Format("Can't change/establish root binding of: {0} with set", _sym));
        }

        /// <summary>
        /// Change the root value.  (And clear the macro flag.)
        /// </summary>
        /// <param name="root">The new value.</param>
        /// <remarks>binding root clears macro flag</remarks>
        [MethodImpl(MethodImplOptions.Synchronized)]
        public void BindRoot(object root)
        {
            Validate(getValidator(), root);
            _root = root;
            alterMeta(_assoc,RT.list(_macroKey, RT.F));
            notifyWatches();
        }


        ///// <summary>
        ///// Change the root value.
        ///// </summary>
        ///// <param name="root">The new value.</param>
        //[MethodImpl(MethodImplOptions.Synchronized)]
        //void SwapRoot(object root)
        //{
        //    Validate(getValidator(), root);
        //    _root = root;
        //    notifyWatches();
        //}

        ///// <summary>
        ///// Unbind the var's root value.
        ///// </summary>
        //[MethodImpl(MethodImplOptions.Synchronized)]
        //void UnbindRoot()
        //{
        //    _root = _rootUnboundValue;
        //}

        ///// <summary>
        ///// Set var's root to a computed value.
        ///// </summary>
        ///// <param name="fn">The function to apply to the current value to get the new value.</param>
        //[MethodImpl(MethodImplOptions.Synchronized)]
        //void CommuteRoot(IFn fn)
        //{
        //    object newRoot = fn.invoke(_root);
        //    Validate(getValidator(), newRoot);
        //    _root = newRoot;
        //    notifyWatches();
        //}

        /// <summary>
        /// Change the var's root to a computed value (based on current value and supplied arguments).
        /// </summary>
        /// <param name="fn">The function to compute the new value.</param>
        /// <param name="args">Additional arguments.</param>
        /// <returns>The new value.</returns>
        /// <remarks> initial lowercase in name needed for core.clj</remarks>
        [MethodImpl(MethodImplOptions.Synchronized)]
        public object alterRoot(IFn fn, ISeq args)
        {
            object newRoot = fn.applyTo(RT.cons(_root, args));
            Validate(getValidator(), newRoot);
            _root = newRoot;
            notifyWatches();
            return newRoot;
        }


        #endregion

        #region Binding stack

        /// <summary>
        /// Push a new frame of bindings onto the binding stack.
        /// </summary>
        /// <param name="bindings">The new bindings.</param>
        /// <remarks>Lowercase name for core.clj compatability.</remarks>
        public static void pushThreadBindings(Associative bindings)
        {
            Frame f = CurrentFrame;
            Associative bmap = f.Bindings;
            for (ISeq bs = bindings.seq(); bs != null; bs = bs.rest())
            {
                IMapEntry e = (IMapEntry)bs.first();
                Var v = (Var)e.key();
                v.Validate(e.val());
                v._count.incrementAndGet();
                bmap = bmap.assoc(v, new Box(e.val()));
            }
            CurrentFrame = new Frame(bindings, bmap, f);
        }

        /// <summary>
        /// Pop the topmost binding frame from the stack.
        /// </summary>
        /// <remarks>Lowercase name for core.clj compatability.</remarks>
        public static void popThreadBindings()
        {
            Frame f = CurrentFrame;
            if (f.Prev == null)
                throw new InvalidOperationException("Pop without matching push");
            for (ISeq bs = RT.keys(f.FrameBindings); bs != null; bs = bs.rest())
            {
                Var v = (Var)bs.first();
                v._count.decrementAndGet();
            }
            CurrentFrame = f.Prev;
        }


        /// <summary>
        /// Pop all binding frames from teh stack.
        /// </summary>
        static void ReleaseThreadBindings()
        {
            Frame f = CurrentFrame;
            if (f.Prev == null)
                throw new InvalidOperationException("Release without full unwind");
            for (ISeq bs = RT.keys(f.Bindings); bs != null; bs = bs.rest())
            {
                Var v = (Var)bs.first();
                v._count.decrementAndGet(); ;
            }
            CurrentFrame = null;
        }

        /// <summary>
        /// Get the box of the current binding on the stack for this var, or null if no binding.
        /// </summary>
        /// <returns>The box of the current binding on the stack (or null if no binding).</returns>
        Box GetThreadBinding()
        {
            if (_count.get() > 0)
            {
                IMapEntry e = CurrentFrame.Bindings.entryAt(this);
                if (e != null)
                    return (Box)e.val();
            }
            return null;
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

        public  object invoke(object arg1, object arg2, object arg3, object arg4)
        {
            return fn().invoke(arg1, arg2, arg3, arg4);
        }

        public  object invoke(object arg1, object arg2, object arg3, object arg4, object arg5)
        {
            return fn().invoke(arg1, arg2, arg3, arg4, arg5);
        }

        public  object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6)
        {
            return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6);
        }

        public  object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7)
        {
            return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7);
        }

        public  object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8)
        {
            return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
        }

        public  object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9)
        {
            return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
        }

        public  object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10)
        {
            return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
        }

        public  object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11)
        {
            return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
        }

        public  object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11, object arg12)
        {
            return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
        }

        public  object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11, object arg12, object arg13)
        {
            return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13);
        }

        public  object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14)
        {
            return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14);
        }

        public  object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14,
                             object arg15)
        {
            return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15);
        }

        public  object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14,
                             object arg15, object arg16)
        {
            return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15,
                               arg16);
        }

        public  object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14,
                             object arg15, object arg16, object arg17)
        {
            return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15,
                               arg16, arg17);
        }

        public  object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14,
                             object arg15, object arg16, object arg17, object arg18)
        {
            return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15,
                               arg16, arg17, arg18);
        }

        public  object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14,
                             object arg15, object arg16, object arg17, object arg18, object arg19)
        {
            return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15,
                               arg16, arg17, arg18, arg19);
        }

        public  object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14,
                             object arg15, object arg16, object arg17, object arg18, object arg19, object arg20)
        {
            return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15,
                               arg16, arg17, arg18, arg19, arg20);
        }

        public  object invoke(object arg1, object arg2, object arg3, object arg4, object arg5, object arg6, object arg7,
                             object arg8, object arg9, object arg10, object arg11, object arg12, object arg13, object arg14,
                             object arg15, object arg16, object arg17, object arg18, object arg19, object arg20,
                             params object[] args)
        {
            return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15,
                               arg16, arg17, arg18, arg19, arg20, args);
        }

        public  object applyTo(ISeq arglist)
        {
            return AFn.ApplyToHelper(this, arglist);
        }

        #endregion

        #region IDeref Members

        ///// <summary>
        ///// Gets the (immutable) value the reference is holding.
        ///// </summary>
        ///// <returns>The value</returns>
        ///// <remarks>When IDeref was added and get() was renamed to deref(), this was put in.  
        ///// Why?  Perhaps to avoid having to change Var.get() references all over.  
        ///// I don't know.
        ///// But then they rename all uses anyway.</remarks>
        //public object get()
        //{
        //    return deref();
        //}

        /// <summary>
        /// Gets the (immutable) value the reference is holding.
        /// </summary>
        /// <returns>The value</returns>
        public override object deref()
        {
            Box b = GetThreadBinding();
            if (b != null)
                return b.Val;
            if (hasRoot())
                return _root;
            throw new InvalidOperationException(String.Format("Var {0}/{1} is unbound.", _ns,_sym));
        }


        /// <summary>
        /// Sets the validator.
        /// </summary>
        /// <param name="vf">The new validtor</param>
        public override void setValidator(IFn vf)
        {
            if (IsBound)
                Validate(vf, getRoot());
            _validator = vf;
        }

        #endregion

        #region core.clj compatibility methods

        /// <summary>
        /// Find the var from a namespace-qualified symbol.
        /// </summary>
        /// <param name="nsQualifiedSym">A namespace-qualified symbol.</param>
        /// <returns>The var, if found.</returns>
        public static Var find(Symbol nsQualifiedSym)
        {
            if (nsQualifiedSym.Namespace == null)
                throw new ArgumentException("Symbol must be namespace-qualified");
            Namespace ns = Namespace.find(Symbol.create(nsQualifiedSym.Namespace));
            if (ns == null)
                throw new ArgumentException("No such namespace: " + nsQualifiedSym.Namespace);
            return ns.FindInternedVar(Symbol.create(nsQualifiedSym.Name));
        }

        /// <summary>
        /// The namespace this var is interned in.
        /// </summary>
        /// <returns></returns>
        public Namespace ns()
        {
            return Namespace;
        }

        /// <summary>
        /// Is this var public?
        /// </summary>
        /// <returns></returns>
        public bool isPublic()
        {
            return IsPublic;
        }

        #endregion

        #region Settable Members

        /// <summary>
        /// Sets the value.
        /// </summary>
        /// <param name="val">The new value</param>
        /// <returns>The new value.</returns>
        /// <remarks>Can only be called with a binding on the stack, else throws an exception.</remarks>
        public object doSet(object val)
        {
            return set(val);
        }

        /// <summary>
        /// Sets the root value.
        /// </summary>
        /// <param name="val">The new value</param>
        /// <returns>The new value.</returns>
        public object doReset(object val)
        {
            BindRoot(val);
            return val;
        }

        #endregion

        #region other

        class AssocFn : AFn
        {
            public override object invoke(object m, object k, object v)
            {
                return RT.assoc(m,k,v);
            }
        }

        /// <summary>
        /// Used in calls to alterMeta, above.
        /// </summary>
        static IFn _assoc = new AssocFn();

        #endregion
    }
}
