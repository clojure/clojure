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
using System.IO;
using System.Threading;

namespace clojure.lang
{
    public class Compiler
    {

        #region Symbols

        public static readonly Symbol DEF = Symbol.create("def");
        public static readonly Symbol LOOP = Symbol.create("loop*");
        public static readonly Symbol RECUR = Symbol.create("recur");
        public static readonly Symbol IF = Symbol.create("if");
        public static readonly Symbol LET = Symbol.create("let*");
        public static readonly Symbol DO = Symbol.create("do");
        public static readonly Symbol FN = Symbol.create("fn*");
        public static readonly Symbol QUOTE = Symbol.create("quote");
        public static readonly Symbol THE_VAR = Symbol.create("var");
        public static readonly Symbol DOT = Symbol.create(".");
        public static readonly Symbol ASSIGN = Symbol.create("set!");
        public static readonly Symbol TRY = Symbol.create("try");
        public static readonly Symbol CATCH = Symbol.create("catch");
        public static readonly Symbol FINALLY = Symbol.create("finally");
        public static readonly Symbol THROW = Symbol.create("throw");
        public static readonly Symbol MONITOR_ENTER = Symbol.create("monitor-enter");
        public static readonly Symbol MONITOR_EXIT = Symbol.create("monitor-exit");
        public static readonly Symbol NEW = Symbol.create("new");
        public static readonly Symbol _AMP_ = Symbol.create("&");


        public static readonly Symbol IDENTITY = Symbol.create("clojure.core", "identity");

        static readonly Symbol NS = Symbol.create("ns");
        static readonly Symbol IN_NS = Symbol.create("in-ns");

        #endregion

        #region Vars

        //boolean
        static readonly Var COMPILE_FILES = Var.intern(Namespace.findOrCreate(Symbol.create("clojure.core")),
                                                 Symbol.create("*compile-files*"), false);  //JAVA: Boolean.FALSE -- changed from RT.F in rev 1108, not sure why

        //String
        static readonly Var COMPILE_PATH = Var.intern(Namespace.findOrCreate(Symbol.create("clojure.core")),
                                                 Symbol.create("*compile-path*"), null);
        // String
        static readonly Var SOURCE_PATH = Var.intern(Namespace.findOrCreate(Symbol.create("clojure.core")),
            Symbol.create("*file*"), null);

        #endregion

        #region Special forms

        public static readonly IPersistentSet _specials = PersistentHashSet.create(
            DEF,
            LOOP, 
            RECUR, 
            IF, 
            LET, 
            DO, 
            FN, 
            QUOTE, 
            THE_VAR, 
            DOT, 
            ASSIGN, 
            TRY, 
            THROW, 
            MONITOR_ENTER, 
            MONITOR_EXIT, 
            CATCH, 
            FINALLY, 
            NEW, 
            _AMP_ 
        );

        public static bool isSpecial(Object sym)
        {
            return _specials.contains(sym);
        }

        #endregion

        #region Symbol/namespace resolving

        // TODO: we have duplicate code below.

        public static Symbol resolveSymbol(Symbol sym)
        {
            //already qualified or classname?
            if (sym.Name.IndexOf('.') > 0)
                return sym;
            if (sym.Namespace != null)
            {
                Namespace ns = namespaceFor(sym);
                if (ns == null || ns.Name.Name == sym.Namespace)
                    return sym;
                return Symbol.create(ns.Name.Name, sym.Name);
            }
            Object o = CurrentNamespace.GetMapping(sym);
            if (o == null)
                return Symbol.intern(CurrentNamespace.Name.Name, sym.Name);
            else if (o is Type)
                return Symbol.intern(null, ((Type)o).Name);
            else if (o is Var)
            {
                Var v = (Var)o;
                return Symbol.create(v.Namespace.Name.Name, v.Symbol.Name);
            }
            return null;

        }


        public static Namespace namespaceFor(Symbol sym)
        {
            return namespaceFor(CurrentNamespace, sym);
        }

        public static Namespace namespaceFor(Namespace inns, Symbol sym)
        {
            //note, presumes non-nil sym.ns
            // first check against currentNS' aliases...
            Symbol nsSym = Symbol.create(sym.Namespace);
            Namespace ns = inns.LookupAlias(nsSym);
            if (ns == null)
            {
                // ...otherwise check the Namespaces map.
                ns = Namespace.find(nsSym);
            }
            return ns;
        }

        public static Namespace CurrentNamespace
        {
            get { return (Namespace)RT.CURRENT_NS.deref(); }
        }



        public static object Resolve(Symbol symbol, bool allowPrivate)
        {
            return ResolveIn(CurrentNamespace, symbol, allowPrivate);
        }

        public static object Resolve(Symbol symbol)
        {
            return ResolveIn(CurrentNamespace, symbol, false);
        }

        private static object ResolveIn(Namespace n, Symbol symbol, bool allowPrivate)
        {
            // note: ns-qualified vars must already exist
            if (symbol.Namespace != null)
            {
                Namespace ns = NamespaceFor(n, symbol);
                if (ns == null)
                    throw new Exception("No such namespace: " + symbol.Namespace);

                Var v = ns.FindInternedVar(Symbol.create(symbol.Name));
                if (v == null)
                    throw new Exception("No such var: " + symbol);
                else if (v.Namespace != CurrentNamespace && !v.IsPublic && !allowPrivate)
                    throw new InvalidOperationException(string.Format("var: {0} is not public", symbol));
                return v;
            }
            else if (symbol.Name.IndexOf('.') > 0 || symbol.Name[0] == '[')
                return RT.classForName(symbol.Name);
            else if (symbol.Equals(NS))
                return RT.NS_VAR;
            else if (symbol.Equals(IN_NS))
                return RT.IN_NS_VAR;
            else
            {
                object o = n.GetMapping(symbol);
                if (o == null)
                {
                    if (RT.booleanCast(RT.ALLOW_UNRESOLVED_VARS.deref()))
                        return symbol;
                    else
                        throw new Exception(string.Format("Unable to resolve symbol: {0} in this context", symbol));
                }
                return o;
            }
        }

        // core.clj compatibility
        public static object maybeResolveIn(Namespace n, Symbol symbol)
        {
            // note: ns-qualified vars must already exist
            if (symbol.Namespace != null)
            {
                Namespace ns = NamespaceFor(n, symbol);
                if (ns == null)
                    return null;

                Var v = ns.FindInternedVar(Symbol.create(symbol.Name));
                if (v == null)
                    return null;
                 return v;
            }
            else if (symbol.Name.IndexOf('.') > 0 || symbol.Name[0] == '[')
                return RT.classForName(symbol.Name);
            else if (symbol.Equals(NS))
                return RT.NS_VAR;
            else if (symbol.Equals(IN_NS))
                return RT.IN_NS_VAR;
            else
            {
                object o = n.GetMapping(symbol);
                return o;
            }
        }

        public  static Namespace NamespaceFor(Symbol symbol)
        {
            return NamespaceFor(CurrentNamespace, symbol);
        }

        public  static Namespace NamespaceFor(Namespace n, Symbol symbol)
        {
            // Note: presumes non-nil sym.ns
            // first check against CurrentNamespace's aliases
            Symbol nsSym = Symbol.create(symbol.Namespace);
            Namespace ns = n.LookupAlias(nsSym);
            if (ns == null)
                // otherwise, check the namespaces map
                ns = Namespace.find(nsSym);
            return ns;
        }

        #endregion

        #region Hooks from execution engine

        public interface EEHooks
        {
            object Eval(object form);
            object Macroexpand1(object form);
            object LoadFromStream(TextReader rdr);
            object LoadFile(string filename);
            Delegate GenerateTypedDelegate(Type delegateType, Symbol optName, IPersistentVector argList, ISeq body);
        }

        // Needs to be initialized by the execution engine.
        private static EEHooks _hooks;

        public static bool SetHooks(EEHooks hooks)
        {
            return null == Interlocked.CompareExchange(ref _hooks,hooks,null);
        }


        // The following methods are named (and initial LC) for core.clj compatibility

        public static object eval(object form)
        {
            ValidateHooks();

            return _hooks.Eval(form);
        }

        public static object macroexpand1(object form)
        {
            ValidateHooks();

            return _hooks.Macroexpand1(form);
        }

        public static object load(TextReader rdr)
        {
            ValidateHooks();
            return _hooks.LoadFromStream(rdr);
        }


        // This one is mine.
        public static object GenerateTypedDelegate(Type delegateType, Symbol optName, IPersistentVector argList, ISeq body)
        {
            ValidateHooks();
            return _hooks.GenerateTypedDelegate(delegateType, optName, argList, body);
        }

        private static void ValidateHooks()
        {
            if ( _hooks == null )
                throw new InvalidOperationException("Hooks from execution engine not set yet.  Major blowage.");
        }

        #endregion

        // Added in revision 1108, not sure where it is used.  (Made public in rev 1109)
        public static void pushNS()
        {
            Var.pushThreadBindings(PersistentHashMap.create(Var.intern(Symbol.create("clojure.core"),
                                                                       Symbol.create("*ns*")), null));
        }


        // Java version has this in Reflector, but that is in my SimpleREPL. DOn't want to embed calls there.
        public static Object prepRet(Object x)
        {
            //	if(c == boolean.class)
            //		return ((Boolean) x).booleanValue() ? RT.T : null;
            if (x is Boolean)
                return ((Boolean)x) ? RT.T : RT.F;
            return x;
        }
    }
}
