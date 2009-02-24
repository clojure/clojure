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
using System.Collections;

using System.Reflection;
using System.Text.RegularExpressions;
using System.Threading;
using System.IO;
using System.Diagnostics;


namespace clojure.lang
{
    public static class RT
    {
        #region Default symbol-to-class map

        //simple-symbol->class
        internal static readonly IPersistentMap DEFAULT_IMPORTS = map(
            //classes
            Symbol.create("AccessViolationException"), typeof(AccessViolationException),
            Symbol.create("ActivationContext"), typeof(ActivationContext),
            Symbol.create("Activator"), typeof(Activator),
            Symbol.create("AppDomain"), typeof(AppDomain),
            Symbol.create("AppDomainManager"), typeof(AppDomainManager),
            Symbol.create("AppDomainSetup"), typeof(AppDomainSetup),
            Symbol.create("AppDomainUnloadedException"), typeof(AppDomainUnloadedException),
            Symbol.create("ApplicationException"), typeof(ApplicationException),
            Symbol.create("ApplicationId"), typeof(ApplicationId),
            Symbol.create("ApplicationIdentity"), typeof(ApplicationIdentity),
            Symbol.create("ArgumentException"), typeof(ArgumentException),
            Symbol.create("ArgumentNullException"), typeof(ArgumentNullException),
            Symbol.create("ArgumentOutOfRangeException"), typeof(ArgumentOutOfRangeException),
            Symbol.create("ArithmeticException"), typeof(ArithmeticException),
            Symbol.create("Array"), typeof(Array),
            Symbol.create("ArrayTypeMismatchException"), typeof(ArrayTypeMismatchException),
            Symbol.create("AssemblyLoadEventArgs"), typeof(AssemblyLoadEventArgs),
            Symbol.create("Attribute"), typeof(Attribute),
            Symbol.create("AttributeUsageAttribute"), typeof(AttributeUsageAttribute),
            Symbol.create("BadImageFormatException"), typeof(BadImageFormatException),
            Symbol.create("BitConverter"), typeof(BitConverter),
            Symbol.create("Buffer"), typeof(Buffer),
            Symbol.create("CannotUnloadAppDomainException"), typeof(CannotUnloadAppDomainException),
            Symbol.create("CharEnumerator"), typeof(CharEnumerator),
            Symbol.create("CLSCompliantAttribute"), typeof(CLSCompliantAttribute),
            Symbol.create("Console"), typeof(Console),
            Symbol.create("ConsoleCancelEventArgs"), typeof(ConsoleCancelEventArgs),
            Symbol.create("ContextBoundObject"), typeof(ContextBoundObject),
            //Symbol.create("ContextMarshalException"), typeof(ContextMarshalException), -- obsolete
            Symbol.create("ContextStaticAttribute"), typeof(ContextStaticAttribute),
            Symbol.create("Convert"), typeof(Convert),
            Symbol.create("DataMisalignedException"), typeof(DataMisalignedException),
            Symbol.create("DBNull"), typeof(DBNull),
            Symbol.create("Delegate"), typeof(Delegate),
            Symbol.create("DivideByZeroException"), typeof(DivideByZeroException),
            Symbol.create("DllNotFoundException"), typeof(DllNotFoundException),
            Symbol.create("DuplicateWaitObjectException"), typeof(DuplicateWaitObjectException),
            Symbol.create("EntryPointNotFoundException"), typeof(EntryPointNotFoundException),
            Symbol.create("Enum"), typeof(Enum),
            Symbol.create("Environment"), typeof(Environment),
            Symbol.create("EventArgs"), typeof(EventArgs),
            Symbol.create("Exception"), typeof(Exception),
            Symbol.create("ExecutionEngineException"), typeof(ExecutionEngineException),
            Symbol.create("FieldAccessException"), typeof(FieldAccessException),
            Symbol.create("FileStyleUriParser"), typeof(FileStyleUriParser),
            Symbol.create("FlagsAttribute"), typeof(FlagsAttribute),
            Symbol.create("FormatException"), typeof(FormatException),
            Symbol.create("FtpStyleUriParser"), typeof(FtpStyleUriParser),
            Symbol.create("GC"), typeof(GC),
            Symbol.create("GenericUriParser"), typeof(GenericUriParser),
            Symbol.create("GopherStyleUriParser"), typeof(GopherStyleUriParser),
            Symbol.create("HttpStyleUriParser"), typeof(HttpStyleUriParser),
            Symbol.create("IndexOutOfRangeException"), typeof(IndexOutOfRangeException),
            Symbol.create("InsufficientMemoryException"), typeof(InsufficientMemoryException),
            Symbol.create("InvalidCastException"), typeof(InvalidCastException),
            Symbol.create("InvalidOperationException"), typeof(InvalidOperationException),
            Symbol.create("InvalidProgramException"), typeof(InvalidProgramException),
            Symbol.create("InvalidTimeZoneException"), typeof(InvalidTimeZoneException),
            Symbol.create("LdapStyleUriParser"), typeof(LdapStyleUriParser),
            Symbol.create("LoaderOptimizationAttribute"), typeof(LoaderOptimizationAttribute),
            Symbol.create("LocalDataStoreSlot"), typeof(LocalDataStoreSlot),
            Symbol.create("MarshalByRefObject"), typeof(MarshalByRefObject),
            Symbol.create("Math"), typeof(Math),
            Symbol.create("MemberAccessException"), typeof(MemberAccessException),
            Symbol.create("MethodAccessException"), typeof(MethodAccessException),
            Symbol.create("MissingFieldException"), typeof(MissingFieldException),
            Symbol.create("MissingMemberException"), typeof(MissingMemberException),
            Symbol.create("MTAThreadAttribute"), typeof(MTAThreadAttribute),
            Symbol.create("MulticastDelegate"), typeof(MulticastDelegate),
            Symbol.create("MulticastNotSupportedException"), typeof(MulticastNotSupportedException),
            Symbol.create("NetPipeStyleUriParser"), typeof(NetPipeStyleUriParser),
            Symbol.create("NetTcpStyleUriParser"), typeof(NetTcpStyleUriParser),
            Symbol.create("NewsStyleUriParser"), typeof(NewsStyleUriParser),
            Symbol.create("NonSerializedAttribute"), typeof(NonSerializedAttribute),
            Symbol.create("NotFiniteNumberException"), typeof(NotFiniteNumberException),
            Symbol.create("NotImplementedException"), typeof(NotImplementedException),
            Symbol.create("NotSupportedException"), typeof(NotSupportedException),
            Symbol.create("Nullable"), typeof(Nullable),
            Symbol.create("NullReferenceException"), typeof(NullReferenceException),
            Symbol.create("Object"), typeof(Object),
            Symbol.create("ObjectDisposedException"), typeof(ObjectDisposedException),
            Symbol.create("ObsoleteAttribute"), typeof(ObsoleteAttribute),
            Symbol.create("OperatingSystem"), typeof(OperatingSystem),
            Symbol.create("OperationCanceledException"), typeof(OperationCanceledException),
            Symbol.create("OutOfMemoryException"), typeof(OutOfMemoryException),
            Symbol.create("OverflowException"), typeof(OverflowException),
            Symbol.create("ParamArrayAttribute"), typeof(ParamArrayAttribute),
            Symbol.create("PlatformNotSupportedException"), typeof(PlatformNotSupportedException),
            Symbol.create("Random"), typeof(Random),
            Symbol.create("RankException"), typeof(RankException),
            Symbol.create("ResolveEventArgs"), typeof(ResolveEventArgs),
            Symbol.create("SerializableAttribute"), typeof(SerializableAttribute),
            Symbol.create("StackOverflowException"), typeof(StackOverflowException),
            Symbol.create("STAThreadAttribute"), typeof(STAThreadAttribute),
            Symbol.create("String"), typeof(String),
            Symbol.create("StringComparer"), typeof(StringComparer),
            Symbol.create("SystemException"), typeof(SystemException),
            Symbol.create("ThreadStaticAttribute"), typeof(ThreadStaticAttribute),
            Symbol.create("TimeoutException"), typeof(TimeoutException),
            Symbol.create("TimeZone"), typeof(TimeZone),
            Symbol.create("TimeZoneInfo"), typeof(TimeZoneInfo),
            Symbol.create("TimeZoneNotFoundException"), typeof(TimeZoneNotFoundException),
            // Symbol.create("TimeZoneInfo.AdjustmentRule"),typeof(TimeZoneInfo.AdjustmentRule),
            Symbol.create("Type"), typeof(Type),
            Symbol.create("TypeInitializationException"), typeof(TypeInitializationException),
            Symbol.create("TypeLoadException"), typeof(TypeLoadException),
            Symbol.create("TypeUnloadedException"), typeof(TypeUnloadedException),
            Symbol.create("UnauthorizedAccessException"), typeof(UnauthorizedAccessException),
            Symbol.create("UnhandledExceptionEventArgs"), typeof(UnhandledExceptionEventArgs),
            Symbol.create("Uri"), typeof(Uri),
            Symbol.create("UriBuilder"), typeof(UriBuilder),
            Symbol.create("UriFormatException"), typeof(UriFormatException),
            Symbol.create("UriParser"), typeof(UriParser),
            // Symbol.create(""),typeof(UriTemplate),
            // Symbol.create(""),typeof(UriTemplateEquivalenceComparer),
            // Symbol.create(""),typeof(UriTemplateMatch),
            // Symbol.create(""),typeof(UriTemplateMatchException),
            // Symbol.create(""),typeof(UriTemplateTable),
            Symbol.create("UriTypeConverter"), typeof(UriTypeConverter),
            Symbol.create("ValueType"), typeof(ValueType),
            Symbol.create("Version"), typeof(Version),
            Symbol.create("WeakReference"), typeof(WeakReference),
            // structures/
            Symbol.create("ArgIterator"), typeof(ArgIterator),
            // Symbol.create(""),typeof(ArraySegment<T>),
            Symbol.create("Boolean"), typeof(Boolean),
            Symbol.create("Byte"), typeof(Byte),
            Symbol.create("Char"), typeof(Char),
            Symbol.create("ConsoleKeyInfo"), typeof(ConsoleKeyInfo),
            Symbol.create("DateTime"), typeof(DateTime),
            Symbol.create("DateTimeOffset"), typeof(DateTimeOffset),
            Symbol.create("Decimal"), typeof(Decimal),
            Symbol.create("Double"), typeof(Double),
            Symbol.create("Guid"), typeof(Guid),
            Symbol.create("Int16"), typeof(Int16),
            Symbol.create("Int32"), typeof(Int32),
            Symbol.create("Int64"), typeof(Int64),
            Symbol.create("IntPtr"), typeof(IntPtr),
            Symbol.create("ModuleHandle"), typeof(ModuleHandle),
            // Symbol.create(""),typeof(Nullable<T>),
            Symbol.create("RuntimeArgumentHandle"), typeof(RuntimeArgumentHandle),
            Symbol.create("RuntimeFieldHandle"), typeof(RuntimeFieldHandle),
            Symbol.create("RuntimeMethodHandle"), typeof(RuntimeMethodHandle),
            Symbol.create("RuntimeTypeHandle"), typeof(RuntimeTypeHandle),
            Symbol.create("SByte"), typeof(SByte),
            Symbol.create("Single"), typeof(Single),
            Symbol.create("TimeSpan"), typeof(TimeSpan),
            Symbol.create("TimeZoneInfo.TransitionTime"), typeof(TimeZoneInfo.TransitionTime),
            Symbol.create("TypedReference"), typeof(TypedReference),
            Symbol.create("UInt16"), typeof(UInt16),
            Symbol.create("UInt32"), typeof(UInt32),
            Symbol.create("UInt64"), typeof(UInt64),
            Symbol.create("UIntPtr"), typeof(UIntPtr),
            // Symbol.create(""),typeof(Void),
            // interfaces/
            Symbol.create("AppDomain"), typeof(AppDomain),
            Symbol.create("IAppDomainSetup"), typeof(IAppDomainSetup),
            Symbol.create("IAsyncResult"), typeof(IAsyncResult),
            Symbol.create("ICloneable"), typeof(ICloneable),
            Symbol.create("IComparable"), typeof(IComparable),
            //Symbol.create(""),typeof(IComparable<T>),
            Symbol.create("IConvertible"), typeof(IConvertible),
            Symbol.create("ICustomFormatter"), typeof(ICustomFormatter),
            Symbol.create("IDisposable"), typeof(IDisposable),
            //Symbol.create(""),typeof(IEquatable<T>),
            Symbol.create("IFormatProvider"), typeof(IFormatProvider),
            Symbol.create("IFormattable"), typeof(IFormattable),
            Symbol.create("IServiceProvider"), typeof(IServiceProvider),
            // delegates/
            Symbol.create("Action"), typeof(Action),
            // Symbol.create(""),typeof(Action<T>/
            // Symbol.create(""),typeof(Action<T1,T2>/
            // Symbol.create(""),typeof(Action<T1,T2,T3>/
            // Symbol.create(""),typeof(Action<T1,T2,T3,T4>/
            Symbol.create("AppDomainInitializer"), typeof(AppDomainInitializer),
            Symbol.create("AssemblyLoadEventHandler"), typeof(AssemblyLoadEventHandler),
            Symbol.create("AsyncCallback"), typeof(AsyncCallback),
            // Symbol.create(""),typeof(Comparison<T>),
            Symbol.create("ConsoleCancelEventHandler"), typeof(ConsoleCancelEventHandler),
            //Symbol.create(""),typeof(Converter<TInput,TOutput>),
            Symbol.create("CrossAppDomainDelegate"), typeof(CrossAppDomainDelegate),
            Symbol.create("EventHandler"), typeof(EventHandler),
            // Symbol.create(""),typeof(EventHandler<TEventArgs>),
            // Symbol.create(""),typeof(Func<TResult>),
            // Symbol.create(""),typeof(Func<T,TResult>/
            // Symbol.create(""),typeof(Func<T1, T2, TResult>/
            // Symbol.create(""),typeof(Func<T1, T2, T3, TResult>/
            // FSymbol.create(""),typeof(Func<T1, T2, T3, T4, TResult>/
            // Symbol.create(""),typeof(Predicate<T>),
            Symbol.create("ResolveEventHandler"), typeof(ResolveEventHandler),
            Symbol.create("UnhandledExceptionEventHandler"), typeof(UnhandledExceptionEventHandler),
            // Enumerations/
            Symbol.create("ActivationContext.ContextForm"), typeof(ActivationContext.ContextForm),
            Symbol.create("AppDomainManagerInitializationOptions"), typeof(AppDomainManagerInitializationOptions),
            Symbol.create("AttributeTargets"), typeof(AttributeTargets),
            Symbol.create("Base64FormattingOptions"), typeof(Base64FormattingOptions),
            Symbol.create("ConsoleColor"), typeof(ConsoleColor),
            Symbol.create("ConsoleKey"), typeof(ConsoleKey),
            Symbol.create("ConsoleModifiers"), typeof(ConsoleModifiers),
            Symbol.create("ConsoleSpecialKey"), typeof(ConsoleSpecialKey),
            Symbol.create("DateTimeKind"), typeof(DateTimeKind),
            Symbol.create("DayOfWeek"), typeof(DayOfWeek),
            Symbol.create("Environment.SpecialFolder"), typeof(Environment.SpecialFolder),
            Symbol.create("EnvironmentVariableTarget"), typeof(EnvironmentVariableTarget),
            Symbol.create("GCCollectionMode"), typeof(GCCollectionMode),
            Symbol.create("GenericUriParserOptions"), typeof(GenericUriParserOptions),
            Symbol.create("LoaderOptimization"), typeof(LoaderOptimization),
            Symbol.create("MidpointRounding"), typeof(MidpointRounding),
            Symbol.create("PlatformID"), typeof(PlatformID),
            Symbol.create("StringComparison"), typeof(StringComparison),
            Symbol.create("StringSplitOptions"), typeof(StringSplitOptions),
            Symbol.create("TypeCode"), typeof(TypeCode),
            Symbol.create("UriComponents"), typeof(UriComponents),
            Symbol.create("UriFormat"), typeof(UriFormat),
            Symbol.create("UriHostNameType"), typeof(UriHostNameType),
            Symbol.create("UriIdnScope"), typeof(UriIdnScope),
            Symbol.create("UriKind"), typeof(UriKind),
            Symbol.create("UriPartial"), typeof(UriPartial),
            // ADDED THESE TO SUPPORT THE BOOTSTRAPPING IN THE JAVA CORE.CLJ
            Symbol.create("StringBuilder"), typeof(StringBuilder),
            Symbol.create("BigInteger"), typeof(java.math.BigInteger),
            Symbol.create("BigDecimal"), typeof(java.math.BigDecimal)
     );

        #endregion

        #region Some misc. goodies

        static public readonly object[] EMPTY_ARRAY = new Object[] { };

        #endregion

        #region  It's true (or not)

        // TODO:  Should these really be object?  In ClojureJVM, we would be trying to avoid boxing.

        public static readonly Boolean T = true;//Keyword.intern(Symbol.create(null, "t"));
        public static readonly Boolean F = false;//Keyword.intern(Symbol.create(null, "t"));

        public static bool IsTrue(object o)
        {
            if (o == null)
                return false;
            if (o is Boolean)
                return (Boolean)o;
            else
                return true;
        }

        #endregion

        #region Predefined namespaces

        // We need this initialization to happen earlier than most of the Var inits.
        public static readonly Namespace CLOJURE_NS 
            = Namespace.findOrCreate(Symbol.create("clojure.core"));

        #endregion

        #region Useful Keywords

        public static readonly Keyword TAG_KEY 
            = Keyword.intern(null, "tag");
        
        public static readonly Keyword LINE_KEY 
            = Keyword.intern(null, "line");

        public static readonly Keyword FILE_KEY
            = Keyword.intern(null, "file");

        #endregion

        #region Vars (namespace-related)

        public static readonly Var CURRENT_NS 
            = Var.intern(CLOJURE_NS, Symbol.create("*ns*"),CLOJURE_NS);


        public static readonly Var IN_NS_VAR 
            = Var.intern(CLOJURE_NS, Symbol.create("in-ns"), F);

        public static readonly Var NS_VAR 
            = Var.intern(CLOJURE_NS, Symbol.create("ns"), F);

        #endregion

        #region Vars (I/O-related)

        // TODO:  These need to be tied into the DLR IO subsystem

        public static readonly Var OUT 
            = Var.intern(CLOJURE_NS, Symbol.create("*out*"), System.Console.Out);
        
        public static readonly Var ERR
            = Var.intern(CLOJURE_NS, Symbol.create("*err*"), System.Console.Error);
        
        public static readonly Var IN =
            Var.intern(CLOJURE_NS, Symbol.create("*in*"),
            new clojure.lang.Readers.LineNumberingReader(System.Console.In));

        static readonly Var PRINT_READABLY 
            = Var.intern(CLOJURE_NS, Symbol.create("*print-readably*"), T);
        
        static readonly Var PRINT_META 
            = Var.intern(CLOJURE_NS, Symbol.create("*print-meta*"), F);
        
        static readonly Var PRINT_DUP 
            = Var.intern(CLOJURE_NS, Symbol.create("*print-dup*"), F);
        
        static readonly Var FLUSH_ON_NEWLINE 
            = Var.intern(CLOJURE_NS, Symbol.create("*flush-on-newline*"), T);
        
        static readonly Var PRINT_INITIALIZED 
            = Var.intern(CLOJURE_NS, Symbol.create("print-initialized"));
        
        static readonly Var PR_ON 
            = Var.intern(CLOJURE_NS, Symbol.create("pr-on"));

        #endregion

        #region Vars (miscellaneous)

        public static readonly Var ALLOW_UNRESOLVED_VARS 
            = Var.intern(CLOJURE_NS, Symbol.create("*allow-unresolved-vars*"), F);
        
        public static readonly Var WARN_ON_REFLECTION 
            = Var.intern(CLOJURE_NS, Symbol.create("*warn-on-reflection*"), F);

        public static readonly Var MACRO_META 
            = Var.intern(CLOJURE_NS, Symbol.create("*macro-meta*"), null);

        public static readonly Var MATH_CONTEXT 
            = Var.intern(CLOJURE_NS, Symbol.create("*math-context*"), null);
        
        public static readonly Var AGENT 
            = Var.intern(CLOJURE_NS, Symbol.create("*agent*"), null);

        public static readonly Var CMD_LINE_ARGS 
            = Var.intern(CLOJURE_NS, Symbol.create("*command-line-args*"), null);

        #endregion

        #region  Clojure-environment IFns needing support

        static readonly Symbol IN_NAMESPACE = Symbol.create("in-ns");

        sealed class InNamespaceFn : AFn
        {
            public override object invoke(object arg1)
            {
                Symbol nsname = (Symbol)arg1;
                Namespace ns = Namespace.findOrCreate(nsname);
                CURRENT_NS.set(ns);
                return ns;
            }
        }
        static readonly Symbol NAMESPACE = Symbol.create("ns");


        static readonly Symbol IDENTICAL = Symbol.create("identical?");

        sealed class IdenticalFn : AFn
        {
            public override object invoke(object arg1, object arg2)
            {
                return Object.ReferenceEquals(arg1, arg2) ? RT.T : RT.F;
            }
        }

        static readonly Symbol LOAD_FILE = Symbol.create("load-file");

        sealed class LoadFileFn : AFn
        {
            public override object invoke(object arg1)
            {
                // TODO: Hook in loading here.
                return base.invoke(arg1);
            }
        }

        #endregion

        #region Initialization

        static RT()
        {
            Keyword dockw = Keyword.intern(null, "doc");
            Keyword arglistskw = Keyword.intern(null, "arglists");
            Symbol namesym = Symbol.create("name");

            OUT.Tag = Symbol.create("System.IO.TextWriter");

            CURRENT_NS.Tag = Symbol.create("closure.lang.Namespace");

            AGENT.SetMeta(map(dockw, "The agent currently running an action on this thread, else nil."));
            AGENT.Tag = Symbol.create("clojure.lang.Agent");

            // We don't have MathContext (yet)
            //MATH_CONTEXT.Tag = Symbol.create("java.math.MathContext");

            // during bootstrap, ns same as in-ns
            Var nv = Var.intern(CLOJURE_NS, NAMESPACE, new InNamespaceFn());
            nv.setMacro();

            Var v;
            v = Var.intern(CLOJURE_NS, IN_NAMESPACE, new InNamespaceFn());
            v.SetMeta(map(dockw, "Sets *ns* to the namespace named by the symbol, creating it if needed.",
                arglistskw, list(vector(namesym))));

            v = Var.intern(CLOJURE_NS, LOAD_FILE, new LoadFileFn());
            v.SetMeta(map(dockw, "Sequentially read and evaluate the set of forms contained in the file.",
                arglistskw, list(vector(namesym))));

            v = Var.intern(CLOJURE_NS, IDENTICAL, new IdenticalFn());
            v.SetMeta(map(dockw, "tests if 2 arguments are the same object",
                arglistskw, list(vector(Symbol.create("x"), Symbol.create("y")))));

            DoInit();
        }

        //  The original Java is doing this here.
        // We're pushing this over to the console, for now.
        // Eventually, we'll push it back here because it is always needed.
        static void DoInit()
        {
            // Eventually, load core.clj and other support files from here (?)
            //load("clojure/core");
            //load("clojure/zip", false);
            //load("clojure/xml", false);
            //load("clojure/set", false);

            //PostBootstrapInit();
        }

        public static void PostBootstrapInit()
        {
            Var.pushThreadBindings(
                RT.map(CURRENT_NS, CURRENT_NS.deref(),
                WARN_ON_REFLECTION, WARN_ON_REFLECTION.deref()));
            try
            {
                Symbol USER = Symbol.create("user");
                Symbol CLOJURE = Symbol.create("clojure.core");

                Var in_ns = var("clojure.core", "in-ns");
                Var refer = var("clojure.core", "refer");
                in_ns.invoke(USER);
                refer.invoke(CLOJURE);
                //maybeLoadResourceScript("user.clj");
            }
            finally
            {
                Var.popThreadBindings();
            }
        }

        #endregion

        #region Id generation

        // This is AtomicInteger in the JVM version.
        // The only place accessed is in nextID, so seems unnecessary.
        private static int _id;

        // initial-lowercase name, used in core.clj
        static public int nextID()
        {
            return Interlocked.Increment(ref _id);
        }

        #endregion

        #region Var support

        static public Var var(String ns, String name)
        {
            return Var.intern(Namespace.findOrCreate(Symbol.intern(null, ns)), Symbol.intern(null, name));
        }

        static public Var var(String ns, String name, Object init)
        {
            return Var.intern(Namespace.findOrCreate(Symbol.intern(null, ns)), Symbol.intern(null, name), init);
        }

        #endregion

        #region Collections support

        public static ISeq seq(object coll)
        {
            if (coll == null)
                return null;
            else if (coll is ISeq)
                return (ISeq)coll;
            else if (coll is IPersistentCollection)
                return ((IPersistentCollection)coll).seq();
            else
                return seqFrom(coll);
        }

        private static ISeq seqFrom(object coll)
        {
            if (coll is IEnumerable)  // java: Iterable
                return EnumeratorSeq.create(((IEnumerable)coll).GetEnumerator());  // IteratorSeq
            else if (coll.GetType().IsArray)
                return ArraySeq.createFromObject(coll);
            else if (coll is string)
                return StringSeq.create((string)coll);
            // The equivalent for Java:Map is IDictionary.  IDictionary is IEnumerable, so is handled above.
            //else if(coll isntanceof Map)  
            //     return seq(((Map) coll).entrySet());
            else if (coll is IEnumerator)  // java: Iterator
                return EnumeratorSeq.create((IEnumerator)coll);
             else
                throw new ArgumentException("Don't know how to create ISeq from: " + coll.GetType().Name);
        }



        static public IStream stream(object coll) {
            if (coll == null)
                return EMPTY_STREAM;
            else if (coll is IStream)
                return (IStream)coll;
            else if (coll is Streamable)
                return ((Streamable)coll).stream();
            else if (coll is Fn)  // TODO: Note use of Fn to imply castable to IFn.  Should we do this? Why not just check for IFn?
                return new FnStream((IFn)coll);
            else if (coll is IEnumerable)  // java: Iterable
                return new IteratorStream(((IEnumerable)coll).GetEnumerator());  // java: IteratorStream
            else if (coll.GetType().IsArray)
                return ArrayStream.createFromObject(coll);
            else if (coll is String)
                return ArrayStream.createFromObject(((String)coll).ToCharArray());

            throw new ArgumentException("Don't know how to create IStream from: " + coll.GetType().Name);
        }



        public static ISeq keys(object coll)
        {
            return APersistentMap.KeySeq.create(seq(coll));
        }

        public static ISeq vals(object coll)
        {
            return APersistentMap.ValSeq.create(seq(coll));
        }

        public static IPersistentMap meta(object x)
        {
            return x is IMeta
                ? ((IMeta)x).meta()
                : null;
        }


        public static int count(object o)
        {
            if (o == null)
                return 0;
            else if (o is Counted)
                return ((Counted)o).count();
            else if (o is IPersistentCollection)
            {
                ISeq s = seq(o);
                o = null;
                int i = 0;
                for (; s != null; s = s.rest())
                {
                    if (s is Counted)
                        return i + s.count();
                    i++;
                }
                return i;
            }
            else if (o is String)
                return ((String)o).Length;
            else if (o is ICollection)
                return ((ICollection)o).Count;
            else if (o is IDictionary)
                return ((IDictionary)o).Count;
            else if (o is Array)
                return ((Array)o).GetLength(0);

            throw new InvalidOperationException("count not supported on this type: " + o.GetType().Name);
        }

        public static IPersistentCollection conj(IPersistentCollection coll, Object x)
        {
            if (coll == null)
                return new PersistentList(x);
            return coll.cons(x);
        }

        public static ISeq cons(object x, object coll)
        {
            ISeq y = seq(coll);
            return y == null
                ? new PersistentList(x)
                : y.cons(x);
        }

        public static object first(object x)
        {
            if (x is ISeq)
                return ((ISeq)x).first();
            ISeq seq = RT.seq(x);
            return (seq == null)
                ? null
                : seq.first();
        }

        public static object second(object x)
        {
            return first(rest(x));
        }

        public static object third(object x)
        {
            return first(rest(rest(x)));
        }

        public static object fourth(object x)
        {
            return first(rest(rest(rest(x))));
        }

        public static ISeq rest(object x)
        {
            if (x is ISeq)
                return ((ISeq)x).rest();
            ISeq seq = RT.seq(x);
            if (seq == null)
                return null;
            return seq.rest();
        }

        public static ISeq rrest(object x)
        {
            return rest(rest(x));
        }

        public static object peek(object x)
        {
            return x == null
                ? null
                : ((IPersistentStack)x).peek();
        }

        public static object pop(object x)
        {
            return x == null
                ? null
                : ((IPersistentStack)x).pop();
        }

        static public Object get(Object coll, Object key)
        {
            if (coll == null)
                return null;
            else if (coll is Associative)
                return ((Associative)coll).valAt(key);
            else if (coll is IDictionary)
            {
                IDictionary m = (IDictionary)coll;
                return m[key];
            }
            else if (coll is IPersistentSet)
            {
                IPersistentSet set = (IPersistentSet)coll;
                return set.get(key);

            }
            else if (Util.IsNumeric(key) && (coll is string || coll.GetType().IsArray))
            {
                int n = Util.ConvertToInt(key);
                return n >= 0 && n < count(coll) ? nth(coll, n) : null;
            }
            return null;
        }

        static public Object get(Object coll, Object key, Object notFound)
        {
            if (coll == null)
                return notFound;
            else if (coll is Associative)
                return ((Associative)coll).valAt(key, notFound);
            else if (coll is IDictionary)
            {
                IDictionary m = (IDictionary)coll;
                if (m.Contains(key))
                    return m[key];
                return notFound;
            }
            else if (coll is IPersistentSet)
            {
                IPersistentSet set = (IPersistentSet)coll;
                if (set.contains(key))
                    return set.get(key);
                return notFound;
            }
            else if (Util.IsNumeric(key) && (coll is string || coll.GetType().IsArray))
            {
                int n = Util.ConvertToInt(key);
                return n >= 0 && n < count(coll) ? nth(coll, n) : notFound;
            }
            return notFound;
        }

        public static Associative assoc(object coll, object key, Object val)
        {
            if (coll == null)
                return new PersistentArrayMap(new object[] { key, val });
            return ((Associative)coll).assoc(key, val);
        }

        public static object contains(object coll, object key)
        {
            if (coll == null)
                return F;
            else if (coll is Associative)
                return ((Associative)coll).containsKey(key) ? T : F;
            else if (coll is IPersistentSet)
                return ((IPersistentSet)coll).contains(key) ? T : F;
            else if (coll is IDictionary)
            {
                IDictionary m = (IDictionary)coll;
                return m.Contains(key) ? T : F;
            }
            else if (Util.IsNumeric(key) && (coll is String || coll.GetType().IsArray))
            {
                int n = Util.ConvertToInt(key);
                return n >= 0 && n < count(coll);
            }
            return F;
        }

        public static object find(object coll, object key)
        {
            if (coll == null)
                return null;
            else if (coll is Associative)
                return ((Associative)coll).entryAt(key);
            else
            {
                IDictionary m = (IDictionary)coll;
                if (m.Contains(key))
                    return new MapEntry(key, m[key]);
                return null;
            }
        }

        public static object dissoc(object coll, object key)
        {
            return coll == null
                ? null
                : ((IPersistentMap)coll).without(key);
        }

        static public Object nth(Object coll, int n)
        {
            if (coll == null)
                return null;
            else if (coll is IPersistentVector)
                return ((IPersistentVector)coll).nth(n);
            else if (coll is String)
                return ((string)coll)[n];
            else if (coll.GetType().IsArray)
                return ((Array)coll).GetValue(n);  // TODO: Java has Reflector.prepRet -- check all uses.
            // Java has RandomAccess here.  CLR has no equiv.
            else if (coll is IList)                // Caused some infinite loops in places ASeq[].
                return ((IList)coll)[n];
            else if (coll is Match)
                return ((Match)coll).Groups[n];
            else if (coll is DictionaryEntry)
            {
                DictionaryEntry e = (DictionaryEntry)coll;
                if (n == 0)
                    return e.Key;
                else if (n == 1)
                    return e.Value;
                throw new IndexOutOfRangeException();
            }
            else if (coll.GetType().IsGenericType && coll.GetType().Name == "KeyValuePair`2")
            {
                if (n == 0)
                    return coll.GetType().InvokeMember("Key", BindingFlags.GetProperty, null, coll, null);
                else if (n == 1)
                    return coll.GetType().InvokeMember("Value", BindingFlags.GetProperty, null, coll, null);
                throw new IndexOutOfRangeException();
            }
            else if (coll is IMapEntry)
            {
                IMapEntry e = (IMapEntry)coll;
                if (n == 0)
                    return e.key();
                else if (n == 1)
                    return e.val();
                throw new IndexOutOfRangeException();
            }
            else if (coll is Sequential)
            {
                // TODO: FIX: Another assumption that Sequential implies castable to IPersistentCollection
                ISeq seq = ((IPersistentCollection)coll).seq();
                coll = null;  
                for (int i = 0; i <= n && seq != null; ++i, seq = seq.rest())
                {
                    if (i == n)
                        return seq.first();
                }
                throw new IndexOutOfRangeException();
            }
            else
                throw new InvalidOperationException("nth not supported on this type: " + coll.GetType().Name);
        }


        static public Object nth(Object coll, int n, Object notFound)
        {
            if (coll == null)
                return notFound;
            else if (n < 0)
                return notFound;
            else if (coll is IPersistentVector)
            {
                IPersistentVector v = (IPersistentVector)coll;
                if (n < v.count())
                    return v.nth(n);
                return notFound;
            }
            else if (coll is String)
            {
                String s = (String)coll;
                if (n < s.Length)
                    return s[n];
                return notFound;
            }
            else if (coll.GetType().IsArray)
            {
                Array a = (Array)coll;
                if (n < a.Length)
                    return a.GetValue(n);  // Java: has call to Reflector.prepRet wrapped here.
                return notFound;
            }
            else if (coll is IList)   // Changed to RandomAccess in Java Rev 1218.  
            {
                IList list = (IList)coll;
                if (n < list.Count)
                    return list[n];
                return notFound;
            }
            else if (coll is Match)
            {
                Match m = (Match)coll;
                if (n < m.Groups.Count)
                    return m.Groups[n];
                return notFound;
            }
            else if (coll is DictionaryEntry)
            {
                DictionaryEntry e = (DictionaryEntry)coll;
                if (n == 0)
                    return e.Key;
                else if (n == 1)
                    return e.Value;
                return notFound;
            }
            else if (coll.GetType().IsGenericType && coll.GetType().Name == "KeyValuePair`2")
            {
                if (n == 0)
                    return coll.GetType().InvokeMember("Key", BindingFlags.GetProperty, null, coll, null);
                else if (n == 1)
                    return coll.GetType().InvokeMember("Value", BindingFlags.GetProperty, null, coll, null);
                return notFound;
            }
            else if (coll is IMapEntry)
            {
                IMapEntry e = (IMapEntry)coll;
                if (n == 0)
                    return e.key();
                else if (n == 1)
                    return e.val();
                return notFound;
            }
            else if (coll is Sequential)
            {
                // TODO: FIX: ANother place where Sequential => IPersistentCollection
                ISeq seq = ((IPersistentCollection)coll).seq();
                coll = null;  // release in case GC
                for (int i = 0; i <= n && seq != null; ++i, seq = seq.rest())
                {
                    if (i == n)
                        return seq.first();
                }
                return notFound;
            }
            else
                throw new InvalidOperationException("nth not supported on this type: " + coll.GetType().Name);
        }

        #endregion

        #region boxing/casts

        public static char charCast(object x)
        {
            return Convert.ToChar(x);
        }

        static public bool booleanCast(object x)
        {
            if (x is Boolean)
                return ((Boolean)x);
            return x != null;
        }

        public static byte byteCast(object x)
        {
            return (byte)Convert.ToDouble(x);
        }

        public static short shortCast(object x)
        {
            return (short)Convert.ToDouble(x);
        }

        public static int intCast(object x)
        {
            // ToInt32 rounds.  We need truncation.
            return (int)Convert.ToDouble(x);
        }

        static public int intCast(char x)
        {
            return x;
        }

        static public int intCast(byte x)
        {
            return x;
        }

        static public int intCast(short x)
        {
            return x;
        }

        static public int intCast(int x)
        {
            return x;
        }

        static public int intCast(float x)
        {
            return (int)x;
        }

        static public int intCast(long x)
        {
            return (int)x;
        }

        static public int intCast(double x)
        {
            return (int)x;
        }

        public static long longCast(object x)
        {
            return (long)Convert.ToDouble(x);
        }

        public static long longCast(int x)
        {
            return x;
        }

        static public long longCast(float x)
        {
            return (long)x;
        }

        static public long longCast(long x)
        {
            return x;
        }

        static public long longCast(double x)
        {
            return (long)x;
        }

        public static float floatCast(object x)
        {
            return Convert.ToSingle(x);
        }

        public static float floatCast(int x)
        {
            return x;
        }

        public static float floatCast(float x)
        {
            return x;
        }

        public static float floatCast(long x)
        {
            return x;
        }

        public static float floatCast(double x)
        {
            return (float)x;
        }

        public static double doubleCast(object x)
        {
            return Convert.ToDouble(x);
        }


        public static double doubleCast(int x)
        {
            return x;
        }

        public static double doubleCast(float x)
        {
            return x;
        }

        public static double doubleCast(long x)
        {
            return x;
        }

        public static double doubleCast(double x)
        {
            return x;
        }

        #endregion

        #region  More collection support

        public static IPersistentMap map(params object[] init)
        {
            return (init == null)
                ? PersistentArrayMap.EMPTY
                : (init.Length <= PersistentArrayMap.HASHTABLE_THRESHOLD)
                    ? (IPersistentMap)new PersistentArrayMap(init)
                    : (IPersistentMap)PersistentHashMap.create(init);
        }

        public static IPersistentSet set(params object[] init)
        {
            return PersistentHashSet.create1(init);
        }

        public static IPersistentVector vector(params object[] init)
        {
            return LazilyPersistentVector.createOwning(init);
        }

        public static IPersistentVector subvec(IPersistentVector v, int start, int end)
        {
            if (end < start || start < 0 || end > v.count())
                throw new IndexOutOfRangeException();
            if (start == end)
                return PersistentVector.EMPTY;
            return new APersistentVector.SubVector(null, v, start, end);
        }


        #endregion

        #region List support

        public static ISeq list()
        {
            return null;
        }

        public static ISeq list(object arg1)
        {
            return new PersistentList(arg1);
        }

        public static ISeq list(object arg1, object arg2)
        {
            return listStar(arg1, arg2, null);
        }

        public static ISeq list(object arg1, object arg2, object arg3)
        {
            return listStar(arg1, arg2, arg3, null);
        }

        public static ISeq list(object arg1, object arg2, object arg3, object arg4)
        {
            return listStar(arg1, arg2, arg3, arg4, null);
        }

        public static ISeq list(object arg1, object arg2, object arg3, object arg4, object arg5)
        {
            return listStar(arg1, arg2, arg3, arg4, arg5, null);
        }



        public static ISeq listStar(object arg1, ISeq rest)
        {
            return cons(arg1, rest);
        }

        public static ISeq listStar(object arg1, object arg2, ISeq rest)
        {
            return cons(arg1, cons(arg2, rest));
        }

        public static ISeq listStar(object arg1, object arg2, object arg3, ISeq rest)
        {
            return cons(arg1, cons(arg2, cons(arg3, rest)));
        }

        public static ISeq listStar(object arg1, object arg2, object arg3, object arg4,  ISeq rest)
        {
            return cons(arg1, cons(arg2, cons(arg3, cons(arg4, rest))));
        }

        public static ISeq listStar(object arg1, object arg2, object arg3, object arg4, object arg5, ISeq rest)
        {
            return cons(arg1, cons(arg2, cons(arg3, cons(arg4, cons(arg5, rest)))));
        }

        public static ISeq arrayToList(object[] items)
        {
            ISeq ret = null;
            for (int i = items.Length - 1; i >= 0; --i)
                ret = (ISeq)cons(items[i], ret);
            return ret;
        }

        public static object[] toArray(object coll)
        {
            if (coll == null)
                return EMPTY_ARRAY;
            else if (coll is object[])
                return (object[])coll;
            // In CLR, ICollection does not have a toArray.  
            // ICollection derives from IEnumerable, so the IEnumerable clause will take care of it.
            //if (coll instanceof Collection)
            //  return ((Collection)coll).toArray();
            //  TODO: List has a toArray -- generic -- need type. 
            else if (coll is IEnumerable)
                return IEnumToArray((IEnumerable)coll);
            // Java has Map here, but IDictionary is IEnumerable, so it will be handled by previous clause.
            else if (coll is String)
            {
                char[] chars = ((String)coll).ToCharArray();
                // TODO: Determine if we need to make a copy (Java version does, not sure if CLR requires it)
                object[] ret = new object[chars.Length];
                for (int i = 0; i < chars.Length; i++)
                    ret[i] = chars[i];
                return ret;
            }
            // This used to be in the java version.  No longer.  Do we need?
            //else if (coll is ISeq)
            //    return toArray((ISeq)coll);
            //else if (coll is IPersistentCollection)
            //    return toArray(((IPersistentCollection)coll).seq());
            else if (coll.GetType().IsArray)
            {
                ISeq s = (seq(coll));
                object[] ret = new object[count(s)];
                for (int i = 0; i < ret.Length; i++, s = s.rest())
                    ret[i] = s.first();
                return ret;
            }
            else
                throw new Exception("Unable to convert: " + coll.GetType() + " to Object[]");
        }

        private static object[] IEnumToArray(IEnumerable e)
        {
            List<object> list = new List<object>();
            foreach (object o in e)
                list.Add(o);

            return list.ToArray();
        }

        //private static object[] toArray(ISeq seq)
        //{
        //    object[] array = new object[seq.count()];
        //    int i = 0;
        //    for (ISeq s = seq; s != null; s = s.rest(), i++)
        //        array[i] = s.first();
        //    return array;
        //}

        public static T[] SeqToArray<T>(ISeq x)
        {
            if (x == null)
                return new T[0];

            T[] array = new T[RT.Length(x)];
            int i = 0;
            for (ISeq s = x; s != null; s = s.rest(), i++)
                array[i] = (T)s.first();
            return array;
        }

        static public object seqToTypedArray(ISeq seq)
        {
            Type type = (seq != null)
                ? (seq.first() == null ? typeof(object) : seq.first().GetType())
                : typeof(Object);
            return seqToTypedArray(type, seq);
        }

        static public object seqToTypedArray(Type type, ISeq seq)
        {
            Array ret = Array.CreateInstance(type, seq == null ? 0 : seq.count());
            for (int i = 0; seq != null; ++i, seq = seq.rest())
                ret.SetValue(seq.first(), i);
            return ret;
        }

        static public int Length(ISeq list)
        {
            int i = 0;
            for (ISeq c = list; c != null; c = c.rest())
                i++;
            return i;
        }

        public static int BoundedLength(ISeq list, int limit)
        {
            int i = 0;
            for (ISeq c = list; c != null && i <= limit; c = c.rest())
            {
                i++;
            }
            return i;
        }

        #endregion

        #region Reader support

        public static bool suppressRead()
        {
            // TODO: look up in suppress-read var  (java todo)
            return false;
        }

        static public string printString(object x)
        {
            StringWriter sw = new StringWriter();
            print(x, sw);
            return sw.ToString();
        }

        static public Object readString(String s)
        {
            TextReader r = new StringReader(s);
            return LispReader.read(r, true, null, false);
        }

        static public void print(Object x, TextWriter w)
        {
            //call multimethod
            if (PRINT_INITIALIZED.IsBound && RT.booleanCast(PRINT_INITIALIZED.deref()))
            {
                PR_ON.invoke(x, w);
                return;
            }

            bool readably = booleanCast(PRINT_READABLY.deref());

            // Print meta, if exists & should be printed
            if (x is Obj)
            {
                Obj o = x as Obj;
                if (RT.count(o.meta()) > 0 && readably && booleanCast(PRINT_META.deref()))
                {
                    IPersistentMap meta = o.meta();
                    w.Write("#^");
                    if (meta.count() == 1 && meta.containsKey(TAG_KEY))
                        print(meta.valAt(TAG_KEY), w);
                    else
                        print(meta, w);
                    w.Write(' ');
                }
            }

            if (x == null)
                w.Write("nil");
            else if (x is ISeq || x is IPersistentList)
            {
                w.Write('(');
                printInnerSeq(seq(x), w);
                w.Write(')');
            }
            else if (x is string)
            {
                string s = x as string;
                if (!readably)
                    w.Write(s);
                else
                {
                    w.Write('"');
                    foreach (char c in s)
                    {
                        switch (c)
                        {
                            case '\n':
                                w.Write("\\n");
                                break;
                            case '\t':
                                w.Write("\\t");
                                break;
                            case '\r':
                                w.Write("\\r");
                                break;
                            case '"':
                                w.Write("\\\"");
                                break;
                            case '\\':
                                w.Write("\\\\");
                                break;
                            case '\f':
                                w.Write("\\f");
                                break;
                            case '\b':
                                w.Write("\\b");
                                break;
                            default:
                                w.Write(c);
                                break;
                        }
                    }
                    w.Write('"');
                }
            }
            else if (x is IPersistentMap)
            {
                w.Write('{');
                for (ISeq s = seq(x); s != null; s = s.rest())
                {
                    IMapEntry e = (IMapEntry)s.first();
                    print(e.key(), w);
                    w.Write(' ');
                    print(e.val(), w);
                    if (s.rest() != null)
                        w.Write(", ");
                }
                w.Write('}');
            }
            else if (x is IPersistentVector)
            {
                IPersistentVector v = x as IPersistentVector;
                int n = v.count();
                w.Write('[');
                for (int i = 0; i < n; i++)
                {
                    print(v.nth(i), w);
                    if (i < n - 1)
                        w.Write(" ");
                }
                w.Write(']');
            }
            else if (x is IPersistentSet)
            {
                w.Write("#{");
                for (ISeq s = seq(x); s != null; s = s.rest())
                {
                    print(s.first(), w);
                    if (s.rest() != null)
                        w.Write(" ");
                }
                w.Write('}');
            }
            else if (x is Char)
            {
                char c = (char)x;
                if (!readably)
                    w.Write(c);
                else
                {
                    w.Write('\\');
                    switch (c)
                    {
                        case '\n':
                            w.Write("newline");
                            break;
                        case '\t':
                            w.Write("tab");
                            break;
                        case ' ':
                            w.Write("space");
                            break;
                        case '\b':
                            w.Write("backspace");
                            break;
                        case '\f':
                            w.Write("formfeed");
                            break;
                        case '\r':
                            w.Write("return");
                            break;
                        default:
                            w.Write(c);
                            break;
                    }
                }
            }
            else if (x is Type)
            {
                w.Write("#=");
                w.Write(((Type)x).FullName);
            }
            else if (x is java.math.BigDecimal && readably)
            {
                w.Write(x.ToString());
                w.Write("M");
            }
            else if (x is Var)
            {
                Var v = x as Var;
                w.Write("#=(var {0}/{1})", v.Namespace.Name, v.Symbol);
            }
            else
                w.Write(x.ToString());
        }


        private static void printInnerSeq(ISeq x, TextWriter w)
        {
            for (ISeq s = x; s != null; s = s.rest())
            {
                print(s.first(), w);
                if (s.rest() != null)
                    w.Write(' ');
            }
        }

        public static string printToConsole(object x)
        {
            string ret = printString(x);
            Console.WriteLine(ret);
            return ret;
        }



        #endregion

        #region Locating types

        public static Type classForName(string p)
        {
            // TODO: We're really going to have to work on this.
            Type t = null;

            t = Type.GetType(p, false);

            if (t != null)
                return t;

            Assembly assy = Assembly.GetExecutingAssembly();
            t = assy.GetType(p, false);
            if (t != null)
                return t;

            AppDomain domain = AppDomain.CurrentDomain;
            Assembly[] assys = domain.GetAssemblies();
            List<Type> candidateTypes = new List<Type>();

            foreach (Assembly assy1 in assys)
            {
                Type t1 = assy1.GetType(p, false);
                if (t1 != null)
                    candidateTypes.Add(t1);
            }

            if (candidateTypes.Count == 0)
                t = null;
            else if (candidateTypes.Count == 1)
                t = candidateTypes[0];
            else // multiple, ambiguous
                t = null;



            //    IEnumerable<Type> types1 = assy.GetTypes();
            //    List<Type> typeList1 = new List<Type>(types1);

            //    IEnumerable<Type> types = assy.GetTypes().Where(t1 => t1.FullName == p);
            //    List<Type> typeList = new List<Type>(types);
            //    if (typeList.Count == 1)
            //        t = typeList[0];
            //}
            //catch
            //{
            //}

            return t;
        }

        #endregion

        #region Array interface

        public static int alength(Array a)
        {
            return a.Length;
        }

        public static Array aclone(Array a)
        {
            return (Array)a.Clone();
        }

        public static object aget(Array a, int idx)
        {
            return a.GetValue(idx);
        }

        public static object aset(Array a, int idx, object val)
        {
            a.SetValue(val, idx);
            return val;
        }

        #endregion

        #region Stream support

        private static readonly object EOS = new object();

        public static object eos()
        {
            return EOS;
        }

        public static bool isEOS(object o)
        {
            return o == EOS;
        }

        public static readonly IStream EMPTY_STREAM = new EmptyStream();

        private class EmptyStream : IStream
        {
            #region IStream Members

            public object next()
            {
                return eos();
            }

            #endregion
        }


        private class FnStream : IStream
        {

            #region Data

            IFn _fn;

            #endregion

            #region C-tors

            public FnStream(IFn fn)
            {
                _fn = fn;
            }

            #endregion

            #region IStream Members

            public object next()
            {
                return _fn.invoke();
            }

            #endregion
        }


        #endregion

        #region Things not in the Java version

        class DefaultComparer : IComparer
        {
            #region IComparer Members

            public int Compare(object x, object y)
            {
                return Util.compare(x, y);  // was ((IComparable)x).CompareTo(y); -- changed in Java rev 1145
            }

            #endregion

            #region core.clj compatibility

            //  Somewhere, there is an explicit call to compare
            public int compare(object x, object y)
            {
                return Util.compare(x, y);  // was ((IComparable)x).CompareTo(y);-- changed in Java rev 1145
            }

            #endregion
        }

        static public readonly IComparer DEFAULT_COMPARER = new DefaultComparer();




        // NOT SURE WHY WE NEED THIS.  THIS VERSION ADDED IN REV 1112  (But in Reflector)
        public static Object prepRet(Object x)
        {
            //	if(c == boolean.class)
            //		return ((Boolean) x).booleanValue() ? RT.T : null;
            if (x is Boolean)
                return ((Boolean)x) ? RT.T : RT.F; // Java version has Boolean.TRUE and Boolean.FALSE
            return x;
        }


        // do we need this?
        //static Boolean HasTag(object o, object tag)
        //{
        //    return Util.equals(tag,,RT.get(RT.meta(o),TAG_KEY);
        //}

        public static long nanoTime()
        {
            return DateTime.Now.Ticks * 100;
        }

        private static readonly Stopwatch _stopwatch = new Stopwatch();

        public static object StartStopwatch()
        {
            _stopwatch.Reset();
            _stopwatch.Start();
            return null;
        }

        public static long StopStopwatch()
        {
            _stopwatch.Stop();
            return _stopwatch.ElapsedMilliseconds;
        }


        
        // In core.clj, we see (cast Number x)  in a number of numeric methods.
        // There is no Number wrapper here.
        // The intent is:  if x is not a numeric value, throw a ClassCastException (Java),
        //                 else return x
        // And here it is:
        public static object NumberCast(object x)
        {
            if (!Util.IsNumeric(x))
                throw new InvalidCastException("Expected a number");
            return x;
        }

        // The Java guys use Class.cast to do casting.
        // We don't have that.
        // Perhaps this will work.
        // NOPE!
        //public static object Cast(Type t, object o)
        //{
        //    return Type.DefaultBinder.ChangeType(o, t, null);
        //}


        // The Java version goes through Array.sort to do this,
        // but I don't have a way to pass a comparator.

        class ComparerConverter : IComparer
        {
            readonly IFn _fn;

            public ComparerConverter(IFn fn)
            {
                _fn = fn;
            }

            #region IComparer Members

            public int  Compare(object x, object y)
            {
 	            return (int) _fn.invoke( x,y );
            }

            #endregion
        }

        public static void SortArray(Array a, IFn fn)
        {
                Array.Sort(a, new ComparerConverter(fn));
        }



        static readonly Random _random = new Random();

        public static double random()
        {
            lock (_random)
            {
                return _random.NextDouble();
            }
        }

        // TODO: Figure out how to do a load.
        public static object load(object pathname)
        {
            return null;
        }

        public static void LookAtMe(object o)
        {
            Console.WriteLine("Here it is: {0}", o);
        }

        #endregion
    }
}
