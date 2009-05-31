
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
using Microsoft.Linq.Expressions;
using clojure.lang.CljCompiler.Ast;
using clojure.runtime;
using System.Reflection;
using System.Reflection.Emit;


namespace clojure.lang
{
    /// <summary>
    /// 
    /// </summary>
    /// <remarks>Originally, I tried to do this as one-pass compiler, direct from SEXPRs to DLR Expression Trees.  And it was working just fine.
    /// <para>Then Rich added a change in the new lazy version of Clojure that required a variable base class for implementations of <see cref="AFn"/>.  
    /// Due to the fact that DLR can only generate lambda expressions to static methods in assemblies, some nasty workarounds are required
    /// that force a multi-pass compiler.  With that, I gave up and added an AST intermediate stage.</para>
    /// <para>So now we go SEXPR -> AST -> ExpressionTree.  As a result, once again, my code converges toward the JVM code.</para>
    /// <para>PS: And then later the damnable :super-class went away.</para>
    /// </remarks>
    public static class Compiler
    {
        #region other constants

        internal const int MAX_POSITIONAL_ARITY = 20;

        #endregion

        #region Symbols

        public static readonly Symbol DEF = Symbol.create("def");
        public static readonly Symbol LOOP = Symbol.create("loop*");
        public static readonly Symbol RECUR = Symbol.create("recur");
        public static readonly Symbol IF = Symbol.create("if");
        public static readonly Symbol LET = Symbol.create("let*");
        public static readonly Symbol LETFN = Symbol.create("letfn*");
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
        public static readonly Symbol IMPORT = Symbol.create("clojure.core","import*");
        public static readonly Symbol NEW = Symbol.create("new");
        public static readonly Symbol _AMP_ = Symbol.create("&");


        public static readonly Symbol IDENTITY = Symbol.create("clojure.core", "identity");

        static readonly Symbol NS = Symbol.create("ns");
        static readonly Symbol IN_NS = Symbol.create("in-ns");

        internal static readonly Symbol ISEQ = Symbol.create("clojure.lang.ISeq");


        #endregion

        #region Keywords

        static readonly Keyword INLINE_KEY = Keyword.intern(null, "inline");
        static readonly Keyword INLINE_ARITIES_KEY = Keyword.intern(null, "inline-arities");

        #endregion

        #region Vars

        //boolean
        internal static readonly Var COMPILE_FILES = Var.intern(Namespace.findOrCreate(Symbol.create("clojure.core")),
                                                 Symbol.create("*compile-files*"), false);  //JAVA: Boolean.FALSE -- changed from RT.F in rev 1108, not sure why

        //String
        public static readonly Var COMPILE_PATH = Var.intern(Namespace.findOrCreate(Symbol.create("clojure.core")),
                                                 Symbol.create("*compile-path*"), null);

        public static readonly Var COMPILE = Var.intern(Namespace.findOrCreate(Symbol.create("clojure.core")),
                                                Symbol.create("compile"));

        // String
        static readonly Var SOURCE = Var.intern(Namespace.findOrCreate(Symbol.create("clojure.core")),
                                        Symbol.create("*source-path*"), "NO_SOURCE_FILE");
        // String
        internal static readonly Var SOURCE_PATH = Var.intern(Namespace.findOrCreate(Symbol.create("clojure.core")),
            Symbol.create("*file*"), "NO_SOURCE_PATH");
        //Integer
        internal static readonly Var LINE_BEFORE = Var.create(0);
        internal static readonly Var LINE_AFTER = Var.create(0);

        internal static readonly Var METHODS = Var.create(null);
        internal static readonly Var LOCAL_ENV = Var.create(PersistentHashMap.EMPTY);
        //Integer
        internal static readonly Var NEXT_LOCAL_NUM = Var.create(0);
        internal static readonly Var LOOP_LOCALS = Var.create(null);
        // Label
        internal static readonly Var LOOP_LABEL = Var.create();


        internal static readonly Var IN_CATCH_FINALLY = Var.create(null);          //null or not
        internal static readonly Var IN_TAIL_POSITION = Var.create(null);        //null or not


        internal static readonly Var VARS = Var.create();           //var->constid
        internal static readonly Var CONSTANTS = Var.create();      //vector<object>
        internal static readonly Var KEYWORDS = Var.create();       //keyword->constid

        #endregion

        #region Special forms

        // TODO: Figure out why clojure's special-form?  shows if as special, instead of if*.

        public static readonly IPersistentMap _specials = PersistentHashMap.create(
            DEF, new DefExpr.Parser(),
            LOOP, new LetExpr.Parser(),
            RECUR, new RecurExpr.Parser(),
            IF, new IfExpr.Parser(),
            LET, new LetExpr.Parser(),
            LETFN, new LetFnExpr.Parser(),
            DO, new BodyExpr.Parser(),
            FN, null,
            QUOTE, new ConstantExpr.Parser(),
            THE_VAR, new TheVarExpr.Parser(),
            IMPORT, new ImportExpr.Parser(),
            DOT, new HostExpr.Parser(),
            ASSIGN, new AssignExpr.Parser(),
            TRY, new TryExpr.Parser(),
            THROW, new ThrowExpr.Parser(),
            MONITOR_ENTER, new MonitorEnterExpr.Parser(),
            MONITOR_EXIT, new MonitorExitExpr.Parser(),
            CATCH, null,
            FINALLY, null,
            NEW, new NewExpr.Parser(),
            _AMP_, null
        );

        public static bool isSpecial(Object sym)
        {
            return _specials.containsKey(sym);
        }

        static IParser GetSpecialFormParser(object op)
        {
            return (IParser)_specials.valAt(op);
        }

        #endregion

        #region MethodInfos, etc.

        //static readonly MethodInfo Method_ArraySeq_create_array_int = typeof(ArraySeq).GetMethod("create", new Type[] { typeof(object[]), typeof(int) });

        //static readonly MethodInfo Method_CGen_MakeMap = typeof(Generator).GetMethod("MakeMap");
        //static readonly MethodInfo Method_CGen_MakeSet = typeof(Generator).GetMethod("MakeSet");
        //static readonly MethodInfo Method_CGen_MakeVector = typeof(Generator).GetMethod("MakeVector");

        internal static readonly PropertyInfo Method_Compiler_CurrentNamespace = typeof(Compiler).GetProperty("CurrentNamespace");
        internal static readonly MethodInfo Method_Compiler_PushNS = typeof(Compiler).GetMethod("PushNS");


        internal static readonly MethodInfo Method_IObj_withMeta = typeof(IObj).GetMethod("withMeta");

        internal static readonly MethodInfo Method_Keyword_intern = typeof(Keyword).GetMethod("intern", new Type[] { typeof(Symbol) });

        internal static readonly MethodInfo Method_Monitor_Enter = typeof(Monitor).GetMethod("Enter");
        internal static readonly MethodInfo Method_Monitor_Exit = typeof(Monitor).GetMethod("Exit");

        internal static readonly MethodInfo Method_Namespace_importClass1 = typeof(Namespace).GetMethod("importClass", new Type[] { typeof(Type) });

        internal static readonly MethodInfo Method_PersistentList_create = typeof(PersistentList).GetMethod("create", new Type[] { typeof(System.Collections.IList) });

        internal static readonly MethodInfo Method_Reflector_CallInstanceMethod = typeof(Reflector).GetMethod("CallInstanceMethod");
        internal static readonly MethodInfo Method_Reflector_CallStaticMethod = typeof(Reflector).GetMethod("CallStaticMethod");
        internal static readonly MethodInfo Method_Reflector_InvokeConstructor = typeof(Reflector).GetMethod("InvokeConstructor");
        internal static readonly MethodInfo Method_Reflector_SetInstanceFieldOrProperty = typeof(Reflector).GetMethod("SetInstanceFieldOrProperty");

        internal static readonly MethodInfo Method_RT_arrayToList = typeof(RT).GetMethod("arrayToList");
        internal static readonly MethodInfo Method_RT_classForName = typeof(RT).GetMethod("classForName");
        internal static readonly MethodInfo Method_RT_IsTrue = typeof(RT).GetMethod("IsTrue");
        internal static readonly MethodInfo Method_RT_map = typeof(RT).GetMethod("map");
        internal static readonly MethodInfo Method_RT_printToConsole = typeof(RT).GetMethod("printToConsole");
        internal static readonly MethodInfo Method_RT_set = typeof(RT).GetMethod("set");
        internal static readonly MethodInfo Method_RT_vector = typeof(RT).GetMethod("vector");
        internal static readonly MethodInfo Method_RT_readString = typeof(RT).GetMethod("readString");
        internal static readonly MethodInfo Method_RT_var2 = typeof(RT).GetMethod("var", new Type[] { typeof(string), typeof(string) });

        internal static readonly MethodInfo Method_Symbol_create2 = typeof(Symbol).GetMethod("create", new Type[] { typeof(string), typeof(string) });
        
        internal static readonly MethodInfo Method_Var_BindRoot = typeof(Var).GetMethod("BindRoot");
        internal static readonly MethodInfo Method_Var_get = typeof(Var).GetMethod("deref");
        internal static readonly MethodInfo Method_Var_set = typeof(Var).GetMethod("set");
        internal static readonly MethodInfo Method_Var_setMeta = typeof(Var).GetMethod("setMeta");
        internal static readonly MethodInfo Method_Var_popThreadBindings = typeof(Var).GetMethod("popThreadBindings");


        //static readonly ConstructorInfo Ctor_AFnImpl_0 = typeof(AFnImpl).GetConstructor(Type.EmptyTypes);
        internal static readonly ConstructorInfo Ctor_RestFnImpl_1 = typeof(RestFnImpl).GetConstructor(new Type[] { typeof(int) });

        internal static readonly MethodInfo[] Methods_IFn_invoke = new MethodInfo[MAX_POSITIONAL_ARITY + 2];

        internal static Type[] CreateObjectTypeArray(int size)
        {
            Type[] typeArray = new Type[size];
            for (int i = 0; i < size; i++)
                typeArray[i] = typeof(Object);
            return typeArray;
        }



        #endregion

        #region C-tors & factory methods

        static Compiler()
        {
            for (int i = 0; i <= Compiler.MAX_POSITIONAL_ARITY; i++)
                Methods_IFn_invoke[i] = typeof(IFn).GetMethod("invoke", CreateObjectTypeArray(i));

            Type[] types = new Type[Compiler.MAX_POSITIONAL_ARITY + 1];
            CreateObjectTypeArray(Compiler.MAX_POSITIONAL_ARITY).CopyTo(types, 0);
            types[Compiler.MAX_POSITIONAL_ARITY] = typeof(object[]);
            Methods_IFn_invoke[Compiler.MAX_POSITIONAL_ARITY + 1]
                = typeof(IFn).GetMethod("invoke", types);

            MethodInfo[] mis = typeof(IFn).GetMethods();

        }

        static GenContext _context = new GenContext("eval", CompilerMode.Immediate);

        static int _saveId = 0;
        public static void SaveEvalContext()
        {
            _context.AssyBldr.Save("done" + _saveId++ + ".dll");
            _context = new GenContext("eval", CompilerMode.Immediate);
        }


        public static LambdaExpression GenerateLambda(object form, bool addPrint)
        {
            return GenerateLambda(_context, form, addPrint);
        }


        internal static LambdaExpression GenerateLambda(GenContext context, object form, bool addPrint)
        {
            // TODO: Clean this up.
            form = RT.list(FN, PersistentVector.EMPTY, form);

            Expr ast = GenerateAST(form);
            Expression formExpr = GenerateDlrExpression(context,ast);
            Expression finalExpr = Expression.Call(formExpr, formExpr.Type.GetMethod("invoke", System.Type.EmptyTypes));

            if (addPrint)
            {
                finalExpr = Expression.Call(Method_RT_printToConsole, finalExpr);
            }

            return Expression.Lambda(finalExpr, "REPLCall", null);
        }




        static Expression[] MaybeBox(Expression[] args)
        {
            // TODO: avoid copying array if not necessary
            Expression[] boxedArgs = new Expression[args.Length];
            for (int i1 = 0; i1 < args.Length; ++i1)
                boxedArgs[i1] = MaybeBox(args[i1]);
            return boxedArgs;
        }

        internal static Expression MaybeBox(Expression expr)
        {
            if (expr.Type == typeof(void))
                // I guess we'll pass a void.  This happens when we have a throw, for example.
                return Expression.Block(expr, Expression.Default(typeof(object)));

            return expr.Type.IsValueType
                ? Expression.Convert(expr, typeof(object))
                : expr;
        }

        #endregion

        #region Entry points



        #endregion

        #region  AST generation

        internal static LiteralExpr NIL_EXPR = new NilExpr();
        static LiteralExpr TRUE_EXPR = new BooleanExpr(true);
        static LiteralExpr FALSE_EXPR = new BooleanExpr(false);

        // Equivalent to Java: Compiler.analyze()
        internal static Expr GenerateAST(object form)
        {
            return GenerateAST(form,null);
        }

        internal static Expr GenerateAST(object form, string name)
        {
            if (form is LazySeq)
            {
                form = RT.seq(form);
                if (form == null)
                    form = PersistentList.EMPTY;
            }
            if (form == null)
                return NIL_EXPR;
            else if (form is Boolean)
                return ((bool)form) ? TRUE_EXPR : FALSE_EXPR;

            Type type = form.GetType();

            if (type == typeof(Symbol))
                return AnalyzeSymbol((Symbol)form);
            else if (type == typeof(Keyword))
                return RegisterKeyword((Keyword)form);
            else if (type == typeof(String))
                return new StringExpr((String)form);
            else if (form is IPersistentCollection && ((IPersistentCollection)form).count() == 0)
                return OptionallyGenerateMetaInit(form, new EmptyExpr(form));
            else if (form is ISeq)
                return AnalyzeSeq((ISeq)form,name);
            else if (form is IPersistentVector)
                return VectorExpr.Parse((IPersistentVector)form);
            else if (form is IPersistentMap)
                return MapExpr.Parse((IPersistentMap)form);
            else if (form is IPersistentSet)
                return SetExpr.Parse((IPersistentSet)form);
            else
                return new ConstantExpr(form);
        }

        internal static Expr OptionallyGenerateMetaInit(object form, Expr expr)
        {
            Expr ret = expr;

            IObj o = form as IObj;
            if (o != null && o.meta() != null)
                ret = new MetaExpr(ret, (MapExpr)MapExpr.Parse(o.meta()));
                    
            return ret;
        }


        private static Expr AnalyzeSymbol(Symbol symbol)
        {
            Symbol tag = TagOf(symbol);

            if (symbol.Namespace == null)
            {
                LocalBinding b = ReferenceLocal(symbol);
                if (b != null)
                    return new LocalBindingExpr(b, tag);
            }
            else
            {
                if (namespaceFor(symbol) == null)
                {
                    Symbol nsSym = Symbol.create(symbol.Namespace);
                    Type t = MaybeType(nsSym, false);
                    if (t != null)
                        if (Reflector.GetField(t, symbol.Name, true) != null)
                            return new StaticFieldExpr(t, symbol.Name);
                    throw new Exception(string.Format("Unable to find static field: {0} in {1}", symbol.Name, t));
                }
            }

            object o = Compiler.Resolve(symbol);
            if (o is Var)
            {
                Var v = (Var)o;
                if (IsMacro(v) != null)
                    throw new Exception("Can't take the value of a macro: " + v);
                RegisterVar(v);
                return new VarExpr(v, tag);
            }
            else if (o is Type)
                return new ConstantExpr(o);
            else if (o is Symbol)
                return new UnresolvedVarExpr((Symbol)o);

            throw new Exception(string.Format("Unable to resolve symbol: {0} in this context", symbol));
        }


        private static Expr AnalyzeSeq(ISeq form, string name)
        {
            object exp = MacroexpandSeq1(form);
            if (exp != form)
                return GenerateAST(exp,name);

            object op = RT.first(form);

            if (op == null)
                throw new ArgumentNullException("Can't call nil");

            IFn inline = IsInline(op, RT.count(RT.next(form)));

            if (inline != null)
                return GenerateAST(inline.applyTo(RT.next(form)));

            IParser p;
            if (op.Equals(FN))
                return FnExpr.Parse(form, name);
            if ((p = GetSpecialFormParser(op)) != null)
                return p.Parse(form);
            else 
                return InvokeExpr.Parse(form);
        }


        static object Macroexpand1(object form)
        {
            return (form is ISeq)
                ? MacroexpandSeq1((ISeq)form)
                : form;
        }

        static object Macroexpand(object form)
        {
            object exf = Macroexpand1(form);
            if (exf != form)
                return Macroexpand(exf);
            return form;
        }

        private static object MacroexpandSeq1(ISeq form)
        {
            object op = RT.first(form);

            if (isSpecial(op))
                return form;

            // macro expansion
            Var v = IsMacro(op);
            if (v != null)
            {
                try
                {
                    Var.pushThreadBindings(RT.map(RT.MACRO_META, RT.meta(form)));
                    return v.applyTo(form.next());
                }
                finally
                {
                    Var.popThreadBindings();
                }
            }
            else
            {
                if (op is Symbol)
                {
                    Symbol sym = (Symbol)op;
                    string sname = sym.Name;
                    // (.substring s 2 5) => (. x substring 2 5)
                    if (sname[0] == '.')
                    {
                        if (form.count() < 2)
                            throw new ArgumentException("Malformed member expression, expecting (.member target ...)");
                        Symbol method = Symbol.intern(sname.Substring(1));
                        // TODO:  Figure out why the following change made in Java Rev 1158 breaks ants.clj
                        // Note on that revision: force instance member interpretation of (.method ClassName), e.g. (.getMethods String) works
                        //  However, when I do this, it makes ants.clj choke on: (def white-brush (new SolidBrush (.White Color)))
                        object target = RT.second(form);
                        if (MaybeType(target, false) != null)
                            target = RT.list(IDENTITY, target);
                        return RT.listStar(DOT, target, method, form.next().next());
                        // safe substitute: return RT.listStar(Compiler.DOT, RT.second(form), method, form.next().next());
                    }
                    else if (NamesStaticMember(sym))
                    {
                        Symbol target = Symbol.intern(sym.Namespace);
                        Type t = MaybeType(target, false);
                        if (t != null)
                        {
                            Symbol method = Symbol.intern(sym.Name);
                            return RT.listStar(Compiler.DOT, target, method, form.next());
                        }
                    }
                    else
                    {
                        // (x.substring 2 5) =>  (. s substring 2 5)
                        int index = sname.LastIndexOf('.');
                        if (index == sname.Length - 1)
                            return RT.listStar(Compiler.NEW, Symbol.intern(sname.Substring(0, index)), form.next());
                    }
                }

            }
            return form;
        }

        internal static bool NamesStaticMember(Symbol sym)
        {
            return sym.Namespace != null && NamespaceFor(sym) == null;
        }

        private static IFn IsInline(object op, int arity)
        {
            // Java:  	//no local inlines for now
            if (op is Symbol && ReferenceLocal((Symbol)op) != null)
                return null;

            if (op is Symbol || op is Var)
            {
                Var v = (op is Var) ? (Var)op : LookupVar((Symbol)op, false);
                if (v != null)
                {
                    if (v.Namespace != CurrentNamespace && !v.isPublic())
                        throw new InvalidOperationException("var: " + v + " is not public");
                    IFn ret = (IFn)RT.get(v.meta(), INLINE_KEY);
                    if (ret != null)
                    {
                        IPersistentSet arities = (IPersistentSet)RT.get(v.meta(), INLINE_ARITIES_KEY);
                        if (arities == null || arities.contains(arity))
                            return ret;
                    }
                }
            }
            return null;
        }

        internal static Var LookupVar(Symbol sym, bool internNew)
        {
            Var var = null;

            // Note: ns-qualified vars in other namespaces must exist already
            if (sym.Namespace != null)
            {
                Namespace ns = Compiler.NamespaceFor(sym);
                if (ns == null)
                    return null;
                Symbol name = Symbol.create(sym.Name);
                if (internNew && ns == CurrentNamespace)
                    var = CurrentNamespace.intern(name);
                else
                    var = ns.FindInternedVar(name);
            }
            else if (sym.Equals(NS))
                var = RT.NS_VAR;
            else if (sym.Equals(IN_NS))
                var = RT.IN_NS_VAR;
            else
            {
                // is it mapped?
                Object o = CurrentNamespace.GetMapping(sym);
                if (o == null)
                {
                    // introduce a new var in the current ns
                    if (internNew)
                        var = CurrentNamespace.intern(Symbol.create(sym.Name));
                }
                else if (o is Var)
                    var = (Var)o;
                else
                    throw new Exception(string.Format("Expecting var, but {0} is mapped to {1}", sym, o));
            }
            if (var != null)
                RegisterVar(var);
            return var;
        }

        private static Var IsMacro(Object op)
        {
            if (op is Symbol && ReferenceLocal((Symbol)op) != null)
                return null;
            if (op is Symbol || op is Var)
            {
                Var v = (op is Var) ? (Var)op : LookupVar((Symbol)op, false);
                if (v != null && v.IsMacro)
                {
                    if (v.Namespace != CurrentNamespace && !v.IsPublic)
                        throw new InvalidOperationException(string.Format("Var: {0} is not public", v));
                    return v;
                }
            }
            return null;
        }

        private static void RegisterVar(Var v)
        {
            if (!VARS.IsBound)
                return;
            IPersistentMap varsMap = (IPersistentMap)VARS.deref();
            Object id = RT.get(varsMap, v);
            if (id == null)
            {
                VARS.set(RT.assoc(varsMap, v, RegisterConstant(v)));
            }
        }


        internal static int RegisterConstant(Object o)
        {
            if (!CONSTANTS.IsBound)
                return -1;
            PersistentVector v = (PersistentVector)CONSTANTS.deref();
            CONSTANTS.set(RT.conj(v, o));
            return v.count();
        }

        internal static KeywordExpr RegisterKeyword(Keyword keyword)
        {
            if (!KEYWORDS.IsBound)
                return new KeywordExpr(keyword);

            IPersistentMap keywordsMap = (IPersistentMap)KEYWORDS.deref();
            object id = RT.get(keywordsMap, keyword);
            if (id == null)
                KEYWORDS.set(RT.assoc(keywordsMap, keyword, RegisterConstant(keyword)));
            return new KeywordExpr(keyword);
        }


        internal static LocalBinding RegisterLocal(Symbol sym, Symbol tag, Expr init)
        {
            int num = GetAndIncLocalNum();

            LocalBinding b = new LocalBinding(num,sym, tag, init);

            IPersistentMap localsMap = (IPersistentMap)LOCAL_ENV.deref();
            LOCAL_ENV.set(RT.assoc(localsMap,b.Symbol, b));
            FnMethod method = (FnMethod)METHODS.deref();
            method.Locals = (IPersistentMap)RT.assoc(method.Locals,b, b);
            method.IndexLocals = (IPersistentMap)RT.assoc(method.IndexLocals, num, b);
            return b;
        }

        internal static int GetAndIncLocalNum()
        {
            int num = (int)NEXT_LOCAL_NUM.deref();
            FnMethod m = (FnMethod)METHODS.deref();
            if (num > m.MaxLocal)
                m.MaxLocal = num;
            NEXT_LOCAL_NUM.set(num + 1);
            return num;
        }

        internal static LocalBinding ReferenceLocal(Symbol symbol)
        {
            if (!LOCAL_ENV.IsBound)
                return null;

            LocalBinding b = (LocalBinding)RT.get(LOCAL_ENV.deref(), symbol);
            if (b != null)
            {
                FnMethod method = (FnMethod)METHODS.deref();
                CloseOver(b, method);
            }

            return b;
        }

        static void CloseOver(LocalBinding b, FnMethod method)
        {
            if (b != null && method != null)
            {
                if (RT.get(method.Locals, b) == null)
                {
                    method.Fn.Closes = (IPersistentMap)RT.assoc(method.Fn.Closes, b, b);
                    CloseOver(b, method.Parent);
                }
                else if (IN_CATCH_FINALLY.deref() != null)
                {
                    method.LocalsUsedInCatchFinally = (PersistentHashSet)method.LocalsUsedInCatchFinally.cons(b.Index);
                }
            }
        }

        internal static Symbol TagOf(object o)
        {
            object tag = RT.get(RT.meta(o), RT.TAG_KEY);
            if (tag is Symbol)
                return (Symbol)tag;
            else if (tag is string)
                return Symbol.intern(null, (String)tag);
            return null;
        }


        internal static Type MaybeType(object form, bool stringOk)
        {
            if (form is Type)
                return (Type)form;

            Type t = null;
            if (form is Symbol)
            {
                Symbol sym = (Symbol)form;
                if (sym.Namespace == null) // if ns-qualified, can't be classname
                {
                    // TODO:  This uses Java  [whatever  notation.  Figure out what to do here.
                    if (sym.Name.IndexOf('.') > 0 || sym.Name[0] == '[')
                        t = RT.classForName(sym.Name);
                    else
                    {
                        object o = CurrentNamespace.GetMapping(sym);
                        if (o is Type)
                            t = (Type)o;
                    }

                }
            }
            else if (stringOk && form is string)
                t = RT.classForName((string)form);

            return t;
        }

        internal static Type TagToType(object tag)
        {
            Type t = MaybeType(tag, true);
            if (tag is Symbol)
            {
                Symbol sym = (Symbol)tag;
                if (sym.Namespace == null) // if ns-qualified, can't be classname
                {
                    switch (sym.Name)
                    {
                        case "ints": t = typeof(int[]); break;
                        case "longs": t = typeof(long[]); break;
                        case "floats": t = typeof(float[]); break;
                        case "doubles": t = typeof(double[]); break;
                        case "chars": t = typeof(char[]); break;
                        case "shorts": t = typeof(short[]); break;
                        case "bytes": t = typeof(byte[]); break;
                        case "booleans":
                        case "bools": t = typeof(bool[]); break;
                    }
                }
            }
            else if (tag is String)
            {
                // TODO: Find a general solution to this problem.
                string strTag = (string)tag;
                switch (strTag)
                {
                    case "Object[]":
                    case "object[]":
                        t = typeof(object[]);
                        break;
                    case "Object[][]":
                    case "object[][]":
                        t = typeof(object[][]);
                        break;
                }
            }
                    
            if (t != null)
                return t;

            throw new ArgumentException("Unable to resolve typename: " + tag);
        }    

        private static IPersistentMap CHAR_MAP = PersistentHashMap.create('-', "_",
            //		                         '.', "_DOT_",
             ':', "_COLON_",
             '+', "_PLUS_",
             '>', "_GT_",
             '<', "_LT_",
             '=', "_EQ_",
             '~', "_TILDE_",
             '!', "_BANG_",
             '@', "_CIRCA_",
             '#', "_SHARP_",
             '$', "_DOLLARSIGN_",
             '%', "_PERCENT_",
             '^', "_CARET_",
             '&', "_AMPERSAND_",
             '*', "_STAR_",
             '|', "_BAR_",
             '{', "_LBRACE_",
             '}', "_RBRACE_",
             '[', "_LBRACK_",
             ']', "_RBRACK_",
             '/', "_SLASH_",
             '\\', "_BSLASH_",
             '?', "_QMARK_"
             );


        public static string Munge(string name)
        {
            StringBuilder sb = new StringBuilder();
            foreach (char c in name)
            {
                string sub = (string)CHAR_MAP.valAt(c);
                if (sub == null)
                    sb.Append(c);
                else
                    sb.Append(sub);
            }
            return sb.ToString();
        }


        //private static Expr OptionallyGenerateMetaInit(object form, Expr expr)
        //{
        //    Expr ret = expr;

        //    if (RT.meta(form) != null )
        //    {
        //        Expression metaExpr = new MetaExpr(expr, GenerateMapExpr(o.meta());
        //        ret = Expression.Call(Expression.Convert(expr, typeof(IObj)), Method_IObj_withMeta, metaExpr);
        //    }
        //    return ret;
        //}

        #endregion

        #region Code generation

        internal static Expression GenerateDlrExpression(GenContext context, Expr expr)
        {

            return expr.GenDlr(context);
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

        #region Interface to core.clj


        // The following methods are named (and initial LC) for core.clj compatibility

        public static object eval(object form)
        {
            LambdaExpression ast = Compiler.GenerateLambda(form, false);
            return ast.Compile().DynamicInvoke();
        }

        public static object macroexpand1(object form)
        {
            return Macroexpand1(form);
        }
        
        #endregion

        #region Loading

        public static object loadFile(string filename)
        {
            FileInfo finfo = new FileInfo(filename);
            if ( ! finfo.Exists )
                throw new FileNotFoundException("Cannot find file to load",filename);

            using (TextReader rdr = finfo.OpenText())
                return load(rdr, finfo.FullName, finfo.Name);
        }


        public static object load(TextReader rdr)
        {
            return load(rdr, null, "NO_SOURCE_FILE");
        }

        public static object load(TextReader rdr, string sourcePath, string sourceName)
        {
            object ret = null;
            object eofVal = new object();
            object form;

            LineNumberingTextReader lntr =
                (rdr is LineNumberingTextReader) ? (LineNumberingTextReader)rdr : new LineNumberingTextReader(rdr);

            Var.pushThreadBindings(RT.map(
                //LOADER, RT.makeClassLoader(),
                SOURCE_PATH, sourcePath,
                SOURCE, sourceName,
                RT.CURRENT_NS, RT.CURRENT_NS.deref(),
                LINE_BEFORE, lntr.LineNumber,
                LINE_AFTER, lntr.LineNumber
                ));

            try
            {
                while ((form = LispReader.read(lntr, false, eofVal, false)) != eofVal)
                {
                    LINE_AFTER.set(lntr.LineNumber);
                    LambdaExpression ast = Compiler.GenerateLambda(form, false);  
                    ret = ast.Compile().DynamicInvoke();
                    LINE_BEFORE.set(lntr.LineNumber);
                }
            }
            catch (LispReader.ReaderException e)
            {
                throw new CompilerException(sourceName, e.Line, e.InnerException);
            }
            finally
            {
                Var.popThreadBindings();
            }

            return ret;
        }
  

        //public Delegate GenerateTypedDelegate(Type delegateType, Symbol optName, IPersistentVector argList, ISeq body)
        //{
        //    ScriptSource scriptSource = Engine.CreateScriptSourceFromString("<internal>");

        //    LambdaExpression ast = Generator.GenerateTypedDelegateExpression(GetLanguageContext(), delegateType, optName, argList, body);
        //    return ast.Compile();

        //    //ast = new GlobalLookupRewriter().RewriteLambda(ast);  -- doesn't work unless no args
        //    //ScriptCode code = new ScriptCode(ast, GetSourceUnit(scriptSource));
        //    //return code;
        //}
        //// This one is mine.
        //public static Delegate GenerateTypedDelegate(Type delegateType, Symbol optName, IPersistentVector argList, ISeq body)
        //{
            
        //}

        #endregion

        #region Compiling

        public static object TestCompile(string filename)
        {
            using (TextReader rdr = File.OpenText(filename))
                return Compile(rdr, null, filename);
        }

        internal static object Compile(TextReader rdr, string sourceDirectory, string sourceName)
        {
            if (COMPILE_PATH.deref() == null)
                throw new Exception("*compile-path* not set");

            object eofVal = new object();
            object form;

            string sourcePath = sourceDirectory == null ? sourceName : sourceDirectory + "\\" + sourceName;

            LineNumberingTextReader lntr =
                (rdr is LineNumberingTextReader) ? (LineNumberingTextReader)rdr : new LineNumberingTextReader(rdr);

            Var.pushThreadBindings(RT.map(
                SOURCE_PATH, sourcePath,
                SOURCE, sourceName,
                RT.CURRENT_NS, RT.CURRENT_NS.deref(),
                LINE_BEFORE, lntr.LineNumber,
                LINE_AFTER, lntr.LineNumber,
                CONSTANTS, PersistentVector.EMPTY,
                KEYWORDS, PersistentHashMap.EMPTY,
                VARS, PersistentHashMap.EMPTY
                ));
            try
            {
                GenContext context = new GenContext(sourceName, sourceDirectory, CompilerMode.File);
                TypeBuilder exprTB = context.ModuleBldr.DefineType("__REPL__", TypeAttributes.Class | TypeAttributes.Public);

                List<string> names = new List<string>();

                int i = 0;
                while ((form = LispReader.read(lntr, false, eofVal, false)) != eofVal)
                {
                    LINE_AFTER.set(lntr.LineNumber);
                    LambdaExpression ast = Compiler.GenerateLambda(context,form, false); 

                    // Compile to assembly
                    MethodBuilder methodBuilder = exprTB.DefineMethod(String.Format("REPL_{0:0000}", i++), 
                        MethodAttributes.Public | MethodAttributes.Static);
                    ast.CompileToMethod(methodBuilder);

                    names.Add(methodBuilder.Name);

                    // evaluate in this environment
                    ast.Compile().DynamicInvoke();
                    LINE_BEFORE.set(lntr.LineNumber);
                }

                Type exprType = exprTB.CreateType();

                // Need to put the loader init in its own type because we can't generate calls on the MethodBuilders
                //  until after their types have been closed.

                TypeBuilder initTB = context.ModuleBldr.DefineType("__Init__", TypeAttributes.Class | TypeAttributes.Public);


                Expression pushNSExpr = Expression.Call(null, Method_Compiler_PushNS);
                Expression popExpr = Expression.Call(null, Method_Var_popThreadBindings);

                List<Expression> inits = new List<Expression>();
                foreach (string name in names)
                {
                    Expression call = Expression.Call(exprType, name, Type.EmptyTypes);
                    inits.Add(call);
                }

                Expression tryCatch = Expression.TryCatchFinally(Expression.Block(inits), popExpr);

                Expression body = Expression.Block(pushNSExpr, tryCatch);

                // create initializer call
                MethodBuilder mbInit = initTB.DefineMethod("Initialize", MethodAttributes.Public | MethodAttributes.Static);
                LambdaExpression initFn = Expression.Lambda(body);
                initFn.CompileToMethod(mbInit);

                initTB.CreateType();

                context.AssyBldr.Save(sourceName  + ".dll");
            }
            catch (LispReader.ReaderException e)
            {
                throw new CompilerException(sourceName, e.Line, e.InnerException);
            }
            finally
            {
                Var.popThreadBindings();
            }
            return null;
        }

        public static void PushNS()
        {
            Var.pushThreadBindings(PersistentHashMap.create(Var.intern(Symbol.create("clojure.core"),
                                                                       Symbol.create("*ns*")), null));
        }


        internal static bool LoadAssembly(FileInfo assyInfo)
        {
            Assembly assy = Assembly.LoadFile(assyInfo.FullName);
            Type initType = assy.GetType("__Init__");
            if (initType == null)
            {
                Console.WriteLine("Bad assembly");
                return false;
            }
            try
            {
                initType.InvokeMember("Initialize", BindingFlags.InvokeMethod | BindingFlags.Static | BindingFlags.Public, Type.DefaultBinder, null, new object[0]);
                return true;
            }
            catch (Exception e)
            {
                Console.WriteLine("Error initializing {0}: {1}", assyInfo.FullName, e.Message);
                return false;
            }
        }

        #endregion

        #region CompilerException

        public sealed class CompilerException : Exception
        {
            public CompilerException(string source, int line, Exception cause)
                : base(ErrorMsg(source, line, cause.ToString()), cause)
            {
            }

            public override string ToString()
            {
                return Message;
            }

            static string ErrorMsg(string source, int line, string s)
            {
                return string.Format("{0} ({1}:{2})",s, source,line);
            }

        }   

        #endregion

        #region Things to move elsewhere

 

        internal static Type MaybePrimitiveType(Expr e)
        {
            if (e is MaybePrimitiveExpr && e.HasClrType)
            {
                Type t = e.ClrType;
                if (Util.IsPrimitive(t))
                    return t;
            }
            return null;
        }



        internal static Expression GenArgArray(GenContext context, IPersistentVector args)
        {
            Expression[] exprs = new Expression[args.count()];

            for (int i = 0; i < args.count(); i++)
            {
                Expr arg = (Expr)args.nth(i);
                exprs[i] = Compiler.MaybeBox(arg.GenDlr(context));
            }

            Expression argArray = Expression.NewArrayInit(typeof(object), exprs);
            return argArray;
        }

        internal static Expression[] GenTypedArgArray(GenContext context, ParameterInfo[] infos, IPersistentVector args)
        {
            Expression[] exprs = new Expression[args.count()];

            for (int i = 0; i < infos.Length; i++)
            {
                Expr e = (Expr)args.nth(i);
                // Java: this is in a try/catch, where the catch prints a stack trace
                if (MaybePrimitiveType(e) == infos[i].ParameterType)
                    exprs[i] = ((MaybePrimitiveExpr)e).GenDlrUnboxed(context);
                else
                    // Java follows this with: HostExpr.emitUnboxArg(fn, gen, parameterTypes[i]);
                    //exprs[i] = e.GenDlr(context);
                    exprs[i] = Expression.Convert(e.GenDlr(context), infos[i].ParameterType); ;
            }
            return exprs;
        }



        #endregion
    }
}
