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
using Microsoft.Linq.Expressions;
using System.Reflection;
using clojure.lang;
using Microsoft.Scripting.Ast;
using System.IO;
using System.Threading;
using AstUtils = Microsoft.Scripting.Ast.Utils;

using clojure.runtime;

namespace clojure.compiler
{
    public static class Generator
    {
        #region Data

        static readonly Symbol ISEQ = Symbol.create("clojure.lang.ISeq");
        static readonly Symbol NS = Symbol.create("ns");
        static readonly Symbol IN_NS = Symbol.create("in-ns");
        static readonly Keyword INLINE_KEY = Keyword.intern(null, "inline");
        static readonly Keyword INLINE_ARITIES_KEY = Keyword.intern(null,"inline-arities");


        #endregion

        #region MethodInfos, etc.

        static readonly MethodInfo Method_ArraySeq_create_array_int = typeof(ArraySeq).GetMethod("create",new Type[] { typeof(object[]),typeof(int)});

        static readonly MethodInfo Method_CGen_MakeMap = typeof(Generator).GetMethod("MakeMap");
        static readonly MethodInfo Method_CGen_MakeSet = typeof(Generator).GetMethod("MakeSet");
        static readonly MethodInfo Method_CGen_MakeVector = typeof(Generator).GetMethod("MakeVector");

        static readonly MethodInfo Method_IObj_withMeta = typeof(IObj).GetMethod("withMeta");

        static readonly MethodInfo Method_Monitor_Enter = typeof(Monitor).GetMethod("Enter");
        static readonly MethodInfo Method_Monitor_Exit = typeof(Monitor).GetMethod("Exit");

        static readonly MethodInfo Method_Reflector_CallInstanceMethod = typeof(Reflector).GetMethod("CallInstanceMethod");
        static readonly MethodInfo Method_Reflector_CallStaticMethod = typeof(Reflector).GetMethod("CallStaticMethod");
        static readonly MethodInfo Method_Reflector_InvokeConstructor = typeof(Reflector).GetMethod("InvokeConstructor");

        static readonly MethodInfo Method_RT_ConvertToCRD = typeof(RT).GetMethod("ConvertToCRD");
        static readonly MethodInfo Method_RT_IsTrue = typeof(RT).GetMethod("IsTrue");
        static readonly MethodInfo Method_RT_map = typeof(RT).GetMethod("map");
        static readonly MethodInfo Method_RT_printToConsole = typeof(RT).GetMethod("printToConsole");
        static readonly MethodInfo Method_RT_vector = typeof(RT).GetMethod("vector");

        static readonly MethodInfo Method_Var_BindRoot = typeof(Var).GetMethod("BindRoot");
        static readonly MethodInfo Method_Var_get = typeof(Var).GetMethod("deref");
        static readonly MethodInfo Method_Var_set = typeof(Var).GetMethod("set");
        static readonly MethodInfo Method_Var_SetMeta = typeof(Var).GetMethod("SetMeta");

        static readonly ConstructorInfo Ctor_AFnImpl_0 = typeof(AFnImpl).GetConstructor(Type.EmptyTypes);
        static readonly ConstructorInfo Ctor_RestFnImpl_1 = typeof(RestFnImpl).GetConstructor(new Type[] {typeof(int)});

        static readonly MethodInfo[] Methods_IFn_invoke = new MethodInfo[MAX_POSITIONAL_ARITY+2];

        static Type[] CreateObjectTypeArray(int size)
        {
            Type[] typeArray = new Type[size];
            for (int i = 0; i < size; i++)
                typeArray[i] = typeof(Object);
            return typeArray;
        }



        #endregion

        #region Special forms map

        delegate Expression ExprGenerator(ISeq form);

        private static readonly Dictionary<Symbol, ExprGenerator> _specials = new Dictionary<Symbol, ExprGenerator>();

        static Generator()
        {
            _specials.Add(Compiler.DEF, GenerateDefExpr);
            _specials.Add(Compiler.LOOP, GenerateLetExpr);
            _specials.Add(Compiler.RECUR, GenerateRecurExpr);
            _specials.Add(Compiler.IF, GenerateIfExpr);
            _specials.Add(Compiler.LET, GenerateLetExpr);
            _specials.Add(Compiler.DO, GenerateBodyExpr);
            _specials.Add(Compiler.FN, GenerateFnExpr);
            _specials.Add(Compiler.QUOTE, GenerateQuoteExpr);
            _specials.Add(Compiler.THE_VAR, GenerateTheVarExpr);
            _specials.Add(Compiler.DOT, GenerateHostExpr);
            _specials.Add(Compiler.ASSIGN, GenerateAssignExpr);
            _specials.Add(Compiler.TRY, GenerateTryExpr);
            _specials.Add(Compiler.THROW, GenerateThrowExpr);
            _specials.Add(Compiler.MONITOR_ENTER, GenerateMonitorEnterExpr);
            _specials.Add(Compiler.MONITOR_EXIT, GenerateMonitorExitExpr);
            _specials.Add(Compiler.NEW, GenerateNewExpr);

            for (int i = 0; i <= MAX_POSITIONAL_ARITY; i++)
                Methods_IFn_invoke[i] = typeof(IFn).GetMethod("invoke", CreateObjectTypeArray(i));

            Type[] types = new Type[MAX_POSITIONAL_ARITY + 1];
            CreateObjectTypeArray(MAX_POSITIONAL_ARITY).CopyTo(types, 0);
            types[MAX_POSITIONAL_ARITY ] = typeof(object[]);
            Methods_IFn_invoke[MAX_POSITIONAL_ARITY + 1]
                = typeof(IFn).GetMethod("invoke",
                       BindingFlags.Public | BindingFlags.InvokeMethod,
                       Type.DefaultBinder,
                       CallingConventions.VarArgs | CallingConventions.HasThis,
                       types,
                       null);
 
        
        }

        static bool HasSpecialFormGenerator(object head)
        {
            return head is Symbol && _specials.ContainsKey(head as Symbol);
        }

        static ExprGenerator GetSpecialFormGenerator(object head)
        {
            return _specials[head as Symbol];
        }



        #endregion
        
        #region C-tors & factory methods

        public static LambdaExpression Generate(object form, bool addPrint)
        {
            Expression formExpr = Generate(form);

            Expression finalExpr = formExpr; 
            
            if (formExpr.Type == typeof(void))
                finalExpr = Expression.Block(formExpr, Expression.Constant(null));


            if (addPrint)
            {
                finalExpr = Expression.Call(Method_RT_printToConsole, finalExpr);
            }

            return Expression.Lambda(finalExpr, "REPLCall", null);
        }

        private static string MaybeToString(object x)
        {
            return x == null ? string.Empty : x.ToString();
        }

        public  static LambdaExpression Generate(object p, Microsoft.Scripting.SourceUnit sourceUnit)
        {
            // TODO: Deal with sourceUnit
            return Generate(p,false);
        }

        public static Expression Eval(ClojureContext clc, object form)
        {
            return Generate(form);
        }

        public static object Macroexpand1(ClojureContext clc, object form)
        {
            if (!(form is ISeq))
                return form;
            return MacroexpandSeq1((ISeq)form);
        }


        public static LambdaExpression GenerateTypedDelegateExpression(ClojureContext clc, Type delegateType, Symbol name, IPersistentVector parameters, ISeq body)
        {
            return GenerateTypedDelegateExpression(delegateType, name, parameters, body);
        }

        #endregion

        #region Entry points

        private static Expression Generate(object form)
        {
            if (form == null)
                return GenerateNilExpr();
            else if (form is Boolean)
                return ((bool)form) ? GenerateTrueExpr() : GenerateFalseExpr();

            Type type = form.GetType();

            if (type == typeof(Symbol))
                return GenerateSymbolExpr((Symbol)form);
            else if (type == typeof(Keyword))
                return GenerateKeywordExpr((Keyword)form);
            else if (type == typeof(String))
                return GenerateStringExpr((String)form);
            else if (form is IPersistentCollection && ((IPersistentCollection)form).count() == 0)
                return GenerateEmptyExpr(form);
            else if (form is ISeq)
                return GenerateSeqExpr((ISeq)form);
            else if (form is IPersistentVector)
                return GenerateVectorExpr((IPersistentVector)form);
            else if (form is IPersistentMap)
                return GenerateMapExpr((IPersistentMap)form);
            else if (form is IPersistentSet)
                return GenerateSetExpr((IPersistentSet)form);
            else
                return GenerateConstExpr(form);
        }

        

        #endregion

        #region Various constant expressions

        private static ConstantExpression NIL_EXPR = Expression.Constant(null);
        private static ConstantExpression TRUE_EXPR = Expression.Constant(RT.T);
        private static ConstantExpression FALSE_EXPR = Expression.Constant(RT.F);
        
        private static Expression GenerateConstExpr(object form)
        {
            return Expression.Constant(form);
        }

        private static Expression GenerateNilExpr()
        {
            return NIL_EXPR;
        }

        private static Expression GenerateTrueExpr()
        {
            return TRUE_EXPR;
        }

        private static Expression GenerateFalseExpr()
        {
            return FALSE_EXPR;
        }

        private static Expression GenerateKeywordExpr(Keyword keyword)
        {
            // in the Java version:
            //if (!KEYWORDS.isBound())
            //    return new KeywordExpr(keyword);
            //IPersistentMap keywordsMap = (IPersistentMap)KEYWORDS.get();
            //Object id = RT.get(keywordsMap, keyword);
            //if (id == null)
            //{
            //    KEYWORDS.set(RT.assoc(keywordsMap, keyword, registerConstant(keyword)));
            //}
            //return new KeywordExpr(keyword);

            return Expression.Constant(keyword);
        }

        private static Expression GenerateStringExpr(string p)
        {
            return Expression.Constant(String.Intern(p));
        }



        #endregion

        #region Helpers

        private static Namespace CurrentNamespace
        {
            get { return Compiler.CurrentNamespace; }
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

        public static string munge(string name)
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

        private static Symbol TagOf(object o)
        {
            //IObj iobj = o as IObj;
            //if ( iobj != null && iobj.meta() != null )
            //{
            //    object tag = iobj.meta().valAt(RT.TAG_KEY);
            //    if ( tag is Symbol )
            //        return (Symbol) tag;
            //    else if ( tag is string )
            //        return Symbol.intern(null, (string) tag);
            //}
            //return null;
            object tag = RT.get(RT.meta(o), RT.TAG_KEY);
            if (tag is Symbol)
                return (Symbol)tag;
            else if (tag is string)
                return Symbol.intern(null, (String)tag);
            return null;
        }

        #endregion

        #region Symbols

        // var > constid

        // this ties into local variables and vars
        private static Expression GenerateSymbolExpr(Symbol symbol)
        {
            Symbol tag = TagOf(symbol);

            if (symbol.Namespace == null)
            {
                LocalBinding b = ReferenceLocal(symbol);
                if (b != null)
                    return b.ParamExpression;  //asdf-tag
            }
            else
            {
                if (Compiler.namespaceFor(symbol) == null)
                {
                    Symbol nsSym = Symbol.create(symbol.Namespace);
                    Type t = MaybeType(nsSym, false);
                    if (t != null)
                        if ( Reflector.GetField(t,symbol.Name,true) != null )
                            return GenerateStaticFieldExpr(t,symbol.Name);
                }
            }

            object o = Compiler.Resolve(symbol);
            if (o is Var)
            {
                Var v = (Var)o;
                if (IsMacro(v) != null)
                    throw new Exception("Can't take the value of a macro: " + v);
                RegisterVar(v);
                return GenerateVarExpr(v, tag);
            }
            else if (o is Type)
                return GenerateConstExpr(o);
            else if (o is Symbol)
                return GenerateUnresolvedVarExpr((Symbol)o);

            throw new Exception(string.Format("Unable to resolve symbol: {0} in this context", symbol));
        }

        private static Type MaybeType(object form, bool stringOk)
        {
            if (form is Type)
                return (Type)form;

            Type t = null;
            if (form is Symbol)
            {
                Symbol sym = (Symbol)form;
                if (sym.Namespace == null) // if ns-qualified, can't be classname
                {
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

        
        private static void RegisterVar(Var v)
        {
            // do nothing, I think, in my implementation.
            // However, this may be needed when writing out a binary file
        }

        private static Var IsMacro(Object op)
        {
            if (op is Symbol && ReferenceLocal((Symbol)op) != null)
                return null;
            if (op is Symbol || op is Var)
            {
                Var v = (op is Var) ? (Var)op : lookupVar((Symbol)op, false);
                if (v != null && v.IsMacro)
                {
                    if (v.Namespace != CurrentNamespace && !v.IsPublic)
                        throw new InvalidOperationException(string.Format("Var: {0} is not public", v));
                    return v;
                }
            }
            return null;
        }


        private static LocalBinding ReferenceLocal(Symbol symbol)
        {
            if (!LOCAL_ENV.IsBound)
                return null;
            LocalBinding b = (LocalBinding)((IPersistentMap)LOCAL_ENV.deref()).valAt(symbol);
            //if (b != null)
            //{
            //    MethodDef method = (MethodDef)METHODS.get();
            //    //  here is where we might note a variable to close over.
            //    // need to move up the chain here?????????????????????????????????????????????????????
            //    // I don't think we need method.localsUsedInCatchFinally
            //    //if (method.Locals.valAt(b) != null && IN_CATCH_FINALLY.get() != null)
            //    //    method.localsUsedinCatchFinally = method.localsUsedinCatchFinally.cons(b); // do we need this?
            //}
            return b;
        }

        static Var lookupVar(Symbol sym, bool internNew)
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

        private static Expression GenerateUnresolvedVarExpr(Symbol symbol)
        {
            return null;  // ??????
        }

        private static Expression GenerateVarExpr(Var v, Symbol tag)
        {
            object tagToUse = tag ?? v.Tag;

            Expression expr = Expression.Call(Expression.Constant(v), Method_Var_get);  //asdf-tag
            //if (tagToUse != null)
            //    expr = Expression.Convert(expr, TagToType(tagToUse));  // NOPE
            return expr;
        }

        private static Expression GenerateStaticFieldExpr(Type t, string fieldName)
        {
            //return Expression.Field(Expression.Constant(t), fieldName);
            return Expression.Field(null, t, fieldName);
        }

        
        #endregion
      
        #region General collections

        static Expression EMPTY_VECTOR_EXPR = Expression.Constant(PersistentVector.EMPTY);
        static Expression EMPTY_LIST_EXPR = Expression.Constant(PersistentList.EMPTY);
        static Expression EMPTY_HASHMAP_EXPR = Expression.Constant(PersistentArrayMap.EMPTY);
        static Expression EMPTY_HASHSET_EXPR = Expression.Constant(PersistentHashSet.EMPTY);

        private static Expression GenerateEmptyExpr(object form)
        {
            Expression expr = null;

            if (form is IPersistentList)
                expr = EMPTY_LIST_EXPR;
            else if (form is IPersistentVector)
                expr = EMPTY_VECTOR_EXPR;
            else if (form is IPersistentMap)
                expr = EMPTY_HASHMAP_EXPR;
            else if (form is IPersistentSet)
                expr = EMPTY_HASHSET_EXPR;
            else
                throw new InvalidOperationException("Unknown collection type.");

            if (RT.meta(form) != null)
            {
                expr = OptionallyGenerateMetaInit(form, expr);
            }
            return expr;
        }

        private static Expression GenerateVectorExpr(IPersistentVector v)
        {
            int n = v.count();
            Expression[] args = new Expression[v.count()];
            for (int i = 0; i < n; i++)
                args[i] = Generate(v.nth(i));

            Expression arrayExpr = Expression.NewArrayInit(typeof(object), MaybeBox(args));
            Expression ret = Expression.Call(Method_RT_vector, arrayExpr);
            ret = OptionallyGenerateMetaInit(v,ret);

            return ret;
        }


        private static Expression GenerateMapExpr(IPersistentMap m)
        {
            Expression[] args = new Expression[m.count() * 2];
            int i = 0;
            for ( ISeq s = RT.seq(m); s != null; s = s.rest(), i+=2)
            {
                IMapEntry me = (IMapEntry)s.first();
                args[i] = MaybeBox(Generate(me.key()));
                args[i + 1] = MaybeBox(Generate(me.val()));
            }
            Expression argArray = Expression.NewArrayInit(typeof(object), args);

            Expression ret = Expression.Call(Method_RT_map,argArray);
            ret = OptionallyGenerateMetaInit(m,ret);

            return ret;
        }

        private static Expression GenerateSetExpr(IPersistentSet set)
        {
            Expression[] args = new Expression[set.count()];
            int i = 0;
            for (ISeq s = RT.seq(set); s != null; s = s.rest(), i++)
                args[i] = MaybeBox(Generate(s.first()));

            Expression argArray = Expression.NewArrayInit(typeof(object), args);

            Expression ret = Expression.Call(Method_CGen_MakeSet, argArray);
            ret = OptionallyGenerateMetaInit(set, ret);

            return ret;
        }


        public static IPersistentVector MakeVector(params object[] elements)
        {
            return LazilyPersistentVector.createOwning(elements);
        }

        public static IPersistentMap MakeMap(params object[] init)
        {
            return( init != null && init.Length == 2 )
                ? (IPersistentMap) new PersistentArrayMap(init)
                : (IPersistentMap) PersistentHashMap.create(init);
        }

        public static IPersistentSet MakeSet(params object[] elements)
        {
            return PersistentHashSet.create(elements);
        }

        private static Expression OptionallyGenerateMetaInit(object form, Expression expr)
        {
            Expression ret = expr;

            IObj o = form as IObj;
            if (o != null && o.meta() != null)
            {
                Expression metaExpr = GenerateMapExpr(o.meta());
                ret = Expression.Call(Expression.Convert(expr, typeof(IObj)),Method_IObj_withMeta, metaExpr);
            }
            return ret;
        }


        #endregion

        #region ISeq forms = calls

        private static Expression GenerateSeqExpr(ISeq form)
        {
            object exp = MacroexpandSeq1(form);
            if (exp != form)
                return Generate(exp);

            object op = RT.first(form);

            IFn inline = IsInline(op, RT.count(RT.rest(form)));

            if (inline != null)
                return Generate(inline.applyTo(RT.rest(form)));
            else if (HasSpecialFormGenerator(op))
                return GetSpecialFormGenerator(op)(form);
            else
                return GenerateInvoke(form);
        }

        private static Expression GenerateInvoke(ISeq form)
        {
            Expression fn = Generate(form.first());

            fn = Expression.Convert(fn,typeof(IFn));

            ISeq s = RT.seq(form.rest());
            int n = s == null ? 0 : s.count();
            Expression[] args = new Expression[n];
            for (int i = 0; s != null; s = s.rest(), i++)
                args[i] = MaybeBox(Generate(s.first()));

            Type returnType = ComputeInvocationReturnType(form.first(), form);

            Expression call = GenerateInvocation(returnType, fn, args);

            return call;
        }

        private static Type ComputeInvocationReturnType(object op, ISeq form)
        {
            Symbol tag = TagOf(form);
            if (tag == null && op is Symbol)
            {
                Symbol sym = (Symbol)op;
                tag = TagOf(sym);
                if (tag == null)
                {
                    Var var = SymbolMapsToVar(sym);
                    if (var != null)
                        tag = var.Tag as Symbol;
                }
            }
            return (tag == null)
                ? null
                : TagToType(tag);
        }

        // Tremendously duplicative of GenerateSymbolExpr -- maybe just cache the info somewhere.
        static Var SymbolMapsToVar(Symbol symbol)
        {
            if ( symbol.Namespace == null  && ReferenceLocal(symbol) != null )
                // maps to local
                return null;

            if (symbol.Namespace != null && Compiler.namespaceFor(symbol) == null)
            {
                  Symbol nsSym = Symbol.create(symbol.Namespace);
                    Type t = MaybeType(nsSym, false);
                    if (t != null &&  Reflector.GetField(t,symbol.Name,true) != null )
                            return null;
            }

            object o = Compiler.Resolve(symbol);

            if ( o is Var )
                return (Var) o;

            return null;
        }

        private static Expression GenerateInvocation(Type returnType, Expression fn, Expression[] args)
        {
            MethodInfo mi;
            Expression[] actualArgs;

            if (args.Length <= MAX_POSITIONAL_ARITY)
            {
                mi = Methods_IFn_invoke[args.Length];
                actualArgs = args;
            }
            else
            {
                // pick up the extended version.
                mi = Methods_IFn_invoke[MAX_POSITIONAL_ARITY + 1];
                Expression[] leftoverArgs = new Expression[args.Length-MAX_POSITIONAL_ARITY];
                Array.ConstrainedCopy(args,MAX_POSITIONAL_ARITY,leftoverArgs,0,args.Length-MAX_POSITIONAL_ARITY);

                Expression restArg = Expression.NewArrayInit(typeof(object), leftoverArgs);

                actualArgs = new Expression[MAX_POSITIONAL_ARITY + 1];
                Array.ConstrainedCopy(args, 0, actualArgs, 0, MAX_POSITIONAL_ARITY);
                actualArgs[MAX_POSITIONAL_ARITY] = restArg;
            }

            Expression call = Expression.Call(fn, mi, actualArgs);
            // Java version doesn't seem to do this.  Instead, its InvokeExpression carries the type information so someone else can use it.
            // Not sure if this is useful here.
            if (returnType != null)
                call = Expression.Convert(call, returnType);

            return call;
        }

        private static object MacroexpandSeq1(ISeq form)
        {
            object op = RT.first(form);
            if (Compiler.isSpecial(op))
                return form;

            // macro expansion
            Var v = IsMacro(op);
            if (v != null)
            {
                try
                {
                    Var.pushThreadBindings(RT.map(RT.MACRO_META, RT.meta(form)));
                    return v.applyTo(form.rest());
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
                        //object target = Second(form);
                        //if (MaybeType(target, false) != null)
                        //    target = RT.list(Compiler.IDENTITY, target);
                        //return RT.listStar(Compiler.DOT, target, method, form.rest().rest());
                        return RT.listStar(Compiler.DOT, RT.second(form), method, form.rest().rest());
                    }
                    else if (NamesStaticMember(sym))
                    {
                        Symbol target = Symbol.intern(sym.Namespace);
                        Type t = MaybeType(target, false);
                        if (t != null)
                        {
                            Symbol method = Symbol.intern(sym.Name);
                            return RT.listStar(Compiler.DOT, target, method, form.rest());
                        }
                    }
                    else
                    {
                        // (x.substring 2 5) =>  (. s substring 2 5)
                        int index = sname.LastIndexOf('.');
                        if (index == sname.Length - 1)
                            return RT.listStar(Compiler.NEW, Symbol.intern(sname.Substring(0, index)), form.rest());
                    }
                }

            }
            return form;
        }


        public static bool NamesStaticMember(Symbol sym)
        {
            return sym.Namespace != null && Compiler.NamespaceFor(sym) == null;
        }


        private static IFn IsInline(object op, int arity)
        {
            // Java:  	//no local inlines for now
            if (op is Symbol && ReferenceLocal((Symbol)op) != null)
                return null;
            if (op is Symbol || op is Var)
            {
                Var v = (op is Var) ? (Var)op : lookupVar((Symbol)op, false);
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

        #endregion

        #region Special form generation

        private static Expression GenerateQuoteExpr(ISeq form)
        {
            object v = form.rest().first();

            return v == null ? GenerateNilExpr() : GenerateConstExpr(v);
        }

        private static Expression GenerateIfExpr(ISeq form)
        {
            if (form.count() > 4)
                throw new Exception("Too many arguments to if");

            if (form.count() < 3)
                throw new Exception("Too few arguments to if");

            //form = form.rest();
            //object test = form.first();
            //form = form.rest();
            //object trueClause = form.first();
            //form = form.rest();
            //object falseClause = form == null ? null : form.first();
            object test = RT.second(form);
            object trueClause = RT.third(form);
            object falseClause = RT.fourth(form);

            // TODO: if test has Boolean type, no need to box, just test directly.
            Expression realExpr = Expression.Call(Method_RT_IsTrue, MaybeBox(Generate(test)));
            Expression thenExpr = Generate(trueClause);
            Expression elseExpr = Generate(falseClause);

            if (thenExpr.Type != elseExpr.Type)
            {
                // Try to reconcile
                if (thenExpr.Type.IsAssignableFrom(elseExpr.Type) && elseExpr.Type != typeof(void))
                    elseExpr = Expression.Convert(elseExpr, thenExpr.Type);
                else if (elseExpr.Type.IsAssignableFrom(thenExpr.Type) && thenExpr.Type != typeof(void))
                    thenExpr = Expression.Convert(thenExpr, elseExpr.Type);
                else
                {
                    if (thenExpr.Type == typeof(void))
                        thenExpr = Expression.Block(thenExpr, Expression.Default(elseExpr.Type));
                    else if (elseExpr.Type == typeof(void))
                        elseExpr = Expression.Block(elseExpr, Expression.Default(thenExpr.Type));
                    else
                    {
                        // TODO: Can we find a common ancestor?  probably not.
                        thenExpr = Expression.Convert(thenExpr, typeof(object));
                        elseExpr = Expression.Convert(elseExpr, typeof(object));
                    }
                }
            }
            return Expression.Condition(realExpr, thenExpr, elseExpr);
        }

        private static Expression GenerateBodyExpr(ISeq form)
        {
            ISeq forms = (Compiler.DO.Equals(RT.first(form))) ? RT.rest(form) : form;

            Expression[] exprs;

            if ( forms == null )
            {
                exprs = new Expression[1];
                exprs[0] = GenerateNilExpr();
            }
            else 
            {
                exprs = new Expression[forms.count()];
                int i=0;
                for (ISeq s = forms; s != null; s = s.rest(), i++)
                {
                    if (s.rest() == null)
                    {
                        // in tail recurive position
                        try
                        {
                            Var.pushThreadBindings(PersistentHashMap.create(IN_TAIL_POSITION, RT.T));
                            exprs[i] = Generate(s.first());
                        }
                        finally
                        {
                            Var.popThreadBindings();
                        }
                    }
                    else
                        exprs[i] = Generate(s.first());
                }                 
            }

            return Expression.Block(exprs);
        }

        private static Expression GenerateTheVarExpr(ISeq form)
        {
            Symbol sym = RT.second(form) as Symbol;
            Var v = lookupVar(sym, false);
            if (v != null)
                return GenerateConstExpr(v);  // Really not sure on this one.
            throw new Exception(string.Format("Unable to resolve var: {0} in this context", sym));
        }

        private static Expression GenerateDefExpr(ISeq form)
        {
            if (form.count() > 3)
                throw new Exception("Too many arguments to def");

            if (form.count() < 2)
                throw new Exception("Too few arguments to def");

            Symbol sym = RT.second(form) as Symbol;
            bool initProvided = form.count() == 3;

            if (sym == null)
                throw new Exception("Second argument to def must be a Symbol.");

            Var v = lookupVar(sym, true);

            if (v == null)
                throw new Exception("Can't refer to qualified var that doesn't exist");

            if (!v.Namespace.Equals(CurrentNamespace))
            {
                if (sym.Namespace == null)
                    throw new Exception(string.Format("Name conflict, can't def {0} because namespace: {1} refers to: {2}",
                                sym, CurrentNamespace.Name, v));
                else
                    throw new Exception("Can't create defs outside of current namespace");
            }

            IPersistentMap mm = sym.meta();
            // TODO: add source line info metadata.
            //mm = (IPersistentMap) RT.assoc(RT.LINE_KEY, LINE.get()).assoc(RT.FILE_KEY, SOURCE.get());

            // Bizarrely, we don't have to do anything to actually create the var, the lookupVar did that for us.
            // Will this work in a compiled class file?


            List<Expression> exprs = new List<Expression>();

            Expression varExpr = GenerateConstExpr(v);

            if (initProvided)
                exprs.Add(Expression.Call(varExpr, Method_Var_BindRoot, MaybeBox(Generate(RT.third(form))))); ;

            if (mm != null)
                exprs.Add(Expression.Call(varExpr, Method_Var_SetMeta, GenerateMapExpr(mm)));

            exprs.Add(varExpr);

            return Expression.Block(exprs);
        }

        static Expression MaybeBox(Expression expr)
        {
            if (expr.Type == typeof(void))
                // I guess we'll pass a void.  This happens when we have a throw, for example.
                return Expression.Block(expr, Expression.Default(typeof(object)));
            
            return expr.Type.IsValueType
                ? Expression.Convert(expr, typeof(object))
                : expr;
        }


        static Expression[] MaybeBox(Expression[] args)
        {
            // TODO: avoid copying array if not necessary
            Expression[] boxedArgs = new Expression[args.Length];
            for (int i1 = 0; i1 < args.Length; ++i1)
                boxedArgs[i1] = MaybeBox(args[i1]);
            return boxedArgs;
        }

        //  DLR TryStatement has void type, so we must wrap it in a scope
        //  that has a target to return to.
        private static Expression GenerateTryExpr(ISeq form)
        {
            // (try try-expr* catch-expr* finall-expr?)
            // catch-expr: (catch classname sym expr*)
            // finally-expr: (finally expr*)

            IPersistentVector body = PersistentVector.EMPTY;
            List<CatchBlock> catches = new List<CatchBlock>();
            Expression finallyExpr = null;
            bool caught = false;

            for ( ISeq fs = form.rest(); fs != null; fs = fs.rest() )
            {
                object f = fs.first();
                object op = (f is ISeq) ? ((ISeq)f).first() : null;
                if (!Compiler.CATCH.Equals(op) && !Compiler.FINALLY.Equals(op))
                {
                    if ( caught )
                        throw new Exception("Only catch or finally clause can follow catch in try expression");
                    body = body.cons(f);
                }
                else 
                {
                    if (Compiler.CATCH.Equals(op))
                    {
                        ISeq f1 = f as ISeq;
                        Type t = MaybeType(RT.second(f1),false);
                        if ( t == null )
                            throw new ArgumentException("Unable to resolve classname: " + RT.second(form));
                        if ( ! (RT.third(f1) is Symbol ))
                            throw new ArgumentException("Bad binding form, expected symbol, got: " + RT.third(f1));
                        Symbol sym = (Symbol) RT.third(f1);
                        if ( sym.Namespace != null )
                            throw new Exception("Can't bind qualified name: " + sym);

                        IPersistentMap dynamicBindings = RT.map( LOCAL_ENV, LOCAL_ENV.deref(),
                            IN_CATCH_FINALLY, RT.T);

                        try
                        {
                            Var.pushThreadBindings(dynamicBindings);
                            LocalBinding lb = RegisterLocal(sym,
                                (Symbol)(RT.second(f1) is Symbol ? RT.second(f1) : null),
                                null);
                            ParameterExpression exParam = Expression.Parameter(typeof(object),sym.Name); //asdf-tag
                            lb.ParamExpression = exParam;
                            Expression handler = GenerateBodyExpr(f1.rest().rest().rest());
                            catches.Add(Expression.Catch(t, exParam, handler));
                        }
                        finally
                        {
                            Var.popThreadBindings();
                        }
                        caught = true;
                    }
                    else // finally
                    {
                        if ( fs.rest() != null )
                            throw new Exception("finally clause must be last in try expression");
                        try
                        {
                            Var.pushThreadBindings(RT.map(IN_CATCH_FINALLY,RT.T));
                            finallyExpr = GenerateBodyExpr(RT.rest(f));
                        }
                        finally
                        {
                            Var.popThreadBindings();
                        }
                    }
                }
            }

            Expression basicBody = GenerateBodyExpr(body.seq()); 
            // Wrap the basic body, a Comma, in a return to a label
            LabelTarget target = Expression.Label(basicBody.Type, "ret_label");
            Expression tryBody = Expression.Return(target, basicBody);
            TryExpression tryStmt = finallyExpr == null
                ? Expression.TryCatch(tryBody,catches.ToArray())
                : Expression.TryCatchFinally(tryBody, finallyExpr, catches.ToArray());
            // TODO: What if basicBody.Type is typeof(void)?  What if Enum?
            //Expression defaultValue = basicBody.Type.IsValueType ? Expression.Constant(0, basicBody.Type) : Expression.Null(basicBody.Type);
            Expression defaultValue = Expression.Default(basicBody.Type);
            Expression whole = Expression.Block(tryStmt, Expression.Label(target, defaultValue));
            return whole;
        }

        private static Expression GenerateThrowExpr(ISeq form)
        {
            return Expression.Throw(Expression.Convert(Generate(RT.second(form)), typeof(Exception)));
        }

        private static Expression GenerateMonitorEnterExpr(ISeq form)
        {
            return Expression.Call(Method_Monitor_Enter, Generate(RT.second(form)));
        }

        private static Expression GenerateMonitorExitExpr(ISeq form)
        {
            return Expression.Call(Method_Monitor_Exit, Generate(RT.second(form)));
        }

        #endregion

        #region Fn generation

        const int MAX_POSITIONAL_ARITY = 20;

        sealed class FnDef
        {
            string _name;
            public string Name
            {
                get { return _name; }
                set { _name = value; }
            }


            string _simpleName;
            public string SimpleName
            {
                get { return _simpleName; }
                set { _simpleName = value; }
            }

            string _internalName;

            public string InternalName
            {
                get { return _internalName; }
                set { _internalName = value; }
            }

            string _thisName;
            public string ThisName
            {
                get { return _thisName; }
                set { _thisName = value; }
            }

            ParameterExpression _thisParam;
            public ParameterExpression ThisParam
            {
                get { return _thisParam; }
                set { _thisParam = value; }
            }

            bool _isVariadic;

            public bool IsVariadic
            {
                get { return _isVariadic; }
                set { _isVariadic = value; }
            }


            public Type ImplType
            {
                get { return IsVariadic ? typeof(RestFnImpl) : typeof(AFnImpl); }
            }


            internal void ComputeNames(ISeq form)
            {
                MethodDef enclosingMethod = (MethodDef)METHODS.deref();

                string baseName = enclosingMethod != null
                    ? (enclosingMethod.Fn.Name + "$")
                    : (munge(CurrentNamespace.Name.Name) + "$");

                if (form.rest().first() is Symbol)
                    _thisName = ((Symbol)form.rest().first()).Name;

                _simpleName = (_thisName == null ? "fn" : munge(_thisName).Replace(".","_DOT_"));
                _name = baseName + _simpleName;
                _internalName = _name.Replace('.','/');
                // fn.fntype = Type.getObjectType(fn.internalName) -- JAVA            
            }
        }

        sealed class MethodDef
        {
            FnDef _fn;
            public FnDef Fn
            {
                get { return _fn; }
                set { _fn = value; }
            }

            MethodDef _parent;
            public MethodDef Parent
            {
                get { return _parent; }
                set { _parent = value; }
            }

            LambdaExpression _lambda;
            public LambdaExpression Lambda
            {
                get { return _lambda; }
                set { _lambda = value; }
            }

            // LocalBinding => LocalBinding
            // TODO: Why not use a set?
            IPersistentMap _locals = PersistentHashMap.EMPTY;
            public IPersistentMap Locals
            {
              get { return _locals; }
              set { _locals = value; }
            }

            // LocalBinding => LocalBinding
            // TODO: Why not use a set?
            IPersistentVector _reqParms = PersistentVector.EMPTY;
            public IPersistentVector ReqParms
            {
              get { return _reqParms; }
              set { _reqParms = value; }
            }

            LocalBinding _restParm = null;
            public LocalBinding RestParm
            {
              get { return _restParm; }
              set { _restParm = value; }
            }

            IPersistentVector _argLocals;
            public IPersistentVector ArgLocals
            {
              get { return _argLocals; }
              set { _argLocals = value; }
            }

            public int RequiredArity
            {
                get { return _reqParms.count(); }
            }

            public bool IsVariadic
            {
                get { return _restParm != null; }
            }

            public int NumParams
            {
                get { return RequiredArity + (IsVariadic ? 1 : 0); }
            }

            internal MethodDef(FnDef fn, MethodDef parent)
            {
                _fn = fn;
                _parent = parent;
            }
        }

        sealed class LocalBinding
        {
            private readonly Symbol _sym;
            public Symbol Symbol
            {
                get { return _sym; }
            }

            private readonly Symbol _tag;
            public Symbol Tag
            {
                get { return _tag; }
            }

            private readonly Expression _init;
            public Expression Init
            {
                get { return _init; }
            }

            private readonly String _name;
            public String Name
            {
                get { return _name; }
            }

            private Expression _paramExpression;
            public Expression ParamExpression
            {
                get { return _paramExpression; }
                set { _paramExpression = value; }
            }

            public LocalBinding(Symbol sym, Symbol tag, Expression init)
            {
                // Java version:
                //if(maybePrimitiveType(init) != null && tag != null)
                //    throw new UnsupportedOperationException("Can't type hint a local with a primitive initializer");

                _sym = sym;
                _tag = tag;
                _init = init;
                _name = munge(sym.Name);
            }
        }

        private static LocalBinding RegisterLocal(Symbol sym, Symbol tag, Expression init )
        {
            LocalBinding b = new LocalBinding(sym,tag,init);
            IPersistentMap localsMap = (IPersistentMap) LOCAL_ENV.deref();
            LOCAL_ENV.set(localsMap.assoc(b.Symbol,b));
            MethodDef method = (MethodDef)METHODS.deref();
            if ( method != null )
                method.Locals = (IPersistentMap)method.Locals.assoc(b,b);
            return b;
        }

        private static readonly Var METHODS = Var.create(null);
        private static readonly Var LOCAL_ENV = Var.create(PersistentHashMap.EMPTY);
        private static readonly Var LOOP_LOCALS = Var.create(null);


        // We need to pass the 'this' parameter to the methods when they are analyzed.
        // The type of the 'this' parameter depends on whether there is an [ ... & .] signature.
        // Do a quick scan to determine.
        static bool ComputeIsVariadicQuickly(ISeq body)
        {
            for (ISeq s = body; s != null; s = s.rest())
            {
                if (!(((ISeq)s.first()).first() is IPersistentVector))  // bad syntax -- will be caught later
                    return false;
                IPersistentVector paramList = (IPersistentVector)((ISeq)s.first()).first();
                for (int i = 0; i < paramList.count(); i++)
                    if (Compiler._AMP_.Equals(paramList.nth(i)))
                        return true;
            }
            return false;
        }

        private static Expression GenerateFnExpr(ISeq form)
        {
            // This naming convention drawn from the Java code.
            FnDef fn = new FnDef();
            fn.ComputeNames(form);

            Symbol name = null;
            if ( RT.second(form) is Symbol )
            {
                name = (Symbol)RT.second(form);
                form = RT.cons(Compiler.FN, RT.rest(RT.rest(form)));
            }

            // Normalize body
            // If it is (fn [arg...] body ...), turn it into
            //  (fn ([arg...] body...))
            // so that we can treat uniformly as (fn ([arg...] body...) ([arg...] body...) ... )
            if (RT.second(form) is IPersistentVector)
                form = RT.list(Compiler.FN, RT.rest(form));

            // needs to be called after normalization
            fn.IsVariadic = ComputeIsVariadicQuickly(RT.rest(form));


            // Create the 'this' parameter needed for recursion
            // we no longer need the name (second element) if it is given
            if (name != null )
            {
                // ThisName will be non-null;
                fn.ThisParam = Expression.Parameter(fn.ImplType, fn.ThisName);
            }

 
            MethodDef variadicMethod = null;
            SortedDictionary<int, MethodDef> methods = new SortedDictionary<int, MethodDef>();

            for (ISeq s = RT.rest(form); s != null; s = s.rest())
            {
                MethodDef method = GenerateFnMethod(fn, (ISeq) s.first());
                if (method.IsVariadic)
                {
                    if (variadicMethod == null)
                        variadicMethod = method;
                    else
                        throw new Exception("Can't have more than 1 variadic overload");
                }
                else if (! methods.ContainsKey(method.RequiredArity))
                    methods[method.RequiredArity] = method;
                else
                    throw new Exception("Can't have 2 overloads with the same arity.");
            }

            if ( variadicMethod != null && methods.Count > 0 && methods.Keys.Max() >= variadicMethod.NumParams )
                throw new Exception("Can't have fixed arity methods with more params than the variadic method.");

            if (fn.IsVariadic != (variadicMethod != null))
                throw new Exception("Internal error:  ComputeIsVariadicQuickly failed!!!");

            return GenerateFnLambda(fn, methods, variadicMethod);
        }

        enum ParamParseState { Required, Rest, Done };


        private static MethodDef GenerateFnMethod(FnDef fn, ISeq form)
        {
            // form == ([args] body ... )
            IPersistentVector parms = (IPersistentVector)RT.first(form);
            ISeq body = RT.rest(form);

            MethodDef method = new MethodDef(fn, (MethodDef)METHODS.deref());

            try
            {
                LabelTarget loopLabel = Expression.Label();

                Var.pushThreadBindings(PersistentHashMap.create(
                    METHODS, method,
                    LOOP_LABEL, loopLabel,
                    LOCAL_ENV, LOCAL_ENV.deref(),
                    LOOP_LOCALS, null));
                
                // register 'this' as local 0  
                LocalBinding thisB = RegisterLocal(Symbol.intern(fn.ThisName ?? "fn__" + RT.nextID()), null, null);  //asdf-tag
                thisB.ParamExpression = fn.ThisParam;


                IPersistentVector argLocals = PersistentVector.EMPTY;
                int parmsCount = parms.count();
                ParamParseState paramState = ParamParseState.Required;

                for (int i = 0; i < parmsCount; i++)
                {
                    if (!(parms.nth(i) is Symbol))
                        throw new ArgumentException("fn params must be Symbols");
                    Symbol p = parms.nth(i) as Symbol;
                    if (p.Namespace != null)
                        throw new Exception("Can't use qualified name as parameter: " + p);
                    if (p.Equals(Compiler._AMP_))
                    {
                        if (paramState == ParamParseState.Required)
                            paramState = ParamParseState.Rest;
                        else
                            throw new Exception("Invalid parameter list");
                    }
                    else
                    {
                        LocalBinding b = RegisterLocal(p, paramState == ParamParseState.Rest ? ISEQ : TagOf(p), null); // asdf-tag
                        //LocalBinding b = RegisterLocal(p, TagOf(p), null);

                        argLocals = argLocals.cons(b);
                        switch (paramState)
                        {
                            case ParamParseState.Required:
                                method.ReqParms = method.ReqParms.cons(b);
                                break;
                            case ParamParseState.Rest:
                                method.RestParm = b;
                                paramState = ParamParseState.Done;
                                break;
                            default:
                                throw new Exception("Unexpected parameter");
                        }
                    }
                }

                if (method.NumParams > MAX_POSITIONAL_ARITY)
                    throw new Exception(string.Format("Can't specify more than {0} parameters",MAX_POSITIONAL_ARITY));
                LOOP_LOCALS.set(argLocals);
                method.ArgLocals = argLocals;

                List<ParameterExpression> parmExprs = new List<ParameterExpression>(argLocals.count());
                List<ParameterExpression> typedParmExprs = new List<ParameterExpression>();
                List<Expression> typedParmInitExprs = new List<Expression>();

                for (int i = 0; i < argLocals.count(); i++)
                {
                    LocalBinding b = (LocalBinding)argLocals.nth(i);

                    ParameterExpression pexpr = Expression.Parameter(typeof(object), b.Name);  //asdf-tag
                    b.ParamExpression = pexpr;
                    parmExprs.Add(pexpr);

                    if (b.Tag != null)
                    {
                        // we have a type hint
                        // The ParameterExpression above will be the parameter to the function.
                        // We need to generate another local parameter that is typed.  
                        // This will be the parameter tied to the LocalBinding so that the typing information is seen in the body.
                        Type t = TagToType(b.Tag);
                        ParameterExpression p2 = Expression.Parameter(t, b.Name);
                        b.ParamExpression = p2;
                        typedParmExprs.Add(p2);
                        typedParmInitExprs.Add(Expression.Assign(p2, Expression.Convert(pexpr, t)));
                    }
                }


                // TODO:  Eventually, type this param to ISeq.  
                // This will require some reworking with signatures in various places around here.
                //if (fn.IsVariadic)
                //    parmExprs.Add(Expression.Parameter(typeof(object), "____REST"));

                // If we have any typed parameters, we need to add an extra block to do the initialization.

                List<Expression> bodyExprs = new List<Expression>();
                bodyExprs.AddRange(typedParmInitExprs);
                bodyExprs.Add(Expression.Label(loopLabel));
                bodyExprs.Add(MaybeBox(GenerateBodyExpr(body)));

                Expression block;
                if ( typedParmExprs.Count > 0 )
                    block = Expression.Block(typedParmExprs,bodyExprs);
                else
                    block = Expression.Block(bodyExprs);

                method.Lambda = Expression.Lambda(
                    FuncTypeHelpers.GetFFuncType(parmExprs.Count),
                    block,
                    fn.Name,
                    parmExprs);

                //method.Lambda = Expression.Lambda(
                //    FuncTypeHelpers.GetFFuncType(parmExprs.Count),
                //    Expression.Block(Expression.Label(loopLabel), MaybeBox(GenerateBodyExpr(body))), 
                //    fn.Name, 
                //    parmExprs);

                return method;
            }
            finally
            {
                Var.popThreadBindings();
            }
        }

        private static Expression GenerateFnLambda(FnDef fn, SortedDictionary<int, MethodDef> methods, MethodDef variadicMethod)
        {
            Type fnType = fn.IsVariadic ? typeof(RestFnImpl) : typeof(AFnImpl);

            ParameterExpression p1 = fn.ThisParam ?? Expression.Parameter(fnType, "____x");
            List<Expression> exprs = new List<Expression>();

            if (fn.IsVariadic)
                exprs.Add(Expression.Assign(p1, Expression.New(Ctor_RestFnImpl_1, Expression.Constant(variadicMethod.RequiredArity))));
            else
                exprs.Add(Expression.Assign(p1, Expression.New(Ctor_AFnImpl_0)));

            foreach (KeyValuePair<int, MethodDef> pair in methods)
            {
                int arity = pair.Key;
                LambdaExpression lambda = pair.Value.Lambda;
                exprs.Add(Expression.Assign(Expression.Field(p1, "_fn" + arity), lambda));
            }

            if (fn.IsVariadic)
                exprs.Add(Expression.Assign(Expression.Field(p1, "_fnDo" + variadicMethod.RequiredArity), variadicMethod.Lambda));

           exprs.Add(p1);

           Expression expr = Expression.Block(new ParameterExpression[] { p1 }, exprs);
           return expr;
        }



        // There is a tremendous overlap between this and GenerateFnExpr+GenerateFnMethod.  TODO: DRY it.
        private static LambdaExpression GenerateTypedDelegateExpression(Type delegateType, Symbol name, IPersistentVector parms, ISeq body)
        {
             // Create the form that is more or less correct

            ISeq form = (name == null)
                ? RT.cons(Compiler.FN, RT.cons(parms, body))
                : RT.cons(Compiler.FN, RT.cons(name, RT.cons(parms, body)));

            FnDef fnDef = new FnDef();
            fnDef.ComputeNames(form);

            MethodDef methodDef = new MethodDef(fnDef, (MethodDef)METHODS.deref());

            try
            {
                LabelTarget loopLabel = Expression.Label();

                Var.pushThreadBindings(PersistentHashMap.create(
                    METHODS, methodDef,
                    LOOP_LABEL, loopLabel,
                    LOCAL_ENV, LOCAL_ENV.deref(),
                    LOOP_LOCALS, null));
                
                // register 'this' as local 0  
                LocalBinding thisB = RegisterLocal(Symbol.intern(fnDef.ThisName ?? "fn__" + RT.nextID()), null, null);
                thisB.ParamExpression = fnDef.ThisParam;
                
                IPersistentVector argLocals = PersistentVector.EMPTY;
                int parmsCount = parms.count();
                ParamParseState paramState = ParamParseState.Required;

                for (int i = 0; i < parmsCount; i++)
                {
                    if (!(parms.nth(i) is Symbol))
                        throw new ArgumentException("fn params must be Symbols");
                    Symbol p = parms.nth(i) as Symbol;
                    if (p.Namespace != null)
                        throw new Exception("Can't use qualified name as parameter: " + p);
                    if (p.Equals(Compiler._AMP_))
                    {
                        if (paramState == ParamParseState.Required)
                            paramState = ParamParseState.Rest;
                        else
                            throw new Exception("Invalid parameter list");
                    }
                    else
                    {
                        // TODO: Need more type inferencing to make this work.
                        //LocalBinding b = RegisterLocal(p, paramState == ParamParseState.Rest ? ISEQ : TagOf(p), null);
                        LocalBinding b = RegisterLocal(p, TagOf(p), null);

                        argLocals = argLocals.cons(b);
                        switch (paramState)
                        {
                            case ParamParseState.Required:
                                methodDef.ReqParms = methodDef.ReqParms.cons(b);
                                break;
                            case ParamParseState.Rest:
                                methodDef.RestParm = b;
                                paramState = ParamParseState.Done;
                                break;
                            default:
                                throw new Exception("Unexpected parameter");
                        }
                    }
                }

                MethodInfo invokeMI = delegateType.GetMethod("Invoke");
                Type returnType = invokeMI.ReturnType;
                ParameterInfo[] delParams = invokeMI.GetParameters();

                bool isVariadic = (invokeMI.CallingConvention & CallingConventions.VarArgs) != 0;
                if (isVariadic != methodDef.IsVariadic)
                    throw new ArgumentException("Arglist and delegate type must agree on being variadic.");

                if (delParams.Length != argLocals.count() )
                    throw new ArgumentException("Wrong number of parameters to generate typed delegate");


                if (methodDef.NumParams > MAX_POSITIONAL_ARITY)
                    throw new Exception(string.Format("Can't specify more than {0} parameters",MAX_POSITIONAL_ARITY));

                LOOP_LOCALS.set(argLocals);
                methodDef.ArgLocals = argLocals;
                
                List<ParameterExpression> parmExprs = new List<ParameterExpression>(argLocals.count());
                for (int i = 0; i < argLocals.count(); i++)
                {
                    LocalBinding b = (LocalBinding)argLocals.nth(i);
                    ParameterExpression pexpr = Expression.Parameter(delParams[i].ParameterType, b.Name);  //asdf-tag
                    b.ParamExpression = pexpr;
                    parmExprs.Add(pexpr);
                }


                methodDef.Lambda = Expression.Lambda(
                    delegateType,
                    Expression.Block(
                      Expression.Label(loopLabel), 
                      Expression.Convert(GenerateBodyExpr(body),returnType)),
                      fnDef.Name,
                      parmExprs);

                return methodDef.Lambda;

            }
            finally
            {
                Var.popThreadBindings();
            }            

       }

        //private Expression GenerateFixedArgMethodCall(MethodDef method, ParameterExpression restParam, out Type methodType)
        //{
        //    LambdaExpression lambda = method.Lambda;
        //    InvocationExpression ret;

        //    switch (method.RequiredArity)
        //    {
        //        case 0:
        //            ret = Expression.Invoke(lambda);
        //            methodType = typeof(Microsoft.Func<object>);
        //            break;
        //        case 1:
        //            ret = Expression.Invoke(lambda,
        //                GetParamArrayItem(restParam, 0));
        //            methodType = typeof(Microsoft.Func<object, object>);
        //            break;
        //        case 2:
        //            ret = Expression.Invoke(lambda,
        //                GetParamArrayItem(restParam, 0), GetParamArrayItem(restParam, 1));
        //            methodType = typeof(Microsoft.Func<object, object, object>);
        //            break;
        //        case 3:
        //            ret = Expression.Invoke(lambda,
        //                GetParamArrayItem(restParam, 0), GetParamArrayItem(restParam, 1), GetParamArrayItem(restParam, 2));
        //            methodType = typeof(Microsoft.Func<object, object, object, object>);
        //            break;
        //        case 4:
        //            ret = Expression.Invoke(lambda,
        //                GetParamArrayItem(restParam, 0), GetParamArrayItem(restParam, 1), GetParamArrayItem(restParam, 2), GetParamArrayItem(restParam, 3));
        //            methodType = typeof(Microsoft.Func<object, object, object, object, object>);
        //            break;
        //        case 5:
        //            ret = Expression.Invoke(lambda,
        //                GetParamArrayItem(restParam, 0), GetParamArrayItem(restParam, 1), GetParamArrayItem(restParam, 2), GetParamArrayItem(restParam, 3),
        //                GetParamArrayItem(restParam, 4));
        //            methodType = typeof(Microsoft.Func<object, object, object, object, object, object>);
        //            break;
        //        case 6:
        //            ret = Expression.Invoke(lambda,
        //                GetParamArrayItem(restParam, 0), GetParamArrayItem(restParam, 1), GetParamArrayItem(restParam, 2), GetParamArrayItem(restParam, 3),
        //                GetParamArrayItem(restParam, 4), GetParamArrayItem(restParam, 5));
        //            methodType = typeof(Microsoft.Func<object, object, object, object, object, object, object>);
        //            break;
        //        case 7:
        //            ret = Expression.Invoke(lambda,
        //                GetParamArrayItem(restParam, 0), GetParamArrayItem(restParam, 1), GetParamArrayItem(restParam, 2), GetParamArrayItem(restParam, 3),
        //                GetParamArrayItem(restParam, 4), GetParamArrayItem(restParam, 5), GetParamArrayItem(restParam, 6));
        //            methodType = typeof(Microsoft.Func<object, object, object, object, object, object, object, object>);
        //            break;
        //        case 8:
        //            ret = Expression.Invoke(lambda,
        //                GetParamArrayItem(restParam, 0), GetParamArrayItem(restParam, 1), GetParamArrayItem(restParam, 2), GetParamArrayItem(restParam, 3),
        //                GetParamArrayItem(restParam, 4), GetParamArrayItem(restParam, 5), GetParamArrayItem(restParam, 6), GetParamArrayItem(restParam, 7));
        //            methodType = typeof(Microsoft.Func<object, object, object, object, object, object, object, object, object>);
        //            break;
        //        case 9:
        //            ret = Expression.Invoke(lambda,
        //                GetParamArrayItem(restParam, 0), GetParamArrayItem(restParam, 1), GetParamArrayItem(restParam, 2), GetParamArrayItem(restParam, 3),
        //                GetParamArrayItem(restParam, 4), GetParamArrayItem(restParam, 5), GetParamArrayItem(restParam, 6), GetParamArrayItem(restParam, 7),
        //                GetParamArrayItem(restParam, 8));
        //            methodType = typeof(Microsoft.Func<object, object, object, object, object, object, object, object, object, object>);
        //            break;
        //        case 10:
        //            ret = Expression.Invoke(lambda,
        //                GetParamArrayItem(restParam, 0), GetParamArrayItem(restParam, 1), GetParamArrayItem(restParam, 2), GetParamArrayItem(restParam, 3),
        //                GetParamArrayItem(restParam, 4), GetParamArrayItem(restParam, 5), GetParamArrayItem(restParam, 6), GetParamArrayItem(restParam, 7),
        //                GetParamArrayItem(restParam, 8), GetParamArrayItem(restParam, 9));
        //            methodType = typeof(Microsoft.Func<object, object, object, object, object, object, object, object, object, object, object>);
        //            break;
        //        case 11:
        //            ret = Expression.Invoke(lambda,
        //                GetParamArrayItem(restParam, 0), GetParamArrayItem(restParam, 1), GetParamArrayItem(restParam, 2), GetParamArrayItem(restParam, 3),
        //                GetParamArrayItem(restParam, 4), GetParamArrayItem(restParam, 5), GetParamArrayItem(restParam, 6), GetParamArrayItem(restParam, 7),
        //                GetParamArrayItem(restParam, 8), GetParamArrayItem(restParam, 9), GetParamArrayItem(restParam, 10));
        //            methodType = typeof(Microsoft.Func<object, object, object, object, object, object, object, object, object, object, object, object>);
        //            break;
        //        case 12:
        //            ret = Expression.Invoke(lambda,
        //                GetParamArrayItem(restParam, 0), GetParamArrayItem(restParam, 1), GetParamArrayItem(restParam, 2), GetParamArrayItem(restParam, 3),
        //                GetParamArrayItem(restParam, 4), GetParamArrayItem(restParam, 5), GetParamArrayItem(restParam, 6), GetParamArrayItem(restParam, 7),
        //                GetParamArrayItem(restParam, 8), GetParamArrayItem(restParam, 9), GetParamArrayItem(restParam, 10), GetParamArrayItem(restParam, 11));
        //            methodType = typeof(Microsoft.Func<object, object, object, object, object, object, object, object, object, object, object, object, object>);
        //            break;
        //        case 13:
        //            ret = Expression.Invoke(lambda,
        //                GetParamArrayItem(restParam, 0), GetParamArrayItem(restParam, 1), GetParamArrayItem(restParam, 2), GetParamArrayItem(restParam, 3),
        //                GetParamArrayItem(restParam, 4), GetParamArrayItem(restParam, 5), GetParamArrayItem(restParam, 6), GetParamArrayItem(restParam, 7),
        //                GetParamArrayItem(restParam, 8), GetParamArrayItem(restParam, 9), GetParamArrayItem(restParam, 10), GetParamArrayItem(restParam, 11),
        //                GetParamArrayItem(restParam, 12));
        //            methodType = typeof(Microsoft.Func<object, object, object, object, object, object, object, object, object, object, object, object, object, object>);
        //            break;
        //        case 14:
        //            ret = Expression.Invoke(lambda,
        //                GetParamArrayItem(restParam, 0), GetParamArrayItem(restParam, 1), GetParamArrayItem(restParam, 2), GetParamArrayItem(restParam, 3),
        //                GetParamArrayItem(restParam, 4), GetParamArrayItem(restParam, 5), GetParamArrayItem(restParam, 6), GetParamArrayItem(restParam, 7),
        //                GetParamArrayItem(restParam, 8), GetParamArrayItem(restParam, 9), GetParamArrayItem(restParam, 10), GetParamArrayItem(restParam, 11),
        //                GetParamArrayItem(restParam, 12), GetParamArrayItem(restParam, 13));
        //            methodType = typeof(Microsoft.Func<object, object, object, object, object, object, object, object, object, object, object, object, object, object, object>);
        //            break;
        //        case 15:
        //            ret = Expression.Invoke(lambda,
        //                GetParamArrayItem(restParam, 0), GetParamArrayItem(restParam, 1), GetParamArrayItem(restParam, 2), GetParamArrayItem(restParam, 3),
        //                GetParamArrayItem(restParam, 4), GetParamArrayItem(restParam, 5), GetParamArrayItem(restParam, 6), GetParamArrayItem(restParam, 7),
        //                GetParamArrayItem(restParam, 8), GetParamArrayItem(restParam, 9), GetParamArrayItem(restParam, 10), GetParamArrayItem(restParam, 11),
        //                GetParamArrayItem(restParam, 12), GetParamArrayItem(restParam, 13), GetParamArrayItem(restParam, 14));
        //            methodType = typeof(Microsoft.Func<object, object, object, object, object, object, object, object, object, object, object, object, object, object, object, object>);
        //            break;
        //        case 16:
        //            ret = Expression.Invoke(lambda,
        //                GetParamArrayItem(restParam, 0), GetParamArrayItem(restParam, 1), GetParamArrayItem(restParam, 2), GetParamArrayItem(restParam, 3),
        //                GetParamArrayItem(restParam, 4), GetParamArrayItem(restParam, 5), GetParamArrayItem(restParam, 6), GetParamArrayItem(restParam, 7),
        //                GetParamArrayItem(restParam, 8), GetParamArrayItem(restParam, 9), GetParamArrayItem(restParam, 10), GetParamArrayItem(restParam, 11),
        //                GetParamArrayItem(restParam, 12), GetParamArrayItem(restParam, 13), GetParamArrayItem(restParam, 14), GetParamArrayItem(restParam, 15));
        //            methodType = typeof(Microsoft.Func<object, object, object, object, object, object, object, object, object, object, object, object, object, object, object, object>);
        //            break;


        //        default:
        //            throw new Exception("We should never have been able to get here: 20 arguments?");
        //    }
        //    return ret;
        //}


        //private Expression GenerateVariadicMethodCall(MethodDef variM , ParameterExpression restParam, out Type methodType)
        //{
        //    LambdaExpression lambda = variM.Lambda;
        //    InvocationExpression ret;

        //    switch (variM.RequiredArity)
        //    {
        //        case 0:
        //            ret = Expression.Invoke(lambda,
        //                ConvertParamArrayToISeq(restParam, 0));
        //            methodType = typeof(Microsoft.Func<ISeq, object>);
        //            break;
        //        case 1:
        //            ret = Expression.Invoke(lambda,
        //                GetParamArrayItem(restParam, 0),
        //                ConvertParamArrayToISeq(restParam, 1));
        //            methodType = typeof(Microsoft.Func<object, ISeq, object>);
        //            break;
        //        case 2:
        //            ret = Expression.Invoke(lambda,
        //                GetParamArrayItem(restParam, 0), GetParamArrayItem(restParam, 1),
        //                ConvertParamArrayToISeq(restParam, 2));
        //            methodType = typeof(Microsoft.Func< object, object, ISeq, object>);
        //            break;
        //        case 3:
        //            ret = Expression.Invoke(lambda,
        //                GetParamArrayItem(restParam, 0), GetParamArrayItem(restParam, 1), GetParamArrayItem(restParam, 2),
        //                ConvertParamArrayToISeq(restParam, 3));
        //            methodType = typeof(Microsoft.Func<object, object, object, ISeq, object>);
        //            break;
        //        case 4:
        //            ret = Expression.Invoke(lambda,
        //                GetParamArrayItem(restParam, 0), GetParamArrayItem(restParam, 1), GetParamArrayItem(restParam, 2), GetParamArrayItem(restParam, 3),
        //                ConvertParamArrayToISeq(restParam, 4));
        //            methodType = typeof(Microsoft.Func<object, object, object, object, ISeq, object>);
        //            break;
        //        case 5:
        //            ret = Expression.Invoke(lambda,
        //                GetParamArrayItem(restParam, 0), GetParamArrayItem(restParam, 1), GetParamArrayItem(restParam, 2), GetParamArrayItem(restParam, 3),
        //                GetParamArrayItem(restParam, 4),
        //                ConvertParamArrayToISeq(restParam, 5));
        //            methodType = typeof(Microsoft.Func<object, object, object, object, object, ISeq, object>);
        //            break;
        //        case 6:
        //            ret = Expression.Invoke(lambda,
        //                GetParamArrayItem(restParam, 0), GetParamArrayItem(restParam, 1), GetParamArrayItem(restParam, 2), GetParamArrayItem(restParam, 3),
        //                GetParamArrayItem(restParam, 4), GetParamArrayItem(restParam, 5),
        //                ConvertParamArrayToISeq(restParam, 6));
        //            methodType = typeof(Microsoft.Func<object, object, object, object, object, object, ISeq, object>);
        //            break;
        //        case 7:
        //            ret = Expression.Invoke(lambda,
        //                GetParamArrayItem(restParam, 0), GetParamArrayItem(restParam, 1), GetParamArrayItem(restParam, 2), GetParamArrayItem(restParam, 3),
        //                GetParamArrayItem(restParam, 4), GetParamArrayItem(restParam, 5), GetParamArrayItem(restParam, 6),
        //                ConvertParamArrayToISeq(restParam, 7));
        //            methodType = typeof(Microsoft.Func<object, object, object, object, object, object, object, ISeq, object>);
        //            break;
        //        case 8:
        //            ret = Expression.Invoke(lambda,
        //                GetParamArrayItem(restParam, 0), GetParamArrayItem(restParam, 1), GetParamArrayItem(restParam, 2), GetParamArrayItem(restParam, 3),
        //                GetParamArrayItem(restParam, 4), GetParamArrayItem(restParam, 5), GetParamArrayItem(restParam, 6), GetParamArrayItem(restParam, 7),
        //                ConvertParamArrayToISeq(restParam, 8));
        //            methodType = typeof(Microsoft.Func<object, object, object, object, object, object, object, object, ISeq, object>);
        //            break;
        //        case 9:
        //            ret = Expression.Invoke(lambda,
        //                GetParamArrayItem(restParam, 0), GetParamArrayItem(restParam, 1), GetParamArrayItem(restParam, 2), GetParamArrayItem(restParam, 3),
        //                GetParamArrayItem(restParam, 4), GetParamArrayItem(restParam, 5), GetParamArrayItem(restParam, 6), GetParamArrayItem(restParam, 7),
        //                GetParamArrayItem(restParam, 8),
        //                ConvertParamArrayToISeq(restParam, 9));
        //            methodType = typeof(Microsoft.Func<object, object, object, object, object, object, object, object, object, ISeq, object>);
        //            break;
        //        case 10:
        //            ret = Expression.Invoke(lambda,
        //                GetParamArrayItem(restParam, 0), GetParamArrayItem(restParam, 1), GetParamArrayItem(restParam, 2), GetParamArrayItem(restParam, 3),
        //                GetParamArrayItem(restParam, 4), GetParamArrayItem(restParam, 5), GetParamArrayItem(restParam, 6), GetParamArrayItem(restParam, 7),
        //                GetParamArrayItem(restParam, 8), GetParamArrayItem(restParam, 9),
        //                ConvertParamArrayToISeq(restParam, 10));
        //            methodType = typeof(Microsoft.Func<object, object, object, object, object, object, object, object, object, object, ISeq, object>);
        //            break;
        //        case 11:
        //            ret = Expression.Invoke(lambda,
        //                GetParamArrayItem(restParam, 0), GetParamArrayItem(restParam, 1), GetParamArrayItem(restParam, 2), GetParamArrayItem(restParam, 3),
        //                GetParamArrayItem(restParam, 4), GetParamArrayItem(restParam, 5), GetParamArrayItem(restParam, 6), GetParamArrayItem(restParam, 7),
        //                GetParamArrayItem(restParam, 8), GetParamArrayItem(restParam, 9), GetParamArrayItem(restParam, 10),
        //                ConvertParamArrayToISeq(restParam, 11));
        //            methodType = typeof(Microsoft.Func<object, object, object, object, object, object, object, object, object, object, object, ISeq, object>);
        //            break;
        //        case 12:
        //            ret = Expression.Invoke(lambda,
        //                GetParamArrayItem(restParam, 0), GetParamArrayItem(restParam, 1), GetParamArrayItem(restParam, 2), GetParamArrayItem(restParam, 3),
        //                GetParamArrayItem(restParam, 4), GetParamArrayItem(restParam, 5), GetParamArrayItem(restParam, 6), GetParamArrayItem(restParam, 7),
        //                GetParamArrayItem(restParam, 8), GetParamArrayItem(restParam, 9), GetParamArrayItem(restParam, 10), GetParamArrayItem(restParam, 11),
        //                ConvertParamArrayToISeq(restParam, 12));
        //            methodType = typeof(Microsoft.Func<object, object, object, object, object, object, object, object, object, object, object, object, ISeq, object>);
        //            break;
        //        case 13:
        //            ret = Expression.Invoke(lambda,
        //                GetParamArrayItem(restParam, 0), GetParamArrayItem(restParam, 1), GetParamArrayItem(restParam, 2), GetParamArrayItem(restParam, 3),
        //                GetParamArrayItem(restParam, 4), GetParamArrayItem(restParam, 5), GetParamArrayItem(restParam, 6), GetParamArrayItem(restParam, 7),
        //                GetParamArrayItem(restParam, 8), GetParamArrayItem(restParam, 9), GetParamArrayItem(restParam, 10), GetParamArrayItem(restParam, 11),
        //                GetParamArrayItem(restParam, 12),
        //                ConvertParamArrayToISeq(restParam, 13));
        //            methodType = typeof(Microsoft.Func<object, object, object, object, object, object, object, object, object, object, object, object, object, ISeq, object>);
        //            break;
        //        case 14:
        //            ret = Expression.Invoke(lambda,
        //                GetParamArrayItem(restParam, 0), GetParamArrayItem(restParam, 1), GetParamArrayItem(restParam, 2), GetParamArrayItem(restParam, 3),
        //                GetParamArrayItem(restParam, 4), GetParamArrayItem(restParam, 5), GetParamArrayItem(restParam, 6), GetParamArrayItem(restParam, 7),
        //                GetParamArrayItem(restParam, 8), GetParamArrayItem(restParam, 9), GetParamArrayItem(restParam, 10), GetParamArrayItem(restParam, 11),
        //                GetParamArrayItem(restParam, 12), GetParamArrayItem(restParam, 13),
        //                ConvertParamArrayToISeq(restParam, 14));
        //            methodType = typeof(Microsoft.Func<object, object, object, object, object, object, object, object, object, object, object, object, object, object, ISeq, object>);
        //            break;
        //        case 15:
        //            ret = Expression.Invoke(lambda,
        //                GetParamArrayItem(restParam, 0), GetParamArrayItem(restParam, 1), GetParamArrayItem(restParam, 2), GetParamArrayItem(restParam, 3),
        //                GetParamArrayItem(restParam, 4), GetParamArrayItem(restParam, 5), GetParamArrayItem(restParam, 6), GetParamArrayItem(restParam, 7),
        //                GetParamArrayItem(restParam, 8), GetParamArrayItem(restParam, 9), GetParamArrayItem(restParam, 10), GetParamArrayItem(restParam, 11),
        //                GetParamArrayItem(restParam, 12), GetParamArrayItem(restParam, 13), GetParamArrayItem(restParam, 14),
        //                ConvertParamArrayToISeq(restParam, 15));
        //            methodType = typeof(Microsoft.Func<object, object, object, object, object, object, object, object, object, object, object, object, object, object, object, ISeq, object>);
        //            break;
        //       default:
        //            throw new Exception("We should never have been able to get here: 20 arguments?");
        //    }
        //    return ret;
        //}

        private static Expression GetParamArrayItem(Expression e, int i)
        {
            return Expression.ArrayIndex(e, Expression.Constant(i));
        }

        private static Expression ConvertParamArrayToISeq(Expression e, int i)
        {
            return Expression.Call(Method_ArraySeq_create_array_int, e, Expression.Constant(i));
        }

        #endregion

        #region fn-related special forms

        struct BindingInit
        {
            private readonly LocalBinding _binding;
            public LocalBinding Binding
            {
                get { return _binding; }
            }

            private readonly Expression _init;
            public Expression Init
            {
                get { return _init; }
            }

            public BindingInit(LocalBinding binding, Expression init)
            {
                _binding = binding;
                _init = init;
            }

        }

        public static readonly Var LOOP_LABEL = Var.create(null);

        private static Expression GenerateLetExpr(ISeq form)
        {
            // form => (let [var1 val1 var2 val2 ... ] body ... )
            //      or (loop [var1 val1 var2 val2 ... ] body ... )

            bool isLoop = form.first().Equals(Compiler.LOOP);
            IPersistentVector bindings = RT.second(form) as IPersistentVector;

            if (bindings == null)
                throw new ArgumentException("Bad binding form, expected vector");

            if ((bindings.count() % 2) != 0)
                throw new ArgumentException("Bad binding form, expected matched symbol/value pairs.");

            ISeq body = RT.rest(RT.rest(form));

            // TODO: This is one place where context makes a difference.  Need to figure this out.
            //  Second test clause added in Rev 1216.
            //if (ctxt == C.EVAL || (context == c.EXPRESSION && isLoop))
            //    return Generate(RT.list(RT.list(Compiler.FN, PersistentVector.EMPTY, form)));

            // As of Rev 1216, I tried it out. 
            // However, it goes into an infinite loop.  Still need to figure this out.
            //if (isLoop)
            //    Generate(RT.list(RT.list(Compiler.FN, PersistentVector.EMPTY, form)));

            IPersistentMap dynamicBindings = PersistentHashMap.create(LOCAL_ENV, LOCAL_ENV.deref());

            if (isLoop)
                dynamicBindings = dynamicBindings.assoc(LOOP_LOCALS, null);

            try
            {
                Var.pushThreadBindings(dynamicBindings);
                IPersistentVector bindingInits = PersistentVector.EMPTY;
                IPersistentVector loopLocals = PersistentVector.EMPTY;

                for ( int i=0; i<bindings.count(); i+=2 )
                {
                    if (!(bindings.nth(i) is Symbol))
                        throw new ArgumentException("Bad binding form, expected symbol, got " + bindings.nth(i));
                    Symbol sym = (Symbol) bindings.nth(i);
                    if ( sym.Namespace != null )
                        throw new Exception("Can't let qualified name: " + sym);
                    Expression init = Generate(/*C.EXPRESSION, */ bindings.nth(i+1) /* , sym.Name */);
                    // Sequential enhancement of env (like Lisp let*)
                    LocalBinding b = RegisterLocal(sym,TagOf(sym),init);
                    b.ParamExpression = Expression.Variable(typeof(object), b.Name);  //asdf-tag
                    bindingInits = bindingInits.cons(new BindingInit(b,init));

                    if ( isLoop )
                        loopLocals = loopLocals.cons(b);
                }
                if ( isLoop )
                    LOOP_LOCALS.set(loopLocals);

                LabelTarget loopLabel = Expression.Label();

                List<ParameterExpression> parms = new List<ParameterExpression>();
                List<Expression> forms = new List<Expression>();

                for ( int i=0; i<bindingInits.count(); i++ )
                {
                    BindingInit bi = (BindingInit) bindingInits.nth(i);
                    ParameterExpression parmExpr = (ParameterExpression) bi.Binding.ParamExpression;
                    parms.Add(parmExpr);
                    forms.Add(Expression.Assign(parmExpr,MaybeBox(bi.Init)));
                }
   
                
                forms.Add(Expression.Label(loopLabel));

                try 
                {
                    if ( isLoop )
                        Var.pushThreadBindings(PersistentHashMap.create(LOOP_LABEL,loopLabel));
                    
                    forms.Add(GenerateBodyExpr(  /* isLoop ? C.RETURN : context , */ body)); 
                }
                finally 
                {
                    if ( isLoop )
                        Var.popThreadBindings();
                }

                Expression block = Expression.Block(parms,forms);             
                return block;
            }
            finally
            {
                Var.popThreadBindings();
            }
        }


        //null or not
        public static readonly Var IN_CATCH_FINALLY = Var.create(null);
        public static readonly Var IN_TAIL_POSITION = Var.create(null);

        // Don't do what I did the first time:  Evaluate the forms/assignments sequentially.
        // Need to evaluate all the forms, then assign them.

        private static Expression GenerateRecurExpr(ISeq form)
        {
            IPersistentVector loopLocals = (IPersistentVector) LOOP_LOCALS.deref();
            if ( IN_TAIL_POSITION.deref() == null || loopLocals == null )
                throw new InvalidOperationException("Can only recur from tail position");
            if (IN_CATCH_FINALLY.deref() != null)
                throw new InvalidOperationException("Cannot recur from catch/finally.");
            IPersistentVector args = PersistentVector.EMPTY;
            for ( ISeq s = form.rest(); s != null; s = s.rest() )
                args = args.cons(Generate(s.first()));
            if ( args.count() != loopLocals.count())
                throw new ArgumentException(string.Format("Mismatched argument count to recur, expected: {0} args, got {1}",loopLocals.count(),args.count()));

            LabelTarget loopLabel = (LabelTarget)LOOP_LABEL.deref();
            if (loopLabel == null)
                throw new InvalidOperationException("Recur not in proper context.");

            int argCount = args.count();

            List<ParameterExpression> tempVars = new List<ParameterExpression>(argCount);
            List<Expression> tempAssigns = new List<Expression>(2 * argCount+1);
            List<Expression> finalAssigns = new List<Expression>(argCount);

            // Evaluate all the init forms into local variables.
            for ( int i=0; i<loopLocals.count(); i++ )
            {   
                LocalBinding b = (LocalBinding)loopLocals.nth(i);
                ParameterExpression tempVar = Expression.Parameter(b.ParamExpression.Type, "local" + i);  //asdf-tag
                Expression arg = (Expression) args.nth(i);
                tempVars.Add(tempVar);

                if ( tempVar.Type == typeof(Object) )
                    tempAssigns.Add(Expression.Assign(tempVar,MaybeBox(arg)));
                else
                    tempAssigns.Add(Expression.Assign(tempVar, Expression.Convert(arg, tempVar.Type)));                     //asdf-tag

                finalAssigns.Add(Expression.Assign(b.ParamExpression, tempVar));  //asdf-tag
            }

            List<Expression> exprs = tempAssigns;
            exprs.AddRange(finalAssigns);
            exprs.Add(Expression.Goto(loopLabel));
            // need to do this to get a return value in the type inferencing -- else can't use this in a then or else clause.
            exprs.Add(Expression.Constant(null));
            return Expression.Block(tempVars,exprs);
        }

        #endregion

        #region Assign

        private static Expression GenerateAssignExpr(ISeq form)
        {
            if (form.count() != 3)
                throw new ArgumentException("Malformed assignment, expecting (set! target val)");

            object target = RT.second(form);
            object init = RT.third(form);

            Var v;


            if ( (v = FindAsVar(target)) != null)
                return GenerateVarAssignExpr(v, init);

            Type t;

            if ((t = FindAsDirectStaticFieldReference(target)) != null)
                return GenerateDirectStaticFieldAssignExpr(t, (target as Symbol).Name, init);
            
            if ( IsFieldReference(target))
                return GenerateFieldAssignExpr(RT.second((ISeq)target),(string) RT.third((ISeq)target),init);

            throw new ArgumentException("Invalid assignment target");

        }

        private static Var FindAsVar(object target)
        {
            Symbol sym = target as Symbol;
            if (sym == null)
                return null;

            if (sym.Namespace == null && ReferenceLocal(sym) != null)
                return null;

            // There is case in GenerateSymbolExpr that deals with a symbol representing a static field reference.
            // Should we allow that here?

            object o = Compiler.Resolve(sym);
            return o as Var;
        }

        private static Type FindAsDirectStaticFieldReference(object target)
        {
            Symbol sym = target as Symbol;
            if (sym == null)
                return null;

            if (sym.Namespace == null && ReferenceLocal(sym) != null)
                return null;

            if (Compiler.namespaceFor(sym) == null)
            {
                Symbol nsSym = Symbol.create(sym.Namespace);
                Type t = MaybeType(nsSym, false);
                if (t != null)
                    return t;
            }
            return null;
        }

        private static bool IsFieldReference(object target)
        {
            ISeq form = target as ISeq;
            if (form == null)
                return false;

            if (form.count() != 3)
                return false;

            if (!Compiler.DOT.Equals(form.first()))
                return false;

            if (!(RT.third(form) is string))
                return false;

            return true;
        }

        private static Expression GenerateVarAssignExpr(Var v, object init)
        {
            Expression initExpr = Generate(init);

            return Expression.Call(Expression.Constant(v), Method_Var_set, MaybeBox(initExpr));
        }


        private static Expression GenerateDirectStaticFieldAssignExpr(Type t, string fieldName, object init)
        {
            Expression initExpr = Generate(init);

            FieldInfo f = t.GetField(fieldName, BindingFlags.Static | BindingFlags.Public | BindingFlags.SetField);
            if (f != null)
                return Expression.Assign(Expression.Field(null, f), initExpr);

            PropertyInfo p = t.GetProperty(fieldName, BindingFlags.Static | BindingFlags.Public | BindingFlags.SetField);
            if (p != null)
                return Expression.Assign(Expression.Property(null, p), initExpr);

            throw new ArgumentException(string.Format("No field/property named: {0} for type: {1}", fieldName, t.Name));
        }

        private static Expression GenerateFieldAssignExpr(object classOrInstance, string fieldName, object init)
        {
            Type t = MaybeType(classOrInstance, false);
            if (t != null)
                return GenerateDirectStaticFieldAssignExpr(t, fieldName, init);

            // we are an instance 
            Expression instance = Generate(classOrInstance);
            Expression initExpr = Generate(init);

            // I doubt that this will work.  We will have to do a runtime determination
            FieldInfo f = instance.Type.GetField(fieldName, BindingFlags.Public);
            if (f != null)
                return Expression.Field(instance, f);

            PropertyInfo p = instance.Type.GetProperty(fieldName, BindingFlags.Static | BindingFlags.Public);
            if (p != null)
                return Expression.Property(instance, p);

            throw new ArgumentException(string.Format("No field/property named: {0} ", fieldName));
        }

        #endregion

        #region .NET-interop special forms

        private static Expression GenerateHostExpr(ISeq form)
        {
            // form is one of:
            //  (. x fieldname-sym)
            //  (. x 0-ary-method)
            //  (. x propertyname-sym)
            //  (. x methodname-sym args+)
            //  (. x (methodname-sym args?))
            if (form.count() < 3)
                throw new ArgumentException("Malformed member expression, expecting (. target member ... )");
            // determine static or instance
            // static target must be symbol, either fully.qualified.Typename or Typename that has been imported
            Type t = MaybeType(RT.second(form),false);
            // at this point, t will be non-null if static
            Expression instance = null;
            if (t == null)
                instance = Generate(RT.second(form));

            if (  form.count() == 3 && RT.third(form) is Symbol )
            {
                Symbol sym = (Symbol) RT.third(form);
                if ( t != null ) 
                {
                    FieldInfo f = t.GetField(sym.Name, BindingFlags.Static | BindingFlags.Public);
                    if (f != null)
                        return Expression.Field(null, f);

                    PropertyInfo p = t.GetProperty(sym.Name, BindingFlags.Static | BindingFlags.Public);
                    if (p != null)
                        return Expression.Property(null, p);
                }
                else    
                {
                    // I doubt that this will work.  We will have to do a runtime determination
                    FieldInfo f = instance.Type.GetField(sym.Name, BindingFlags.Instance | BindingFlags.Public);
                    if (f != null)
                        return Expression.Field(instance, f);

                    PropertyInfo p = instance.Type.GetProperty(sym.Name, BindingFlags.Instance | BindingFlags.Public);
                    if (p != null)
                        return Expression.Property(instance, p);
                }
            }

            ISeq call = RT.third(form) is ISeq ? (ISeq)RT.third(form) : RT.rest(RT.rest(form));

            if (!(RT.first(call) is Symbol))
                throw new ArgumentException("Malformed member exception");

            string methodName = ((Symbol)RT.first(call)).Name;
            int numArgs = call.count() - 1;
            
            Expression[] args = new Expression[numArgs];
            int i = 0;
            for (ISeq s = call.rest(); s != null; s = s.rest(), i++)
                args[i] = Generate(s.first());

            BindingFlags flags = BindingFlags.Public | BindingFlags.FlattenHierarchy | BindingFlags.InvokeMethod;

            if (t != null)
                flags |= BindingFlags.Static;
            else
                flags |= BindingFlags.Instance;

            Type targetType = t ?? instance.Type;
            
            //DEBUG:
            //IEnumerable<MethodInfo> einfo1 = targetType.GetMethods();
            //List<MethodInfo> infos1 = new List<MethodInfo>(einfo1);

            IEnumerable<MethodInfo> einfo = targetType.GetMethods(flags).Where(info => info.Name == methodName && info.GetParameters().Length == args.Length);
            List<MethodInfo> infos = new List<MethodInfo>(einfo);

            if (t != null && infos.Count == 0)
                throw new InvalidOperationException(string.Format("No method named: {0} in type: {1}", methodName, targetType.Name));
            else if (infos.Count == 1)
            {
                // TODO: if t is not null, but instance isn't typed, we may be missing overloads.  So I added a t != null.
                // We can improve this when we add better type info propagation.

                // we have a unique match, generate call directly
                if (t != null)
                    return AstUtils.SimpleCallHelper(infos[0], args);
                else
                    //return Expression.Call(instance, infos[0], args);  //asdf-tag
                    return AstUtils.SimpleCallHelper(instance,infos[0], args);
            }
            else
            {
                if (RT.booleanCast(RT.WARN_ON_REFLECTION.deref()))
                {
                    // TODO: use DLR IO
                    ((TextWriter)RT.ERR.deref()).WriteLine(string.Format("Reflection warning, line: {0} - call to {1} can't be resolved.\n", /* line ,*/0, methodName));
                }

                Expression[] moreArgs = new Expression[3];
                moreArgs[0] = Expression.Constant(methodName);
                moreArgs[1] = t != null ? Expression.Constant(t) : instance;
                moreArgs[2] = Expression.NewArrayInit(typeof(object), MaybeBox(args));

                if (t != null)
                    return Expression.Call(Method_Reflector_CallStaticMethod, moreArgs);
                else
                    return Expression.Call(Method_Reflector_CallInstanceMethod, moreArgs);
            }
        }



        private static Expression GenerateNewExpr(ISeq form)
        {
            // form => (new Classname args ... )
            if (form.count() < 2)
                throw new Exception("wrong number of arguments, expecting: (new Classname args ...)");
            Type t = MaybeType(RT.second(form), false);
            if (t == null)
                throw new ArgumentException("Unable to resolve classname: " + RT.second(form));

            int numArgs = form.count() - 2;
            Expression[] args = new Expression[numArgs];
            int i = 0;
            for (ISeq s = RT.rest(RT.rest(form)); s != null; s = s.rest(), i++)
                args[i] = Generate(s.first());

            List<ConstructorInfo> cinfos = new List<ConstructorInfo>(t.GetConstructors().Where(x => x.GetParameters().Length == numArgs && x.IsPublic));

            if (cinfos.Count == 0)
                throw new InvalidOperationException(string.Format("No constructor in type: {0} with {1} arguments", t.Name, numArgs));

            else if (cinfos.Count == 1)
            {
                // we have a unique match, generate directly
                // Need to try to convert the arguments, or the call to Expression.New will choke
                ConstructorInfo info = cinfos[0];
                Expression[] convArgs = new Expression[numArgs];
                for ( i=0; i < numArgs; i++ )
                    convArgs[i] = Expression.Convert(args[i],info.GetParameters()[i].ParameterType);
                return Expression.New(info, convArgs);
            }
            else
            {
                // we must defer to runtime

                if (RT.booleanCast(RT.WARN_ON_REFLECTION.deref()))
                {
                    // TODO: use DLR IO
                    ((TextWriter)RT.ERR.deref()).WriteLine(string.Format("Reflection warning, line: {0} - call to new can't be resolved.\n", /* line ,*/0));
                }

                Expression[] moreArgs = new Expression[2];
                moreArgs[0] = Expression.Constant(t);
                moreArgs[1] = Expression.NewArrayInit(typeof(object), MaybeBox(args));

                return Expression.Call(Method_Reflector_InvokeConstructor, moreArgs);
            }
        }


        #endregion


        static Type TagToType(object tag)
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
            if (t != null)
                return t;

            throw new ArgumentException("Unable to resolve classname: " + tag);
        }    
                            
    }
}