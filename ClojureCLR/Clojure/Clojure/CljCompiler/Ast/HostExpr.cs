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
using System.Reflection;
using Microsoft.Linq.Expressions;
using clojure.runtime;
using System.IO;

namespace clojure.lang.CljCompiler.Ast
{
    abstract class HostExpr : Expr, MaybePrimitiveExpr
    {
        #region Parsing
        
        public sealed class Parser : IParser
        {
            public Expr Parse(object frm)
            {
                ISeq form = (ISeq)frm;

                // form is one of:
                //  (. x fieldname-sym)
                //  (. x 0-ary-method)
                //  (. x propertyname-sym)
                //  (. x methodname-sym args+)
                //  (. x (methodname-sym args?))

                if (RT.Length(form) < 3)
                    throw new ArgumentException("Malformed member expression, expecting (. target member ... )");

                // determine static or instance
                // static target must be symbol, either fully.qualified.Typename or Typename that has been imported

                Type t = Compiler.MaybeType(RT.second(form), false);
                // at this point, t will be non-null if static

                Expr instance = null;
                if (t == null)
                    instance = Compiler.GenerateAST(RT.second(form));

                bool isFieldOrProperty = false;

                if (RT.Length(form) == 3 && RT.third(form) is Symbol)
                {
                    Symbol sym = (Symbol)RT.third(form);
                    if (t != null)
                        isFieldOrProperty =
                            t.GetField(sym.Name, BindingFlags.Static | BindingFlags.Public) != null
                            || t.GetProperty(sym.Name, BindingFlags.Static | BindingFlags.Public) != null;
                    else if (instance != null && instance.HasClrType && instance.ClrType != null)
                    {
                        Type instanceType = instance.ClrType;
                        isFieldOrProperty =
                            instanceType.GetField(sym.Name, BindingFlags.Instance | BindingFlags.Public) != null
                            || instanceType.GetProperty(sym.Name, BindingFlags.Instance | BindingFlags.Public) != null;
                    }
                }

                if (isFieldOrProperty)
                {
                    Symbol sym = (Symbol)RT.third(form);
                    if (t != null)
                        return new StaticFieldExpr(t, sym.Name);
                    else
                        return new InstanceFieldExpr(instance, sym.Name);
                }


                ISeq call = RT.third(form) is ISeq ? (ISeq)RT.third(form) : RT.next(RT.next(form));

                if (!(RT.first(call) is Symbol))
                    throw new ArgumentException("Malformed member exception");

                string methodName = ((Symbol)RT.first(call)).Name;
                IPersistentVector args = PersistentVector.EMPTY;

                for (ISeq s = RT.next(call); s != null; s = s.next())
                    args = args.cons(Compiler.GenerateAST(s.first()));

                return t != null
                    ? (MethodExpr)(new StaticMethodExpr(t, methodName, args))
                    : (MethodExpr)(new InstanceMethodExpr(instance, methodName, args));
            }
        }

        public abstract Expression GenDlrUnboxed(GenContext context);


        protected static List<MethodInfo> GetMethods(Type targetType, int arity,  string methodName, bool getStatics)
        {
            BindingFlags flags = BindingFlags.Public | BindingFlags.FlattenHierarchy | BindingFlags.InvokeMethod;

            flags |= getStatics ? BindingFlags.Static : BindingFlags.Instance;

            IEnumerable<MethodInfo> einfo 
                = targetType.GetMethods(flags).Where(info => info.Name == methodName && info.GetParameters().Length == arity);
            List<MethodInfo> infos = new List<MethodInfo>(einfo);

            return infos;
        }


        protected static MethodInfo GetMatchingMethod(Type targetType, IPersistentVector args, string methodName)
        {
            MethodInfo method = GetMatchingMethodAux(targetType, args, methodName, true);

            MaybeReflectionWarn(method, methodName);

            return method;
        }

        protected static MethodInfo GetMatchingMethod(Expr target, IPersistentVector args, string methodName)
        {
            MethodInfo method = target.HasClrType ? GetMatchingMethodAux(target.ClrType, args, methodName, false) : null;

            MaybeReflectionWarn(method, methodName);

            return method;
        }

        private static void MaybeReflectionWarn(MethodInfo method, string methodName)
        {
            if ( method == null && RT.booleanCast(RT.WARN_ON_REFLECTION.deref()) )
                // TODO: use DLR IO
                ((TextWriter)RT.ERR.deref()).WriteLine(string.Format("Reflection warning, {0}:{1} - call to {2} can't be resolved.\n",
                    Compiler.SOURCE_PATH.deref(), /* line ,*/0, methodName));
        }

        private static MethodInfo GetMatchingMethodAux(Type targetType, IPersistentVector args, string methodName, bool getStatics)
        {
            MethodInfo method = null;

            List<MethodInfo> methods = HostExpr.GetMethods(targetType, args.count(), methodName, getStatics);

            if (methods.Count == 0)
                method = null;
            else
            {

                int index = 0;
                if (methods.Count > 1)
                {
                    List<ParameterInfo[]> parms = new List<ParameterInfo[]>(methods.Count);
                    List<Type> rets = new List<Type>(methods.Count);

                    foreach (MethodInfo mi in methods)
                    {
                        parms.Add(mi.GetParameters());
                        rets.Add(mi.ReturnType);
                    }
                    index = GetMatchingParams(methodName, parms, args, rets);
                }
                method = (index >= 0 ? methods[index] : null);
            }

            return method;
        }



 
 
        internal static int GetMatchingParams(string methodName, List<ParameterInfo[]> parmlists, IPersistentVector argexprs, List<Type> rets)
        {
            // Assume matching lengths
            int matchIndex = -1;
            bool tied = false;
            bool foundExact = false;

            for (int i = 0; i < parmlists.Count; i++)
            {
                bool match = true;
                ISeq aseq = argexprs.seq();
                int exact = 0;
                for (int p = 0; match && p < argexprs.count() && aseq != null; ++p, aseq = aseq.next())
                {
                    Expr arg = (Expr)aseq.first();
                    Type atype = arg.HasClrType ? arg.ClrType : typeof(object);
                    Type ptype = parmlists[i][p].ParameterType;
                    if (arg.HasClrType && atype == ptype)
                        exact++;
                    else
                        match = Reflector.ParamArgTypeMatch(ptype, atype);
                }

                if (exact == argexprs.count())
                {
                    if ( !foundExact || matchIndex == -1 || rets[matchIndex].IsAssignableFrom(rets[i]))
                        matchIndex = i;
                    foundExact = true;
                }
                else if (match && !foundExact)
                {
                    if (matchIndex == -1)
                        matchIndex = i;
                    else
                    {
                        if (Reflector.Subsumes(parmlists[i], parmlists[matchIndex]))
                        {
                            matchIndex = i;
                            tied = false;
                        }
                        else if (Array.Equals(parmlists[i], parmlists[matchIndex]))
                            if (rets[matchIndex].IsAssignableFrom(rets[i]))
                                matchIndex = i;
                            else if (!Reflector.Subsumes(parmlists[matchIndex], parmlists[i]))
                                tied = true;
                    }
                }
            }

            if (tied)
                throw new ArgumentException("More than one matching method found: " + methodName);

            return matchIndex;
        }

        internal static Expression[] GenTypedArgs(GenContext context, ParameterInfo[] parms, IPersistentVector args)
        {
            Expression[] exprs = new Expression[parms.Length];
            for (int i = 0; i < parms.Length; i++)
                exprs[i] = GenTypedArg(context,parms[i].ParameterType, (Expr)args.nth(i));
            return exprs;
        }

        internal static Expression GenTypedArg(GenContext context, Type type, Expr arg)
        {
            if (Compiler.MaybePrimitiveType(arg) == type)
                return ((MaybePrimitiveExpr)arg).GenDlrUnboxed(context);
            else
                // Java has emitUnboxArg -- should we do something similar?
                return arg.GenDlr(context);
        }

        #endregion
    }
}
