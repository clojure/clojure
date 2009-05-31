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

namespace clojure.lang.CljCompiler.Ast
{
    class InvokeExpr : Expr
    {
        #region Data

        readonly Expr _fexpr;
        readonly Object _tag;
        readonly IPersistentVector _args;

        #endregion

        #region Ctors

        public InvokeExpr(Symbol tag, Expr fexpr, IPersistentVector args)
        {
            _fexpr = fexpr;
            _args = args;
            _tag = tag ?? (fexpr is VarExpr ? ((VarExpr)fexpr).Tag : null);
        }

        #endregion

        #region Type mangling

        public override bool HasClrType
        {
            get { return _tag != null; }
        }

        public override Type ClrType
        {
            get { return Compiler.TagToType(_tag); }
        }

        #endregion

        #region Parsing

        public static Expr Parse(ISeq form)
        {
            Expr fexpr = Compiler.GenerateAST(form.first());
            IPersistentVector args = PersistentVector.EMPTY;
            for ( ISeq s = RT.seq(form.next()); s != null; s = s.next())
                args = args.cons(Compiler.GenerateAST(s.first()));
            return new InvokeExpr(Compiler.TagOf(form),fexpr,args);
        }

        #endregion

        #region Code generation

        public override Expression GenDlr(GenContext context)
        {
            Expression fn = _fexpr.GenDlr(context);
            fn = Expression.Convert(fn, typeof(IFn));

            int argCount = _args.count();

            Expression[] args = new Expression[argCount];

            for (int i = 0; i < argCount; i++ )
                args[i] = Compiler.MaybeBox(((Expr)_args.nth(i)).GenDlr(context));

            Expression call = GenerateInvocation(InvocationReturnType, fn, args);

            return call;
        }

        private Type InvocationReturnType
        {
            get
            {
                return (_tag == null)
                    ? null
                    : Compiler.TagToType(_tag);
            }
        }

        private static Expression GenerateInvocation(Type returnType, Expression fn, Expression[] args)
        {
            MethodInfo mi;
            Expression[] actualArgs;

            if (args.Length <= Compiler.MAX_POSITIONAL_ARITY)
            {
                mi = Compiler.Methods_IFn_invoke[args.Length];
                actualArgs = args;
            }
            else
            {
                // pick up the extended version.
                mi = Compiler.Methods_IFn_invoke[Compiler.MAX_POSITIONAL_ARITY + 1];
                Expression[] leftoverArgs = new Expression[args.Length - Compiler.MAX_POSITIONAL_ARITY];
                Array.ConstrainedCopy(args, Compiler.MAX_POSITIONAL_ARITY, leftoverArgs, 0, args.Length - Compiler.MAX_POSITIONAL_ARITY);

                Expression restArg = Expression.NewArrayInit(typeof(object), leftoverArgs);

                actualArgs = new Expression[Compiler.MAX_POSITIONAL_ARITY + 1];
                Array.ConstrainedCopy(args, 0, actualArgs, 0, Compiler.MAX_POSITIONAL_ARITY);
                actualArgs[Compiler.MAX_POSITIONAL_ARITY] = restArg;
            }

            Expression call = Expression.Call(fn, mi, actualArgs);
            // Java version doesn't seem to do this.  Instead, its InvokeExpression carries the type information so someone else can use it.
            // Not sure if this is useful here.
            if (returnType != null)
                call = Expression.Convert(call, returnType);

            return call;
        }

        #endregion
    }
}
