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
using AstUtils = Microsoft.Scripting.Ast.Utils;

namespace clojure.lang.CljCompiler.Ast
{
    class InstanceMethodExpr : MethodExpr
    {
        #region Data

        readonly Expr _target;
        readonly string _methodName;
        readonly IPersistentVector _args;
        readonly MethodInfo _method;

        #endregion

        #region Ctors

        public InstanceMethodExpr(Expr target, string methodName, IPersistentVector args)
        {
            _target = target;
            _methodName = methodName;
            _args = args;

            _method = GetMatchingMethod(target, _args, _methodName);
        }

        #endregion

        #region Type mangling

        public override bool HasClrType
        {
            get { return _method != null; }
        }

        public override Type ClrType
        {
            get { return _method.ReturnType; }
        }

        #endregion

        #region Code generation

        public override Expression GenDlr(GenContext context)
        {
            if (_method != null)
                return Compiler.MaybeBox(GenDlrForMethod(context));
            else
                return GenDlrViaReflection(context);
        }

        public override Expression GenDlrUnboxed(GenContext context)
        {
            if (_method != null)
                return GenDlrForMethod(context);
            else
                throw new InvalidOperationException("Unboxed emit of unknown member.");
        }

        private Expression GenDlrForMethod(GenContext context)
        {
            Expression target = _target.GenDlr(context);
            Expression[] args = GenTypedArgs(context, _method.GetParameters(), _args);

            return AstUtils.SimpleCallHelper(target,_method, args); ;

        }

        private Expression GenDlrViaReflection(GenContext context)
        {
            Expression[] parms = new Expression[_args.count()];
            for (int i = 0; i < _args.count(); i++)
                parms[i] = Compiler.MaybeBox(((Expr)_args.nth(i)).GenDlr(context));

            Expression[] moreArgs = new Expression[3];
            moreArgs[0] = Expression.Constant(_methodName);
            moreArgs[1] = _target.GenDlr(context);
            moreArgs[2] = Expression.NewArrayInit(typeof(object), parms);

            return Expression.Call(Compiler.Method_Reflector_CallInstanceMethod, moreArgs);
        }

        #endregion
    }
}
