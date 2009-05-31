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

namespace clojure.lang.CljCompiler.Ast
{
    class ThrowExpr : UntypedExpr
    {
        #region Data

        readonly Expr _excExpr;

        #endregion

        #region Ctors

        public ThrowExpr(Expr excExpr)
        {
            _excExpr = excExpr;
        }

        #endregion

        #region Parsing

        public sealed class Parser : IParser
        {
            public Expr Parse(object form)
            {
                // Java:
                // TODO: figure out if it matters
                //if (context == C.EVAL)
                //    return analyze(context, RT.list(RT.list(FN, PersistentVector.EMPTY, form)));

                return new ThrowExpr(Compiler.GenerateAST(RT.second(form)));
               
            }
        }

        #endregion

        #region Code generation

        public override Expression GenDlr(GenContext context)
        {
            Expression exc = _excExpr.GenDlr(context);
            Expression exc2 = Expression.Convert(exc, typeof(Exception));

            return Expression.Throw(exc2);
        }

        #endregion
    }
}
