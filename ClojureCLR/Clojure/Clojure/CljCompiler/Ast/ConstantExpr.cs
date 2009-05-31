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
    class ConstantExpr : LiteralExpr
    {
        #region Data

        readonly object _v;
        readonly int _id;

        public override object val()
        {
            return _v;
        }

        #endregion

        #region Ctors

        public ConstantExpr(object v)
        {
            _v = v;
            _id = Compiler.RegisterConstant(v);
        }            

        #endregion

        #region Type mangling

        public override bool HasClrType
        {
            get { return _v.GetType().IsPublic; }
        }

        public override Type ClrType
        {
            get { return _v.GetType(); }
        }

        #endregion

        #region Parsing

        public sealed class Parser : IParser
        {
            public Expr Parse(object form)
            {
                object v = RT.second(form);
                if (v == null)
                    return Compiler.NIL_EXPR;
                else
                    return new ConstantExpr(v);
            }
        }

        #endregion

        #region Code generation

        public override Expression GenDlr(GenContext context)
        {
            // Java: fn.emitConstant(gen,id)
            //return Expression.Constant(_v);
            return context.FnExpr.GenConstant(context,_id,_v);
        }

        #endregion
    }
}
