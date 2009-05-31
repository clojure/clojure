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
    class VarExpr : Expr, AssignableExpr
    {
        #region Data

        readonly Var _var;
        readonly object _tag;

        public object Tag
        {
            get { return _tag; }
        } 


        #endregion

        #region Ctors

        public VarExpr(Var var, Symbol tag)
        {
            _var = var;
            _tag = tag ?? var.Tag;
        }


        #endregion

        #region Type mangling

        public override bool HasClrType
        {
            get { return _tag != null; }
        }

        public override Type ClrType
        {
            get { return Compiler.TagToType(_tag);  }
        }

        #endregion

        #region Code generation

        public override Expression GenDlr(GenContext context)
        {
            Expression varExpr = context.FnExpr.GenVar(context,_var);
            return Expression.Call(varExpr, Compiler.Method_Var_get);
        }

        #endregion

        #region AssignableExpr Members

        public Expression GenAssignDlr(GenContext context, Expr val)
        {
            Expression varExpr = context.FnExpr.GenVar(context, _var);
            Expression valExpr = val.GenDlr(context);
            return Expression.Call(varExpr, Compiler.Method_Var_set, valExpr);
        }

        #endregion
    }
}
