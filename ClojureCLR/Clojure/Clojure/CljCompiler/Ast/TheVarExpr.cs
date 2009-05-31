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
    class TheVarExpr : Expr
    {
        #region Data

        readonly Var _var;

        #endregion

        #region Ctors

        public TheVarExpr(Var var)
        {
            _var = var;
        }

        #endregion

        #region Type mangling

        public override bool HasClrType
        {
            get { return true; }
        }

        public override Type ClrType
        {
            get { return typeof(Var); }
        }

        #endregion

        #region Parsing

        public sealed class Parser : IParser
        {
            public Expr Parse(object form)
            {
                Symbol sym = (Symbol)RT.second(form);
                Var v = Compiler.LookupVar(sym, false);
                if (v != null)
                    return new TheVarExpr(v);
                throw new Exception(string.Format("Unable to resolve var: {0} in this context", sym));
            }
        }

        #endregion

        #region Code generation

        public override Expression GenDlr(GenContext context)
        {
            return context.FnExpr.GenVar(context,_var);
        }

        #endregion
    }
}
