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
    class UnresolvedVarExpr : Expr
    {
        #region Data

        readonly Symbol _symbol;

        #endregion

        #region Ctors

        public UnresolvedVarExpr(Symbol symbol)
        {
            _symbol = symbol;
        }

        #endregion

        #region Type mangling

        public override bool HasClrType
        {
            get { return false; }
        }

        public override Type ClrType
        {
            get { throw new InvalidOperationException("UnresolvedVarExpr has no CLR type");  }
        }

        #endregion

        #region Code generation

        public override Expression GenDlr(GenContext context)
        {
            return Expression.Empty();
        }

        #endregion
    }
}
