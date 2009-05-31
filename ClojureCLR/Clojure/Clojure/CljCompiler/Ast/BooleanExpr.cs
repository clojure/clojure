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
    class BooleanExpr : LiteralExpr
    {
        #region Data

        readonly bool _val;

        public override object val()
        {
            return _val;
        }

        #endregion

        #region C-tors

        public BooleanExpr(bool val)
        {
            _val = val;
        }

        #endregion

        #region Type mangling

        public override bool HasClrType
        {
            get { return true; }
        }

        public override Type ClrType
        {
            get { return typeof(Boolean); }
        }

        public override Expression GenDlr(GenContext context)
        {
            return Expression.Constant(_val);
        }

        #endregion
    }
}
