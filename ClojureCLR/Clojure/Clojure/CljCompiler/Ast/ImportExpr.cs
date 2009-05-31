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
    class ImportExpr : Expr
    {
        #region Data

        readonly string _c;

        #endregion

        #region Ctors

        public ImportExpr(string c)
        {
            _c = c;
        }

        #endregion

        #region Type mangling

        public override bool HasClrType
        {
            get { return false; }
        }

        public override Type ClrType
        {
            get { throw new ArgumentException("ImportExpr has no Java class"); }
        }

        #endregion

        #region Parsing

        public sealed class Parser : IParser
        {
            public Expr Parse(object frm)
            {
                return new ImportExpr((string)RT.second(frm));
            }
        }


        #endregion

        #region Code generation

        public override Expression GenDlr(GenContext context)
        {
            Expression getTypeExpr = Expression.Call(null, Compiler.Method_RT_classForName, Expression.Constant(_c));
            //Expression getNsExpr = Expression.Call(null, Compiler.Method_Compiler_CurrentNamespace);
            Expression getNsExpr = Expression.Property(null, Compiler.Method_Compiler_CurrentNamespace);
            return Expression.Call(getNsExpr, Compiler.Method_Namespace_importClass1, getTypeExpr);   
        }

        #endregion

    }
}
