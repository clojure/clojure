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
    class AssignExpr : Expr
    {
        #region Data

        readonly AssignableExpr _target;
        readonly Expr _val;

        #endregion

        #region Ctors

        public AssignExpr(AssignableExpr target, Expr val)
        {
            _target = target;
            _val = val;
        }

        #endregion

        #region Type mangling

        public override bool HasClrType
        {
            get { return _val.HasClrType; }
        }

        public override Type ClrType
        {
            get { return _val.ClrType; }
        }

        #endregion

        #region Parsing

        public sealed class Parser : IParser
        {
            public Expr Parse(object frm)
            {
                ISeq form = (ISeq)frm;
                if (RT.Length(form) != 3)
                    throw new ArgumentException("Malformed assignment, expecting (set! target val)");
                Expr target = Compiler.GenerateAST(RT.second(form));
                if (!(target is AssignableExpr))
                    throw new ArgumentException("Invalid assignment target");
                return new AssignExpr((AssignableExpr)target,
                    Compiler.GenerateAST(RT.third(form)));
            }
        }

        #endregion

        #region Code generation

        public override Expression GenDlr(GenContext context)
        {
            return _target.GenAssignDlr(context, _val);
        }

        #endregion
    }
}
