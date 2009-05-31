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
    class BodyExpr : Expr
    {
        #region Data

        readonly IPersistentVector _exprs;

        Expr LastExpr
        {
            get
            {
                return (Expr)_exprs.nth(_exprs.count() - 1);
            }
        }

        #endregion

        #region Ctors

        public BodyExpr(IPersistentVector exprs)
        {
            _exprs = exprs;
        }

        #endregion

        #region Type mangling

        public override bool HasClrType
        {
            get { return LastExpr.HasClrType; }
        }

        public override Type ClrType
        {
            get { return LastExpr.ClrType; }
        }

        #endregion

        #region Parsing

        public sealed class Parser : IParser
        {
            public Expr Parse(object frms)
            {
                ISeq forms = (ISeq)frms;

                if (Util.equals(RT.first(forms), Compiler.DO))
                    forms = RT.next(forms);

                IPersistentVector exprs = PersistentVector.EMPTY;

                for (ISeq s = forms; s != null; s = s.next())
                {
                    if (s.next() == null)
                    {
                        // in tail recurive position
                        try
                        {
                            Var.pushThreadBindings(PersistentHashMap.create(Compiler.IN_TAIL_POSITION, RT.T));
                            exprs = exprs.cons(Compiler.GenerateAST(s.first()));
                        }
                        finally
                        {
                            Var.popThreadBindings();
                        }
                    }
                    else
                        exprs = exprs.cons(Compiler.GenerateAST(s.first()));
                }
                if (exprs.count() == 0)
                    exprs = exprs.cons(Compiler.NIL_EXPR);

                return new BodyExpr(exprs);

            }
        }

        #endregion

        #region Code generateion

        public override Expression GenDlr(GenContext context)
        {
            List<Expression> exprs = new List<Expression>(_exprs.count());

            for (int i = 0; i < _exprs.count() - 1; i++)
            {
                Expr e = (Expr)_exprs.nth(i);
                exprs.Add(e.GenDlr(context));
            }

            // In Java version, this is split off because the Context in the calls above is forced to be C.STATEMENT.
            // TODO: Wrap this into the loop above.  No real need to do this way.
            Expr last = (Expr)_exprs.nth(_exprs.count() - 1);
            exprs.Add(last.GenDlr(context));

            return Expression.Block(exprs);
        }

        #endregion
    }
}
