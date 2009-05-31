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
    class RecurExpr : Expr
    {
        #region Data

        readonly IPersistentVector _args;
        readonly IPersistentVector _loopLocals;

        #endregion

        #region Ctors

        public RecurExpr(IPersistentVector loopLocals, IPersistentVector args)
        {
            _loopLocals = loopLocals;
            _args = args;
        }

        #endregion

        #region Type mangling

        public override bool HasClrType
        {
            get { return true; }
        }

        public override Type ClrType
        {
            get { return typeof(void); }  // Java: returns null.
        }

        #endregion

        #region Parsing

        public sealed class Parser : IParser
        {
            public Expr Parse(object frm)
            {
                ISeq form = (ISeq)frm;

                IPersistentVector loopLocals = (IPersistentVector)Compiler.LOOP_LOCALS.deref();

                if (Compiler.IN_TAIL_POSITION.deref() == null || loopLocals == null)
                    throw new InvalidOperationException("Can only recur from tail position");

                if (Compiler.IN_CATCH_FINALLY.deref() != null)
                    throw new InvalidOperationException("Cannot recur from catch/finally.");

                IPersistentVector args = PersistentVector.EMPTY;

                for (ISeq s = form.next(); s != null; s = s.next())
                    args = args.cons(Compiler.GenerateAST(s.first()));
                if (args.count() != loopLocals.count())
                    throw new ArgumentException(string.Format("Mismatched argument count to recur, expected: {0} args, got {1}", 
                        loopLocals.count(), args.count()));

                return new RecurExpr(loopLocals, args);
            }
        }

        #endregion

        #region Code generation

        public override Expression GenDlr(GenContext context)
        {
            LabelTarget loopLabel = (LabelTarget)Compiler.LOOP_LABEL.deref();
            if (loopLabel == null)
                throw new InvalidOperationException("Recur not in proper context.");

            int argCount = _args.count();

            List<ParameterExpression> tempVars = new List<ParameterExpression>(argCount);
            List<Expression> tempAssigns = new List<Expression>(argCount);
            List<Expression> finalAssigns = new List<Expression>(argCount);

            // Evaluate all the init forms into local variables.
            // TODO: Check the typing here.
            for (int i = 0; i < _loopLocals.count(); i++)
            {
                LocalBinding b = (LocalBinding)_loopLocals.nth(i);
                Expr arg = (Expr)_args.nth(i);
                ParameterExpression tempVar = Expression.Parameter(b.ParamExpression.Type, "__local__" + i);  //asdf-tag
                Expression valExpr = ((Expr)_args.nth(i)).GenDlr(context);
                tempVars.Add(tempVar);

                if (tempVar.Type == typeof(Object))
                    tempAssigns.Add(Expression.Assign(tempVar, Compiler.MaybeBox(valExpr)));
                else
                    tempAssigns.Add(Expression.Assign(tempVar, Expression.Convert(valExpr, tempVar.Type)));                     //asdf-tag

                finalAssigns.Add(Expression.Assign(b.ParamExpression, tempVar));  //asdf-tag
            }

            List<Expression> exprs = tempAssigns;
            exprs.AddRange(finalAssigns);
            exprs.Add(Expression.Goto(loopLabel));
            // need to do this to get a return value in the type inferencing -- else can't use this in a then or else clause.
            exprs.Add(Expression.Constant(null));
            return Expression.Block(tempVars, exprs);
        }

        #endregion
    }
}
