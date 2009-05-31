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
using clojure.runtime;

namespace clojure.lang.CljCompiler.Ast
{
    class IfExpr : Expr
    {
        #region Data

        readonly Expr _testExpr;
        readonly Expr _thenExpr;
        readonly Expr _elseExpr;

        #endregion

        #region Ctors

        public IfExpr(Expr testExpr, Expr thenExpr, Expr elseExpr)
        {
            _testExpr = testExpr;
            _thenExpr = thenExpr;
            _elseExpr = elseExpr;
        }

        #endregion

        #region Type mangling

        public override bool HasClrType
        {
            get
            {
                if (_elseExpr == null)
                    return _thenExpr.HasClrType;
                else
                    return _thenExpr.HasClrType
                    && _elseExpr.HasClrType
                    && (_thenExpr.ClrType == _elseExpr.ClrType
                        || _thenExpr.ClrType == null
                        || _elseExpr.ClrType == null);
            }
        }

        public override Type ClrType
        {
            get
            {
                Type thenType = _thenExpr.ClrType;

                if (_elseExpr == null)
                    return thenType;
                else
                    return thenType ?? _elseExpr.ClrType;
            }
        }

        #endregion

        #region Parsing

        public sealed class Parser : IParser
        {
            public Expr Parse(object frm)
            {
                ISeq form = (ISeq)frm;

                // (if test then) or (if test then else)

                if (form.count() > 4)
                    throw new Exception("Too many arguments to if");

                if (form.count() < 3)
                    throw new Exception("Too few arguments to if");


                Expr testExpr = Compiler.GenerateAST(RT.second(form));
                Expr thenExpr = Compiler.GenerateAST(RT.third(form));
                Expr elseExpr = form.count() == 4 ? Compiler.GenerateAST(RT.fourth(form)) : null;

                return new IfExpr(testExpr, thenExpr, elseExpr);
            }
        }

        #endregion

        #region Code generation

        public override Expression GenDlr(GenContext context)
        {
            // Original code made a call to RT.IsTrue.
            // Now we inline the test.
            // Not clear if there is much speedup from this.

            //bool testIsBool = _testExpr is MaybePrimitiveExpr && _testExpr.HasClrType && _testExpr.ClrType == typeof(bool);

            //Expression testCode = testIsBool
            //    ? ((MaybePrimitiveExpr)_testExpr).GenDlrUnboxed(context)
            //    // TODO: Verify the call to MaybeBox is needed.
            //    // TODO: See if we can write the code more directly than calling RT.IsTrue.
            //    : Expression.Call(Compiler.Method_RT_IsTrue, Compiler.MaybeBox(_testExpr.GenDlr(context)));


            bool testIsBool = _testExpr is MaybePrimitiveExpr && _testExpr.HasClrType && _testExpr.ClrType == typeof(bool);

            Expression testCode;


            if (testIsBool)
                testCode = ((MaybePrimitiveExpr)_testExpr).GenDlrUnboxed(context);
            else
            {
                ParameterExpression testVar = Expression.Parameter(typeof(object), "__test");
                Expression assign = Expression.Assign(testVar, Compiler.MaybeBox(_testExpr.GenDlr(context)));
                Expression boolExpr =
                    Expression.Not(
                        Expression.OrElse(
                            Expression.Equal(testVar, Expression.Constant(null)),
                            Expression.AndAlso(Expression.TypeIs(testVar, typeof(bool)), Expression.IsFalse(Expression.Unbox(testVar, typeof(bool))))));
                //Expression.Not(Expression.AndAlso(Expression.TypeIs(testVar, typeof(bool)), Expression.IsFalse(Expression.Convert(testVar,typeof(bool))))));
                testCode = Expression.Block(typeof(bool), new ParameterExpression[] { testVar }, assign, boolExpr);
            }

            Expression thenCode = _thenExpr.GenDlr(context);
            Expression elseCode = _elseExpr == null
                ? Expression.Constant(null, typeof(object))
                : _elseExpr.GenDlr(context);

            Type targetType = typeof(object);
            if (this.HasClrType && this.ClrType != null)
                // In this case, both _thenExpr and _elseExpr have types, and they are the same, or one is null.
                // TODO: Not sure if this works if one has a null value.
                targetType = this.ClrType;

            if (thenCode.Type == typeof(void) && elseCode.Type != typeof(void))
                thenCode = Expression.Block(thenCode, Expression.Default(elseCode.Type));
            else if (elseCode.Type == typeof(void) && thenCode.Type != typeof(void))
                elseCode = Expression.Block(elseCode, Expression.Default(thenCode.Type));
            else if (!Reflector.AreReferenceAssignable(targetType, thenCode.Type) || !Reflector.AreReferenceAssignable(targetType, elseCode.Type))
            // Above: this is the test that Expression.Condition does.
            {
                // Try to reconcile
                if (thenCode.Type.IsAssignableFrom(elseCode.Type) && elseCode.Type != typeof(void))
                {
                    elseCode = Expression.Convert(elseCode, thenCode.Type);
                    targetType = thenCode.Type;
                }
                else if (elseCode.Type.IsAssignableFrom(thenCode.Type) && thenCode.Type != typeof(void))
                {
                    thenCode = Expression.Convert(thenCode, elseCode.Type);
                    targetType = elseCode.Type;
                }
                else
                {
                    //if (thenCode.Type == typeof(void))
                    //{
                    //    thenCode = Expression.Block(thenCode, Expression.Default(elseCode.Type));
                    //    targetType = elseCode.Type;
                    //}
                    //else if (elseCode.Type == typeof(void))
                    //{
                    //    elseCode = Expression.Block(elseCode, Expression.Default(thenCode.Type));
                    //    targetType = thenCode.Type;
                    //}
                    //else
                    //{
                    // TODO: Can we find a common ancestor?  probably not.
                    thenCode = Expression.Convert(thenCode, typeof(object));
                    elseCode = Expression.Convert(elseCode, typeof(object));
                    targetType = typeof(object);
                    //}
                }
            }

            return Expression.Condition(testCode, thenCode, elseCode, targetType);
        }

        #endregion
    }
}
