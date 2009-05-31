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
    class LetFnExpr : Expr
    {
        #region Data

        readonly IPersistentVector _bindingInits;
        readonly Expr _body;

        #endregion

        #region Ctors

        public LetFnExpr(IPersistentVector bindingInits, Expr body)
        {
            _bindingInits = bindingInits;
            _body = body;

        }

        #endregion

        #region Type mangling

        public override bool HasClrType
        {
            get { return _body.HasClrType; }
        }

        public override Type ClrType
        {
            get { return _body.ClrType; }
        }

        #endregion

        #region Parsing

        public sealed class Parser : IParser
        {
            public Expr Parse(object frm)
            {
                ISeq form = (ISeq)frm;

                // form => (letfn*  [var1 (fn [args] body) ... ] body ... )

                IPersistentVector bindings = RT.second(form) as IPersistentVector;

                if (bindings == null)
                    throw new ArgumentException("Bad binding form, expected vector");

                if ((bindings.count() % 2) != 0)
                    throw new ArgumentException("Bad binding form, expected matched symbol/value pairs.");

                ISeq body = RT.next(RT.next(form));

                // TODO: This is one place where context makes a difference.  Need to figure this out.
                // if (ctxt == C.EVAL)
                //    return Generate(RT.list(RT.list(Compiler.FN, PersistentVector.EMPTY, form)));


                IPersistentMap dynamicBindings = RT.map(
                    Compiler.LOCAL_ENV, Compiler.LOCAL_ENV.deref(),
                    Compiler.NEXT_LOCAL_NUM, Compiler.NEXT_LOCAL_NUM.deref());

                try
                {
                    Var.pushThreadBindings(dynamicBindings);

                    // pre-seed env (like Lisp labels)
                    IPersistentVector lbs = PersistentVector.EMPTY;
                    for (int i = 0; i < bindings.count(); i += 2)
                    {
                        if (!(bindings.nth(i) is Symbol))
                            throw new ArgumentException("Bad binding form, expected symbol, got " + bindings.nth(i));

                        Symbol sym = (Symbol)bindings.nth(i);
                        if (sym.Namespace != null)
                            throw new Exception("Can't let qualified name: " + sym);

                        LocalBinding b = Compiler.RegisterLocal(sym, Compiler.TagOf(sym), null);
                        lbs = lbs.cons(b);
                    }

                    IPersistentVector bindingInits = PersistentVector.EMPTY;

                    for (int i = 0; i < bindings.count(); i += 2)
                    {
                        Symbol sym = (Symbol)bindings.nth(i);
                        Expr init = Compiler.GenerateAST(bindings.nth(i + 1),sym.Name);
                        // Sequential enhancement of env (like Lisp let*)
                        LocalBinding b = (LocalBinding)lbs.nth(i / 2);
                        b.Init = init;
                        BindingInit bi = new BindingInit(b, init);
                        bindingInits = bindingInits.cons(bi);
                    }

                    return new LetFnExpr(bindingInits,new BodyExpr.Parser().Parse(body));
                }
                finally
                {
                    Var.popThreadBindings();
                }
            }
        }

        #endregion

        #region Code generation

        public override Expression GenDlr(GenContext context)
        {
            throw new NotImplementedException();

            //List<ParameterExpression> parms = new List<ParameterExpression>();
            //List<Expression> forms = new List<Expression>();

            //for (int i = 0; i < _bindingInits.count(); i++)
            //{
            //    BindingInit bi = (BindingInit)_bindingInits.nth(i);
            //    Type primType = Compiler.MaybePrimitiveType(bi.Init);
            //    ParameterExpression parmExpr = Expression.Parameter(primType ?? typeof(object), bi.Binding.Name);
            //    bi.Binding.ParamExpression = parmExpr;
            //    parms.Add(parmExpr);
            //    //forms.Add(Expression.Assign(parmExpr, Compiler.MaybeBox(bi.Init.GenDlr(context))));
            //    Expression initExpr = primType != null ? ((MaybePrimitiveExpr)bi.Init).GenDlrUnboxed(context) : Compiler.MaybeBox(bi.Init.GenDlr(context));
            //    forms.Add(Expression.Assign(parmExpr, initExpr));
            //}
        }

        #endregion
    }
}
