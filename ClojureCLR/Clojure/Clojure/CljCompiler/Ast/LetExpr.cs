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
    class LetExpr : Expr
    {
        #region Data

        readonly IPersistentVector _bindingInits;
        readonly Expr _body;
        readonly bool _isLoop;

        #endregion

        #region Ctors

        public LetExpr(IPersistentVector bindingInits, Expr body, bool isLoop)
        {
            _bindingInits = bindingInits;
            _body = body;
            _isLoop = isLoop;
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
                ISeq form = (ISeq) frm;

                // form => (let  [var1 val1 var2 val2 ... ] body ... )
                //      or (loop [var1 val1 var2 val2 ... ] body ... )

                bool isLoop = RT.first(form).Equals(Compiler.LOOP);

                IPersistentVector bindings = RT.second(form) as IPersistentVector;

                if (bindings == null)
                    throw new ArgumentException("Bad binding form, expected vector");

                if ((bindings.count() % 2) != 0)
                    throw new ArgumentException("Bad binding form, expected matched symbol/value pairs.");

                ISeq body = RT.next(RT.next(form));

                // TODO: This is one place where context makes a difference.  Need to figure this out.
                //  Second test clause added in Rev 1216.
                // if (ctxt == C.EVAL || (context == c.EXPRESSION && isLoop))
                //    return Generate(RT.list(RT.list(Compiler.FN, PersistentVector.EMPTY, form)));

                // As of Rev 1216, I tried tjos out. 
                // However, it goes into an infinite loop.  Still need to figure this out.
                //if (isLoop)
                //    Generate(RT.list(RT.list(Compiler.FN, PersistentVector.EMPTY, form)));

                IPersistentMap dynamicBindings = RT.map(
                    Compiler.LOCAL_ENV, Compiler.LOCAL_ENV.deref(),
                    Compiler.NEXT_LOCAL_NUM,Compiler.NEXT_LOCAL_NUM.deref());

                if (isLoop)
                    dynamicBindings = dynamicBindings.assoc(Compiler.LOOP_LOCALS, null);

                try
                {
                    Var.pushThreadBindings(dynamicBindings);

                    IPersistentVector bindingInits = PersistentVector.EMPTY;
                    IPersistentVector loopLocals = PersistentVector.EMPTY;

                    for (int i = 0; i < bindings.count(); i += 2)
                    {
                        if (!(bindings.nth(i) is Symbol))
                            throw new ArgumentException("Bad binding form, expected symbol, got " + bindings.nth(i));

                        Symbol sym = (Symbol)bindings.nth(i);
                        if (sym.Namespace != null)
                            throw new Exception("Can't let qualified name: " + sym);

                        Expr init = Compiler.GenerateAST(bindings.nth(i + 1));
                        // Sequential enhancement of env (like Lisp let*)
                        LocalBinding b = Compiler.RegisterLocal(sym, Compiler.TagOf(sym), init);
                        BindingInit bi = new BindingInit(b, init);
                        bindingInits = bindingInits.cons(bi);

                        if (isLoop)
                            loopLocals = loopLocals.cons(b);
                    }
                    if (isLoop)
                        Compiler.LOOP_LOCALS.set(loopLocals);

                    return new LetExpr(bindingInits,
                        new BodyExpr.Parser().Parse(body),
                        isLoop);
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
            LabelTarget loopLabel = Expression.Label();

            List<ParameterExpression> parms = new List<ParameterExpression>();
            List<Expression> forms = new List<Expression>();

            for (int i = 0; i < _bindingInits.count(); i++)
            {
                BindingInit bi = (BindingInit)_bindingInits.nth(i);
                Type primType = Compiler.MaybePrimitiveType(bi.Init);
                ParameterExpression parmExpr = Expression.Parameter(primType ?? typeof(object), bi.Binding.Name);
                bi.Binding.ParamExpression = parmExpr;
                parms.Add(parmExpr);
                //forms.Add(Expression.Assign(parmExpr, Compiler.MaybeBox(bi.Init.GenDlr(context))));
                Expression initExpr = primType != null ? ((MaybePrimitiveExpr)bi.Init).GenDlrUnboxed(context) : Compiler.MaybeBox(bi.Init.GenDlr(context));
                forms.Add(Expression.Assign(parmExpr, initExpr));
            }


            forms.Add(Expression.Label(loopLabel));

            try
            {
                if (_isLoop)
                    Var.pushThreadBindings(PersistentHashMap.create(Compiler.LOOP_LABEL, loopLabel));

                forms.Add(_body.GenDlr(context));
            }
            finally
            {
                if (_isLoop)
                    Var.popThreadBindings();
            }

            Expression block = Expression.Block(parms, forms);
            return block;
        }

 

        #endregion
    }
}
