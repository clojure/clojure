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
    class TryExpr : Expr
    {
        #region Nested classes

        public sealed class CatchClause
        {
            readonly Type _type;
            public Type Type
            {
              get { return _type; }  
            } 

            readonly LocalBinding _lb;
            internal LocalBinding Lb
            {
              get { return _lb; }  
            } 

            readonly Expr _handler;
            internal Expr Handler
            {
              get { return _handler; }  
            } 


            public CatchClause(Type type, LocalBinding lb, Expr handler)
            {
                _type = type;
                _lb = lb;
                _handler = handler;
            }
        }

        #endregion

        #region Data

        readonly Expr _tryExpr;
        readonly Expr _finallyExpr;
        readonly IPersistentVector _catchExprs;
        readonly int _retLocal;
        readonly int _finallyLocal;

        #endregion

        #region Ctors

        public TryExpr(Expr tryExpr, IPersistentVector catchExprs, Expr finallyExpr, int retLocal, int finallyLocal)
        {
            _tryExpr = tryExpr;
            _catchExprs = catchExprs;
            _finallyExpr = finallyExpr;
            _retLocal = retLocal;
            _finallyLocal = finallyLocal;

        }

        #endregion

        #region Type mangling

        public override bool HasClrType
        {
            get { return _tryExpr.HasClrType; }
        }

        public override Type ClrType
        {
            get { return _tryExpr.ClrType; }
        }

        #endregion

        #region Parsing

        public sealed class Parser : IParser
        {
            public Expr Parse(object frm)
            {
                ISeq form = (ISeq)frm;

                // Java version has this:
                //if (context != C.RETURN)
                //    return analyze(context, RT.list(RT.list(FN, PersistentVector.EMPTY, form)));
                // TODO: figure out why it matters.

                // (try try-expr* catch-expr* finally-expr?)
                // catch-expr: (catch class sym expr*)
                // finally-expr: (finally expr*)

                IPersistentVector body = PersistentVector.EMPTY;
                IPersistentVector catches = PersistentVector.EMPTY;
                Expr finallyExpr = null;
                bool caught = false;

                int retLocal = Compiler.GetAndIncLocalNum();
                int finallyLocal = Compiler.GetAndIncLocalNum();

                for (ISeq fs = form.next(); fs != null; fs = fs.next())
                {
                    object f = fs.first();
                    object op = (f is ISeq) ? ((ISeq)f).first() : null;
                    if (!Util.equals(op, Compiler.CATCH) && !Util.equals(op, Compiler.FINALLY))
                    {
                        if (caught)
                            throw new Exception("Only catch or finally clause can follow catch in try expression");
                        body = body.cons(f);
                    }
                    else
                    {
                        if (Util.equals(op, Compiler.CATCH))
                        {
                            Type t = Compiler.MaybeType(RT.second(f), false);
                            if (t == null)
                                throw new ArgumentException("Unable to resolve classname: " + RT.second(f));
                            if (!(RT.third(f) is Symbol))
                                throw new ArgumentException("Bad binding form, expected symbol, got: " + RT.third(f));
                            Symbol sym = (Symbol)RT.third(f);
                            if (sym.Namespace != null)
                                throw new Exception("Can't bind qualified name: " + sym);

                            IPersistentMap dynamicBindings = RT.map(
                                Compiler.LOCAL_ENV, Compiler.LOCAL_ENV.deref(),
                                Compiler.NEXT_LOCAL_NUM, Compiler.NEXT_LOCAL_NUM.deref(),
                                Compiler.IN_CATCH_FINALLY, RT.T);

                            try
                            {
                                Var.pushThreadBindings(dynamicBindings);
                                LocalBinding lb = Compiler.RegisterLocal(sym,
                                    (Symbol)(RT.second(f) is Symbol ? RT.second(f) : null),
                                    null);
                                Expr handler = (new BodyExpr.Parser()).Parse(RT.next(RT.next(RT.next(f))));
                                catches = catches.cons(new CatchClause(t, lb, handler)); ;
                            }
                            finally
                            {
                                Var.popThreadBindings();
                            }
                            caught = true;
                        }
                        else // finally
                        {
                            if (fs.next() != null)
                                throw new Exception("finally clause must be last in try expression");
                            try
                            {
                                Var.pushThreadBindings(RT.map(Compiler.IN_CATCH_FINALLY, RT.T));
                                finallyExpr = (new BodyExpr.Parser()).Parse(RT.next(f));
                            }
                            finally
                            {
                                Var.popThreadBindings();
                            }
                        }
                    }
                }

                Expr bodyExpr = (new BodyExpr.Parser()).Parse(RT.seq(body));
                return new TryExpr(bodyExpr, catches, finallyExpr, retLocal, finallyLocal);
              }
        }

        #endregion

        #region Code generation

        public override Expression GenDlr(GenContext context)
        {
            Expression basicBody = _tryExpr.GenDlr(context);
            // Wrap the basic body, a Comma, in a return to a label
            LabelTarget target = Expression.Label(basicBody.Type, "ret_label");
            //Expression tryBody = Expression.Return(target, basicBody);
            Expression tryBody = basicBody;

            CatchBlock[] catches = new CatchBlock[_catchExprs.count()];
            for ( int i=0; i<_catchExprs.count(); i++ )
            {
                CatchClause clause = (CatchClause) _catchExprs.nth(i);
                ParameterExpression parmExpr = Expression.Parameter(clause.Type, clause.Lb.Name);
                clause.Lb.ParamExpression = parmExpr;
                catches[i] = Expression.Catch(parmExpr,clause.Handler.GenDlr(context));
            }

            TryExpression tryStmt = _finallyExpr == null
                ? Expression.TryCatch(tryBody, catches)
                : Expression.TryCatchFinally(tryBody, _finallyExpr.GenDlr(context), catches);

            Expression defaultValue = Expression.Default(basicBody.Type);
            Expression whole = Expression.Block(tryStmt, Expression.Label(target, defaultValue));
            return whole;
        }

        #endregion
    }
}
