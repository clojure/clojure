/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Sep 3, 2006 */

package clojure.lang;

public class Compiler{
/*
static Symbol DEFN = Symbol.intern("defn");
static Symbol T = Symbol.intern("t");
static public Var OUT = Namespace.intern("clojure", "^out");
static NilExpr NIL_EXPR = new NilExpr();
static TExpr T_EXPR = new TExpr();

enum C{
	STATEMENT,EXPRESSION,RETURN,TOP}

;

interface Expr{
	public void emit(C context) throws Exception;

}

static class AnExpr implements Expr{

	public void emit(C context) throws Exception{
		if(context == C.RETURN)
			emitReturn();
		else if(context == C.STATEMENT)
			emitStatement();
		else if(context == C.EXPRESSION)
			emitExpression();
		else
			throw new UnsupportedOperationException();
	}

	void emitReturn() throws Exception{
		RT.format(T, "return ");
		emitExpression();
		RT.format(T, ";~%");
	}

	void emitStatement() throws Exception{
		//no op
	}

	void emitExpression() throws Exception{
		throw new UnsupportedOperationException();
	}
}

public static Object processForm(Object form) throws Exception{
	if(RT.equal(RT.first(form), DEFN))
		{
		return convert(form);
		}
}

private static Object convert(Object form) throws Exception{
	Expr e = analyze(C.TOP, form);
	emit(C.TOP, e);
}

private static void emit(C context, Object e) throws Exception{
	if(e instanceof Expr)
		((Expr) e).emit(context);
	else
		emitLiteral(context, e);
}

private static void emitLiteral(C context, Object e) throws Exception{
	if(context == C.RETURN)
		emitReturn(e);
	else if(context == C.STATEMENT)
		return;
	else if(context == C.EXPRESSION)
		{
		if(e == null)
			RT.format(T, "null");
		else if(RT.equal(e, T))
			RT.format(T, "RT.T");
		}
}

private static void emitReturn(Object e) throws Exception{
	RT.format(T, "return ");
	emit(C.EXPRESSION, e);
	RT.format(T, ";~%");
}

private static Expr analyze(C context, Object form){
	if(form == null)
		return NIL_EXPR;
	else if(RT.equal(form, T))
		return T_EXPR;
	else if(form instanceof Symbol)
		return analyzeSymbol(context, (Symbol) form);
}

private static Expr analyzeSymbol(C context, Symbol sym){

}


static class NilExpr extends AnExpr{
	void emitExpression() throws Exception{
		RT.format(T, "null");
	}
}
static class TExpr extends AnExpr{
	void emitExpression() throws Exception{
		RT.format(T, "RT.T");
	}
}
//*/

}
