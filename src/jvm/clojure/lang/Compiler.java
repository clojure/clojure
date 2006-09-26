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

import java.util.HashMap;

public class Compiler{
///*
static Symbol DEF = Symbol.intern("def");
static public Var OUT = Module.intern("clojure", "^out");
static public Var IMPORT_MAP = Module.intern("clojure", "^import-map");
static NilExpr NIL_EXPR = new NilExpr();

enum C{STATEMENT,EXPRESSION,RETURN}

interface Expr{

    void emitReturn() throws Exception;

    void emitStatement() throws Exception;

    void emitExpression() throws Exception;
}

static void format(String str,Object... args) throws Exception {
    RT.format(RT.T, str, args);
}

static class AnExpr implements Expr{

    public void emitReturn() throws Exception{
        format("return ");
        emitExpression();
        format(";~%");
    }

    public void emitStatement() throws Exception{
        //no op
    }

    public void emitExpression() throws Exception{
        throw new UnsupportedOperationException();
    }
}

public static void processForm(Object form) throws Exception{
    if(RT.first(form) == DEF)
        {
        convert(form);
        }
    else
        throw new UnsupportedOperationException();
}

private static void convert(Object form) throws Exception{
    Expr e = analyze(C.STATEMENT, form);
}

private static Expr analyze(C context, Object form){
    if(form == null)
        return NIL_EXPR;
    else if(form instanceof Symbol)
        return analyzeSymbol((Symbol) form);
    else if(form instanceof ISeq)
        return analyzeSeq(context, (ISeq) form);
    else if(form instanceof Num || form instanceof String || form instanceof Character)
        return new LiteralExpr(form);
    else
        throw new UnsupportedOperationException();
}

private static Expr analyzeSeq(C context, ISeq form) {
    return null;
}

private static Expr analyzeSymbol(Symbol sym){
    if(sym instanceof HostSymbol)
        return new HostExpr((HostSymbol)sym);
    else
        {
        int slash = sym.name.indexOf('/');
        String typehint = null;
        if(slash > 0)
            {
            typehint = sym.name.substring(slash + 1);
            sym = Symbol.intern(sym.name.substring(0, slash));
            }
        return new SymExpr(sym, typehint);
        }
}


static class NilExpr extends AnExpr{
    public void emitExpression() throws Exception{
        format("null");
    }
}

static class LiteralExpr extends AnExpr{
    Object val;

    public LiteralExpr(Object val){
        this.val = val;
    }

    public void emitExpression() throws Exception{
        format("%S",val);
    }
}

static String resolveHostClassname(String classname) throws Exception {
    if(classname.indexOf('.') != -1)    //presume fully qualified if contains .
        return classname;
    HashMap importMap = (HashMap) IMPORT_MAP.getValue();
    String fullyQualifiedName = (String) importMap.get(classname);
    if(fullyQualifiedName == null)
        throw new Exception("Can't resolve type name: " + classname);
    return fullyQualifiedName;
}

static class HostExpr extends AnExpr{
    HostSymbol sym;

    public HostExpr(HostSymbol sym){
        this.sym = sym;
    }

    public void emitExpression() throws Exception{
        format("%S",sym);
    }
}

static class SymExpr extends AnExpr{
    Symbol sym;
    String typehint;

    public SymExpr(Symbol sym, String typehint){
        this.sym = sym;
        this.typehint = typehint;
    }

    public void emitExpression() throws Exception{
        format("%S",sym);
    }
}

}
//*/


