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

import java.io.Writer;
import java.io.StringWriter;

public class Compiler{
///*
static Symbol DEF = Symbol.intern("def");
static public Var OUT = Module.intern("clojure", "^out");
static public Var MODULE = Module.intern("clojure", "^module");
static NilExpr NIL_EXPR = new NilExpr();

//short-name-string->full-name-string
static public Var IMPORTS = Module.intern("clojure", "^compiler-imports");
//keyword->keywordexpr
static public Var KEYWORDS = Module.intern("clojure", "^compiler-keywords");
//var->varexpr
static public Var VARS = Module.intern("clojure", "^compiler-vars");
//symbol->localbindingexpr
static public Var LOCALS = Module.intern("clojure", "^compiler-locals");

static public  IPersistentMap CHAR_MAP =
        new PersistentArrayMap('-', "_HY_",
                               '.', "_DT_",
                               ':', "_CL_",
                               '+', "_PL_",
                               '>', "_GT_",
                               '<', "_LT_",
                               '=', "_EQ_",
                               '~', "_TL_",
                               '!', "_EP_",
                               '@', "_AT_",
                               '#', "_SH_",
                               '$', "_DS_",
                               '%', "_PT_",
                               '^', "_CT_",
                               '&', "_AM_",
                               '*', "_ST_",
                               '{', "_LB_",
                               '}', "_RB_",
                               '[', "_LK_",
                               ']', "_RK_",
                               '/', "_SL_",
                               '\\',"_BS_",
                               '?', "_QM_");

static String munge(String name){
    StringBuilder sb = new StringBuilder();
    for(char c : name.toCharArray())
        {
        String sub = (String) CHAR_MAP.get(c);
        if(sub != null)
            sb.append(sub);
        else
            sb.append(c);
        }
    return sb.toString();
}

enum C{STATEMENT,EXPRESSION,RETURN}

interface Expr{

    void emitReturn() throws Exception;

    void emitStatement() throws Exception;

    void emitExpression() throws Exception;

    String emitExpressionString() throws Exception;
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
        emitExpression();
        format(";~%");
    }

    public void emitExpression() throws Exception{
        throw new UnsupportedOperationException();
    }

    public String emitExpressionString() throws Exception {
        StringWriter w = new StringWriter();
        try{
            OUT.pushThreadBinding(w);
            emitExpression();
            return w.toString();
        }
        finally{
            OUT.popThreadBinding();
        }
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
    else if(form instanceof Num || form instanceof String)
        return new LiteralExpr(form);
    else if(form instanceof Character)
        return new CharExpr((Character)form);
    else
        throw new UnsupportedOperationException();
}

private static Expr analyzeSeq(C context, ISeq form) {
    Object op = RT.first(form);
    if(op == DEF)
        return analyzeDef(context, form);
    else
        throw new UnsupportedOperationException();
}

private static Expr analyzeDef(C context, ISeq form) {
    //(def x) or (def x initexpr)
    Symbol name = (Symbol) RT.nth(1, form);
    Module module = (Module) MODULE.getValue();
    Var var = module.intern(name);
    Expr init = analyze(C.EXPRESSION, macroexpand(RT.nth(2, form)));
}

private static Expr analyzeSymbol(Symbol sym){
    if(sym instanceof Keyword)
        return registerKeyword((Keyword)sym);
    else if(sym instanceof HostSymbol)
        return new HostExpr((HostSymbol)sym);
    else
        {
        int slash = sym.name.indexOf('/');
        String typeHint = null;
        if(slash > 0)
            {
            typeHint = sym.name.substring(slash + 1);
            sym = Symbol.intern(sym.name.substring(0, slash));
            }
        return new SymExpr(sym, typeHint);
        }
}

static Object macroexpand(Object x){
    return x; //placeholder
}

private static Expr registerKeyword(Keyword keyword) {
    IPersistentMap keywordsMap = (IPersistentMap) KEYWORDS.getValue();
    KeywordExpr ke = (KeywordExpr) RT.get(keyword,keywordsMap);
    if(ke == null)
        KEYWORDS.setValue(RT.assoc(keyword, ke = new KeywordExpr(keyword),keywordsMap));
    return ke;
}
/*
(defun reference-var (sym)
  (let ((b (first (member sym *var-env* :key (lambda (b)
                                               (@ :symbol b))))))
    (labels
        ((check-closed (b frame)
           (when (and b frame
                      (not (member b (@ :local-bindings frame)))) ;closed over
             (setf (@ :closed? b) t)
             (pushnew b (@ :closes frame))
             (check-closed b (@ :parent frame)))))
      (check-closed b *frame*))
    b))
    */

static String resolveHostClassname(String classname) throws Exception {
    if(classname.indexOf('.') != -1)    //presume fully qualified if contains .
        return classname;
    IPersistentMap importMap = (IPersistentMap) IMPORTS.getValue();
    String fullyQualifiedName = (String) RT.get(classname,importMap);
    if(fullyQualifiedName == null)
        throw new Exception("Can't resolve type name: " + classname);
    return fullyQualifiedName;
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

static class CharExpr extends AnExpr{
    Character val;

    public CharExpr(Character val){
        this.val = val;
    }

    public void emitExpression() throws Exception{
        format("'~A'",val);
    }
}


static class HostExpr extends AnExpr{
    HostSymbol sym;

    public HostExpr(HostSymbol sym){
        this.sym = sym;
    }

    public void emitExpression() throws Exception{
        if(sym instanceof ClassSymbol)
            format("%A.class", resolveHostClassname(((ClassSymbol) sym).className));
    }
}

static class SymExpr extends AnExpr{
    Symbol sym;
    String typeHint;

    public SymExpr(Symbol sym, String typeHint){
        this.sym = sym;
        this.typeHint = typeHint;
    }

    public void emitExpression() throws Exception{
        format("%A", munge(sym.name));
    }
}

static class KeywordExpr extends AnExpr{
    Symbol sym;

    public KeywordExpr(Symbol sym){
        this.sym = sym;
    }

    public void emitExpression() throws Exception {
        format("%A", munge(sym.name));
    }
}

static class LocalBindingExpr extends AnExpr{
    Symbol sym;
    String typeHint;

    public LocalBindingExpr(Symbol sym, String typeHint){
        this.sym = sym;
        this.typeHint = typeHint;
    }

    public void emitExpression() throws Exception{
        format("%A", munge(sym.name));
    }
}

static class VarExpr extends AnExpr{
    VarExpr var;
    Expr init;

    public VarExpr(VarExpr var, Expr init){
        this.var = var;
        this.init = init;
    }

    public void emitExpression() throws Exception{
        format("%A.bind(%A)", var.emitExpressionString(),init.emitExpressionString());
    }
}

}
//*/


