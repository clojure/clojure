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

import java.io.StringWriter;

public class Compiler{
///*
static Symbol DEF = Symbol.intern("def");
static Symbol FN = Symbol.intern("fn");
static Symbol DO = Symbol.intern("do");
static Symbol _AM_KEY = Symbol.intern("&key");
static Symbol _AM_REST = Symbol.intern("&rest");
static public Var OUT = Module.intern("clojure", "^out");
static public Var MODULE = Module.intern("clojure", "^module");
static NilExpr NIL_EXPR = new NilExpr();

//short-name-string->full-name-string
static public Var IMPORTS = Module.intern("clojure", "^compiler-imports");
//keyword->keywordexpr
static public Var KEYWORDS = Module.intern("clojure", "^compiler-keywords");
//var->var
static public Var VARS = Module.intern("clojure", "^compiler-vars");
//symbol->localbinding
static public Var LOCAL_ENV = Module.intern("clojure", "^compiler-local-env");
//FnFrame
static public Var METHOD = Module.intern("clojure", "^compiler-method");
//module->module
static public Var USES = Module.intern("clojure", "^compiler-uses");
//ISeq FnExprs
static public Var FNS = Module.intern("clojure", "^compiler-fns");

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

private static final int MAX_POSITIONAL_ARITY = 20;

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

private static Expr analyze(C context, Object form) throws Exception {
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

private static Expr analyzeSeq(C context, ISeq form) throws Exception {
    Object op = RT.first(form);
    if(op == DEF)
        return analyzeDef(context, form);
    if(op == FN)
        return analyzeFn(context, form);
    else
        throw new UnsupportedOperationException();
}

private static Expr analyzeFn(C context, ISeq form) throws Exception {
    //(fn (args) body) or (fn ((args) body) ((args2) body2) ...)
    //turn former into latter
    if(!(RT.first(RT.second(form)) instanceof ISeq))
        return analyzeFn(context, RT.list(FN, RT.rest(form)));

    FnMethod[] methodArray = new FnMethod[MAX_POSITIONAL_ARITY+1];
    FnMethod variadicMethod = null;
    FnExpr fn = new FnExpr();
    for(ISeq s = RT.rest(form);s != null;s = RT.rest(s))
        {
        FnMethod f = analyzeMethod(fn,(ISeq) RT.first(s));
        if(f.restParm != null || f.keyParms != null)
            {
            if(variadicMethod == null)
                variadicMethod = f;
            else
                throw new Exception("Can't have more than 1 variadic overload");
            }
        else if(methodArray[f.reqParms.count()] == null)
            methodArray[f.reqParms.count()] = f;
        else
            throw new Exception("Can't have 2 overloads with same arity");
        }
    if(variadicMethod != null)
        {
        for(int i = variadicMethod.reqParms.count() + 1;i<=MAX_POSITIONAL_ARITY;i++)
            if(methodArray[i] != null)
                throw new Exception("Can't have fixed arity function with more params than variadic function");
        }

    IPersistentCollection methods = null;
    for(int i = 0;i<methodArray.length;i++)
        if(methodArray[i] != null)
            methods = RT.cons(methodArray[i], methods);
    if(variadicMethod != null)
        methods = RT.cons(variadicMethod, methods);

    fn.methods = methods;
    fn.isVariadic = variadicMethod != null;
    registerFn(fn);
    return fn;
}

static class FnExpr extends AnExpr{
    IPersistentCollection methods;
    boolean isVariadic;
    LocalBinding binding;
    String name = null;
    boolean isCalledDirectly = false;
    //localbinding->itself
    IPersistentMap closes = null;

    String getName(){
        if(name == null)
            {
            if(binding != null)
                name = "FN__" + binding.sym.name + "__" + RT.nextID();
            else
                name = "FN__" + RT.nextID();
            }
        return name;
    }

    public void emitExpression() throws Exception{
        format("(new ~A(", getName());
        for(ISeq s = RT.seq(closes);s!=null;s=s.rest())
            {
            LocalBinding b = (LocalBinding) ((MapEntry) s.first()).key();
            format("~A", b.getName());
            if (s.rest() != null)
                format(",");
            }
        format("))");
    }

    public void emitDeclaration() throws Exception {
        PersistentArrayList closesDecls = null;
        if(closes != null)
            {
            closesDecls = new PersistentArrayList(closes.count() * 2);
            for (ISeq s = RT.seq(closes); s != null; s = s.rest())
                {
                LocalBinding b = (LocalBinding) ((MapEntry) s.first()).key();
                closesDecls.cons(b.typeDeclaration());
                closesDecls.cons(b.getName());
                }
            }
        if(!willBeStaticMethod())
            {
            //emit class declaration
            format("static public class ~A extends ~A{~%",
                   getName(),
                   isVariadic ? "clojure.lang.RestFn" : "AFn");
            if(closes != null)
                {
                //emit members and ctor if closure
                format("~{~A ~A;~%~}", closesDecls);
                format("public ~A (~{~A ~A~^, ~}){~%", getName(), closesDecls);
                for (ISeq s = RT.seq(closes); s != null; s = s.rest())
                    {
                    LocalBinding b = (LocalBinding) ((MapEntry) s.first()).key();
                    format("this.~A = ~A;~%", b.getName(), b.getName());
                    if (s.rest() != null)
                        format(",");
                    }
                format("}~%");
                }
            }
        else
            {
            format("static public Object ~A(~{~A ~A~^, ~}",
s                   getName(),
                   closesDecls);
            }
    }

    boolean willBeStaticMethod() {
        return !isVariadic
               && methods.count() == 1
               &&
               (
                       isCalledDirectly
                       ||
                       (binding != null && !binding.isClosed && !binding.valueTaken)
               );
    }
}

static class FnMethod {
    FnMethod parent = null;
    //localbinding->localbinding
    IPersistentMap locals = null;
    //localbinding->localbinding
    PersistentArrayList reqParms = new PersistentArrayList(4);
    PersistentArrayList keyParms = null;
    LocalBindingExpr restParm = null;
    Expr body = null;
    FnExpr fn;

    public FnMethod(FnExpr fn,FnMethod parent) {
        this.parent = parent;
        this.fn = fn;
    }
}

enum PSTATE{REQ,REST,KEY,DONE}

private static FnMethod analyzeMethod(FnExpr fn,ISeq form) throws Exception {
    //((args) body)
    ISeq parms = (ISeq) RT.first(form);
    ISeq body = RT.rest(form);
    try
        {
        FnMethod method = new FnMethod(fn,(FnMethod) METHOD.getValue());
        METHOD.pushThreadBinding(method);
        LOCAL_ENV.pushThreadBinding(LOCAL_ENV.getValue());
        PSTATE state = PSTATE.REQ;
        for (ISeq ps = parms; ps != null; ps = ps.rest())
            {
            Object p = ps.first();
            if (p == _AM_REST)
                {
                if (state == PSTATE.REQ)
                    state = PSTATE.REST;
                else
                    throw new Exception("Invalid parameter list");
                }
            else if (p == _AM_KEY)
                {
                if (state == PSTATE.REQ)
                    {
                    state = PSTATE.KEY;
                    method.keyParms = new PersistentArrayList(4);
                    }
                else
                    throw new Exception("Invalid parameter list");
                }
            else
                {
                switch (state)
                    {
                    case REQ:
                        method.reqParms = method.reqParms.cons(createParamBinding((Symbol) p));
                        break;
                    case REST:
                        method.restParm = createParamBinding((Symbol) p);
                        state = PSTATE.DONE;
                        break;
                    case KEY:
                        if(p instanceof ISeq)
                            method.keyParms = method.keyParms.cons(
                                        new KeyParam(createParamBinding((Symbol) RT.first(p)),
                                            analyze(C.EXPRESSION, RT.second(p))));
                        else
                            method.keyParms = method.keyParms.cons(
                                        new KeyParam(createParamBinding((Symbol) p)));

                        break;
                    default:
                        throw new Exception("Unexpected parameter");
                    }
                }
            }
        if(method.reqParms.count() > MAX_POSITIONAL_ARITY)
            throw new Exception("Sorry, can't specify more than " + MAX_POSITIONAL_ARITY + " params");
        method.body = analyze(C.RETURN, RT.cons(DO, body));
        return method;
        }
    finally{
        METHOD.popThreadBinding();
        LOCAL_ENV.popThreadBinding();
    }
}



static LocalBindingExpr createParamBinding(Symbol p) {
    Symbol basep = baseSymbol(p);
    LocalBinding b = new LocalBinding(basep);
    b.isParam = true;
    String typeHint = typeHint(p);
    b.typeHint = typeHint;
    registerLocal(b);
    return new LocalBindingExpr(b, typeHint);
}

private static Expr analyzeDef(C context, ISeq form) throws Exception {
    //(def x) or (def x initexpr)
    Symbol sym = (Symbol) RT.second(form);
    Module module = (Module) MODULE.getValue();
    Var var = module.intern(baseSymbol(sym));
    registerVar(var);
    VarExpr ve = new VarExpr(var, typeHint(sym));
    Expr init = analyze(C.EXPRESSION, macroexpand(RT.third(form)));
    return new DefExpr(ve, init);
}

static Symbol baseSymbol(Symbol sym) {
    String base = baseName(sym);

    if(base == sym.name) //no typeHint
        return sym;

    return Symbol.intern(base);
}

static String baseName(Symbol sym){
    int slash = sym.name.indexOf('/');
    if(slash > 0)
       return sym.name.substring(0, slash);
    return sym.name;
}

static String typeHint(Symbol sym){
    int slash = sym.name.indexOf('/');
    if(slash > 0)
       return sym.name.substring(slash + 1);
    return null;
}

private static Expr analyzeSymbol(Symbol sym) throws Exception {
    if(sym instanceof Keyword)
        return registerKeyword((Keyword)sym);
    else if(sym instanceof HostSymbol)
        return new HostExpr((HostSymbol)sym);
    else
        {
        String typeHint = typeHint(sym);
        sym = baseSymbol(sym);
        LocalBinding b = referenceLocal(sym);
        if(b != null)
            return new LocalBindingExpr(b, typeHint);
        Var v = lookupVar(sym);
        if(v != null)
            return new VarExpr(v, typeHint);
        throw new Exception("Unable to resolve symbol: " + sym.name + " in this context");
        }
}

static Var lookupVar(Symbol sym){
    Module module = (Module) MODULE.getValue();
    Var v = module.find(sym);
    if(v != null)
        return v;
    for(ISeq seq = RT.seq(USES.getValue());seq != null;seq = RT.rest(seq))
        {
        module = (Module) ((MapEntry)RT.first(seq)).key();
        v = module.find(sym);
        if(v != null && !v.hidden)
            return v;
        }
    return null;
}

static Object macroexpand(Object x){
    return x; //placeholder
}

private static KeywordExpr registerKeyword(Keyword keyword) {
    IPersistentMap keywordsMap = (IPersistentMap) KEYWORDS.getValue();
    KeywordExpr ke = (KeywordExpr) RT.get(keyword,keywordsMap);
    if(ke == null)
        KEYWORDS.setValue(RT.assoc(keyword, ke = new KeywordExpr(keyword),keywordsMap));
    return ke;
}

private static void registerVar(Var var) {
    IPersistentMap varsMap = (IPersistentMap) VARS.getValue();
    if(RT.get(var,varsMap) == null)
        VARS.setValue(RT.assoc(var, var, varsMap));
}

private static void registerFn(FnExpr fn) {
    FNS.setValue(RT.cons(fn, (ISeq)FNS.getValue()));
}

static void closeOver(LocalBinding b,FnMethod method){
    if(b != null && method != null && RT.get(b,method.locals) == null)
        {
        b.isClosed = true;
        method.fn.closes = (IPersistentMap)RT.assoc(b, b, method.fn.closes);
        closeOver(b,method.parent);
        }
}

static LocalBinding referenceLocal(Symbol sym) {
    LocalBinding b = (LocalBinding) RT.get(sym, LOCAL_ENV.getValue());
    closeOver(b,(FnMethod) METHOD.getValue());
    return b;
}

private static void registerLocal(LocalBinding b) {
    IPersistentMap localsMap = (IPersistentMap) LOCAL_ENV.getValue();
    LOCAL_ENV.setValue(RT.assoc(b.sym, b, localsMap));
    FnMethod method = (FnMethod) METHOD.getValue();
    method.locals = (IPersistentMap) RT.assoc(b, b, method.locals);
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



static class KeyParam{
    public KeyParam(LocalBindingExpr b, Expr init) {
        this.b = b;
        this.init = init;
    }

    public KeyParam(LocalBindingExpr b) {
        this.b = b;
        this.init = NIL_EXPR;
    }

    LocalBindingExpr b;
    Expr init;
}


static class NilExpr extends AnExpr{
    public void emitExpression() throws Exception{
        format("null");
    }
}

static class LiteralExpr extends AnExpr{
    final Object val;

    public LiteralExpr(Object val){
        this.val = val;
    }

    public void emitExpression() throws Exception{
        format("%S",val);
    }
}

static class CharExpr extends AnExpr{
    final Character val;

    public CharExpr(Character val){
        this.val = val;
    }

    public void emitExpression() throws Exception{
        format("'~A'",val);
    }
}


static class HostExpr extends AnExpr{
    final HostSymbol sym;

    public HostExpr(HostSymbol sym){
        this.sym = sym;
    }

    public void emitExpression() throws Exception{
        if(sym instanceof ClassSymbol)
            format("%A.class", resolveHostClassname(((ClassSymbol) sym).className));
    }
}
/*
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
*/
static class KeywordExpr extends AnExpr{
    final Symbol sym;

    public KeywordExpr(Symbol sym){
        this.sym = sym;
    }

    public void emitExpression() throws Exception {
        format("%A", munge(sym.name));
    }
}

static class LocalBinding{
    final Symbol sym;
    boolean isClosed = false;
    boolean isParam = false;
    final int id = RT.nextID();
    String typeHint;
    public boolean valueTaken = false;
    boolean isAssigned = false;


    public LocalBinding(Symbol sym) {
        this.sym = sym;
    }

    public String getName(){
        return munge(sym.name) + "__" + id;
    }

    boolean needsBox(){
        return isClosed && isAssigned;
    }

    String typeDeclaration(){
        if(needsBox())
            return "clojure.lang.Box";
        return "Object";
    }
}

static class LocalBindingExpr extends AnExpr{
    final LocalBinding b;
    final String typeHint;

    public LocalBindingExpr(LocalBinding b, String typeHint){
        this.b = b;
        this.typeHint = typeHint;
    }

    public void emitExpression() throws Exception{
        format("%A", b.getName());
    }
}

static class VarExpr extends AnExpr{
    final Var var;
    final String typeHint;

    public VarExpr(Var var, String typeHint){
        this.var = var;
        this.typeHint = typeHint;
    }

    public void emitExpression() throws Exception{
        format("%A", munge(var.toString()));
    }
}

static class DefExpr extends AnExpr{
    final VarExpr var;
    final Expr init;

    public DefExpr(VarExpr var, Expr init){
        this.var = var;
        this.init = init;
    }

    public void emitExpression() throws Exception{
        format("%A.bind(%A)", var.emitExpressionString(),init.emitExpressionString());
    }
}

}
//*/


