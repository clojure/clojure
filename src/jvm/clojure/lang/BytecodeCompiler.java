/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Aug 21, 2007 */

package clojure.lang;

public class BytecodeCompiler{

static Symbol DEF = Symbol.create("def");
static Symbol LOOP = Symbol.create("loop");
static Symbol RECUR = Symbol.create("recur");
static Symbol DOT = Symbol.create(".");
static Symbol IF = Symbol.create("if");
static Symbol LET = Symbol.create("let");
static Symbol DO = Symbol.create("do");
static Symbol FN = Symbol.create("fn");
static Symbol QUOTE = Symbol.create("quote");
static Symbol THISFN = Symbol.create("thisfn");
static Symbol CLASS = Symbol.create("class");

static Symbol IMPORT = Symbol.create("import");
static Symbol USE = Symbol.create("use");
static Symbol _AMP_ = Symbol.create("&");

private static final int MAX_POSITIONAL_ARITY = 20;

//symbol->localbinding
static public DynamicVar LOCAL_ENV = DynamicVar.create();

//vector<localbinding>
static public DynamicVar LOOP_LOCALS = DynamicVar.create();

//keyword->keywordexpr
static public DynamicVar KEYWORDS = DynamicVar.create();

//var->var
static public DynamicVar VARS = DynamicVar.create();

//FnFrame
static public DynamicVar METHOD = DynamicVar.create();

enum C{
	STATEMENT,  //value ignored
	EXPRESSION, //value required
	RETURN,      //tail position relative to enclosing recur frame
	EVAL
}

interface Expr{
	Object eval() throws Exception;
}

static class DefExpr implements Expr{
	final DynamicVar var;
	final Expr init;

	public DefExpr(DynamicVar var, Expr init){
		this.var = var;
		this.init = init;
	}

	public Object eval() throws Exception{
		return var.bindRoot(init.eval());
	}

	public static Expr parse(C context, ISeq form) throws Exception{
		//(def x) or (def x initexpr)
		if(form.count() > 3)
			throw new Exception("Too many arguments to def");
		else if(form.count() < 2)
			throw new Exception("Too few arguments to def");
		else if(!(RT.second(form) instanceof Symbol))
			throw new Exception("Second argument to def must be a Symbol");
		DynamicVar v = lookupVar((Symbol) RT.second(form));
		if(!v.sym.ns.equals(currentNS()))
			throw new Exception("Can't create defs outside of current ns");
		return new DefExpr(v, analyze(C.EXPRESSION, RT.third(form), v.sym.name));
	}
}

static class VarExpr implements Expr{
	final DynamicVar var;
	final Symbol tag;

	public VarExpr(DynamicVar var, Symbol tag){
		this.var = var;
		this.tag = tag;
	}

	public Object eval() throws Exception{
		return var.get();
	}
}

static class KeywordExpr implements Expr{
	final Keyword k;

	public KeywordExpr(Keyword k){
		this.k = k;
	}

	public Object eval() throws Exception{
		return k;
	}
}

static abstract class LiteralExpr implements Expr{
	abstract Object val();

	public Object eval(){
		return val();
	}
}

static class NilExpr extends LiteralExpr{
	Object val(){
		return null;
	}
}

static NilExpr NIL_EXPR = new NilExpr();


static class NumExpr extends LiteralExpr{
	final Num num;

	public NumExpr(Num num){
		this.num = num;
	}

	Object val(){
		return num;
	}
}

static class StringExpr extends LiteralExpr{
	final String str;

	public StringExpr(String str){
		this.str = str;
	}

	Object val(){
		return str;
	}
}

static class CharExpr extends LiteralExpr{
	final Character ch;

	public CharExpr(Character ch){
		this.ch = ch;
	}

	Object val(){
		return ch;
	}
}

static class IfExpr implements Expr{
	final Expr testExpr;
	final Expr thenExpr;
	final Expr elseExpr;


	public IfExpr(Expr testExpr, Expr thenExpr, Expr elseExpr){
		this.testExpr = testExpr;
		this.thenExpr = thenExpr;
		this.elseExpr = elseExpr;
	}

	public Object eval() throws Exception{
		if(testExpr.eval() != null)
			return thenExpr.eval();
		return elseExpr.eval();
	}

	public static Expr parse(C context, ISeq form) throws Exception{
		//(if test then) or (if test then else)
		if(form.count() > 4)
			throw new Exception("Too many arguments to if");
		else if(form.count() < 3)
			throw new Exception("Too few arguments to if");
		return new IfExpr(analyze(C.EXPRESSION, RT.second(form)),
		                  analyze(context, RT.third(form)),
		                  analyze(context, RT.fourth(form)));
	}
}

static public IPersistentMap CHAR_MAP =
		PersistentHashMap.create('-', "_DASH_",
		                         '.', "_DOT_",
		                         ':', "_COLON_",
		                         '+', "_PLUS_",
		                         '>', "_GT_",
		                         '<', "_LT_",
		                         '=', "_EQ_",
		                         '~', "_TILDE_",
		                         '!', "_BANG_",
		                         '@', "_CIRCA_",
		                         '#', "_SHARP_",
		                         '$', "_DOLLARSIGN_",
		                         '%', "_PERCENT_",
		                         '^', "_CARET_",
		                         '&', "_AMPERSAND_",
		                         '*', "_STAR_",
		                         '{', "_LBRACE_",
		                         '}', "_RBRACE_",
		                         '[', "_LBRACK_",
		                         ']', "_RBRACK_",
		                         '/', "_SLASH_",
		                         '\\', "_BSLASH_",
		                         '?', "_QMARK_");

static String munge(String name){
	StringBuilder sb = new StringBuilder();
	for(char c : name.toCharArray())
		{
		String sub = (String) CHAR_MAP.valAt(c);
		if(sub != null)
			sb.append(sub);
		else
			sb.append(c);
		}
	return sb.toString();
}

static class FnExpr implements Expr{
	IPersistentCollection methods;
	//if there is a variadic overload (there can only be one) it is stored here
	FnMethod variadicMethod = null;
	String name = null;
	//localbinding->itself
	IPersistentMap closes = null;
	IPersistentMap keywords = null;
	IPersistentMap vars = null;
	Class compiledClass;

	static Expr parse(C context, ISeq form, String name) throws Exception{
		FnExpr fn = new FnExpr();
		FnMethod enclosingMethod = (FnMethod) METHOD.get();
		String basename = enclosingMethod != null ?
		                  (enclosingMethod.fn.name + "$")
		                  : (munge(currentNS()) + ".");
		fn.name = basename + (name != null ?
		                      munge(name)
		                      : ("fn__" + RT.nextID()));
		try
			{
			DynamicVar.pushThreadBindings(
					RT.map(
							KEYWORDS, PersistentHashMap.EMPTY,
							VARS, PersistentHashMap.EMPTY));
			//(fn [args] body...) or (fn ([args] body...) ([args2] body2...) ...)
			//turn former into latter
			if(RT.second(form) instanceof IPersistentArray)
				form = RT.list(FN, RT.rest(form));

			FnMethod[] methodArray = new FnMethod[MAX_POSITIONAL_ARITY + 1];
			FnMethod variadicMethod = null;
			for(ISeq s = RT.rest(form); s != null; s = RT.rest(s))
				{
				FnMethod f = analyzeMethod(fn, (ISeq) RT.first(s));
				if(f.isVariadic())
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
				for(int i = variadicMethod.reqParms.count() + 1; i <= MAX_POSITIONAL_ARITY; i++)
					if(methodArray[i] != null)
						throw new Exception("Can't have fixed arity function with more params than variadic function");
				}

			IPersistentCollection methods = null;
			for(int i = 0; i < methodArray.length; i++)
				if(methodArray[i] != null)
					methods = RT.cons(methodArray[i], methods);
			if(variadicMethod != null)
				methods = RT.cons(variadicMethod, methods);

			fn.methods = methods;
			fn.variadicMethod = variadicMethod;
			fn.keywords = (IPersistentMap) KEYWORDS.get();
			fn.vars = (IPersistentMap) VARS.get();
			}
		finally
			{
			DynamicVar.popThreadBindings();
			}
		fn.compile();
		return fn;
	}

	boolean isVariadic(){
		return variadicMethod != null;
	}

	private void compile(){

		//create bytecode for a class
		//with name current_ns.defname[$letname]+
		//anonymous fns get names fn__id
		//derived from AFn/RestFn
		//with static fields for keywords and vars
		//with instance fields for closed-overs
		//with a ctor that takes closed-overs and inits fields
		//with an override of invoke/doInvoke for each method
	}

	public Object eval() throws Exception{
		return compiledClass.newInstance();
	}
}

enum PSTATE{
	REQ, REST, DONE
}

private static FnMethod analyzeMethod(FnExpr fn, ISeq form) throws Exception{
	//([args] body...)
	ISeq parms = (ISeq) RT.first(form);
	ISeq body = RT.rest(form);
	try
		{
		FnMethod method = new FnMethod(fn, (FnMethod) METHOD.get());
		//register as the current method and set up a new env frame
		DynamicVar.pushThreadBindings(
				RT.map(
						METHOD, method,
						LOCAL_ENV, LOCAL_ENV.get(),
						LOOP_LOCALS, null));
		PSTATE state = PSTATE.REQ;
		PersistentVector loopLocals = PersistentVector.EMPTY;
		for(ISeq ps = parms; ps != null; ps = ps.rest())
			{
			Object p = ps.first();
			if(p == _AMP_)
				{
				if(state == PSTATE.REQ)
					state = PSTATE.REST;
				else
					throw new Exception("Invalid parameter list");
				}

			else
				{
				switch(state)
					{
					case REQ:
						LocalBinding lb = createParamBinding((Symbol) p);
						loopLocals = loopLocals.cons(lb);
						method.reqParms = method.reqParms.cons(lb);
						break;
					case REST:
						method.restParm = createParamBinding((Symbol) p);
						state = PSTATE.DONE;
						break;

					default:
						throw new Exception("Unexpected parameter");
					}
				}
			}
		if(method.reqParms.count() > MAX_POSITIONAL_ARITY)
			throw new Exception("Can't specify more than " + MAX_POSITIONAL_ARITY + " params");
		//only set loop locals if non-variadic
		if(method.restParm == null)
			LOOP_LOCALS.set(loopLocals);
		method.body = BodyExpr.parse(C.RETURN, body);
		return method;
		}
	finally
		{
		DynamicVar.popThreadBindings();
		}
}

static LocalBinding createParamBinding(Symbol p) throws Exception{
	LocalBinding b = new LocalBinding(p, tagOf(p));
	registerLocal(b);
	return b;
}

static class FnMethod{
	//when closures are defined inside other closures,
	//the closed over locals need to be propagated to the enclosing fn
	final FnMethod parent;
	//localbinding->localbinding
	IPersistentMap locals = null;
	//localbinding->localbinding
	PersistentVector reqParms = PersistentVector.EMPTY;
	PersistentVector keyParms = null;
	LocalBinding restParm = null;
	Expr body = null;
	FnExpr fn;

	public FnMethod(FnExpr fn, FnMethod parent){
		this.parent = parent;
		this.fn = fn;
	}

	boolean isVariadic(){
		return keyParms != null || restParm != null;
	}
}

static class LocalBinding{
	final Symbol sym;
	final Symbol tag;

	public LocalBinding(Symbol sym, Symbol tag){
		this.sym = sym;
		this.tag = tag;
	}
}

static class LocalBindingExpr implements Expr{
	final LocalBinding b;
	final Symbol tag;

	public LocalBindingExpr(LocalBinding b, Symbol tag){
		this.b = b;
		this.tag = tag;
	}

	public Object eval() throws Exception{
		throw new UnsupportedOperationException("Can't eval locals");
	}
}

static class BodyExpr implements Expr{
	PersistentVector exprs;

	public BodyExpr(PersistentVector exprs){
		this.exprs = exprs;
	}

	static Expr parse(C context, ISeq forms) throws Exception{
		PersistentVector exprs = PersistentVector.EMPTY;
		for(; forms != null; forms = forms.rest())
			{
			Expr e = (context == C.STATEMENT || forms.rest() != null) ?
			         analyze(C.STATEMENT, forms.first())
			         :
			         analyze(context, forms.first());
			exprs = exprs.cons(e);
			}
		return new BodyExpr(exprs);
	}

	public Object eval() throws Exception{
		Object ret = null;
		for(Object o : exprs)
			{
			Expr e = (Expr) o;
			ret = e.eval();
			}
		return ret;
	}
}

static class BindingInit{
	LocalBinding binding;
	Expr init;

	public BindingInit(LocalBinding binding, Expr init){
		this.binding = binding;
		this.init = init;
	}
}

static class LetExpr implements Expr{
	final PersistentVector bindingInits;
	final Expr body;
	final boolean isLoop;

	public LetExpr(PersistentVector bindingInits, Expr body, boolean isLoop){
		this.bindingInits = bindingInits;
		this.body = body;
		this.isLoop = isLoop;
	}

	static Expr parse(C context, ISeq form, boolean isLoop) throws Exception{
		//(let [var val var2 val2 ...] body...)
		ISeq bindings = (ISeq) RT.second(form);

		ISeq body = RT.rest(RT.rest(form));

		if(context == C.EVAL)
			return analyze(context, RT.list(RT.list(FN, PersistentVector.EMPTY, form)));

		IPersistentMap dynamicBindings = RT.map(LOCAL_ENV, LOCAL_ENV.get());
		if(isLoop)
			dynamicBindings = dynamicBindings.assoc(LOOP_LOCALS, null);

		try
			{
			DynamicVar.pushThreadBindings(dynamicBindings);

			PersistentVector bindingInits = PersistentVector.EMPTY;
			PersistentVector loopLocals = PersistentVector.EMPTY;
			for(ISeq bs = bindings; bs != null; bs = RT.rest(RT.rest(bs)))
				{
				if(!(RT.first(bs) instanceof Symbol))
					throw new IllegalArgumentException("Bad binding form, expected symbol, got: " + RT.first(bs));
				Symbol sym = (Symbol) RT.first(bs);
				if(bs.rest() == null)
					throw new IllegalArgumentException("Bad binding form, expected expression following: " + sym);
				LocalBinding lb = new LocalBinding(sym, tagOf(sym));
				BindingInit bi = new BindingInit(lb, analyze(C.EXPRESSION, RT.second(bs), sym.name));
				bindingInits = bindingInits.cons(bi);

				//sequential enhancement of env
				registerLocal(lb);

				if(isLoop)
					loopLocals = loopLocals.cons(lb);
				}
			if(isLoop)
				LOOP_LOCALS.set(loopLocals);
			return new LetExpr(bindingInits, BodyExpr.parse(isLoop ? C.RETURN : context, body), isLoop);
			}
		finally
			{
			DynamicVar.popThreadBindings();
			}
	}

	public Object eval() throws Exception{
		throw new UnsupportedOperationException("Can't eval let");
	}
}

private static void registerLocal(LocalBinding b) throws Exception{
	IPersistentMap localsMap = (IPersistentMap) LOCAL_ENV.get();
	LOCAL_ENV.set(RT.assoc(b.sym, b, localsMap));
	FnMethod method = (FnMethod) METHOD.get();
	method.locals = (IPersistentMap) RT.assoc(method.locals, b, b);
}

private static Expr analyze(C context, Object form) throws Exception{
	return analyze(context, form, null);
}

private static Expr analyze(C context, Object form, String name) throws Exception{
	//todo macro expansion
	if(form == null)
		return NIL_EXPR;
	Class fclass = form.getClass();
	if(fclass == Symbol.class)
		return analyzeSymbol((Symbol) form);
	else if(fclass == Keyword.class)
		return registerKeyword((Keyword) form);
	else if(form instanceof Num)
		return new NumExpr((Num) form);
	else if(fclass == String.class)
		return new StringExpr((String) form);
	else if(fclass == Character.class)
		return new CharExpr((Character) form);
	else if(form instanceof ISeq)
		return analyzeSeq(context, (ISeq) form, name);

//	else
	throw new UnsupportedOperationException();
}

private static Expr analyzeSeq(C context, ISeq form, String name) throws Exception{
	Object op = RT.first(form);
	if(op == DEF)
		return DefExpr.parse(context, form);
	else if(op == IF)
		return IfExpr.parse(context, form);
	else if(op == FN)
		return FnExpr.parse(context, form, name);
	else if(op == DO)
		return BodyExpr.parse(context, form.rest());
	else if(op == LET)
		return LetExpr.parse(context, form, false);
	else if(op == LOOP)
		return LetExpr.parse(context, form, true);
}

Object eval(Object form) throws Exception{
	Expr expr = analyze(C.EXPRESSION, form);
	return expr.eval();
}

private static KeywordExpr registerKeyword(Keyword keyword){
	IPersistentMap keywordsMap = (IPersistentMap) KEYWORDS.get();
	if(keywordsMap == null) //not bound, no fn context
		return new KeywordExpr(keyword);
	KeywordExpr ke = (KeywordExpr) RT.get(keyword, keywordsMap);
	if(ke == null)
		KEYWORDS.set(RT.assoc(keyword, ke = new KeywordExpr(keyword), keywordsMap));
	return ke;
}

private static Expr analyzeSymbol(Symbol sym) throws Exception{
	Symbol tag = tagOf(sym);
	if(sym.ns == null) //ns-qualified syms are always Vars
		{
		LocalBinding b = referenceLocal(sym);
		if(b != null)
			return new LocalBindingExpr(b, tag);
		}
	DynamicVar v = lookupVar(sym);
	if(v != null)
		return new VarExpr(v, tag);
	throw new Exception("Unable to resolve symbol: " + sym + " in this context");

}

static DynamicVar lookupVar(Symbol sym) throws Exception{
	DynamicVar var = null;

	//note - ns-qualified vars must already exist
	if(sym.ns != null)
		{
		var = DynamicVar.find(sym);
		}
	else
		{
		//is it an alias?
		IPersistentMap uses = (IPersistentMap) RT.USES.get();
		var = (DynamicVar) uses.valAt(sym);
		if(var == null)
			{
			//introduce a new var in the current ns
			String ns = currentNS();
			var = DynamicVar.intern(Symbol.intern(ns, sym.name));
			}
		}
	if(var != null)
		registerVar(var);
	return var;
}

private static void registerVar(DynamicVar var) throws Exception{
	IPersistentMap varsMap = (IPersistentMap) VARS.get();
	if(varsMap != null && RT.get(var, varsMap) == null)
		VARS.set(RT.assoc(varsMap, var, var));
}

private static String currentNS(){
	return (String) RT.CURRENT_NS.get();
}

static void closeOver(LocalBinding b, FnMethod method){
	if(b != null && method != null && RT.get(b, method.locals) == null)
		{
		method.fn.closes = (IPersistentMap) RT.assoc(method.fn.closes, b, b);
		closeOver(b, method.parent);
		}
}


static LocalBinding referenceLocal(Symbol sym) throws Exception{
	LocalBinding b = (LocalBinding) RT.get(sym, LOCAL_ENV.get());
	if(b != null)
		{
		closeOver(b, (FnMethod) METHOD.get());
		}
	return b;
}

private static Symbol tagOf(Symbol sym){
	if(sym.meta() != null)
		return (Symbol) sym.meta().valAt(RT.TAG_KEY);
	return null;
}

}
