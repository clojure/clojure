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

import org.objectweb.asm.Opcodes;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.Type;
import org.objectweb.asm.commons.Method;
import org.objectweb.asm.commons.GeneratorAdapter;
import org.objectweb.asm.util.TraceClassVisitor;

import java.io.PrintWriter;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;

public class BytecodeCompiler implements Opcodes{

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
static Symbol IFN = Symbol.create("clojure.lang", "IFn");
static Symbol CLASS = Symbol.create("class");

static Symbol IMPORT = Symbol.create("import");
static Symbol USE = Symbol.create("use");
static Symbol _AMP_ = Symbol.create("&");

private static final int MAX_POSITIONAL_ARITY = 20;
private static Type OBJECT_TYPE;
private static Type KEYWORD_TYPE = Type.getType(Keyword.class);
private static Type VAR_TYPE = Type.getType(Var.class);

private static Type[][] ARG_TYPES;

static
	{
	OBJECT_TYPE = Type.getType(Object.class);
	ARG_TYPES = new Type[MAX_POSITIONAL_ARITY][];
	for(int i = 0; i < MAX_POSITIONAL_ARITY; ++i)
		{
		Type[] a = new Type[i];
		for(int j = 0; j < i; j++)
			a[j] = OBJECT_TYPE;
		ARG_TYPES[i] = a;
		}
	}


//symbol->localbinding
static public Var LOCAL_ENV = Var.create(null);

//vector<localbinding>
static public Var LOOP_LOCALS = Var.create();

//keyword->keywordexpr
static public Var KEYWORDS = Var.create();

//var->var
static public Var VARS = Var.create();

//FnFrame
static public Var METHOD = Var.create(null);

//String
static public Var SOURCE = Var.create(null);

//Integer
static public Var NEXT_LOCAL_NUM = Var.create(0);

//DynamicClassLoader
static public Var LOADER = Var.create();

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
	final Var var;
	final Expr init;

	public DefExpr(Var var, Expr init){
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
		Var v = lookupVar((Symbol) RT.second(form));
		if(!v.sym.ns.equals(currentNS()))
			throw new Exception("Can't create defs outside of current ns");
		return new DefExpr(v, analyze(C.EXPRESSION, RT.third(form), v.sym.name));
	}
}

static class VarExpr implements Expr{
	final Var var;
	final Symbol tag;

	public VarExpr(Var var, Symbol tag){
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
		PersistentHashMap.create('-', "_",
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
	String name;
	String internalName;
	//localbinding->itself
	IPersistentMap closes = PersistentHashMap.EMPTY;
	//KeywordExpr->iteself
	IPersistentMap keywords = PersistentHashMap.EMPTY;
	IPersistentMap vars = PersistentHashMap.EMPTY;
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
		fn.internalName = fn.name.replace('.', '/');
		try
			{
			Var.pushThreadBindings(
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
				FnMethod f = FnMethod.parse(fn, (ISeq) RT.first(s));
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
			Var.popThreadBindings();
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
		ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_MAXS);
		//ClassVisitor cv = cw;
		ClassVisitor cv = new TraceClassVisitor(cw, new PrintWriter(System.out));
		cv.visit(V1_5, ACC_PUBLIC, internalName, null, isVariadic() ? "clojure/lang/RestFn" : "clojure/lang/AFn", null);
		String source = (String) SOURCE.get();
		if(source != null)
			cv.visitSource(source, null);
		//todo static fields for keywords and vars
		for(ISeq s = RT.keys(keywords); s != null; s = s.rest())
			{
			KeywordExpr k = (KeywordExpr) s.first();
			cv.visitField(ACC_PUBLIC + ACC_FINAL + ACC_STATIC, munge(k.k.sym.toString()),
			              OBJECT_TYPE.getDescriptor(), null, null);
			}
		//todo static init for keywords and vars
		//instance fields for closed-overs
		for(ISeq s = RT.keys(closes); s != null; s = s.rest())
			{
			LocalBinding lb = (LocalBinding) s.first();
			cv.visitField(ACC_PUBLIC + ACC_FINAL, lb.name, OBJECT_TYPE.getDescriptor(), null, null);
			}
		//ctor that takes closed-overs and inits base + fields
		Method m = new Method("<init>", Type.VOID_TYPE, ARG_TYPES[closes.count()]);
		GeneratorAdapter mg = new GeneratorAdapter(ACC_PUBLIC,
		                                           m,
		                                           null,
		                                           null,
		                                           cw);
		mg.loadThis();
		if(isVariadic()) //RestFn ctor takes reqArity arg
			mg.push(variadicMethod.reqParms.count());
		mg.invokeConstructor(Type.getType(isVariadic() ? RestFn.class : AFn.class), m);
		int a = 1;
		for(ISeq s = RT.keys(closes); s != null; s = s.rest(), ++a)
			{
			LocalBinding lb = (LocalBinding) s.first();
			mg.loadLocal(a);
			mg.putField(Type.getObjectType(internalName), lb.name, OBJECT_TYPE);
			}
		mg.returnValue();
		mg.endMethod();

		//todo override of invoke/doInvoke for each method
		cv.visitEnd();

		//define class and store
		DynamicClassLoader loader = (DynamicClassLoader) LOADER.get();
		compiledClass = loader.defineClass(name, cw.toByteArray());
	}

	public Object eval() throws Exception{
		return compiledClass.newInstance();
	}
}

enum PSTATE{
	REQ, REST, DONE
}


static class FnMethod{
	//when closures are defined inside other closures,
	//the closed over locals need to be propagated to the enclosing fn
	final FnMethod parent;
	//localbinding->localbinding
	IPersistentMap locals = null;
	//localbinding->localbinding
	PersistentVector reqParms = PersistentVector.EMPTY;
	LocalBinding restParm = null;
	Expr body = null;
	FnExpr fn;

	public FnMethod(FnExpr fn, FnMethod parent){
		this.parent = parent;
		this.fn = fn;
	}

	boolean isVariadic(){
		return restParm != null;
	}

	int numParams(){
		return reqParms.count() + (isVariadic() ? 1 : 0);
	}

	private static FnMethod parse(FnExpr fn, ISeq form) throws Exception{
		//([args] body...)
		IPersistentArray parms = (IPersistentArray) RT.first(form);
		ISeq body = RT.rest(form);
		try
			{
			FnMethod method = new FnMethod(fn, (FnMethod) METHOD.get());
			//register as the current method and set up a new env frame
			Var.pushThreadBindings(
					RT.map(
							METHOD, method,
							LOCAL_ENV, LOCAL_ENV.get(),
							LOOP_LOCALS, null,
							NEXT_LOCAL_NUM, 0));

			//register 'this' as local 0
			registerLocal(THISFN, null);

			PSTATE state = PSTATE.REQ;
			PersistentVector loopLocals = PersistentVector.EMPTY;
			for(int i = 0; i < parms.count(); i++)
				{
				if(!(parms.nth(i) instanceof Symbol))
					throw new IllegalArgumentException("fn params must be Symbols");
				Symbol p = (Symbol) parms.nth(i);
				if(p.equals(_AMP_))
					{
					if(state == PSTATE.REQ)
						state = PSTATE.REST;
					else
						throw new Exception("Invalid parameter list");
					}

				else
					{
					LocalBinding lb = registerLocal(p, tagOf(p));
					loopLocals = loopLocals.cons(lb);
					switch(state)
						{
						case REQ:
							method.reqParms = method.reqParms.cons(lb);
							break;
						case REST:
							method.restParm = lb;
							state = PSTATE.DONE;
							break;

						default:
							throw new Exception("Unexpected parameter");
						}
					}
				}
			if(method.reqParms.count() > MAX_POSITIONAL_ARITY)
				throw new Exception("Can't specify more than " + MAX_POSITIONAL_ARITY + " params");
			LOOP_LOCALS.set(loopLocals);
			method.body = BodyExpr.parse(C.RETURN, body);
			return method;
			}
		finally
			{
			Var.popThreadBindings();
			}
	}
}

static class LocalBinding{
	final Symbol sym;
	final Symbol tag;
	final int num;
	final String name;

	public LocalBinding(int num, Symbol sym, Symbol tag){
		this.num = num;
		this.sym = sym;
		this.tag = tag;
		name = munge(sym.name);
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
		if(!(RT.second(form) instanceof IPersistentArray))
			throw new IllegalArgumentException("Bad binding form, expected vector");

		IPersistentArray bindings = (IPersistentArray) RT.second(form);
		if((bindings.count() % 2) != 0)
			throw new IllegalArgumentException("Bad binding form, expected matched symbol expression pairs");

		ISeq body = RT.rest(RT.rest(form));

		if(context == C.EVAL)
			return analyze(context, RT.list(RT.list(FN, PersistentVector.EMPTY, form)));

		IPersistentMap dynamicBindings = RT.map(LOCAL_ENV, LOCAL_ENV.get(),
		                                        NEXT_LOCAL_NUM, NEXT_LOCAL_NUM.get());
		if(isLoop)
			dynamicBindings = dynamicBindings.assoc(LOOP_LOCALS, null);

		try
			{
			Var.pushThreadBindings(dynamicBindings);

			PersistentVector bindingInits = PersistentVector.EMPTY;
			PersistentVector loopLocals = PersistentVector.EMPTY;
			for(int i = 0; i < bindings.count(); i += 2)
				{
				if(!(bindings.nth(i) instanceof Symbol))
					throw new IllegalArgumentException("Bad binding form, expected symbol, got: " + bindings.nth(i));
				Symbol sym = (Symbol) bindings.nth(i);

				Expr init = analyze(C.EXPRESSION, bindings.nth(i + 1), sym.name);
				//sequential enhancement of env (like Lisp let*)
				LocalBinding lb = registerLocal(sym, tagOf(sym));
				BindingInit bi = new BindingInit(lb, init);
				bindingInits = bindingInits.cons(bi);

				if(isLoop)
					loopLocals = loopLocals.cons(lb);
				}
			if(isLoop)
				LOOP_LOCALS.set(loopLocals);
			return new LetExpr(bindingInits, BodyExpr.parse(isLoop ? C.RETURN : context, body), isLoop);
			}
		finally
			{
			Var.popThreadBindings();
			}
	}

	public Object eval() throws Exception{
		throw new UnsupportedOperationException("Can't eval let/loop");
	}
}

private static LocalBinding registerLocal(Symbol sym, Symbol tag) throws Exception{
	int num = ((Number) NEXT_LOCAL_NUM.get()).intValue();
	LocalBinding b = new LocalBinding(num, sym, tag);
	NEXT_LOCAL_NUM.set(num + 1);
	IPersistentMap localsMap = (IPersistentMap) LOCAL_ENV.get();
	LOCAL_ENV.set(RT.assoc(localsMap, b.sym, b));
	FnMethod method = (FnMethod) METHOD.get();
	method.locals = (IPersistentMap) RT.assoc(method.locals, b, b);
	return b;
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
	if(op.equals(DEF))
		return DefExpr.parse(context, form);
	else if(op.equals(IF))
		return IfExpr.parse(context, form);
	else if(op.equals(FN))
		return FnExpr.parse(context, form, name);
	else if(op.equals(DO))
		return BodyExpr.parse(context, form.rest());
	else if(op.equals(LET))
		return LetExpr.parse(context, form, false);
	else if(op.equals(LOOP))
		return LetExpr.parse(context, form, true);

	throw new UnsupportedOperationException();
}

static Object eval(Object form) throws Exception{
	Expr expr = analyze(C.EXPRESSION, form);
	return expr.eval();
}

private static KeywordExpr registerKeyword(Keyword keyword){
	if(!KEYWORDS.isBound())
		return new KeywordExpr(keyword);

	IPersistentMap keywordsMap = (IPersistentMap) KEYWORDS.get();
	KeywordExpr ke = (KeywordExpr) RT.get(keywordsMap, keyword);
	if(ke == null)
		KEYWORDS.set(RT.assoc(keywordsMap, keyword, ke = new KeywordExpr(keyword)));
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
	Var v = lookupVar(sym);
	if(v != null)
		return new VarExpr(v, tag);
	throw new Exception("Unable to resolve symbol: " + sym + " in this context");

}

static Var lookupVar(Symbol sym) throws Exception{
	Var var = null;

	//note - ns-qualified vars must already exist
	if(sym.ns != null)
		{
		var = Var.find(sym);
		}
	else
		{
		//is it an alias?
		IPersistentMap uses = (IPersistentMap) RT.USES.get();
		var = (Var) uses.valAt(sym);
		if(var == null)
			{
			//introduce a new var in the current ns
			String ns = currentNS();
			var = Var.intern(Symbol.intern(ns, sym.name));
			}
		}
	if(var != null)
		registerVar(var);
	return var;
}

private static void registerVar(Var var) throws Exception{
	if(!VARS.isBound())
		return;
	IPersistentMap varsMap = (IPersistentMap) VARS.get();
	if(varsMap != null && RT.get(varsMap, var) == null)
		VARS.set(RT.assoc(varsMap, var, var));
}

private static String currentNS(){
	return (String) RT.CURRENT_NS.get();
}

static void closeOver(LocalBinding b, FnMethod method){
	if(b != null && method != null && RT.get(method.locals, b) == null)
		{
		method.fn.closes = (IPersistentMap) RT.assoc(method.fn.closes, b, b);
		closeOver(b, method.parent);
		}
}


static LocalBinding referenceLocal(Symbol sym) throws Exception{
	if(!LOCAL_ENV.isBound())
		return null;
	LocalBinding b = (LocalBinding) RT.get(LOCAL_ENV.get(), sym);
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


public static void main(String[] args){
	//repl
	LineNumberingPushbackReader rdr = new LineNumberingPushbackReader(new InputStreamReader(System.in));
	OutputStreamWriter w = new OutputStreamWriter(System.out);
	Object EOF = new Object();
	for(; ;)
		{
		try
			{
			Var.pushThreadBindings(
					RT.map(LOADER, new DynamicClassLoader()));
			Object r = LispReader.read(rdr, false, EOF, false);
			if(r == EOF)
				break;
			Object ret = eval(r);
			RT.print(ret, w);
			w.write('\n');
			w.flush();
			}
		catch(Exception e)
			{
			Var.popThreadBindings();
			e.printStackTrace();
			}
		}
}
}
