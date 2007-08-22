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
static Symbol NOT = Symbol.create("not");

static Symbol IMPORT = Symbol.create("import");
static Symbol USE = Symbol.create("use");
static Symbol _AMP_KEY = Symbol.create("&key");
static Symbol _AMP_REST = Symbol.create("&rest");

//symbol->localbinding
static public DynamicVar LOCAL_ENV = DynamicVar.create();


//FnFrame
static public DynamicVar METHOD = DynamicVar.create();

enum C{
	STATEMENT,  //value ignored
	EXPRESSION, //value required
	RETURN      //tail position relative to enclosing recur frame
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
}

static class VarExpr implements Expr{
	final DynamicVar var;
	Symbol tag;

	public VarExpr(DynamicVar var, Symbol tag){
		this.var = var;
		this.tag = tag;
	}

	public Object eval() throws Exception{
		return var.get();
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
	Num num;

	public NumExpr(Num num){
		this.num = num;
	}

	Object val(){
		return num;
	}
}

static class StringExpr extends LiteralExpr{
	String str;

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

static class FnExpr implements Expr{
	IPersistentCollection methods;
	//if there is a variadic overload (there can only be one) it is stored here
	FnMethod variadicMethod = null;
	//acts both as a flag to indicate a let-bound fn
	//and a path to the binding in order to detect if its value is ever taken
	LocalBinding binding;
	String name = null;
	boolean isCalledDirectly = false;
	//localbinding->itself
	IPersistentMap closes = null;

	boolean willBeStaticMethod(){
		return variadicMethod == null
		       && methods.count() == 1
		       &&
		       (
				       isCalledDirectly
				       ||
				       (binding != null && !binding.valueTaken)
		       );
	}

	public Object eval() throws Exception{
		//todo - implement
		//ask the DynamicClassLoader (found through Var?) to load our class
		//create instance through Class object newInstance()
		return null;
	}
}

static class FnMethod{
	//when closures are defined inside other closures,
	//the closed over locals need to be propagated to the enclosing fn
	FnMethod parent = null;
	//localbinding->localbinding
	IPersistentMap locals = null;
	//localbinding->localbinding
	PersistentVector reqParms = PersistentVector.EMPTY;
	PersistentVector keyParms = null;
	LocalBindingExpr restParm = null;
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
	boolean isClosed = false;
	Symbol tag;
	public boolean valueTaken = false;
	FnExpr letfn = null;


	public LocalBinding(Symbol sym, Symbol tag){
		this.sym = sym;
		this.tag = tag;
	}

	boolean bindsToStaticFn(){
		return letfn != null && letfn.willBeStaticMethod();
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

private static Expr analyze(C context, Object form) throws Exception{
	return analyze(context, form, null);
}

private static Expr analyze(C context, Object form, String name) throws Exception{
	if(form == null)
		return NIL_EXPR;
	else if(form instanceof Num)
		return new NumExpr((Num) form);
	else if(form instanceof String)
		return new StringExpr((String) form);
	else if(form instanceof Character)
		return new CharExpr((Character) form);
	else if(form instanceof Symbol)
		return analyzeSymbol((Symbol) form, false);
//	else if(form instanceof ISeq)
//		return analyzeSeq(context, (ISeq) form);

//	else
	throw new UnsupportedOperationException();
}

Object eval(Object form) throws Exception{
	Expr expr = analyze(C.EXPRESSION, form);
	return expr.eval();
}

private static Expr analyzeSymbol(Symbol sym, boolean inFnPosition) throws Exception{
	Symbol tag = tagOf(sym);
	if(sym.ns == null) //ns-qualified syms are always Vars
		{
		LocalBinding b = referenceLocal(sym);
		if(b != null)
			{
			if(!inFnPosition)
				b.valueTaken = true;
			return new LocalBindingExpr(b, tag);
			}
		}
	DynamicVar v = lookupVar(sym);
	if(v != null)
		return new VarExpr(v, tag);
	throw new Exception("Unable to resolve symbol: " + sym.name + " in this context");

}

static DynamicVar lookupVar(Symbol sym) throws Exception{
	//qualified vars must already exist
	if(sym.ns != null)
		return DynamicVar.find(sym);
	IPersistentMap uses = (IPersistentMap) RT.USES.get();
	DynamicVar var = (DynamicVar) uses.valAt(sym);
	if(var != null)
		return var;
	String ns = (String) RT.CURRENT_NS.get();
	return DynamicVar.intern(Symbol.intern(ns, sym.name));
}

static void closeOver(LocalBinding b, FnMethod method){
	if(b != null && method != null && RT.get(b, method.locals) == null)
		{
		b.isClosed = true;
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
