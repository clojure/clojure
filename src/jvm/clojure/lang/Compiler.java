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

import clojure.asm.*;
import clojure.asm.commons.Method;
import clojure.asm.commons.GeneratorAdapter;
//import org.objectweb.asm.util.TraceClassVisitor;
//import org.objectweb.asm.util.CheckClassAdapter;

import java.io.*;
import java.math.BigInteger;
import java.util.List;
import java.util.ArrayList;
import java.lang.reflect.Constructor;

public class Compiler implements Opcodes{

static final Symbol DEF = Symbol.create("def");
static final Symbol LOOP = Symbol.create("loop");
static final Symbol RECUR = Symbol.create("recur");
static final Symbol IF = Symbol.create("if");
static final Symbol LET = Symbol.create("let");
static final Symbol DO = Symbol.create("do");
static final Symbol FN = Symbol.create("fn");
static final Symbol QUOTE = Symbol.create("quote");
static final Symbol THE_VAR = Symbol.create("the-var");
static final Symbol DOT = Symbol.create(".");
static final Symbol ASSIGN = Symbol.create("set!");
static final Symbol TRY_FINALLY = Symbol.create("try-finally");
static final Symbol THROW = Symbol.create("throw");
static final Symbol MONITOR_ENTER = Symbol.create("monitor-enter");
static final Symbol MONITOR_EXIT = Symbol.create("monitor-exit");
static final Symbol INSTANCE = Symbol.create("instance?");

static final Symbol THISFN = Symbol.create("thisfn");
static final Symbol CLASS = Symbol.create("class");
static final Symbol NEW = Symbol.create("new");
//static final Symbol UNQUOTE = Symbol.create("unquote");
//static final Symbol UNQUOTE_SPLICING = Symbol.create("unquote-splicing");
//static final Symbol SYNTAX_QUOTE = Symbol.create("clojure", "syntax-quote");
static final Symbol LIST = Symbol.create("clojure", "list");
static final Symbol HASHMAP = Symbol.create("clojure", "hash-map");
static final Symbol VECTOR = Symbol.create("clojure", "vector");

static final Symbol _AMP_ = Symbol.create("&");

//static final Symbol IMPORT = Symbol.create("import");
//static final Symbol USE = Symbol.create("use");

//static final Symbol IFN = Symbol.create("clojure.lang", "IFn");

static IPersistentMap specials = RT.map(
		DEF, new DefExpr.Parser(),
		LOOP, new LetExpr.Parser(),
		RECUR, new RecurExpr.Parser(),
		IF, new IfExpr.Parser(),
		LET, new LetExpr.Parser(),
		DO, new BodyExpr.Parser(),
		FN, null,
		QUOTE, new QuoteExpr.Parser(),
		THE_VAR, new TheVarExpr.Parser(),
		DOT, new HostExpr.Parser(),
		ASSIGN, new AssignExpr.Parser(),
		TRY_FINALLY, new TryFinallyExpr.Parser(),
		THROW, new ThrowExpr.Parser(),
		MONITOR_ENTER, new MonitorEnterExpr.Parser(),
		MONITOR_EXIT, new MonitorExitExpr.Parser(),
		INSTANCE, new InstanceExpr.Parser(),
		THISFN, null,
		CLASS, new ClassExpr.Parser(),
		NEW, new NewExpr.Parser(),
//		UNQUOTE, null,
//		UNQUOTE_SPLICING, null,
//		SYNTAX_QUOTE, null,
_AMP_, null
);

private static final int MAX_POSITIONAL_ARITY = 20;
private static final Type OBJECT_TYPE;
private static final Type KEYWORD_TYPE = Type.getType(Keyword.class);
private static final Type VAR_TYPE = Type.getType(Var.class);
private static final Type SYMBOL_TYPE = Type.getType(Symbol.class);
private static final Type NUM_TYPE = Type.getType(Num.class);
private static final Type IFN_TYPE = Type.getType(IFn.class);
private static final Type RT_TYPE = Type.getType(RT.class);
final static Type CLASS_TYPE = Type.getType(Class.class);
final static Type REFLECTOR_TYPE = Type.getType(Reflector.class);
final static Type THROWABLE_TYPE = Type.getType(Throwable.class);

private static final Type[][] ARG_TYPES;
private static final Type[] EXCEPTION_TYPES = {Type.getType(Exception.class)};

static
	{
	OBJECT_TYPE = Type.getType(Object.class);
	ARG_TYPES = new Type[MAX_POSITIONAL_ARITY + 1][];
	for(int i = 0; i < MAX_POSITIONAL_ARITY; ++i)
		{
		Type[] a = new Type[i];
		for(int j = 0; j < i; j++)
			a[j] = OBJECT_TYPE;
		ARG_TYPES[i] = a;
		}
	Type[] a = new Type[MAX_POSITIONAL_ARITY + 1];
	for(int j = 0; j < MAX_POSITIONAL_ARITY; j++)
		a[j] = OBJECT_TYPE;
	a[MAX_POSITIONAL_ARITY] = Type.getType("[LObject;");
	ARG_TYPES[MAX_POSITIONAL_ARITY] = a;

	}


//symbol->localbinding
static public Var LOCAL_ENV = Var.create(null);

//vector<localbinding>
static public Var LOOP_LOCALS = Var.create();

//Label
static public Var LOOP_LABEL = Var.create();

//keyword->keywordexpr
static public Var KEYWORDS = Var.create();

//var->var
static public Var VARS = Var.create();

//FnFrame
static public Var METHOD = Var.create(null);

//String
static public Var SOURCE = Var.create(null);

//String
static public Var SOURCE_PATH = Var.create(null);

//Integer
static public Var LINE = Var.create(0);

//Integer
static public Var NEXT_LOCAL_NUM = Var.create(0);

//Integer
static public Var RET_LOCAL_NUM = Var.create();

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

	void emit(C context, FnExpr fn, GeneratorAdapter gen);

	boolean hasJavaClass() throws Exception;

	Class getJavaClass() throws Exception;
}

static abstract class UntypedExpr implements Expr{

	public Class getJavaClass(){
		throw new IllegalArgumentException("Has no Java class");
	}

	public boolean hasJavaClass(){
		return false;
	}
}

interface IParser{
	Expr parse(C context, Object form) throws Exception;
}

static boolean isSpecial(Object sym){
	return specials.containsKey(sym);
}

static Symbol resolveSymbol(Symbol sym){
	//already qualified or classname?
	if(sym.ns != null || sym.name.indexOf('.') > 0)
		return sym;
	IPersistentMap imports = (IPersistentMap) ((Var) RT.NS_IMPORTS.get()).get();
	//imported class?
	String className = (String) imports.valAt(sym);
	if(className != null)
		return Symbol.intern(null, className);
	//refers?
	IPersistentMap refers = (IPersistentMap) ((Var) RT.NS_REFERS.get()).get();
	Var var = (Var) refers.valAt(sym);
	if(var != null)
		return var.sym;

	return Symbol.intern(currentNS().name, sym.name);
}

static class DefExpr implements Expr{
	final Var var;
	final Expr init;
	final boolean initProvided;
	final static Method bindRootMethod = Method.getMethod("void bindRoot(Object)");

	public DefExpr(Var var, Expr init, boolean initProvided){
		this.var = var;
		this.init = init;
		this.initProvided = initProvided;
	}

	public Object eval() throws Exception{
		if(initProvided)
			var.bindRoot(init.eval());
		return var;
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		fn.emitVar(gen, var);
		if(initProvided)
			{
			gen.dup();
			init.emit(C.EXPRESSION, fn, gen);
			gen.invokeVirtual(VAR_TYPE, bindRootMethod);
			}
		if(context == C.STATEMENT)
			gen.pop();
	}

	public boolean hasJavaClass(){
		return true;
	}

	public Class getJavaClass(){
		return Var.class;
	}

	static class Parser implements IParser{
		public Expr parse(C context, Object form) throws Exception{
			//(def x) or (def x initexpr)
			if(RT.count(form) > 3)
				throw new Exception("Too many arguments to def");
			else if(RT.count(form) < 2)
				throw new Exception("Too few arguments to def");
			else if(!(RT.second(form) instanceof Symbol))
				throw new Exception("Second argument to def must be a Symbol");
			Symbol sym = (Symbol) RT.second(form);
			Var v = lookupVar(sym, true);
			if(v == null)
				throw new Exception("Can't refer to qualified var that doesn't exist");
			if(!v.sym.ns.equals(currentNS().name))
				{
				if(sym.ns == null)
					throw new Exception("Name conflict, can't def " + sym + " because namespace: " + currentNS().name +
					                    " refers to:" + v.sym);
				else
					throw new Exception("Can't create defs outside of current ns");
				}
			return new DefExpr(v, analyze(C.EXPRESSION, RT.third(form), v.sym.name), RT.count(form) == 3);
		}
	}
}

static class AssignExpr implements Expr{
	final AssignableExpr target;
	final Expr val;


	public AssignExpr(AssignableExpr target, Expr val){
		this.target = target;
		this.val = val;
	}

	public Object eval() throws Exception{
		return target.evalAssign(val);
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		target.emitAssign(context, fn, gen, val);
	}

	public boolean hasJavaClass() throws Exception{
		return val.hasJavaClass();
	}

	public Class getJavaClass() throws Exception{
		return val.getJavaClass();
	}

	static class Parser implements IParser{
		public Expr parse(C context, Object frm) throws Exception{
			ISeq form = (ISeq) frm;
			if(RT.length(form) != 3)
				throw new IllegalArgumentException("Malformed assignment, expecting (= target val)");
			Expr target = analyze(C.EXPRESSION, RT.second(form));
			if(!(target instanceof AssignableExpr))
				throw new IllegalArgumentException("Invalid assignment target");
			return new AssignExpr((AssignableExpr) target, analyze(C.EXPRESSION, RT.third(form)));
		}
	}
}

static class VarExpr implements Expr, AssignableExpr{
	final Var var;
	final Symbol tag;
	final static Method getMethod = Method.getMethod("Object get()");
	final static Method setMethod = Method.getMethod("Object set(Object)");

	public VarExpr(Var var, Symbol tag){
		this.var = var;
		this.tag = tag;
	}

	public Object eval() throws Exception{
		return var.get();
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		if(context != C.STATEMENT)
			{
			fn.emitVar(gen, var);
			gen.invokeVirtual(VAR_TYPE, getMethod);
			}
	}

	public boolean hasJavaClass(){
		return tag != null;
	}

	public Class getJavaClass() throws ClassNotFoundException{
		return HostExpr.tagToClass(tag);
	}

	public Object evalAssign(Expr val) throws Exception{
		return var.set(val.eval());
	}

	public void emitAssign(C context, FnExpr fn, GeneratorAdapter gen,
	                       Expr val){
		fn.emitVar(gen, var);
		val.emit(C.EXPRESSION, fn, gen);
		gen.invokeVirtual(VAR_TYPE, setMethod);
		if(context == C.STATEMENT)
			gen.pop();
	}
}

static class TheVarExpr implements Expr{
	final Var var;

	public TheVarExpr(Var var){
		this.var = var;
	}

	public Object eval() throws Exception{
		return var;
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		if(context != C.STATEMENT)
			fn.emitVar(gen, var);
	}

	public boolean hasJavaClass(){
		return true;
	}

	public Class getJavaClass() throws ClassNotFoundException{
		return Var.class;
	}

	static class Parser implements IParser{
		public Expr parse(C context, Object form) throws Exception{
			Symbol sym = (Symbol) RT.second(form);
			Var v = lookupVar(sym, false);
			if(v != null)
				return new TheVarExpr(v);
			throw new Exception("Unable to resolve var: " + sym + " in this context");
		}
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

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		if(context != C.STATEMENT)
			fn.emitKeyword(gen, k);

	}

	public boolean hasJavaClass(){
		return true;
	}

	public Class getJavaClass() throws ClassNotFoundException{
		return Keyword.class;
	}
}

static abstract class LiteralExpr implements Expr{
	abstract Object val();

	public Object eval(){
		return val();
	}
}

static interface AssignableExpr{
	Object evalAssign(Expr val) throws Exception;

	void emitAssign(C context, FnExpr fn, GeneratorAdapter gen, Expr val);
}

static abstract class HostExpr implements Expr{
	final static Type CHAR_TYPE = Type.getType(Character.class);
	final static Type NUMBER_TYPE = Type.getType(Number.class);
	final static Method charValueMethod = Method.getMethod("char charValue()");
	final static Method valueOfMethod = Method.getMethod("Character valueOf(char c)");
	final static Method intValueMethod = Method.getMethod("int intValue()");
	final static Method longValueMethod = Method.getMethod("long longValue()");
	final static Method floatValueMethod = Method.getMethod("float floatValue()");
	final static Method doubleValueMethod = Method.getMethod("double doubleValue()");
	final static Method byteValueMethod = Method.getMethod("byte byteValue()");
	final static Method shortValueMethod = Method.getMethod("short shortValue()");

	final static Method fromIntMethod = Method.getMethod("clojure.lang.Num from(int)");
	final static Method fromLongMethod = Method.getMethod("clojure.lang.Num from(long)");
	final static Method fromDoubleMethod = Method.getMethod("clojure.lang.Num from(double)");

	public static void emitBoxReturn(FnExpr fn, GeneratorAdapter gen, Class returnType){
		if(returnType.isPrimitive())
			{
			if(returnType == boolean.class)
				{
				Label falseLabel = gen.newLabel();
				Label endLabel = gen.newLabel();
				gen.ifZCmp(GeneratorAdapter.EQ, falseLabel);
				gen.getStatic(RT_TYPE, "T", KEYWORD_TYPE);
				gen.goTo(endLabel);
				gen.mark(falseLabel);
				NIL_EXPR.emit(C.EXPRESSION, fn, gen);
				gen.mark(endLabel);
				}
			else if(returnType == void.class)
				{
				NIL_EXPR.emit(C.EXPRESSION, fn, gen);
				}
			else if(returnType == char.class)
				{
				gen.invokeStatic(CHAR_TYPE, valueOfMethod);
				}
			else
				{
				Method m = fromIntMethod;
				if(returnType == int.class)
					m = fromIntMethod;
				else if(returnType == float.class)
					{
					gen.visitInsn(F2D);
					m = fromDoubleMethod;
					}
				else if(returnType == double.class)
					m = fromDoubleMethod;
				else if(returnType == long.class)
					m = fromLongMethod;
				else if(returnType == byte.class)
					m = fromIntMethod;
				else if(returnType == short.class)
					m = fromIntMethod;
				gen.invokeStatic(NUM_TYPE, m);
				}
			}
	}

	public static void emitUnboxArg(FnExpr fn, GeneratorAdapter gen, Class paramType){
		if(paramType.isPrimitive())
			{
			if(paramType == boolean.class)
				{
				Label falseLabel = gen.newLabel();
				Label endLabel = gen.newLabel();
				gen.ifNull(falseLabel);
				gen.push(1);
				gen.goTo(endLabel);
				gen.mark(falseLabel);
				gen.push(0);
				gen.mark(endLabel);
				}
			else if(paramType == char.class)
				{
				gen.checkCast(CHAR_TYPE);
				gen.invokeStatic(CHAR_TYPE, charValueMethod);
				}
			else
				{
				Method m = intValueMethod;
				gen.checkCast(NUMBER_TYPE);
				if(paramType == int.class)
					m = intValueMethod;
				else if(paramType == float.class)
					m = floatValueMethod;
				else if(paramType == double.class)
					m = doubleValueMethod;
				else if(paramType == long.class)
					m = longValueMethod;
				else if(paramType == byte.class)
					m = byteValueMethod;
				else if(paramType == short.class)
					m = shortValueMethod;
				gen.invokeVirtual(NUMBER_TYPE, m);
				}
			}
		else
			{
			gen.checkCast(Type.getType(paramType));
			}
	}

	static class Parser implements IParser{
		public Expr parse(C context, Object frm) throws Exception{
			ISeq form = (ISeq) frm;
			//(. x fieldname-sym) or (. x (methodname-sym args...))
			if(RT.length(form) != 3)
				throw new IllegalArgumentException("Malformed member expression, expecting (. target member)");
			//determine static or instance
			//static target must be symbol, either fully.qualified.Classname or Classname that has been imported
			int line = (Integer) LINE.get();
			String className = maybeClassName(RT.second(form));
			//at this point className will be non-null if static
			Expr instance = null;
			if(className == null)
				instance = analyze(context == C.EVAL ? context : C.EXPRESSION, RT.second(form));

			if(RT.third(form) instanceof Symbol)    //field
				{
				Symbol sym = (Symbol) RT.third(form);
				if(className != null)
					return new StaticFieldExpr(line, className, sym.name);
				else
					return new InstanceFieldExpr(line, instance, sym.name);
				}
			else if(RT.third(form) instanceof ISeq && RT.first(RT.third(form)) instanceof Symbol)
				{
				Symbol sym = (Symbol) RT.first(RT.third(form));
				PersistentVector args = PersistentVector.EMPTY;
				for(ISeq s = RT.rest(RT.third(form)); s != null; s = s.rest())
					args = args.cons(analyze(context == C.EVAL ? context : C.EXPRESSION, s.first()));
				if(className != null)
					return new StaticMethodExpr(line, className, sym.name, args);
				else
					return new InstanceMethodExpr(line, instance, sym.name, args);
				}
			else
				throw new IllegalArgumentException("Malformed member expression");
		}
	}

	private static String maybeClassName(Object form){
		String className = null;
		if(form instanceof Symbol)
			{
			Symbol sym = (Symbol) form;
			if(sym.ns == null) //if ns-qualified can't be classname
				{
				if(sym.name.indexOf('.') > 0)
					className = sym.name;
				else
					{
					IPersistentMap imports = (IPersistentMap) ((Var) RT.NS_IMPORTS.get()).get();
					className = (String) imports.valAt(sym);
					}
				}
			}
		return className;
	}

	static Class tagToClass(Symbol tag) throws ClassNotFoundException{
		String className = maybeClassName(tag);
		if(className != null)
			return Class.forName(className);
		throw new IllegalArgumentException("Unable to resolve classname: " + tag);
	}
}

static abstract class FieldExpr extends HostExpr{
}

static class InstanceFieldExpr extends FieldExpr implements AssignableExpr{
	final Expr target;
	final String fieldName;
	final int line;
	final static Method getInstanceFieldMethod = Method.getMethod("Object getInstanceField(Object,String)");
	final static Method setInstanceFieldMethod = Method.getMethod("Object setInstanceField(Object,String,Object)");


	public InstanceFieldExpr(int line, Expr target, String fieldName){
		this.target = target;
		this.fieldName = fieldName;
		this.line = line;
	}

	public Object eval() throws Exception{
		return Reflector.getInstanceField(target.eval(), fieldName);
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		if(context != C.STATEMENT)
			{
			gen.visitLineNumber(line, gen.mark());
			target.emit(C.EXPRESSION, fn, gen);
			gen.push(fieldName);
			gen.invokeStatic(REFLECTOR_TYPE, getInstanceFieldMethod);
			}
	}

	public boolean hasJavaClass() throws Exception{
		return target.hasJavaClass();
	}

	public Class getJavaClass() throws Exception{
		Class targetClass = target.getJavaClass();
		java.lang.reflect.Field field = targetClass.getField(fieldName);
		return field.getType();
	}

	public Object evalAssign(Expr val) throws Exception{
		return Reflector.setInstanceField(target.eval(), fieldName, val.eval());
	}

	public void emitAssign(C context, FnExpr fn, GeneratorAdapter gen,
	                       Expr val){
		target.emit(C.EXPRESSION, fn, gen);
		gen.push(fieldName);
		val.emit(C.EXPRESSION, fn, gen);
		gen.invokeStatic(REFLECTOR_TYPE, setInstanceFieldMethod);
		if(context == C.STATEMENT)
			gen.pop();
	}
}

static class StaticFieldExpr extends FieldExpr implements AssignableExpr{
	final String className;
	final String fieldName;
	final static Method getStaticFieldMethod = Method.getMethod("Object getStaticField(String,String)");
	final static Method setStaticFieldMethod = Method.getMethod("Object setStaticField(String,String,Object)");
	final int line;

	public StaticFieldExpr(int line, String className, String fieldName){
		this.className = className;
		this.fieldName = fieldName;
		this.line = line;
	}

	public Object eval() throws Exception{
		return Reflector.getStaticField(className, fieldName);
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		gen.visitLineNumber(line, gen.mark());
		gen.push(className);
		gen.push(fieldName);
		gen.invokeStatic(REFLECTOR_TYPE, getStaticFieldMethod);
	}

	public boolean hasJavaClass(){
		return true;
	}

	public Class getJavaClass() throws Exception{
		Class c = Class.forName(className);
		java.lang.reflect.Field field = c.getField(fieldName);
		return field.getType();
	}

	public Object evalAssign(Expr val) throws Exception{
		return Reflector.setStaticField(className, fieldName, val.eval());
	}

	public void emitAssign(C context, FnExpr fn, GeneratorAdapter gen,
	                       Expr val){
		gen.push(className);
		gen.push(fieldName);
		val.emit(C.EXPRESSION, fn, gen);
		gen.invokeStatic(REFLECTOR_TYPE, setStaticFieldMethod);
		if(context == C.STATEMENT)
			gen.pop();
	}
}

static abstract class MethodExpr extends HostExpr{
	static void emitArgsAsArray(IPersistentVector args, FnExpr fn, GeneratorAdapter gen){
		gen.push(args.count());
		gen.newArray(OBJECT_TYPE);
		for(int i = 0; i < args.count(); i++)
			{
			gen.dup();
			gen.push(i);
			((Expr) args.nth(i)).emit(C.EXPRESSION, fn, gen);
			gen.arrayStore(OBJECT_TYPE);
			}
	}

	public static void emitTypedArgs(FnExpr fn, GeneratorAdapter gen, Class[] parameterTypes, IPersistentVector args){
		for(int i = 0; i < parameterTypes.length; i++)
			{
			Expr e = (Expr) args.nth(i);
			e.emit(C.EXPRESSION, fn, gen);
			HostExpr.emitUnboxArg(fn, gen, parameterTypes[i]);
			}
	}
}

static class InstanceMethodExpr extends MethodExpr{
	final Expr target;
	final String methodName;
	final IPersistentVector args;
	final int line;
	final java.lang.reflect.Method method;

	final static Method invokeInstanceMethodMethod =
			Method.getMethod("Object invokeInstanceMethod(Object,String,Object[])");


	public InstanceMethodExpr(int line, Expr target, String methodName, IPersistentVector args) throws Exception{
		this.line = line;
		this.args = args;
		this.methodName = methodName;
		this.target = target;
		if(target.hasJavaClass())
			{
			List methods = Reflector.getMethods(target.getJavaClass(), args.count(), methodName, false);
			if(methods.isEmpty())
				method = null;
			//throw new IllegalArgumentException("No matching method found");
			else
				method = (java.lang.reflect.Method) ((methods.size() == 1) ? methods.get(0) : null);
			}
		else
			method = null;
	}

	public Object eval() throws Exception{
		Object targetval = target.eval();
		Object[] argvals = new Object[args.count()];
		for(int i = 0; i < args.count(); i++)
			argvals[i] = ((Expr) args.nth(i)).eval();
		return Reflector.invokeInstanceMethod(targetval, methodName, argvals);
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		gen.visitLineNumber(line, gen.mark());
		if(method != null)
			{
			Type type = Type.getType(method.getDeclaringClass());
			target.emit(C.EXPRESSION, fn, gen);
			gen.checkCast(type);
			MethodExpr.emitTypedArgs(fn, gen, method.getParameterTypes(), args);
			Method m = new Method(methodName, Type.getReturnType(method), Type.getArgumentTypes(method));
			if(method.getDeclaringClass().isInterface())
				gen.invokeInterface(type, m);
			else
				gen.invokeVirtual(type, m);
			HostExpr.emitBoxReturn(fn, gen, method.getReturnType());
			}
		else
			{
			target.emit(C.EXPRESSION, fn, gen);
			gen.push(methodName);
			emitArgsAsArray(args, fn, gen);
			gen.invokeStatic(REFLECTOR_TYPE, invokeInstanceMethodMethod);
			}
		if(context == C.STATEMENT)
			gen.pop();
	}

	public boolean hasJavaClass(){
		return method != null;
	}

	public Class getJavaClass() throws Exception{
		return method.getReturnType();
	}
}


static class StaticMethodExpr extends MethodExpr{
	final String className;
	final String methodName;
	final IPersistentVector args;
	final int line;
	final java.lang.reflect.Method method;
	final static Method invokeStaticMethodMethod =
			Method.getMethod("Object invokeStaticMethod(String,String,Object[])");


	public StaticMethodExpr(int line, String className, String methodName, IPersistentVector args)
			throws ClassNotFoundException{
		this.className = className;
		this.methodName = methodName;
		this.args = args;
		this.line = line;

		List methods = Reflector.getMethods(Class.forName(className), args.count(), methodName, true);
		if(methods.isEmpty())
			throw new IllegalArgumentException("No matching method: " + methodName);
		method = (java.lang.reflect.Method) ((methods.size() == 1) ? methods.get(0) : null);
	}

	public Object eval() throws Exception{
		Object[] argvals = new Object[args.count()];
		for(int i = 0; i < args.count(); i++)
			argvals[i] = ((Expr) args.nth(i)).eval();
		return Reflector.invokeStaticMethod(className, methodName, argvals);
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		gen.visitLineNumber(line, gen.mark());
		if(method != null)
			{
			MethodExpr.emitTypedArgs(fn, gen, method.getParameterTypes(), args);
			Type type = Type.getObjectType(className.replace('.', '/'));
			Method m = new Method(methodName, Type.getReturnType(method), Type.getArgumentTypes(method));
			gen.invokeStatic(type, m);
			HostExpr.emitBoxReturn(fn, gen, method.getReturnType());
			}
		else
			{
			gen.push(className);
			gen.push(methodName);
			emitArgsAsArray(args, fn, gen);
			gen.invokeStatic(REFLECTOR_TYPE, invokeStaticMethodMethod);
			}
		if(context == C.STATEMENT)
			gen.pop();
	}

	public boolean hasJavaClass(){
		return method != null;
	}

	public Class getJavaClass() throws Exception{
		return method.getReturnType();
	}
}


static class QuoteExpr extends LiteralExpr{
	//stuff quoted vals in classloader at compile time, pull out at runtime
	//this won't work for static compilation...
	final Object v;
	final int id;
	final static Type DYNAMIC_CLASSLOADER_TYPE = Type.getType(DynamicClassLoader.class);
	final static Method getClassMethod = Method.getMethod("Class getClass()");
	final static Method getClassLoaderMethod = Method.getMethod("ClassLoader getClassLoader()");
	final static Method getQuotedValMethod = Method.getMethod("Object getQuotedVal(int)");

	public QuoteExpr(Object v){
		this.v = v;
		this.id = RT.nextID();
		DynamicClassLoader loader = (DynamicClassLoader) LOADER.get();
		loader.registerQuotedVal(id, v);
	}

	Object val(){
		return v;
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		if(context != C.STATEMENT)
			{
			gen.loadThis();
			gen.invokeVirtual(OBJECT_TYPE, getClassMethod);
			gen.invokeVirtual(CLASS_TYPE, getClassLoaderMethod);
			gen.checkCast(DYNAMIC_CLASSLOADER_TYPE);
			gen.push(id);
			gen.invokeVirtual(DYNAMIC_CLASSLOADER_TYPE, getQuotedValMethod);
			}
	}

	public boolean hasJavaClass(){
		return false;
	}

	public Class getJavaClass() throws Exception{
		throw new IllegalArgumentException("Has no Java class");
	}

	static class Parser implements IParser{
		public Expr parse(C context, Object form){
			Object v = RT.second(form);

			if(v == null)
				return NIL_EXPR;
			Class fclass = v.getClass();
			if(fclass == Keyword.class)
				return registerKeyword((Keyword) v);
			else if(v instanceof Num)
				return new NumExpr((Num) v);
			else if(fclass == String.class)
				return new StringExpr((String) v);
			else if(fclass == Character.class)
				return new CharExpr((Character) v);
			else if(v instanceof IPersistentCollection && ((IPersistentCollection) v).count() == 0)
				return new EmptyExpr(v);
			else
				return new QuoteExpr(v);
		}
	}
}

static class NilExpr extends LiteralExpr{
	Object val(){
		return null;
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		if(context != C.STATEMENT)
			gen.visitInsn(Opcodes.ACONST_NULL);
	}

	public boolean hasJavaClass(){
		return true;
	}

	public Class getJavaClass() throws Exception{
		return null;
	}
}

static NilExpr NIL_EXPR = new NilExpr();


static class NumExpr extends LiteralExpr{
	final Num num;
	final static Method numFromIntMethod = Method.getMethod("clojure.lang.Num from(int)");
	final static Method numFromDoubleMethod = Method.getMethod("clojure.lang.Num from(double)");
	final static Method numFromBigIntMethod = Method.getMethod("clojure.lang.Num from(java.math.BigInteger)");
	final static Method numDivideMethod =
			Method.getMethod("clojure.lang.Num divide(java.math.BigInteger,java.math.BigInteger)");
	final static Type BIGINT_TYPE = Type.getType(BigInteger.class);
	final static Method bigintFromStringCtor = Method.getMethod("void <init>(String)");

	public NumExpr(Num num){
		this.num = num;
	}

	Object val(){
		return num;
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		if(context != C.STATEMENT)
			{
			Class nclass = num.getClass();
			if(nclass == FixNum.class)
				{
				gen.push(num.intValue());
				gen.invokeStatic(NUM_TYPE, numFromIntMethod);
				}
			else if(nclass == DoubleNum.class)
				{
				gen.push(num.doubleValue());
				gen.invokeStatic(NUM_TYPE, numFromDoubleMethod);
				}
			else if(nclass == BigNum.class)
				{
				emitBigInteger(gen, num);
				gen.invokeStatic(NUM_TYPE, numFromBigIntMethod);
				}
			else if(nclass == RatioNum.class)
				{
				RatioNum r = (RatioNum) num;
				emitBigInteger(gen, r.numerator);
				emitBigInteger(gen, r.denominator);
				gen.invokeStatic(NUM_TYPE, numDivideMethod);
				}
			else
				throw new UnsupportedOperationException("Unknown Num type");
			}
	}

	public boolean hasJavaClass(){
		return true;
	}

	public Class getJavaClass() throws Exception{
		return Num.class;
	}

	static void emitBigInteger(GeneratorAdapter gen, Num num){
		gen.newInstance(BIGINT_TYPE);
		gen.dup();
		gen.push(num.toString());
		gen.invokeConstructor(BIGINT_TYPE, bigintFromStringCtor);
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

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		if(context != C.STATEMENT)
			gen.push(str);
	}

	public boolean hasJavaClass(){
		return true;
	}

	public Class getJavaClass() throws Exception{
		return String.class;
	}
}

static class CharExpr extends LiteralExpr{
	final Character ch;
	final static Type CHARACTER_TYPE = Type.getObjectType("java/lang/Character");
	final static Method charValueOfMethod = Method.getMethod("Character valueOf(char)");

	public CharExpr(Character ch){
		this.ch = ch;
	}

	Object val(){
		return ch;
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		if(context != C.STATEMENT)
			{
			gen.push(ch.charValue());
			gen.invokeStatic(CHARACTER_TYPE, charValueOfMethod);
			}
	}

	public boolean hasJavaClass(){
		return true;
	}

	public Class getJavaClass() throws Exception{
		return Character.class;
	}
}

static class MonitorEnterExpr extends UntypedExpr{
	final Expr target;

	public MonitorEnterExpr(Expr target){
		this.target = target;
	}

	public Object eval() throws Exception{
		throw new UnsupportedOperationException("Can't eval monitor-enter");
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		target.emit(C.EXPRESSION, fn, gen);
		gen.monitorEnter();
		if(context != C.STATEMENT)
			{
			NIL_EXPR.emit(context, fn, gen);
			}
	}

	static class Parser implements IParser{
		public Expr parse(C context, Object form) throws Exception{
			return analyze(C.EXPRESSION, RT.second(form));
		}
	}
}

static class MonitorExitExpr extends UntypedExpr{
	final Expr target;

	public MonitorExitExpr(Expr target){
		this.target = target;
	}

	public Object eval() throws Exception{
		throw new UnsupportedOperationException("Can't eval monitor-exit");
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		target.emit(C.EXPRESSION, fn, gen);
		gen.monitorExit();
		if(context != C.STATEMENT)
			{
			NIL_EXPR.emit(context, fn, gen);
			}
	}

	static class Parser implements IParser{
		public Expr parse(C context, Object form) throws Exception{
			return analyze(C.EXPRESSION, RT.second(form));
		}
	}

}

static class TryFinallyExpr implements Expr{
	final Expr tryExpr;
	final Expr finallyExpr;


	public TryFinallyExpr(Expr tryExpr, Expr finallyExpr){
		this.tryExpr = tryExpr;
		this.finallyExpr = finallyExpr;
	}

	public Object eval() throws Exception{
		throw new UnsupportedOperationException("Can't eval try");
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		Label startTry = gen.newLabel();
		Label endTry = gen.newLabel();
		Label end = gen.newLabel();
		Label finallyLabel = gen.newLabel();
		gen.visitTryCatchBlock(startTry, endTry, finallyLabel, null);
		gen.mark(startTry);
		tryExpr.emit(context, fn, gen);
		gen.mark(endTry);
		finallyExpr.emit(C.STATEMENT, fn, gen);
		gen.goTo(end);
		gen.mark(finallyLabel);
		//exception should be on stack
		finallyExpr.emit(C.STATEMENT, fn, gen);
		gen.throwException();
		gen.mark(end);
	}

	public boolean hasJavaClass() throws Exception{
		return tryExpr.hasJavaClass();
	}

	public Class getJavaClass() throws Exception{
		return tryExpr.getJavaClass();
	}

	static class Parser implements IParser{
		public Expr parse(C context, Object frm) throws Exception{
			ISeq form = (ISeq) frm;
			//(try-finally try-expr finally-expr)
			if(form.count() != 3)
				throw new IllegalArgumentException(
						"Wrong number of arguments, expecting: (try-finally try-expr finally-expr) ");

			if(context == C.EVAL || context == C.EXPRESSION)
				return analyze(context, RT.list(RT.list(FN, PersistentVector.EMPTY, form)));

			return new TryFinallyExpr(analyze(context, RT.second(form)),
			                          analyze(C.STATEMENT, RT.third(form)));
		}
	}
}

static class ThrowExpr extends UntypedExpr{
	final Expr excExpr;

	public ThrowExpr(Expr excExpr){
		this.excExpr = excExpr;
	}


	public Object eval() throws Exception{
		throw (Exception) excExpr.eval();
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		excExpr.emit(C.EXPRESSION, fn, gen);
		gen.checkCast(THROWABLE_TYPE);
		gen.throwException();
	}

	static class Parser implements IParser{
		public Expr parse(C context, Object form) throws Exception{
			return new ThrowExpr(analyze(C.EXPRESSION, RT.second(form)));
		}
	}
}

static class ClassExpr implements Expr{
	final String className;
	final static Method forNameMethod = Method.getMethod("Class forName(String)");


	public ClassExpr(String className){
		this.className = className;
	}

	public Object eval() throws Exception{
		return Class.forName(className);
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		if(context != C.STATEMENT)
			{
			gen.push(className);
			gen.invokeStatic(CLASS_TYPE, forNameMethod);
			}
	}

	public boolean hasJavaClass(){
		return true;
	}

	public Class getJavaClass() throws Exception{
		return Class.class;
	}

	static class Parser implements IParser{
		public Expr parse(C context, Object frm) throws Exception{
			ISeq form = (ISeq) frm;
			//(class Classname)
			if(form.count() != 2)
				throw new Exception("wrong number of arguments, expecting: (class Classname)");
			String className = HostExpr.maybeClassName(RT.second(form));
			if(className == null)
				throw new IllegalArgumentException("Unable to resolve classname: " + RT.second(form));
			return new ClassExpr(className);
		}
	}
}

static class NewExpr implements Expr{
	final String className;
	final IPersistentVector args;
	final Constructor ctor;
	final Class c;
	final static Method invokeConstructorMethod =
			Method.getMethod("Object invokeConstructor(Class,Object[])");
	final static Method forNameMethod = Method.getMethod("Class forName(String)");


	public NewExpr(String className, IPersistentVector args) throws ClassNotFoundException{
		this.args = args;
		this.className = className;
		this.c = Class.forName(className);
		Constructor[] allctors = c.getConstructors();
		ArrayList ctors = new ArrayList();
		for(int i = 0; i < allctors.length; i++)
			{
			Constructor ctor = allctors[i];
			if(ctor.getParameterTypes().length == args.count())
				ctors.add(ctor);
			}
		if(ctors.isEmpty())
			throw new IllegalArgumentException("No matching ctor found");

		this.ctor = (ctors.size() == 1) ? (Constructor) ctors.get(0) : null;
	}

	public Object eval() throws Exception{
		Object[] argvals = new Object[args.count()];
		for(int i = 0; i < args.count(); i++)
			argvals[i] = ((Expr) args.nth(i)).eval();
		return Reflector.invokeConstructor(Class.forName(className), argvals);
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		if(this.ctor != null)
			{
			Type type = Type.getType(c);
			gen.newInstance(type);
			gen.dup();
			MethodExpr.emitTypedArgs(fn, gen, ctor.getParameterTypes(), args);
			gen.invokeConstructor(type, new Method("<init>", Type.getConstructorDescriptor(ctor)));
			}
		else
			{
			gen.push(className);
			gen.invokeStatic(CLASS_TYPE, forNameMethod);
			MethodExpr.emitArgsAsArray(args, fn, gen);
			gen.invokeStatic(REFLECTOR_TYPE, invokeConstructorMethod);
			}
		if(context == C.STATEMENT)
			gen.pop();
	}

	public boolean hasJavaClass(){
		return true;
	}

	public Class getJavaClass() throws Exception{
		return Class.forName(className);
	}

	static class Parser implements IParser{
		public Expr parse(C context, Object frm) throws Exception{
			ISeq form = (ISeq) frm;
			//(new Classname args...)
			if(form.count() < 2)
				throw new Exception("wrong number of arguments, expecting: (new Classname args...)");
			String className = HostExpr.maybeClassName(RT.second(form));
			if(className == null)
				throw new IllegalArgumentException("Unable to resolve classname: " + RT.second(form));
			PersistentVector args = PersistentVector.EMPTY;
			for(ISeq s = RT.rest(RT.rest(form)); s != null; s = s.rest())
				args = args.cons(analyze(C.EXPRESSION, s.first()));
			return new NewExpr(className, args);
		}
	}

}

static class InstanceExpr extends UntypedExpr{
	final Expr expr;
	final String className;


	public InstanceExpr(Expr expr, String className){
		this.expr = expr;
		this.className = className;
	}

	public Object eval() throws Exception{
		return Class.forName(className).isInstance(expr.eval()) ?
		       RT.T : null;
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		if(context != C.STATEMENT)
			{
			Label not = gen.newLabel();
			Label end = gen.newLabel();
			expr.emit(C.EXPRESSION, fn, gen);
			gen.instanceOf(Type.getObjectType(className.replace('.', '/')));
			gen.ifZCmp(GeneratorAdapter.EQ, not);
			gen.getStatic(RT_TYPE, "T", KEYWORD_TYPE);
			gen.goTo(end);
			gen.mark(not);
			NIL_EXPR.emit(C.EXPRESSION, fn, gen);
			gen.mark(end);
			}
	}

	static class Parser implements IParser{
		public Expr parse(C context, Object frm) throws Exception{
			ISeq form = (ISeq) frm;
			//(instance? x Classname)
			if(form.count() != 3)
				throw new Exception("wrong number of arguments, expecting: (instance? x Classname)");
			String className = HostExpr.maybeClassName(RT.third(form));
			if(className == null)
				throw new IllegalArgumentException("Unable to resolve classname: " + RT.third(form));
			return new InstanceExpr(analyze(C.EXPRESSION, RT.second(form)), className);
		}
	}
}

static class MetaExpr implements Expr{
	final Expr expr;
	final MapExpr meta;
	final static Type IPERSISTENTMAP_TYPE = Type.getType(IPersistentMap.class);
	final static Type IOBJ_TYPE = Type.getType(IObj.class);
	final static Method withMetaMethod = Method.getMethod("clojure.lang.IObj withMeta(clojure.lang.IPersistentMap)");


	public MetaExpr(Expr expr, MapExpr meta){
		this.expr = expr;
		this.meta = meta;
	}

	public Object eval() throws Exception{
		return ((IObj) expr.eval()).withMeta((IPersistentMap) meta.eval());
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		if(context != C.STATEMENT)
			{
			expr.emit(C.EXPRESSION, fn, gen);
			gen.checkCast(IOBJ_TYPE);
			meta.emit(C.EXPRESSION, fn, gen);
			gen.checkCast(IPERSISTENTMAP_TYPE);
			gen.invokeInterface(IOBJ_TYPE, withMetaMethod);
			}
	}

	public boolean hasJavaClass() throws Exception{
		return expr.hasJavaClass();
	}

	public Class getJavaClass() throws Exception{
		return expr.getJavaClass();
	}
}

static class IfExpr implements Expr{
	final Expr testExpr;
	final Expr thenExpr;
	final Expr elseExpr;
	final int line;


	public IfExpr(int line, Expr testExpr, Expr thenExpr, Expr elseExpr){
		this.testExpr = testExpr;
		this.thenExpr = thenExpr;
		this.elseExpr = elseExpr;
		this.line = line;
	}

	public Object eval() throws Exception{
		if(testExpr.eval() != null)
			return thenExpr.eval();
		return elseExpr.eval();
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		Label elseLabel = gen.newLabel();
		Label endLabel = gen.newLabel();

		gen.visitLineNumber(line, gen.mark());

		testExpr.emit(C.EXPRESSION, fn, gen);
		gen.ifNull(elseLabel);
		thenExpr.emit(context, fn, gen);
		gen.goTo(endLabel);
		gen.mark(elseLabel);
		elseExpr.emit(context, fn, gen);
		gen.mark(endLabel);
	}

	public boolean hasJavaClass() throws Exception{
		return thenExpr.hasJavaClass()
		       && elseExpr.hasJavaClass()
		       &&
		       (thenExpr.getJavaClass() == elseExpr.getJavaClass()
		        || thenExpr.getJavaClass() == null
		        || elseExpr.getJavaClass() == null);
	}

	public Class getJavaClass() throws Exception{
		Class thenClass = thenExpr.getJavaClass();
		if(thenClass != null)
			return thenClass;
		return elseExpr.getJavaClass();
	}

	static class Parser implements IParser{
		public Expr parse(C context, Object frm) throws Exception{
			ISeq form = (ISeq) frm;
			//(if test then) or (if test then else)
			if(form.count() > 4)
				throw new Exception("Too many arguments to if");
			else if(form.count() < 3)
				throw new Exception("Too few arguments to if");
			return new IfExpr((Integer) LINE.get(),
			                  analyze(context == C.EVAL ? context : C.EXPRESSION, RT.second(form)),
			                  analyze(context, RT.third(form)),
			                  analyze(context, RT.fourth(form)));
		}
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

static class EmptyExpr implements Expr{
	final Object coll;
	final static Type HASHMAP_TYPE = Type.getType(PersistentHashMap.class);
	final static Type VECTOR_TYPE = Type.getType(PersistentVector.class);
	final static Type LIST_TYPE = Type.getType(PersistentList.class);


	public EmptyExpr(Object coll){
		this.coll = coll;
	}

	public Object eval() throws Exception{
		return coll;
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		if(context != C.STATEMENT)
			{
			if(coll instanceof IPersistentList)
				gen.getStatic(LIST_TYPE, "EMPTY", LIST_TYPE);
			else if(coll instanceof IPersistentVector)
				gen.getStatic(VECTOR_TYPE, "EMPTY", VECTOR_TYPE);
			else if(coll instanceof IPersistentMap)
				gen.getStatic(HASHMAP_TYPE, "EMPTY", HASHMAP_TYPE);
			else
				throw new UnsupportedOperationException("Unknown Collection type");
			}
	}

	public boolean hasJavaClass() throws Exception{
		return true;
	}

	public Class getJavaClass() throws Exception{
		if(coll instanceof IPersistentList)
			return IPersistentList.class;
		else if(coll instanceof IPersistentVector)
			return IPersistentVector.class;
		else if(coll instanceof IPersistentMap)
			return IPersistentMap.class;
		else
			throw new UnsupportedOperationException("Unknown Collection type");
	}
}

static class ListExpr implements Expr{
	final IPersistentVector args;
	final static Method arrayToListMethod = Method.getMethod("clojure.lang.ISeq arrayToList(Object[])");


	public ListExpr(IPersistentVector args){
		this.args = args;
	}

	public Object eval() throws Exception{
		IPersistentVector ret = PersistentVector.EMPTY;
		for(int i = 0; i < args.count(); i++)
			ret = (IPersistentVector) ret.cons(((Expr) args.nth(i)).eval());
		return ret.seq();
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		MethodExpr.emitArgsAsArray(args, fn, gen);
		gen.invokeStatic(RT_TYPE, arrayToListMethod);
		if(context == C.STATEMENT)
			gen.pop();
	}

	public boolean hasJavaClass() throws Exception{
		return true;
	}

	public Class getJavaClass() throws Exception{
		return IPersistentList.class;
	}

}

static class MapExpr implements Expr{
	final IPersistentVector keyvals;
	final static Method mapMethod = Method.getMethod("clojure.lang.IPersistentMap map(Object[])");


	public MapExpr(IPersistentVector keyvals){
		this.keyvals = keyvals;
	}

	public Object eval() throws Exception{
		Object[] ret = new Object[keyvals.count()];
		for(int i = 0; i < keyvals.count(); i++)
			ret[i] = ((Expr) keyvals.nth(i)).eval();
		return RT.map(ret);
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		MethodExpr.emitArgsAsArray(keyvals, fn, gen);
		gen.invokeStatic(RT_TYPE, mapMethod);
		if(context == C.STATEMENT)
			gen.pop();
	}

	public boolean hasJavaClass() throws Exception{
		return true;
	}

	public Class getJavaClass() throws Exception{
		return IPersistentMap.class;
	}


	static public Expr parse(C context, IPersistentMap form) throws Exception{
		IPersistentVector keyvals = PersistentVector.EMPTY;
		for(ISeq s = RT.seq(form); s != null; s = s.rest())
			{
			IMapEntry e = (IMapEntry) s.first();
			keyvals = (IPersistentVector) keyvals.cons(analyze(context == C.EVAL ? context : C.EXPRESSION, e.key()));
			keyvals = (IPersistentVector) keyvals.cons(analyze(context == C.EVAL ? context : C.EXPRESSION, e.val()));
			}
		Expr ret = new MapExpr(keyvals);
		if(form instanceof IObj && ((IObj) form).meta() != null)
			return new MetaExpr(ret, (MapExpr) MapExpr
					.parse(context == C.EVAL ? context : C.EXPRESSION, ((IObj) form).meta()));
		else
			return ret;
	}
}

static class VectorExpr implements Expr{
	final IPersistentVector args;
	final static Method vectorMethod = Method.getMethod("clojure.lang.IPersistentVector vector(Object[])");


	public VectorExpr(IPersistentVector args){
		this.args = args;
	}

	public Object eval() throws Exception{
		IPersistentVector ret = PersistentVector.EMPTY;
		for(int i = 0; i < args.count(); i++)
			ret = (IPersistentVector) ret.cons(((Expr) args.nth(i)).eval());
		return ret;
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		MethodExpr.emitArgsAsArray(args, fn, gen);
		gen.invokeStatic(RT_TYPE, vectorMethod);
		if(context == C.STATEMENT)
			gen.pop();
	}

	public boolean hasJavaClass() throws Exception{
		return true;
	}

	public Class getJavaClass() throws Exception{
		return IPersistentVector.class;
	}

	static public Expr parse(C context, IPersistentVector form) throws Exception{
		IPersistentVector args = PersistentVector.EMPTY;
		for(int i = 0; i < form.count(); i++)
			args = (IPersistentVector) args.cons(analyze(context == C.EVAL ? context : C.EXPRESSION, form.nth(i)));
		Expr ret = new VectorExpr(args);
		if(form instanceof IObj && ((IObj) form).meta() != null)
			return new MetaExpr(ret, (MapExpr) MapExpr
					.parse(context == C.EVAL ? context : C.EXPRESSION, ((IObj) form).meta()));
		else
			return ret;
	}

}

static class InvokeExpr implements Expr{
	final Expr fexpr;
	final Symbol tag;
	final IPersistentVector args;
	final int line;

	public InvokeExpr(int line, Symbol tag, Expr fexpr, IPersistentVector args){
		this.fexpr = fexpr;
		this.args = args;
		this.line = line;
		this.tag = tag;
	}

	public Object eval() throws Exception{
		IFn fn = (IFn) fexpr.eval();
		PersistentVector argvs = PersistentVector.EMPTY;
		for(int i = 0; i < args.count(); i++)
			argvs = argvs.cons(((Expr) args.nth(i)).eval());
		return Reflector.prepRet(fn.applyTo(RT.seq(argvs)));
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		gen.visitLineNumber(line, gen.mark());
		fexpr.emit(C.EXPRESSION, fn, gen);
		gen.checkCast(IFN_TYPE);
		for(int i = 0; i < Math.min(MAX_POSITIONAL_ARITY, args.count()); i++)
			{
			Expr e = (Expr) args.nth(i);
			e.emit(C.EXPRESSION, fn, gen);
			}
		if(args.count() > MAX_POSITIONAL_ARITY)
			{
			PersistentVector restArgs = PersistentVector.EMPTY;
			for(int i = MAX_POSITIONAL_ARITY; i < args.count(); i++)
				{
				restArgs = restArgs.cons(args.nth(i));
				}
			MethodExpr.emitArgsAsArray(restArgs, fn, gen);
			}
		gen.invokeInterface(IFN_TYPE, new Method("invoke", OBJECT_TYPE, ARG_TYPES[Math.min(MAX_POSITIONAL_ARITY,
		                                                                                   args.count())]));
		if(context == C.STATEMENT)
			gen.pop();
	}

	public boolean hasJavaClass() throws Exception{
		return tag != null;
	}

	public Class getJavaClass() throws Exception{
		return HostExpr.tagToClass(tag);
	}

	static public Expr parse(C context, ISeq form) throws Exception{
		if(context != C.EVAL)
			context = C.EXPRESSION;
		Expr fexpr = analyze(context, form.first());
		PersistentVector args = PersistentVector.EMPTY;
		for(ISeq s = RT.seq(form.rest()); s != null; s = s.rest())
			{
			args = args.cons(analyze(context, s.first()));
			}
//		if(args.count() > MAX_POSITIONAL_ARITY)
//			throw new IllegalArgumentException(
//					String.format("No more than %d args supported", MAX_POSITIONAL_ARITY));

		return new InvokeExpr((Integer) LINE.get(), tagOf(form), fexpr, args);
	}
}

static class SourceDebugExtensionAttribute extends Attribute{
	public SourceDebugExtensionAttribute(){
		super("SourceDebugExtension");
	}

	void writeSMAP(ClassWriter cw, String smap){
		ByteVector bv = write(cw, null, -1, -1, -1);
		bv.putUTF8(smap);
	}
}

static class FnExpr implements Expr{
	IPersistentCollection methods;
	//if there is a variadic overload (there can only be one) it is stored here
	FnMethod variadicMethod = null;
	String name;
	String simpleName;
	String internalName;
	Type fntype;
	//localbinding->itself
	IPersistentMap closes = PersistentHashMap.EMPTY;
	//Keyword->KeywordExpr
	IPersistentMap keywords = PersistentHashMap.EMPTY;
	IPersistentMap vars = PersistentHashMap.EMPTY;
	Class compiledClass;
	int line;

	final static Method kwintern = Method.getMethod("clojure.lang.Keyword intern(String, String)");
	final static Method symcreate = Method.getMethod("clojure.lang.Symbol create(String, String)");
	final static Method varintern = Method.getMethod("clojure.lang.Var intern(clojure.lang.Symbol)");
	final static Method afnctor = Method.getMethod("void <init>()");
	final static Method restfnctor = Method.getMethod("void <init>(int)");
	final static Type aFnType = Type.getType(AFn.class);
	final static Type restFnType = Type.getType(RestFn.class);

	static Expr parse(C context, ISeq form, String name) throws Exception{
		FnExpr fn = new FnExpr();
		FnMethod enclosingMethod = (FnMethod) METHOD.get();
		String basename = enclosingMethod != null ?
		                  (enclosingMethod.fn.name + "$")
		                  : (munge(currentNS().name) + ".");
		fn.simpleName = (name != null ?
		                 munge(name)
		                 : ("fn__" + RT.nextID()));
		fn.name = basename + fn.simpleName;
		fn.internalName = fn.name.replace('.', '/');
		fn.fntype = Type.getObjectType(fn.internalName);
		try
			{
			Var.pushThreadBindings(
					RT.map(
							KEYWORDS, PersistentHashMap.EMPTY,
							VARS, PersistentHashMap.EMPTY));
			//(fn [args] body...) or (fn ([args] body...) ([args2] body2...) ...)
			//turn former into latter
			if(RT.second(form) instanceof IPersistentVector)
				form = RT.list(FN, RT.rest(form));
			fn.line = (Integer) LINE.get();
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
						throw new Exception(
								"Can't have fixed arity function with more params than variadic function");
				}

			IPersistentCollection methods = null;
			for(int i = 0; i < methodArray.length; i++)
				if(methodArray[i] != null)
					methods = RT.conj(methods, methodArray[i]);
			if(variadicMethod != null)
				methods = RT.conj(methods, variadicMethod);

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
//		ClassWriter cw = new ClassWriter(0);
		ClassVisitor cv = cw;
		//ClassVisitor cv = new TraceClassVisitor(new CheckClassAdapter(cw), new PrintWriter(System.out));
		//ClassVisitor cv = new TraceClassVisitor(cw, new PrintWriter(System.out));
		cv.visit(V1_5, ACC_PUBLIC, internalName, null, isVariadic() ? "clojure/lang/RestFn" : "clojure/lang/AFn", null);
		String source = (String) SOURCE.get();
		String smap = "SMAP\n" +
		              simpleName + ".java\n" +
		              "Clojure\n" +
		              "*S Clojure\n" +
		              "*F\n" +
		              "+ 1 " + source + "\n" +
		              (String) SOURCE_PATH.get() + "\n" +
		              "*L\n" +
		              "1#1,1000:1\n" +
		              "*E";
		if(source != null && SOURCE_PATH.get() != null)
		//cv.visitSource(source, null);
			cv.visitSource(source, smap);
		//static fields for keywords
		for(ISeq s = RT.keys(keywords); s != null; s = s.rest())
			{
			Keyword k = (Keyword) s.first();
			cv.visitField(ACC_PUBLIC + ACC_FINAL + ACC_STATIC, munge(k.sym.toString()),
			              KEYWORD_TYPE.getDescriptor(), null, null);
			}
		//static fields for vars
		for(ISeq s = RT.keys(vars); s != null; s = s.rest())
			{
			Var v = (Var) s.first();
			cv.visitField(ACC_PUBLIC + ACC_FINAL + ACC_STATIC, munge(v.sym.toString()),
			              VAR_TYPE.getDescriptor(), null, null);
			}
		//static init for keywords and vars
		GeneratorAdapter clinitgen = new GeneratorAdapter(ACC_PUBLIC + ACC_STATIC,
		                                                  Method.getMethod("void <clinit> ()"),
		                                                  null,
		                                                  null,
		                                                  cv);
		clinitgen.visitCode();
		clinitgen.visitLineNumber(line, clinitgen.mark());
		for(ISeq s = RT.keys(keywords); s != null; s = s.rest())
			{
			Keyword k = (Keyword) s.first();
			clinitgen.push(k.sym.ns);
			clinitgen.push(k.sym.name);
			clinitgen.invokeStatic(KEYWORD_TYPE, kwintern);
			clinitgen.putStatic(fntype, munge(k.sym.toString()), KEYWORD_TYPE);
			}
		for(ISeq s = RT.keys(vars); s != null; s = s.rest())
			{
			Var v = (Var) s.first();
			clinitgen.push(v.sym.ns);
			clinitgen.push(v.sym.name);
			clinitgen.invokeStatic(SYMBOL_TYPE, symcreate);
			clinitgen.invokeStatic(VAR_TYPE, varintern);
			clinitgen.putStatic(fntype, munge(v.sym.toString()), VAR_TYPE);
			}
		clinitgen.returnValue();
		clinitgen.endMethod();
//		clinitgen.visitMaxs(1, 1);
		//instance fields for closed-overs
		for(ISeq s = RT.keys(closes); s != null; s = s.rest())
			{
			LocalBinding lb = (LocalBinding) s.first();
			cv.visitField(ACC_PUBLIC + ACC_FINAL, lb.name, OBJECT_TYPE.getDescriptor(), null, null);
			}
		//ctor that takes closed-overs and inits base + fields
		Method m = new Method("<init>", Type.VOID_TYPE, ARG_TYPES[closes.count()]);
		GeneratorAdapter ctorgen = new GeneratorAdapter(ACC_PUBLIC,
		                                                m,
		                                                null,
		                                                null,
		                                                cv);
		ctorgen.visitCode();
		ctorgen.visitLineNumber(line, ctorgen.mark());
		ctorgen.loadThis();
		if(isVariadic()) //RestFn ctor takes reqArity arg
			{
			ctorgen.push(variadicMethod.reqParms.count());
			ctorgen.invokeConstructor(restFnType, restfnctor);
			}
		else
			ctorgen.invokeConstructor(aFnType, afnctor);
		int a = 1;
		for(ISeq s = RT.keys(closes); s != null; s = s.rest(), ++a)
			{
			LocalBinding lb = (LocalBinding) s.first();
			ctorgen.loadThis();
			ctorgen.visitVarInsn(OBJECT_TYPE.getOpcode(Opcodes.ILOAD), a);
			ctorgen.putField(fntype, lb.name, OBJECT_TYPE);
			}
		ctorgen.returnValue();
		//	ctorgen.visitMaxs(1, 1);
		ctorgen.endMethod();

		//override of invoke/doInvoke for each method
		for(ISeq s = RT.seq(methods); s != null; s = s.rest())
			{
			FnMethod method = (FnMethod) s.first();
			method.emit(this, cv);
			}
		//end of class
		cv.visitEnd();

		//define class and store
		DynamicClassLoader loader = (DynamicClassLoader) LOADER.get();
		compiledClass = loader.defineClass(name, cw.toByteArray());
	}

	public Object eval() throws Exception{
		return compiledClass.newInstance();
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		//emitting a Fn means constructing an instance, feeding closed-overs from enclosing scope, if any
		//fn arg is enclosing fn, not this
		if(context != C.STATEMENT)
			{
			gen.newInstance(fntype);
			gen.dup();
			for(ISeq s = RT.keys(closes); s != null; s = s.rest())
				{
				LocalBinding lb = (LocalBinding) s.first();
				fn.emitLocal(gen, lb);
				}
			gen.invokeConstructor(fntype, new Method("<init>", Type.VOID_TYPE, ARG_TYPES[closes.count()]));
			}
	}

	public boolean hasJavaClass() throws Exception{
		return true;
	}

	public Class getJavaClass() throws Exception{
		return IFn.class;
	}

	private void emitLocal(GeneratorAdapter gen, LocalBinding lb){
		if(closes.containsKey(lb))
			{
			gen.loadThis();
			gen.getField(fntype, lb.name, OBJECT_TYPE);
			}
		else
			gen.visitVarInsn(OBJECT_TYPE.getOpcode(Opcodes.ILOAD), lb.idx);
	}

	public void emitVar(GeneratorAdapter gen, Var var){
		gen.getStatic(fntype, munge(var.sym.toString()), VAR_TYPE);
	}

	public void emitKeyword(GeneratorAdapter gen, Keyword k){
		gen.getStatic(fntype, munge(k.sym.toString()), KEYWORD_TYPE);
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
	PersistentVector argLocals;
	int line;

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
		IPersistentVector parms = (IPersistentVector) RT.first(form);
		ISeq body = RT.rest(form);
		try
			{
			FnMethod method = new FnMethod(fn, (FnMethod) METHOD.get());
			method.line = (Integer) LINE.get();
			//register as the current method and set up a new env frame
			Var.pushThreadBindings(
					RT.map(
							METHOD, method,
							LOCAL_ENV, LOCAL_ENV.get(),
							LOOP_LOCALS, null,
							NEXT_LOCAL_NUM, 0));

			//register 'this' as local 0
			registerLocal(THISFN, null, null);

			PSTATE state = PSTATE.REQ;
			PersistentVector argLocals = PersistentVector.EMPTY;
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
					LocalBinding lb = registerLocal(p, tagOf(p), null);
					argLocals = argLocals.cons(lb);
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
			LOOP_LOCALS.set(argLocals);
			method.argLocals = argLocals;
			method.body = (new BodyExpr.Parser()).parse(C.RETURN, body);
			return method;
			}
		finally
			{
			Var.popThreadBindings();
			}
	}

	public void emit(FnExpr fn, ClassVisitor cv){
		Method m = new Method(isVariadic() ? "doInvoke" : "invoke",
		                      OBJECT_TYPE, ARG_TYPES[numParams()]);

		GeneratorAdapter gen = new GeneratorAdapter(ACC_PUBLIC,
		                                            m,
		                                            null,
		                                            EXCEPTION_TYPES,
		                                            cv);
		gen.visitCode();
		Label loopLabel = gen.mark();
		gen.visitLineNumber(line, loopLabel);
		try
			{
			Var.pushThreadBindings(RT.map(LOOP_LABEL, loopLabel));
			body.emit(C.RETURN, fn, gen);
			Label end = gen.mark();
			gen.visitLocalVariable("this", "Ljava/lang/Object;", null, loopLabel, end, 0);
			for(ISeq lbs = argLocals.seq(); lbs != null; lbs = lbs.rest())
				{
				LocalBinding lb = (LocalBinding) lbs.first();
				gen.visitLocalVariable(lb.name, "Ljava/lang/Object;", null, loopLabel, end, lb.idx);
				}
			}
		finally
			{
			Var.popThreadBindings();
			}

		gen.returnValue();
		//gen.visitMaxs(1, 1);
		gen.endMethod();
	}
}

static class LocalBinding{
	final Symbol sym;
	final Symbol tag;
	final Expr init;
	final int idx;
	final String name;

	public LocalBinding(int num, Symbol sym, Symbol tag, Expr init){
		this.idx = num;
		this.sym = sym;
		this.tag = tag;
		this.init = init;
		name = munge(sym.name);
	}

	public boolean hasJavaClass() throws Exception{
		return tag != null
		       || (init != null && init.hasJavaClass());
	}

	public Class getJavaClass() throws Exception{
		return tag != null ? HostExpr.tagToClass(tag)
		       : init.getJavaClass();
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

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		if(context != C.STATEMENT)
			fn.emitLocal(gen, b);
	}

	public boolean hasJavaClass() throws Exception{
		return tag != null || b.hasJavaClass();
	}

	public Class getJavaClass() throws Exception{
		if(tag != null)
			return HostExpr.tagToClass(tag);
		return b.getJavaClass();
	}
}

static class BodyExpr implements Expr{
	PersistentVector exprs;

	public BodyExpr(PersistentVector exprs){
		this.exprs = exprs;
	}

	static class Parser implements IParser{
		public Expr parse(C context, Object frms) throws Exception{
			ISeq forms = (ISeq) frms;
			if(RT.equal(RT.first(forms), DO))
				forms = RT.rest(forms);
			PersistentVector exprs = PersistentVector.EMPTY;
			for(; forms != null; forms = forms.rest())
				{
				Expr e = (context != C.EVAL &&
				          (context == C.STATEMENT || forms.rest() != null)) ?
				                                                            analyze(C.STATEMENT, forms.first())
				                                                            :
				                                                            analyze(context, forms.first());
				exprs = exprs.cons(e);
				}
			if(exprs.count() == 0)
				exprs = exprs.cons(NIL_EXPR);
			return new BodyExpr(exprs);
		}
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

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		for(int i = 0; i < exprs.count() - 1; i++)
			{
			Expr e = (Expr) exprs.nth(i);
			e.emit(C.STATEMENT, fn, gen);
			}
		Expr last = (Expr) exprs.nth(exprs.count() - 1);
		last.emit(context, fn, gen);
	}

	public boolean hasJavaClass() throws Exception{
		return lastExpr().hasJavaClass();
	}

	public Class getJavaClass() throws Exception{
		return lastExpr().getJavaClass();
	}

	private Expr lastExpr(){
		return (Expr) exprs.nth(exprs.count() - 1);
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

	static class Parser implements IParser{
		public Expr parse(C context, Object frm) throws Exception{
			ISeq form = (ISeq) frm;
			//(let [var val var2 val2 ...] body...)
			boolean isLoop = RT.first(form).equals(LOOP);
			if(!(RT.second(form) instanceof IPersistentVector))
				throw new IllegalArgumentException("Bad binding form, expected vector");

			IPersistentVector bindings = (IPersistentVector) RT.second(form);
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
						throw new IllegalArgumentException(
								"Bad binding form, expected symbol, got: " + bindings.nth(i));
					Symbol sym = (Symbol) bindings.nth(i);

					Expr init = analyze(C.EXPRESSION, bindings.nth(i + 1), sym.name);
					//sequential enhancement of env (like Lisp let*)
					LocalBinding lb = registerLocal(sym, tagOf(sym), init);
					BindingInit bi = new BindingInit(lb, init);
					bindingInits = bindingInits.cons(bi);

					if(isLoop)
						loopLocals = loopLocals.cons(lb);
					}
				if(isLoop)
					LOOP_LOCALS.set(loopLocals);
				return new LetExpr(bindingInits, (new BodyExpr.Parser()).parse(isLoop ? C.RETURN : context, body),
				                   isLoop);
				}
			finally
				{
				Var.popThreadBindings();
				}
		}
	}

	public Object eval() throws Exception{
		throw new UnsupportedOperationException("Can't eval let/loop");
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		for(int i = 0; i < bindingInits.count(); i++)
			{
			BindingInit bi = (BindingInit) bindingInits.nth(i);
			bi.init.emit(C.EXPRESSION, fn, gen);
			gen.visitVarInsn(OBJECT_TYPE.getOpcode(Opcodes.ISTORE), bi.binding.idx);
			}
		Label loopLabel = gen.mark();
		if(isLoop)
			{
			try
				{
				Var.pushThreadBindings(RT.map(LOOP_LABEL, loopLabel));
				body.emit(context, fn, gen);
				}
			finally
				{
				Var.popThreadBindings();
				}
			}
		else
			body.emit(context, fn, gen);
		Label end = gen.mark();
		gen.visitLocalVariable("this", "Ljava/lang/Object;", null, loopLabel, end, 0);
		for(ISeq bis = bindingInits.seq(); bis != null; bis = bis.rest())
			{
			BindingInit bi = (BindingInit) bis.first();
			gen.visitLocalVariable(bi.binding.name, "Ljava/lang/Object;", null, loopLabel, end, bi.binding.idx);
			}
	}

	public boolean hasJavaClass() throws Exception{
		return body.hasJavaClass();
	}

	public Class getJavaClass() throws Exception{
		return body.getJavaClass();
	}
}

static class RecurExpr implements Expr{
	final IPersistentVector args;
	final IPersistentVector loopLocals;

	public RecurExpr(IPersistentVector loopLocals, IPersistentVector args){
		this.loopLocals = loopLocals;
		this.args = args;
	}

	public Object eval() throws Exception{
		throw new UnsupportedOperationException("Can't eval recur");
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		Label loopLabel = (Label) LOOP_LABEL.get();
		if(loopLabel == null)
			throw new IllegalStateException();
		for(int i = 0; i < loopLocals.count(); i++)
			{
			LocalBinding lb = (LocalBinding) loopLocals.nth(i);
			Expr arg = (Expr) args.nth(i);
			arg.emit(C.EXPRESSION, fn, gen);
			}

		for(int i = loopLocals.count() - 1; i >= 0; i--)
			{
			LocalBinding lb = (LocalBinding) loopLocals.nth(i);
			gen.visitVarInsn(OBJECT_TYPE.getOpcode(Opcodes.ISTORE), lb.idx);
			}

		gen.goTo(loopLabel);
	}

	public boolean hasJavaClass() throws Exception{
		return true;
	}

	public Class getJavaClass() throws Exception{
		return null;
	}

	static class Parser implements IParser{
		public Expr parse(C context, Object frm) throws Exception{
			ISeq form = (ISeq) frm;
			IPersistentVector loopLocals = (IPersistentVector) LOOP_LOCALS.get();
			if(context != C.RETURN || loopLocals == null)
				throw new UnsupportedOperationException("Can only recur from tail position");
			PersistentVector args = PersistentVector.EMPTY;
			for(ISeq s = RT.seq(form.rest()); s != null; s = s.rest())
				{
				args = args.cons(analyze(C.EXPRESSION, s.first()));
				}
			if(args.count() != loopLocals.count())
				throw new IllegalArgumentException(
						String.format("Mismatched argument count to recur, expected: %d args, got: %d",
						              loopLocals.count(), args.count()));
			return new RecurExpr(loopLocals, args);
		}
	}
}

private static LocalBinding registerLocal(Symbol sym, Symbol tag, Expr init) throws Exception{
	int num = getAndIncLocalNum();
	LocalBinding b = new LocalBinding(num, sym, tag, init);
	IPersistentMap localsMap = (IPersistentMap) LOCAL_ENV.get();
	LOCAL_ENV.set(RT.assoc(localsMap, b.sym, b));
	FnMethod method = (FnMethod) METHOD.get();
	method.locals = (IPersistentMap) RT.assoc(method.locals, b, b);
	return b;
}

private static int getAndIncLocalNum(){
	int num = ((Number) NEXT_LOCAL_NUM.get()).intValue();
	NEXT_LOCAL_NUM.set(num + 1);
	return num;
}

private static Expr analyze(C context, Object form) throws Exception{
	return analyze(context, form, null);
}

private static Expr analyze(C context, Object form, String name) throws Exception{
	//todo symbol macro expansion?
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
	else if(form instanceof IPersistentCollection && ((IPersistentCollection) form).count() == 0)
		return new EmptyExpr(form);
	else if(form instanceof ISeq)
		return analyzeSeq(context, (ISeq) form, name);
	else if(form instanceof IPersistentVector)
		return VectorExpr.parse(context, (IPersistentVector) form);
	else if(form instanceof IPersistentMap)
		return MapExpr.parse(context, (IPersistentMap) form);

//	else
	//throw new UnsupportedOperationException();
	return new QuoteExpr(form);
}

private static Expr analyzeSeq(C context, ISeq form, String name) throws Exception{
	Integer line = (Integer) LINE.get();
	try
		{
		if(RT.meta(form) != null && RT.meta(form).containsKey(LispReader.LINE_KEY))
			line = (Integer) RT.meta(form).valAt(LispReader.LINE_KEY);
		Var.pushThreadBindings(
				RT.map(LINE, line));
		Object op = RT.first(form);
		//macro expansion
		if(op instanceof Symbol || op instanceof Var)
			{
			Var v = (op instanceof Var) ? (Var) op : lookupVar((Symbol) op, false);
			if(v != null && v.isMacro())
				{
				return analyze(context, v.applyTo(form.rest()));
				}
			}
		IParser p;
		if(op.equals(FN))
			return FnExpr.parse(context, form, name);
		else if((p = (IParser) specials.valAt(op)) != null)
			return p.parse(context, form);
		else
			return InvokeExpr.parse(context, form);
		}
	catch(Throwable e)
		{
		throw new Exception(String.format("%s:%d: %s", SOURCE.get(), (Integer) LINE.get(), e.getMessage()),
		                    e);
		}
	finally
		{
		Var.popThreadBindings();
		}
}

public static Object eval(Object form) throws Exception{
	Expr expr = analyze(C.EVAL, form);
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
	Var v = lookupVar(sym, false);
	if(v != null)
		return new VarExpr(v, tag);
	throw new Exception("Unable to resolve symbol: " + sym + " in this context");

}

static Var lookupVar(Symbol sym, boolean internNew) throws Exception{
	Var var;

	//note - ns-qualified vars must already exist
	if(sym.ns != null)
		{
		var = Var.find(sym);
		}
	else
		{
		//is it an alias?
		IPersistentMap refers = (IPersistentMap) ((Var) RT.NS_REFERS.get()).get();
		var = (Var) refers.valAt(sym);
		if(var == null && sym.ns == null)
			var = Var.find(Symbol.intern(currentNS().name, sym.name));
		if(var == null && internNew)
			{
			//introduce a new var in the current ns
			String ns = currentNS().name;
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

static Symbol currentNS(){
	return (Symbol) RT.CURRENT_NS_SYM.get();
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

private static Symbol tagOf(Object o){
	if(o instanceof IObj)
		{
		IObj obj = (IObj) o;
		if(obj.meta() != null)
			return (Symbol) obj.meta().valAt(RT.TAG_KEY);
		}
	return null;
}

public static Object loadFile(String file) throws Exception{
	FileInputStream f = new FileInputStream(file);
	try
		{
		Var.pushThreadBindings(RT.map(SOURCE_PATH, file,
		                              SOURCE, (new File(file)).getName()));
		return load(f);
		}
	finally
		{
		Var.popThreadBindings();
		f.close();
		}
}

public static Object load(InputStream s) throws Exception{
	Object EOF = new Object();
	try
		{
		Var.pushThreadBindings(
				RT.map(LOADER, new DynamicClassLoader(),
				       RT.NS_REFERS, RT.NS_REFERS.get(),
				       RT.NS_IMPORTS, RT.NS_IMPORTS.get(),
				       RT.CURRENT_NS_SYM, RT.CURRENT_NS_SYM.get()
				));
		LineNumberingPushbackReader rdr = new LineNumberingPushbackReader(new InputStreamReader(s));
		for(Object r = LispReader.read(rdr, false, EOF, false); r != EOF; r = LispReader.read(rdr, false, EOF, false))
			eval(r);
		}
	finally
		{
		Var.popThreadBindings();
		}
	return RT.T;
}

public static void main(String[] args){

	for(String file : args)
		try
			{
			loadFile(file);
			}
		catch(Exception e)
			{
			e.printStackTrace();
			}

	//repl
	LineNumberingPushbackReader rdr = (LineNumberingPushbackReader) RT.IN.get();
	OutputStreamWriter w = (OutputStreamWriter) RT.OUT.get();//new OutputStreamWriter(System.out);

	Object EOF = new Object();
	try
		{
		Var.pushThreadBindings(
				RT.map(RT.NS_REFERS, RT.NS_REFERS.get(),
				       RT.NS_IMPORTS, RT.NS_IMPORTS.get(),
				       RT.CURRENT_NS_SYM, RT.CURRENT_NS_SYM.get(),
				       SOURCE, "REPL"
				));
		w.write("Clojure\n");
		RT.inNamespace.invoke(Symbol.create("user"));

		for(; ;)
			{
			try
				{
				Var.pushThreadBindings(
						RT.map(LOADER, new DynamicClassLoader()));
				w.write(currentNS().name + "=> ");
				w.flush();
				Object r = LispReader.read(rdr, false, EOF, false);
				if(r == EOF)
					break;
				Object ret = eval(r);
				RT.print(ret, w);
				w.write('\n');
				//w.flush();
				}
			catch(Throwable e)
				{
				e.printStackTrace();
				}
			finally
				{
				Var.popThreadBindings();
				}
			}
		}
	catch(Exception e)
		{
		e.printStackTrace();
		}
	finally
		{
		Var.popThreadBindings();
		}
}
}
