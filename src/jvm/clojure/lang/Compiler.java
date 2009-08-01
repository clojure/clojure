/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Aug 21, 2007 */

package clojure.lang;

//*

import clojure.asm.*;
import clojure.asm.commons.Method;
import clojure.asm.commons.GeneratorAdapter;
//*/
/*

import org.objectweb.asm.*;
import org.objectweb.asm.commons.Method;
import org.objectweb.asm.commons.GeneratorAdapter;
import org.objectweb.asm.util.TraceClassVisitor;
import org.objectweb.asm.util.CheckClassAdapter;
//*/

import java.io.*;
import java.util.*;
import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;

public class Compiler implements Opcodes{

static final Symbol DEF = Symbol.create("def");
static final Symbol LOOP = Symbol.create("loop*");
static final Symbol RECUR = Symbol.create("recur");
static final Symbol IF = Symbol.create("if");
static final Symbol LET = Symbol.create("let*");
static final Symbol LETFN = Symbol.create("letfn*");
static final Symbol DO = Symbol.create("do");
static final Symbol FN = Symbol.create("fn*");
static final Symbol QUOTE = Symbol.create("quote");
static final Symbol THE_VAR = Symbol.create("var");
static final Symbol DOT = Symbol.create(".");
static final Symbol ASSIGN = Symbol.create("set!");
//static final Symbol TRY_FINALLY = Symbol.create("try-finally");
static final Symbol TRY = Symbol.create("try");
static final Symbol CATCH = Symbol.create("catch");
static final Symbol FINALLY = Symbol.create("finally");
static final Symbol THROW = Symbol.create("throw");
static final Symbol MONITOR_ENTER = Symbol.create("monitor-enter");
static final Symbol MONITOR_EXIT = Symbol.create("monitor-exit");
static final Symbol IMPORT = Symbol.create("clojure.core", "import*");
//static final Symbol INSTANCE = Symbol.create("instance?");

//static final Symbol THISFN = Symbol.create("thisfn");
static final Symbol CLASS = Symbol.create("Class");
static final Symbol NEW = Symbol.create("new");
//static final Symbol UNQUOTE = Symbol.create("unquote");
//static final Symbol UNQUOTE_SPLICING = Symbol.create("unquote-splicing");
//static final Symbol SYNTAX_QUOTE = Symbol.create("clojure.core", "syntax-quote");
static final Symbol LIST = Symbol.create("clojure.core", "list");
static final Symbol HASHMAP = Symbol.create("clojure.core", "hash-map");
static final Symbol VECTOR = Symbol.create("clojure.core", "vector");
static final Symbol IDENTITY = Symbol.create("clojure.core", "identity");

static final Symbol _AMP_ = Symbol.create("&");
static final Symbol ISEQ = Symbol.create("clojure.lang.ISeq");

static final Keyword inlineKey = Keyword.intern(null, "inline");
static final Keyword inlineAritiesKey = Keyword.intern(null, "inline-arities");

static final Symbol NS = Symbol.create("ns");
static final Symbol IN_NS = Symbol.create("in-ns");

//static final Symbol IMPORT = Symbol.create("import");
//static final Symbol USE = Symbol.create("use");

//static final Symbol IFN = Symbol.create("clojure.lang", "IFn");

static final public IPersistentMap specials = PersistentHashMap.create(
		DEF, new DefExpr.Parser(),
		LOOP, new LetExpr.Parser(),
		RECUR, new RecurExpr.Parser(),
		IF, new IfExpr.Parser(),
		LET, new LetExpr.Parser(),
		LETFN, new LetFnExpr.Parser(),
		DO, new BodyExpr.Parser(),
		FN, null,
		QUOTE, new ConstantExpr.Parser(),
		THE_VAR, new TheVarExpr.Parser(),
		IMPORT, new ImportExpr.Parser(),
		DOT, new HostExpr.Parser(),
		ASSIGN, new AssignExpr.Parser(),
//		TRY_FINALLY, new TryFinallyExpr.Parser(),
TRY, new TryExpr.Parser(),
THROW, new ThrowExpr.Parser(),
MONITOR_ENTER, new MonitorEnterExpr.Parser(),
MONITOR_EXIT, new MonitorExitExpr.Parser(),
//		INSTANCE, new InstanceExpr.Parser(),
//		IDENTICAL, new IdenticalExpr.Parser(),
//THISFN, null,
CATCH, null,
FINALLY, null,
//		CLASS, new ClassExpr.Parser(),
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
//private static final Type NUM_TYPE = Type.getType(Num.class);
private static final Type IFN_TYPE = Type.getType(IFn.class);
private static final Type RT_TYPE = Type.getType(RT.class);
final static Type CLASS_TYPE = Type.getType(Class.class);
final static Type NS_TYPE = Type.getType(Namespace.class);
final static Type REFLECTOR_TYPE = Type.getType(Reflector.class);
final static Type THROWABLE_TYPE = Type.getType(Throwable.class);
final static Type BOOLEAN_OBJECT_TYPE = Type.getType(Boolean.class);
final static Type IPERSISTENTMAP_TYPE = Type.getType(IPersistentMap.class);
final static Type IOBJ_TYPE = Type.getType(IObj.class);

private static final Type[][] ARG_TYPES;
private static final Type[] EXCEPTION_TYPES = {Type.getType(Exception.class)};

static
	{
	OBJECT_TYPE = Type.getType(Object.class);
	ARG_TYPES = new Type[MAX_POSITIONAL_ARITY + 2][];
	for(int i = 0; i <= MAX_POSITIONAL_ARITY; ++i)
		{
		Type[] a = new Type[i];
		for(int j = 0; j < i; j++)
			a[j] = OBJECT_TYPE;
		ARG_TYPES[i] = a;
		}
	Type[] a = new Type[MAX_POSITIONAL_ARITY + 1];
	for(int j = 0; j < MAX_POSITIONAL_ARITY; j++)
		a[j] = OBJECT_TYPE;
	a[MAX_POSITIONAL_ARITY] = Type.getType("[Ljava/lang/Object;");
	ARG_TYPES[MAX_POSITIONAL_ARITY + 1] = a;


	}


//symbol->localbinding
static final public Var LOCAL_ENV = Var.create(null);

//vector<localbinding>
static final public Var LOOP_LOCALS = Var.create();

//Label
static final public Var LOOP_LABEL = Var.create();

//vector<object>
static final public Var CONSTANTS = Var.create();

//keyword->constid
static final public Var KEYWORDS = Var.create();

//var->constid
static final public Var VARS = Var.create();

//FnFrame
static final public Var METHOD = Var.create(null);

//null or not
static final public Var IN_CATCH_FINALLY = Var.create(null);

//DynamicClassLoader
static final public Var LOADER = Var.create();

//String
static final public Var SOURCE = Var.intern(Namespace.findOrCreate(Symbol.create("clojure.core")),
                                            Symbol.create("*source-path*"), "NO_SOURCE_FILE");

//String
static final public Var SOURCE_PATH = Var.intern(Namespace.findOrCreate(Symbol.create("clojure.core")),
                                                 Symbol.create("*file*"), "NO_SOURCE_PATH");

//String
static final public Var COMPILE_PATH = Var.intern(Namespace.findOrCreate(Symbol.create("clojure.core")),
                                                  Symbol.create("*compile-path*"), null);
//boolean
static final public Var COMPILE_FILES = Var.intern(Namespace.findOrCreate(Symbol.create("clojure.core")),
                                                   Symbol.create("*compile-files*"), Boolean.FALSE);

//Integer
static final public Var LINE = Var.create(0);

//Integer
static final public Var LINE_BEFORE = Var.create(0);
static final public Var LINE_AFTER = Var.create(0);

//Integer
static final public Var NEXT_LOCAL_NUM = Var.create(0);

//Integer
static final public Var RET_LOCAL_NUM = Var.create();


public enum C{
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

public static abstract class UntypedExpr implements Expr{

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
	if(sym.name.indexOf('.') > 0)
		return sym;
	if(sym.ns != null)
		{
		Namespace ns = namespaceFor(sym);
		if(ns == null || ns.name.name == sym.ns)
			return sym;
		return Symbol.create(ns.name.name, sym.name);
		}
	Object o = currentNS().getMapping(sym);
	if(o == null)
		return Symbol.intern(currentNS().name.name, sym.name);
	else if(o instanceof Class)
		return Symbol.intern(null, ((Class) o).getName());
	else if(o instanceof Var)
			{
			Var v = (Var) o;
			return Symbol.create(v.ns.name.name, v.sym.name);
			}
	return null;

}

static class DefExpr implements Expr{
	public final Var var;
	public final Expr init;
	public final Expr meta;
	public final boolean initProvided;
	public final String source;
	public final int line;
	final static Method bindRootMethod = Method.getMethod("void bindRoot(Object)");
	final static Method setTagMethod = Method.getMethod("void setTag(clojure.lang.Symbol)");
	final static Method setMetaMethod = Method.getMethod("void setMeta(clojure.lang.IPersistentMap)");
	final static Method symcreate = Method.getMethod("clojure.lang.Symbol create(String, String)");

	public DefExpr(String source, int line, Var var, Expr init, Expr meta, boolean initProvided){
		this.source = source;
		this.line = line;
		this.var = var;
		this.init = init;
		this.meta = meta;
		this.initProvided = initProvided;
	}

	public Object eval() throws Exception{
		try
			{
			if(initProvided)
				{
//			if(init instanceof FnExpr && ((FnExpr) init).closes.count()==0)
//				var.bindRoot(new FnLoaderThunk((FnExpr) init,var));
//			else
				var.bindRoot(init.eval());
				}
			if(meta != null)
				{
				var.setMeta((IPersistentMap) meta.eval());
				}
			return var;
			}
		catch(Throwable e)
			{
			if(!(e instanceof CompilerException))
				throw new CompilerException(source, line, e);
			else
				throw (CompilerException) e;
			}
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		fn.emitVar(gen, var);
		if(initProvided)
			{
			gen.dup();
			init.emit(C.EXPRESSION, fn, gen);
			gen.invokeVirtual(VAR_TYPE, bindRootMethod);
			}
		if(meta != null)
			{
			gen.dup();
			meta.emit(C.EXPRESSION, fn, gen);
			gen.checkCast(IPERSISTENTMAP_TYPE);
			gen.invokeVirtual(VAR_TYPE, setMetaMethod);
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
			if(!v.ns.equals(currentNS()))
				{
				if(sym.ns == null)
					throw new Exception("Name conflict, can't def " + sym + " because namespace: " + currentNS().name +
					                    " refers to:" + v);
				else
					throw new Exception("Can't create defs outside of current ns");
				}
			IPersistentMap mm = sym.meta();
            Object source_path = SOURCE_PATH.get();
            source_path = source_path == null ? "NO_SOURCE_FILE" : source_path;
            mm = (IPersistentMap) RT.assoc(mm, RT.LINE_KEY, LINE.get()).assoc(RT.FILE_KEY, source_path);
			Expr meta = analyze(context == C.EVAL ? context : C.EXPRESSION, mm);
			return new DefExpr((String) SOURCE.deref(), (Integer) LINE.deref(),
			                   v, analyze(context == C.EVAL ? context : C.EXPRESSION, RT.third(form), v.sym.name),
			                   meta, RT.count(form) == 3);
		}
	}
}

public static class AssignExpr implements Expr{
	public final AssignableExpr target;
	public final Expr val;

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
				throw new IllegalArgumentException("Malformed assignment, expecting (set! target val)");
			Expr target = analyze(C.EXPRESSION, RT.second(form));
			if(!(target instanceof AssignableExpr))
				throw new IllegalArgumentException("Invalid assignment target");
			return new AssignExpr((AssignableExpr) target, analyze(C.EXPRESSION, RT.third(form)));
		}
	}
}

public static class VarExpr implements Expr, AssignableExpr{
	public final Var var;
	public final Object tag;
	final static Method getMethod = Method.getMethod("Object get()");
	final static Method setMethod = Method.getMethod("Object set(Object)");

	public VarExpr(Var var, Symbol tag){
		this.var = var;
		this.tag = tag != null ? tag : var.getTag();
	}

	public Object eval() throws Exception{
		return var.deref();
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		fn.emitVar(gen, var);
		gen.invokeVirtual(VAR_TYPE, getMethod);
		if(context == C.STATEMENT)
			{
			gen.pop();
			}
	}

	public boolean hasJavaClass(){
		return tag != null;
	}

	public Class getJavaClass() throws Exception{
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

public static class TheVarExpr implements Expr{
	public final Var var;

	public TheVarExpr(Var var){
		this.var = var;
	}

	public Object eval() throws Exception{
		return var;
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		fn.emitVar(gen, var);
		if(context == C.STATEMENT)
			gen.pop();
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

public static class KeywordExpr implements Expr{
	public final Keyword k;

	public KeywordExpr(Keyword k){
		this.k = k;
	}

	public Object eval() throws Exception{
		return k;
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		fn.emitKeyword(gen, k);
		if(context == C.STATEMENT)
			gen.pop();

	}

	public boolean hasJavaClass(){
		return true;
	}

	public Class getJavaClass() throws ClassNotFoundException{
		return Keyword.class;
	}
}

public static class ImportExpr implements Expr{
	public final String c;
	final static Method forNameMethod = Method.getMethod("Class forName(String)");
	final static Method importClassMethod = Method.getMethod("Class importClass(Class)");
	final static Method derefMethod = Method.getMethod("Object deref()");

	public ImportExpr(String c){
		this.c = c;
	}

	public Object eval() throws Exception{
		Namespace ns = (Namespace) RT.CURRENT_NS.deref();
		ns.importClass(RT.classForName(c));
		return null;
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		gen.getStatic(RT_TYPE,"CURRENT_NS",VAR_TYPE);
		gen.invokeVirtual(VAR_TYPE, derefMethod);
		gen.checkCast(NS_TYPE);
		gen.push(c);
		gen.invokeStatic(CLASS_TYPE, forNameMethod);
		gen.invokeVirtual(NS_TYPE, importClassMethod);
		if(context == C.STATEMENT)
			gen.pop();
	}

	public boolean hasJavaClass(){
		return false;
	}

	public Class getJavaClass() throws ClassNotFoundException{
		throw new IllegalArgumentException("ImportExpr has no Java class");
	}

	static class Parser implements IParser{
		public Expr parse(C context, Object form) throws Exception{
			return new ImportExpr((String) RT.second(form));
		}
	}
}

public static abstract class LiteralExpr implements Expr{
	abstract Object val();

	public Object eval(){
		return val();
	}
}

static interface AssignableExpr{
	Object evalAssign(Expr val) throws Exception;

	void emitAssign(C context, FnExpr fn, GeneratorAdapter gen, Expr val);
}

static public interface MaybePrimitiveExpr{
	public void emitUnboxed(C context, FnExpr fn, GeneratorAdapter gen);
}

static public abstract class HostExpr implements Expr, MaybePrimitiveExpr{
	final static Type BOOLEAN_TYPE = Type.getType(Boolean.class);
	final static Type CHAR_TYPE = Type.getType(Character.class);
	final static Type INTEGER_TYPE = Type.getType(Integer.class);
	final static Type LONG_TYPE = Type.getType(Long.class);
	final static Type FLOAT_TYPE = Type.getType(Float.class);
	final static Type DOUBLE_TYPE = Type.getType(Double.class);
	final static Type SHORT_TYPE = Type.getType(Short.class);
	final static Type BYTE_TYPE = Type.getType(Byte.class);
	final static Type NUMBER_TYPE = Type.getType(Number.class);

	final static Method charValueMethod = Method.getMethod("char charValue()");
	final static Method booleanValueMethod = Method.getMethod("boolean booleanValue()");

	final static Method charValueOfMethod = Method.getMethod("Character valueOf(char)");
	final static Method intValueOfMethod = Method.getMethod("Integer valueOf(int)");
	final static Method longValueOfMethod = Method.getMethod("Long valueOf(long)");
	final static Method floatValueOfMethod = Method.getMethod("Float valueOf(float)");
	final static Method doubleValueOfMethod = Method.getMethod("Double valueOf(double)");
	final static Method shortValueOfMethod = Method.getMethod("Short valueOf(short)");
	final static Method byteValueOfMethod = Method.getMethod("Byte valueOf(byte)");

	final static Method intValueMethod = Method.getMethod("int intValue()");
	final static Method longValueMethod = Method.getMethod("long longValue()");
	final static Method floatValueMethod = Method.getMethod("float floatValue()");
	final static Method doubleValueMethod = Method.getMethod("double doubleValue()");
	final static Method byteValueMethod = Method.getMethod("byte byteValue()");
	final static Method shortValueMethod = Method.getMethod("short shortValue()");

	final static Method fromIntMethod = Method.getMethod("clojure.lang.Num from(int)");
	final static Method fromLongMethod = Method.getMethod("clojure.lang.Num from(long)");
	final static Method fromDoubleMethod = Method.getMethod("clojure.lang.Num from(double)");


	/*
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
				gen.invokeStatic(CHAR_TYPE, charValueOfMethod);
				}
			else if(returnType == int.class)
				gen.invokeStatic(INTEGER_TYPE, intValueOfMethod);
				//m = fromIntMethod;
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
  */
	//*
	public static void emitBoxReturn(FnExpr fn, GeneratorAdapter gen, Class returnType){
		if(returnType.isPrimitive())
			{
			if(returnType == boolean.class)
				{
				Label falseLabel = gen.newLabel();
				Label endLabel = gen.newLabel();
				gen.ifZCmp(GeneratorAdapter.EQ, falseLabel);
				gen.getStatic(BOOLEAN_OBJECT_TYPE, "TRUE", BOOLEAN_OBJECT_TYPE);
				gen.goTo(endLabel);
				gen.mark(falseLabel);
				gen.getStatic(BOOLEAN_OBJECT_TYPE, "FALSE", BOOLEAN_OBJECT_TYPE);
//				NIL_EXPR.emit(C.EXPRESSION, fn, gen);
				gen.mark(endLabel);
				}
			else if(returnType == void.class)
				{
				NIL_EXPR.emit(C.EXPRESSION, fn, gen);
				}
			else if(returnType == char.class)
					{
					gen.invokeStatic(CHAR_TYPE, charValueOfMethod);
					}
				else
					{
					if(returnType == int.class)
					//gen.invokeStatic(NUM_TYPE, fromIntMethod);
						gen.invokeStatic(INTEGER_TYPE, intValueOfMethod);
					else if(returnType == float.class)
						{
						//gen.visitInsn(F2D);
						gen.invokeStatic(FLOAT_TYPE, floatValueOfMethod);
						//m = floatValueOfMethod;
						}
					else if(returnType == double.class)
							gen.invokeStatic(DOUBLE_TYPE, doubleValueOfMethod);
						else if(returnType == long.class)
								gen.invokeStatic(LONG_TYPE, longValueOfMethod);
							else if(returnType == byte.class)
									gen.invokeStatic(BYTE_TYPE, byteValueOfMethod);
								else if(returnType == short.class)
										gen.invokeStatic(SHORT_TYPE, shortValueOfMethod);
					}
			}
	}

	//*/
	public static void emitUnboxArg(FnExpr fn, GeneratorAdapter gen, Class paramType){
		if(paramType.isPrimitive())
			{
			if(paramType == boolean.class)
				{
				gen.checkCast(BOOLEAN_TYPE);
				gen.invokeVirtual(BOOLEAN_TYPE, booleanValueMethod);
//				Label falseLabel = gen.newLabel();
//				Label endLabel = gen.newLabel();
//				gen.ifNull(falseLabel);
//				gen.push(1);
//				gen.goTo(endLabel);
//				gen.mark(falseLabel);
//				gen.push(0);
//				gen.mark(endLabel);
				}
			else if(paramType == char.class)
				{
				gen.checkCast(CHAR_TYPE);
				gen.invokeVirtual(CHAR_TYPE, charValueMethod);
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
			//(. x fieldname-sym) or
			//(. x 0-ary-method)
			// (. x methodname-sym args+)
			// (. x (methodname-sym args?))
			if(RT.length(form) < 3)
				throw new IllegalArgumentException("Malformed member expression, expecting (. target member ...)");
			//determine static or instance
			//static target must be symbol, either fully.qualified.Classname or Classname that has been imported
			int line = (Integer) LINE.deref();
			String source = (String) SOURCE.deref();
			Class c = maybeClass(RT.second(form), false);
			//at this point c will be non-null if static
			Expr instance = null;
			if(c == null)
				instance = analyze(context == C.EVAL ? context : C.EXPRESSION, RT.second(form));
			boolean maybeField = RT.length(form) == 3 && RT.third(form) instanceof Symbol;
			if(maybeField)
				{
				Symbol sym = (Symbol) RT.third(form);
				if(c != null)
					maybeField = Reflector.getMethods(c, 0, sym.name, true).size() == 0;
				else if(instance != null && instance.hasJavaClass() && instance.getJavaClass() != null)
					maybeField = Reflector.getMethods(instance.getJavaClass(), 0, sym.name, false).size() == 0;
				}
			if(maybeField)    //field
				{
				Symbol sym = (Symbol) RT.third(form);
				if(c != null)
					return new StaticFieldExpr(line, c, sym.name);
				else
					return new InstanceFieldExpr(line, instance, sym.name);
				}
			else
				{
				ISeq call = (ISeq) ((RT.third(form) instanceof ISeq) ? RT.third(form) : RT.next(RT.next(form)));
				if(!(RT.first(call) instanceof Symbol))
					throw new IllegalArgumentException("Malformed member expression");
				Symbol sym = (Symbol) RT.first(call);
				PersistentVector args = PersistentVector.EMPTY;
				for(ISeq s = RT.next(call); s != null; s = s.next())
					args = args.cons(analyze(context == C.EVAL ? context : C.EXPRESSION, s.first()));
				if(c != null)
					return new StaticMethodExpr(source, line, c, sym.name, args);
				else
					return new InstanceMethodExpr(source, line, instance, sym.name, args);
				}
		}
	}

	private static Class maybeClass(Object form, boolean stringOk) throws Exception{
		if(form instanceof Class)
			return (Class) form;
		Class c = null;
		if(form instanceof Symbol)
			{
			Symbol sym = (Symbol) form;
			if(sym.ns == null) //if ns-qualified can't be classname
				{
				if(sym.name.indexOf('.') > 0 || sym.name.charAt(0) == '[')
					c = RT.classForName(sym.name);
				else
					{
					Object o = currentNS().getMapping(sym);
					if(o instanceof Class)
						c = (Class) o;
					}
				}
			}
		else if(stringOk && form instanceof String)
			c = RT.classForName((String) form);
		return c;
	}

	/*
	 private static String maybeClassName(Object form, boolean stringOk){
		 String className = null;
		 if(form instanceof Symbol)
			 {
			 Symbol sym = (Symbol) form;
			 if(sym.ns == null) //if ns-qualified can't be classname
				 {
				 if(sym.name.indexOf('.') > 0 || sym.name.charAt(0) == '[')
					 className = sym.name;
				 else
					 {
					 IPersistentMap imports = (IPersistentMap) ((Var) RT.NS_IMPORTS.get()).get();
					 className = (String) imports.valAt(sym);
					 }
				 }
			 }
		 else if(stringOk && form instanceof String)
			 className = (String) form;
		 return className;
	 }
 */
	static Class tagToClass(Object tag) throws Exception{
		Class c = maybeClass(tag, true);
		if(tag instanceof Symbol)
			{
			Symbol sym = (Symbol) tag;
			if(sym.ns == null) //if ns-qualified can't be classname
				{
				if(sym.name.equals("objects"))
					c = Object[].class;
				else if(sym.name.equals("ints"))
					c = int[].class;
				else if(sym.name.equals("longs"))
					c = long[].class;
				else if(sym.name.equals("floats"))
						c = float[].class;
					else if(sym.name.equals("doubles"))
							c = double[].class;
						else if(sym.name.equals("chars"))
								c = char[].class;
							else if(sym.name.equals("shorts"))
									c = short[].class;
								else if(sym.name.equals("bytes"))
										c = byte[].class;
									else if(sym.name.equals("booleans"))
											c = boolean[].class;
				}
			}
		if(c != null)
			return c;
		throw new IllegalArgumentException("Unable to resolve classname: " + tag);
	}
}

static abstract class FieldExpr extends HostExpr{
}

static class InstanceFieldExpr extends FieldExpr implements AssignableExpr{
	public final Expr target;
	public final Class targetClass;
	public final java.lang.reflect.Field field;
	public final String fieldName;
	public final int line;
	final static Method invokeNoArgInstanceMember = Method.getMethod("Object invokeNoArgInstanceMember(Object,String)");
	final static Method setInstanceFieldMethod = Method.getMethod("Object setInstanceField(Object,String,Object)");


	public InstanceFieldExpr(int line, Expr target, String fieldName) throws Exception{
		this.target = target;
		this.targetClass = target.hasJavaClass() ? target.getJavaClass() : null;
		this.field = targetClass != null ? Reflector.getField(targetClass, fieldName, false) : null;
		this.fieldName = fieldName;
		this.line = line;
		if(field == null && RT.booleanCast(RT.WARN_ON_REFLECTION.deref()))
			{
			((PrintWriter) RT.ERR.deref())
					.format("Reflection warning, %s:%d - reference to field %s can't be resolved.\n",
							SOURCE_PATH.deref(), line, fieldName);
			}
	}

	public Object eval() throws Exception{
		return Reflector.invokeNoArgInstanceMember(target.eval(), fieldName);
	}

	public void emitUnboxed(C context, FnExpr fn, GeneratorAdapter gen){
		gen.visitLineNumber(line, gen.mark());
		if(targetClass != null && field != null)
			{
			target.emit(C.EXPRESSION, fn, gen);
			gen.checkCast(Type.getType(targetClass));
			gen.getField(Type.getType(targetClass), fieldName, Type.getType(field.getType()));
			}
		else
			throw new UnsupportedOperationException("Unboxed emit of unknown member");
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		gen.visitLineNumber(line, gen.mark());
		if(targetClass != null && field != null)
			{
			target.emit(C.EXPRESSION, fn, gen);
			gen.checkCast(Type.getType(targetClass));
			gen.getField(Type.getType(targetClass), fieldName, Type.getType(field.getType()));
			//if(context != C.STATEMENT)
			HostExpr.emitBoxReturn(fn, gen, field.getType());
			if(context == C.STATEMENT)
				{
				gen.pop();
				}
			}
		else
			{
			target.emit(C.EXPRESSION, fn, gen);
			gen.push(fieldName);
			gen.invokeStatic(REFLECTOR_TYPE, invokeNoArgInstanceMember);
			if(context == C.STATEMENT)
				gen.pop();
			}
	}

	public boolean hasJavaClass() throws Exception{
		return field != null;
	}

	public Class getJavaClass() throws Exception{
		return field.getType();
	}

	public Object evalAssign(Expr val) throws Exception{
		return Reflector.setInstanceField(target.eval(), fieldName, val.eval());
	}

	public void emitAssign(C context, FnExpr fn, GeneratorAdapter gen,
	                       Expr val){
		gen.visitLineNumber(line, gen.mark());
		if(targetClass != null && field != null)
			{
			target.emit(C.EXPRESSION, fn, gen);
			gen.checkCast(Type.getType(targetClass));
			val.emit(C.EXPRESSION, fn, gen);
			gen.dupX1();
			HostExpr.emitUnboxArg(fn, gen, field.getType());
			gen.putField(Type.getType(targetClass), fieldName, Type.getType(field.getType()));
			}
		else
			{
			target.emit(C.EXPRESSION, fn, gen);
			gen.push(fieldName);
			val.emit(C.EXPRESSION, fn, gen);
			gen.invokeStatic(REFLECTOR_TYPE, setInstanceFieldMethod);
			}
		if(context == C.STATEMENT)
			gen.pop();
	}
}

static class StaticFieldExpr extends FieldExpr implements AssignableExpr{
	//final String className;
	public final String fieldName;
	public final Class c;
	public final java.lang.reflect.Field field;
//	final static Method getStaticFieldMethod = Method.getMethod("Object getStaticField(String,String)");
//	final static Method setStaticFieldMethod = Method.getMethod("Object setStaticField(String,String,Object)");
	final int line;

	public StaticFieldExpr(int line, Class c, String fieldName) throws Exception{
		//this.className = className;
		this.fieldName = fieldName;
		this.line = line;
		//c = Class.forName(className);
		this.c = c;
		field = c.getField(fieldName);
	}

	public Object eval() throws Exception{
		return Reflector.getStaticField(c, fieldName);
	}

	public void emitUnboxed(C context, FnExpr fn, GeneratorAdapter gen){
		gen.visitLineNumber(line, gen.mark());
		gen.getStatic(Type.getType(c), fieldName, Type.getType(field.getType()));
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		gen.visitLineNumber(line, gen.mark());

		gen.getStatic(Type.getType(c), fieldName, Type.getType(field.getType()));
		//if(context != C.STATEMENT)
		HostExpr.emitBoxReturn(fn, gen, field.getType());
		if(context == C.STATEMENT)
			{
			gen.pop();
			}
//		gen.push(className);
//		gen.push(fieldName);
//		gen.invokeStatic(REFLECTOR_TYPE, getStaticFieldMethod);
	}

	public boolean hasJavaClass(){
		return true;
	}

	public Class getJavaClass() throws Exception{
		//Class c = Class.forName(className);
		//java.lang.reflect.Field field = c.getField(fieldName);
		return field.getType();
	}

	public Object evalAssign(Expr val) throws Exception{
		return Reflector.setStaticField(c, fieldName, val.eval());
	}

	public void emitAssign(C context, FnExpr fn, GeneratorAdapter gen,
	                       Expr val){
		gen.visitLineNumber(line, gen.mark());
		val.emit(C.EXPRESSION, fn, gen);
		gen.dup();
		HostExpr.emitUnboxArg(fn, gen, field.getType());
		gen.putStatic(Type.getType(c), fieldName, Type.getType(field.getType()));
		if(context == C.STATEMENT)
			gen.pop();
	}


}

static Class maybePrimitiveType(Expr e){
	try
		{
		if(e instanceof MaybePrimitiveExpr && e.hasJavaClass())
			{
			Class c = e.getJavaClass();
			if(Util.isPrimitive(c))
				return c;
			}
		}
	catch(Exception ex)
		{
		throw new RuntimeException(ex);
		}
	return null;
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
			try
				{
				if(maybePrimitiveType(e) == parameterTypes[i])
					{
					((MaybePrimitiveExpr) e).emitUnboxed(C.EXPRESSION, fn, gen);
					}
				else
					{
					e.emit(C.EXPRESSION, fn, gen);
					HostExpr.emitUnboxArg(fn, gen, parameterTypes[i]);
					}
				}
			catch(Exception e1)
				{
				e1.printStackTrace((PrintWriter) RT.ERR
						.deref());  //To change body of catch statement use File | Settings | File Templates.
				}

			}
	}
}

static class InstanceMethodExpr extends MethodExpr{
	public final Expr target;
	public final String methodName;
	public final IPersistentVector args;
	public final String source;
	public final int line;
	public final java.lang.reflect.Method method;

	final static Method invokeInstanceMethodMethod =
			Method.getMethod("Object invokeInstanceMethod(Object,String,Object[])");


	public InstanceMethodExpr(String source, int line, Expr target, String methodName, IPersistentVector args)
			throws Exception{
		this.source = source;
		this.line = line;
		this.args = args;
		this.methodName = methodName;
		this.target = target;
		if(target.hasJavaClass() && target.getJavaClass() != null)
			{
			List methods = Reflector.getMethods(target.getJavaClass(), args.count(), methodName, false);
			if(methods.isEmpty())
				method = null;
			//throw new IllegalArgumentException("No matching method found");
			else
				{
				int methodidx = 0;
				if(methods.size() > 1)
					{
					ArrayList<Class[]> params = new ArrayList();
					ArrayList<Class> rets = new ArrayList();
					for(int i = 0; i < methods.size(); i++)
						{
						java.lang.reflect.Method m = (java.lang.reflect.Method) methods.get(i);
						params.add(m.getParameterTypes());
						rets.add(m.getReturnType());
						}
					methodidx = getMatchingParams(methodName, params, args, rets);
					}
				java.lang.reflect.Method m =
						(java.lang.reflect.Method) (methodidx >= 0 ? methods.get(methodidx) : null);
				if(m != null && !Modifier.isPublic(m.getDeclaringClass().getModifiers()))
					{
					//public method of non-public class, try to find it in hierarchy
					m = Reflector.getAsMethodOfPublicBase(m.getDeclaringClass(), m);
					}
				method = m;
				}
			}
		else
			method = null;

		if(method == null && RT.booleanCast(RT.WARN_ON_REFLECTION.deref()))
			{
			((PrintWriter) RT.ERR.deref())
					.format("Reflection warning, %s:%d - call to %s can't be resolved.\n",
							SOURCE_PATH.deref(), line, methodName);
			}
	}

	public Object eval() throws Exception{
		try
			{
			Object targetval = target.eval();
			Object[] argvals = new Object[args.count()];
			for(int i = 0; i < args.count(); i++)
				argvals[i] = ((Expr) args.nth(i)).eval();
			if(method != null)
				{
				LinkedList ms = new LinkedList();
				ms.add(method);
				return Reflector.invokeMatchingMethod(methodName, ms, targetval, argvals);
				}
			return Reflector.invokeInstanceMethod(targetval, methodName, argvals);
			}
		catch(Throwable e)
			{
			if(!(e instanceof CompilerException))
				throw new CompilerException(source, line, e);
			else
				throw (CompilerException) e;
			}
	}

	public void emitUnboxed(C context, FnExpr fn, GeneratorAdapter gen){
		gen.visitLineNumber(line, gen.mark());
		if(method != null)
			{
			Type type = Type.getType(method.getDeclaringClass());
			target.emit(C.EXPRESSION, fn, gen);
			//if(!method.getDeclaringClass().isInterface())
			gen.checkCast(type);
			MethodExpr.emitTypedArgs(fn, gen, method.getParameterTypes(), args);
			if(context == C.RETURN)
				{
				FnMethod method = (FnMethod) METHOD.deref();
				method.emitClearLocals(gen);
				}
			Method m = new Method(methodName, Type.getReturnType(method), Type.getArgumentTypes(method));
			if(method.getDeclaringClass().isInterface())
				gen.invokeInterface(type, m);
			else
				gen.invokeVirtual(type, m);
			}
		else
			throw new UnsupportedOperationException("Unboxed emit of unknown member");
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		gen.visitLineNumber(line, gen.mark());
		if(method != null)
			{
			Type type = Type.getType(method.getDeclaringClass());
			target.emit(C.EXPRESSION, fn, gen);
			//if(!method.getDeclaringClass().isInterface())
			gen.checkCast(type);
			MethodExpr.emitTypedArgs(fn, gen, method.getParameterTypes(), args);
			if(context == C.RETURN)
				{
				FnMethod method = (FnMethod) METHOD.deref();
				method.emitClearLocals(gen);
				}
			Method m = new Method(methodName, Type.getReturnType(method), Type.getArgumentTypes(method));
			if(method.getDeclaringClass().isInterface())
				gen.invokeInterface(type, m);
			else
				gen.invokeVirtual(type, m);
			//if(context != C.STATEMENT || method.getReturnType() == Void.TYPE)
			HostExpr.emitBoxReturn(fn, gen, method.getReturnType());
			}
		else
			{
			target.emit(C.EXPRESSION, fn, gen);
			gen.push(methodName);
			emitArgsAsArray(args, fn, gen);
			if(context == C.RETURN)
				{
				FnMethod method = (FnMethod) METHOD.deref();
				method.emitClearLocals(gen);
				}
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
	//final String className;
	public final Class c;
	public final String methodName;
	public final IPersistentVector args;
	public final String source;
	public final int line;
	public final java.lang.reflect.Method method;
	final static Method forNameMethod = Method.getMethod("Class forName(String)");
	final static Method invokeStaticMethodMethod =
			Method.getMethod("Object invokeStaticMethod(Class,String,Object[])");


	public StaticMethodExpr(String source, int line, Class c, String methodName, IPersistentVector args)
			throws Exception{
		this.c = c;
		this.methodName = methodName;
		this.args = args;
		this.source = source;
		this.line = line;

		List methods = Reflector.getMethods(c, args.count(), methodName, true);
		if(methods.isEmpty())
			throw new IllegalArgumentException("No matching method: " + methodName);

		int methodidx = 0;
		if(methods.size() > 1)
			{
			ArrayList<Class[]> params = new ArrayList();
			ArrayList<Class> rets = new ArrayList();
			for(int i = 0; i < methods.size(); i++)
				{
				java.lang.reflect.Method m = (java.lang.reflect.Method) methods.get(i);
				params.add(m.getParameterTypes());
				rets.add(m.getReturnType());
				}
			methodidx = getMatchingParams(methodName, params, args, rets);
			}
		method = (java.lang.reflect.Method) (methodidx >= 0 ? methods.get(methodidx) : null);
		if(method == null && RT.booleanCast(RT.WARN_ON_REFLECTION.deref()))
			{
			((PrintWriter) RT.ERR.deref())
					.format("Reflection warning, %s:%d - call to %s can't be resolved.\n",
							SOURCE_PATH.deref(), line, methodName);
			}
	}

	public Object eval() throws Exception{
		try
			{
			Object[] argvals = new Object[args.count()];
			for(int i = 0; i < args.count(); i++)
				argvals[i] = ((Expr) args.nth(i)).eval();
			if(method != null)
				{
				LinkedList ms = new LinkedList();
				ms.add(method);
				return Reflector.invokeMatchingMethod(methodName, ms, null, argvals);
				}
			return Reflector.invokeStaticMethod(c, methodName, argvals);
			}
		catch(Throwable e)
			{
			if(!(e instanceof CompilerException))
				throw new CompilerException(source, line, e);
			else
				throw (CompilerException) e;
			}
	}

	public void emitUnboxed(C context, FnExpr fn, GeneratorAdapter gen){
		gen.visitLineNumber(line, gen.mark());
		if(method != null)
			{
			MethodExpr.emitTypedArgs(fn, gen, method.getParameterTypes(), args);
			//Type type = Type.getObjectType(className.replace('.', '/'));
			if(context == C.RETURN)
				{
				FnMethod method = (FnMethod) METHOD.deref();
				method.emitClearLocals(gen);
				}
			Type type = Type.getType(c);
			Method m = new Method(methodName, Type.getReturnType(method), Type.getArgumentTypes(method));
			gen.invokeStatic(type, m);
			}
		else
			throw new UnsupportedOperationException("Unboxed emit of unknown member");
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		gen.visitLineNumber(line, gen.mark());
		if(method != null)
			{
			MethodExpr.emitTypedArgs(fn, gen, method.getParameterTypes(), args);
			//Type type = Type.getObjectType(className.replace('.', '/'));
			if(context == C.RETURN)
				{
				FnMethod method = (FnMethod) METHOD.deref();
				method.emitClearLocals(gen);
				}
			Type type = Type.getType(c);
			Method m = new Method(methodName, Type.getReturnType(method), Type.getArgumentTypes(method));
			gen.invokeStatic(type, m);
			//if(context != C.STATEMENT || method.getReturnType() == Void.TYPE)
			HostExpr.emitBoxReturn(fn, gen, method.getReturnType());
			}
		else
			{
			gen.push(c.getName());
			gen.invokeStatic(CLASS_TYPE, forNameMethod);
			gen.push(methodName);
			emitArgsAsArray(args, fn, gen);
			if(context == C.RETURN)
				{
				FnMethod method = (FnMethod) METHOD.deref();
				method.emitClearLocals(gen);
				}
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

static class UnresolvedVarExpr implements Expr{
	public final Symbol symbol;

	public UnresolvedVarExpr(Symbol symbol){
		this.symbol = symbol;
	}

	public boolean hasJavaClass(){
		return false;
	}

	public Class getJavaClass() throws Exception{
		throw new IllegalArgumentException(
				"UnresolvedVarExpr has no Java class");
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
	}

	public Object eval() throws Exception{
		throw new IllegalArgumentException(
				"UnresolvedVarExpr cannot be evalled");
	}
}

static class ConstantExpr extends LiteralExpr{
	//stuff quoted vals in classloader at compile time, pull out at runtime
	//this won't work for static compilation...
	public final Object v;
	public final int id;

	public ConstantExpr(Object v){
		this.v = v;
		this.id = registerConstant(v);
//		this.id = RT.nextID();
//		DynamicClassLoader loader = (DynamicClassLoader) LOADER.get();
//		loader.registerQuotedVal(id, v);
	}

	Object val(){
		return v;
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		fn.emitConstant(gen, id);
		if(context == C.STATEMENT)
			{
			gen.pop();
//			gen.loadThis();
//			gen.invokeVirtual(OBJECT_TYPE, getClassMethod);
//			gen.invokeVirtual(CLASS_TYPE, getClassLoaderMethod);
//			gen.checkCast(DYNAMIC_CLASSLOADER_TYPE);
//			gen.push(id);
//			gen.invokeVirtual(DYNAMIC_CLASSLOADER_TYPE, getQuotedValMethod);
			}
	}

	public boolean hasJavaClass(){
		return Modifier.isPublic(v.getClass().getModifiers());
		//return false;
	}

	public Class getJavaClass() throws Exception{
		return v.getClass();
		//throw new IllegalArgumentException("Has no Java class");
	}

	static class Parser implements IParser{
		public Expr parse(C context, Object form){
			Object v = RT.second(form);

			if(v == null)
				return NIL_EXPR;
//			Class fclass = v.getClass();
//			if(fclass == Keyword.class)
//				return registerKeyword((Keyword) v);
//			else if(v instanceof Num)
//				return new NumExpr((Num) v);
//			else if(fclass == String.class)
//				return new StringExpr((String) v);
//			else if(fclass == Character.class)
//				return new CharExpr((Character) v);
//			else if(v instanceof IPersistentCollection && ((IPersistentCollection) v).count() == 0)
//				return new EmptyExpr(v);
			else
				return new ConstantExpr(v);
		}
	}
}

static class NilExpr extends LiteralExpr{
	Object val(){
		return null;
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		gen.visitInsn(Opcodes.ACONST_NULL);
		if(context == C.STATEMENT)
			gen.pop();
	}

	public boolean hasJavaClass(){
		return true;
	}

	public Class getJavaClass() throws Exception{
		return null;
	}
}

final static NilExpr NIL_EXPR = new NilExpr();

static class BooleanExpr extends LiteralExpr{
	public final boolean val;


	public BooleanExpr(boolean val){
		this.val = val;
	}

	Object val(){
		return val ? RT.T : RT.F;
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		if(val)
			gen.getStatic(BOOLEAN_OBJECT_TYPE, "TRUE", BOOLEAN_OBJECT_TYPE);
		else
			gen.getStatic(BOOLEAN_OBJECT_TYPE, "FALSE", BOOLEAN_OBJECT_TYPE);
		if(context == C.STATEMENT)
			{
			gen.pop();
			}
	}

	public boolean hasJavaClass(){
		return true;
	}

	public Class getJavaClass() throws Exception{
		return Boolean.class;
	}
}

final static BooleanExpr TRUE_EXPR = new BooleanExpr(true);
final static BooleanExpr FALSE_EXPR = new BooleanExpr(false);

static class StringExpr extends LiteralExpr{
	public final String str;

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

/*
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
 */
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
		NIL_EXPR.emit(context, fn, gen);
	}

	static class Parser implements IParser{
		public Expr parse(C context, Object form) throws Exception{
			return new MonitorEnterExpr(analyze(C.EXPRESSION, RT.second(form)));
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
		NIL_EXPR.emit(context, fn, gen);
	}

	static class Parser implements IParser{
		public Expr parse(C context, Object form) throws Exception{
			return new MonitorExitExpr(analyze(C.EXPRESSION, RT.second(form)));
		}
	}

}

public static class TryExpr implements Expr{
	public final Expr tryExpr;
	public final Expr finallyExpr;
	public final PersistentVector catchExprs;
	public final int retLocal;
	public final int finallyLocal;

	public static class CatchClause{
		//final String className;
		public final Class c;
		public final LocalBinding lb;
		public final Expr handler;
		Label label;
		Label endLabel;


		public CatchClause(Class c, LocalBinding lb, Expr handler){
			this.c = c;
			this.lb = lb;
			this.handler = handler;
		}
	}

	public TryExpr(Expr tryExpr, PersistentVector catchExprs, Expr finallyExpr, int retLocal, int finallyLocal){
		this.tryExpr = tryExpr;
		this.catchExprs = catchExprs;
		this.finallyExpr = finallyExpr;
		this.retLocal = retLocal;
		this.finallyLocal = finallyLocal;
	}

	public Object eval() throws Exception{
		throw new UnsupportedOperationException("Can't eval try");
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		Label startTry = gen.newLabel();
		Label endTry = gen.newLabel();
		Label endTryCatch = gen.newLabel();
		Label end = gen.newLabel();
		Label ret = gen.newLabel();
		Label finallyLabel = gen.newLabel();
		for(int i = 0; i < catchExprs.count(); i++)
			{
			CatchClause clause = (CatchClause) catchExprs.nth(i);
			clause.label = gen.newLabel();
			clause.endLabel = gen.newLabel();
			}

		gen.mark(startTry);
		tryExpr.emit(context, fn, gen);
		if(context != C.STATEMENT)
			gen.visitVarInsn(OBJECT_TYPE.getOpcode(Opcodes.ISTORE), retLocal);
		gen.mark(endTry);
		if(finallyExpr != null)
			finallyExpr.emit(C.STATEMENT, fn, gen);
		gen.goTo(ret);

		for(int i = 0; i < catchExprs.count(); i++)
			{
			CatchClause clause = (CatchClause) catchExprs.nth(i);
			gen.mark(clause.label);
			//exception should be on stack
			//put in clause local
			gen.visitVarInsn(OBJECT_TYPE.getOpcode(Opcodes.ISTORE), clause.lb.idx);
			clause.handler.emit(context, fn, gen);
			if(context != C.STATEMENT)
				gen.visitVarInsn(OBJECT_TYPE.getOpcode(Opcodes.ISTORE), retLocal);
			gen.mark(clause.endLabel);

			if(finallyExpr != null)
				finallyExpr.emit(C.STATEMENT, fn, gen);
			gen.goTo(ret);
			}
		gen.mark(endTryCatch);
		if(finallyExpr != null)
			{
			gen.mark(finallyLabel);
			//exception should be on stack
			gen.visitVarInsn(OBJECT_TYPE.getOpcode(Opcodes.ISTORE), finallyLocal);
			finallyExpr.emit(C.STATEMENT, fn, gen);
			gen.visitVarInsn(OBJECT_TYPE.getOpcode(Opcodes.ILOAD), finallyLocal);
			gen.throwException();
			}
		gen.mark(ret);
		if(context != C.STATEMENT)
			gen.visitVarInsn(OBJECT_TYPE.getOpcode(Opcodes.ILOAD), retLocal);
		gen.mark(end);
		for(int i = 0; i < catchExprs.count(); i++)
			{
			CatchClause clause = (CatchClause) catchExprs.nth(i);
			gen.visitTryCatchBlock(startTry, endTry, clause.label, clause.c.getName().replace('.', '/'));
			}
		if(finallyExpr != null)
			gen.visitTryCatchBlock(startTry, endTryCatch, finallyLabel, null);
		for(int i = 0; i < catchExprs.count(); i++)
			{
			CatchClause clause = (CatchClause) catchExprs.nth(i);
			gen.visitLocalVariable(clause.lb.name, "Ljava/lang/Object;", null, clause.label, clause.endLabel,
			                       clause.lb.idx);
			}
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
//			if(context == C.EVAL || context == C.EXPRESSION)
			if(context != C.RETURN)
				return analyze(context, RT.list(RT.list(FN, PersistentVector.EMPTY, form)));

			//(try try-expr* catch-expr* finally-expr?)
			//catch-expr: (catch class sym expr*)
			//finally-expr: (finally expr*)

			PersistentVector body = PersistentVector.EMPTY;
			PersistentVector catches = PersistentVector.EMPTY;
			Expr finallyExpr = null;
			boolean caught = false;

			int retLocal = getAndIncLocalNum();
			int finallyLocal = getAndIncLocalNum();
			for(ISeq fs = form.next(); fs != null; fs = fs.next())
				{
				Object f = fs.first();
				Object op = (f instanceof ISeq) ? ((ISeq) f).first() : null;
				if(!Util.equals(op, CATCH) && !Util.equals(op, FINALLY))
					{
					if(caught)
						throw new Exception("Only catch or finally clause can follow catch in try expression");
					body = body.cons(f);
					}
				else
					{
					if(Util.equals(op, CATCH))
						{
						Class c = HostExpr.maybeClass(RT.second(f), false);
						if(c == null)
							throw new IllegalArgumentException("Unable to resolve classname: " + RT.second(f));
						if(!(RT.third(f) instanceof Symbol))
							throw new IllegalArgumentException(
									"Bad binding form, expected symbol, got: " + RT.third(f));
						Symbol sym = (Symbol) RT.third(f);
						if(sym.getNamespace() != null)
							throw new Exception("Can't bind qualified name:" + sym);

						IPersistentMap dynamicBindings = RT.map(LOCAL_ENV, LOCAL_ENV.deref(),
						                                        NEXT_LOCAL_NUM, NEXT_LOCAL_NUM.deref(),
						                                        IN_CATCH_FINALLY, RT.T);
						try
							{
							Var.pushThreadBindings(dynamicBindings);
							LocalBinding lb = registerLocal(sym,
							                                (Symbol) (RT.second(f) instanceof Symbol ? RT.second(f)
							                                                                         : null), null);
							Expr handler = (new BodyExpr.Parser()).parse(context, RT.next(RT.next(RT.next(f))));
							catches = catches.cons(new CatchClause(c, lb, handler));
							}
						finally
							{
							Var.popThreadBindings();
							}
						caught = true;
						}
					else //finally
						{
						if(fs.next() != null)
							throw new Exception("finally clause must be last in try expression");
						try
							{
							Var.pushThreadBindings(RT.map(IN_CATCH_FINALLY, RT.T));
							finallyExpr = (new BodyExpr.Parser()).parse(C.STATEMENT, RT.next(f));
							}
						finally
							{
							Var.popThreadBindings();
							}
						}
					}
				}

			return new TryExpr((new BodyExpr.Parser()).parse(context, RT.seq(body)), catches, finallyExpr, retLocal,
			                   finallyLocal);
		}
	}
}

//static class TryFinallyExpr implements Expr{
//	final Expr tryExpr;
//	final Expr finallyExpr;
//
//
//	public TryFinallyExpr(Expr tryExpr, Expr finallyExpr){
//		this.tryExpr = tryExpr;
//		this.finallyExpr = finallyExpr;
//	}
//
//	public Object eval() throws Exception{
//		throw new UnsupportedOperationException("Can't eval try");
//	}
//
//	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
//		Label startTry = gen.newLabel();
//		Label endTry = gen.newLabel();
//		Label end = gen.newLabel();
//		Label finallyLabel = gen.newLabel();
//		gen.visitTryCatchBlock(startTry, endTry, finallyLabel, null);
//		gen.mark(startTry);
//		tryExpr.emit(context, fn, gen);
//		gen.mark(endTry);
//		finallyExpr.emit(C.STATEMENT, fn, gen);
//		gen.goTo(end);
//		gen.mark(finallyLabel);
//		//exception should be on stack
//		finallyExpr.emit(C.STATEMENT, fn, gen);
//		gen.throwException();
//		gen.mark(end);
//	}
//
//	public boolean hasJavaClass() throws Exception{
//		return tryExpr.hasJavaClass();
//	}
//
//	public Class getJavaClass() throws Exception{
//		return tryExpr.getJavaClass();
//	}
//
//	static class Parser implements IParser{
//		public Expr parse(C context, Object frm) throws Exception{
//			ISeq form = (ISeq) frm;
//			//(try-finally try-expr finally-expr)
//			if(form.count() != 3)
//				throw new IllegalArgumentException(
//						"Wrong number of arguments, expecting: (try-finally try-expr finally-expr) ");
//
//			if(context == C.EVAL || context == C.EXPRESSION)
//				return analyze(context, RT.list(RT.list(FN, PersistentVector.EMPTY, form)));
//
//			return new TryFinallyExpr(analyze(context, RT.second(form)),
//			                          analyze(C.STATEMENT, RT.third(form)));
//		}
//	}
//}

static class ThrowExpr extends UntypedExpr{
	public final Expr excExpr;

	public ThrowExpr(Expr excExpr){
		this.excExpr = excExpr;
	}


	public Object eval() throws Exception{
		throw new Exception("Can't eval throw");
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		excExpr.emit(C.EXPRESSION, fn, gen);
		gen.checkCast(THROWABLE_TYPE);
		gen.throwException();
	}

	static class Parser implements IParser{
		public Expr parse(C context, Object form) throws Exception{
			if(context == C.EVAL)
				return analyze(context, RT.list(RT.list(FN, PersistentVector.EMPTY, form)));
			return new ThrowExpr(analyze(C.EXPRESSION, RT.second(form)));
		}
	}
}

/*
static class ClassExpr implements Expr{
	final Class c;
	final static Method forNameMethod = Method.getMethod("Class forName(String)");


	public ClassExpr(Class c){
		this.c = c;
	}

	public Object eval() throws Exception{
		return c;
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		if(context != C.STATEMENT)
			{
			gen.push(c.getName());
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
			Class c = HostExpr.maybeClass(RT.second(form), true);
			if(c == null)
				throw new IllegalArgumentException("Unable to resolve classname: " + RT.second(form));
			return new ClassExpr(c);
		}
	}
}
*/

static public boolean subsumes(Class[] c1, Class[] c2){
	//presumes matching lengths
	Boolean better = false;
	for(int i = 0; i < c1.length; i++)
		{
		if(c1[i] != c2[i])// || c2[i].isPrimitive() && c1[i] == Object.class))
			{
			if(!c1[i].isPrimitive() && c2[i].isPrimitive()
			   //|| Number.class.isAssignableFrom(c1[i]) && c2[i].isPrimitive()
			   ||
			   c2[i].isAssignableFrom(c1[i]))
				better = true;
			else
				return false;
			}
		}
	return better;
}

static int getMatchingParams(String methodName, ArrayList<Class[]> paramlists, IPersistentVector argexprs,
                             List<Class> rets)
		throws Exception{
	//presumes matching lengths
	int matchIdx = -1;
	boolean tied = false;
    boolean foundExact = false;
	for(int i = 0; i < paramlists.size(); i++)
		{
		boolean match = true;
		ISeq aseq = argexprs.seq();
		int exact = 0;
		for(int p = 0; match && p < argexprs.count() && aseq != null; ++p, aseq = aseq.next())
			{
			Expr arg = (Expr) aseq.first();
			Class aclass = arg.hasJavaClass() ? arg.getJavaClass() : Object.class;
			Class pclass = paramlists.get(i)[p];
			if(arg.hasJavaClass() && aclass == pclass)
				exact++;
			else
				match = Reflector.paramArgTypeMatch(pclass, aclass);
			}
		if(exact == argexprs.count())
            {
            if(!foundExact || matchIdx == -1 || rets.get(matchIdx).isAssignableFrom(rets.get(i)))
                matchIdx = i;
            foundExact = true;
            }
		else if(match && !foundExact)
			{
			if(matchIdx == -1)
				matchIdx = i;
			else
				{
				if(subsumes(paramlists.get(i), paramlists.get(matchIdx)))
					{
					matchIdx = i;
					tied = false;
					}
				else if(Arrays.equals(paramlists.get(matchIdx), paramlists.get(i)))
					{
					if(rets.get(matchIdx).isAssignableFrom(rets.get(i)))
						matchIdx = i;
					}
				else if(!(subsumes(paramlists.get(matchIdx), paramlists.get(i))))
						tied = true;
				}
			}
		}
	if(tied)
		throw new IllegalArgumentException("More than one matching method found: " + methodName);

	return matchIdx;
}

public static class NewExpr implements Expr{
	public final IPersistentVector args;
	public final Constructor ctor;
	public final Class c;
	final static Method invokeConstructorMethod =
			Method.getMethod("Object invokeConstructor(Class,Object[])");
//	final static Method forNameMethod = Method.getMethod("Class classForName(String)");
	final static Method forNameMethod = Method.getMethod("Class forName(String)");


	public NewExpr(Class c, IPersistentVector args, int line) throws Exception{
		this.args = args;
		this.c = c;
		Constructor[] allctors = c.getConstructors();
		ArrayList ctors = new ArrayList();
		ArrayList<Class[]> params = new ArrayList();
		ArrayList<Class> rets = new ArrayList();
		for(int i = 0; i < allctors.length; i++)
			{
			Constructor ctor = allctors[i];
			if(ctor.getParameterTypes().length == args.count())
				{
				ctors.add(ctor);
				params.add(ctor.getParameterTypes());
				rets.add(c);
				}
			}
		if(ctors.isEmpty())
			throw new IllegalArgumentException("No matching ctor found for " + c);

		int ctoridx = 0;
		if(ctors.size() > 1)
			{
			ctoridx = getMatchingParams(c.getName(), params, args, rets);
			}

		this.ctor = ctoridx >= 0 ? (Constructor) ctors.get(ctoridx) : null;
		if(ctor == null && RT.booleanCast(RT.WARN_ON_REFLECTION.deref()))
			{
			((PrintWriter) RT.ERR.deref())
					.format("Reflection warning, %s:%d - call to %s ctor can't be resolved.\n",
							SOURCE_PATH.deref(), line, c.getName());
			}
	}

	public Object eval() throws Exception{
		Object[] argvals = new Object[args.count()];
		for(int i = 0; i < args.count(); i++)
			argvals[i] = ((Expr) args.nth(i)).eval();
		if(this.ctor != null)
			{
			return ctor.newInstance(Reflector.boxArgs(ctor.getParameterTypes(), argvals));
			}
		return Reflector.invokeConstructor(c, argvals);
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		if(this.ctor != null)
			{
			Type type = Type.getType(c);
			gen.newInstance(type);
			gen.dup();
			MethodExpr.emitTypedArgs(fn, gen, ctor.getParameterTypes(), args);
			if(context == C.RETURN)
				{
				FnMethod method = (FnMethod) METHOD.deref();
				method.emitClearLocals(gen);
				}
			gen.invokeConstructor(type, new Method("<init>", Type.getConstructorDescriptor(ctor)));
			}
		else
			{
			gen.push(c.getName());
			gen.invokeStatic(CLASS_TYPE, forNameMethod);
			MethodExpr.emitArgsAsArray(args, fn, gen);
			if(context == C.RETURN)
				{
				FnMethod method = (FnMethod) METHOD.deref();
				method.emitClearLocals(gen);
				}
			gen.invokeStatic(REFLECTOR_TYPE, invokeConstructorMethod);
			}
		if(context == C.STATEMENT)
			gen.pop();
	}

	public boolean hasJavaClass(){
		return true;
	}

	public Class getJavaClass() throws Exception{
		return c;
	}

	static class Parser implements IParser{
		public Expr parse(C context, Object frm) throws Exception{
			int line = (Integer) LINE.deref();
			ISeq form = (ISeq) frm;
			//(new Classname args...)
			if(form.count() < 2)
				throw new Exception("wrong number of arguments, expecting: (new Classname args...)");
			Class c = HostExpr.maybeClass(RT.second(form), false);
			if(c == null)
				throw new IllegalArgumentException("Unable to resolve classname: " + RT.second(form));
			PersistentVector args = PersistentVector.EMPTY;
			for(ISeq s = RT.next(RT.next(form)); s != null; s = s.next())
				args = args.cons(analyze(context == C.EVAL ? context : C.EXPRESSION, s.first()));
			return new NewExpr(c, args, line);
		}
	}

}

//static class IdenticalExpr implements Expr{
//	final Expr expr1;
//	final Expr expr2;
//
//
//	public IdenticalExpr(Expr expr1, Expr expr2){
//		this.expr1 = expr1;
//		this.expr2 = expr2;
//	}
//
//	public boolean hasJavaClass(){
//		return true;
//	}
//
//	public Class getJavaClass(){
//		return Boolean.class;
//	}
//
//	public Object eval() throws Exception{
//		return expr1.eval() == expr2.eval() ?
//		       RT.T : RT.F;
//	}
//
//	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
//		if(context != C.STATEMENT)
//			{
//			Label not = gen.newLabel();
//			Label end = gen.newLabel();
//			expr1.emit(C.EXPRESSION, fn, gen);
//			expr2.emit(C.EXPRESSION, fn, gen);
//			gen.visitJumpInsn(IF_ACMPNE, not);
//			gen.getStatic(BOOLEAN_OBJECT_TYPE, "TRUE", BOOLEAN_OBJECT_TYPE);
////			gen.getStatic(RT_TYPE, "T", KEYWORD_TYPE);
//			gen.goTo(end);
//			gen.mark(not);
//			gen.getStatic(BOOLEAN_OBJECT_TYPE, "FALSE", BOOLEAN_OBJECT_TYPE);
////			NIL_EXPR.emit(C.EXPRESSION, fn, gen);
//			gen.mark(end);
//			}
//	}
//
//	static class Parser implements IParser{
//		public Expr parse(C context, Object frm) throws Exception{
//			ISeq form = (ISeq) frm;
//			if(form.count() != 3)
//				throw new Exception("wrong number of arguments, expecting: (identical? x y)");
//
//			return new IdenticalExpr(analyze(C.EXPRESSION, RT.second(form)), analyze(C.EXPRESSION, RT.third(form)));
//		}
//	}
//}

//static class InstanceExpr implements Expr{
//	final Expr expr;
//	final Class c;
//
//
//	public InstanceExpr(Expr expr, Class c){
//		this.expr = expr;
//		this.c = c;
//	}
//
//	public Object eval() throws Exception{
//		return c.isInstance(expr.eval()) ?
//		       RT.T : RT.F;
//	}
//
//	public boolean hasJavaClass(){
//		return true;
//	}
//
//	public Class getJavaClass(){
//		return Boolean.class;
//	}
//
//	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
//		if(context != C.STATEMENT)
//			{
//			Label not = gen.newLabel();
//			Label end = gen.newLabel();
//			expr.emit(C.EXPRESSION, fn, gen);
//			gen.instanceOf(Type.getType(c));
//			gen.ifZCmp(GeneratorAdapter.EQ, not);
//			gen.getStatic(BOOLEAN_OBJECT_TYPE, "TRUE", BOOLEAN_OBJECT_TYPE);
////			gen.getStatic(RT_TYPE, "T", KEYWORD_TYPE);
//			gen.goTo(end);
//			gen.mark(not);
//			gen.getStatic(BOOLEAN_OBJECT_TYPE, "FALSE", BOOLEAN_OBJECT_TYPE);
////			NIL_EXPR.emit(C.EXPRESSION, fn, gen);
//			gen.mark(end);
//			}
//	}
//
//	static class Parser implements IParser{
//		public Expr parse(C context, Object frm) throws Exception{
//			ISeq form = (ISeq) frm;
//			//(instance? x Classname)
//			if(form.count() != 3)
//				throw new Exception("wrong number of arguments, expecting: (instance? x Classname)");
//			Class c = HostExpr.maybeClass(RT.third(form), true);
//			if(c == null)
//				throw new IllegalArgumentException("Unable to resolve classname: " + RT.third(form));
//			return new InstanceExpr(analyze(C.EXPRESSION, RT.second(form)), c);
//		}
//	}
//}

public static class MetaExpr implements Expr{
	public final Expr expr;
	public final MapExpr meta;
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
		expr.emit(C.EXPRESSION, fn, gen);
		gen.checkCast(IOBJ_TYPE);
		meta.emit(C.EXPRESSION, fn, gen);
		gen.checkCast(IPERSISTENTMAP_TYPE);
		gen.invokeInterface(IOBJ_TYPE, withMetaMethod);
		if(context == C.STATEMENT)
			{
			gen.pop();
			}
	}

	public boolean hasJavaClass() throws Exception{
		return expr.hasJavaClass();
	}

	public Class getJavaClass() throws Exception{
		return expr.getJavaClass();
	}
}

public static class IfExpr implements Expr{
	public final Expr testExpr;
	public final Expr thenExpr;
	public final Expr elseExpr;
	public final int line;


	public IfExpr(int line, Expr testExpr, Expr thenExpr, Expr elseExpr){
		this.testExpr = testExpr;
		this.thenExpr = thenExpr;
		this.elseExpr = elseExpr;
		this.line = line;
	}

	public Object eval() throws Exception{
		Object t = testExpr.eval();
		if(t != null && t != Boolean.FALSE)
			return thenExpr.eval();
		return elseExpr.eval();
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		Label nullLabel = gen.newLabel();
		Label falseLabel = gen.newLabel();
		Label endLabel = gen.newLabel();

		gen.visitLineNumber(line, gen.mark());

		try
			{
			if(testExpr instanceof MaybePrimitiveExpr && testExpr.hasJavaClass() &&
			   testExpr.getJavaClass() == boolean.class)
				{
				((MaybePrimitiveExpr) testExpr).emitUnboxed(C.EXPRESSION, fn, gen);
				gen.ifZCmp(gen.EQ, falseLabel);
				}
			else
				{
				testExpr.emit(C.EXPRESSION, fn, gen);
				gen.dup();
				gen.ifNull(nullLabel);
				gen.getStatic(BOOLEAN_OBJECT_TYPE, "FALSE", BOOLEAN_OBJECT_TYPE);
				gen.visitJumpInsn(IF_ACMPEQ, falseLabel);
				}
			}
		catch(Exception e)
			{
			throw new RuntimeException(e);
			}
		thenExpr.emit(context, fn, gen);
		gen.goTo(endLabel);
		gen.mark(nullLabel);
		gen.pop();
		gen.mark(falseLabel);
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
			return new IfExpr((Integer) LINE.deref(),
			                  analyze(context == C.EVAL ? context : C.EXPRESSION, RT.second(form)),
			                  analyze(context, RT.third(form)),
			                  analyze(context, RT.fourth(form)));
		}
	}
}

static final public IPersistentMap CHAR_MAP =
		PersistentHashMap.create('-', "_",
//		                         '.', "_DOT_",
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
'|', "_BAR_",
'{', "_LBRACE_",
'}', "_RBRACE_",
'[', "_LBRACK_",
']', "_RBRACK_",
'/', "_SLASH_",
'\\', "_BSLASH_",
'?', "_QMARK_");

static public String munge(String name){
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

public static class EmptyExpr implements Expr{
	public final Object coll;
	final static Type HASHMAP_TYPE = Type.getType(PersistentArrayMap.class);
	final static Type HASHSET_TYPE = Type.getType(PersistentHashSet.class);
	final static Type VECTOR_TYPE = Type.getType(PersistentVector.class);
	final static Type LIST_TYPE = Type.getType(PersistentList.class);
	final static Type EMPTY_LIST_TYPE = Type.getType(PersistentList.EmptyList.class);


	public EmptyExpr(Object coll){
		this.coll = coll;
	}

	public Object eval() throws Exception{
		return coll;
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		if(coll instanceof IPersistentList)
			gen.getStatic(LIST_TYPE, "EMPTY", EMPTY_LIST_TYPE);
		else if(coll instanceof IPersistentVector)
			gen.getStatic(VECTOR_TYPE, "EMPTY", VECTOR_TYPE);
		else if(coll instanceof IPersistentMap)
				gen.getStatic(HASHMAP_TYPE, "EMPTY", HASHMAP_TYPE);
			else if(coll instanceof IPersistentSet)
					gen.getStatic(HASHSET_TYPE, "EMPTY", HASHSET_TYPE);
				else
					throw new UnsupportedOperationException("Unknown Collection type");
		if(context == C.STATEMENT)
			{
			gen.pop();
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
			else if(coll instanceof IPersistentSet)
					return IPersistentSet.class;
				else
					throw new UnsupportedOperationException("Unknown Collection type");
	}
}

public static class ListExpr implements Expr{
	public final IPersistentVector args;
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

public static class MapExpr implements Expr{
	public final IPersistentVector keyvals;
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
		for(ISeq s = RT.seq(form); s != null; s = s.next())
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

public static class SetExpr implements Expr{
	public final IPersistentVector keys;
	final static Method setMethod = Method.getMethod("clojure.lang.IPersistentSet set(Object[])");


	public SetExpr(IPersistentVector keys){
		this.keys = keys;
	}

	public Object eval() throws Exception{
		Object[] ret = new Object[keys.count()];
		for(int i = 0; i < keys.count(); i++)
			ret[i] = ((Expr) keys.nth(i)).eval();
		return RT.set(ret);
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		MethodExpr.emitArgsAsArray(keys, fn, gen);
		gen.invokeStatic(RT_TYPE, setMethod);
		if(context == C.STATEMENT)
			gen.pop();
	}

	public boolean hasJavaClass() throws Exception{
		return true;
	}

	public Class getJavaClass() throws Exception{
		return IPersistentSet.class;
	}


	static public Expr parse(C context, IPersistentSet form) throws Exception{
		IPersistentVector keys = PersistentVector.EMPTY;
		for(ISeq s = RT.seq(form); s != null; s = s.next())
			{
			Object e = s.first();
			keys = (IPersistentVector) keys.cons(analyze(context == C.EVAL ? context : C.EXPRESSION, e));
			}
		Expr ret = new SetExpr(keys);
		if(form instanceof IObj && ((IObj) form).meta() != null)
			return new MetaExpr(ret, (MapExpr) MapExpr
					.parse(context == C.EVAL ? context : C.EXPRESSION, ((IObj) form).meta()));
		else
			return ret;
	}
}

public static class VectorExpr implements Expr{
	public final IPersistentVector args;
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
	public final Expr fexpr;
	public final Object tag;
	public final IPersistentVector args;
	public final int line;
	public final String source;

	public InvokeExpr(String source, int line, Symbol tag, Expr fexpr, IPersistentVector args){
		this.source = source;
		this.fexpr = fexpr;
		this.args = args;
		this.line = line;
		this.tag = tag != null ? tag : (fexpr instanceof VarExpr ? ((VarExpr) fexpr).tag : null);
	}

	public Object eval() throws Exception{
		try
			{
			IFn fn = (IFn) fexpr.eval();
			PersistentVector argvs = PersistentVector.EMPTY;
			for(int i = 0; i < args.count(); i++)
				argvs = argvs.cons(((Expr) args.nth(i)).eval());
			return fn.applyTo(RT.seq(argvs));
			}
		catch(Throwable e)
			{
			if(!(e instanceof CompilerException))
				throw new CompilerException(source, line, e);
			else
				throw (CompilerException) e;
			}
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

		if(context == C.RETURN)
			{
			FnMethod method = (FnMethod) METHOD.deref();
			method.emitClearLocals(gen);
			}

		gen.invokeInterface(IFN_TYPE, new Method("invoke", OBJECT_TYPE, ARG_TYPES[Math.min(MAX_POSITIONAL_ARITY + 1,
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
		for(ISeq s = RT.seq(form.next()); s != null; s = s.next())
			{
			args = args.cons(analyze(context, s.first()));
			}
//		if(args.count() > MAX_POSITIONAL_ARITY)
//			throw new IllegalArgumentException(
//					String.format("No more than %d args supported", MAX_POSITIONAL_ARITY));

		return new InvokeExpr((String) SOURCE.deref(), (Integer) LINE.deref(), tagOf(form), fexpr, args);
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

static class FnLoaderThunk extends RestFn{
	FnExpr fx;
	Var v;
	IFn f;

	FnLoaderThunk(FnExpr fx, Var v){
		super(0);
		this.fx = fx;
		this.v = v;
	}

	protected Object doInvoke(Object args) throws Exception{
		IFn f = loadFn();
		return f.applyTo((ISeq) args);
	}

	synchronized private IFn loadFn() throws Exception{
		if(f == null)
			{
			Class fc = fx.getCompiledClass();
			f = (IFn) fc.newInstance();
			v.swapRoot(f);
			}
		return f;
	}
}

static public class FnExpr implements Expr{
	static final String CONST_PREFIX = "const__";
	IPersistentCollection methods;
	//if there is a variadic overload (there can only be one) it is stored here
	FnMethod variadicMethod = null;
	String name;
	String simpleName;
	String internalName;
	String thisName;
	Type fntype;
	public final Object tag;
	//localbinding->itself
	IPersistentMap closes = PersistentHashMap.EMPTY;
	//Keyword->KeywordExpr
	IPersistentMap keywords = PersistentHashMap.EMPTY;
	IPersistentMap vars = PersistentHashMap.EMPTY;
	Class compiledClass;
	int line;
	PersistentVector constants;
	int constantsID;
	boolean onceOnly = false;
	String superName = null;

	public final IPersistentCollection methods(){
		return methods;
	}

	public final FnMethod variadicMethod(){
		return variadicMethod;
	}

	public final String name(){
		return name;
	}

	public final String simpleName(){
		return simpleName;
	}

	public final String internalName(){
		return internalName;
	}

	public final String thisName(){
		return thisName;
	}

	public final Type fntype(){
		return fntype;
	}

	public final IPersistentMap closes(){
		return closes;
	}

	public final IPersistentMap keywords(){
		return keywords;
	}

	public final IPersistentMap vars(){
		return vars;
	}

	public final Class compiledClass(){
		return compiledClass;
	}

	public final int line(){
		return line;
	}

	public final PersistentVector constants(){
		return constants;
	}

	public final int constantsID(){
		return constantsID;
	}

	final static Method kwintern = Method.getMethod("clojure.lang.Keyword intern(String, String)");
	final static Method symcreate = Method.getMethod("clojure.lang.Symbol create(String)");
	final static Method varintern =
			Method.getMethod("clojure.lang.Var intern(clojure.lang.Symbol, clojure.lang.Symbol)");
	final static Method afnctor = Method.getMethod("void <init>()");
	final static Method restfnctor = Method.getMethod("void <init>(int)");
	final static Type aFnType = Type.getType(AFunction.class);
	final static Type restFnType = Type.getType(RestFn.class);

	final static Type DYNAMIC_CLASSLOADER_TYPE = Type.getType(DynamicClassLoader.class);
	final static Method getClassMethod = Method.getMethod("Class getClass()");
	final static Method getClassLoaderMethod = Method.getMethod("ClassLoader getClassLoader()");
	final static Method getConstantsMethod = Method.getMethod("Object[] getConstants(int)");
	final static Method readStringMethod = Method.getMethod("Object readString(String)");
	private DynamicClassLoader loader;
	private byte[] bytecode;

	public FnExpr(Object tag){
		this.tag = tag;
	}

	static Expr parse(C context, ISeq form, String name) throws Exception{
		FnExpr fn = new FnExpr(tagOf(form));
		FnMethod enclosingMethod = (FnMethod) METHOD.deref();
		if(((IMeta) form.first()).meta() != null)
			{
			fn.onceOnly = RT.booleanCast(RT.get(RT.meta(form.first()), Keyword.intern(null, "once")));
			fn.superName = (String) RT.get(RT.meta(form.first()), Keyword.intern(null, "super-name"));
			}
		//fn.thisName = name;
		String basename = enclosingMethod != null ?
		                  (enclosingMethod.fn.name + "$")
		                                          : //"clojure.fns." +
		                  (munge(currentNS().name.name) + "$");
		if(RT.second(form) instanceof Symbol)
			name = ((Symbol) RT.second(form)).name;
		fn.simpleName = ((name != null ?
		                  munge(name).replace(".", "_DOT_") : "fn")
		                 + "__" + RT.nextID());
		fn.name = basename + fn.simpleName;
		fn.internalName = fn.name.replace('.', '/');
		fn.fntype = Type.getObjectType(fn.internalName);
		try
			{
			Var.pushThreadBindings(
					RT.map(CONSTANTS, PersistentVector.EMPTY,
					       KEYWORDS, PersistentHashMap.EMPTY,
					       VARS, PersistentHashMap.EMPTY));

			//arglist might be preceded by symbol naming this fn
			if(RT.second(form) instanceof Symbol)
				{
				fn.thisName = ((Symbol) RT.second(form)).name;
				form = RT.cons(FN, RT.next(RT.next(form)));
				}

			//now (fn [args] body...) or (fn ([args] body...) ([args2] body2...) ...)
			//turn former into latter
			if(RT.second(form) instanceof IPersistentVector)
				form = RT.list(FN, RT.next(form));
			fn.line = (Integer) LINE.deref();
			FnMethod[] methodArray = new FnMethod[MAX_POSITIONAL_ARITY + 1];
			FnMethod variadicMethod = null;
			for(ISeq s = RT.next(form); s != null; s = RT.next(s))
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
			fn.keywords = (IPersistentMap) KEYWORDS.deref();
			fn.vars = (IPersistentMap) VARS.deref();
			fn.constants = (PersistentVector) CONSTANTS.deref();
			fn.constantsID = RT.nextID();
//			DynamicClassLoader loader = (DynamicClassLoader) LOADER.get();
//			loader.registerConstants(fn.constantsID, fn.constants.toArray());
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

	Type[] ctorTypes(){
		if(closes.count() == 0)
			return ARG_TYPES[0];
		PersistentVector tv = PersistentVector.EMPTY;
		for(ISeq s = RT.keys(closes); s != null; s = s.next())
			{
			LocalBinding lb = (LocalBinding) s.first();
			if(lb.getPrimitiveType() != null)
				tv = tv.cons(Type.getType(lb.getPrimitiveType()));
			else
				tv = tv.cons(OBJECT_TYPE);
			}
		Type[] ret = new Type[tv.count()];
		for(int i = 0; i < tv.count(); i++)
			ret[i] = (Type) tv.nth(i);
		return ret;
	}

	private void compile() throws Exception{
		//create bytecode for a class
		//with name current_ns.defname[$letname]+
		//anonymous fns get names fn__id
		//derived from AFn/RestFn
		ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_MAXS);
//		ClassWriter cw = new ClassWriter(0);
		ClassVisitor cv = cw;
//		ClassVisitor cv = new TraceClassVisitor(new CheckClassAdapter(cw), new PrintWriter(System.out));
		//ClassVisitor cv = new TraceClassVisitor(cw, new PrintWriter(System.out));
		cv.visit(V1_5, ACC_PUBLIC + ACC_SUPER, internalName, null,
		         superName != null ? superName :
		         (isVariadic() ? "clojure/lang/RestFn" : "clojure/lang/AFunction"), null);
		String source = (String) SOURCE.deref();
		int lineBefore = (Integer) LINE_BEFORE.deref();
		int lineAfter = (Integer) LINE_AFTER.deref() + 1;

		if(source != null && SOURCE_PATH.deref() != null)
			{
			//cv.visitSource(source, null);
			String smap = "SMAP\n" +
			              ((source.lastIndexOf('.') > 0) ?
			               source.substring(0, source.lastIndexOf('.'))
			                                             : simpleName)
			              + ".java\n" +
			              "Clojure\n" +
			              "*S Clojure\n" +
			              "*F\n" +
			              "+ 1 " + source + "\n" +
			              (String) SOURCE_PATH.deref() + "\n" +
			              "*L\n" +
			              String.format("%d#1,%d:%d\n", lineBefore, lineAfter - lineBefore, lineBefore) +
			              "*E";
			cv.visitSource(source, smap);
			}

		//static fields for constants
		for(int i = 0; i < constants.count(); i++)
			{
			cv.visitField(ACC_PUBLIC + ACC_FINAL
			              + ACC_STATIC, constantName(i), constantType(i).getDescriptor(),
			              null, null);
			}

/*
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
			*/
		//static init for constants, keywords and vars
		GeneratorAdapter clinitgen = new GeneratorAdapter(ACC_PUBLIC + ACC_STATIC,
		                                                  Method.getMethod("void <clinit> ()"),
		                                                  null,
		                                                  null,
		                                                  cv);
		clinitgen.visitCode();
		clinitgen.visitLineNumber(line, clinitgen.mark());
//		Label begin = clinitgen.newLabel();
//		Label end = clinitgen.newLabel();

		if(constants.count() > 0)
			{
			emitConstants(clinitgen);
			}

		clinitgen.returnValue();

		clinitgen.endMethod();
//		clinitgen.visitMaxs(1, 1);
		//instance fields for closed-overs
		for(ISeq s = RT.keys(closes); s != null; s = s.next())
			{
			LocalBinding lb = (LocalBinding) s.first();
			if(lb.getPrimitiveType() != null)
				cv.visitField(ACC_PUBLIC //+ ACC_FINAL
						, lb.name, Type.getType(lb.getPrimitiveType()).getDescriptor(),
						      null, null);
			else
				cv.visitField(ACC_PUBLIC //+ (onceOnly ? 0 : ACC_FINAL)
						, lb.name, OBJECT_TYPE.getDescriptor(), null, null);
			}
		//ctor that takes closed-overs and inits base + fields
//		Method m = new Method("<init>", Type.VOID_TYPE, ARG_TYPES[closes.count()]);
		Method m = new Method("<init>", Type.VOID_TYPE, ctorTypes());
		GeneratorAdapter ctorgen = new GeneratorAdapter(ACC_PUBLIC,
		                                                m,
		                                                null,
		                                                null,
		                                                cv);
		Label start = ctorgen.newLabel();
		Label end = ctorgen.newLabel();
		ctorgen.visitCode();
		ctorgen.visitLineNumber(line, ctorgen.mark());
		ctorgen.visitLabel(start);
		ctorgen.loadThis();
		if(superName != null)
			ctorgen.invokeConstructor(Type.getObjectType(superName), afnctor);
		else if(isVariadic()) //RestFn ctor takes reqArity arg
			{
			ctorgen.push(variadicMethod.reqParms.count());
			ctorgen.invokeConstructor(restFnType, restfnctor);
			}
		else
			ctorgen.invokeConstructor(aFnType, afnctor);
		int a = 1;
		for(ISeq s = RT.keys(closes); s != null; s = s.next(), ++a)
			{
			LocalBinding lb = (LocalBinding) s.first();
			ctorgen.loadThis();
			Class primc = lb.getPrimitiveType();
			if(primc != null)
				{
				ctorgen.visitVarInsn(Type.getType(primc).getOpcode(Opcodes.ILOAD), a);
				ctorgen.putField(fntype, lb.name, Type.getType(primc));
				if(primc == Long.TYPE || primc == Double.TYPE)
					++a;
				}
			else
				{
				ctorgen.visitVarInsn(OBJECT_TYPE.getOpcode(Opcodes.ILOAD), a);
				ctorgen.putField(fntype, lb.name, OBJECT_TYPE);
				}
			}
		ctorgen.visitLabel(end);
//		a = 1;
//		for(ISeq s = RT.keys(closes); s != null; s = s.rest(), ++a)
//			{
//			LocalBinding lb = (LocalBinding) s.first();
//			Class primc = lb.getPrimitiveType();
//			if(primc != null)
//				{
//				ctorgen.visitLocalVariable(lb.name, Type.getDescriptor(primc), null, start, end, a);
//				if(primc == Long.TYPE || primc == Double.TYPE)
//					++a;
//				}
//			else
//				{
//				ctorgen.visitLocalVariable(lb.name, "Ljava/lang/Object;", null, start, end, a);
//				}
//			}
		ctorgen.returnValue();
		//	ctorgen.visitMaxs(1, 1);
		ctorgen.endMethod();

		//override of invoke/doInvoke for each method
		for(ISeq s = RT.seq(methods); s != null; s = s.next())
			{
			FnMethod method = (FnMethod) s.first();
			method.emit(this, cv);
			}
		//end of class
		cv.visitEnd();

		bytecode = cw.toByteArray();
		if(RT.booleanCast(COMPILE_FILES.deref()))
			writeClassFile(internalName, bytecode);
//		else
//			getCompiledClass();
	}

	void emitListAsObjectArray(Object value, GeneratorAdapter gen){
		gen.push(((List) value).size());
		gen.newArray(OBJECT_TYPE);
		int i = 0;
		for(Iterator it = ((List) value).iterator(); it.hasNext(); i++)
			{
			gen.dup();
			gen.push(i);
			emitValue(it.next(), gen);
			gen.arrayStore(OBJECT_TYPE);
			}
	}

	void emitValue(Object value, GeneratorAdapter gen){
		boolean partial = true;
		//System.out.println(value.getClass().toString());

		if(value instanceof String)
			{
			gen.push((String) value);
			}
		else if(value instanceof Integer)
			{
			gen.push(((Integer) value).intValue());
			gen.invokeStatic(Type.getType(Integer.class), Method.getMethod("Integer valueOf(int)"));
			}
		else if(value instanceof Double)
				{
				gen.push(((Double) value).doubleValue());
				gen.invokeStatic(Type.getType(Double.class), Method.getMethod("Double valueOf(double)"));
				}
			else if(value instanceof Character)
					{
					gen.push(((Character) value).charValue());
					gen.invokeStatic(Type.getType(Character.class), Method.getMethod("Character valueOf(char)"));
					}
				else if(value instanceof Class)
						{
                                                Class cc = (Class)value;
                                                if(cc.isPrimitive())
                                                        {
                                                        Type bt;
                                                        if ( cc == boolean.class ) bt = Type.getType(Boolean.class);
                                                        else if ( cc == byte.class ) bt = Type.getType(Byte.class);
                                                        else if ( cc == char.class ) bt = Type.getType(Character.class);
                                                        else if ( cc == double.class ) bt = Type.getType(Double.class);
                                                        else if ( cc == float.class ) bt = Type.getType(Float.class);
                                                        else if ( cc == int.class ) bt = Type.getType(Integer.class);
                                                        else if ( cc == long.class ) bt = Type.getType(Long.class);
                                                        else if ( cc == short.class ) bt = Type.getType(Short.class);
                                                        else throw new RuntimeException(
                                                                "Can't embed unknown primitive in code: " + value);
                                                        gen.getStatic( bt, "TYPE", Type.getType(Class.class) );
                                                        }
                                                else
                                                        {
                                                        gen.push(cc.getName());
                                                        gen.invokeStatic(Type.getType(Class.class), Method.getMethod("Class forName(String)"));
                                                        }
						}
					else if(value instanceof Symbol)
							{
							gen.push(((Symbol) value).ns);
							gen.push(((Symbol) value).name);
							gen.invokeStatic(Type.getType(Symbol.class),
							                 Method.getMethod("clojure.lang.Symbol create(String,String)"));
							}
						else if(value instanceof Keyword)
								{
								emitValue(((Keyword) value).sym, gen);
								gen.invokeStatic(Type.getType(Keyword.class),
								                 Method.getMethod("clojure.lang.Keyword intern(clojure.lang.Symbol)"));
								}
							else if(value instanceof Var)
									{
									Var var = (Var) value;
									gen.push(var.ns.name.toString());
									gen.push(var.sym.toString());
									gen.invokeStatic(RT_TYPE, Method.getMethod("clojure.lang.Var var(String,String)"));
									}
								else if(value instanceof IPersistentMap)
										{
										List entries = new ArrayList();
										for(Map.Entry entry : (Set<Map.Entry>) ((Map) value).entrySet())
											{
											entries.add(entry.getKey());
											entries.add(entry.getValue());
											}
										emitListAsObjectArray(entries, gen);
										gen.invokeStatic(RT_TYPE,
										                 Method.getMethod("clojure.lang.IPersistentMap map(Object[])"));
										}
									else if(value instanceof IPersistentVector)
											{
											emitListAsObjectArray(value, gen);
											gen.invokeStatic(RT_TYPE, Method.getMethod(
													"clojure.lang.IPersistentVector vector(Object[])"));
											}
										else if(value instanceof ISeq || value instanceof IPersistentList)
												{
												emitListAsObjectArray(value, gen);
												gen.invokeStatic(Type.getType(java.util.Arrays.class),
												                 Method.getMethod("java.util.List asList(Object[])"));
												gen.invokeStatic(Type.getType(PersistentList.class),
												                 Method.getMethod(
														                 "clojure.lang.IPersistentList create(java.util.List)"));
												}
											else
												{
												String cs = null;
												try
													{
													cs = RT.printString(value);
													//System.out.println("WARNING SLOW CODE: " + value.getClass() + " -> " + cs);
													}
												catch(Exception e)
													{
													throw new RuntimeException(
															"Can't embed object in code, maybe print-dup not defined: " +
															value);
													}
												if(cs.length() == 0)
													throw new RuntimeException(
															"Can't embed unreadable object in code: " + value);

												if(cs.startsWith("#<"))
													throw new RuntimeException(
															"Can't embed unreadable object in code: " + cs);

												gen.push(cs);
												gen.invokeStatic(RT_TYPE, readStringMethod);
												partial = false;
												}

		if(partial)
			{
			if(value instanceof Obj && RT.count(((Obj) value).meta()) > 0)
				{
				gen.checkCast(IOBJ_TYPE);
				emitValue(((Obj) value).meta(), gen);
				gen.checkCast(IPERSISTENTMAP_TYPE);
				gen.invokeInterface(IOBJ_TYPE,
				                    Method.getMethod("clojure.lang.IObj withMeta(clojure.lang.IPersistentMap)"));
				}
			}
	}


	void emitConstants(GeneratorAdapter clinitgen){
		try
			{
			Var.pushThreadBindings(RT.map(RT.PRINT_DUP, RT.T));

			for(int i = 0; i < constants.count(); i++)
				{
				emitValue(constants.nth(i), clinitgen);
				clinitgen.checkCast(constantType(i));
				clinitgen.putStatic(fntype, constantName(i), constantType(i));
				}
			}
		finally
			{
			Var.popThreadBindings();
			}
	}

//	void emitConstants(GeneratorAdapter clinitgen){
//		try
//			{
//			Var.pushThreadBindings(RT.map(RT.PRINT_DUP, RT.T));
//
//			for(int i = 0; i < constants.count(); i++)
//				{
//				Object o = constants.nth(i);
//				if(o instanceof String)
//					{
//					clinitgen.push((String) constants.nth(i));
//					}
//				else
//					{
//					String cs = null;
//					try
//						{
//						cs = RT.printString(o);
//						}
//					catch(Exception e)
//						{
//						throw new RuntimeException("Can't embed object in code, maybe print-dup not defined: "
//						                           + o);
//						}
//					if(cs.length() == 0)
//						throw new RuntimeException("Can't embed unreadable object in code: " + o);
//
//					if(cs.startsWith("#<"))
//						throw new RuntimeException("Can't embed unreadable object in code: " + cs);
//					clinitgen.push(cs);
//					clinitgen.invokeStatic(RT_TYPE, readStringMethod);
//					clinitgen.checkCast(constantType(i));
//					}
////				clinitgen.dup();
////				clinitgen.push(i);
////				clinitgen.arrayLoad(OBJECT_TYPE);
//				clinitgen.putStatic(fntype, constantName(i), constantType(i));
//				}
//			}
//		finally
//			{
//			Var.popThreadBindings();
//			}
//	}

	void emitClearCloses(GeneratorAdapter gen){
		int a = 1;
		for(ISeq s = RT.keys(closes); s != null; s = s.next(), ++a)
			{
			LocalBinding lb = (LocalBinding) s.first();
			Class primc = lb.getPrimitiveType();
			if(primc == null)
				{
				gen.loadThis();
				gen.visitInsn(Opcodes.ACONST_NULL);
				gen.putField(fntype, lb.name, OBJECT_TYPE);
				}
			}
	}

	synchronized Class getCompiledClass(){
		if(compiledClass == null)
			try
				{
				if(RT.booleanCast(COMPILE_FILES.deref()))
					compiledClass = RT.classForName(name);//loader.defineClass(name, bytecode);
				else
					{
					loader = (DynamicClassLoader) LOADER.deref();
					compiledClass = loader.defineClass(name, bytecode);
					}
				}
			catch(Exception e)
				{
				throw new RuntimeException(e);
				}
		return compiledClass;
	}

	public Object eval() throws Exception{
		return getCompiledClass().newInstance();
	}

	public void emitLetFnInits(GeneratorAdapter gen, FnExpr fn, IPersistentSet letFnLocals){
		//fn arg is enclosing fn, not this
		gen.checkCast(fntype);

		for(ISeq s = RT.keys(closes); s != null; s = s.next())
			{
			LocalBinding lb = (LocalBinding) s.first();
			if(letFnLocals.contains(lb))
				{
				Class primc = lb.getPrimitiveType();
				gen.dup();
				if(primc != null)
					{
					fn.emitUnboxedLocal(gen, lb);
					gen.putField(fntype, lb.name, Type.getType(primc));
					}
				else
					{
					fn.emitLocal(gen, lb);
					gen.putField(fntype, lb.name, OBJECT_TYPE);
					}
				}
			}
		gen.pop();

	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		//emitting a Fn means constructing an instance, feeding closed-overs from enclosing scope, if any
		//fn arg is enclosing fn, not this
		getCompiledClass();
		gen.newInstance(fntype);
		gen.dup();
		for(ISeq s = RT.keys(closes); s != null; s = s.next())
			{
			LocalBinding lb = (LocalBinding) s.first();
			if(lb.getPrimitiveType() != null)
				fn.emitUnboxedLocal(gen, lb);
			else
				fn.emitLocal(gen, lb);
			}
//		gen.invokeConstructor(fntype, new Method("<init>", Type.VOID_TYPE, ARG_TYPES[closes.count()]));
		gen.invokeConstructor(fntype, new Method("<init>", Type.VOID_TYPE, ctorTypes()));
		if(context == C.STATEMENT)
			{
			gen.pop();
			}
	}

	public boolean hasJavaClass() throws Exception{
		return true;
	}

	public Class getJavaClass() throws Exception{
		return (tag != null) ? HostExpr.tagToClass(tag) : IFn.class;
	}

	private void emitLocal(GeneratorAdapter gen, LocalBinding lb){
		if(closes.containsKey(lb))
			{
			Class primc = lb.getPrimitiveType();
			gen.loadThis();
			if(primc != null)
				{
				gen.getField(fntype, lb.name, Type.getType(primc));
				HostExpr.emitBoxReturn(this, gen, primc);
				}
			else
				gen.getField(fntype, lb.name, OBJECT_TYPE);
			}
		else
			{
			Class primc = lb.getPrimitiveType();
			if(primc != null)
				{
				gen.visitVarInsn(Type.getType(primc).getOpcode(Opcodes.ILOAD), lb.idx);
				HostExpr.emitBoxReturn(this, gen, primc);
				}
			else
				gen.visitVarInsn(OBJECT_TYPE.getOpcode(Opcodes.ILOAD), lb.idx);
			}
	}

	private void emitUnboxedLocal(GeneratorAdapter gen, LocalBinding lb){
		Class primc = lb.getPrimitiveType();
		if(closes.containsKey(lb))
			{
			gen.loadThis();
			gen.getField(fntype, lb.name, Type.getType(primc));
			}
		else
			gen.visitVarInsn(Type.getType(primc).getOpcode(Opcodes.ILOAD), lb.idx);
	}

	public void emitVar(GeneratorAdapter gen, Var var){
		Integer i = (Integer) vars.valAt(var);
		emitConstant(gen, i);
		//gen.getStatic(fntype, munge(var.sym.toString()), VAR_TYPE);
	}

	public void emitKeyword(GeneratorAdapter gen, Keyword k){
		Integer i = (Integer) keywords.valAt(k);
		emitConstant(gen, i);
//		gen.getStatic(fntype, munge(k.sym.toString()), KEYWORD_TYPE);
	}

	public void emitConstant(GeneratorAdapter gen, int id){
		gen.getStatic(fntype, constantName(id), constantType(id));
	}


	String constantName(int id){
		return CONST_PREFIX + id;
	}

	Type constantType(int id){
		Object o = constants.nth(id);
		Class c = o.getClass();
		if(Modifier.isPublic(c.getModifiers()))
			{
			//can't emit derived fn types due to visibility
			if(LazySeq.class.isAssignableFrom(c))
				return Type.getType(ISeq.class);
			else if(RestFn.class.isAssignableFrom(c))
				return Type.getType(RestFn.class);
			else if(AFn.class.isAssignableFrom(c))
					return Type.getType(AFn.class);
				else if(c == Var.class)
						return Type.getType(Var.class);
					else if(c == String.class)
							return Type.getType(String.class);
//			return Type.getType(c);
			}
		return OBJECT_TYPE;
	}

}

enum PSTATE{
	REQ, REST, DONE
}


public static class FnMethod{
	//when closures are defined inside other closures,
	//the closed over locals need to be propagated to the enclosing fn
	public final FnMethod parent;
	//localbinding->localbinding
	IPersistentMap locals = null;
	//num->localbinding
	IPersistentMap indexlocals = null;
	//localbinding->localbinding
	PersistentVector reqParms = PersistentVector.EMPTY;
	LocalBinding restParm = null;
	Expr body = null;
	FnExpr fn;
	PersistentVector argLocals;
	int maxLocal = 0;
	int line;
	PersistentHashSet localsUsedInCatchFinally = PersistentHashSet.EMPTY;

	public final IPersistentMap locals(){
		return locals;
	}

	public final PersistentVector reqParms(){
		return reqParms;
	}

	public final LocalBinding restParm(){
		return restParm;
	}

	public final Expr body(){
		return body;
	}

	public final FnExpr fn(){
		return fn;
	}

	public final PersistentVector argLocals(){
		return argLocals;
	}

	public final int maxLocal(){
		return maxLocal;
	}

	public final int line(){
		return line;
	}

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
		ISeq body = RT.next(form);
		try
			{
			FnMethod method = new FnMethod(fn, (FnMethod) METHOD.deref());
			method.line = (Integer) LINE.deref();
			//register as the current method and set up a new env frame
			Var.pushThreadBindings(
					RT.map(
							METHOD, method,
							LOCAL_ENV, LOCAL_ENV.deref(),
							LOOP_LOCALS, null,
							NEXT_LOCAL_NUM, 0));

			//register 'this' as local 0
			//registerLocal(THISFN, null, null);
			registerLocal(Symbol.intern(fn.thisName != null ? fn.thisName : "fn__" + RT.nextID()), null, null);

			PSTATE state = PSTATE.REQ;
			PersistentVector argLocals = PersistentVector.EMPTY;
			for(int i = 0; i < parms.count(); i++)
				{
				if(!(parms.nth(i) instanceof Symbol))
					throw new IllegalArgumentException("fn params must be Symbols");
				Symbol p = (Symbol) parms.nth(i);
				if(p.getNamespace() != null)
					throw new Exception("Can't use qualified name as parameter: " + p);
				if(p.equals(_AMP_))
					{
					if(state == PSTATE.REQ)
						state = PSTATE.REST;
					else
						throw new Exception("Invalid parameter list");
					}

				else
					{
					LocalBinding lb = registerLocal(p, state == PSTATE.REST ? ISEQ : tagOf(p), null);
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
			Var.pushThreadBindings(RT.map(LOOP_LABEL, loopLabel, METHOD, this));
			body.emit(C.RETURN, fn, gen);
			Label end = gen.mark();
			gen.visitLocalVariable("this", "Ljava/lang/Object;", null, loopLabel, end, 0);
			for(ISeq lbs = argLocals.seq(); lbs != null; lbs = lbs.next())
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

	void emitClearLocals(GeneratorAdapter gen){
		//this seems shaky given primitive locals - revisit
		for(int i = 1; i < numParams() + 1; i++)
			{
			if(!localsUsedInCatchFinally.contains(i))
				{
				gen.visitInsn(Opcodes.ACONST_NULL);
				gen.visitVarInsn(OBJECT_TYPE.getOpcode(Opcodes.ISTORE), i);
//				gen.storeArg(i);
				}
			}
		for(int i = numParams() + 1; i < maxLocal + 1; i++)
			{
			if(!localsUsedInCatchFinally.contains(i))
				{
				LocalBinding b = (LocalBinding) RT.get(indexlocals, i);
				if(b == null || maybePrimitiveType(b.init) == null)
					{
					gen.visitInsn(Opcodes.ACONST_NULL);
					gen.visitVarInsn(OBJECT_TYPE.getOpcode(Opcodes.ISTORE), i);
					}
				}
			}
		if(fn.onceOnly)
			{
			fn.emitClearCloses(gen);
			}
	}
}

public static class LocalBinding{
	public final Symbol sym;
	public final Symbol tag;
	public Expr init;
	public final int idx;
	public final String name;

	public LocalBinding(int num, Symbol sym, Symbol tag, Expr init) throws Exception{
		if(maybePrimitiveType(init) != null && tag != null)
			throw new UnsupportedOperationException("Can't type hint a local with a primitive initializer");
		this.idx = num;
		this.sym = sym;
		this.tag = tag;
		this.init = init;
		name = munge(sym.name);
	}

	public boolean hasJavaClass() throws Exception{
		if(init != null && init.hasJavaClass()
		   && Util.isPrimitive(init.getJavaClass())
		   && !(init instanceof MaybePrimitiveExpr))
			return false;
		return tag != null
		       || (init != null && init.hasJavaClass());
	}

	public Class getJavaClass() throws Exception{
		return tag != null ? HostExpr.tagToClass(tag)
		                   : init.getJavaClass();
	}

	public Class getPrimitiveType(){
		return maybePrimitiveType(init);
	}
}

public static class LocalBindingExpr implements Expr, MaybePrimitiveExpr{
	public final LocalBinding b;
	public final Symbol tag;

	public LocalBindingExpr(LocalBinding b, Symbol tag) throws Exception{
		if(b.getPrimitiveType() != null && tag != null)
			throw new UnsupportedOperationException("Can't type hint a primitive local");
		this.b = b;
		this.tag = tag;
	}

	public Object eval() throws Exception{
		throw new UnsupportedOperationException("Can't eval locals");
	}

	public void emitUnboxed(C context, FnExpr fn, GeneratorAdapter gen){
		fn.emitUnboxedLocal(gen, b);
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

public static class BodyExpr implements Expr{
	PersistentVector exprs;

	public final PersistentVector exprs(){
		return exprs;
	}

	public BodyExpr(PersistentVector exprs){
		this.exprs = exprs;
	}

	static class Parser implements IParser{
		public Expr parse(C context, Object frms) throws Exception{
			ISeq forms = (ISeq) frms;
			if(Util.equals(RT.first(forms), DO))
				forms = RT.next(forms);
			PersistentVector exprs = PersistentVector.EMPTY;
			for(; forms != null; forms = forms.next())
				{
				Expr e = (context != C.EVAL &&
				          (context == C.STATEMENT || forms.next() != null)) ?
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

public static class BindingInit{
	LocalBinding binding;
	Expr init;

	public final LocalBinding binding(){
		return binding;
	}

	public final Expr init(){
		return init;
	}

	public BindingInit(LocalBinding binding, Expr init){
		this.binding = binding;
		this.init = init;
	}
}

public static class LetFnExpr implements Expr{
	public final PersistentVector bindingInits;
	public final Expr body;

	public LetFnExpr(PersistentVector bindingInits, Expr body){
		this.bindingInits = bindingInits;
		this.body = body;
	}

	static class Parser implements IParser{
		public Expr parse(C context, Object frm) throws Exception{
			ISeq form = (ISeq) frm;
			//(letfns* [var (fn [args] body) ...] body...)
			if(!(RT.second(form) instanceof IPersistentVector))
				throw new IllegalArgumentException("Bad binding form, expected vector");

			IPersistentVector bindings = (IPersistentVector) RT.second(form);
			if((bindings.count() % 2) != 0)
				throw new IllegalArgumentException("Bad binding form, expected matched symbol expression pairs");

			ISeq body = RT.next(RT.next(form));

			if(context == C.EVAL)
				return analyze(context, RT.list(RT.list(FN, PersistentVector.EMPTY, form)));

			IPersistentMap dynamicBindings = RT.map(LOCAL_ENV, LOCAL_ENV.deref(),
			                                        NEXT_LOCAL_NUM, NEXT_LOCAL_NUM.deref());

			try
				{
				Var.pushThreadBindings(dynamicBindings);

				//pre-seed env (like Lisp labels)
				PersistentVector lbs = PersistentVector.EMPTY;
				for(int i = 0; i < bindings.count(); i += 2)
					{
					if(!(bindings.nth(i) instanceof Symbol))
						throw new IllegalArgumentException(
								"Bad binding form, expected symbol, got: " + bindings.nth(i));
					Symbol sym = (Symbol) bindings.nth(i);
					if(sym.getNamespace() != null)
						throw new Exception("Can't let qualified name: " + sym);
					LocalBinding lb = registerLocal(sym, tagOf(sym), null);
					lbs = lbs.cons(lb);
					}
				PersistentVector bindingInits = PersistentVector.EMPTY;
				for(int i = 0; i < bindings.count(); i += 2)
					{
					Symbol sym = (Symbol) bindings.nth(i);
					Expr init = analyze(C.EXPRESSION, bindings.nth(i + 1), sym.name);
					LocalBinding lb = (LocalBinding) lbs.nth(i / 2);
					lb.init = init;
					BindingInit bi = new BindingInit(lb, init);
					bindingInits = bindingInits.cons(bi);
					}
				return new LetFnExpr(bindingInits, (new BodyExpr.Parser()).parse(context, body));
				}
			finally
				{
				Var.popThreadBindings();
				}
		}
	}

	public Object eval() throws Exception{
		throw new UnsupportedOperationException("Can't eval letfns");
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		for(int i = 0; i < bindingInits.count(); i++)
			{
			BindingInit bi = (BindingInit) bindingInits.nth(i);
			gen.visitInsn(Opcodes.ACONST_NULL);
			gen.visitVarInsn(OBJECT_TYPE.getOpcode(Opcodes.ISTORE), bi.binding.idx);
			}

		IPersistentSet lbset = PersistentHashSet.EMPTY;

		for(int i = 0; i < bindingInits.count(); i++)
			{
			BindingInit bi = (BindingInit) bindingInits.nth(i);
			lbset = (IPersistentSet) lbset.cons(bi.binding);
			bi.init.emit(C.EXPRESSION, fn, gen);
			gen.visitVarInsn(OBJECT_TYPE.getOpcode(Opcodes.ISTORE), bi.binding.idx);
			}

		for(int i = 0; i < bindingInits.count(); i++)
			{
			BindingInit bi = (BindingInit) bindingInits.nth(i);
			FnExpr fe = (FnExpr) bi.init;
			gen.visitVarInsn(OBJECT_TYPE.getOpcode(Opcodes.ILOAD), bi.binding.idx);
			fe.emitLetFnInits(gen, fn, lbset);
			}

		Label loopLabel = gen.mark();

		body.emit(context, fn, gen);

		Label end = gen.mark();
//		gen.visitLocalVariable("this", "Ljava/lang/Object;", null, loopLabel, end, 0);
		for(ISeq bis = bindingInits.seq(); bis != null; bis = bis.next())
			{
			BindingInit bi = (BindingInit) bis.first();
			String lname = bi.binding.name;
			if(lname.endsWith("__auto__"))
				lname += RT.nextID();
			Class primc = maybePrimitiveType(bi.init);
			if(primc != null)
				gen.visitLocalVariable(lname, Type.getDescriptor(primc), null, loopLabel, end,
				                       bi.binding.idx);
			else
				gen.visitLocalVariable(lname, "Ljava/lang/Object;", null, loopLabel, end, bi.binding.idx);
			}
	}

	public boolean hasJavaClass() throws Exception{
		return body.hasJavaClass();
	}

	public Class getJavaClass() throws Exception{
		return body.getJavaClass();
	}
}

public static class LetExpr implements Expr{
	public final PersistentVector bindingInits;
	public final Expr body;
	public final boolean isLoop;

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

			ISeq body = RT.next(RT.next(form));

			if(context == C.EVAL
			   || (context == C.EXPRESSION && isLoop))
				return analyze(context, RT.list(RT.list(FN, PersistentVector.EMPTY, form)));

			IPersistentMap dynamicBindings = RT.map(LOCAL_ENV, LOCAL_ENV.deref(),
			                                        NEXT_LOCAL_NUM, NEXT_LOCAL_NUM.deref());
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
					if(sym.getNamespace() != null)
						throw new Exception("Can't let qualified name: " + sym);
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
			Class primc = maybePrimitiveType(bi.init);
			if(primc != null)
				{
				((MaybePrimitiveExpr) bi.init).emitUnboxed(C.EXPRESSION, fn, gen);
				gen.visitVarInsn(Type.getType(primc).getOpcode(Opcodes.ISTORE), bi.binding.idx);
				}
			else
				{
				bi.init.emit(C.EXPRESSION, fn, gen);
				gen.visitVarInsn(OBJECT_TYPE.getOpcode(Opcodes.ISTORE), bi.binding.idx);
				}
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
//		gen.visitLocalVariable("this", "Ljava/lang/Object;", null, loopLabel, end, 0);
		for(ISeq bis = bindingInits.seq(); bis != null; bis = bis.next())
			{
			BindingInit bi = (BindingInit) bis.first();
			String lname = bi.binding.name;
			if(lname.endsWith("__auto__"))
				lname += RT.nextID();
			Class primc = maybePrimitiveType(bi.init);
			if(primc != null)
				gen.visitLocalVariable(lname, Type.getDescriptor(primc), null, loopLabel, end,
				                       bi.binding.idx);
			else
				gen.visitLocalVariable(lname, "Ljava/lang/Object;", null, loopLabel, end, bi.binding.idx);
			}
	}

	public boolean hasJavaClass() throws Exception{
		return body.hasJavaClass();
	}

	public Class getJavaClass() throws Exception{
		return body.getJavaClass();
	}
}

public static class RecurExpr implements Expr{
	public final IPersistentVector args;
	public final IPersistentVector loopLocals;

	public RecurExpr(IPersistentVector loopLocals, IPersistentVector args){
		this.loopLocals = loopLocals;
		this.args = args;
	}

	public Object eval() throws Exception{
		throw new UnsupportedOperationException("Can't eval recur");
	}

	public void emit(C context, FnExpr fn, GeneratorAdapter gen){
		Label loopLabel = (Label) LOOP_LABEL.deref();
		if(loopLabel == null)
			throw new IllegalStateException();
		for(int i = 0; i < loopLocals.count(); i++)
			{
			LocalBinding lb = (LocalBinding) loopLocals.nth(i);
			Expr arg = (Expr) args.nth(i);
			if(lb.getPrimitiveType() != null)
				{
				Class primc = lb.getPrimitiveType();
				try
					{
					if(!(arg instanceof MaybePrimitiveExpr && arg.hasJavaClass() && arg.getJavaClass() == primc))
						throw new IllegalArgumentException("recur arg for primitive local: " +
						                                   lb.name + " must be matching primitive");
					}
				catch(Exception e)
					{
					throw new RuntimeException(e);
					}
				((MaybePrimitiveExpr) arg).emitUnboxed(C.EXPRESSION, fn, gen);
				}
			else
				{
				arg.emit(C.EXPRESSION, fn, gen);
				}
			}

		for(int i = loopLocals.count() - 1; i >= 0; i--)
			{
			LocalBinding lb = (LocalBinding) loopLocals.nth(i);
			Class primc = lb.getPrimitiveType();
			if(primc != null)
				gen.visitVarInsn(Type.getType(primc).getOpcode(Opcodes.ISTORE), lb.idx);
			else
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
			IPersistentVector loopLocals = (IPersistentVector) LOOP_LOCALS.deref();
			if(context != C.RETURN || loopLocals == null)
				throw new UnsupportedOperationException("Can only recur from tail position");
			if(IN_CATCH_FINALLY.deref() != null)
				throw new UnsupportedOperationException("Cannot recur from catch/finally");
			PersistentVector args = PersistentVector.EMPTY;
			for(ISeq s = RT.seq(form.next()); s != null; s = s.next())
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
	IPersistentMap localsMap = (IPersistentMap) LOCAL_ENV.deref();
	LOCAL_ENV.set(RT.assoc(localsMap, b.sym, b));
	FnMethod method = (FnMethod) METHOD.deref();
	method.locals = (IPersistentMap) RT.assoc(method.locals, b, b);
	method.indexlocals = (IPersistentMap) RT.assoc(method.indexlocals, num, b);
	return b;
}

private static int getAndIncLocalNum(){
	int num = ((Number) NEXT_LOCAL_NUM.deref()).intValue();
	FnMethod m = (FnMethod) METHOD.deref();
	if(num > m.maxLocal)
		m.maxLocal = num;
	NEXT_LOCAL_NUM.set(num + 1);
	return num;
}

public static Expr analyze(C context, Object form) throws Exception{
	return analyze(context, form, null);
}

private static Expr analyze(C context, Object form, String name) throws Exception{
	//todo symbol macro expansion?
	try
		{
		if(form instanceof LazySeq)
			{
			form = RT.seq(form);
			if(form == null)
				form = PersistentList.EMPTY;
			}
		if(form == null)
			return NIL_EXPR;
		else if(form == Boolean.TRUE)
			return TRUE_EXPR;
		else if(form == Boolean.FALSE)
				return FALSE_EXPR;
		Class fclass = form.getClass();
		if(fclass == Symbol.class)
			return analyzeSymbol((Symbol) form);
		else if(fclass == Keyword.class)
			return registerKeyword((Keyword) form);
//	else if(form instanceof Num)
//		return new NumExpr((Num) form);
		else if(fclass == String.class)
				return new StringExpr(((String) form).intern());
//	else if(fclass == Character.class)
//		return new CharExpr((Character) form);
			else if(form instanceof IPersistentCollection && ((IPersistentCollection) form).count() == 0)
					{
					Expr ret = new EmptyExpr(form);
					if(RT.meta(form) != null)
						ret = new MetaExpr(ret, (MapExpr) MapExpr
								.parse(context == C.EVAL ? context : C.EXPRESSION, ((IObj) form).meta()));
					return ret;
					}
				else if(form instanceof ISeq)
						return analyzeSeq(context, (ISeq) form, name);
					else if(form instanceof IPersistentVector)
							return VectorExpr.parse(context, (IPersistentVector) form);
						else if(form instanceof IPersistentMap)
								return MapExpr.parse(context, (IPersistentMap) form);
							else if(form instanceof IPersistentSet)
									return SetExpr.parse(context, (IPersistentSet) form);

//	else
		//throw new UnsupportedOperationException();
		return new ConstantExpr(form);
		}
	catch(Throwable e)
		{
		if(!(e instanceof CompilerException))
			throw new CompilerException((String) SOURCE.deref(), (Integer) LINE.deref(), e);
		else
			throw (CompilerException) e;
		}
}

static public class CompilerException extends Exception{

	public CompilerException(String source, int line, Throwable cause){
		super(errorMsg(source, line, cause.toString()), cause);
	}

	public String toString(){
		return getMessage();
	}
}

static public Var isMacro(Object op) throws Exception{
	//no local macros for now
	if(op instanceof Symbol && referenceLocal((Symbol) op) != null)
		return null;
	if(op instanceof Symbol || op instanceof Var)
		{
		Var v = (op instanceof Var) ? (Var) op : lookupVar((Symbol) op, false);
		if(v != null && v.isMacro())
			{
			if(v.ns != currentNS() && !v.isPublic())
				throw new IllegalStateException("var: " + v + " is not public");
			return v;
			}
		}
	return null;
}

static public IFn isInline(Object op, int arity) throws Exception{
	//no local inlines for now
	if(op instanceof Symbol && referenceLocal((Symbol) op) != null)
		return null;
	if(op instanceof Symbol || op instanceof Var)
		{
		Var v = (op instanceof Var) ? (Var) op : lookupVar((Symbol) op, false);
		if(v != null)
			{
			if(v.ns != currentNS() && !v.isPublic())
				throw new IllegalStateException("var: " + v + " is not public");
			IFn ret = (IFn) RT.get(v.meta(), inlineKey);
			if(ret != null)
				{
				IPersistentSet arities = (IPersistentSet) RT.get(v.meta(), inlineAritiesKey);
				if(arities == null || arities.contains(arity))
					return ret;
				}
			}
		}
	return null;
}

public static boolean namesStaticMember(Symbol sym){
	return sym.ns != null && namespaceFor(sym) == null;
}

public static Object macroexpand1(Object x) throws Exception{
	if(x instanceof ISeq)
		{
		ISeq form = (ISeq) x;
		Object op = RT.first(form);
		if(isSpecial(op))
			return x;
		//macro expansion
		Var v = isMacro(op);
		if(v != null)
			{
			try
				{
				Var.pushThreadBindings(RT.map(RT.MACRO_META, RT.meta(form)));
				return v.applyTo(form.next());
				}
			finally
				{
				Var.popThreadBindings();
				}
			}
		else
			{
			if(op instanceof Symbol)
				{
				Symbol sym = (Symbol) op;
				String sname = sym.name;
				//(.substring s 2 5) => (. s substring 2 5)
				if(sym.name.charAt(0) == '.')
					{
					if(RT.length(form) < 2)
						throw new IllegalArgumentException(
								"Malformed member expression, expecting (.member target ...)");
					Symbol meth = Symbol.intern(sname.substring(1));
					Object target = RT.second(form);
					if(HostExpr.maybeClass(target, false) != null)
						{
						target = ((IObj)RT.list(IDENTITY, target)).withMeta(RT.map(RT.TAG_KEY,CLASS));
						}
					return RT.listStar(DOT, target, meth, form.next().next());
					}
				else if(namesStaticMember(sym))
					{
					Symbol target = Symbol.intern(sym.ns);
					Class c = HostExpr.maybeClass(target, false);
					if(c != null)
						{
						Symbol meth = Symbol.intern(sym.name);
						return RT.listStar(DOT, target, meth, form.next());
						}
					}
				else
					{
					//(s.substring 2 5) => (. s substring 2 5)
					//also (package.class.name ...) (. package.class name ...)
					int idx = sname.lastIndexOf('.');
//					if(idx > 0 && idx < sname.length() - 1)
//						{
//						Symbol target = Symbol.intern(sname.substring(0, idx));
//						Symbol meth = Symbol.intern(sname.substring(idx + 1));
//						return RT.listStar(DOT, target, meth, form.rest());
//						}
					//(StringBuilder. "foo") => (new StringBuilder "foo")	
					//else 
					if(idx == sname.length() - 1)
						return RT.listStar(NEW, Symbol.intern(sname.substring(0, idx)), form.next());
					}
				}
			}
		}
	return x;
}

static Object macroexpand(Object form) throws Exception{
	Object exf = macroexpand1(form);
	if(exf != form)
		return macroexpand(exf);
	return form;
}

private static Expr analyzeSeq(C context, ISeq form, String name) throws Exception{
	Integer line = (Integer) LINE.deref();
	if(RT.meta(form) != null && RT.meta(form).containsKey(RT.LINE_KEY))
		line = (Integer) RT.meta(form).valAt(RT.LINE_KEY);
	Var.pushThreadBindings(
			RT.map(LINE, line));
	try
		{
		Object me = macroexpand1(form);
		if(me != form)
			return analyze(context, me, name);

		Object op = RT.first(form);
		if(op == null)
			throw new IllegalArgumentException("Can't call nil");
		IFn inline = isInline(op, RT.count(RT.next(form)));
		if(inline != null)
			return analyze(context, inline.applyTo(RT.next(form)));
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
		if(!(e instanceof CompilerException))
			throw new CompilerException((String) SOURCE.deref(), (Integer) LINE.deref(), e);
		else
			throw (CompilerException) e;
		}
	finally
		{
		Var.popThreadBindings();
		}
}

static String errorMsg(String source, int line, String s){
	return String.format("%s (%s:%d)", s, source, line);
}

public static Object eval(Object form) throws Exception{
	boolean createdLoader = false;
	if(true)//!LOADER.isBound())
		{
		Var.pushThreadBindings(RT.map(LOADER, RT.makeClassLoader()));
		createdLoader = true;
		}
	try
		{
		Integer line = (Integer) LINE.deref();
		if(RT.meta(form) != null && RT.meta(form).containsKey(RT.LINE_KEY))
			line = (Integer) RT.meta(form).valAt(RT.LINE_KEY);
		Var.pushThreadBindings(RT.map(LINE, line));
		try
			{
			form = macroexpand(form);
			if(form instanceof IPersistentCollection && Util.equals(RT.first(form), DO))
				{
				ISeq s = RT.next(form);
				for(; RT.next(s) != null; s = RT.next(s))
					eval(RT.first(s));
				return eval(RT.first(s));
				}
			else if(form instanceof IPersistentCollection
			        && !(RT.first(form) instanceof Symbol
			             && ((Symbol) RT.first(form)).name.startsWith("def")))
				{
				FnExpr fexpr = (FnExpr) analyze(C.EXPRESSION, RT.list(FN, PersistentVector.EMPTY, form), "eval");
				IFn fn = (IFn) fexpr.eval();
				return fn.invoke();
				}
			else
				{
				Expr expr = analyze(C.EVAL, form);
				return expr.eval();
				}
			}
		finally
			{
			Var.popThreadBindings();
			}
		}
	catch(Throwable e)
		{
		if(!(e instanceof CompilerException))
			throw new CompilerException((String) SOURCE.deref(), (Integer) LINE.deref(), e);
		else
			throw (CompilerException) e;
		}
	finally
		{
		if(createdLoader)
			Var.popThreadBindings();
		}
}

private static int registerConstant(Object o){
	if(!CONSTANTS.isBound())
		return -1;
	PersistentVector v = (PersistentVector) CONSTANTS.deref();
	CONSTANTS.set(RT.conj(v, o));
	return v.count();
}

private static KeywordExpr registerKeyword(Keyword keyword){
	if(!KEYWORDS.isBound())
		return new KeywordExpr(keyword);

	IPersistentMap keywordsMap = (IPersistentMap) KEYWORDS.deref();
	Object id = RT.get(keywordsMap, keyword);
	if(id == null)
		{
		KEYWORDS.set(RT.assoc(keywordsMap, keyword, registerConstant(keyword)));
		}
	return new KeywordExpr(keyword);
//	KeywordExpr ke = (KeywordExpr) RT.get(keywordsMap, keyword);
//	if(ke == null)
//		KEYWORDS.set(RT.assoc(keywordsMap, keyword, ke = new KeywordExpr(keyword)));
//	return ke;
}

private static Expr analyzeSymbol(Symbol sym) throws Exception{
	Symbol tag = tagOf(sym);
	if(sym.ns == null) //ns-qualified syms are always Vars
		{
		LocalBinding b = referenceLocal(sym);
		if(b != null)
			return new LocalBindingExpr(b, tag);
		}
	else
		{
		if(namespaceFor(sym) == null)
			{
			Symbol nsSym = Symbol.create(sym.ns);
			Class c = HostExpr.maybeClass(nsSym, false);
			if(c != null)
				{
				if(Reflector.getField(c, sym.name, true) != null)
					return new StaticFieldExpr((Integer) LINE.deref(), c, sym.name);
				throw new Exception("Unable to find static field: " + sym.name + " in " + c);
				}
			}
		}
	//Var v = lookupVar(sym, false);
//	Var v = lookupVar(sym, false);
//	if(v != null)
//		return new VarExpr(v, tag);
	Object o = resolve(sym);
	if(o instanceof Var)
		{
		Var v = (Var) o;
		if(isMacro(v) != null)
			throw new Exception("Can't take value of a macro: " + v);
		registerVar(v);
		return new VarExpr(v, tag);
		}
	else if(o instanceof Class)
		return new ConstantExpr(o);
	else if(o instanceof Symbol)
			return new UnresolvedVarExpr((Symbol) o);

	throw new Exception("Unable to resolve symbol: " + sym + " in this context");

}

static Object resolve(Symbol sym, boolean allowPrivate) throws Exception{
	return resolveIn(currentNS(), sym, allowPrivate);
}

static Object resolve(Symbol sym) throws Exception{
	return resolveIn(currentNS(), sym, false);
}

static Namespace namespaceFor(Symbol sym){
	return namespaceFor(currentNS(), sym);
}

static Namespace namespaceFor(Namespace inns, Symbol sym){
	//note, presumes non-nil sym.ns
	// first check against currentNS' aliases...
	Symbol nsSym = Symbol.create(sym.ns);
	Namespace ns = inns.lookupAlias(nsSym);
	if(ns == null)
		{
		// ...otherwise check the Namespaces map.
		ns = Namespace.find(nsSym);
		}
	return ns;
}

static public Object resolveIn(Namespace n, Symbol sym, boolean allowPrivate) throws Exception{
	//note - ns-qualified vars must already exist
	if(sym.ns != null)
		{
		Namespace ns = namespaceFor(n, sym);
		if(ns == null)
			throw new Exception("No such namespace: " + sym.ns);

		Var v = ns.findInternedVar(Symbol.create(sym.name));
		if(v == null)
			throw new Exception("No such var: " + sym);
		else if(v.ns != currentNS() && !v.isPublic() && !allowPrivate)
			throw new IllegalStateException("var: " + sym + " is not public");
		return v;
		}
	else if(sym.name.indexOf('.') > 0 || sym.name.charAt(0) == '[')
		{
		return RT.classForName(sym.name);
		}
	else if(sym.equals(NS))
			return RT.NS_VAR;
		else if(sym.equals(IN_NS))
				return RT.IN_NS_VAR;
			else
				{
				Object o = n.getMapping(sym);
				if(o == null)
					{
					if(RT.booleanCast(RT.ALLOW_UNRESOLVED_VARS.deref()))
						{
						return sym;
						}
					else
						{
						throw new Exception("Unable to resolve symbol: " + sym + " in this context");
						}
					}
				return o;
				}
}


static public Object maybeResolveIn(Namespace n, Symbol sym) throws Exception{
	//note - ns-qualified vars must already exist
	if(sym.ns != null)
		{
		Namespace ns = namespaceFor(n, sym);
		if(ns == null)
			return null;
		Var v = ns.findInternedVar(Symbol.create(sym.name));
		if(v == null)
			return null;
		return v;
		}
	else if(sym.name.indexOf('.') > 0 || sym.name.charAt(0) == '[')
		{
		return RT.classForName(sym.name);
		}
	else if(sym.equals(NS))
			return RT.NS_VAR;
		else if(sym.equals(IN_NS))
				return RT.IN_NS_VAR;
			else
				{
				Object o = n.getMapping(sym);
				return o;
				}
}


static Var lookupVar(Symbol sym, boolean internNew) throws Exception{
	Var var = null;

	//note - ns-qualified vars in other namespaces must already exist
	if(sym.ns != null)
		{
		Namespace ns = namespaceFor(sym);
		if(ns == null)
			return null;
		//throw new Exception("No such namespace: " + sym.ns);
		Symbol name = Symbol.create(sym.name);
		if(internNew && ns == currentNS())
			var = currentNS().intern(name);
		else
			var = ns.findInternedVar(name);
		}
	else if(sym.equals(NS))
		var = RT.NS_VAR;
	else if(sym.equals(IN_NS))
			var = RT.IN_NS_VAR;
		else
			{
			//is it mapped?
			Object o = currentNS().getMapping(sym);
			if(o == null)
				{
				//introduce a new var in the current ns
				if(internNew)
					var = currentNS().intern(Symbol.create(sym.name));
				}
			else if(o instanceof Var)
				{
				var = (Var) o;
				}
			else
				{
				throw new Exception("Expecting var, but " + sym + " is mapped to " + o);
				}
			}
	if(var != null)
		registerVar(var);
	return var;
}

private static void registerVar(Var var) throws Exception{
	if(!VARS.isBound())
		return;
	IPersistentMap varsMap = (IPersistentMap) VARS.deref();
	Object id = RT.get(varsMap, var);
	if(id == null)
		{
		VARS.set(RT.assoc(varsMap, var, registerConstant(var)));
		}
//	if(varsMap != null && RT.get(varsMap, var) == null)
//		VARS.set(RT.assoc(varsMap, var, var));
}

static Namespace currentNS(){
	return (Namespace) RT.CURRENT_NS.deref();
}

static void closeOver(LocalBinding b, FnMethod method){
	if(b != null && method != null)
		{
		if(RT.get(method.locals, b) == null)
			{
			method.fn.closes = (IPersistentMap) RT.assoc(method.fn.closes, b, b);
			closeOver(b, method.parent);
			}
		else if(IN_CATCH_FINALLY.deref() != null)
			{
			method.localsUsedInCatchFinally = (PersistentHashSet) method.localsUsedInCatchFinally.cons(b.idx);
			}
		}
}


static LocalBinding referenceLocal(Symbol sym) throws Exception{
	if(!LOCAL_ENV.isBound())
		return null;
	LocalBinding b = (LocalBinding) RT.get(LOCAL_ENV.deref(), sym);
	if(b != null)
		{
		FnMethod method = (FnMethod) METHOD.deref();
		closeOver(b, method);
		}
	return b;
}

private static Symbol tagOf(Object o){
	Object tag = RT.get(RT.meta(o), RT.TAG_KEY);
	if(tag instanceof Symbol)
		return (Symbol) tag;
	else if(tag instanceof String)
		return Symbol.intern(null, (String) tag);
	return null;
}

public static Object loadFile(String file) throws Exception{
//	File fo = new File(file);
//	if(!fo.exists())
//		return null;

	FileInputStream f = new FileInputStream(file);
	try
		{
		return load(new InputStreamReader(f, RT.UTF8), new File(file).getAbsolutePath(), (new File(file)).getName());
		}
	finally
		{
		f.close();
		}
}

public static Object load(Reader rdr) throws Exception{
	return load(rdr, null, "NO_SOURCE_FILE");
}

public static Object load(Reader rdr, String sourcePath, String sourceName) throws Exception{
	Object EOF = new Object();
	Object ret = null;
	LineNumberingPushbackReader pushbackReader =
			(rdr instanceof LineNumberingPushbackReader) ? (LineNumberingPushbackReader) rdr :
			new LineNumberingPushbackReader(rdr);
	Var.pushThreadBindings(
			RT.map(LOADER, RT.makeClassLoader(),
			       SOURCE_PATH, sourcePath,
			       SOURCE, sourceName,
			       RT.CURRENT_NS, RT.CURRENT_NS.deref(),
			       LINE_BEFORE, pushbackReader.getLineNumber(),
			       LINE_AFTER, pushbackReader.getLineNumber()
			));

	try
		{
		for(Object r = LispReader.read(pushbackReader, false, EOF, false); r != EOF;
		    r = LispReader.read(pushbackReader, false, EOF, false))
			{
			LINE_AFTER.set(pushbackReader.getLineNumber());
			ret = eval(r);
			LINE_BEFORE.set(pushbackReader.getLineNumber());
			}
		}
	catch(LispReader.ReaderException e)
		{
		throw new CompilerException(sourceName, e.line, e.getCause());
		}
	finally
		{
		Var.popThreadBindings();
		}
	return ret;
}

static public void writeClassFile(String internalName, byte[] bytecode) throws Exception{
	String genPath = (String) COMPILE_PATH.deref();
	if(genPath == null)
		throw new Exception("*compile-path* not set");
	String[] dirs = internalName.split("/");
	String p = genPath;
	for(int i = 0; i < dirs.length - 1; i++)
		{
		p += File.separator + dirs[i];
		(new File(p)).mkdir();
		}
	String path = genPath + File.separator + internalName + ".class";
	File cf = new File(path);
	cf.createNewFile();
	FileOutputStream cfs = new FileOutputStream(cf);
	try
		{
		cfs.write(bytecode);
		cfs.flush();
		cfs.getFD().sync();
		}
	finally
		{
		cfs.close();
		}
}

public static void pushNS(){
	Var.pushThreadBindings(PersistentHashMap.create(Var.intern(Symbol.create("clojure.core"),
	                                                           Symbol.create("*ns*")), null));
}

static void compile1(GeneratorAdapter gen, FnExpr fn, Object form) throws Exception{
	Integer line = (Integer) LINE.deref();
	if(RT.meta(form) != null && RT.meta(form).containsKey(RT.LINE_KEY))
		line = (Integer) RT.meta(form).valAt(RT.LINE_KEY);
	Var.pushThreadBindings(
			RT.map(LINE, line));
	try
		{
		form = macroexpand(form);
		if(form instanceof IPersistentCollection && Util.equals(RT.first(form), DO))
			{
			for(ISeq s = RT.next(form); s != null; s = RT.next(s))
				{
				compile1(gen, fn, RT.first(s));
				}
			}
		else
			{
			Expr expr = analyze(C.EVAL, form);
			fn.keywords = (IPersistentMap) KEYWORDS.deref();
			fn.vars = (IPersistentMap) VARS.deref();
			fn.constants = (PersistentVector) CONSTANTS.deref();
			expr.emit(C.EXPRESSION, fn, gen);
			expr.eval();
			}
		}
	finally
		{
		Var.popThreadBindings();
		}
}

public static Object compile(Reader rdr, String sourcePath, String sourceName) throws Exception{
	if(COMPILE_PATH.deref() == null)
		throw new Exception("*compile-path* not set");

	Object EOF = new Object();
	Object ret = null;
	LineNumberingPushbackReader pushbackReader =
			(rdr instanceof LineNumberingPushbackReader) ? (LineNumberingPushbackReader) rdr :
			new LineNumberingPushbackReader(rdr);
	Var.pushThreadBindings(
			RT.map(SOURCE_PATH, sourcePath,
			       SOURCE, sourceName,
			       RT.CURRENT_NS, RT.CURRENT_NS.deref(),
			       LINE_BEFORE, pushbackReader.getLineNumber(),
			       LINE_AFTER, pushbackReader.getLineNumber(),
			       CONSTANTS, PersistentVector.EMPTY,
			       KEYWORDS, PersistentHashMap.EMPTY,
			       VARS, PersistentHashMap.EMPTY
			));

	try
		{
		//generate loader class
		FnExpr fn = new FnExpr(null);
		fn.internalName = sourcePath.replace(File.separator, "/").substring(0, sourcePath.lastIndexOf('.'))
		                  + RT.LOADER_SUFFIX;

		fn.fntype = Type.getObjectType(fn.internalName);
		ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_MAXS);
		ClassVisitor cv = cw;
		cv.visit(V1_5, ACC_PUBLIC + ACC_SUPER, fn.internalName, null, "java/lang/Object", null);

		//static load method
		GeneratorAdapter gen = new GeneratorAdapter(ACC_PUBLIC + ACC_STATIC,
		                                            Method.getMethod("void load ()"),
		                                            null,
		                                            null,
		                                            cv);
		gen.visitCode();

		for(Object r = LispReader.read(pushbackReader, false, EOF, false); r != EOF;
		    r = LispReader.read(pushbackReader, false, EOF, false))
			{
				LINE_AFTER.set(pushbackReader.getLineNumber());
				compile1(gen, fn, r);
				LINE_BEFORE.set(pushbackReader.getLineNumber());
			}
		//end of load
		gen.returnValue();
		gen.endMethod();

		//static fields for constants
		for(int i = 0; i < fn.constants.count(); i++)
			{
			cv.visitField(ACC_PUBLIC + ACC_FINAL + ACC_STATIC, fn.constantName(i), fn.constantType(i).getDescriptor(),
			              null, null);
			}

		//static init for constants, keywords and vars
		GeneratorAdapter clinitgen = new GeneratorAdapter(ACC_PUBLIC + ACC_STATIC,
		                                                  Method.getMethod("void <clinit> ()"),
		                                                  null,
		                                                  null,
		                                                  cv);
		clinitgen.visitCode();
		Label startTry = clinitgen.newLabel();
		Label endTry = clinitgen.newLabel();
		Label end = clinitgen.newLabel();
		Label finallyLabel = clinitgen.newLabel();

		if(fn.constants.count() > 0)
			{
			fn.emitConstants(clinitgen);
			}
		clinitgen.invokeStatic(Type.getType(Compiler.class), Method.getMethod("void pushNS()"));
		clinitgen.mark(startTry);
		clinitgen.invokeStatic(fn.fntype, Method.getMethod("void load()"));
		clinitgen.mark(endTry);
		clinitgen.invokeStatic(VAR_TYPE, Method.getMethod("void popThreadBindings()"));
		clinitgen.goTo(end);

		clinitgen.mark(finallyLabel);
		//exception should be on stack
		clinitgen.invokeStatic(VAR_TYPE, Method.getMethod("void popThreadBindings()"));
		clinitgen.throwException();
		clinitgen.mark(end);
		clinitgen.visitTryCatchBlock(startTry, endTry, finallyLabel, null);

		//end of static init
		clinitgen.returnValue();
		clinitgen.endMethod();

		//end of class
		cv.visitEnd();

		writeClassFile(fn.internalName, cw.toByteArray());
		}
	catch(LispReader.ReaderException e)
		{
		throw new CompilerException(sourceName, e.line, e.getCause());
		}
	finally
		{
		Var.popThreadBindings();
		}
	return ret;
}

}
