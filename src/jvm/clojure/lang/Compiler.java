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
import clojure.asm.commons.GeneratorAdapter;
import clojure.asm.commons.Method;

import java.io.*;
import java.lang.reflect.Constructor;
import java.lang.reflect.Modifier;
import java.util.*;
import java.util.regex.Pattern;
import java.util.regex.Matcher;

//*/
/*

import org.objectweb.asm.*;
import org.objectweb.asm.commons.Method;
import org.objectweb.asm.commons.GeneratorAdapter;
import org.objectweb.asm.util.TraceClassVisitor;
import org.objectweb.asm.util.CheckClassAdapter;
//*/

public class Compiler implements Opcodes{

static final Symbol DEF = Symbol.intern("def");
static final Symbol LOOP = Symbol.intern("loop*");
static final Symbol RECUR = Symbol.intern("recur");
static final Symbol IF = Symbol.intern("if");
static final Symbol LET = Symbol.intern("let*");
static final Symbol LETFN = Symbol.intern("letfn*");
static final Symbol DO = Symbol.intern("do");
static final Symbol FN = Symbol.intern("fn*");
static final Symbol FNONCE = (Symbol) Symbol.intern("fn*").withMeta(RT.map(Keyword.intern(null, "once"), RT.T));
static final Symbol QUOTE = Symbol.intern("quote");
static final Symbol THE_VAR = Symbol.intern("var");
static final Symbol DOT = Symbol.intern(".");
static final Symbol ASSIGN = Symbol.intern("set!");
//static final Symbol TRY_FINALLY = Symbol.intern("try-finally");
static final Symbol TRY = Symbol.intern("try");
static final Symbol CATCH = Symbol.intern("catch");
static final Symbol FINALLY = Symbol.intern("finally");
static final Symbol THROW = Symbol.intern("throw");
static final Symbol MONITOR_ENTER = Symbol.intern("monitor-enter");
static final Symbol MONITOR_EXIT = Symbol.intern("monitor-exit");
static final Symbol IMPORT = Symbol.intern("clojure.core", "import*");
//static final Symbol INSTANCE = Symbol.intern("instance?");
static final Symbol DEFTYPE = Symbol.intern("deftype*");
static final Symbol CASE = Symbol.intern("case*");

//static final Symbol THISFN = Symbol.intern("thisfn");
static final Symbol CLASS = Symbol.intern("Class");
static final Symbol NEW = Symbol.intern("new");
static final Symbol THIS = Symbol.intern("this");
static final Symbol REIFY = Symbol.intern("reify*");
//static final Symbol UNQUOTE = Symbol.intern("unquote");
//static final Symbol UNQUOTE_SPLICING = Symbol.intern("unquote-splicing");
//static final Symbol SYNTAX_QUOTE = Symbol.intern("clojure.core", "syntax-quote");
static final Symbol LIST = Symbol.intern("clojure.core", "list");
static final Symbol HASHMAP = Symbol.intern("clojure.core", "hash-map");
static final Symbol VECTOR = Symbol.intern("clojure.core", "vector");
static final Symbol IDENTITY = Symbol.intern("clojure.core", "identity");

static final Symbol _AMP_ = Symbol.intern("&");
static final Symbol ISEQ = Symbol.intern("clojure.lang.ISeq");

static final Keyword inlineKey = Keyword.intern(null, "inline");
static final Keyword inlineAritiesKey = Keyword.intern(null, "inline-arities");
static final Keyword staticKey = Keyword.intern(null, "static");
static final Keyword arglistsKey = Keyword.intern(null, "arglists");
static final Symbol INVOKE_STATIC = Symbol.intern("invokeStatic");

static final Keyword volatileKey = Keyword.intern(null, "volatile");
static final Keyword implementsKey = Keyword.intern(null, "implements");
static final String COMPILE_STUB_PREFIX = "compile__stub";

static final Keyword protocolKey = Keyword.intern(null, "protocol");
static final Keyword onKey = Keyword.intern(null, "on");
static Keyword dynamicKey = Keyword.intern("dynamic");

static final Symbol NS = Symbol.intern("ns");
static final Symbol IN_NS = Symbol.intern("in-ns");

//static final Symbol IMPORT = Symbol.intern("import");
//static final Symbol USE = Symbol.intern("use");

//static final Symbol IFN = Symbol.intern("clojure.lang", "IFn");

static final public IPersistentMap specials = PersistentHashMap.create(
		DEF, new DefExpr.Parser(),
		LOOP, new LetExpr.Parser(),
		RECUR, new RecurExpr.Parser(),
		IF, new IfExpr.Parser(),
		CASE, new CaseExpr.Parser(),
		LET, new LetExpr.Parser(),
		LETFN, new LetFnExpr.Parser(),
		DO, new BodyExpr.Parser(),
		FN, null,
		QUOTE, new ConstantExpr.Parser(),
		THE_VAR, new TheVarExpr.Parser(),
		IMPORT, new ImportExpr.Parser(),
		DOT, new HostExpr.Parser(),
		ASSIGN, new AssignExpr.Parser(),
		DEFTYPE, new NewInstanceExpr.DeftypeParser(),
		REIFY, new NewInstanceExpr.ReifyParser(),
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
private static final Type AFUNCTION_TYPE = Type.getType(AFunction.class);
private static final Type RT_TYPE = Type.getType(RT.class);
private static final Type NUMBERS_TYPE = Type.getType(Numbers.class);
final static Type CLASS_TYPE = Type.getType(Class.class);
final static Type NS_TYPE = Type.getType(Namespace.class);
final static Type UTIL_TYPE = Type.getType(Util.class);
final static Type REFLECTOR_TYPE = Type.getType(Reflector.class);
final static Type THROWABLE_TYPE = Type.getType(Throwable.class);
final static Type BOOLEAN_OBJECT_TYPE = Type.getType(Boolean.class);
final static Type IPERSISTENTMAP_TYPE = Type.getType(IPersistentMap.class);
final static Type IOBJ_TYPE = Type.getType(IObj.class);

private static final Type[][] ARG_TYPES;
//private static final Type[] EXCEPTION_TYPES = {Type.getType(Exception.class)};
private static final Type[] EXCEPTION_TYPES = {};

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
static final public Var LOCAL_ENV = Var.create(null).setDynamic();

//vector<localbinding>
static final public Var LOOP_LOCALS = Var.create().setDynamic();

//Label
static final public Var LOOP_LABEL = Var.create().setDynamic();

//vector<object>
static final public Var CONSTANTS = Var.create().setDynamic();

//IdentityHashMap
static final public Var CONSTANT_IDS = Var.create().setDynamic();

//vector<keyword>
static final public Var KEYWORD_CALLSITES = Var.create().setDynamic();

//vector<var>
static final public Var PROTOCOL_CALLSITES = Var.create().setDynamic();

//set<var>
static final public Var VAR_CALLSITES = Var.create().setDynamic();

//keyword->constid
static final public Var KEYWORDS = Var.create().setDynamic();

//var->constid
static final public Var VARS = Var.create().setDynamic();

//FnFrame
static final public Var METHOD = Var.create(null).setDynamic();

//null or not
static final public Var IN_CATCH_FINALLY = Var.create(null).setDynamic();

static final public Var NO_RECUR = Var.create(null).setDynamic();

//DynamicClassLoader
static final public Var LOADER = Var.create().setDynamic();

//String
static final public Var SOURCE = Var.intern(Namespace.findOrCreate(Symbol.intern("clojure.core")),
                                            Symbol.intern("*source-path*"), "NO_SOURCE_FILE").setDynamic();

//String
static final public Var SOURCE_PATH = Var.intern(Namespace.findOrCreate(Symbol.intern("clojure.core")),
                                                 Symbol.intern("*file*"), "NO_SOURCE_PATH").setDynamic();

//String
static final public Var COMPILE_PATH = Var.intern(Namespace.findOrCreate(Symbol.intern("clojure.core")),
                                                  Symbol.intern("*compile-path*"), null).setDynamic();
//boolean
static final public Var COMPILE_FILES = Var.intern(Namespace.findOrCreate(Symbol.intern("clojure.core")),
                                                   Symbol.intern("*compile-files*"), Boolean.FALSE).setDynamic();

static final public Var INSTANCE = Var.intern(Namespace.findOrCreate(Symbol.intern("clojure.core")),
                                            Symbol.intern("instance?"));

static final public Var ADD_ANNOTATIONS = Var.intern(Namespace.findOrCreate(Symbol.intern("clojure.core")),
                                            Symbol.intern("add-annotations"));

static final public Keyword disableLocalsClearingKey = Keyword.intern("disable-locals-clearing");
static final public Keyword elideMetaKey = Keyword.intern("elide-meta");

static final public Var COMPILER_OPTIONS = Var.intern(Namespace.findOrCreate(Symbol.intern("clojure.core")),
                                                      Symbol.intern("*compiler-options*"), null).setDynamic();

static public Object getCompilerOption(Keyword k){
	return RT.get(COMPILER_OPTIONS.deref(),k);
}

static Object elideMeta(Object m){
        Collection<Object> elides = (Collection<Object>) getCompilerOption(elideMetaKey);
        if(elides != null)
            {
            for(Object k : elides)
                {
//                System.out.println("Eliding:" + k + " : " + RT.get(m, k));
                m = RT.dissoc(m, k);
                }
//            System.out.println("Remaining: " + RT.keys(m));
            }
        return m;
    }

//Integer
static final public Var LINE = Var.create(0).setDynamic();
static final public Var COLUMN = Var.create(0).setDynamic();

static int lineDeref(){
	return ((Number)LINE.deref()).intValue();
}

static int columnDeref(){
	return ((Number)COLUMN.deref()).intValue();
}

//Integer
static final public Var LINE_BEFORE = Var.create(0).setDynamic();
static final public Var COLUMN_BEFORE = Var.create(0).setDynamic();
static final public Var LINE_AFTER = Var.create(0).setDynamic();
static final public Var COLUMN_AFTER = Var.create(0).setDynamic();

//Integer
static final public Var NEXT_LOCAL_NUM = Var.create(0).setDynamic();

//Integer
static final public Var RET_LOCAL_NUM = Var.create().setDynamic();


static final public Var COMPILE_STUB_SYM = Var.create(null).setDynamic();
static final public Var COMPILE_STUB_CLASS = Var.create(null).setDynamic();


//PathNode chain
static final public Var CLEAR_PATH = Var.create(null).setDynamic();

//tail of PathNode chain
static final public Var CLEAR_ROOT = Var.create(null).setDynamic();

//LocalBinding -> Set<LocalBindingExpr>
static final public Var CLEAR_SITES = Var.create(null).setDynamic();

    public enum C{
	STATEMENT,  //value ignored
	EXPRESSION, //value required
	RETURN,      //tail position relative to enclosing recur frame
	EVAL
}

private class Recur {};
static final public Class RECUR_CLASS = Recur.class;
    
interface Expr{
	Object eval() ;

	void emit(C context, ObjExpr objx, GeneratorAdapter gen);

	boolean hasJavaClass() ;

	Class getJavaClass() ;
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
	Expr parse(C context, Object form) ;
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
		return Symbol.intern(ns.name.name, sym.name);
		}
	Object o = currentNS().getMapping(sym);
	if(o == null)
		return Symbol.intern(currentNS().name.name, sym.name);
	else if(o instanceof Class)
		return Symbol.intern(null, ((Class) o).getName());
	else if(o instanceof Var)
			{
			Var v = (Var) o;
			return Symbol.intern(v.ns.name.name, v.sym.name);
			}
	return null;

}

static class DefExpr implements Expr{
	public final Var var;
	public final Expr init;
	public final Expr meta;
	public final boolean initProvided;
	public final boolean isDynamic;
	public final String source;
	public final int line;
	public final int column;
	final static Method bindRootMethod = Method.getMethod("void bindRoot(Object)");
	final static Method setTagMethod = Method.getMethod("void setTag(clojure.lang.Symbol)");
	final static Method setMetaMethod = Method.getMethod("void setMeta(clojure.lang.IPersistentMap)");
	final static Method setDynamicMethod = Method.getMethod("clojure.lang.Var setDynamic(boolean)");
	final static Method symintern = Method.getMethod("clojure.lang.Symbol intern(String, String)");

	public DefExpr(String source, int line, int column, Var var, Expr init, Expr meta, boolean initProvided, boolean isDynamic){
		this.source = source;
		this.line = line;
		this.column = column;
		this.var = var;
		this.init = init;
		this.meta = meta;
		this.isDynamic = isDynamic;
		this.initProvided = initProvided;
	}

    private boolean includesExplicitMetadata(MapExpr expr) {
        for(int i=0; i < expr.keyvals.count(); i += 2)
            {
                Keyword k  = ((KeywordExpr) expr.keyvals.nth(i)).k;
                if ((k != RT.FILE_KEY) &&
                    (k != RT.DECLARED_KEY) &&
                    (k != RT.LINE_KEY) &&
                    (k != RT.COLUMN_KEY))
                    return true;
            }
        return false;
    }

    public Object eval() {
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
                IPersistentMap metaMap = (IPersistentMap) meta.eval();
                if (initProvided || true)//includesExplicitMetadata((MapExpr) meta))
				    var.setMeta((IPersistentMap) meta.eval());
				}
			return var.setDynamic(isDynamic);
			}
		catch(Throwable e)
			{
			if(!(e instanceof CompilerException))
				throw new CompilerException(source, line, column, e);
			else
				throw (CompilerException) e;
			}
	}

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
		objx.emitVar(gen, var);
		if(isDynamic)
			{
			gen.push(isDynamic);
			gen.invokeVirtual(VAR_TYPE, setDynamicMethod);
			}
		if(meta != null)
			{
            if (initProvided || true)//includesExplicitMetadata((MapExpr) meta))
                {
                gen.dup();
                meta.emit(C.EXPRESSION, objx, gen);
                gen.checkCast(IPERSISTENTMAP_TYPE);
                gen.invokeVirtual(VAR_TYPE, setMetaMethod);
                }
			}
		if(initProvided)
			{
			gen.dup();
			if(init instanceof FnExpr)
				{
				((FnExpr)init).emitForDefn(objx, gen);
				}
			else
				init.emit(C.EXPRESSION, objx, gen);
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
		public Expr parse(C context, Object form) {
			//(def x) or (def x initexpr) or (def x "docstring" initexpr)
			String docstring = null;
			if(RT.count(form) == 4 && (RT.third(form) instanceof String)) {
				docstring = (String) RT.third(form);
				form = RT.list(RT.first(form), RT.second(form), RT.fourth(form));
			}
			if(RT.count(form) > 3)
				throw Util.runtimeException("Too many arguments to def");
			else if(RT.count(form) < 2)
				throw Util.runtimeException("Too few arguments to def");
			else if(!(RT.second(form) instanceof Symbol))
					throw Util.runtimeException("First argument to def must be a Symbol");
			Symbol sym = (Symbol) RT.second(form);
			Var v = lookupVar(sym, true);
			if(v == null)
				throw Util.runtimeException("Can't refer to qualified var that doesn't exist");
			if(!v.ns.equals(currentNS()))
				{
				if(sym.ns == null)
					v = currentNS().intern(sym);
//					throw Util.runtimeException("Name conflict, can't def " + sym + " because namespace: " + currentNS().name +
//					                    " refers to:" + v);
				else
					throw Util.runtimeException("Can't create defs outside of current ns");
				}
			IPersistentMap mm = sym.meta();
			boolean isDynamic = RT.booleanCast(RT.get(mm,dynamicKey));
			if(isDynamic)
			   v.setDynamic();
            if(!isDynamic && sym.name.startsWith("*") && sym.name.endsWith("*") && sym.name.length() > 2)
                {
                RT.errPrintWriter().format("Warning: %1$s not declared dynamic and thus is not dynamically rebindable, "
                                          +"but its name suggests otherwise. Please either indicate ^:dynamic %1$s or change the name. (%2$s:%3$d)\n",
                                           sym, SOURCE_PATH.get(), LINE.get());
                }
			if(RT.booleanCast(RT.get(mm, arglistsKey)))
				{
				IPersistentMap vm = v.meta();
				//vm = (IPersistentMap) RT.assoc(vm,staticKey,RT.T);
				//drop quote
				vm = (IPersistentMap) RT.assoc(vm,arglistsKey,RT.second(mm.valAt(arglistsKey)));
				v.setMeta(vm);
				}
            Object source_path = SOURCE_PATH.get();
            source_path = source_path == null ? "NO_SOURCE_FILE" : source_path;
            mm = (IPersistentMap) RT.assoc(mm, RT.LINE_KEY, LINE.get()).assoc(RT.COLUMN_KEY, COLUMN.get()).assoc(RT.FILE_KEY, source_path);
			if (docstring != null)
			  mm = (IPersistentMap) RT.assoc(mm, RT.DOC_KEY, docstring);
//			mm = mm.without(RT.DOC_KEY)
//					.without(Keyword.intern(null, "arglists"))
//					.without(RT.FILE_KEY)
//					.without(RT.LINE_KEY)
//					.without(RT.COLUMN_KEY)
//					.without(Keyword.intern(null, "ns"))
//					.without(Keyword.intern(null, "name"))
//					.without(Keyword.intern(null, "added"))
//					.without(Keyword.intern(null, "static"));
            mm = (IPersistentMap) elideMeta(mm);
			Expr meta = mm.count()==0 ? null:analyze(context == C.EVAL ? context : C.EXPRESSION, mm);
			return new DefExpr((String) SOURCE.deref(), lineDeref(), columnDeref(),
			                   v, analyze(context == C.EVAL ? context : C.EXPRESSION, RT.third(form), v.sym.name),
			                   meta, RT.count(form) == 3, isDynamic);
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

	public Object eval() {
		return target.evalAssign(val);
	}

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
		target.emitAssign(context, objx, gen, val);
	}

	public boolean hasJavaClass() {
		return val.hasJavaClass();
	}

	public Class getJavaClass() {
		return val.getJavaClass();
	}

	static class Parser implements IParser{
		public Expr parse(C context, Object frm) {
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

	public Object eval() {
		return var.deref();
	}

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
		objx.emitVarValue(gen,var);
		if(context == C.STATEMENT)
			{
			gen.pop();
			}
	}

	public boolean hasJavaClass(){
		return tag != null;
	}

	public Class getJavaClass() {
		return HostExpr.tagToClass(tag);
	}

	public Object evalAssign(Expr val) {
		return var.set(val.eval());
	}

	public void emitAssign(C context, ObjExpr objx, GeneratorAdapter gen,
	                       Expr val){
		objx.emitVar(gen, var);
		val.emit(C.EXPRESSION, objx, gen);
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

	public Object eval() {
		return var;
	}

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
		objx.emitVar(gen, var);
		if(context == C.STATEMENT)
			gen.pop();
	}

	public boolean hasJavaClass(){
		return true;
	}

	public Class getJavaClass() {
		return Var.class;
	}

	static class Parser implements IParser{
		public Expr parse(C context, Object form) {
			Symbol sym = (Symbol) RT.second(form);
			Var v = lookupVar(sym, false);
			if(v != null)
				return new TheVarExpr(v);
			throw Util.runtimeException("Unable to resolve var: " + sym + " in this context");
		}
	}
}

public static class KeywordExpr extends LiteralExpr{
	public final Keyword k;

	public KeywordExpr(Keyword k){
		this.k = k;
	}

	Object val(){
		return k;
	}

	public Object eval() {
		return k;
	}

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
		objx.emitKeyword(gen, k);
		if(context == C.STATEMENT)
			gen.pop();

	}

	public boolean hasJavaClass(){
		return true;
	}

	public Class getJavaClass() {
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

	public Object eval() {
		Namespace ns = (Namespace) RT.CURRENT_NS.deref();
		ns.importClass(RT.classForName(c));
		return null;
	}

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
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

	public Class getJavaClass() {
		throw new IllegalArgumentException("ImportExpr has no Java class");
	}

	static class Parser implements IParser{
		public Expr parse(C context, Object form) {
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
	Object evalAssign(Expr val) ;

	void emitAssign(C context, ObjExpr objx, GeneratorAdapter gen, Expr val);
}

static public interface MaybePrimitiveExpr extends Expr{
	public boolean canEmitPrimitive();
	public void emitUnboxed(C context, ObjExpr objx, GeneratorAdapter gen);
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


	//*
	public static void emitBoxReturn(ObjExpr objx, GeneratorAdapter gen, Class returnType){
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
				NIL_EXPR.emit(C.EXPRESSION, objx, gen);
				}
			else if(returnType == char.class)
					{
					gen.invokeStatic(CHAR_TYPE, charValueOfMethod);
					}
				else
					{
					if(returnType == int.class)
						{
						gen.invokeStatic(INTEGER_TYPE, intValueOfMethod);
//						gen.visitInsn(I2L);
//						gen.invokeStatic(NUMBERS_TYPE, Method.getMethod("Number num(long)"));
						}
					else if(returnType == float.class)
						{
						gen.invokeStatic(FLOAT_TYPE, floatValueOfMethod);

//						gen.visitInsn(F2D);
//						gen.invokeStatic(DOUBLE_TYPE, doubleValueOfMethod);
						}
					else if(returnType == double.class)
							gen.invokeStatic(DOUBLE_TYPE, doubleValueOfMethod);
					else if(returnType == long.class)
							gen.invokeStatic(NUMBERS_TYPE, Method.getMethod("Number num(long)"));
					else if(returnType == byte.class)
							gen.invokeStatic(BYTE_TYPE, byteValueOfMethod);
					else if(returnType == short.class)
							gen.invokeStatic(SHORT_TYPE, shortValueOfMethod);
					}
			}
	}

	//*/
	public static void emitUnboxArg(ObjExpr objx, GeneratorAdapter gen, Class paramType){
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
				Method m = null;
				gen.checkCast(NUMBER_TYPE);
				if(RT.booleanCast(RT.UNCHECKED_MATH.deref()))
					{
					if(paramType == int.class)
						m = Method.getMethod("int uncheckedIntCast(Object)");
					else if(paramType == float.class)
						m = Method.getMethod("float uncheckedFloatCast(Object)");
					else if(paramType == double.class)
						m = Method.getMethod("double uncheckedDoubleCast(Object)");
					else if(paramType == long.class)
						m = Method.getMethod("long uncheckedLongCast(Object)");
					else if(paramType == byte.class)
						m = Method.getMethod("byte uncheckedByteCast(Object)");
					else if(paramType == short.class)
						m = Method.getMethod("short uncheckedShortCast(Object)");					
					}
				else
					{
					if(paramType == int.class)
						m = Method.getMethod("int intCast(Object)");
					else if(paramType == float.class)
						m = Method.getMethod("float floatCast(Object)");
					else if(paramType == double.class)
						m = Method.getMethod("double doubleCast(Object)");
					else if(paramType == long.class)
						m = Method.getMethod("long longCast(Object)");
					else if(paramType == byte.class)
						m = Method.getMethod("byte byteCast(Object)");
					else if(paramType == short.class)
						m = Method.getMethod("short shortCast(Object)");
					}
				gen.invokeStatic(RT_TYPE, m);
				}
			}
		else
			{
			gen.checkCast(Type.getType(paramType));
			}
	}

	static class Parser implements IParser{
		public Expr parse(C context, Object frm) {
			ISeq form = (ISeq) frm;
			//(. x fieldname-sym) or
			//(. x 0-ary-method)
			// (. x methodname-sym args+)
			// (. x (methodname-sym args?))
			if(RT.length(form) < 3)
				throw new IllegalArgumentException("Malformed member expression, expecting (. target member ...)");
			//determine static or instance
			//static target must be symbol, either fully.qualified.Classname or Classname that has been imported
			int line = lineDeref();
			int column = columnDeref();
			String source = (String) SOURCE.deref();
			Class c = maybeClass(RT.second(form), false);
			//at this point c will be non-null if static
			Expr instance = null;
			if(c == null)
				instance = analyze(context == C.EVAL ? context : C.EXPRESSION, RT.second(form));

			boolean maybeField = RT.length(form) == 3 && (RT.third(form) instanceof Symbol);

			if(maybeField && !(((Symbol)RT.third(form)).name.charAt(0) == '-'))
				{
				Symbol sym = (Symbol) RT.third(form);
				if(c != null)
					maybeField = Reflector.getMethods(c, 0, munge(sym.name), true).size() == 0;
				else if(instance != null && instance.hasJavaClass() && instance.getJavaClass() != null)
					maybeField = Reflector.getMethods(instance.getJavaClass(), 0, munge(sym.name), false).size() == 0;
				}

			if(maybeField)    //field
				{
				Symbol sym = (((Symbol)RT.third(form)).name.charAt(0) == '-') ?
					Symbol.intern(((Symbol)RT.third(form)).name.substring(1))
						:(Symbol) RT.third(form);
				Symbol tag = tagOf(form);
				if(c != null) {
					return new StaticFieldExpr(line, column, c, munge(sym.name), tag);
				} else
					return new InstanceFieldExpr(line, column, instance, munge(sym.name), tag, (((Symbol)RT.third(form)).name.charAt(0) == '-'));
				}
			else
				{
				ISeq call = (ISeq) ((RT.third(form) instanceof ISeq) ? RT.third(form) : RT.next(RT.next(form)));
				if(!(RT.first(call) instanceof Symbol))
					throw new IllegalArgumentException("Malformed member expression");
				Symbol sym = (Symbol) RT.first(call);
				Symbol tag = tagOf(form);
				PersistentVector args = PersistentVector.EMPTY;
				for(ISeq s = RT.next(call); s != null; s = s.next())
					args = args.cons(analyze(context == C.EVAL ? context : C.EXPRESSION, s.first()));
				if(c != null)
					return new StaticMethodExpr(source, line, column, tag, c, munge(sym.name), args);
				else
					return new InstanceMethodExpr(source, line, column, tag, instance, munge(sym.name), args);
				}
		}
	}

	private static Class maybeClass(Object form, boolean stringOk) {
		if(form instanceof Class)
			return (Class) form;
		Class c = null;
		if(form instanceof Symbol)
			{
			Symbol sym = (Symbol) form;
			if(sym.ns == null) //if ns-qualified can't be classname
				{
				if(Util.equals(sym,COMPILE_STUB_SYM.get()))
					return (Class) COMPILE_STUB_CLASS.get();
				if(sym.name.indexOf('.') > 0 || sym.name.charAt(0) == '[')
					c = RT.classForName(sym.name);
				else
					{
					Object o = currentNS().getMapping(sym);
					if(o instanceof Class)
						c = (Class) o;
					else
						{
						try{
						c = RT.classForName(sym.name);
						}
						catch(Exception e){
							// aargh
							// leave c set to null -> return null
						}
						}
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
	static Class tagToClass(Object tag) {
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
				else if(sym.name.equals("int"))
					c = Integer.TYPE;
				else if(sym.name.equals("long"))
					c = Long.TYPE;
				else if(sym.name.equals("float"))
					c = Float.TYPE;
				else if(sym.name.equals("double"))
					c = Double.TYPE;
				else if(sym.name.equals("char"))
					c = Character.TYPE;
				else if(sym.name.equals("short"))
					c = Short.TYPE;
				else if(sym.name.equals("byte"))
					c = Byte.TYPE;
				else if(sym.name.equals("boolean"))
					c = Boolean.TYPE;
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
	public final int column;
	public final Symbol tag;
	public final boolean requireField;
	final static Method invokeNoArgInstanceMember = Method.getMethod("Object invokeNoArgInstanceMember(Object,String,boolean)");
	final static Method setInstanceFieldMethod = Method.getMethod("Object setInstanceField(Object,String,Object)");


	public InstanceFieldExpr(int line, int column, Expr target, String fieldName, Symbol tag, boolean requireField) {
		this.target = target;
		this.targetClass = target.hasJavaClass() ? target.getJavaClass() : null;
		this.field = targetClass != null ? Reflector.getField(targetClass, fieldName, false) : null;
		this.fieldName = fieldName;
		this.line = line;
		this.column = column;
		this.tag = tag;
		this.requireField = requireField;
		if(field == null && RT.booleanCast(RT.WARN_ON_REFLECTION.deref()))
			{
			if(targetClass == null)
				{
				RT.errPrintWriter()
					.format("Reflection warning, %s:%d:%d - reference to field %s can't be resolved.\n",
									SOURCE_PATH.deref(), line, column, fieldName);
				}
			else
				{
				RT.errPrintWriter()
					.format("Reflection warning, %s:%d:%d - reference to field %s on %s can't be resolved.\n",
									SOURCE_PATH.deref(), line, column, fieldName, targetClass.getName());
				}
			}
	}

	public Object eval() {
		return Reflector.invokeNoArgInstanceMember(target.eval(), fieldName, requireField);
	}

	public boolean canEmitPrimitive(){
		return targetClass != null && field != null &&
		       Util.isPrimitive(field.getType());
	}

	public void emitUnboxed(C context, ObjExpr objx, GeneratorAdapter gen){
		gen.visitLineNumber(line, gen.mark());
		if(targetClass != null && field != null)
			{
			target.emit(C.EXPRESSION, objx, gen);
			gen.checkCast(getType(targetClass));
			gen.getField(getType(targetClass), fieldName, Type.getType(field.getType()));
			}
		else
			throw new UnsupportedOperationException("Unboxed emit of unknown member");
	}

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
		gen.visitLineNumber(line, gen.mark());
		if(targetClass != null && field != null)
			{
			target.emit(C.EXPRESSION, objx, gen);
			gen.checkCast(getType(targetClass));
			gen.getField(getType(targetClass), fieldName, Type.getType(field.getType()));
			//if(context != C.STATEMENT)
			HostExpr.emitBoxReturn(objx, gen, field.getType());
			if(context == C.STATEMENT)
				{
				gen.pop();
				}
			}
		else
			{
			target.emit(C.EXPRESSION, objx, gen);
			gen.push(fieldName);
			gen.push(requireField);
			gen.invokeStatic(REFLECTOR_TYPE, invokeNoArgInstanceMember);
			if(context == C.STATEMENT)
				gen.pop();
			}
	}

	public boolean hasJavaClass() {
		return field != null || tag != null;
	}

	public Class getJavaClass() {
		return tag != null ? HostExpr.tagToClass(tag) : field.getType();
	}

	public Object evalAssign(Expr val) {
		return Reflector.setInstanceField(target.eval(), fieldName, val.eval());
	}

	public void emitAssign(C context, ObjExpr objx, GeneratorAdapter gen,
	                       Expr val){
		gen.visitLineNumber(line, gen.mark());
		if(targetClass != null && field != null)
			{
			target.emit(C.EXPRESSION, objx, gen);
			gen.checkCast(Type.getType(targetClass));
			val.emit(C.EXPRESSION, objx, gen);
			gen.dupX1();
			HostExpr.emitUnboxArg(objx, gen, field.getType());
			gen.putField(Type.getType(targetClass), fieldName, Type.getType(field.getType()));
			}
		else
			{
			target.emit(C.EXPRESSION, objx, gen);
			gen.push(fieldName);
			val.emit(C.EXPRESSION, objx, gen);
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
	public final Symbol tag;
//	final static Method getStaticFieldMethod = Method.getMethod("Object getStaticField(String,String)");
//	final static Method setStaticFieldMethod = Method.getMethod("Object setStaticField(String,String,Object)");
	final int line;
	final int column;

	public StaticFieldExpr(int line, int column, Class c, String fieldName, Symbol tag) {
		//this.className = className;
		this.fieldName = fieldName;
		this.line = line;
		this.column = column;
		//c = Class.forName(className);
		this.c = c;
		try
			{
			field = c.getField(fieldName);
			}
		catch(NoSuchFieldException e)
			{
			throw Util.sneakyThrow(e);
			}
		this.tag = tag;
	}

	public Object eval() {
		return Reflector.getStaticField(c, fieldName);
	}

	public boolean canEmitPrimitive(){
		return Util.isPrimitive(field.getType());
	}

	public void emitUnboxed(C context, ObjExpr objx, GeneratorAdapter gen){
		gen.visitLineNumber(line, gen.mark());
		gen.getStatic(Type.getType(c), fieldName, Type.getType(field.getType()));
	}

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
		gen.visitLineNumber(line, gen.mark());

		gen.getStatic(Type.getType(c), fieldName, Type.getType(field.getType()));
		//if(context != C.STATEMENT)
		HostExpr.emitBoxReturn(objx, gen, field.getType());
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

	public Class getJavaClass() {
		//Class c = Class.forName(className);
		//java.lang.reflect.Field field = c.getField(fieldName);
		return tag != null ? HostExpr.tagToClass(tag) : field.getType();
	}

	public Object evalAssign(Expr val) {
		return Reflector.setStaticField(c, fieldName, val.eval());
	}

	public void emitAssign(C context, ObjExpr objx, GeneratorAdapter gen,
	                       Expr val){
		gen.visitLineNumber(line, gen.mark());
		val.emit(C.EXPRESSION, objx, gen);
		gen.dup();
		HostExpr.emitUnboxArg(objx, gen, field.getType());
		gen.putStatic(Type.getType(c), fieldName, Type.getType(field.getType()));
		if(context == C.STATEMENT)
			gen.pop();
	}


}

static Class maybePrimitiveType(Expr e){
	if(e instanceof MaybePrimitiveExpr && e.hasJavaClass() && ((MaybePrimitiveExpr)e).canEmitPrimitive())
		{
		Class c = e.getJavaClass();
		if(Util.isPrimitive(c))
			return c;
		}
	return null;
}

static Class maybeJavaClass(Collection<Expr> exprs){
    Class match = null;
    try
    {
    for (Expr e : exprs)
        {
        if (e instanceof ThrowExpr)
            continue;
        if (!e.hasJavaClass())
            return null;
        Class c = e.getJavaClass();
        if (match == null)
            match = c;
        else if (match != c)
            return null;
        }
    }
    catch(Exception e)
    {
        return null;
    }
    return match;
}


static abstract class MethodExpr extends HostExpr{
	static void emitArgsAsArray(IPersistentVector args, ObjExpr objx, GeneratorAdapter gen){
		gen.push(args.count());
		gen.newArray(OBJECT_TYPE);
		for(int i = 0; i < args.count(); i++)
			{
			gen.dup();
			gen.push(i);
			((Expr) args.nth(i)).emit(C.EXPRESSION, objx, gen);
			gen.arrayStore(OBJECT_TYPE);
			}
	}

	public static void emitTypedArgs(ObjExpr objx, GeneratorAdapter gen, Class[] parameterTypes, IPersistentVector args){
		for(int i = 0; i < parameterTypes.length; i++)
			{
			Expr e = (Expr) args.nth(i);
			try
				{
				final Class primc = maybePrimitiveType(e);
				if(primc == parameterTypes[i])
					{
					final MaybePrimitiveExpr pe = (MaybePrimitiveExpr) e;
					pe.emitUnboxed(C.EXPRESSION, objx, gen);
					}
				else if(primc == int.class && parameterTypes[i] == long.class)
					{
					final MaybePrimitiveExpr pe = (MaybePrimitiveExpr) e;
					pe.emitUnboxed(C.EXPRESSION, objx, gen);
					gen.visitInsn(I2L);
					}
				else if(primc == long.class && parameterTypes[i] == int.class)
					{
					final MaybePrimitiveExpr pe = (MaybePrimitiveExpr) e;
					pe.emitUnboxed(C.EXPRESSION, objx, gen);
					if(RT.booleanCast(RT.UNCHECKED_MATH.deref()))
						gen.invokeStatic(RT_TYPE, Method.getMethod("int uncheckedIntCast(long)"));
					else
						gen.invokeStatic(RT_TYPE, Method.getMethod("int intCast(long)"));
					}
				else if(primc == float.class && parameterTypes[i] == double.class)
					{
					final MaybePrimitiveExpr pe = (MaybePrimitiveExpr) e;
					pe.emitUnboxed(C.EXPRESSION, objx, gen);
					gen.visitInsn(F2D);
					}
				else if(primc == double.class && parameterTypes[i] == float.class)
					{
					final MaybePrimitiveExpr pe = (MaybePrimitiveExpr) e;
					pe.emitUnboxed(C.EXPRESSION, objx, gen);
					gen.visitInsn(D2F);
					}
				else
					{
					e.emit(C.EXPRESSION, objx, gen);
					HostExpr.emitUnboxArg(objx, gen, parameterTypes[i]);
					}
				}
			catch(Exception e1)
				{
				e1.printStackTrace(RT.errPrintWriter());
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
	public final int column;
	public final Symbol tag;
	public final java.lang.reflect.Method method;

	final static Method invokeInstanceMethodMethod =
			Method.getMethod("Object invokeInstanceMethod(Object,String,Object[])");


	public InstanceMethodExpr(String source, int line, int column, Symbol tag, Expr target, String methodName, IPersistentVector args)
			{
		this.source = source;
		this.line = line;
		this.column = column;
		this.args = args;
		this.methodName = methodName;
		this.target = target;
		this.tag = tag;
		if(target.hasJavaClass() && target.getJavaClass() != null)
			{
			List methods = Reflector.getMethods(target.getJavaClass(), args.count(), methodName, false);
			if(methods.isEmpty())
				{
				method = null;
				if(RT.booleanCast(RT.WARN_ON_REFLECTION.deref()))
					{
					RT.errPrintWriter()
						.format("Reflection warning, %s:%d:%d - call to method %s on %s can't be resolved (no such method).\n",
							SOURCE_PATH.deref(), line, column, methodName, target.getJavaClass().getName());
					}
				}
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
				if(method == null && RT.booleanCast(RT.WARN_ON_REFLECTION.deref()))
					{
					RT.errPrintWriter()
						.format("Reflection warning, %s:%d:%d - call to method %s on %s can't be resolved (argument types: %s).\n",
							SOURCE_PATH.deref(), line, column, methodName, target.getJavaClass().getName(), getTypeStringForArgs(args));
					}
				}
			}
		else
			{
			method = null;
			if(RT.booleanCast(RT.WARN_ON_REFLECTION.deref()))
				{
				RT.errPrintWriter()
					.format("Reflection warning, %s:%d:%d - call to method %s can't be resolved (target class is unknown).\n",
						SOURCE_PATH.deref(), line, column, methodName);
				}
			}
	}

	public Object eval() {
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
				throw new CompilerException(source, line, column, e);
			else
				throw (CompilerException) e;
			}
	}

	public boolean canEmitPrimitive(){
		return method != null && Util.isPrimitive(method.getReturnType());
	}

	public void emitUnboxed(C context, ObjExpr objx, GeneratorAdapter gen){
		gen.visitLineNumber(line, gen.mark());
		if(method != null)
			{
			Type type = Type.getType(method.getDeclaringClass());
			target.emit(C.EXPRESSION, objx, gen);
			//if(!method.getDeclaringClass().isInterface())
			gen.checkCast(type);
			MethodExpr.emitTypedArgs(objx, gen, method.getParameterTypes(), args);
			if(context == C.RETURN)
				{
				ObjMethod method = (ObjMethod) METHOD.deref();
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

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
		gen.visitLineNumber(line, gen.mark());
		if(method != null)
			{
			Type type = Type.getType(method.getDeclaringClass());
			target.emit(C.EXPRESSION, objx, gen);
			//if(!method.getDeclaringClass().isInterface())
			gen.checkCast(type);
			MethodExpr.emitTypedArgs(objx, gen, method.getParameterTypes(), args);
			if(context == C.RETURN)
				{
				ObjMethod method = (ObjMethod) METHOD.deref();
				method.emitClearLocals(gen);
				}
			Method m = new Method(methodName, Type.getReturnType(method), Type.getArgumentTypes(method));
			if(method.getDeclaringClass().isInterface())
				gen.invokeInterface(type, m);
			else
				gen.invokeVirtual(type, m);
			//if(context != C.STATEMENT || method.getReturnType() == Void.TYPE)
			HostExpr.emitBoxReturn(objx, gen, method.getReturnType());
			}
		else
			{
			target.emit(C.EXPRESSION, objx, gen);
			gen.push(methodName);
			emitArgsAsArray(args, objx, gen);
			if(context == C.RETURN)
				{
				ObjMethod method = (ObjMethod) METHOD.deref();
				method.emitClearLocals(gen);
				}
			gen.invokeStatic(REFLECTOR_TYPE, invokeInstanceMethodMethod);
			}
		if(context == C.STATEMENT)
			gen.pop();
	}

	public boolean hasJavaClass(){
		return method != null || tag != null;
	}

	public Class getJavaClass() {
		return tag != null ? HostExpr.tagToClass(tag) : method.getReturnType();
	}
}


static class StaticMethodExpr extends MethodExpr{
	//final String className;
	public final Class c;
	public final String methodName;
	public final IPersistentVector args;
	public final String source;
	public final int line;
	public final int column;
	public final java.lang.reflect.Method method;
	public final Symbol tag;
	final static Method forNameMethod = Method.getMethod("Class forName(String)");
	final static Method invokeStaticMethodMethod =
			Method.getMethod("Object invokeStaticMethod(Class,String,Object[])");


	public StaticMethodExpr(String source, int line, int column, Symbol tag, Class c, String methodName, IPersistentVector args)
			{
		this.c = c;
		this.methodName = methodName;
		this.args = args;
		this.source = source;
		this.line = line;
		this.column = column;
		this.tag = tag;

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
			RT.errPrintWriter()
				.format("Reflection warning, %s:%d:%d - call to static method %s on %s can't be resolved (argument types: %s).\n",
					SOURCE_PATH.deref(), line, column, methodName, c.getName(), getTypeStringForArgs(args));
			}
	}

	public Object eval() {
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
				throw new CompilerException(source, line, column, e);
			else
				throw (CompilerException) e;
			}
	}

	public boolean canEmitPrimitive(){
		return method != null && Util.isPrimitive(method.getReturnType());
	}

	public boolean canEmitIntrinsicPredicate(){
		return method != null && RT.get(Intrinsics.preds, method.toString()) != null;
	}

	public void emitIntrinsicPredicate(C context, ObjExpr objx, GeneratorAdapter gen, Label falseLabel){
		gen.visitLineNumber(line, gen.mark());
		if(method != null)
			{
			MethodExpr.emitTypedArgs(objx, gen, method.getParameterTypes(), args);
			if(context == C.RETURN)
				{
				ObjMethod method = (ObjMethod) METHOD.deref();
				method.emitClearLocals(gen);
				}
			Object[] predOps = (Object[]) RT.get(Intrinsics.preds, method.toString());
			for(int i=0;i<predOps.length-1;i++)
				gen.visitInsn((Integer)predOps[i]);
			gen.visitJumpInsn((Integer)predOps[predOps.length-1],falseLabel);
			}
		else
			throw new UnsupportedOperationException("Unboxed emit of unknown member");
	}

	public void emitUnboxed(C context, ObjExpr objx, GeneratorAdapter gen){
		gen.visitLineNumber(line, gen.mark());
		if(method != null)
			{
			MethodExpr.emitTypedArgs(objx, gen, method.getParameterTypes(), args);
			//Type type = Type.getObjectType(className.replace('.', '/'));
			if(context == C.RETURN)
				{
				ObjMethod method = (ObjMethod) METHOD.deref();
				method.emitClearLocals(gen);
				}
			Object ops = RT.get(Intrinsics.ops, method.toString());
			if(ops != null)
				{
				if(ops instanceof Object[])
					{
					for(Object op : (Object[])ops)
						gen.visitInsn((Integer) op);
					}
				else
					gen.visitInsn((Integer) ops);
				}
			else
				{
				Type type = Type.getType(c);
				Method m = new Method(methodName, Type.getReturnType(method), Type.getArgumentTypes(method));
				gen.invokeStatic(type, m);
				}
			}
		else
			throw new UnsupportedOperationException("Unboxed emit of unknown member");
	}

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
		gen.visitLineNumber(line, gen.mark());
		if(method != null)
			{
			MethodExpr.emitTypedArgs(objx, gen, method.getParameterTypes(), args);
			//Type type = Type.getObjectType(className.replace('.', '/'));
			if(context == C.RETURN)
				{
				ObjMethod method = (ObjMethod) METHOD.deref();
				method.emitClearLocals(gen);
				}
			Type type = Type.getType(c);
			Method m = new Method(methodName, Type.getReturnType(method), Type.getArgumentTypes(method));
			gen.invokeStatic(type, m);
			//if(context != C.STATEMENT || method.getReturnType() == Void.TYPE)
			Class retClass = method.getReturnType();
			if(context == C.STATEMENT)
				{
				if(retClass == long.class || retClass == double.class)
					gen.pop2();
				else if(retClass != void.class)
					gen.pop();
				}
			else
				{
				HostExpr.emitBoxReturn(objx, gen, method.getReturnType());
				}
			}
		else
			{
			gen.push(c.getName());
			gen.invokeStatic(CLASS_TYPE, forNameMethod);
			gen.push(methodName);
			emitArgsAsArray(args, objx, gen);
			if(context == C.RETURN)
				{
				ObjMethod method = (ObjMethod) METHOD.deref();
				method.emitClearLocals(gen);
				}
			gen.invokeStatic(REFLECTOR_TYPE, invokeStaticMethodMethod);
			if(context == C.STATEMENT)
				gen.pop();
			}
	}

	public boolean hasJavaClass(){
		return method != null || tag != null;
	}

	public Class getJavaClass() {
		return tag != null ? HostExpr.tagToClass(tag) : method.getReturnType();
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

	public Class getJavaClass() {
		throw new IllegalArgumentException(
				"UnresolvedVarExpr has no Java class");
	}

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
	}

	public Object eval() {
		throw new IllegalArgumentException(
				"UnresolvedVarExpr cannot be evalled");
	}
}

static class NumberExpr extends LiteralExpr implements MaybePrimitiveExpr{
	final Number n;
	public final int id;

	public NumberExpr(Number n){
		this.n = n;
		this.id = registerConstant(n);
	}

	Object val(){
		return n;
	}

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
		if(context != C.STATEMENT)
			{
			objx.emitConstant(gen, id);
//			emitUnboxed(context,objx,gen);
//			HostExpr.emitBoxReturn(objx,gen,getJavaClass());
			}
	}

	public boolean hasJavaClass() {
		return true;
	}

	public Class getJavaClass(){
		if(n instanceof Integer)
			return long.class;
		else if(n instanceof Double)
			return double.class;
		else if(n instanceof Long)
			return long.class;
		else
			throw new IllegalStateException("Unsupported Number type: " + n.getClass().getName());
	}

	public boolean canEmitPrimitive(){
		return true;
	}

	public void emitUnboxed(C context, ObjExpr objx, GeneratorAdapter gen){
		if(n instanceof Integer)
			gen.push(n.longValue());
		else if(n instanceof Double)
			gen.push(n.doubleValue());
		else if(n instanceof Long)
			gen.push(n.longValue());
	}

	static public Expr parse(Number form){
		if(form instanceof Integer
			|| form instanceof Double
			|| form instanceof Long)
			return new NumberExpr(form);
		else
			return new ConstantExpr(form);
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

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
		objx.emitConstant(gen, id);

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

	public Class getJavaClass() {
		return v.getClass();
		//throw new IllegalArgumentException("Has no Java class");
	}

	static class Parser implements IParser{
		public Expr parse(C context, Object form){
			Object v = RT.second(form);

			if(v == null)
				return NIL_EXPR;
			else if(v == Boolean.TRUE)
				return TRUE_EXPR;
			else if(v == Boolean.FALSE)
				return FALSE_EXPR;
			if(v instanceof Number)
				return NumberExpr.parse((Number)v);
			else if(v instanceof String)
				return new StringExpr((String) v);
			else if(v instanceof IPersistentCollection && ((IPersistentCollection) v).count() == 0)
				return new EmptyExpr(v);
			else
				return new ConstantExpr(v);
		}
	}
}

static class NilExpr extends LiteralExpr{
	Object val(){
		return null;
	}

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
		gen.visitInsn(Opcodes.ACONST_NULL);
		if(context == C.STATEMENT)
			gen.pop();
	}

	public boolean hasJavaClass(){
		return true;
	}

	public Class getJavaClass() {
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

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
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

	public Class getJavaClass() {
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

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
		if(context != C.STATEMENT)
			gen.push(str);
	}

	public boolean hasJavaClass(){
		return true;
	}

	public Class getJavaClass() {
		return String.class;
	}
}


static class MonitorEnterExpr extends UntypedExpr{
	final Expr target;

	public MonitorEnterExpr(Expr target){
		this.target = target;
	}

	public Object eval() {
		throw new UnsupportedOperationException("Can't eval monitor-enter");
	}

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
		target.emit(C.EXPRESSION, objx, gen);
		gen.monitorEnter();
		NIL_EXPR.emit(context, objx, gen);
	}

	static class Parser implements IParser{
		public Expr parse(C context, Object form) {
			return new MonitorEnterExpr(analyze(C.EXPRESSION, RT.second(form)));
		}
	}
}

static class MonitorExitExpr extends UntypedExpr{
	final Expr target;

	public MonitorExitExpr(Expr target){
		this.target = target;
	}

	public Object eval() {
		throw new UnsupportedOperationException("Can't eval monitor-exit");
	}

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
		target.emit(C.EXPRESSION, objx, gen);
		gen.monitorExit();
		NIL_EXPR.emit(context, objx, gen);
	}

	static class Parser implements IParser{
		public Expr parse(C context, Object form) {
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

	public Object eval() {
		throw new UnsupportedOperationException("Can't eval try");
	}

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
		Label startTry = gen.newLabel();
		Label endTry = gen.newLabel();
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
		tryExpr.emit(context, objx, gen);
		if(context != C.STATEMENT)
			gen.visitVarInsn(OBJECT_TYPE.getOpcode(Opcodes.ISTORE), retLocal);
		gen.mark(endTry);
		if(finallyExpr != null)
			finallyExpr.emit(C.STATEMENT, objx, gen);
		gen.goTo(ret);

		for(int i = 0; i < catchExprs.count(); i++)
			{
			CatchClause clause = (CatchClause) catchExprs.nth(i);
			gen.mark(clause.label);
			//exception should be on stack
			//put in clause local
			gen.visitVarInsn(OBJECT_TYPE.getOpcode(Opcodes.ISTORE), clause.lb.idx);
			clause.handler.emit(context, objx, gen);
			if(context != C.STATEMENT)
				gen.visitVarInsn(OBJECT_TYPE.getOpcode(Opcodes.ISTORE), retLocal);
			gen.mark(clause.endLabel);

			if(finallyExpr != null)
				finallyExpr.emit(C.STATEMENT, objx, gen);
			gen.goTo(ret);
			}
		if(finallyExpr != null)
			{
			gen.mark(finallyLabel);
			//exception should be on stack
			gen.visitVarInsn(OBJECT_TYPE.getOpcode(Opcodes.ISTORE), finallyLocal);
			finallyExpr.emit(C.STATEMENT, objx, gen);
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
			{
				gen.visitTryCatchBlock(startTry, endTry, finallyLabel, null);
				for(int i = 0; i < catchExprs.count(); i++)
					{
					CatchClause clause = (CatchClause) catchExprs.nth(i);
					gen.visitTryCatchBlock(clause.label, clause.endLabel, finallyLabel, null);
					}
			}
		for(int i = 0; i < catchExprs.count(); i++)
			{
			CatchClause clause = (CatchClause) catchExprs.nth(i);
			gen.visitLocalVariable(clause.lb.name, "Ljava/lang/Object;", null, clause.label, clause.endLabel,
			                       clause.lb.idx);
			}
	}

	public boolean hasJavaClass() {
		return tryExpr.hasJavaClass();
	}

	public Class getJavaClass() {
		return tryExpr.getJavaClass();
	}

	static class Parser implements IParser{

		public Expr parse(C context, Object frm) {
			ISeq form = (ISeq) frm;
//			if(context == C.EVAL || context == C.EXPRESSION)
			if(context != C.RETURN)
				return analyze(context, RT.list(RT.list(FNONCE, PersistentVector.EMPTY, form)));

			//(try try-expr* catch-expr* finally-expr?)
			//catch-expr: (catch class sym expr*)
			//finally-expr: (finally expr*)

			PersistentVector body = PersistentVector.EMPTY;
			PersistentVector catches = PersistentVector.EMPTY;
            Expr bodyExpr = null;
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
                                            throw Util.runtimeException("Only catch or finally clause can follow catch in try expression");
					body = body.cons(f);
					}
				else
					{
                                            if(bodyExpr == null)
                                                try {
                                                    Var.pushThreadBindings(RT.map(NO_RECUR, true));
						                            bodyExpr = (new BodyExpr.Parser()).parse(context, RT.seq(body));
                                                } finally {
                                                    Var.popThreadBindings();
                                                }
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
							throw Util.runtimeException("Can't bind qualified name:" + sym);

						IPersistentMap dynamicBindings = RT.map(LOCAL_ENV, LOCAL_ENV.deref(),
						                                        NEXT_LOCAL_NUM, NEXT_LOCAL_NUM.deref(),
						                                        IN_CATCH_FINALLY, RT.T);
						try
							{
							Var.pushThreadBindings(dynamicBindings);
							LocalBinding lb = registerLocal(sym,
							                                (Symbol) (RT.second(f) instanceof Symbol ? RT.second(f)
							                                                                         : null),
							                                null,false);
							Expr handler = (new BodyExpr.Parser()).parse(C.EXPRESSION, RT.next(RT.next(RT.next(f))));
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
							throw Util.runtimeException("finally clause must be last in try expression");
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
                        if(bodyExpr == null) {
                            try 
                                {
                                    Var.pushThreadBindings(RT.map(NO_RECUR, true));
				    bodyExpr = (new BodyExpr.Parser()).parse(C.EXPRESSION, RT.seq(body));
                                } 
                            finally
                                {
                                    Var.popThreadBindings();
                                }
                        }

			return new TryExpr(bodyExpr, catches, finallyExpr, retLocal,
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
//	public Object eval() {
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
//	public boolean hasJavaClass() {
//		return tryExpr.hasJavaClass();
//	}
//
//	public Class getJavaClass() {
//		return tryExpr.getJavaClass();
//	}
//
//	static class Parser implements IParser{
//		public Expr parse(C context, Object frm) {
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


	public Object eval() {
		throw Util.runtimeException("Can't eval throw");
	}

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
		excExpr.emit(C.EXPRESSION, objx, gen);
		gen.checkCast(THROWABLE_TYPE);
		gen.throwException();
	}

	static class Parser implements IParser{
		public Expr parse(C context, Object form) {
			if(context == C.EVAL)
				return analyze(context, RT.list(RT.list(FNONCE, PersistentVector.EMPTY, form)));
			return new ThrowExpr(analyze(C.EXPRESSION, RT.second(form)));
		}
	}
}


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

static String getTypeStringForArgs(IPersistentVector args){
	StringBuilder sb = new StringBuilder();
	for(int i = 0; i < args.count(); i++)
		{
		Expr arg = (Expr) args.nth(i);
		if (i > 0) sb.append(", ");
		sb.append(arg.hasJavaClass() ? arg.getJavaClass().getName() : "unknown");
		}
	return sb.toString();
}

static int getMatchingParams(String methodName, ArrayList<Class[]> paramlists, IPersistentVector argexprs,
                             List<Class> rets)
		{
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
            tied = false;
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


	public NewExpr(Class c, IPersistentVector args, int line, int column) {
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
			RT.errPrintWriter()
              .format("Reflection warning, %s:%d:%d - call to %s ctor can't be resolved.\n",
                      SOURCE_PATH.deref(), line, column, c.getName());
			}
	}

	public Object eval() {
		Object[] argvals = new Object[args.count()];
		for(int i = 0; i < args.count(); i++)
			argvals[i] = ((Expr) args.nth(i)).eval();
		if(this.ctor != null)
			{
			try
				{
				return ctor.newInstance(Reflector.boxArgs(ctor.getParameterTypes(), argvals));
				}
			catch(Exception e)
				{
				throw Util.sneakyThrow(e);
				}
			}
		return Reflector.invokeConstructor(c, argvals);
	}

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
		if(this.ctor != null)
			{
			Type type = getType(c);
			gen.newInstance(type);
			gen.dup();
			MethodExpr.emitTypedArgs(objx, gen, ctor.getParameterTypes(), args);
			if(context == C.RETURN)
				{
				ObjMethod method = (ObjMethod) METHOD.deref();
				method.emitClearLocals(gen);
				}
			gen.invokeConstructor(type, new Method("<init>", Type.getConstructorDescriptor(ctor)));
			}
		else
			{
			gen.push(destubClassName(c.getName()));
			gen.invokeStatic(CLASS_TYPE, forNameMethod);
			MethodExpr.emitArgsAsArray(args, objx, gen);
			if(context == C.RETURN)
				{
				ObjMethod method = (ObjMethod) METHOD.deref();
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

	public Class getJavaClass() {
		return c;
	}

	static class Parser implements IParser{
		public Expr parse(C context, Object frm) {
			int line = lineDeref();
			int column = columnDeref();
			ISeq form = (ISeq) frm;
			//(new Classname args...)
			if(form.count() < 2)
				throw Util.runtimeException("wrong number of arguments, expecting: (new Classname args...)");
			Class c = HostExpr.maybeClass(RT.second(form), false);
			if(c == null)
				throw new IllegalArgumentException("Unable to resolve classname: " + RT.second(form));
			PersistentVector args = PersistentVector.EMPTY;
			for(ISeq s = RT.next(RT.next(form)); s != null; s = s.next())
				args = args.cons(analyze(context == C.EVAL ? context : C.EXPRESSION, s.first()));
			return new NewExpr(c, args, line, column);
		}
	}

}

public static class MetaExpr implements Expr{
	public final Expr expr;
	public final Expr meta;
	final static Type IOBJ_TYPE = Type.getType(IObj.class);
	final static Method withMetaMethod = Method.getMethod("clojure.lang.IObj withMeta(clojure.lang.IPersistentMap)");


	public MetaExpr(Expr expr, Expr meta){
		this.expr = expr;
		this.meta = meta;
	}

	public Object eval() {
		return ((IObj) expr.eval()).withMeta((IPersistentMap) meta.eval());
	}

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
		expr.emit(C.EXPRESSION, objx, gen);
		gen.checkCast(IOBJ_TYPE);
		meta.emit(C.EXPRESSION, objx, gen);
		gen.checkCast(IPERSISTENTMAP_TYPE);
		gen.invokeInterface(IOBJ_TYPE, withMetaMethod);
		if(context == C.STATEMENT)
			{
			gen.pop();
			}
	}

	public boolean hasJavaClass() {
		return expr.hasJavaClass();
	}

	public Class getJavaClass() {
		return expr.getJavaClass();
	}
}

public static class IfExpr implements Expr, MaybePrimitiveExpr{
	public final Expr testExpr;
	public final Expr thenExpr;
	public final Expr elseExpr;
	public final int line;
	public final int column;


	public IfExpr(int line, int column, Expr testExpr, Expr thenExpr, Expr elseExpr){
		this.testExpr = testExpr;
		this.thenExpr = thenExpr;
		this.elseExpr = elseExpr;
		this.line = line;
		this.column = column;
	}

	public Object eval() {
		Object t = testExpr.eval();
		if(t != null && t != Boolean.FALSE)
			return thenExpr.eval();
		return elseExpr.eval();
	}

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
		doEmit(context, objx, gen,false);
	}

	public void emitUnboxed(C context, ObjExpr objx, GeneratorAdapter gen){
		doEmit(context, objx, gen, true);
	}

	public void doEmit(C context, ObjExpr objx, GeneratorAdapter gen, boolean emitUnboxed){
		Label nullLabel = gen.newLabel();
		Label falseLabel = gen.newLabel();
		Label endLabel = gen.newLabel();

		gen.visitLineNumber(line, gen.mark());

		if(testExpr instanceof StaticMethodExpr && ((StaticMethodExpr)testExpr).canEmitIntrinsicPredicate())
			{
			((StaticMethodExpr) testExpr).emitIntrinsicPredicate(C.EXPRESSION, objx, gen, falseLabel);
			}
		else if(maybePrimitiveType(testExpr) == boolean.class)
			{
			((MaybePrimitiveExpr) testExpr).emitUnboxed(C.EXPRESSION, objx, gen);
			gen.ifZCmp(gen.EQ, falseLabel);
			}
		else
			{
			testExpr.emit(C.EXPRESSION, objx, gen);
			gen.dup();
			gen.ifNull(nullLabel);
			gen.getStatic(BOOLEAN_OBJECT_TYPE, "FALSE", BOOLEAN_OBJECT_TYPE);
			gen.visitJumpInsn(IF_ACMPEQ, falseLabel);
			}
		if(emitUnboxed)
			((MaybePrimitiveExpr)thenExpr).emitUnboxed(context, objx, gen);
		else
			thenExpr.emit(context, objx, gen);
		gen.goTo(endLabel);
		gen.mark(nullLabel);
		gen.pop();
		gen.mark(falseLabel);
		if(emitUnboxed)
			((MaybePrimitiveExpr)elseExpr).emitUnboxed(context, objx, gen);
		else
			elseExpr.emit(context, objx, gen);
		gen.mark(endLabel);
	}

	public boolean hasJavaClass() {
		return thenExpr.hasJavaClass()
		       && elseExpr.hasJavaClass()
		       &&
		       (thenExpr.getJavaClass() == elseExpr.getJavaClass()
		        || thenExpr.getJavaClass() == RECUR_CLASS
				|| elseExpr.getJavaClass() == RECUR_CLASS		        
		        || (thenExpr.getJavaClass() == null && !elseExpr.getJavaClass().isPrimitive())
		        || (elseExpr.getJavaClass() == null && !thenExpr.getJavaClass().isPrimitive()));
	}

	public boolean canEmitPrimitive(){
		try
			{
			return thenExpr instanceof MaybePrimitiveExpr
			       && elseExpr instanceof MaybePrimitiveExpr
			       && (thenExpr.getJavaClass() == elseExpr.getJavaClass()
			           || thenExpr.getJavaClass() == RECUR_CLASS
			           || elseExpr.getJavaClass() == RECUR_CLASS)
			       && ((MaybePrimitiveExpr)thenExpr).canEmitPrimitive()
				   && ((MaybePrimitiveExpr)elseExpr).canEmitPrimitive();
			}
		catch(Exception e)
			{
			return false;
			}
	}

	public Class getJavaClass() {
		Class thenClass = thenExpr.getJavaClass();
		if(thenClass != null && thenClass != RECUR_CLASS)
			return thenClass;
		return elseExpr.getJavaClass();
	}

	static class Parser implements IParser{
		public Expr parse(C context, Object frm) {
			ISeq form = (ISeq) frm;
			//(if test then) or (if test then else)
			if(form.count() > 4)
				throw Util.runtimeException("Too many arguments to if");
			else if(form.count() < 3)
				throw Util.runtimeException("Too few arguments to if");
            PathNode branch = new PathNode(PATHTYPE.BRANCH, (PathNode) CLEAR_PATH.get());
            Expr testexpr = analyze(context == C.EVAL ? context : C.EXPRESSION, RT.second(form));
            Expr thenexpr, elseexpr;
            try {
                Var.pushThreadBindings(
                        RT.map(CLEAR_PATH, new PathNode(PATHTYPE.PATH,branch)));
                thenexpr = analyze(context, RT.third(form));
                }
            finally{
                Var.popThreadBindings();
                }
            try {
                Var.pushThreadBindings(
                        RT.map(CLEAR_PATH, new PathNode(PATHTYPE.PATH,branch)));
                elseexpr = analyze(context, RT.fourth(form));
                }
            finally{
                Var.popThreadBindings();
                }
			return new IfExpr(lineDeref(),
                              columnDeref(),
			                  testexpr,
			                  thenexpr,
			                  elseexpr);
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
'\'', "_SINGLEQUOTE_",
'"', "_DOUBLEQUOTE_",
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

static final public IPersistentMap DEMUNGE_MAP;
static final public Pattern DEMUNGE_PATTERN;

static {
	// DEMUNGE_MAP maps strings to characters in the opposite
	// direction that CHAR_MAP does, plus it maps "$" to '/'
	IPersistentMap m = RT.map("$", '/');
	for(ISeq s = RT.seq(CHAR_MAP); s != null; s = s.next())
		{
		IMapEntry e = (IMapEntry) s.first();
		Character origCh = (Character) e.key();
		String escapeStr = (String) e.val();
		m = m.assoc(escapeStr, origCh);
		}
	DEMUNGE_MAP = m;

	// DEMUNGE_PATTERN searches for the first of any occurrence of
	// the strings that are keys of DEMUNGE_MAP.
	// Note: Regex matching rules mean that #"_|_COLON_" "_COLON_"
       // returns "_", but #"_COLON_|_" "_COLON_" returns "_COLON_"
       // as desired.  Sorting string keys of DEMUNGE_MAP from longest to
       // shortest ensures correct matching behavior, even if some strings are
	// prefixes of others.
	Object[] mungeStrs = RT.toArray(RT.keys(m));
	Arrays.sort(mungeStrs, new Comparator() {
                public int compare(Object s1, Object s2) {
                    return ((String) s2).length() - ((String) s1).length();
                }});
	StringBuilder sb = new StringBuilder();
	boolean first = true;
	for(Object s : mungeStrs)
		{
		String escapeStr = (String) s;
		if (!first)
			sb.append("|");
		first = false;
		sb.append("\\Q");
		sb.append(escapeStr);
		sb.append("\\E");
		}
	DEMUNGE_PATTERN = Pattern.compile(sb.toString());
}

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

static public String demunge(String mungedName){
	StringBuilder sb = new StringBuilder();
	Matcher m = DEMUNGE_PATTERN.matcher(mungedName);
	int lastMatchEnd = 0;
	while (m.find())
		{
		int start = m.start();
		int end = m.end();
		// Keep everything before the match
		sb.append(mungedName.substring(lastMatchEnd, start));
		lastMatchEnd = end;
		// Replace the match with DEMUNGE_MAP result
		Character origCh = (Character) DEMUNGE_MAP.valAt(m.group());
		sb.append(origCh);
		}
	// Keep everything after the last match
	sb.append(mungedName.substring(lastMatchEnd));
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

	public Object eval() {
		return coll;
	}

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
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

	public boolean hasJavaClass() {
		return true;
	}

	public Class getJavaClass() {
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

	public Object eval() {
		IPersistentVector ret = PersistentVector.EMPTY;
		for(int i = 0; i < args.count(); i++)
			ret = (IPersistentVector) ret.cons(((Expr) args.nth(i)).eval());
		return ret.seq();
	}

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
		MethodExpr.emitArgsAsArray(args, objx, gen);
		gen.invokeStatic(RT_TYPE, arrayToListMethod);
		if(context == C.STATEMENT)
			gen.pop();
	}

	public boolean hasJavaClass() {
		return true;
	}

	public Class getJavaClass() {
		return IPersistentList.class;
	}

}

public static class MapExpr implements Expr{
	public final IPersistentVector keyvals;
	final static Method mapMethod = Method.getMethod("clojure.lang.IPersistentMap map(Object[])");
	final static Method mapUniqueKeysMethod = Method.getMethod("clojure.lang.IPersistentMap mapUniqueKeys(Object[])");


	public MapExpr(IPersistentVector keyvals){
		this.keyvals = keyvals;
	}

	public Object eval() {
		Object[] ret = new Object[keyvals.count()];
		for(int i = 0; i < keyvals.count(); i++)
			ret[i] = ((Expr) keyvals.nth(i)).eval();
		return RT.map(ret);
	}

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
		boolean allKeysConstant = true;
		boolean allConstantKeysUnique = true;
		IPersistentSet constantKeys = PersistentHashSet.EMPTY;
		for(int i = 0; i < keyvals.count(); i+=2)
			{
			Expr k = (Expr) keyvals.nth(i);
			if(k instanceof LiteralExpr)
				{
				Object kval = k.eval();
				if (constantKeys.contains(kval))
					allConstantKeysUnique = false;
				else
					constantKeys = (IPersistentSet)constantKeys.cons(kval);
				}
			else
				allKeysConstant = false;
			}
		MethodExpr.emitArgsAsArray(keyvals, objx, gen);
		if((allKeysConstant && allConstantKeysUnique) || (keyvals.count() <= 2))
			gen.invokeStatic(RT_TYPE, mapUniqueKeysMethod);
		else
			gen.invokeStatic(RT_TYPE, mapMethod);
		if(context == C.STATEMENT)
			gen.pop();
	}

	public boolean hasJavaClass() {
		return true;
	}

	public Class getJavaClass() {
		return IPersistentMap.class;
	}


	static public Expr parse(C context, IPersistentMap form) {
		IPersistentVector keyvals = PersistentVector.EMPTY;
		boolean keysConstant = true;
		boolean valsConstant = true;
		boolean allConstantKeysUnique = true;
		IPersistentSet constantKeys = PersistentHashSet.EMPTY;
		for(ISeq s = RT.seq(form); s != null; s = s.next())
			{
			IMapEntry e = (IMapEntry) s.first();
			Expr k = analyze(context == C.EVAL ? context : C.EXPRESSION, e.key());
			Expr v = analyze(context == C.EVAL ? context : C.EXPRESSION, e.val());
			keyvals = (IPersistentVector) keyvals.cons(k);
			keyvals = (IPersistentVector) keyvals.cons(v);
			if(k instanceof LiteralExpr)
				{
				Object kval = k.eval();
				if (constantKeys.contains(kval))
					allConstantKeysUnique = false;
				else
					constantKeys = (IPersistentSet)constantKeys.cons(kval);
				}
			else
				keysConstant = false;
			if(!(v instanceof LiteralExpr))
				valsConstant = false;
			}

		Expr ret = new MapExpr(keyvals);
		if(form instanceof IObj && ((IObj) form).meta() != null)
			return new MetaExpr(ret, MapExpr
					.parse(context == C.EVAL ? context : C.EXPRESSION, ((IObj) form).meta()));
		else if(keysConstant)
			{
			// TBD: Add more detail to exception thrown below.
			if(!allConstantKeysUnique)
				throw new IllegalArgumentException("Duplicate constant keys in map");
			if(valsConstant)
				{
				IPersistentMap m = PersistentHashMap.EMPTY;
				for(int i=0;i<keyvals.length();i+= 2)
					{
					m = m.assoc(((LiteralExpr)keyvals.nth(i)).val(), ((LiteralExpr)keyvals.nth(i+1)).val());
					}
//				System.err.println("Constant: " + m);
				return new ConstantExpr(m);
				}
			else
				return ret;
			}
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

	public Object eval() {
		Object[] ret = new Object[keys.count()];
		for(int i = 0; i < keys.count(); i++)
			ret[i] = ((Expr) keys.nth(i)).eval();
		return RT.set(ret);
	}

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
		MethodExpr.emitArgsAsArray(keys, objx, gen);
		gen.invokeStatic(RT_TYPE, setMethod);
		if(context == C.STATEMENT)
			gen.pop();
	}

	public boolean hasJavaClass() {
		return true;
	}

	public Class getJavaClass() {
		return IPersistentSet.class;
	}


	static public Expr parse(C context, IPersistentSet form) {
		IPersistentVector keys = PersistentVector.EMPTY;
		boolean constant = true;

		for(ISeq s = RT.seq(form); s != null; s = s.next())
			{
			Object e = s.first();
			Expr expr = analyze(context == C.EVAL ? context : C.EXPRESSION, e);
			keys = (IPersistentVector) keys.cons(expr);
			if(!(expr instanceof LiteralExpr))
				constant = false;
			}
		Expr ret = new SetExpr(keys);
		if(form instanceof IObj && ((IObj) form).meta() != null)
			return new MetaExpr(ret, MapExpr
					.parse(context == C.EVAL ? context : C.EXPRESSION, ((IObj) form).meta()));
		else if(constant)
			{
			IPersistentSet set = PersistentHashSet.EMPTY;
			for(int i=0;i<keys.count();i++)
				{
				LiteralExpr ve = (LiteralExpr)keys.nth(i);
				set = (IPersistentSet)set.cons(ve.val());
				}
//			System.err.println("Constant: " + set);
			return new ConstantExpr(set);
			}
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

	public Object eval() {
		IPersistentVector ret = PersistentVector.EMPTY;
		for(int i = 0; i < args.count(); i++)
			ret = (IPersistentVector) ret.cons(((Expr) args.nth(i)).eval());
		return ret;
	}

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
		MethodExpr.emitArgsAsArray(args, objx, gen);
		gen.invokeStatic(RT_TYPE, vectorMethod);
		if(context == C.STATEMENT)
			gen.pop();
	}

	public boolean hasJavaClass() {
		return true;
	}

	public Class getJavaClass() {
		return IPersistentVector.class;
	}

	static public Expr parse(C context, IPersistentVector form) {
		boolean constant = true;

		IPersistentVector args = PersistentVector.EMPTY;
		for(int i = 0; i < form.count(); i++)
			{
			Expr v = analyze(context == C.EVAL ? context : C.EXPRESSION, form.nth(i));
			args = (IPersistentVector) args.cons(v);
			if(!(v instanceof LiteralExpr))
				constant = false;
			}
		Expr ret = new VectorExpr(args);
		if(form instanceof IObj && ((IObj) form).meta() != null)
			return new MetaExpr(ret, MapExpr
					.parse(context == C.EVAL ? context : C.EXPRESSION, ((IObj) form).meta()));
		else if (constant)
			{
			PersistentVector rv = PersistentVector.EMPTY;
			for(int i =0;i<args.count();i++)
				{
				LiteralExpr ve = (LiteralExpr)args.nth(i);
				rv = rv.cons(ve.val());
				}
//			System.err.println("Constant: " + rv);
			return new ConstantExpr(rv);
			}
		else
			return ret;
	}

}

static class KeywordInvokeExpr implements Expr{
	public final KeywordExpr kw;
	public final Object tag;
	public final Expr target;
	public final int line;
	public final int column;
	public final int siteIndex;
	public final String source;
	static Type ILOOKUP_TYPE = Type.getType(ILookup.class);

	public KeywordInvokeExpr(String source, int line, int column, Symbol tag, KeywordExpr kw, Expr target){
		this.source = source;
		this.kw = kw;
		this.target = target;
		this.line = line;
		this.column = column;
		this.tag = tag;
		this.siteIndex = registerKeywordCallsite(kw.k);
	}

	public Object eval() {
		try
			{
			return kw.k.invoke(target.eval());
			}
		catch(Throwable e)
			{
			if(!(e instanceof CompilerException))
				throw new CompilerException(source, line, column, e);
			else
				throw (CompilerException) e;
			}
	}

    public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
        Label endLabel = gen.newLabel();
        Label faultLabel = gen.newLabel();

        gen.visitLineNumber(line, gen.mark());
        gen.getStatic(objx.objtype, objx.thunkNameStatic(siteIndex),ObjExpr.ILOOKUP_THUNK_TYPE);
        gen.dup();  //thunk, thunk
        target.emit(C.EXPRESSION, objx, gen); //thunk,thunk,target
        gen.dupX2();                          //target,thunk,thunk,target
        gen.invokeInterface(ObjExpr.ILOOKUP_THUNK_TYPE, Method.getMethod("Object get(Object)")); //target,thunk,result
        gen.dupX2();                          //result,target,thunk,result
        gen.visitJumpInsn(IF_ACMPEQ, faultLabel); //result,target
        gen.pop();                                //result
        gen.goTo(endLabel);

        gen.mark(faultLabel);    //result,target
        gen.swap();              //target,result
        gen.pop();               //target
	    gen.dup();               //target,target
        gen.getStatic(objx.objtype, objx.siteNameStatic(siteIndex),ObjExpr.KEYWORD_LOOKUPSITE_TYPE);  //target,target,site
        gen.swap();              //target,site,target
        gen.invokeInterface(ObjExpr.ILOOKUP_SITE_TYPE,
                            Method.getMethod("clojure.lang.ILookupThunk fault(Object)"));    //target,new-thunk
	    gen.dup();   //target,new-thunk,new-thunk
	    gen.putStatic(objx.objtype, objx.thunkNameStatic(siteIndex),ObjExpr.ILOOKUP_THUNK_TYPE);  //target,new-thunk
	    gen.swap();              //new-thunk,target
	    gen.invokeInterface(ObjExpr.ILOOKUP_THUNK_TYPE, Method.getMethod("Object get(Object)")); //result

        gen.mark(endLabel);
        if(context == C.STATEMENT)
            gen.pop();
    }

	public boolean hasJavaClass() {
		return tag != null;
	}

	public Class getJavaClass() {
		return HostExpr.tagToClass(tag);
	}

}
//static class KeywordSiteInvokeExpr implements Expr{
//	public final Expr site;
//	public final Object tag;
//	public final Expr target;
//	public final int line;
//	public final int column;
//	public final String source;
//
//	public KeywordSiteInvokeExpr(String source, int line, int column, Symbol tag, Expr site, Expr target){
//		this.source = source;
//		this.site = site;
//		this.target = target;
//		this.line = line;
//		this.column = column;
//		this.tag = tag;
//	}
//
//	public Object eval() {
//		try
//			{
//			KeywordCallSite s = (KeywordCallSite) site.eval();
//			return s.thunk.invoke(s,target.eval());
//			}
//		catch(Throwable e)
//			{
//			if(!(e instanceof CompilerException))
//				throw new CompilerException(source, line, column, e);
//			else
//				throw (CompilerException) e;
//			}
//	}
//
//	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
//		gen.visitLineNumber(line, gen.mark());
//		site.emit(C.EXPRESSION, objx, gen);
//		gen.dup();
//		gen.getField(Type.getType(KeywordCallSite.class),"thunk",IFN_TYPE);
//		gen.swap();
//		target.emit(C.EXPRESSION, objx, gen);
//
//		gen.invokeInterface(IFN_TYPE, new Method("invoke", OBJECT_TYPE, ARG_TYPES[2]));
//		if(context == C.STATEMENT)
//			gen.pop();
//	}
//
//	public boolean hasJavaClass() {
//		return tag != null;
//	}
//
//	public Class getJavaClass() {
//		return HostExpr.tagToClass(tag);
//	}
//
//}

public static class InstanceOfExpr implements Expr, MaybePrimitiveExpr{
	Expr expr;
	Class c;

	public InstanceOfExpr(Class c, Expr expr){
		this.expr = expr;
		this.c = c;
	}

	public Object eval() {
		if(c.isInstance(expr.eval()))
			return RT.T;
		return RT.F;
	}

	public boolean canEmitPrimitive(){
		return true;
	}

	public void emitUnboxed(C context, ObjExpr objx, GeneratorAdapter gen){
		expr.emit(C.EXPRESSION, objx, gen);
		gen.instanceOf(getType(c));
	}

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
		emitUnboxed(context,objx,gen);
		HostExpr.emitBoxReturn(objx,gen,Boolean.TYPE);
		if(context == C.STATEMENT)
			gen.pop();
	}

	public boolean hasJavaClass() {
		return true;
	}

	public Class getJavaClass() {
		return Boolean.TYPE;
	}

}

static class StaticInvokeExpr implements Expr, MaybePrimitiveExpr{
	public final Type target;
	public final Class retClass;
	public final Class[] paramclasses;
	public final Type[] paramtypes;
	public final IPersistentVector args;
	public final boolean variadic;
	public final Symbol tag;

	StaticInvokeExpr(Type target, Class retClass, Class[] paramclasses, Type[] paramtypes, boolean variadic,
	                 IPersistentVector args,Symbol tag){
		this.target = target;
		this.retClass = retClass;
		this.paramclasses = paramclasses;
		this.paramtypes = paramtypes;
		this.args = args;
		this.variadic = variadic;
		this.tag = tag;
	}

	public Object eval() {
		throw new UnsupportedOperationException("Can't eval StaticInvokeExpr");
	}

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
		emitUnboxed(context, objx, gen);
		if(context != C.STATEMENT)
			HostExpr.emitBoxReturn(objx,gen,retClass);
		if(context == C.STATEMENT)
			{
			if(retClass == long.class || retClass == double.class)
				gen.pop2();
			else
				gen.pop();
			}
	}

	public boolean hasJavaClass() {
		return true;
	}

	public Class getJavaClass() {
		return tag != null ? HostExpr.tagToClass(tag) : retClass;
	}

	public boolean canEmitPrimitive(){
		return retClass.isPrimitive();
	}

	public void emitUnboxed(C context, ObjExpr objx, GeneratorAdapter gen){
		Method ms = new Method("invokeStatic", getReturnType(), paramtypes);
		if(variadic)
			{
			for(int i = 0; i < paramclasses.length - 1; i++)
				{
				Expr e = (Expr) args.nth(i);
				if(maybePrimitiveType(e) == paramclasses[i])
					{
					((MaybePrimitiveExpr) e).emitUnboxed(C.EXPRESSION, objx, gen);
					}
				else
					{
					e.emit(C.EXPRESSION, objx, gen);
					HostExpr.emitUnboxArg(objx, gen, paramclasses[i]);
					}
				}
			IPersistentVector restArgs = RT.subvec(args,paramclasses.length - 1,args.count());
			MethodExpr.emitArgsAsArray(restArgs,objx,gen);
			gen.invokeStatic(Type.getType(ArraySeq.class), Method.getMethod("clojure.lang.ArraySeq create(Object[])"));
			}
		else
			MethodExpr.emitTypedArgs(objx, gen, paramclasses, args);

		gen.invokeStatic(target, ms);
	}

	private Type getReturnType(){
		return Type.getType(retClass);
	}

	public static Expr parse(Var v, ISeq args, Symbol tag) {
		IPersistentCollection paramlists = (IPersistentCollection) RT.get(v.meta(), arglistsKey);
		if(paramlists == null)
			throw new IllegalStateException("Can't call static fn with no arglists: " + v);
		IPersistentVector paramlist = null;
		int argcount = RT.count(args);
		boolean variadic = false;
		for(ISeq aseq = RT.seq(paramlists); aseq != null; aseq = aseq.next())
			{
			if(!(aseq.first() instanceof IPersistentVector))
				throw new IllegalStateException("Expected vector arglist, had: " + aseq.first());
			IPersistentVector alist = (IPersistentVector) aseq.first();
			if(alist.count() > 1
			   && alist.nth(alist.count() - 2).equals(_AMP_))
				{
				if(argcount >= alist.count() - 2)
					{
					paramlist = alist;
					variadic = true;
					}
				}
			else if(alist.count() == argcount)
				{
				paramlist = alist;
				variadic = false;
				break;
				}
			}

		if(paramlist == null)
			throw new IllegalArgumentException("Invalid arity - can't call: " + v + " with " + argcount + " args");

		Class retClass = tagClass(tagOf(paramlist));

		ArrayList<Class> paramClasses = new ArrayList();
		ArrayList<Type> paramTypes = new ArrayList();

		if(variadic)
			{
			for(int i = 0; i < paramlist.count()-2;i++)
				{
				Class pc = tagClass(tagOf(paramlist.nth(i)));
				paramClasses.add(pc);
				paramTypes.add(Type.getType(pc));
				}
			paramClasses.add(ISeq.class);
			paramTypes.add(Type.getType(ISeq.class));
			}
		else
			{
			for(int i = 0; i < argcount;i++)
				{
				Class pc = tagClass(tagOf(paramlist.nth(i)));
				paramClasses.add(pc);
				paramTypes.add(Type.getType(pc));
				}
			}

		String cname = v.ns.name.name.replace('.', '/').replace('-','_') + "$" + munge(v.sym.name);
		Type target = Type.getObjectType(cname);

		PersistentVector argv = PersistentVector.EMPTY;
		for(ISeq s = RT.seq(args); s != null; s = s.next())
			argv = argv.cons(analyze(C.EXPRESSION, s.first()));

		return new StaticInvokeExpr(target,retClass,paramClasses.toArray(new Class[paramClasses.size()]),
		                            paramTypes.toArray(new Type[paramTypes.size()]),variadic, argv, tag);
	}
}

static class InvokeExpr implements Expr{
	public final Expr fexpr;
	public final Object tag;
	public final IPersistentVector args;
	public final int line;
	public final int column;
	public final String source;
	public boolean isProtocol = false;
	public boolean isDirect = false;
	public int siteIndex = -1;
	public Class protocolOn;
	public java.lang.reflect.Method onMethod;
	static Keyword onKey = Keyword.intern("on");
	static Keyword methodMapKey = Keyword.intern("method-map");

	public InvokeExpr(String source, int line, int column, Symbol tag, Expr fexpr, IPersistentVector args) {
		this.source = source;
		this.fexpr = fexpr;
		this.args = args;
		this.line = line;
		this.column = column;
		if(fexpr instanceof VarExpr)
			{
			Var fvar = ((VarExpr)fexpr).var;
			Var pvar =  (Var)RT.get(fvar.meta(), protocolKey);
			if(pvar != null && PROTOCOL_CALLSITES.isBound())
				{
				this.isProtocol = true;
				this.siteIndex = registerProtocolCallsite(((VarExpr)fexpr).var);
				Object pon = RT.get(pvar.get(), onKey);
				this.protocolOn = HostExpr.maybeClass(pon,false);
				if(this.protocolOn != null)
					{
					IPersistentMap mmap = (IPersistentMap) RT.get(pvar.get(), methodMapKey);
                    Keyword mmapVal = (Keyword) mmap.valAt(Keyword.intern(fvar.sym));
                    if (mmapVal == null) {
                        throw new IllegalArgumentException(
                              "No method of interface: " + protocolOn.getName() +
                              " found for function: " + fvar.sym + " of protocol: " + pvar.sym +
                              " (The protocol method may have been defined before and removed.)");
                    }
                    String mname = munge(mmapVal.sym.toString());
 					List methods = Reflector.getMethods(protocolOn, args.count() - 1, mname, false);
					if(methods.size() != 1)
						throw new IllegalArgumentException(
								"No single method: " + mname + " of interface: " + protocolOn.getName() +
								" found for function: " + fvar.sym + " of protocol: " + pvar.sym);
					this.onMethod = (java.lang.reflect.Method) methods.get(0);
					}
				}
			}
		
		if (tag != null) {
		    this.tag = tag;
		} else if (fexpr instanceof VarExpr) {
		    Object arglists = RT.get(RT.meta(((VarExpr) fexpr).var), arglistsKey);
		    Object sigTag = null;
		    for(ISeq s = RT.seq(arglists); s != null; s = s.next()) {
                APersistentVector sig = (APersistentVector) s.first();
                int restOffset = sig.indexOf(_AMP_);
                if (args.count() == sig.count() || (restOffset > -1 && args.count() >= restOffset)) {
                    sigTag = tagOf(sig);
                    break;
                }
            }
		    
		    this.tag = sigTag == null ? ((VarExpr) fexpr).tag : sigTag;
		} else {
		    this.tag = null;
		}
	}

	public Object eval() {
		try
			{
			IFn fn = (IFn) fexpr.eval();
			PersistentVector argvs = PersistentVector.EMPTY;
			for(int i = 0; i < args.count(); i++)
				argvs = argvs.cons(((Expr) args.nth(i)).eval());
			return fn.applyTo(RT.seq( Util.ret1(argvs, argvs = null) ));
			}
		catch(Throwable e)
			{
			if(!(e instanceof CompilerException))
				throw new CompilerException(source, line, column, e);
			else
				throw (CompilerException) e;
			}
	}

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
		gen.visitLineNumber(line, gen.mark());
		if(isProtocol)
			{
			emitProto(context,objx,gen);
			}

		else
			{
			fexpr.emit(C.EXPRESSION, objx, gen);
			gen.checkCast(IFN_TYPE);
			emitArgsAndCall(0, context,objx,gen);
			}
		if(context == C.STATEMENT)
			gen.pop();		
	}

	public void emitProto(C context, ObjExpr objx, GeneratorAdapter gen){
		Label onLabel = gen.newLabel();
		Label callLabel = gen.newLabel();
		Label endLabel = gen.newLabel();

		Var v = ((VarExpr)fexpr).var;

		Expr e = (Expr) args.nth(0);
		e.emit(C.EXPRESSION, objx, gen);
		gen.dup(); //target, target
		gen.invokeStatic(UTIL_TYPE,Method.getMethod("Class classOf(Object)")); //target,class
		gen.getStatic(objx.objtype, objx.cachedClassName(siteIndex),CLASS_TYPE); //target,class,cached-class
		gen.visitJumpInsn(IF_ACMPEQ, callLabel); //target
		if(protocolOn != null)
			{
			gen.dup(); //target, target			
			gen.instanceOf(Type.getType(protocolOn));
			gen.ifZCmp(GeneratorAdapter.NE, onLabel);
			}

		gen.dup(); //target, target
		gen.invokeStatic(UTIL_TYPE,Method.getMethod("Class classOf(Object)")); //target,class
		gen.putStatic(objx.objtype, objx.cachedClassName(siteIndex),CLASS_TYPE); //target

		gen.mark(callLabel); //target
		objx.emitVar(gen, v);
		gen.invokeVirtual(VAR_TYPE, Method.getMethod("Object getRawRoot()")); //target, proto-fn
		gen.swap();
		emitArgsAndCall(1, context,objx,gen);
		gen.goTo(endLabel);

		gen.mark(onLabel); //target
		if(protocolOn != null)
			{
			MethodExpr.emitTypedArgs(objx, gen, onMethod.getParameterTypes(), RT.subvec(args,1,args.count()));
			if(context == C.RETURN)
				{
				ObjMethod method = (ObjMethod) METHOD.deref();
				method.emitClearLocals(gen);
				}
			Method m = new Method(onMethod.getName(), Type.getReturnType(onMethod), Type.getArgumentTypes(onMethod));
			gen.invokeInterface(Type.getType(protocolOn), m);
			HostExpr.emitBoxReturn(objx, gen, onMethod.getReturnType());
			}
		gen.mark(endLabel);
	}

	void emitArgsAndCall(int firstArgToEmit, C context, ObjExpr objx, GeneratorAdapter gen){
		for(int i = firstArgToEmit; i < Math.min(MAX_POSITIONAL_ARITY, args.count()); i++)
			{
			Expr e = (Expr) args.nth(i);
			e.emit(C.EXPRESSION, objx, gen);
			}
		if(args.count() > MAX_POSITIONAL_ARITY)
			{
			PersistentVector restArgs = PersistentVector.EMPTY;
			for(int i = MAX_POSITIONAL_ARITY; i < args.count(); i++)
				{
				restArgs = restArgs.cons(args.nth(i));
				}
			MethodExpr.emitArgsAsArray(restArgs, objx, gen);
			}

		if(context == C.RETURN)
			{
			ObjMethod method = (ObjMethod) METHOD.deref();
			method.emitClearLocals(gen);
			}

		gen.invokeInterface(IFN_TYPE, new Method("invoke", OBJECT_TYPE, ARG_TYPES[Math.min(MAX_POSITIONAL_ARITY + 1,
		                                                                                   args.count())]));
	}

	public boolean hasJavaClass() {
		return tag != null;
	}

	public Class getJavaClass() {
		return HostExpr.tagToClass(tag);
	}

	static public Expr parse(C context, ISeq form) {
		if(context != C.EVAL)
			context = C.EXPRESSION;
		Expr fexpr = analyze(context, form.first());
		if(fexpr instanceof VarExpr && ((VarExpr)fexpr).var.equals(INSTANCE) && RT.count(form) == 3)
			{
			Expr sexpr = analyze(C.EXPRESSION, RT.second(form));
			if(sexpr instanceof ConstantExpr)
				{
				Object val = ((ConstantExpr) sexpr).val();
				if(val instanceof Class)
					{
					return new InstanceOfExpr((Class) val, analyze(context, RT.third(form)));
					}
				}
			}

//		if(fexpr instanceof VarExpr && context != C.EVAL)
//			{
//			Var v = ((VarExpr)fexpr).var;
//			if(RT.booleanCast(RT.get(RT.meta(v),staticKey)))
//				{
//				return StaticInvokeExpr.parse(v, RT.next(form), tagOf(form));
//				}
//			}

		if(fexpr instanceof VarExpr && context != C.EVAL)
			{
			Var v = ((VarExpr)fexpr).var;
			Object arglists = RT.get(RT.meta(v), arglistsKey);
			int arity = RT.count(form.next());
			for(ISeq s = RT.seq(arglists); s != null; s = s.next())
				{
				IPersistentVector args = (IPersistentVector) s.first();
				if(args.count() == arity)
					{
					String primc = FnMethod.primInterface(args);
					if(primc != null)
						return analyze(context,
						               RT.listStar(Symbol.intern(".invokePrim"),
						                        ((Symbol) form.first()).withMeta(RT.map(RT.TAG_KEY, Symbol.intern(primc))),
						                        form.next()));
					break;
					}
				}
			}

		if(fexpr instanceof KeywordExpr && RT.count(form) == 2 && KEYWORD_CALLSITES.isBound())
			{
//			fexpr = new ConstantExpr(new KeywordCallSite(((KeywordExpr)fexpr).k));
			Expr target = analyze(context, RT.second(form));
			return new KeywordInvokeExpr((String) SOURCE.deref(), lineDeref(), columnDeref(), tagOf(form),
			                             (KeywordExpr) fexpr, target);
			}
		PersistentVector args = PersistentVector.EMPTY;
		for(ISeq s = RT.seq(form.next()); s != null; s = s.next())
			{
			args = args.cons(analyze(context, s.first()));
			}
//		if(args.count() > MAX_POSITIONAL_ARITY)
//			throw new IllegalArgumentException(
//					String.format("No more than %d args supported", MAX_POSITIONAL_ARITY));

		return new InvokeExpr((String) SOURCE.deref(), lineDeref(), columnDeref(), tagOf(form), fexpr, args);
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

static public class FnExpr extends ObjExpr{
	final static Type aFnType = Type.getType(AFunction.class);
	final static Type restFnType = Type.getType(RestFn.class);
	//if there is a variadic overload (there can only be one) it is stored here
	FnMethod variadicMethod = null;
	IPersistentCollection methods;
	private boolean hasPrimSigs;
	private boolean hasMeta;
	//	String superName = null;

	public FnExpr(Object tag){
		super(tag);
	}

	public boolean hasJavaClass() {
		return true;
	}

	boolean supportsMeta(){
		return hasMeta;
	}

	public Class getJavaClass() {
		return AFunction.class;
	}

	protected void emitMethods(ClassVisitor cv){
		//override of invoke/doInvoke for each method
		for(ISeq s = RT.seq(methods); s != null; s = s.next())
			{
			ObjMethod method = (ObjMethod) s.first();
			method.emit(this, cv);
			}

		if(isVariadic())
			{
			GeneratorAdapter gen = new GeneratorAdapter(ACC_PUBLIC,
			                                            Method.getMethod("int getRequiredArity()"),
			                                            null,
			                                            null,
			                                            cv);
			gen.visitCode();
			gen.push(variadicMethod.reqParms.count());
			gen.returnValue();
			gen.endMethod();
			}
	}

	static Expr parse(C context, ISeq form, String name) {
		ISeq origForm = form;
		FnExpr fn = new FnExpr(tagOf(form));
		fn.src = form;
		ObjMethod enclosingMethod = (ObjMethod) METHOD.deref();
		if(((IMeta) form.first()).meta() != null)
			{
			fn.onceOnly = RT.booleanCast(RT.get(RT.meta(form.first()), Keyword.intern(null, "once")));
//			fn.superName = (String) RT.get(RT.meta(form.first()), Keyword.intern(null, "super-name"));
			}
		//fn.thisName = name;
		String basename = enclosingMethod != null ?
		                  (enclosingMethod.objx.name + "$")
		                                          : //"clojure.fns." +
		                  (munge(currentNS().name.name) + "$");
		if(RT.second(form) instanceof Symbol)
			name = ((Symbol) RT.second(form)).name;
		String simpleName = name != null ?
		                    (munge(name).replace(".", "_DOT_")
		                    + (enclosingMethod != null ? "__" + RT.nextID() : ""))
		                    : ("fn"
		                      + "__" + RT.nextID());
		fn.name = basename + simpleName;
		fn.internalName = fn.name.replace('.', '/');
		fn.objtype = Type.getObjectType(fn.internalName);
		ArrayList<String> prims = new ArrayList();
		try
			{
			Var.pushThreadBindings(
					RT.mapUniqueKeys(CONSTANTS, PersistentVector.EMPTY,
					       CONSTANT_IDS, new IdentityHashMap(),
					       KEYWORDS, PersistentHashMap.EMPTY,
					       VARS, PersistentHashMap.EMPTY,
					       KEYWORD_CALLSITES, PersistentVector.EMPTY,
					       PROTOCOL_CALLSITES, PersistentVector.EMPTY,
					       VAR_CALLSITES, emptyVarCallSites(),
                                               NO_RECUR, null
					));

			//arglist might be preceded by symbol naming this fn
			if(RT.second(form) instanceof Symbol)
				{
				Symbol nm = (Symbol) RT.second(form);
				fn.thisName = nm.name;
				fn.isStatic = false; //RT.booleanCast(RT.get(nm.meta(), staticKey));
				form = RT.cons(FN, RT.next(RT.next(form)));
				}

			//now (fn [args] body...) or (fn ([args] body...) ([args2] body2...) ...)
			//turn former into latter
			if(RT.second(form) instanceof IPersistentVector)
				form = RT.list(FN, RT.next(form));
			fn.line = lineDeref();
			fn.column = columnDeref();
			FnMethod[] methodArray = new FnMethod[MAX_POSITIONAL_ARITY + 1];
			FnMethod variadicMethod = null;
			for(ISeq s = RT.next(form); s != null; s = RT.next(s))
				{
				FnMethod f = FnMethod.parse(fn, (ISeq) RT.first(s), fn.isStatic);
				if(f.isVariadic())
					{
					if(variadicMethod == null)
						variadicMethod = f;
					else
						throw Util.runtimeException("Can't have more than 1 variadic overload");
					}
				else if(methodArray[f.reqParms.count()] == null)
					methodArray[f.reqParms.count()] = f;
				else
					throw Util.runtimeException("Can't have 2 overloads with same arity");
				if(f.prim != null)
					prims.add(f.prim);
				}
			if(variadicMethod != null)
				{
				for(int i = variadicMethod.reqParms.count() + 1; i <= MAX_POSITIONAL_ARITY; i++)
					if(methodArray[i] != null)
						throw Util.runtimeException(
								"Can't have fixed arity function with more params than variadic function");
				}

			if(fn.isStatic && fn.closes.count() > 0)
				throw new IllegalArgumentException("static fns can't be closures");
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
			fn.keywordCallsites = (IPersistentVector) KEYWORD_CALLSITES.deref();
			fn.protocolCallsites = (IPersistentVector) PROTOCOL_CALLSITES.deref();
			fn.varCallsites = (IPersistentSet) VAR_CALLSITES.deref();

			fn.constantsID = RT.nextID();
//			DynamicClassLoader loader = (DynamicClassLoader) LOADER.get();
//			loader.registerConstants(fn.constantsID, fn.constants.toArray());
			}
		finally
			{
			Var.popThreadBindings();
			}
		fn.hasPrimSigs = prims.size() > 0;
		IPersistentMap fmeta = RT.meta(origForm);
		if(fmeta != null)
			fmeta = fmeta.without(RT.LINE_KEY).without(RT.COLUMN_KEY).without(RT.FILE_KEY);

		fn.hasMeta = RT.count(fmeta) > 0;

		try
			{
			fn.compile(fn.isVariadic() ? "clojure/lang/RestFn" : "clojure/lang/AFunction",
			           (prims.size() == 0)?
			            null
						:prims.toArray(new String[prims.size()]),
			            fn.onceOnly);
			}
		catch(IOException e)
			{
			throw Util.sneakyThrow(e);
			}
		fn.getCompiledClass();

		if(fn.supportsMeta())
			{
			//System.err.println(name + " supports meta");
			return new MetaExpr(fn, MapExpr
					.parse(context == C.EVAL ? context : C.EXPRESSION, fmeta));
			}
		else
			return fn;
	}

	public final ObjMethod variadicMethod(){
		return variadicMethod;
	}

	boolean isVariadic(){
		return variadicMethod != null;
	}

	public final IPersistentCollection methods(){
		return methods;
	}

	public void emitForDefn(ObjExpr objx, GeneratorAdapter gen){
//		if(!hasPrimSigs && closes.count() == 0)
//			{
//			Type thunkType = Type.getType(FnLoaderThunk.class);
////			presumes var on stack
//			gen.dup();
//			gen.newInstance(thunkType);
//			gen.dupX1();
//			gen.swap();
//			gen.push(internalName.replace('/','.'));
//			gen.invokeConstructor(thunkType,Method.getMethod("void <init>(clojure.lang.Var,String)"));
//			}
//		else
			emit(C.EXPRESSION,objx,gen);
	}
}

static public class ObjExpr implements Expr{
	static final String CONST_PREFIX = "const__";
	String name;
	//String simpleName;
	String internalName;
	String thisName;
	Type objtype;
	public final Object tag;
	//localbinding->itself
	IPersistentMap closes = PersistentHashMap.EMPTY;
    //localbndingexprs
    IPersistentVector closesExprs = PersistentVector.EMPTY;
	//symbols
	IPersistentSet volatiles = PersistentHashSet.EMPTY;

	//symbol->lb
	IPersistentMap fields = null;

	//hinted fields
	IPersistentVector hintedFields = PersistentVector.EMPTY;

	//Keyword->KeywordExpr
	IPersistentMap keywords = PersistentHashMap.EMPTY;
	IPersistentMap vars = PersistentHashMap.EMPTY;
	Class compiledClass;
	int line;
	int column;
	PersistentVector constants;
	int constantsID;
	int altCtorDrops = 0;

	IPersistentVector keywordCallsites;
	IPersistentVector protocolCallsites;
	IPersistentSet varCallsites;
	boolean onceOnly = false;

	Object src;

	final static Method voidctor = Method.getMethod("void <init>()");
	protected IPersistentMap classMeta;
	protected boolean isStatic;

	public final String name(){
		return name;
	}

//	public final String simpleName(){
//		return simpleName;
//	}

	public final String internalName(){
		return internalName;
	}

	public final String thisName(){
		return thisName;
	}

	public final Type objtype(){
		return objtype;
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

	public final int column(){
		return column;
	}

	public final PersistentVector constants(){
		return constants;
	}

	public final int constantsID(){
		return constantsID;
	}

	final static Method kwintern = Method.getMethod("clojure.lang.Keyword intern(String, String)");
	final static Method symintern = Method.getMethod("clojure.lang.Symbol intern(String)");
	final static Method varintern =
			Method.getMethod("clojure.lang.Var intern(clojure.lang.Symbol, clojure.lang.Symbol)");

	final static Type DYNAMIC_CLASSLOADER_TYPE = Type.getType(DynamicClassLoader.class);
	final static Method getClassMethod = Method.getMethod("Class getClass()");
	final static Method getClassLoaderMethod = Method.getMethod("ClassLoader getClassLoader()");
	final static Method getConstantsMethod = Method.getMethod("Object[] getConstants(int)");
	final static Method readStringMethod = Method.getMethod("Object readString(String)");

	final static Type ILOOKUP_SITE_TYPE = Type.getType(ILookupSite.class);
	final static Type ILOOKUP_THUNK_TYPE = Type.getType(ILookupThunk.class);
	final static Type KEYWORD_LOOKUPSITE_TYPE = Type.getType(KeywordLookupSite.class);

	private DynamicClassLoader loader;
	private byte[] bytecode;

	public ObjExpr(Object tag){
		this.tag = tag;
	}

	static String trimGenID(String name){
		int i = name.lastIndexOf("__");
		return i==-1?name:name.substring(0,i);
	}
	


	Type[] ctorTypes(){
		IPersistentVector tv = !supportsMeta()?PersistentVector.EMPTY:RT.vector(IPERSISTENTMAP_TYPE);
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

	void compile(String superName, String[] interfaceNames, boolean oneTimeUse) throws IOException{
		//create bytecode for a class
		//with name current_ns.defname[$letname]+
		//anonymous fns get names fn__id
		//derived from AFn/RestFn
		ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_MAXS);
//		ClassWriter cw = new ClassWriter(0);
		ClassVisitor cv = cw;
//		ClassVisitor cv = new TraceClassVisitor(new CheckClassAdapter(cw), new PrintWriter(System.out));
		//ClassVisitor cv = new TraceClassVisitor(cw, new PrintWriter(System.out));
		cv.visit(V1_5, ACC_PUBLIC + ACC_SUPER + ACC_FINAL, internalName, null,superName,interfaceNames);
//		         superName != null ? superName :
//		         (isVariadic() ? "clojure/lang/RestFn" : "clojure/lang/AFunction"), null);
		String source = (String) SOURCE.deref();
		int lineBefore = (Integer) LINE_BEFORE.deref();
		int lineAfter = (Integer) LINE_AFTER.deref() + 1;
		int columnBefore = (Integer) COLUMN_BEFORE.deref();
		int columnAfter = (Integer) COLUMN_AFTER.deref() + 1;

		if(source != null && SOURCE_PATH.deref() != null)
			{
			//cv.visitSource(source, null);
			String smap = "SMAP\n" +
			              ((source.lastIndexOf('.') > 0) ?
			               source.substring(0, source.lastIndexOf('.'))
			                :source)
			                       //                      : simpleName)
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
		addAnnotation(cv, classMeta);
		//static fields for constants
		for(int i = 0; i < constants.count(); i++)
			{
			cv.visitField(ACC_PUBLIC + ACC_FINAL
			              + ACC_STATIC, constantName(i), constantType(i).getDescriptor(),
			              null, null);
			}

		//static fields for lookup sites
		for(int i = 0; i < keywordCallsites.count(); i++)
			{
			cv.visitField(ACC_FINAL
			              + ACC_STATIC, siteNameStatic(i), KEYWORD_LOOKUPSITE_TYPE.getDescriptor(),
			              null, null);
			cv.visitField(ACC_STATIC, thunkNameStatic(i), ILOOKUP_THUNK_TYPE.getDescriptor(),
			              null, null);
			}

//		for(int i=0;i<varCallsites.count();i++)
//			{
//			cv.visitField(ACC_PRIVATE + ACC_STATIC + ACC_FINAL
//					, varCallsiteName(i), IFN_TYPE.getDescriptor(), null, null);
//			}

		//static init for constants, keywords and vars
		GeneratorAdapter clinitgen = new GeneratorAdapter(ACC_PUBLIC + ACC_STATIC,
		                                                  Method.getMethod("void <clinit> ()"),
		                                                  null,
		                                                  null,
		                                                  cv);
		clinitgen.visitCode();
		clinitgen.visitLineNumber(line, clinitgen.mark());

		if(constants.count() > 0)
			{
			emitConstants(clinitgen);
			}

		if(keywordCallsites.count() > 0)
			emitKeywordCallsites(clinitgen);

		/*
		for(int i=0;i<varCallsites.count();i++)
			{
			Label skipLabel = clinitgen.newLabel();
			Label endLabel = clinitgen.newLabel();
			Var var = (Var) varCallsites.nth(i);
			clinitgen.push(var.ns.name.toString());
			clinitgen.push(var.sym.toString());
			clinitgen.invokeStatic(RT_TYPE, Method.getMethod("clojure.lang.Var var(String,String)"));
			clinitgen.dup();
			clinitgen.invokeVirtual(VAR_TYPE,Method.getMethod("boolean hasRoot()"));
			clinitgen.ifZCmp(GeneratorAdapter.EQ,skipLabel);

			clinitgen.invokeVirtual(VAR_TYPE,Method.getMethod("Object getRoot()"));
            clinitgen.dup();
            clinitgen.instanceOf(AFUNCTION_TYPE);
            clinitgen.ifZCmp(GeneratorAdapter.EQ,skipLabel);
			clinitgen.checkCast(IFN_TYPE);
			clinitgen.putStatic(objtype, varCallsiteName(i), IFN_TYPE);
			clinitgen.goTo(endLabel);

			clinitgen.mark(skipLabel);
			clinitgen.pop();

			clinitgen.mark(endLabel);
			}
        */
		clinitgen.returnValue();

		clinitgen.endMethod();
		if(supportsMeta())
			{
			cv.visitField(ACC_FINAL, "__meta", IPERSISTENTMAP_TYPE.getDescriptor(), null, null);
			}
		//instance fields for closed-overs
		for(ISeq s = RT.keys(closes); s != null; s = s.next())
			{
			LocalBinding lb = (LocalBinding) s.first();
			if(isDeftype())
				{
				int access = isVolatile(lb) ? ACC_VOLATILE :
				             isMutable(lb) ? 0 :
				             (ACC_PUBLIC + ACC_FINAL);
				FieldVisitor fv;
				if(lb.getPrimitiveType() != null)
					fv = cv.visitField(access
							, lb.name, Type.getType(lb.getPrimitiveType()).getDescriptor(),
								  null, null);
				else
				//todo - when closed-overs are fields, use more specific types here and in ctor and emitLocal?
					fv = cv.visitField(access
							, lb.name, OBJECT_TYPE.getDescriptor(), null, null);
				addAnnotation(fv, RT.meta(lb.sym));
				}
			else
				{
				//todo - only enable this non-private+writability for letfns where we need it
				if(lb.getPrimitiveType() != null)
					cv.visitField(0 + (isVolatile(lb) ? ACC_VOLATILE : 0)
							, lb.name, Type.getType(lb.getPrimitiveType()).getDescriptor(),
								  null, null);
				else
					cv.visitField(0 //+ (oneTimeUse ? 0 : ACC_FINAL)
							, lb.name, OBJECT_TYPE.getDescriptor(), null, null);
				}
			}

		//static fields for callsites and thunks
		for(int i=0;i<protocolCallsites.count();i++)
			{
			cv.visitField(ACC_PRIVATE + ACC_STATIC, cachedClassName(i), CLASS_TYPE.getDescriptor(), null, null);
			}

 		//ctor that takes closed-overs and inits base + fields
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
//		if(superName != null)
			ctorgen.invokeConstructor(Type.getObjectType(superName), voidctor);
//		else if(isVariadic()) //RestFn ctor takes reqArity arg
//			{
//			ctorgen.push(variadicMethod.reqParms.count());
//			ctorgen.invokeConstructor(restFnType, restfnctor);
//			}
//		else
//			ctorgen.invokeConstructor(aFnType, voidctor);

//		if(vars.count() > 0)
//			{
//			ctorgen.loadThis();
//			ctorgen.getStatic(VAR_TYPE,"rev",Type.INT_TYPE);
//			ctorgen.push(-1);
//			ctorgen.visitInsn(Opcodes.IADD);
//			ctorgen.putField(objtype, "__varrev__", Type.INT_TYPE);
//			}

		if(supportsMeta())
			{
			ctorgen.loadThis();
			ctorgen.visitVarInsn(IPERSISTENTMAP_TYPE.getOpcode(Opcodes.ILOAD), 1);
			ctorgen.putField(objtype, "__meta", IPERSISTENTMAP_TYPE);
			}

		int a = supportsMeta()?2:1;
		for(ISeq s = RT.keys(closes); s != null; s = s.next(), ++a)
			{
			LocalBinding lb = (LocalBinding) s.first();
			ctorgen.loadThis();
			Class primc = lb.getPrimitiveType();
			if(primc != null)
				{
				ctorgen.visitVarInsn(Type.getType(primc).getOpcode(Opcodes.ILOAD), a);
				ctorgen.putField(objtype, lb.name, Type.getType(primc));
				if(primc == Long.TYPE || primc == Double.TYPE)
					++a;
				}
			else
				{
				ctorgen.visitVarInsn(OBJECT_TYPE.getOpcode(Opcodes.ILOAD), a);
				ctorgen.putField(objtype, lb.name, OBJECT_TYPE);
				}
            closesExprs = closesExprs.cons(new LocalBindingExpr(lb, null));
			}


		ctorgen.visitLabel(end);

		ctorgen.returnValue();

		ctorgen.endMethod();

		if(altCtorDrops > 0)
			{
					//ctor that takes closed-overs and inits base + fields
			Type[] ctorTypes = ctorTypes();
			Type[] altCtorTypes = new Type[ctorTypes.length-altCtorDrops];
			for(int i=0;i<altCtorTypes.length;i++)
				altCtorTypes[i] = ctorTypes[i];
			Method alt = new Method("<init>", Type.VOID_TYPE, altCtorTypes);
			ctorgen = new GeneratorAdapter(ACC_PUBLIC,
															alt,
															null,
															null,
															cv);
			ctorgen.visitCode();
			ctorgen.loadThis();
			ctorgen.loadArgs();
			for(int i=0;i<altCtorDrops;i++)
				ctorgen.visitInsn(Opcodes.ACONST_NULL);

			ctorgen.invokeConstructor(objtype, new Method("<init>", Type.VOID_TYPE, ctorTypes));

			ctorgen.returnValue();
			ctorgen.endMethod();
			}

		if(supportsMeta())
			{
			//ctor that takes closed-overs but not meta
			Type[] ctorTypes = ctorTypes();
			Type[] noMetaCtorTypes = new Type[ctorTypes.length-1];
			for(int i=1;i<ctorTypes.length;i++)
				noMetaCtorTypes[i-1] = ctorTypes[i];
			Method alt = new Method("<init>", Type.VOID_TYPE, noMetaCtorTypes);
			ctorgen = new GeneratorAdapter(ACC_PUBLIC,
															alt,
															null,
															null,
															cv);
			ctorgen.visitCode();
			ctorgen.loadThis();
			ctorgen.visitInsn(Opcodes.ACONST_NULL);	//null meta
			ctorgen.loadArgs();
			ctorgen.invokeConstructor(objtype, new Method("<init>", Type.VOID_TYPE, ctorTypes));

			ctorgen.returnValue();
			ctorgen.endMethod();

			//meta()
			Method meth = Method.getMethod("clojure.lang.IPersistentMap meta()");

			GeneratorAdapter gen = new GeneratorAdapter(ACC_PUBLIC,
												meth,
												null,
												null,
												cv);
			gen.visitCode();
			gen.loadThis();
			gen.getField(objtype,"__meta",IPERSISTENTMAP_TYPE);

			gen.returnValue();
			gen.endMethod();

			//withMeta()
			meth = Method.getMethod("clojure.lang.IObj withMeta(clojure.lang.IPersistentMap)");

			gen = new GeneratorAdapter(ACC_PUBLIC,
												meth,
												null,
												null,
												cv);
			gen.visitCode();
			gen.newInstance(objtype);
			gen.dup();
			gen.loadArg(0);

			for(ISeq s = RT.keys(closes); s != null; s = s.next(), ++a)
				{
				LocalBinding lb = (LocalBinding) s.first();
				gen.loadThis();
				Class primc = lb.getPrimitiveType();
				if(primc != null)
					{
					gen.getField(objtype, lb.name, Type.getType(primc));
					}
				else
					{
					gen.getField(objtype, lb.name, OBJECT_TYPE);
					}
				}

			gen.invokeConstructor(objtype, new Method("<init>", Type.VOID_TYPE, ctorTypes));
			gen.returnValue();
			gen.endMethod();
			}

		emitStatics(cv);
		emitMethods(cv);

		if(keywordCallsites.count() > 0)
			{
			Method meth = Method.getMethod("void swapThunk(int,clojure.lang.ILookupThunk)");

			GeneratorAdapter gen = new GeneratorAdapter(ACC_PUBLIC,
												meth,
												null,
												null,
												cv);
			gen.visitCode();
			Label endLabel = gen.newLabel();

			Label[] labels = new Label[keywordCallsites.count()];
			for(int i = 0; i < keywordCallsites.count();i++)
				{
				labels[i] = gen.newLabel();
				}
			gen.loadArg(0);
			gen.visitTableSwitchInsn(0,keywordCallsites.count()-1,endLabel,labels);

			for(int i = 0; i < keywordCallsites.count();i++)
				{
				gen.mark(labels[i]);
//				gen.loadThis();
				gen.loadArg(1);
				gen.putStatic(objtype, thunkNameStatic(i),ILOOKUP_THUNK_TYPE);
				gen.goTo(endLabel);
				}

			gen.mark(endLabel);

			gen.returnValue();
			gen.endMethod();
			}
		
		//end of class
		cv.visitEnd();

		bytecode = cw.toByteArray();
		if(RT.booleanCast(COMPILE_FILES.deref()))
			writeClassFile(internalName, bytecode);
//		else
//			getCompiledClass();
	}

	private void emitKeywordCallsites(GeneratorAdapter clinitgen){
		for(int i=0;i<keywordCallsites.count();i++)
			{
			Keyword k = (Keyword) keywordCallsites.nth(i);
			clinitgen.newInstance(KEYWORD_LOOKUPSITE_TYPE);
			clinitgen.dup();
			emitValue(k,clinitgen);
			clinitgen.invokeConstructor(KEYWORD_LOOKUPSITE_TYPE,
			                            Method.getMethod("void <init>(clojure.lang.Keyword)"));
			clinitgen.dup();
			clinitgen.putStatic(objtype, siteNameStatic(i), KEYWORD_LOOKUPSITE_TYPE);
			clinitgen.putStatic(objtype, thunkNameStatic(i), ILOOKUP_THUNK_TYPE);
			}
	}

	protected void emitStatics(ClassVisitor gen){
	}

	protected void emitMethods(ClassVisitor gen){
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

		if(value == null)
			gen.visitInsn(Opcodes.ACONST_NULL);
		else if(value instanceof String)
			{
			gen.push((String) value);
			}
		else if(value instanceof Boolean)
			{
			if(((Boolean) value).booleanValue())
				gen.getStatic(BOOLEAN_OBJECT_TYPE, "TRUE", BOOLEAN_OBJECT_TYPE);
			else
				gen.getStatic(BOOLEAN_OBJECT_TYPE,"FALSE",BOOLEAN_OBJECT_TYPE);
			}
		else if(value instanceof Integer)
			{
			gen.push(((Integer) value).intValue());
			gen.invokeStatic(Type.getType(Integer.class), Method.getMethod("Integer valueOf(int)"));
			}
		else if(value instanceof Long)
			{
			gen.push(((Long) value).longValue());
			gen.invokeStatic(Type.getType(Long.class), Method.getMethod("Long valueOf(long)"));
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
				else throw Util.runtimeException(
						"Can't embed unknown primitive in code: " + value);
				gen.getStatic( bt, "TYPE", Type.getType(Class.class) );
				}
			else
				{
				gen.push(destubClassName(cc.getName()));
				gen.invokeStatic(Type.getType(Class.class), Method.getMethod("Class forName(String)"));
				}
			}
		else if(value instanceof Symbol)
			{
			gen.push(((Symbol) value).ns);
			gen.push(((Symbol) value).name);
			gen.invokeStatic(Type.getType(Symbol.class),
							 Method.getMethod("clojure.lang.Symbol intern(String,String)"));
			}
		else if(value instanceof Keyword)
			{
			gen.push(((Keyword) value).sym.ns);
			gen.push(((Keyword) value).sym.name);
			gen.invokeStatic(RT_TYPE,
							 Method.getMethod("clojure.lang.Keyword keyword(String,String)"));
			}
//						else if(value instanceof KeywordCallSite)
//								{
//								emitValue(((KeywordCallSite) value).k.sym, gen);
//								gen.invokeStatic(Type.getType(KeywordCallSite.class),
//								                 Method.getMethod("clojure.lang.KeywordCallSite create(clojure.lang.Symbol)"));
//								}
		else if(value instanceof Var)
			{
			Var var = (Var) value;
			gen.push(var.ns.name.toString());
			gen.push(var.sym.toString());
			gen.invokeStatic(RT_TYPE, Method.getMethod("clojure.lang.Var var(String,String)"));
			}
		else if(value instanceof IType)
			{
			Method ctor = new Method("<init>", Type.getConstructorDescriptor(value.getClass().getConstructors()[0]));
			gen.newInstance(Type.getType(value.getClass()));
			gen.dup();
			IPersistentVector fields = (IPersistentVector) Reflector.invokeStaticMethod(value.getClass(), "getBasis", new Object[]{});
			for(ISeq s = RT.seq(fields); s != null; s = s.next())
				{
				Symbol field = (Symbol) s.first();
				Class k = tagClass(tagOf(field));
				Object val = Reflector.getInstanceField(value, field.name);
				emitValue(val, gen);

				if(k.isPrimitive())
					{
					Type b = Type.getType(boxClass(k));
					String p = Type.getType(k).getDescriptor();
					String n = k.getName();

					gen.invokeVirtual(b, new Method(n+"Value", "()"+p));
					}
				}
			gen.invokeConstructor(Type.getType(value.getClass()), ctor);
			}
		else if(value instanceof IRecord)
			{
			Method createMethod = Method.getMethod(value.getClass().getName() + " create(clojure.lang.IPersistentMap)");
            emitValue(PersistentArrayMap.create((java.util.Map) value), gen);
			gen.invokeStatic(getType(value.getClass()), createMethod);
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
		else if(value instanceof PersistentHashSet)
			{
			ISeq vs = RT.seq(value);
			if(vs == null)
				gen.getStatic(Type.getType(PersistentHashSet.class),"EMPTY",Type.getType(PersistentHashSet.class));
			else
				{
				emitListAsObjectArray(vs, gen);
				gen.invokeStatic(Type.getType(PersistentHashSet.class), Method.getMethod(
					"clojure.lang.PersistentHashSet create(Object[])"));
				}
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
		else if(value instanceof Pattern)
			{
			emitValue(value.toString(), gen);
			gen.invokeStatic(Type.getType(Pattern.class),
							 Method.getMethod("java.util.regex.Pattern compile(String)"));
			}
		else
			{
			String cs = null;
			try
				{
				cs = RT.printString(value);
//				System.out.println("WARNING SLOW CODE: " + Util.classOf(value) + " -> " + cs);
				}
			catch(Exception e)
				{
				throw Util.runtimeException(
						"Can't embed object in code, maybe print-dup not defined: " +
						value);
				}
			if(cs.length() == 0)
				throw Util.runtimeException(
						"Can't embed unreadable object in code: " + value);

			if(cs.startsWith("#<"))
				throw Util.runtimeException(
						"Can't embed unreadable object in code: " + cs);

			gen.push(cs);
			gen.invokeStatic(RT_TYPE, readStringMethod);
			partial = false;
			}

		if(partial)
			{
			if(value instanceof IObj && RT.count(((IObj) value).meta()) > 0)
				{
				gen.checkCast(IOBJ_TYPE);
                Object m = ((IObj) value).meta();
				emitValue(elideMeta(m), gen);
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
				clinitgen.putStatic(objtype, constantName(i), constantType(i));
				}
			}
		finally
			{
			Var.popThreadBindings();
			}
	}

	boolean isMutable(LocalBinding lb){
		return isVolatile(lb) ||
		       RT.booleanCast(RT.contains(fields, lb.sym)) &&
		       RT.booleanCast(RT.get(lb.sym.meta(), Keyword.intern("unsynchronized-mutable")));
	}

	boolean isVolatile(LocalBinding lb){
		return RT.booleanCast(RT.contains(fields, lb.sym)) &&
		       RT.booleanCast(RT.get(lb.sym.meta(), Keyword.intern("volatile-mutable")));
	}

	boolean isDeftype(){
		return fields != null;
	}

	boolean supportsMeta(){
		return !isDeftype();
	}
	void emitClearCloses(GeneratorAdapter gen){
//		int a = 1;
//		for(ISeq s = RT.keys(closes); s != null; s = s.next(), ++a)
//			{
//			LocalBinding lb = (LocalBinding) s.first();
//			Class primc = lb.getPrimitiveType();
//			if(primc == null)
//				{
//				gen.loadThis();
//				gen.visitInsn(Opcodes.ACONST_NULL);
//				gen.putField(objtype, lb.name, OBJECT_TYPE);
//				}
//			}
	}

	synchronized Class getCompiledClass(){
		if(compiledClass == null)
//			if(RT.booleanCast(COMPILE_FILES.deref()))
//				compiledClass = RT.classForName(name);//loader.defineClass(name, bytecode);
//			else
				{
				loader = (DynamicClassLoader) LOADER.deref();
				compiledClass = loader.defineClass(name, bytecode, src);
				}
		return compiledClass;
	}

	public Object eval() {
		if(isDeftype())
			return null;
		try
			{
			return getCompiledClass().newInstance();
			}
		catch(Exception e)
			{
			throw Util.sneakyThrow(e);
			}
	}

	public void emitLetFnInits(GeneratorAdapter gen, ObjExpr objx, IPersistentSet letFnLocals){
		//objx arg is enclosing objx, not this
		gen.checkCast(objtype);

		for(ISeq s = RT.keys(closes); s != null; s = s.next())
			{
			LocalBinding lb = (LocalBinding) s.first();
			if(letFnLocals.contains(lb))
				{
				Class primc = lb.getPrimitiveType();
				gen.dup();
				if(primc != null)
					{
					objx.emitUnboxedLocal(gen, lb);
					gen.putField(objtype, lb.name, Type.getType(primc));
					}
				else
					{
					objx.emitLocal(gen, lb, false);
					gen.putField(objtype, lb.name, OBJECT_TYPE);
					}
				}
			}
		gen.pop();

	}

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
		//emitting a Fn means constructing an instance, feeding closed-overs from enclosing scope, if any
		//objx arg is enclosing objx, not this
//		getCompiledClass();
		if(isDeftype())
			{
			gen.visitInsn(Opcodes.ACONST_NULL);
			}
		else
			{
			gen.newInstance(objtype);
			gen.dup();
			if(supportsMeta())
				gen.visitInsn(Opcodes.ACONST_NULL);
			for(ISeq s = RT.seq(closesExprs); s != null; s = s.next())
				{
                LocalBindingExpr lbe = (LocalBindingExpr) s.first();
				LocalBinding lb = lbe.b;
				if(lb.getPrimitiveType() != null)
					objx.emitUnboxedLocal(gen, lb);
				else
					objx.emitLocal(gen, lb, lbe.shouldClear);
				}
			gen.invokeConstructor(objtype, new Method("<init>", Type.VOID_TYPE, ctorTypes()));
			}
		if(context == C.STATEMENT)
			gen.pop();
	}

	public boolean hasJavaClass() {
		return true;
	}

	public Class getJavaClass() {
		return (compiledClass != null) ? compiledClass
			: (tag != null) ? HostExpr.tagToClass(tag)
			: IFn.class;
	}

	public void emitAssignLocal(GeneratorAdapter gen, LocalBinding lb,Expr val){
		if(!isMutable(lb))
			throw new IllegalArgumentException("Cannot assign to non-mutable: " + lb.name);
		Class primc = lb.getPrimitiveType();
		gen.loadThis();
		if(primc != null)
			{
			if(!(val instanceof MaybePrimitiveExpr && ((MaybePrimitiveExpr) val).canEmitPrimitive()))
				throw new IllegalArgumentException("Must assign primitive to primitive mutable: " + lb.name);
			MaybePrimitiveExpr me = (MaybePrimitiveExpr) val;
			me.emitUnboxed(C.EXPRESSION, this, gen);
			gen.putField(objtype, lb.name, Type.getType(primc));
			}
		else
			{
			val.emit(C.EXPRESSION, this, gen);
			gen.putField(objtype, lb.name, OBJECT_TYPE);
			}
	}

	private void emitLocal(GeneratorAdapter gen, LocalBinding lb, boolean clear){
		if(closes.containsKey(lb))
			{
			Class primc = lb.getPrimitiveType();
			gen.loadThis();
			if(primc != null)
				{
				gen.getField(objtype, lb.name, Type.getType(primc));
				HostExpr.emitBoxReturn(this, gen, primc);
				}
			else
				{
				gen.getField(objtype, lb.name, OBJECT_TYPE);
				if(onceOnly && clear && lb.canBeCleared)
					{
					gen.loadThis();
					gen.visitInsn(Opcodes.ACONST_NULL);
					gen.putField(objtype, lb.name, OBJECT_TYPE);
					}
				}
			}
		else
			{
			int argoff = isStatic?0:1;
			Class primc = lb.getPrimitiveType();
//            String rep = lb.sym.name + " " + lb.toString().substring(lb.toString().lastIndexOf('@'));
			if(lb.isArg)
				{
				gen.loadArg(lb.idx-argoff);
				if(primc != null)
					HostExpr.emitBoxReturn(this, gen, primc);
                else
                    {
                    if(clear && lb.canBeCleared)
                        {
//                        System.out.println("clear: " + rep);
                        gen.visitInsn(Opcodes.ACONST_NULL);
                        gen.storeArg(lb.idx - argoff);
                        }
                    else
                        {
//                        System.out.println("use: " + rep);
                        }
                    }     
				}
			else
				{
				if(primc != null)
					{
					gen.visitVarInsn(Type.getType(primc).getOpcode(Opcodes.ILOAD), lb.idx);
					HostExpr.emitBoxReturn(this, gen, primc);
					}
				else
                    {
					gen.visitVarInsn(OBJECT_TYPE.getOpcode(Opcodes.ILOAD), lb.idx);
                    if(clear && lb.canBeCleared)
                        {
//                        System.out.println("clear: " + rep);
                        gen.visitInsn(Opcodes.ACONST_NULL);
                        gen.visitVarInsn(OBJECT_TYPE.getOpcode(Opcodes.ISTORE), lb.idx);
                        }
                    else
                        {
//                        System.out.println("use: " + rep);
                        }
                    }
				}
			}
	}

	private void emitUnboxedLocal(GeneratorAdapter gen, LocalBinding lb){
		int argoff = isStatic?0:1;
		Class primc = lb.getPrimitiveType();
		if(closes.containsKey(lb))
			{
			gen.loadThis();
			gen.getField(objtype, lb.name, Type.getType(primc));
			}
		else if(lb.isArg)
			gen.loadArg(lb.idx-argoff);
		else
			gen.visitVarInsn(Type.getType(primc).getOpcode(Opcodes.ILOAD), lb.idx);
	}

	public void emitVar(GeneratorAdapter gen, Var var){
		Integer i = (Integer) vars.valAt(var);
		emitConstant(gen, i);
		//gen.getStatic(fntype, munge(var.sym.toString()), VAR_TYPE);
	}

	final static Method varGetMethod = Method.getMethod("Object get()");
	final static Method varGetRawMethod = Method.getMethod("Object getRawRoot()");

	public void emitVarValue(GeneratorAdapter gen, Var v){
		Integer i = (Integer) vars.valAt(v);
		if(!v.isDynamic())
			{
			emitConstant(gen, i);
			gen.invokeVirtual(VAR_TYPE, varGetRawMethod);
			}
		else
			{
			emitConstant(gen, i);
			gen.invokeVirtual(VAR_TYPE, varGetMethod);
			}
	}

	public void emitKeyword(GeneratorAdapter gen, Keyword k){
		Integer i = (Integer) keywords.valAt(k);
		emitConstant(gen, i);
//		gen.getStatic(fntype, munge(k.sym.toString()), KEYWORD_TYPE);
	}

	public void emitConstant(GeneratorAdapter gen, int id){
		gen.getStatic(objtype, constantName(id), constantType(id));
	}


	String constantName(int id){
		return CONST_PREFIX + id;
	}

	String siteName(int n){
		return "__site__" + n;
	}

	String siteNameStatic(int n){
		return siteName(n) + "__";
	}

	String thunkName(int n){
		return "__thunk__" + n;
	}

	String cachedClassName(int n){
		return "__cached_class__" + n;
	}

	String cachedVarName(int n){
		return "__cached_var__" + n;
	}

	String varCallsiteName(int n){
		return "__var__callsite__" + n;
	}

	String thunkNameStatic(int n){
		return thunkName(n) + "__";
	}

	Type constantType(int id){
		Object o = constants.nth(id);
		Class c = clojure.lang.Util.classOf(o);
		if(c!= null && Modifier.isPublic(c.getModifiers()))
			{
			//can't emit derived fn types due to visibility
			if(LazySeq.class.isAssignableFrom(c))
				return Type.getType(ISeq.class);
			else if(c == Keyword.class)
				return Type.getType(Keyword.class);
//			else if(c == KeywordCallSite.class)
//				return Type.getType(KeywordCallSite.class);
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

enum PATHTYPE {
    PATH, BRANCH;
}

static class PathNode{
    final PATHTYPE type;
    final PathNode parent;

    PathNode(PATHTYPE type, PathNode parent) {
        this.type = type;
        this.parent = parent;
    }
}

static PathNode clearPathRoot(){
    return (PathNode) CLEAR_ROOT.get();
}
    
enum PSTATE{
	REQ, REST, DONE
}

public static class FnMethod extends ObjMethod{
	//localbinding->localbinding
	PersistentVector reqParms = PersistentVector.EMPTY;
	LocalBinding restParm = null;
	Type[] argtypes;
	Class[] argclasses;
	Class retClass;
	String prim ;

	public FnMethod(ObjExpr objx, ObjMethod parent){
		super(objx, parent);
	}

	static public char classChar(Object x){
		Class c = null;
		if(x instanceof Class)
			c = (Class) x;
		else if(x instanceof Symbol)
			c = primClass((Symbol) x);
		if(c == null || !c.isPrimitive())
			return 'O';
		if(c == long.class)
			return 'L';
		if(c == double.class)
			return 'D';
		throw new IllegalArgumentException("Only long and double primitives are supported");
	}

	static public String primInterface(IPersistentVector arglist) {
		StringBuilder sb = new StringBuilder();
		for(int i=0;i<arglist.count();i++)
			sb.append(classChar(tagOf(arglist.nth(i))));
		sb.append(classChar(tagOf(arglist)));
		String ret = sb.toString();
		boolean prim = ret.contains("L") || ret.contains("D");
		if(prim && arglist.count() > 4)
			throw new IllegalArgumentException("fns taking primitives support only 4 or fewer args");
		if(prim)
			return "clojure.lang.IFn$" + ret;
		return null;
	}

	static FnMethod parse(ObjExpr objx, ISeq form, boolean isStatic) {
		//([args] body...)
		IPersistentVector parms = (IPersistentVector) RT.first(form);
		ISeq body = RT.next(form);
		try
			{
			FnMethod method = new FnMethod(objx, (ObjMethod) METHOD.deref());
			method.line = lineDeref();
			method.column = columnDeref();
			//register as the current method and set up a new env frame
            PathNode pnode =  (PathNode) CLEAR_PATH.get();
			if(pnode == null)
				pnode = new PathNode(PATHTYPE.PATH,null);
			Var.pushThreadBindings(
					RT.mapUniqueKeys(
							METHOD, method,
							LOCAL_ENV, LOCAL_ENV.deref(),
							LOOP_LOCALS, null,
							NEXT_LOCAL_NUM, 0
                            ,CLEAR_PATH, pnode
                            ,CLEAR_ROOT, pnode
                            ,CLEAR_SITES, PersistentHashMap.EMPTY
                        ));

			method.prim = primInterface(parms);
			if(method.prim != null)
				method.prim = method.prim.replace('.', '/');

			method.retClass = tagClass(tagOf(parms));
			if(method.retClass.isPrimitive() && !(method.retClass == double.class || method.retClass == long.class))
				throw new IllegalArgumentException("Only long and double primitives are supported");

			//register 'this' as local 0
			//registerLocal(THISFN, null, null);
			if(!isStatic)
				{
				if(objx.thisName != null)
					registerLocal(Symbol.intern(objx.thisName), null, null,false);
				else
					getAndIncLocalNum();
				}
			PSTATE state = PSTATE.REQ;
			PersistentVector argLocals = PersistentVector.EMPTY;
			ArrayList<Type> argtypes = new ArrayList();
			ArrayList<Class> argclasses = new ArrayList();
			for(int i = 0; i < parms.count(); i++)
				{
				if(!(parms.nth(i) instanceof Symbol))
					throw new IllegalArgumentException("fn params must be Symbols");
				Symbol p = (Symbol) parms.nth(i);
				if(p.getNamespace() != null)
					throw Util.runtimeException("Can't use qualified name as parameter: " + p);
				if(p.equals(_AMP_))
					{
//					if(isStatic)
//						throw Util.runtimeException("Variadic fns cannot be static");
					if(state == PSTATE.REQ)
						state = PSTATE.REST;
					else
						throw Util.runtimeException("Invalid parameter list");
					}

				else
					{
					Class pc = primClass(tagClass(tagOf(p)));
//					if(pc.isPrimitive() && !isStatic)
//						{
//						pc = Object.class;
//						p = (Symbol) ((IObj) p).withMeta((IPersistentMap) RT.assoc(RT.meta(p), RT.TAG_KEY, null));
//						}
//						throw Util.runtimeException("Non-static fn can't have primitive parameter: " + p);
					if(pc.isPrimitive() && !(pc == double.class || pc == long.class))
						throw new IllegalArgumentException("Only long and double primitives are supported: " + p);

					if(state == PSTATE.REST && tagOf(p) != null)
						throw Util.runtimeException("& arg cannot have type hint");
					if(state == PSTATE.REST && method.prim != null)
						throw Util.runtimeException("fns taking primitives cannot be variadic");
					                        
					if(state == PSTATE.REST)
						pc = ISeq.class;
					argtypes.add(Type.getType(pc));
					argclasses.add(pc);
					LocalBinding lb = pc.isPrimitive() ?
					                  registerLocal(p, null, new MethodParamExpr(pc), true)
					                           : registerLocal(p, state == PSTATE.REST ? ISEQ : tagOf(p), null, true);
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
							throw Util.runtimeException("Unexpected parameter");
						}
					}
				}
			if(method.reqParms.count() > MAX_POSITIONAL_ARITY)
				throw Util.runtimeException("Can't specify more than " + MAX_POSITIONAL_ARITY + " params");
			LOOP_LOCALS.set(argLocals);
			method.argLocals = argLocals;
//			if(isStatic)
			if(method.prim != null)
				{
				method.argtypes = argtypes.toArray(new Type[argtypes.size()]);
				method.argclasses = argclasses.toArray(new Class[argtypes.size()]);
				for(int i = 0; i < method.argclasses.length; i++)
					{
					if(method.argclasses[i] == long.class || method.argclasses[i] == double.class)
						getAndIncLocalNum();
					}
				}
			method.body = (new BodyExpr.Parser()).parse(C.RETURN, body);
			return method;
			}
		finally
			{
			Var.popThreadBindings();
			}
	}

	public void emit(ObjExpr fn, ClassVisitor cv){
		if(prim != null)
			doEmitPrim(fn, cv);
		else if(fn.isStatic)
			doEmitStatic(fn,cv);
		else
			doEmit(fn,cv);
	}

	public void doEmitStatic(ObjExpr fn, ClassVisitor cv){
		Method ms = new Method("invokeStatic", getReturnType(), argtypes);

		GeneratorAdapter gen = new GeneratorAdapter(ACC_PUBLIC + ACC_STATIC,
		                                            ms,
		                                            null,
		                                            //todo don't hardwire this
		                                            EXCEPTION_TYPES,
		                                            cv);
		gen.visitCode();
		Label loopLabel = gen.mark();
		gen.visitLineNumber(line, loopLabel);
		try
			{
			Var.pushThreadBindings(RT.map(LOOP_LABEL, loopLabel, METHOD, this));
			emitBody(objx, gen, retClass, body);

			Label end = gen.mark();
			for(ISeq lbs = argLocals.seq(); lbs != null; lbs = lbs.next())
				{
				LocalBinding lb = (LocalBinding) lbs.first();
				gen.visitLocalVariable(lb.name, argtypes[lb.idx].getDescriptor(), null, loopLabel, end, lb.idx);
				}
			}
		finally
			{
			Var.popThreadBindings();
			}

		gen.returnValue();
		//gen.visitMaxs(1, 1);
		gen.endMethod();

	//generate the regular invoke, calling the static method
		Method m = new Method(getMethodName(), OBJECT_TYPE, getArgTypes());

		gen = new GeneratorAdapter(ACC_PUBLIC,
		                           m,
		                           null,
		                           //todo don't hardwire this
		                           EXCEPTION_TYPES,
		                           cv);
		gen.visitCode();
		for(int i = 0; i < argtypes.length; i++)
			{
			gen.loadArg(i);
			HostExpr.emitUnboxArg(fn, gen, argclasses[i]);
			}
		gen.invokeStatic(objx.objtype, ms);
		gen.box(getReturnType());


		gen.returnValue();
		//gen.visitMaxs(1, 1);
		gen.endMethod();

	}

	public void doEmitPrim(ObjExpr fn, ClassVisitor cv){
		Type returnType;
		if (retClass == double.class || retClass == long.class)
			returnType = getReturnType();
		else returnType = OBJECT_TYPE;
		Method ms = new Method("invokePrim", returnType, argtypes);

		GeneratorAdapter gen = new GeneratorAdapter(ACC_PUBLIC + ACC_FINAL,
		                                            ms,
		                                            null,
		                                            //todo don't hardwire this
		                                            EXCEPTION_TYPES,
		                                            cv);
		gen.visitCode();

		Label loopLabel = gen.mark();
		gen.visitLineNumber(line, loopLabel);
		try
			{
			Var.pushThreadBindings(RT.map(LOOP_LABEL, loopLabel, METHOD, this));
			emitBody(objx, gen, retClass, body);

			Label end = gen.mark();
			gen.visitLocalVariable("this", "Ljava/lang/Object;", null, loopLabel, end, 0);
			for(ISeq lbs = argLocals.seq(); lbs != null; lbs = lbs.next())
				{
				LocalBinding lb = (LocalBinding) lbs.first();
				gen.visitLocalVariable(lb.name, argtypes[lb.idx-1].getDescriptor(), null, loopLabel, end, lb.idx);
				}
			}
		finally
			{
			Var.popThreadBindings();
			}

		gen.returnValue();
		//gen.visitMaxs(1, 1);
		gen.endMethod();

	//generate the regular invoke, calling the prim method
		Method m = new Method(getMethodName(), OBJECT_TYPE, getArgTypes());

		gen = new GeneratorAdapter(ACC_PUBLIC,
		                           m,
		                           null,
		                           //todo don't hardwire this
		                           EXCEPTION_TYPES,
		                           cv);
		gen.visitCode();
		gen.loadThis();
		for(int i = 0; i < argtypes.length; i++)
			{
			gen.loadArg(i);
			HostExpr.emitUnboxArg(fn, gen, argclasses[i]);
			}
		gen.invokeInterface(Type.getType("L"+prim+";"), ms);
		gen.box(getReturnType());


		gen.returnValue();
		//gen.visitMaxs(1, 1);
		gen.endMethod();

	}
	public void doEmit(ObjExpr fn, ClassVisitor cv){
		Method m = new Method(getMethodName(), getReturnType(), getArgTypes());

		GeneratorAdapter gen = new GeneratorAdapter(ACC_PUBLIC,
		                                            m,
		                                            null,
		                                            //todo don't hardwire this
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



	public final PersistentVector reqParms(){
		return reqParms;
	}

	public final LocalBinding restParm(){
		return restParm;
	}

	boolean isVariadic(){
		return restParm != null;
	}

	int numParams(){
		return reqParms.count() + (isVariadic() ? 1 : 0);
	}

	String getMethodName(){
		return isVariadic()?"doInvoke":"invoke";
	}

	Type getReturnType(){
		if(prim != null) //objx.isStatic)
			return Type.getType(retClass);
		return OBJECT_TYPE;
	}

	Type[] getArgTypes(){
		if(isVariadic() && reqParms.count() == MAX_POSITIONAL_ARITY)
			{
			Type[] ret = new Type[MAX_POSITIONAL_ARITY + 1];
			for(int i = 0;i<MAX_POSITIONAL_ARITY + 1;i++)
				ret[i] = OBJECT_TYPE;
			return ret;
			}
		return  ARG_TYPES[numParams()];
	}

	void emitClearLocals(GeneratorAdapter gen){
//		for(int i = 1; i < numParams() + 1; i++)
//			{
//			if(!localsUsedInCatchFinally.contains(i))
//				{
//				gen.visitInsn(Opcodes.ACONST_NULL);
//				gen.visitVarInsn(OBJECT_TYPE.getOpcode(Opcodes.ISTORE), i);
//				}
//			}
//		for(int i = numParams() + 1; i < maxLocal + 1; i++)
//			{
//			if(!localsUsedInCatchFinally.contains(i))
//				{
//				LocalBinding b = (LocalBinding) RT.get(indexlocals, i);
//				if(b == null || maybePrimitiveType(b.init) == null)
//					{
//					gen.visitInsn(Opcodes.ACONST_NULL);
//					gen.visitVarInsn(OBJECT_TYPE.getOpcode(Opcodes.ISTORE), i);
//					}
//				}
//			}
//		if(((FnExpr)objx).onceOnly)
//			{
//			objx.emitClearCloses(gen);
//			}
	}
}

abstract public static class ObjMethod{
	//when closures are defined inside other closures,
	//the closed over locals need to be propagated to the enclosing objx
	public final ObjMethod parent;
	//localbinding->localbinding
	IPersistentMap locals = null;
	//num->localbinding
	IPersistentMap indexlocals = null;
	Expr body = null;
	ObjExpr objx;
	PersistentVector argLocals;
	int maxLocal = 0;
	int line;
	int column;
	PersistentHashSet localsUsedInCatchFinally = PersistentHashSet.EMPTY;
	protected IPersistentMap methodMeta;


	public final IPersistentMap locals(){
		return locals;
	}

	public final Expr body(){
		return body;
	}

	public final ObjExpr objx(){
		return objx;
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

	public final int column(){
		return column;
	}

	public ObjMethod(ObjExpr objx, ObjMethod parent){
		this.parent = parent;
		this.objx = objx;
	}

	static void emitBody(ObjExpr objx, GeneratorAdapter gen, Class retClass, Expr body) {
			MaybePrimitiveExpr be = (MaybePrimitiveExpr) body;
			if(Util.isPrimitive(retClass) && be.canEmitPrimitive())
				{
				Class bc = maybePrimitiveType(be);
				if(bc == retClass)
					be.emitUnboxed(C.RETURN, objx, gen);
				else if(retClass == long.class && bc == int.class)
					{
					be.emitUnboxed(C.RETURN, objx, gen);
					gen.visitInsn(I2L);
					}
				else if(retClass == double.class && bc == float.class)
					{
					be.emitUnboxed(C.RETURN, objx, gen);
					gen.visitInsn(F2D);
					}
				else if(retClass == int.class && bc == long.class)
					{
					be.emitUnboxed(C.RETURN, objx, gen);
					gen.invokeStatic(RT_TYPE, Method.getMethod("int intCast(long)"));
					}
				else if(retClass == float.class && bc == double.class)
					{
					be.emitUnboxed(C.RETURN, objx, gen);
					gen.visitInsn(D2F);
					}
				else
					throw new IllegalArgumentException("Mismatched primitive return, expected: "
					                                   + retClass + ", had: " + be.getJavaClass());
				}
			else
				{
				body.emit(C.RETURN, objx, gen);
				if(retClass == void.class)
					{
					gen.pop();
					}
				else
					gen.unbox(Type.getType(retClass));
				}
	}
	abstract int numParams();
	abstract String getMethodName();
	abstract Type getReturnType();
	abstract Type[] getArgTypes();

	public void emit(ObjExpr fn, ClassVisitor cv){
		Method m = new Method(getMethodName(), getReturnType(), getArgTypes());

		GeneratorAdapter gen = new GeneratorAdapter(ACC_PUBLIC,
		                                            m,
		                                            null,
		                                            //todo don't hardwire this
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
    }
    
	void emitClearLocalsOld(GeneratorAdapter gen){
		for(int i=0;i<argLocals.count();i++)
			{
			LocalBinding lb = (LocalBinding) argLocals.nth(i);
			if(!localsUsedInCatchFinally.contains(lb.idx) && lb.getPrimitiveType() == null)
				{
				gen.visitInsn(Opcodes.ACONST_NULL);
				gen.storeArg(lb.idx - 1);				
				}

			}
//		for(int i = 1; i < numParams() + 1; i++)
//			{
//			if(!localsUsedInCatchFinally.contains(i))
//				{
//				gen.visitInsn(Opcodes.ACONST_NULL);
//				gen.visitVarInsn(OBJECT_TYPE.getOpcode(Opcodes.ISTORE), i);
//				}
//			}
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
	}
}

public static class LocalBinding{
	public final Symbol sym;
	public final Symbol tag;
	public Expr init;
	public final int idx;
	public final String name;
	public final boolean isArg;
    public final PathNode clearPathRoot;
	public boolean canBeCleared = !RT.booleanCast(getCompilerOption(disableLocalsClearingKey));
	public boolean recurMistmatch = false;

    public LocalBinding(int num, Symbol sym, Symbol tag, Expr init, boolean isArg,PathNode clearPathRoot)
                {
		if(maybePrimitiveType(init) != null && tag != null)
			throw new UnsupportedOperationException("Can't type hint a local with a primitive initializer");
		this.idx = num;
		this.sym = sym;
		this.tag = tag;
		this.init = init;
		this.isArg = isArg;
        this.clearPathRoot = clearPathRoot;
		name = munge(sym.name);
	}

	public boolean hasJavaClass() {
		if(init != null && init.hasJavaClass()
		   && Util.isPrimitive(init.getJavaClass())
		   && !(init instanceof MaybePrimitiveExpr))
			return false;
		return tag != null
		       || (init != null && init.hasJavaClass());
	}

	public Class getJavaClass() {
		return tag != null ? HostExpr.tagToClass(tag)
		                   : init.getJavaClass();
	}

	public Class getPrimitiveType(){
		return maybePrimitiveType(init);
	}
}

public static class LocalBindingExpr implements Expr, MaybePrimitiveExpr, AssignableExpr{
	public final LocalBinding b;
	public final Symbol tag;

    public final PathNode clearPath;
    public final PathNode clearRoot;
    public boolean shouldClear = false;


	public LocalBindingExpr(LocalBinding b, Symbol tag)
            {
		if(b.getPrimitiveType() != null && tag != null)
			throw new UnsupportedOperationException("Can't type hint a primitive local");
		this.b = b;
		this.tag = tag;

        this.clearPath = (PathNode)CLEAR_PATH.get();
        this.clearRoot = (PathNode)CLEAR_ROOT.get();
        IPersistentCollection sites = (IPersistentCollection) RT.get(CLEAR_SITES.get(),b);

        if(b.idx > 0)
            {
//            Object dummy;

            if(sites != null)
                {
                for(ISeq s = sites.seq();s!=null;s = s.next())
                    {
                    LocalBindingExpr o = (LocalBindingExpr) s.first();
                    PathNode common = commonPath(clearPath,o.clearPath);
                    if(common != null && common.type == PATHTYPE.PATH)
                        o.shouldClear = false;
//                    else
//                        dummy = null;
                    }
                }

            if(clearRoot == b.clearPathRoot)
                {
                this.shouldClear = true;
                sites = RT.conj(sites,this);
                CLEAR_SITES.set(RT.assoc(CLEAR_SITES.get(), b, sites));
                }
//            else
//                dummy = null;
            }
 	    }

	public Object eval() {
		throw new UnsupportedOperationException("Can't eval locals");
	}

	public boolean canEmitPrimitive(){
		return b.getPrimitiveType() != null;
	}

	public void emitUnboxed(C context, ObjExpr objx, GeneratorAdapter gen){
		objx.emitUnboxedLocal(gen, b);
	}

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
		if(context != C.STATEMENT)
			objx.emitLocal(gen, b, shouldClear);
	}

	public Object evalAssign(Expr val) {
		throw new UnsupportedOperationException("Can't eval locals");
	}

	public void emitAssign(C context, ObjExpr objx, GeneratorAdapter gen, Expr val){
		objx.emitAssignLocal(gen, b,val);
		if(context != C.STATEMENT)
			objx.emitLocal(gen, b, false);
	}

	public boolean hasJavaClass() {
		return tag != null || b.hasJavaClass();
	}

	public Class getJavaClass() {
		if(tag != null)
			return HostExpr.tagToClass(tag);
		return b.getJavaClass();
	}


}

public static class BodyExpr implements Expr, MaybePrimitiveExpr{
	PersistentVector exprs;

	public final PersistentVector exprs(){
		return exprs;
	}

	public BodyExpr(PersistentVector exprs){
		this.exprs = exprs;
	}

	static class Parser implements IParser{
		public Expr parse(C context, Object frms) {
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

	public Object eval() {
		Object ret = null;
		for(Object o : exprs)
			{
			Expr e = (Expr) o;
			ret = e.eval();
			}
		return ret;
	}

	public boolean canEmitPrimitive(){
		return lastExpr() instanceof MaybePrimitiveExpr && ((MaybePrimitiveExpr)lastExpr()).canEmitPrimitive();
	}

	public void emitUnboxed(C context, ObjExpr objx, GeneratorAdapter gen){
		for(int i = 0; i < exprs.count() - 1; i++)
			{
			Expr e = (Expr) exprs.nth(i);
			e.emit(C.STATEMENT, objx, gen);
			}
		MaybePrimitiveExpr last = (MaybePrimitiveExpr) exprs.nth(exprs.count() - 1);
		last.emitUnboxed(context, objx, gen);
	}

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
		for(int i = 0; i < exprs.count() - 1; i++)
			{
			Expr e = (Expr) exprs.nth(i);
			e.emit(C.STATEMENT, objx, gen);
			}
		Expr last = (Expr) exprs.nth(exprs.count() - 1);
		last.emit(context, objx, gen);
	}

	public boolean hasJavaClass() {
		return lastExpr().hasJavaClass();
	}

	public Class getJavaClass() {
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
		public Expr parse(C context, Object frm) {
			ISeq form = (ISeq) frm;
			//(letfns* [var (fn [args] body) ...] body...)
			if(!(RT.second(form) instanceof IPersistentVector))
				throw new IllegalArgumentException("Bad binding form, expected vector");

			IPersistentVector bindings = (IPersistentVector) RT.second(form);
			if((bindings.count() % 2) != 0)
				throw new IllegalArgumentException("Bad binding form, expected matched symbol expression pairs");

			ISeq body = RT.next(RT.next(form));

			if(context == C.EVAL)
				return analyze(context, RT.list(RT.list(FNONCE, PersistentVector.EMPTY, form)));

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
						throw Util.runtimeException("Can't let qualified name: " + sym);
					LocalBinding lb = registerLocal(sym, tagOf(sym), null,false);
					lb.canBeCleared = false;
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

	public Object eval() {
		throw new UnsupportedOperationException("Can't eval letfns");
	}

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
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
			bi.init.emit(C.EXPRESSION, objx, gen);
			gen.visitVarInsn(OBJECT_TYPE.getOpcode(Opcodes.ISTORE), bi.binding.idx);
			}

		for(int i = 0; i < bindingInits.count(); i++)
			{
			BindingInit bi = (BindingInit) bindingInits.nth(i);
			ObjExpr fe = (ObjExpr) bi.init;
			gen.visitVarInsn(OBJECT_TYPE.getOpcode(Opcodes.ILOAD), bi.binding.idx);
			fe.emitLetFnInits(gen, objx, lbset);
			}

		Label loopLabel = gen.mark();

		body.emit(context, objx, gen);

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

	public boolean hasJavaClass() {
		return body.hasJavaClass();
	}

	public Class getJavaClass() {
		return body.getJavaClass();
	}
}

public static class LetExpr implements Expr, MaybePrimitiveExpr{
	public final PersistentVector bindingInits;
	public final Expr body;
	public final boolean isLoop;

	public LetExpr(PersistentVector bindingInits, Expr body, boolean isLoop){
		this.bindingInits = bindingInits;
		this.body = body;
		this.isLoop = isLoop;
	}

	static class Parser implements IParser{
		public Expr parse(C context, Object frm) {
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
				return analyze(context, RT.list(RT.list(FNONCE, PersistentVector.EMPTY, form)));

			ObjMethod method = (ObjMethod) METHOD.deref();
			IPersistentMap backupMethodLocals = method.locals;
			IPersistentMap backupMethodIndexLocals = method.indexlocals;
			IPersistentVector recurMismatches = PersistentVector.EMPTY;
			for (int i = 0; i < bindings.count()/2; i++)
				{
				recurMismatches = recurMismatches.cons(RT.F);
				}

			//may repeat once for each binding with a mismatch, return breaks
			while(true){
				IPersistentMap dynamicBindings = RT.map(LOCAL_ENV, LOCAL_ENV.deref(),
														NEXT_LOCAL_NUM, NEXT_LOCAL_NUM.deref());
				method.locals = backupMethodLocals;
				method.indexlocals = backupMethodIndexLocals;

				PathNode looproot = new PathNode(PATHTYPE.PATH, (PathNode) CLEAR_PATH.get());
				PathNode clearroot = new PathNode(PATHTYPE.PATH,looproot);
				PathNode clearpath = new PathNode(PATHTYPE.PATH,looproot);
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
							throw Util.runtimeException("Can't let qualified name: " + sym);
						Expr init = analyze(C.EXPRESSION, bindings.nth(i + 1), sym.name);
						if(isLoop)
							{
							if(recurMismatches != null && RT.booleanCast(recurMismatches.nth(i/2)))
								{
								init = new StaticMethodExpr("", 0, 0, null, RT.class, "box", RT.vector(init));
								if(RT.booleanCast(RT.WARN_ON_REFLECTION.deref()))
									RT.errPrintWriter().println("Auto-boxing loop arg: " + sym);
								}
							else if(maybePrimitiveType(init) == int.class)
								init = new StaticMethodExpr("", 0, 0, null, RT.class, "longCast", RT.vector(init));
							else if(maybePrimitiveType(init) == float.class)
								init = new StaticMethodExpr("", 0, 0, null, RT.class, "doubleCast", RT.vector(init));
							}
						//sequential enhancement of env (like Lisp let*)
						try
							{
							if(isLoop)
								{
	                            Var.pushThreadBindings(
									RT.map(CLEAR_PATH, clearpath,
	                                       CLEAR_ROOT, clearroot,
	                                       NO_RECUR, null));

								}
							LocalBinding lb = registerLocal(sym, tagOf(sym), init,false);
							BindingInit bi = new BindingInit(lb, init);
							bindingInits = bindingInits.cons(bi);
							if(isLoop)
								loopLocals = loopLocals.cons(lb);
							}
						finally
							{
							if(isLoop)
							    Var.popThreadBindings();
							}
						}
					if(isLoop)
						LOOP_LOCALS.set(loopLocals);
					Expr bodyExpr;
					boolean moreMismatches = false;
					try {
						if(isLoop)
							{
                            Var.pushThreadBindings(
								RT.map(CLEAR_PATH, clearpath,
                                       CLEAR_ROOT, clearroot,
                                       NO_RECUR, null));
                                                       
							}
						bodyExpr = (new BodyExpr.Parser()).parse(isLoop ? C.RETURN : context, body);
						}
					finally{
						if(isLoop)
							{
						    Var.popThreadBindings();
							for(int i = 0;i< loopLocals.count();i++)
								{
								LocalBinding lb = (LocalBinding) loopLocals.nth(i);
								if(lb.recurMistmatch)
									{
									recurMismatches = (IPersistentVector)recurMismatches.assoc(i, RT.T);
									moreMismatches = true;
									}
								}
							}
						}
					if(!moreMismatches)
						return new LetExpr(bindingInits, bodyExpr, isLoop);
					}
				finally
					{
					Var.popThreadBindings();
					}
			}
		}
	}

	public Object eval() {
		throw new UnsupportedOperationException("Can't eval let/loop");
	}

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
		doEmit(context, objx, gen, false);
	}

	public void emitUnboxed(C context, ObjExpr objx, GeneratorAdapter gen){
		doEmit(context, objx, gen, true);
	}


	public void doEmit(C context, ObjExpr objx, GeneratorAdapter gen, boolean emitUnboxed){
		HashMap<BindingInit, Label> bindingLabels = new HashMap();
		for(int i = 0; i < bindingInits.count(); i++)
			{
			BindingInit bi = (BindingInit) bindingInits.nth(i);
			Class primc = maybePrimitiveType(bi.init);
			if(primc != null)
				{
				((MaybePrimitiveExpr) bi.init).emitUnboxed(C.EXPRESSION, objx, gen);
				gen.visitVarInsn(Type.getType(primc).getOpcode(Opcodes.ISTORE), bi.binding.idx);
				}
			else
				{
				bi.init.emit(C.EXPRESSION, objx, gen);
				gen.visitVarInsn(OBJECT_TYPE.getOpcode(Opcodes.ISTORE), bi.binding.idx);
				}
			bindingLabels.put(bi, gen.mark());
			}
		Label loopLabel = gen.mark();
		if(isLoop)
			{
			try
				{
				Var.pushThreadBindings(RT.map(LOOP_LABEL, loopLabel));
				if(emitUnboxed)
					((MaybePrimitiveExpr)body).emitUnboxed(context, objx, gen);
				else
					body.emit(context, objx, gen);
				}
			finally
				{
				Var.popThreadBindings();
				}
			}
		else
			{
			if(emitUnboxed)
				((MaybePrimitiveExpr)body).emitUnboxed(context, objx, gen);
			else
				body.emit(context, objx, gen);
			}
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
				gen.visitLocalVariable(lname, Type.getDescriptor(primc), null, bindingLabels.get(bi), end,
				                       bi.binding.idx);
			else
				gen.visitLocalVariable(lname, "Ljava/lang/Object;", null, bindingLabels.get(bi), end, bi.binding.idx);
			}
	}

	public boolean hasJavaClass() {
		return body.hasJavaClass();
	}

	public Class getJavaClass() {
		return body.getJavaClass();
	}

	public boolean canEmitPrimitive(){
		return body instanceof MaybePrimitiveExpr && ((MaybePrimitiveExpr)body).canEmitPrimitive();
	}

}

public static class RecurExpr implements Expr, MaybePrimitiveExpr{
	public final IPersistentVector args;
	public final IPersistentVector loopLocals;
	final int line;
	final int column;
	final String source;


	public RecurExpr(IPersistentVector loopLocals, IPersistentVector args, int line, int column, String source){
		this.loopLocals = loopLocals;
		this.args = args;
		this.line = line;
		this.column = column;
		this.source = source;
	}

	public Object eval() {
		throw new UnsupportedOperationException("Can't eval recur");
	}

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
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
				final Class pc = maybePrimitiveType(arg);
				if(pc == primc)
					((MaybePrimitiveExpr) arg).emitUnboxed(C.EXPRESSION, objx, gen);
				else if(primc == long.class && pc == int.class)
					{
					((MaybePrimitiveExpr) arg).emitUnboxed(C.EXPRESSION, objx, gen);
					gen.visitInsn(I2L);
					}
				else if(primc == double.class && pc == float.class)
					{
					((MaybePrimitiveExpr) arg).emitUnboxed(C.EXPRESSION, objx, gen);
					gen.visitInsn(F2D);
					}
				else if(primc == int.class && pc == long.class)
					{
					((MaybePrimitiveExpr) arg).emitUnboxed(C.EXPRESSION, objx, gen);
					gen.invokeStatic(RT_TYPE, Method.getMethod("int intCast(long)"));
					}
				else if(primc == float.class && pc == double.class)
					{
					((MaybePrimitiveExpr) arg).emitUnboxed(C.EXPRESSION, objx, gen);
					gen.visitInsn(D2F);
					}
				else
					{
//					if(true)//RT.booleanCast(RT.WARN_ON_REFLECTION.deref()))
						throw new IllegalArgumentException
//						RT.errPrintWriter().println
							(//source + ":" + line +
							 " recur arg for primitive local: " +
					                                   lb.name + " is not matching primitive, had: " +
															(arg.hasJavaClass() ? arg.getJavaClass().getName():"Object") +
															", needed: " +
															primc.getName());
//					arg.emit(C.EXPRESSION, objx, gen);
//					HostExpr.emitUnboxArg(objx,gen,primc);
					}
				}
			else
				{
				arg.emit(C.EXPRESSION, objx, gen);
				}
			}

		for(int i = loopLocals.count() - 1; i >= 0; i--)
			{
			LocalBinding lb = (LocalBinding) loopLocals.nth(i);
			Class primc = lb.getPrimitiveType();
			if(lb.isArg)
				gen.storeArg(lb.idx-(objx.isStatic?0:1));
			else
				{
				if(primc != null)
					gen.visitVarInsn(Type.getType(primc).getOpcode(Opcodes.ISTORE), lb.idx);
				else
					gen.visitVarInsn(OBJECT_TYPE.getOpcode(Opcodes.ISTORE), lb.idx);
				}
			}

		gen.goTo(loopLabel);
	}

	public boolean hasJavaClass() {
		return true;
	}

	public Class getJavaClass() {
		return RECUR_CLASS;
	}

	static class Parser implements IParser{
		public Expr parse(C context, Object frm) {
			int line = lineDeref();
			int column = columnDeref();
			String source = (String) SOURCE.deref();

			ISeq form = (ISeq) frm;
			IPersistentVector loopLocals = (IPersistentVector) LOOP_LOCALS.deref();
			if(context != C.RETURN || loopLocals == null)
				throw new UnsupportedOperationException("Can only recur from tail position");
                        if(NO_RECUR.deref() != null)
                            throw new UnsupportedOperationException("Cannot recur across try");
			PersistentVector args = PersistentVector.EMPTY;
			for(ISeq s = RT.seq(form.next()); s != null; s = s.next())
				{
				args = args.cons(analyze(C.EXPRESSION, s.first()));
				}
			if(args.count() != loopLocals.count())
				throw new IllegalArgumentException(
						String.format("Mismatched argument count to recur, expected: %d args, got: %d",
						              loopLocals.count(), args.count()));
			for(int i = 0;i< loopLocals.count();i++)
				{
				LocalBinding lb = (LocalBinding) loopLocals.nth(i);
				Class primc = lb.getPrimitiveType();
				if(primc != null)
					{
					boolean mismatch = false;
					final Class pc = maybePrimitiveType((Expr) args.nth(i));
					if(primc == long.class)
						{
						if(!(pc == long.class
							|| pc == int.class
							|| pc == short.class
							|| pc == char.class
							|| pc == byte.class))
							mismatch = true;
						}
					else if(primc == double.class)
						{
						if(!(pc == double.class
							|| pc == float.class))
							mismatch = true;
						}
					if(mismatch)
						{
						lb.recurMistmatch = true;
						if(RT.booleanCast(RT.WARN_ON_REFLECTION.deref()))
							RT.errPrintWriter().println
								(source + ":" + line +
								 " recur arg for primitive local: " +
						                                   lb.name + " is not matching primitive, had: " +
															(pc != null ? pc.getName():"Object") +
															", needed: " +
															primc.getName());
						}
					}
				}
			return new RecurExpr(loopLocals, args, line, column, source);
		}
	}

	public boolean canEmitPrimitive() {
		return true;
	}

	public void emitUnboxed(C context, ObjExpr objx, GeneratorAdapter gen) {
		emit(context, objx, gen);
	}
}

private static LocalBinding registerLocal(Symbol sym, Symbol tag, Expr init, boolean isArg) {
	int num = getAndIncLocalNum();
	LocalBinding b = new LocalBinding(num, sym, tag, init, isArg, clearPathRoot());
	IPersistentMap localsMap = (IPersistentMap) LOCAL_ENV.deref();
	LOCAL_ENV.set(RT.assoc(localsMap, b.sym, b));
	ObjMethod method = (ObjMethod) METHOD.deref();
	method.locals = (IPersistentMap) RT.assoc(method.locals, b, b);
	method.indexlocals = (IPersistentMap) RT.assoc(method.indexlocals, num, b);
	return b;
}

private static int getAndIncLocalNum(){
	int num = ((Number) NEXT_LOCAL_NUM.deref()).intValue();
	ObjMethod m = (ObjMethod) METHOD.deref();
	if(num > m.maxLocal)
		m.maxLocal = num;
	NEXT_LOCAL_NUM.set(num + 1);
	return num;
}

public static Expr analyze(C context, Object form) {
	return analyze(context, form, null);
}

private static Expr analyze(C context, Object form, String name) {
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
		else if(form instanceof Number)
			return NumberExpr.parse((Number) form);
		else if(fclass == String.class)
				return new StringExpr(((String) form).intern());
//	else if(fclass == Character.class)
//		return new CharExpr((Character) form);
		else if(form instanceof IPersistentCollection && ((IPersistentCollection) form).count() == 0)
				{
				Expr ret = new EmptyExpr(form);
				if(RT.meta(form) != null)
					ret = new MetaExpr(ret, MapExpr
							.parse(context == C.EVAL ? context : C.EXPRESSION, ((IObj) form).meta()));
				return ret;
				}
		else if(form instanceof ISeq)
				return analyzeSeq(context, (ISeq) form, name);
		else if(form instanceof IPersistentVector)
				return VectorExpr.parse(context, (IPersistentVector) form);
		else if(form instanceof IRecord)
				return new ConstantExpr(form);
		else if(form instanceof IType)
				return new ConstantExpr(form);
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
			throw new CompilerException((String) SOURCE_PATH.deref(), lineDeref(), columnDeref(), e);
		else
			throw (CompilerException) e;
		}
}

static public class CompilerException extends RuntimeException{
	final public String source;
	
	final public int line;

	public CompilerException(String source, int line, int column, Throwable cause){
		super(errorMsg(source, line, column, cause.toString()), cause);
		this.source = source;
		this.line = line;
	}

	public String toString(){
		return getMessage();
	}
}

static public Var isMacro(Object op) {
	//no local macros for now
	if(op instanceof Symbol && referenceLocal((Symbol) op) != null)
		return null;
	if(op instanceof Symbol || op instanceof Var)
		{
                Var v = (op instanceof Var) ? (Var) op : lookupVar((Symbol) op, false, false);
		if(v != null && v.isMacro())
			{
			if(v.ns != currentNS() && !v.isPublic())
				throw new IllegalStateException("var: " + v + " is not public");
			return v;
			}
		}
	return null;
}

static public IFn isInline(Object op, int arity) {
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
				IFn arityPred = (IFn) RT.get(v.meta(), inlineAritiesKey);
				if(arityPred == null || RT.booleanCast(arityPred.invoke(arity)))
					return ret;
				}
			}
		}
	return null;
}

public static boolean namesStaticMember(Symbol sym){
	return sym.ns != null && namespaceFor(sym) == null;
}

public static Object preserveTag(ISeq src, Object dst) {
	Symbol tag = tagOf(src);
	if (tag != null && dst instanceof IObj) {
		IPersistentMap meta = RT.meta(dst);
		return ((IObj) dst).withMeta((IPersistentMap) RT.assoc(meta, RT.TAG_KEY, tag));
	}
	return dst;
}

public static Object macroexpand1(Object x) {
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
						return v.applyTo(RT.cons(form,RT.cons(LOCAL_ENV.get(),form.next())));
					}
				catch(ArityException e)
					{
						// hide the 2 extra params for a macro
						throw new ArityException(e.actual - 2, e.name);
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
					return preserveTag(form, RT.listStar(DOT, target, meth, form.next().next()));
					}
				else if(namesStaticMember(sym))
					{
					Symbol target = Symbol.intern(sym.ns);
					Class c = HostExpr.maybeClass(target, false);
					if(c != null)
						{
						Symbol meth = Symbol.intern(sym.name);
						return preserveTag(form, RT.listStar(DOT, target, meth, form.next()));
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

static Object macroexpand(Object form) {
	Object exf = macroexpand1(form);
	if(exf != form)
		return macroexpand(exf);
	return form;
}

private static Expr analyzeSeq(C context, ISeq form, String name) {
	Object line = lineDeref();
	Object column = columnDeref();
	if(RT.meta(form) != null && RT.meta(form).containsKey(RT.LINE_KEY))
		line = RT.meta(form).valAt(RT.LINE_KEY);
	if(RT.meta(form) != null && RT.meta(form).containsKey(RT.COLUMN_KEY))
		column = RT.meta(form).valAt(RT.COLUMN_KEY);
	Var.pushThreadBindings(
			RT.map(LINE, line, COLUMN, column));
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
			return analyze(context, preserveTag(form, inline.applyTo(RT.next(form))));
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
			throw new CompilerException((String) SOURCE_PATH.deref(), lineDeref(), columnDeref(), e);
		else
			throw (CompilerException) e;
		}
	finally
		{
		Var.popThreadBindings();
		}
}

static String errorMsg(String source, int line, int column, String s){
	return String.format("%s, compiling:(%s:%d:%d)", s, source, line, column);
}

public static Object eval(Object form) {
	return eval(form, true);
}

public static Object eval(Object form, boolean freshLoader) {
	boolean createdLoader = false;
	if(true)//!LOADER.isBound())
		{
		Var.pushThreadBindings(RT.map(LOADER, RT.makeClassLoader()));
		createdLoader = true;
		}
	try
		{
		Object line = lineDeref();
		Object column = columnDeref();
		if(RT.meta(form) != null && RT.meta(form).containsKey(RT.LINE_KEY))
			line = RT.meta(form).valAt(RT.LINE_KEY);
		if(RT.meta(form) != null && RT.meta(form).containsKey(RT.COLUMN_KEY))
			column = RT.meta(form).valAt(RT.COLUMN_KEY);
		Var.pushThreadBindings(RT.map(LINE, line, COLUMN, column));
		try
			{
			form = macroexpand(form);
			if(form instanceof ISeq && Util.equals(RT.first(form), DO))
				{
				ISeq s = RT.next(form);
				for(; RT.next(s) != null; s = RT.next(s))
					eval(RT.first(s), false);
				return eval(RT.first(s), false);
				}
			else if((form instanceof IType) ||
					(form instanceof IPersistentCollection
					&& !(RT.first(form) instanceof Symbol
						&& ((Symbol) RT.first(form)).name.startsWith("def"))))
				{
				ObjExpr fexpr = (ObjExpr) analyze(C.EXPRESSION, RT.list(FN, PersistentVector.EMPTY, form),
													"eval" + RT.nextID());
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
	IdentityHashMap<Object,Integer> ids = (IdentityHashMap<Object,Integer>) CONSTANT_IDS.deref();
	Integer i = ids.get(o);
	if(i != null)
		return i;
	CONSTANTS.set(RT.conj(v, o));
	ids.put(o, v.count());
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

private static int registerKeywordCallsite(Keyword keyword){
	if(!KEYWORD_CALLSITES.isBound())
		throw new IllegalAccessError("KEYWORD_CALLSITES is not bound");

	IPersistentVector keywordCallsites = (IPersistentVector) KEYWORD_CALLSITES.deref();

	keywordCallsites = keywordCallsites.cons(keyword);
	KEYWORD_CALLSITES.set(keywordCallsites);
	return keywordCallsites.count()-1;
}

private static int registerProtocolCallsite(Var v){
	if(!PROTOCOL_CALLSITES.isBound())
		throw new IllegalAccessError("PROTOCOL_CALLSITES is not bound");

	IPersistentVector protocolCallsites = (IPersistentVector) PROTOCOL_CALLSITES.deref();

	protocolCallsites = protocolCallsites.cons(v);
	PROTOCOL_CALLSITES.set(protocolCallsites);
	return protocolCallsites.count()-1;
}

private static void registerVarCallsite(Var v){
	if(!VAR_CALLSITES.isBound())
		throw new IllegalAccessError("VAR_CALLSITES is not bound");

	IPersistentCollection varCallsites = (IPersistentCollection) VAR_CALLSITES.deref();

	varCallsites = varCallsites.cons(v);
	VAR_CALLSITES.set(varCallsites);
//	return varCallsites.count()-1;
}

static ISeq fwdPath(PathNode p1){
    ISeq ret = null;
    for(;p1 != null;p1 = p1.parent)
        ret = RT.cons(p1,ret);
    return ret;
}

static PathNode commonPath(PathNode n1, PathNode n2){
    ISeq xp = fwdPath(n1);
    ISeq yp = fwdPath(n2);
    if(RT.first(xp) != RT.first(yp))
        return null;
    while(RT.second(xp) != null && RT.second(xp) == RT.second(yp))
        {
        xp = xp.next();
        yp = yp.next();
        }
    return (PathNode) RT.first(xp);
}

static void addAnnotation(Object visitor, IPersistentMap meta){
	if(meta != null && ADD_ANNOTATIONS.isBound())
		 ADD_ANNOTATIONS.invoke(visitor, meta);
}

static void addParameterAnnotation(Object visitor, IPersistentMap meta, int i){
	if(meta != null && ADD_ANNOTATIONS.isBound())
		 ADD_ANNOTATIONS.invoke(visitor, meta, i);
}

private static Expr analyzeSymbol(Symbol sym) {
	Symbol tag = tagOf(sym);
	if(sym.ns == null) //ns-qualified syms are always Vars
		{
		LocalBinding b = referenceLocal(sym);
		if(b != null)
            {
            return new LocalBindingExpr(b, tag);
            }
		}
	else
		{
		if(namespaceFor(sym) == null)
			{
			Symbol nsSym = Symbol.intern(sym.ns);
			Class c = HostExpr.maybeClass(nsSym, false);
			if(c != null)
				{
				if(Reflector.getField(c, sym.name, true) != null)
					return new StaticFieldExpr(lineDeref(), columnDeref(), c, sym.name, tag);
				throw Util.runtimeException("Unable to find static field: " + sym.name + " in " + c);
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
			throw Util.runtimeException("Can't take value of a macro: " + v);
		if(RT.booleanCast(RT.get(v.meta(),RT.CONST_KEY)))
			return analyze(C.EXPRESSION, RT.list(QUOTE, v.get()));
		registerVar(v);
		return new VarExpr(v, tag);
		}
	else if(o instanceof Class)
		return new ConstantExpr(o);
	else if(o instanceof Symbol)
			return new UnresolvedVarExpr((Symbol) o);

	throw Util.runtimeException("Unable to resolve symbol: " + sym + " in this context");

}

static String destubClassName(String className){
	//skip over prefix + '.' or '/'
	if(className.startsWith(COMPILE_STUB_PREFIX))
		return className.substring(COMPILE_STUB_PREFIX.length()+1);
	return className;
}

static Type getType(Class c){
	String descriptor = Type.getType(c).getDescriptor();
	if(descriptor.startsWith("L"))
		descriptor = "L" + destubClassName(descriptor.substring(1));
	return Type.getType(descriptor);
}

static Object resolve(Symbol sym, boolean allowPrivate) {
	return resolveIn(currentNS(), sym, allowPrivate);
}

static Object resolve(Symbol sym) {
	return resolveIn(currentNS(), sym, false);
}

static Namespace namespaceFor(Symbol sym){
	return namespaceFor(currentNS(), sym);
}

static Namespace namespaceFor(Namespace inns, Symbol sym){
	//note, presumes non-nil sym.ns
	// first check against currentNS' aliases...
	Symbol nsSym = Symbol.intern(sym.ns);
	Namespace ns = inns.lookupAlias(nsSym);
	if(ns == null)
		{
		// ...otherwise check the Namespaces map.
		ns = Namespace.find(nsSym);
		}
	return ns;
}

static public Object resolveIn(Namespace n, Symbol sym, boolean allowPrivate) {
	//note - ns-qualified vars must already exist
	if(sym.ns != null)
		{
		Namespace ns = namespaceFor(n, sym);
		if(ns == null)
			throw Util.runtimeException("No such namespace: " + sym.ns);

		Var v = ns.findInternedVar(Symbol.intern(sym.name));
		if(v == null)
			throw Util.runtimeException("No such var: " + sym);
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
		if(Util.equals(sym, COMPILE_STUB_SYM.get()))
			return COMPILE_STUB_CLASS.get();
		Object o = n.getMapping(sym);
		if(o == null)
			{
			if(RT.booleanCast(RT.ALLOW_UNRESOLVED_VARS.deref()))
				{
				return sym;
				}
			else
				{
				throw Util.runtimeException("Unable to resolve symbol: " + sym + " in this context");
				}
			}
		return o;
		}
}


static public Object maybeResolveIn(Namespace n, Symbol sym) {
	//note - ns-qualified vars must already exist
	if(sym.ns != null)
		{
		Namespace ns = namespaceFor(n, sym);
		if(ns == null)
			return null;
		Var v = ns.findInternedVar(Symbol.intern(sym.name));
		if(v == null)
			return null;
		return v;
		}
	else if(sym.name.indexOf('.') > 0 && !sym.name.endsWith(".") 
			|| sym.name.charAt(0) == '[')
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


static Var lookupVar(Symbol sym, boolean internNew, boolean registerMacro) {
	Var var = null;

	//note - ns-qualified vars in other namespaces must already exist
	if(sym.ns != null)
		{
		Namespace ns = namespaceFor(sym);
		if(ns == null)
			return null;
		//throw Util.runtimeException("No such namespace: " + sym.ns);
		Symbol name = Symbol.intern(sym.name);
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
					var = currentNS().intern(Symbol.intern(sym.name));
					}
			else if(o instanceof Var)
				{
				var = (Var) o;
				}
			else
				{
				throw Util.runtimeException("Expecting var, but " + sym + " is mapped to " + o);
				}
			}
	if(var != null && (!var.isMacro() || registerMacro))
		registerVar(var);
	return var;
}
static Var lookupVar(Symbol sym, boolean internNew) {
    return lookupVar(sym, internNew, true);
}

private static void registerVar(Var var) {
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

static void closeOver(LocalBinding b, ObjMethod method){
	if(b != null && method != null)
		{
		if(RT.get(method.locals, b) == null)
			{
			method.objx.closes = (IPersistentMap) RT.assoc(method.objx.closes, b, b);
			closeOver(b, method.parent);
			}
		else if(IN_CATCH_FINALLY.deref() != null)
			{
			method.localsUsedInCatchFinally = (PersistentHashSet) method.localsUsedInCatchFinally.cons(b.idx);
			}
		}
}


static LocalBinding referenceLocal(Symbol sym) {
	if(!LOCAL_ENV.isBound())
		return null;
	LocalBinding b = (LocalBinding) RT.get(LOCAL_ENV.deref(), sym);
	if(b != null)
		{
		ObjMethod method = (ObjMethod) METHOD.deref();
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

public static Object loadFile(String file) throws IOException{
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

public static Object load(Reader rdr) {
	return load(rdr, null, "NO_SOURCE_FILE");
}

public static Object load(Reader rdr, String sourcePath, String sourceName) {
	Object EOF = new Object();
	Object ret = null;
	LineNumberingPushbackReader pushbackReader =
			(rdr instanceof LineNumberingPushbackReader) ? (LineNumberingPushbackReader) rdr :
			new LineNumberingPushbackReader(rdr);
	Var.pushThreadBindings(
			RT.mapUniqueKeys(LOADER, RT.makeClassLoader(),
			       SOURCE_PATH, sourcePath,
			       SOURCE, sourceName,
			       METHOD, null,
			       LOCAL_ENV, null,
					LOOP_LOCALS, null,
					NEXT_LOCAL_NUM, 0,
					RT.READEVAL, RT.T,
			       RT.CURRENT_NS, RT.CURRENT_NS.deref(),
			       LINE_BEFORE, pushbackReader.getLineNumber(),
			       COLUMN_BEFORE, pushbackReader.getColumnNumber(),
			       LINE_AFTER, pushbackReader.getLineNumber(),
			       COLUMN_AFTER, pushbackReader.getColumnNumber()
			       ,RT.UNCHECKED_MATH, RT.UNCHECKED_MATH.deref()
					,RT.WARN_ON_REFLECTION, RT.WARN_ON_REFLECTION.deref()
			       ,RT.DATA_READERS, RT.DATA_READERS.deref()
                        ));

	try
		{
		for(Object r = LispReader.read(pushbackReader, false, EOF, false); r != EOF;
		    r = LispReader.read(pushbackReader, false, EOF, false))
			{
			LINE_AFTER.set(pushbackReader.getLineNumber());
			COLUMN_AFTER.set(pushbackReader.getColumnNumber());
			ret = eval(r,false);
			LINE_BEFORE.set(pushbackReader.getLineNumber());
			COLUMN_BEFORE.set(pushbackReader.getColumnNumber());
			}
		}
	catch(LispReader.ReaderException e)
		{
		throw new CompilerException(sourcePath, e.line, e.column, e.getCause());
		}
	catch(Throwable e)
		{
		if(!(e instanceof CompilerException))
			throw new CompilerException(sourcePath, (Integer) LINE_BEFORE.deref(), (Integer) COLUMN_BEFORE.deref(), e);
		else
			throw (CompilerException) e;
		}
	finally
		{
		Var.popThreadBindings();
		}
	return ret;
}

static public void writeClassFile(String internalName, byte[] bytecode) throws IOException{
	String genPath = (String) COMPILE_PATH.deref();
	if(genPath == null)
		throw Util.runtimeException("*compile-path* not set");
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
	Var.pushThreadBindings(PersistentHashMap.create(Var.intern(Symbol.intern("clojure.core"),
	                                                           Symbol.intern("*ns*")).setDynamic(), null));
}

public static void pushNSandLoader(ClassLoader loader){
	Var.pushThreadBindings(RT.map(Var.intern(Symbol.intern("clojure.core"),
	                                         Symbol.intern("*ns*")).setDynamic(),
	                              null,
	                              RT.FN_LOADER_VAR, loader,
	                              RT.READEVAL, RT.T
	                              ));
}

public static ILookupThunk getLookupThunk(Object target, Keyword k){
	return null;  //To change body of created methods use File | Settings | File Templates.
}

static void compile1(GeneratorAdapter gen, ObjExpr objx, Object form) {
	Object line = lineDeref();
	Object column = columnDeref();
	if(RT.meta(form) != null && RT.meta(form).containsKey(RT.LINE_KEY))
		line = RT.meta(form).valAt(RT.LINE_KEY);
	if(RT.meta(form) != null && RT.meta(form).containsKey(RT.COLUMN_KEY))
		column = RT.meta(form).valAt(RT.COLUMN_KEY);
	Var.pushThreadBindings(
			RT.map(LINE, line, COLUMN, column
			       ,LOADER, RT.makeClassLoader()
			));
	try
		{
		form = macroexpand(form);
		if(form instanceof ISeq && Util.equals(RT.first(form), DO))
			{
			for(ISeq s = RT.next(form); s != null; s = RT.next(s))
				{
				compile1(gen, objx, RT.first(s));
				}
			}
		else
			{
			Expr expr = analyze(C.EVAL, form);
			objx.keywords = (IPersistentMap) KEYWORDS.deref();
			objx.vars = (IPersistentMap) VARS.deref();
			objx.constants = (PersistentVector) CONSTANTS.deref();
			expr.emit(C.EXPRESSION, objx, gen);
			expr.eval();
			}
		}
	finally
		{
		Var.popThreadBindings();
		}
}

public static Object compile(Reader rdr, String sourcePath, String sourceName) throws IOException{
	if(COMPILE_PATH.deref() == null)
		throw Util.runtimeException("*compile-path* not set");

	Object EOF = new Object();
	Object ret = null;
	LineNumberingPushbackReader pushbackReader =
			(rdr instanceof LineNumberingPushbackReader) ? (LineNumberingPushbackReader) rdr :
			new LineNumberingPushbackReader(rdr);
	Var.pushThreadBindings(
			RT.mapUniqueKeys(SOURCE_PATH, sourcePath,
			       SOURCE, sourceName,
			       METHOD, null,
			       LOCAL_ENV, null,
					LOOP_LOCALS, null,
					NEXT_LOCAL_NUM, 0,
					RT.READEVAL, RT.T,
					RT.CURRENT_NS, RT.CURRENT_NS.deref(),
			       LINE_BEFORE, pushbackReader.getLineNumber(),
			       COLUMN_BEFORE, pushbackReader.getColumnNumber(),
			       LINE_AFTER, pushbackReader.getLineNumber(),
			       COLUMN_AFTER, pushbackReader.getColumnNumber(),
			       CONSTANTS, PersistentVector.EMPTY,
			       CONSTANT_IDS, new IdentityHashMap(),
			       KEYWORDS, PersistentHashMap.EMPTY,
			       VARS, PersistentHashMap.EMPTY
					,RT.UNCHECKED_MATH, RT.UNCHECKED_MATH.deref()
					,RT.WARN_ON_REFLECTION, RT.WARN_ON_REFLECTION.deref()
					,RT.DATA_READERS, RT.DATA_READERS.deref()
			   //    ,LOADER, RT.makeClassLoader()
			));

	try
		{
		//generate loader class
		ObjExpr objx = new ObjExpr(null);
		objx.internalName = sourcePath.replace(File.separator, "/").substring(0, sourcePath.lastIndexOf('.'))
		                  + RT.LOADER_SUFFIX;

		objx.objtype = Type.getObjectType(objx.internalName);
		ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_MAXS);
		ClassVisitor cv = cw;
		cv.visit(V1_5, ACC_PUBLIC + ACC_SUPER, objx.internalName, null, "java/lang/Object", null);

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
				COLUMN_AFTER.set(pushbackReader.getColumnNumber());
				compile1(gen, objx, r);
				LINE_BEFORE.set(pushbackReader.getLineNumber());
				COLUMN_BEFORE.set(pushbackReader.getColumnNumber());
			}
		//end of load
		gen.returnValue();
		gen.endMethod();

		//static fields for constants
		for(int i = 0; i < objx.constants.count(); i++)
			{
			cv.visitField(ACC_PUBLIC + ACC_FINAL + ACC_STATIC, objx.constantName(i), objx.constantType(i).getDescriptor(),
			              null, null);
			}

		final int INITS_PER = 100;
		int numInits =  objx.constants.count() / INITS_PER;
		if(objx.constants.count() % INITS_PER != 0)
			++numInits;

		for(int n = 0;n<numInits;n++)
			{
			GeneratorAdapter clinitgen = new GeneratorAdapter(ACC_PUBLIC + ACC_STATIC,
			                                                  Method.getMethod("void __init" + n + "()"),
			                                                  null,
			                                                  null,
			                                                  cv);
			clinitgen.visitCode();
			try
				{
				Var.pushThreadBindings(RT.map(RT.PRINT_DUP, RT.T));

				for(int i = n*INITS_PER; i < objx.constants.count() && i < (n+1)*INITS_PER; i++)
					{
					objx.emitValue(objx.constants.nth(i), clinitgen);
					clinitgen.checkCast(objx.constantType(i));
					clinitgen.putStatic(objx.objtype, objx.constantName(i), objx.constantType(i));
					}
				}
			finally
				{
				Var.popThreadBindings();
				}
			clinitgen.returnValue();
			clinitgen.endMethod();
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

//		if(objx.constants.count() > 0)
//			{
//			objx.emitConstants(clinitgen);
//			}
		for(int n = 0;n<numInits;n++)
			clinitgen.invokeStatic(objx.objtype, Method.getMethod("void __init" + n + "()"));

		clinitgen.push(objx.internalName.replace('/','.'));
		clinitgen.invokeStatic(CLASS_TYPE, Method.getMethod("Class forName(String)"));
		clinitgen.invokeVirtual(CLASS_TYPE,Method.getMethod("ClassLoader getClassLoader()"));
		clinitgen.invokeStatic(Type.getType(Compiler.class), Method.getMethod("void pushNSandLoader(ClassLoader)"));
		clinitgen.mark(startTry);
		clinitgen.invokeStatic(objx.objtype, Method.getMethod("void load()"));
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

		writeClassFile(objx.internalName, cw.toByteArray());
		}
	catch(LispReader.ReaderException e)
		{
		throw new CompilerException(sourcePath, e.line, e.column, e.getCause());
		}
	finally
		{
		Var.popThreadBindings();
		}
	return ret;
}


static public class NewInstanceExpr extends ObjExpr{
	//IPersistentMap optionsMap = PersistentArrayMap.EMPTY;
	IPersistentCollection methods;

	Map<IPersistentVector,java.lang.reflect.Method> mmap;
	Map<IPersistentVector,Set<Class>> covariants;

	public NewInstanceExpr(Object tag){
		super(tag);
	}

	static class DeftypeParser implements IParser{
		public Expr parse(C context, final Object frm) {
			ISeq rform = (ISeq) frm;
			//(deftype* tagname classname [fields] :implements [interfaces] :tag tagname methods*)
			rform = RT.next(rform);
			String tagname = ((Symbol) rform.first()).toString();
			rform = rform.next();
			Symbol classname = (Symbol) rform.first();
			rform = rform.next();
			IPersistentVector fields = (IPersistentVector) rform.first();
			rform = rform.next();
			IPersistentMap opts = PersistentHashMap.EMPTY;
			while(rform != null && rform.first() instanceof Keyword)
				{
				opts = opts.assoc(rform.first(), RT.second(rform));
				rform = rform.next().next();
				}

			ObjExpr ret = build((IPersistentVector)RT.get(opts,implementsKey,PersistentVector.EMPTY),fields,null,tagname, classname,
			             (Symbol) RT.get(opts,RT.TAG_KEY),rform, frm);
			return ret;
		}
	}

	static class ReifyParser implements IParser{
	public Expr parse(C context, Object frm) {
		//(reify this-name? [interfaces] (method-name [args] body)*)
		ISeq form = (ISeq) frm;
		ObjMethod enclosingMethod = (ObjMethod) METHOD.deref();
		String basename = enclosingMethod != null ?
		                  (trimGenID(enclosingMethod.objx.name) + "$")
		                 : (munge(currentNS().name.name) + "$");
		String simpleName = "reify__" + RT.nextID();
		String classname = basename + simpleName;

		ISeq rform = RT.next(form);

		IPersistentVector interfaces = ((IPersistentVector) RT.first(rform)).cons(Symbol.intern("clojure.lang.IObj"));


		rform = RT.next(rform);


		ObjExpr ret = build(interfaces, null, null, classname, Symbol.intern(classname), null, rform, frm);
		if(frm instanceof IObj && ((IObj) frm).meta() != null)
			return new MetaExpr(ret, MapExpr
					.parse(context == C.EVAL ? context : C.EXPRESSION, ((IObj) frm).meta()));
		else
			return ret;
	}
	}

	static ObjExpr build(IPersistentVector interfaceSyms, IPersistentVector fieldSyms, Symbol thisSym,
	                     String tagName, Symbol className,
	                  Symbol typeTag, ISeq methodForms, Object frm) {
		NewInstanceExpr ret = new NewInstanceExpr(null);

		ret.src = frm;
		ret.name = className.toString();
		ret.classMeta = RT.meta(className);
		ret.internalName = ret.name.replace('.', '/');
		ret.objtype = Type.getObjectType(ret.internalName);

		if(thisSym != null)
			ret.thisName = thisSym.name;

		if(fieldSyms != null)
			{
			IPersistentMap fmap = PersistentHashMap.EMPTY;
			Object[] closesvec = new Object[2 * fieldSyms.count()];
			for(int i=0;i<fieldSyms.count();i++)
				{
				Symbol sym = (Symbol) fieldSyms.nth(i);
				LocalBinding lb = new LocalBinding(-1, sym, null,
				                                   new MethodParamExpr(tagClass(tagOf(sym))),false,null);
				fmap = fmap.assoc(sym, lb);
				closesvec[i*2] = lb;
				closesvec[i*2 + 1] = lb;
				}

			//todo - inject __meta et al into closes - when?
			//use array map to preserve ctor order
			ret.closes = new PersistentArrayMap(closesvec);
			ret.fields = fmap;
			for(int i=fieldSyms.count()-1;i >= 0 && (((Symbol)fieldSyms.nth(i)).name.equals("__meta") || ((Symbol)fieldSyms.nth(i)).name.equals("__extmap"));--i)
				ret.altCtorDrops++;
			}
		//todo - set up volatiles
//		ret.volatiles = PersistentHashSet.create(RT.seq(RT.get(ret.optionsMap, volatileKey)));

		PersistentVector interfaces = PersistentVector.EMPTY;
		for(ISeq s = RT.seq(interfaceSyms);s!=null;s = s.next())
			{
			Class c = (Class) resolve((Symbol) s.first());
			if(!c.isInterface())
				throw new IllegalArgumentException("only interfaces are supported, had: " + c.getName());
			interfaces = interfaces.cons(c);
			}
		Class superClass = Object.class;
		Map[] mc = gatherMethods(superClass,RT.seq(interfaces));
		Map overrideables = mc[0];
		Map covariants = mc[1];
		ret.mmap = overrideables;
		ret.covariants = covariants;
		
		String[] inames = interfaceNames(interfaces);

		Class stub = compileStub(slashname(superClass),ret, inames, frm);
		Symbol thistag = Symbol.intern(null,stub.getName());

		try
			{
			Var.pushThreadBindings(
					RT.mapUniqueKeys(CONSTANTS, PersistentVector.EMPTY,
					       CONSTANT_IDS, new IdentityHashMap(),
					       KEYWORDS, PersistentHashMap.EMPTY,
					       VARS, PersistentHashMap.EMPTY,
					       KEYWORD_CALLSITES, PersistentVector.EMPTY,
					       PROTOCOL_CALLSITES, PersistentVector.EMPTY,
					       VAR_CALLSITES, emptyVarCallSites(),
                                               NO_RECUR, null));
			if(ret.isDeftype())
				{
				Var.pushThreadBindings(RT.mapUniqueKeys(METHOD, null,
				                              LOCAL_ENV, ret.fields
						, COMPILE_STUB_SYM, Symbol.intern(null, tagName)
						, COMPILE_STUB_CLASS, stub));

				ret.hintedFields = RT.subvec(fieldSyms, 0, fieldSyms.count() - ret.altCtorDrops);
				}

			//now (methodname [args] body)*
			ret.line = lineDeref();
			ret.column = columnDeref();
			IPersistentCollection methods = null;
			for(ISeq s = methodForms; s != null; s = RT.next(s))
				{
				NewInstanceMethod m = NewInstanceMethod.parse(ret, (ISeq) RT.first(s),thistag, overrideables);
				methods = RT.conj(methods, m);
				}


			ret.methods = methods;
			ret.keywords = (IPersistentMap) KEYWORDS.deref();
			ret.vars = (IPersistentMap) VARS.deref();
			ret.constants = (PersistentVector) CONSTANTS.deref();
			ret.constantsID = RT.nextID();
			ret.keywordCallsites = (IPersistentVector) KEYWORD_CALLSITES.deref();
			ret.protocolCallsites = (IPersistentVector) PROTOCOL_CALLSITES.deref();
			ret.varCallsites = (IPersistentSet) VAR_CALLSITES.deref();
			}
		finally
			{
			if(ret.isDeftype())
				Var.popThreadBindings();
			Var.popThreadBindings();
			}

		try
			{
			ret.compile(slashname(superClass),inames,false);
			}
		catch(IOException e)
			{
			throw Util.sneakyThrow(e);
			}
		ret.getCompiledClass();
		return ret;
		}

	/***
	 * Current host interop uses reflection, which requires pre-existing classes
	 * Work around this by:
	 * Generate a stub class that has the same interfaces and fields as the class we are generating.
	 * Use it as a type hint for this, and bind the simple name of the class to this stub (in resolve etc)
	 * Unmunge the name (using a magic prefix) on any code gen for classes
	 */
	static Class compileStub(String superName, NewInstanceExpr ret, String[] interfaceNames, Object frm){
		ClassWriter cw = new ClassWriter(ClassWriter.COMPUTE_MAXS);
		ClassVisitor cv = cw;
		cv.visit(V1_5, ACC_PUBLIC + ACC_SUPER, COMPILE_STUB_PREFIX + "/" + ret.internalName,
		         null,superName,interfaceNames);

		//instance fields for closed-overs
		for(ISeq s = RT.keys(ret.closes); s != null; s = s.next())
			{
			LocalBinding lb = (LocalBinding) s.first();
			int access = ACC_PUBLIC + (ret.isVolatile(lb) ? ACC_VOLATILE :
			                           ret.isMutable(lb) ? 0 :
			                           ACC_FINAL);
			if(lb.getPrimitiveType() != null)
				cv.visitField(access
						, lb.name, Type.getType(lb.getPrimitiveType()).getDescriptor(),
							  null, null);
			else
			//todo - when closed-overs are fields, use more specific types here and in ctor and emitLocal?
				cv.visitField(access
						, lb.name, OBJECT_TYPE.getDescriptor(), null, null);
			}

		//ctor that takes closed-overs and does nothing
		Method m = new Method("<init>", Type.VOID_TYPE, ret.ctorTypes());
		GeneratorAdapter ctorgen = new GeneratorAdapter(ACC_PUBLIC,
		                                                m,
		                                                null,
		                                                null,
		                                                cv);
		ctorgen.visitCode();
		ctorgen.loadThis();
		ctorgen.invokeConstructor(Type.getObjectType(superName), voidctor);
		ctorgen.returnValue();
		ctorgen.endMethod();

		if(ret.altCtorDrops > 0)
			{
			Type[] ctorTypes = ret.ctorTypes();
			Type[] altCtorTypes = new Type[ctorTypes.length-ret.altCtorDrops];
			for(int i=0;i<altCtorTypes.length;i++)
				altCtorTypes[i] = ctorTypes[i];
			Method alt = new Method("<init>", Type.VOID_TYPE, altCtorTypes);
			ctorgen = new GeneratorAdapter(ACC_PUBLIC,
															alt,
															null,
															null,
															cv);
			ctorgen.visitCode();
			ctorgen.loadThis();
			ctorgen.loadArgs();
			for(int i=0;i<ret.altCtorDrops;i++)
				ctorgen.visitInsn(Opcodes.ACONST_NULL);

			ctorgen.invokeConstructor(Type.getObjectType(COMPILE_STUB_PREFIX + "/" + ret.internalName),
			                          new Method("<init>", Type.VOID_TYPE, ctorTypes));

			ctorgen.returnValue();
			ctorgen.endMethod();
			}
		//end of class
		cv.visitEnd();

		byte[] bytecode = cw.toByteArray();
		DynamicClassLoader loader = (DynamicClassLoader) LOADER.deref();
		return loader.defineClass(COMPILE_STUB_PREFIX + "." + ret.name, bytecode, frm);
	}

	static String[] interfaceNames(IPersistentVector interfaces){
		int icnt = interfaces.count();
		String[] inames = icnt > 0 ? new String[icnt] : null;
		for(int i=0;i<icnt;i++)
			inames[i] = slashname((Class) interfaces.nth(i));
		return inames;
	}


	static String slashname(Class c){
		return c.getName().replace('.', '/');
	}

	protected void emitStatics(ClassVisitor cv) {
		if(this.isDeftype())
			{
			//getBasis()
			Method meth = Method.getMethod("clojure.lang.IPersistentVector getBasis()");
			GeneratorAdapter gen = new GeneratorAdapter(ACC_PUBLIC + ACC_STATIC,
												meth,
												null,
												null,
												cv);
			emitValue(hintedFields, gen);
			gen.returnValue();
			gen.endMethod();

			if (this.isDeftype() && this.fields.count() > this.hintedFields.count())
				{
				//create(IPersistentMap)
				String className = name.replace('.', '/');
				int i = 1;
				int fieldCount = hintedFields.count();

				MethodVisitor mv = cv.visitMethod(ACC_PUBLIC + ACC_STATIC, "create", "(Lclojure/lang/IPersistentMap;)L"+className+";", null, null);
				mv.visitCode();

				for(ISeq s = RT.seq(hintedFields); s!=null; s=s.next(), i++)
					{
					String bName = ((Symbol)s.first()).name;
					Class k = tagClass(tagOf(s.first()));

					mv.visitVarInsn(ALOAD, 0);
					mv.visitLdcInsn(bName);
					mv.visitMethodInsn(INVOKESTATIC, "clojure/lang/Keyword", "intern", "(Ljava/lang/String;)Lclojure/lang/Keyword;");
					mv.visitInsn(ACONST_NULL);
					mv.visitMethodInsn(INVOKEINTERFACE, "clojure/lang/IPersistentMap", "valAt", "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;");
					if(k.isPrimitive())
						{
						mv.visitTypeInsn(CHECKCAST, Type.getType(boxClass(k)).getInternalName());
						}
					mv.visitVarInsn(ASTORE, i);
					mv.visitVarInsn(ALOAD, 0);
					mv.visitLdcInsn(bName);
					mv.visitMethodInsn(INVOKESTATIC, "clojure/lang/Keyword", "intern", "(Ljava/lang/String;)Lclojure/lang/Keyword;");
					mv.visitMethodInsn(INVOKEINTERFACE, "clojure/lang/IPersistentMap", "without", "(Ljava/lang/Object;)Lclojure/lang/IPersistentMap;");
					mv.visitVarInsn(ASTORE, 0);
					}

				mv.visitTypeInsn(Opcodes.NEW, className);
				mv.visitInsn(DUP);

				Method ctor = new Method("<init>", Type.VOID_TYPE, ctorTypes());

				if(hintedFields.count() > 0)
					for(i=1; i<=fieldCount; i++)
						{
						mv.visitVarInsn(ALOAD, i);
						Class k = tagClass(tagOf(hintedFields.nth(i-1)));
						if(k.isPrimitive())
							{
							String b = Type.getType(boxClass(k)).getInternalName();
							String p = Type.getType(k).getDescriptor();
							String n = k.getName();

							mv.visitMethodInsn(INVOKEVIRTUAL, b, n+"Value", "()"+p);
							}
						}

				mv.visitInsn(ACONST_NULL);
				mv.visitVarInsn(ALOAD, 0);
				mv.visitMethodInsn(INVOKESTATIC, "clojure/lang/RT", "seqOrElse", "(Ljava/lang/Object;)Ljava/lang/Object;");
				mv.visitMethodInsn(INVOKESPECIAL, className, "<init>", ctor.getDescriptor());
				mv.visitInsn(ARETURN);
				mv.visitMaxs(4+fieldCount, 1+fieldCount);
				mv.visitEnd();
				}
			}
	}

	protected void emitMethods(ClassVisitor cv){
		for(ISeq s = RT.seq(methods); s != null; s = s.next())
			{
			ObjMethod method = (ObjMethod) s.first();
			method.emit(this, cv);
			}
		//emit bridge methods
		for(Map.Entry<IPersistentVector,Set<Class>> e : covariants.entrySet())
			{
			java.lang.reflect.Method m = mmap.get(e.getKey());
			Class[] params = m.getParameterTypes();
			Type[] argTypes = new Type[params.length];

			for(int i = 0; i < params.length; i++)
				{
				argTypes[i] = Type.getType(params[i]);
				}

			Method target = new Method(m.getName(), Type.getType(m.getReturnType()), argTypes);

			for(Class retType : e.getValue())
				{
 		        Method meth = new Method(m.getName(), Type.getType(retType), argTypes);

				GeneratorAdapter gen = new GeneratorAdapter(ACC_PUBLIC + ACC_BRIDGE,
		                                            meth,
		                                            null,
		                                            //todo don't hardwire this
		                                            EXCEPTION_TYPES,
		                                            cv);
				gen.visitCode();
				gen.loadThis();
				gen.loadArgs();
				gen.invokeInterface(Type.getType(m.getDeclaringClass()),target);
				gen.returnValue();
				gen.endMethod();
				}
			}
	}

	static public IPersistentVector msig(java.lang.reflect.Method m){
		return RT.vector(m.getName(), RT.seq(m.getParameterTypes()),m.getReturnType());
	}

	static void considerMethod(java.lang.reflect.Method m, Map mm){
		IPersistentVector mk = msig(m);
		int mods = m.getModifiers();

		if(!(mm.containsKey(mk)
		    || !(Modifier.isPublic(mods) || Modifier.isProtected(mods))
		    || Modifier.isStatic(mods)
		    || Modifier.isFinal(mods)))
			{
				mm.put(mk, m);
			}
	}

	static void gatherMethods(Class c, Map mm){
		for(; c != null; c = c.getSuperclass())
			{
			for(java.lang.reflect.Method m : c.getDeclaredMethods())
				considerMethod(m, mm);
			for(java.lang.reflect.Method m : c.getMethods())
				considerMethod(m, mm);
			}
	}

	static public Map[] gatherMethods(Class sc, ISeq interfaces){
		Map allm = new HashMap();
		gatherMethods(sc, allm);
		for(; interfaces != null; interfaces = interfaces.next())
			gatherMethods((Class) interfaces.first(), allm);

		Map<IPersistentVector,java.lang.reflect.Method> mm = new HashMap<IPersistentVector,java.lang.reflect.Method>();
		Map<IPersistentVector,Set<Class>> covariants = new HashMap<IPersistentVector,Set<Class>>();
		for(Object o : allm.entrySet())
			{
			Map.Entry e = (Map.Entry) o;
			IPersistentVector mk = (IPersistentVector) e.getKey();
			mk = (IPersistentVector) mk.pop();
			java.lang.reflect.Method m = (java.lang.reflect.Method) e.getValue();
			if(mm.containsKey(mk)) //covariant return
				{
				Set<Class> cvs = covariants.get(mk);
				if(cvs == null)
					{
					cvs = new HashSet<Class>();
					covariants.put(mk,cvs);
					}
				java.lang.reflect.Method om = mm.get(mk);
				if(om.getReturnType().isAssignableFrom(m.getReturnType()))
					{
					cvs.add(om.getReturnType());
					mm.put(mk, m);
					}
				else
					cvs.add(m.getReturnType());
				}
			else
				mm.put(mk, m);
			}
		return new Map[]{mm,covariants};
	}
}

public static class NewInstanceMethod extends ObjMethod{
	String name;
	Type[] argTypes;
	Type retType;
	Class retClass;
	Class[] exclasses;

	static Symbol dummyThis = Symbol.intern(null,"dummy_this_dlskjsdfower");
	private IPersistentVector parms;

	public NewInstanceMethod(ObjExpr objx, ObjMethod parent){
		super(objx, parent);
	}

	int numParams(){
		return argLocals.count();
	}

	String getMethodName(){
		return name;
	}

	Type getReturnType(){
		return retType;
	}

	Type[] getArgTypes(){
		return argTypes;
	}



	static public IPersistentVector msig(String name,Class[] paramTypes){
		return RT.vector(name,RT.seq(paramTypes));
	}

	static NewInstanceMethod parse(ObjExpr objx, ISeq form, Symbol thistag,
	                               Map overrideables) {
		//(methodname [this-name args*] body...)
		//this-name might be nil
		NewInstanceMethod method = new NewInstanceMethod(objx, (ObjMethod) METHOD.deref());
		Symbol dotname = (Symbol)RT.first(form);
		Symbol name = (Symbol) Symbol.intern(null,munge(dotname.name)).withMeta(RT.meta(dotname));
		IPersistentVector parms = (IPersistentVector) RT.second(form);
		if(parms.count() == 0)
			{
			throw new IllegalArgumentException("Must supply at least one argument for 'this' in: " + dotname);
			}
		Symbol thisName = (Symbol) parms.nth(0);
		parms = RT.subvec(parms,1,parms.count());
		ISeq body = RT.next(RT.next(form));
		try
			{
			method.line = lineDeref();
			method.column = columnDeref();
			//register as the current method and set up a new env frame
            PathNode pnode =  new PathNode(PATHTYPE.PATH, (PathNode) CLEAR_PATH.get());
			Var.pushThreadBindings(
					RT.mapUniqueKeys(
							METHOD, method,
							LOCAL_ENV, LOCAL_ENV.deref(),
							LOOP_LOCALS, null,
							NEXT_LOCAL_NUM, 0
                            ,CLEAR_PATH, pnode
                            ,CLEAR_ROOT, pnode
                            ,CLEAR_SITES, PersistentHashMap.EMPTY
                    ));

			//register 'this' as local 0
			if(thisName != null)
				registerLocal((thisName == null) ? dummyThis:thisName,thistag, null,false);
			else
				getAndIncLocalNum();

			PersistentVector argLocals = PersistentVector.EMPTY;
			method.retClass = tagClass(tagOf(name));
			method.argTypes = new Type[parms.count()];
			boolean hinted = tagOf(name) != null;
			Class[] pclasses = new Class[parms.count()];
			Symbol[] psyms = new Symbol[parms.count()];

			for(int i = 0; i < parms.count(); i++)
				{
				if(!(parms.nth(i) instanceof Symbol))
					throw new IllegalArgumentException("params must be Symbols");
				Symbol p = (Symbol) parms.nth(i);
				Object tag = tagOf(p);
				if(tag != null)
					hinted = true;
				if(p.getNamespace() != null)
					p = Symbol.intern(p.name);
				Class pclass = tagClass(tag);
				pclasses[i] = pclass;
				psyms[i] = p;
				}
			Map matches = findMethodsWithNameAndArity(name.name, parms.count(), overrideables);
			Object mk = msig(name.name, pclasses);
			java.lang.reflect.Method m = null;
			if(matches.size() > 0)
				{
				//multiple methods
				if(matches.size() > 1)
					{
					//must be hinted and match one method
					if(!hinted)
						throw new IllegalArgumentException("Must hint overloaded method: " + name.name);
					m = (java.lang.reflect.Method) matches.get(mk);
					if(m == null)
						throw new IllegalArgumentException("Can't find matching overloaded method: " + name.name);
					if(m.getReturnType() != method.retClass)
						throw new IllegalArgumentException("Mismatched return type: " + name.name +
						", expected: " + m.getReturnType().getName()  + ", had: " + method.retClass.getName());
					}
				else  //one match
					{
					//if hinted, validate match,
					if(hinted)
						{
						m = (java.lang.reflect.Method) matches.get(mk);
						if(m == null)
							throw new IllegalArgumentException("Can't find matching method: " + name.name +
							                                   ", leave off hints for auto match.");
						if(m.getReturnType() != method.retClass)
							throw new IllegalArgumentException("Mismatched return type: " + name.name +
							", expected: " + m.getReturnType().getName()  + ", had: " + method.retClass.getName());
						}
					else //adopt found method sig
						{
						m = (java.lang.reflect.Method) matches.values().iterator().next();
						method.retClass = m.getReturnType();
						pclasses = m.getParameterTypes();
						}
					}
				}
//			else if(findMethodsWithName(name.name,allmethods).size()>0)
//				throw new IllegalArgumentException("Can't override/overload method: " + name.name);
			else
				throw new IllegalArgumentException("Can't define method not in interfaces: " + name.name);

			//else
				//validate unque name+arity among additional methods

			method.retType = Type.getType(method.retClass);
			method.exclasses = m.getExceptionTypes();

			for(int i = 0; i < parms.count(); i++)
				{
				LocalBinding lb = registerLocal(psyms[i], null, new MethodParamExpr(pclasses[i]),true);
				argLocals = argLocals.assocN(i,lb);
				method.argTypes[i] = Type.getType(pclasses[i]);
				}
			for(int i = 0; i < parms.count(); i++)
				{
				if(pclasses[i] == long.class || pclasses[i] == double.class)
					getAndIncLocalNum();
				}
			LOOP_LOCALS.set(argLocals);
			method.name = name.name;
			method.methodMeta = RT.meta(name);
			method.parms = parms;
			method.argLocals = argLocals;
			method.body = (new BodyExpr.Parser()).parse(C.RETURN, body);
			return method;
			}
		finally
			{
			Var.popThreadBindings();
			}
	}

	private static Map findMethodsWithNameAndArity(String name, int arity, Map mm){
		Map ret = new HashMap();
		for(Object o : mm.entrySet())
			{
			Map.Entry e = (Map.Entry) o;
			java.lang.reflect.Method m = (java.lang.reflect.Method) e.getValue();
			if(name.equals(m.getName()) && m.getParameterTypes().length == arity)
				ret.put(e.getKey(), e.getValue());
			}
		return ret;
	}

	private static Map findMethodsWithName(String name, Map mm){
		Map ret = new HashMap();
		for(Object o : mm.entrySet())
			{
			Map.Entry e = (Map.Entry) o;
			java.lang.reflect.Method m = (java.lang.reflect.Method) e.getValue();
			if(name.equals(m.getName()))
				ret.put(e.getKey(), e.getValue());
			}
		return ret;
	}

	public void emit(ObjExpr obj, ClassVisitor cv){
		Method m = new Method(getMethodName(), getReturnType(), getArgTypes());

		Type[] extypes = null;
		if(exclasses.length > 0)
			{
			extypes = new Type[exclasses.length];
			for(int i=0;i<exclasses.length;i++)
				extypes[i] = Type.getType(exclasses[i]);
			}
		GeneratorAdapter gen = new GeneratorAdapter(ACC_PUBLIC,
		                                            m,
		                                            null,
		                                            extypes,
		                                            cv);
		addAnnotation(gen,methodMeta);
		for(int i = 0; i < parms.count(); i++)
			{
			IPersistentMap meta = RT.meta(parms.nth(i));
			addParameterAnnotation(gen, meta, i);
			}
		gen.visitCode();

		Label loopLabel = gen.mark();

		gen.visitLineNumber(line, loopLabel);
		try
			{
			Var.pushThreadBindings(RT.map(LOOP_LABEL, loopLabel, METHOD, this));

			emitBody(objx, gen, retClass, body);
			Label end = gen.mark();
			gen.visitLocalVariable("this", obj.objtype.getDescriptor(), null, loopLabel, end, 0);
			for(ISeq lbs = argLocals.seq(); lbs != null; lbs = lbs.next())
				{
				LocalBinding lb = (LocalBinding) lbs.first();
				gen.visitLocalVariable(lb.name, argTypes[lb.idx-1].getDescriptor(), null, loopLabel, end, lb.idx);
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

	static Class primClass(Symbol sym){
		if(sym == null)
			return null;
		Class c = null;
		if(sym.name.equals("int"))
			c = int.class;
		else if(sym.name.equals("long"))
			c = long.class;
		else if(sym.name.equals("float"))
			c = float.class;
		else if(sym.name.equals("double"))
			c = double.class;
		else if(sym.name.equals("char"))
			c = char.class;
		else if(sym.name.equals("short"))
			c = short.class;
		else if(sym.name.equals("byte"))
			c = byte.class;
		else if(sym.name.equals("boolean"))
			c = boolean.class;
		else if(sym.name.equals("void"))
			c = void.class;
		return c;
	}

	static Class tagClass(Object tag) {
		if(tag == null)
			return Object.class;
		Class c = null;
		if(tag instanceof Symbol)
			c = primClass((Symbol) tag);
		if(c == null)
			c = HostExpr.tagToClass(tag);
		return c;
	}

	static Class primClass(Class c){
		return c.isPrimitive()?c:Object.class;
	}

	static Class boxClass(Class p) {
		if(!p.isPrimitive())
			return p;

		Class c = null;

		if(p == Integer.TYPE)
			c = Integer.class;
		else if(p == Long.TYPE)
			c = Long.class;
		else if(p == Float.TYPE)
			c = Float.class;
		else if(p == Double.TYPE)
			c = Double.class;
		else if(p == Character.TYPE)
			c = Character.class;
		else if(p == Short.TYPE)
			c = Short.class;
		else if(p == Byte.TYPE)
			c = Byte.class;
		else if(p == Boolean.TYPE)
			c = Boolean.class;

		return c;
	}

static public class MethodParamExpr implements Expr, MaybePrimitiveExpr{
	final Class c;

	public MethodParamExpr(Class c){
		this.c = c;
	}

	public Object eval() {
		throw Util.runtimeException("Can't eval");
	}

	public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
		throw Util.runtimeException("Can't emit");
	}

	public boolean hasJavaClass() {
		return c != null;
	}

	public Class getJavaClass() {
		return c;
	}

	public boolean canEmitPrimitive(){
		return Util.isPrimitive(c);
	}

	public void emitUnboxed(C context, ObjExpr objx, GeneratorAdapter gen){
		throw Util.runtimeException("Can't emit");
	}
}

public static class CaseExpr implements Expr, MaybePrimitiveExpr{
	public final LocalBindingExpr expr;
	public final int shift, mask, low, high;
	public final Expr defaultExpr;
	public final SortedMap<Integer,Expr> tests;
	public final HashMap<Integer,Expr> thens;
	public final Keyword switchType;
	public final Keyword testType;
	public final Set<Integer> skipCheck;
	public final Class returnType;
	public final int line;
	public final int column;

	final static Type NUMBER_TYPE = Type.getType(Number.class);
	final static Method intValueMethod = Method.getMethod("int intValue()");

	final static Method hashMethod = Method.getMethod("int hash(Object)");
	final static Method hashCodeMethod = Method.getMethod("int hashCode()");
	final static Method equivMethod = Method.getMethod("boolean equiv(Object, Object)");
    final static Keyword compactKey = Keyword.intern(null, "compact");
    final static Keyword sparseKey = Keyword.intern(null, "sparse");
    final static Keyword hashIdentityKey = Keyword.intern(null, "hash-identity");
    final static Keyword hashEquivKey = Keyword.intern(null, "hash-equiv");
    final static Keyword intKey = Keyword.intern(null, "int");
	//(case* expr shift mask default map<minhash, [test then]> table-type test-type skip-check?)
	public CaseExpr(int line, int column, LocalBindingExpr expr, int shift, int mask, int low, int high, Expr defaultExpr,
	        SortedMap<Integer,Expr> tests,HashMap<Integer,Expr> thens, Keyword switchType, Keyword testType, Set<Integer> skipCheck){
		this.expr = expr;
		this.shift = shift;
		this.mask = mask;
		this.low = low;
		this.high = high;
		this.defaultExpr = defaultExpr;
		this.tests = tests;
		this.thens = thens;
		this.line = line;
		this.column = column;
		if (switchType != compactKey && switchType != sparseKey)
		    throw new IllegalArgumentException("Unexpected switch type: "+switchType);
		this.switchType = switchType;
        if (testType != intKey && testType != hashEquivKey && testType != hashIdentityKey)
            throw new IllegalArgumentException("Unexpected test type: "+switchType);
		this.testType = testType;
		this.skipCheck = skipCheck;
		Collection<Expr> returns = new ArrayList(thens.values());
		returns.add(defaultExpr);
		this.returnType = maybeJavaClass(returns);
        if(RT.count(skipCheck) > 0 && RT.booleanCast(RT.WARN_ON_REFLECTION.deref()))
            {
            RT.errPrintWriter()
              .format("Performance warning, %s:%d:%d - hash collision of some case test constants; if selected, those entries will be tested sequentially.\n",
                      SOURCE_PATH.deref(), line, column);
            }
	}

	public boolean hasJavaClass(){
	    return returnType != null;
	}

	public boolean canEmitPrimitive(){
	return Util.isPrimitive(returnType);
	}

	public Class getJavaClass(){
	    return returnType;
	}

	public Object eval() {
		throw new UnsupportedOperationException("Can't eval case");
	}

    public void emit(C context, ObjExpr objx, GeneratorAdapter gen){
        doEmit(context, objx, gen, false);
    }

    public void emitUnboxed(C context, ObjExpr objx, GeneratorAdapter gen){
        doEmit(context, objx, gen, true);
    }

	public void doEmit(C context, ObjExpr objx, GeneratorAdapter gen, boolean emitUnboxed){
		Label defaultLabel = gen.newLabel();
		Label endLabel = gen.newLabel();
		SortedMap<Integer,Label> labels = new TreeMap();

		for(Integer i : tests.keySet())
			{
			labels.put(i, gen.newLabel());
			}

        gen.visitLineNumber(line, gen.mark());

        Class primExprClass = maybePrimitiveType(expr);
        Type primExprType = primExprClass == null ? null : Type.getType(primExprClass);

        if (testType == intKey)
		    emitExprForInts(objx, gen, primExprType, defaultLabel);
        else
            emitExprForHashes(objx, gen);

        if (switchType == sparseKey)
            {
            Label[] la = new Label[labels.size()];
            la = labels.values().toArray(la);
            int[] ints = Numbers.int_array(tests.keySet());
            gen.visitLookupSwitchInsn(defaultLabel, ints, la);
            }
        else
            {
            Label[] la = new Label[(high-low)+1];
            for(int i=low;i<=high;i++)
                {
                la[i-low] = labels.containsKey(i) ? labels.get(i) : defaultLabel;
                }
            gen.visitTableSwitchInsn(low, high, defaultLabel, la);
            }

		for(Integer i : labels.keySet())
			{
			gen.mark(labels.get(i));
			if (testType == intKey)
			    emitThenForInts(objx, gen, primExprType, tests.get(i), thens.get(i), defaultLabel, emitUnboxed);
			else if (RT.contains(skipCheck, i) == RT.T)
			    emitExpr(objx, gen, thens.get(i), emitUnboxed);
			else
			    emitThenForHashes(objx, gen, tests.get(i), thens.get(i), defaultLabel, emitUnboxed);
			gen.goTo(endLabel);
			}

		gen.mark(defaultLabel);
		emitExpr(objx, gen, defaultExpr, emitUnboxed);
		gen.mark(endLabel);
		if(context == C.STATEMENT)
			gen.pop();
	}

	private boolean isShiftMasked(){
	    return  mask != 0;
	}

	private void emitShiftMask(GeneratorAdapter gen){
	    if (isShiftMasked())
	        {
            gen.push(shift);
            gen.visitInsn(ISHR);
            gen.push(mask);
            gen.visitInsn(IAND);
	        }
	}

    private void emitExprForInts(ObjExpr objx, GeneratorAdapter gen, Type exprType, Label defaultLabel){
        if (exprType == null)
            {
            if(RT.booleanCast(RT.WARN_ON_REFLECTION.deref()))
                {
                RT.errPrintWriter()
                  .format("Performance warning, %s:%d:%d - case has int tests, but tested expression is not primitive.\n",
                          SOURCE_PATH.deref(), line, column);
                }
            expr.emit(C.EXPRESSION, objx, gen);
            gen.instanceOf(NUMBER_TYPE);
            gen.ifZCmp(GeneratorAdapter.EQ, defaultLabel);
            expr.emit(C.EXPRESSION, objx, gen);
            gen.checkCast(NUMBER_TYPE);
            gen.invokeVirtual(NUMBER_TYPE, intValueMethod);
            emitShiftMask(gen);
            }
        else if (exprType == Type.LONG_TYPE
                || exprType == Type.INT_TYPE
                || exprType == Type.SHORT_TYPE
                || exprType == Type.BYTE_TYPE)
            {
            expr.emitUnboxed(C.EXPRESSION, objx, gen);
            gen.cast(exprType, Type.INT_TYPE);
            emitShiftMask(gen);
            }
        else
            {
            gen.goTo(defaultLabel);
            }
    }

    private void emitThenForInts(ObjExpr objx, GeneratorAdapter gen, Type exprType, Expr test, Expr then, Label defaultLabel, boolean emitUnboxed){
        if (exprType == null)
            {
            expr.emit(C.EXPRESSION, objx, gen);
            test.emit(C.EXPRESSION, objx, gen);
            gen.invokeStatic(UTIL_TYPE, equivMethod);
            gen.ifZCmp(GeneratorAdapter.EQ, defaultLabel);
            emitExpr(objx, gen, then, emitUnboxed);
            }
        else if (exprType == Type.LONG_TYPE)
            {
            ((NumberExpr)test).emitUnboxed(C.EXPRESSION, objx, gen);
            expr.emitUnboxed(C.EXPRESSION, objx, gen);
            gen.ifCmp(Type.LONG_TYPE, GeneratorAdapter.NE, defaultLabel);
            emitExpr(objx, gen, then, emitUnboxed);
            }
        else if (exprType == Type.INT_TYPE
                || exprType == Type.SHORT_TYPE
                || exprType == Type.BYTE_TYPE)
            {
            if (isShiftMasked())
                {
                ((NumberExpr)test).emitUnboxed(C.EXPRESSION, objx, gen);
                expr.emitUnboxed(C.EXPRESSION, objx, gen);
                gen.cast(exprType, Type.LONG_TYPE);
                gen.ifCmp(Type.LONG_TYPE, GeneratorAdapter.NE, defaultLabel);
                }
            // else direct match
            emitExpr(objx, gen, then, emitUnboxed);
            }
        else
            {
            gen.goTo(defaultLabel);
            }
    }

    private void emitExprForHashes(ObjExpr objx, GeneratorAdapter gen){
        expr.emit(C.EXPRESSION, objx, gen);
        gen.invokeStatic(UTIL_TYPE,hashMethod);
        emitShiftMask(gen);
    }

    private void emitThenForHashes(ObjExpr objx, GeneratorAdapter gen, Expr test, Expr then, Label defaultLabel, boolean emitUnboxed){
        expr.emit(C.EXPRESSION, objx, gen);
        test.emit(C.EXPRESSION, objx, gen);
        if(testType == hashIdentityKey)
            {
            gen.visitJumpInsn(IF_ACMPNE, defaultLabel);
            }
        else
            {
            gen.invokeStatic(UTIL_TYPE, equivMethod);
            gen.ifZCmp(GeneratorAdapter.EQ, defaultLabel);
            }
        emitExpr(objx, gen, then, emitUnboxed);
    }

    private static void emitExpr(ObjExpr objx, GeneratorAdapter gen, Expr expr, boolean emitUnboxed){
        if (emitUnboxed && expr instanceof MaybePrimitiveExpr)
            ((MaybePrimitiveExpr)expr).emitUnboxed(C.EXPRESSION,objx,gen);
        else
            expr.emit(C.EXPRESSION,objx,gen);
    }


	static class Parser implements IParser{
		//(case* expr shift mask default map<minhash, [test then]> table-type test-type skip-check?)
		//prepared by case macro and presumed correct
		//case macro binds actual expr in let so expr is always a local,
		//no need to worry about multiple evaluation
		public Expr parse(C context, Object frm) {
			ISeq form = (ISeq) frm;
			if(context == C.EVAL)
				return analyze(context, RT.list(RT.list(FNONCE, PersistentVector.EMPTY, form)));
			PersistentVector args = PersistentVector.create(form.next());

			Object exprForm = args.nth(0);
			int shift = ((Number)args.nth(1)).intValue();
			int mask = ((Number)args.nth(2)).intValue();
			Object defaultForm = args.nth(3);
			Map caseMap = (Map)args.nth(4);
			Keyword switchType = ((Keyword)args.nth(5));
			Keyword testType = ((Keyword)args.nth(6));
			Set skipCheck = RT.count(args) < 8 ? null : (Set)args.nth(7);

            ISeq keys = RT.keys(caseMap);
            int low = ((Number)RT.first(keys)).intValue();
            int high = ((Number)RT.nth(keys, RT.count(keys)-1)).intValue();

            LocalBindingExpr testexpr = (LocalBindingExpr) analyze(C.EXPRESSION, exprForm);
			testexpr.shouldClear = false;

            SortedMap<Integer,Expr> tests = new TreeMap();
            HashMap<Integer,Expr> thens = new HashMap();

            PathNode branch = new PathNode(PATHTYPE.BRANCH, (PathNode) CLEAR_PATH.get());

			for(Object o : caseMap.entrySet())
				{
				Map.Entry e = (Map.Entry) o;
				Integer minhash = ((Number)e.getKey()).intValue();
                Object pair = e.getValue(); // [test-val then-expr]
                Expr testExpr = testType == intKey
                                    ? NumberExpr.parse(((Number)RT.first(pair)).intValue())
                                    : new ConstantExpr(RT.first(pair));
                tests.put(minhash, testExpr);

                Expr thenExpr;
                try {
                    Var.pushThreadBindings(
                            RT.map(CLEAR_PATH, new PathNode(PATHTYPE.PATH,branch)));
                    thenExpr = analyze(context, RT.second(pair));
                    }
                finally{
                    Var.popThreadBindings();
                    }
				thens.put(minhash, thenExpr);
				}
            
            Expr defaultExpr;
            try {
                Var.pushThreadBindings(
                        RT.map(CLEAR_PATH, new PathNode(PATHTYPE.PATH,branch)));
                defaultExpr = analyze(context, args.nth(3));
                }
            finally{
                Var.popThreadBindings();
                }

            int line = ((Number)LINE.deref()).intValue();
            int column = ((Number)COLUMN.deref()).intValue();
			return new CaseExpr(line, column, testexpr, shift, mask, low, high,
			        defaultExpr, tests, thens, switchType, testType, skipCheck);
		}
	}
}

static IPersistentCollection emptyVarCallSites(){return PersistentHashSet.EMPTY;}

}
