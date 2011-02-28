/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Jul 31, 2007 */

package clojure.lang;

import java.util.concurrent.atomic.AtomicBoolean;


public final class Var extends ARef implements IFn, IRef, Settable{

static class TBox{

volatile Object val;
final Thread thread;

public TBox(Thread t, Object val){
	this.thread = t;
	this.val = val;
}
}

static public class Unbound extends AFn{
	final public Var v;

	public Unbound(Var v){
		this.v = v;
	}

	public String toString(){
		return "Unbound: " + v;
	}

	public Object throwArity(int n){
		throw new IllegalStateException("Attempting to call unbound fn: " + v);
	}
}

static class Frame{
	//Var->TBox
	Associative bindings;
	//Var->val
//	Associative frameBindings;
	Frame prev;


	public Frame(){
		this(PersistentHashMap.EMPTY, null);
	}

	public Frame(Associative bindings, Frame prev){
//		this.frameBindings = frameBindings;
		this.bindings = bindings;
		this.prev = prev;
	}
}

static final ThreadLocal<Frame> dvals = new ThreadLocal<Frame>(){

	protected Frame initialValue(){
		return new Frame();
	}
};

static public volatile int rev = 0;

static Keyword privateKey = Keyword.intern(null, "private");
static IPersistentMap privateMeta = new PersistentArrayMap(new Object[]{privateKey, Boolean.TRUE});
static Keyword macroKey = Keyword.intern(null, "macro");
static Keyword nameKey = Keyword.intern(null, "name");
static Keyword nsKey = Keyword.intern(null, "ns");
//static Keyword tagKey = Keyword.intern(null, "tag");

volatile Object root;

volatile boolean dynamic = false;
transient final AtomicBoolean threadBound;
public final Symbol sym;
public final Namespace ns;

//IPersistentMap _meta;

public static Object getThreadBindingFrame(){
	Frame f = dvals.get();
	if(f != null)
		return f;
	return new Frame();
}

public static void resetThreadBindingFrame(Object frame){
	dvals.set((Frame) frame);
}

public Var setDynamic(){
	this.dynamic = true;
	return this;
}

public Var setDynamic(boolean b){
	this.dynamic = b;
	return this;
}

public final boolean isDynamic(){
	return dynamic;
}

public static Var intern(Namespace ns, Symbol sym, Object root){
	return intern(ns, sym, root, true);
}

public static Var intern(Namespace ns, Symbol sym, Object root, boolean replaceRoot){
	Var dvout = ns.intern(sym);
	if(!dvout.hasRoot() || replaceRoot)
		dvout.bindRoot(root);
	return dvout;
}


public String toString(){
	if(ns != null)
		return "#'" + ns.name + "/" + sym;
	return "#<Var: " + (sym != null ? sym.toString() : "--unnamed--") + ">";
}

public static Var find(Symbol nsQualifiedSym){
	if(nsQualifiedSym.ns == null)
		throw new IllegalArgumentException("Symbol must be namespace-qualified");
	Namespace ns = Namespace.find(Symbol.intern(nsQualifiedSym.ns));
	if(ns == null)
		throw new IllegalArgumentException("No such namespace: " + nsQualifiedSym.ns);
	return ns.findInternedVar(Symbol.intern(nsQualifiedSym.name));
}

public static Var intern(Symbol nsName, Symbol sym){
	Namespace ns = Namespace.findOrCreate(nsName);
	return intern(ns, sym);
}

public static Var internPrivate(String nsName, String sym){
	Namespace ns = Namespace.findOrCreate(Symbol.intern(nsName));
	Var ret = intern(ns, Symbol.intern(sym));
	ret.setMeta(privateMeta);
	return ret;
}

public static Var intern(Namespace ns, Symbol sym){
	return ns.intern(sym);
}


public static Var create(){
	return new Var(null, null);
}

public static Var create(Object root){
	return new Var(null, null, root);
}

Var(Namespace ns, Symbol sym){
	this.ns = ns;
	this.sym = sym;
	this.threadBound = new AtomicBoolean(false);
	this.root = new Unbound(this);
	setMeta(PersistentHashMap.EMPTY);
}

Var(Namespace ns, Symbol sym, Object root){
	this(ns, sym);
	this.root = root;
	++rev;
}

public boolean isBound(){
	return hasRoot() || (threadBound.get() && dvals.get().bindings.containsKey(this));
}

final public Object get(){
	if(!threadBound.get())
		return root;
	return deref();
}

final public Object deref(){
	TBox b = getThreadBinding();
	if(b != null)
		return b.val;
	return root;
}

public void setValidator(IFn vf){
	if(hasRoot())
		validate(vf, root);
	validator = vf;
}

public Object alter(IFn fn, ISeq args) throws Exception{
	set(fn.applyTo(RT.cons(deref(), args)));
	return this;
}

public Object set(Object val){
	validate(getValidator(), val);
	TBox b = getThreadBinding();
	if(b != null)
		{
		if(Thread.currentThread() != b.thread)
			throw new IllegalStateException(String.format("Can't set!: %s from non-binding thread", sym));
		return (b.val = val);
		}
	throw new IllegalStateException(String.format("Can't change/establish root binding of: %s with set", sym));
}

public Object doSet(Object val) throws Exception {
    return set(val);
    }

public Object doReset(Object val) throws Exception {
    bindRoot(val);
    return val;
    }

public void setMeta(IPersistentMap m) {
    //ensure these basis keys
    resetMeta(m.assoc(nameKey, sym).assoc(nsKey, ns));
}

public void setMacro() {
    try
        {
        alterMeta(assoc, RT.list(macroKey, RT.T));
        }
    catch (Exception e)
        {
        throw new RuntimeException(e);
        }
}

public boolean isMacro(){
	return RT.booleanCast(meta().valAt(macroKey));
}

//public void setExported(boolean state){
//	_meta = _meta.assoc(privateKey, state);
//}

public boolean isPublic(){
	return !RT.booleanCast(meta().valAt(privateKey));
}

final public Object getRawRoot(){
		return root;
}

public Object getTag(){
	return meta().valAt(RT.TAG_KEY);
}

public void setTag(Symbol tag) {
    try
        {
        alterMeta(assoc, RT.list(RT.TAG_KEY, tag));
        }
    catch (Exception e)
        {
        throw new RuntimeException(e);
        }
}

final public boolean hasRoot(){
	return !(root instanceof Unbound);
}

//binding root always clears macro flag
synchronized public void bindRoot(Object root){
	validate(getValidator(), root);
	Object oldroot = this.root;
	this.root = root;
	++rev;
    try
        {
        alterMeta(dissoc, RT.list(macroKey));
        }
    catch (Exception e)
        {
        throw new RuntimeException(e);
        }
    notifyWatches(oldroot,this.root);
}

synchronized void swapRoot(Object root){
	validate(getValidator(), root);
	Object oldroot = this.root;
	this.root = root;
	++rev;
    notifyWatches(oldroot,root);
}

synchronized public void unbindRoot(){
	this.root = new Unbound(this);
	++rev;
}

synchronized public void commuteRoot(IFn fn) throws Exception{
	Object newRoot = fn.invoke(root);
	validate(getValidator(), newRoot);
	Object oldroot = root;
	this.root = newRoot;
	++rev;
    notifyWatches(oldroot,newRoot);
}

synchronized public Object alterRoot(IFn fn, ISeq args) throws Exception{
	Object newRoot = fn.applyTo(RT.cons(root, args));
	validate(getValidator(), newRoot);
	Object oldroot = root;
	this.root = newRoot;
	++rev;
    notifyWatches(oldroot,newRoot);
	return newRoot;
}

public static void pushThreadBindings(Associative bindings){
	Frame f = dvals.get();
	Associative bmap = f.bindings;
	for(ISeq bs = bindings.seq(); bs != null; bs = bs.next())
		{
		IMapEntry e = (IMapEntry) bs.first();
		Var v = (Var) e.key();
		if(!v.dynamic)
			throw new IllegalStateException(String.format("Can't dynamically bind non-dynamic var: %s/%s", v.ns, v.sym));
		v.validate(v.getValidator(), e.val());
		v.threadBound.set(true);
		bmap = bmap.assoc(v, new TBox(Thread.currentThread(), e.val()));
		}
	dvals.set(new Frame(bmap, f));
}

public static void popThreadBindings(){
	Frame f = dvals.get();
	if(f.prev == null)
		throw new IllegalStateException("Pop without matching push");
	dvals.set(f.prev);
}

public static Associative getThreadBindings(){
	Frame f = dvals.get();
	IPersistentMap ret = PersistentHashMap.EMPTY;
	for(ISeq bs = f.bindings.seq(); bs != null; bs = bs.next())
		{
		IMapEntry e = (IMapEntry) bs.first();
		Var v = (Var) e.key();
		TBox b = (TBox) e.val();
		ret = ret.assoc(v, b.val);
		}
	return ret;
}

public final TBox getThreadBinding(){
	if(threadBound.get())
		{
		IMapEntry e = dvals.get().bindings.entryAt(this);
		if(e != null)
			return (TBox) e.val();
		}
	return null;
}

final public IFn fn(){
	return (IFn) deref();
}

public Object call() throws Exception{
	return invoke();
}

public void run(){
	try
		{
		invoke();
		}
	catch(Exception e)
		{
		throw new RuntimeException(e);
		}
}

public Object invoke() throws Exception{
	return fn().invoke();
}

public Object invoke(Object arg1) throws Exception{
	return fn().invoke(arg1);
}

public Object invoke(Object arg1, Object arg2) throws Exception{
	return fn().invoke(arg1, arg2);
}

public Object invoke(Object arg1, Object arg2, Object arg3) throws Exception{
	return fn().invoke(arg1, arg2, arg3);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4) throws Exception{
	return fn().invoke(arg1, arg2, arg3, arg4);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5) throws Exception{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6) throws Exception{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7)
		throws Exception{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8) throws Exception{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9) throws Exception{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10) throws Exception{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11) throws Exception{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12) throws Exception{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13)
		throws Exception{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14)
		throws Exception{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15) throws Exception{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16) throws Exception{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15,
	                   arg16);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17) throws Exception{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15,
	                   arg16, arg17);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18) throws Exception{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15,
	                   arg16, arg17, arg18);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19) throws Exception{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15,
	                   arg16, arg17, arg18, arg19);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20)
		throws Exception{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15,
	                   arg16, arg17, arg18, arg19, arg20);
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20,
                     Object... args)
		throws Exception{
	return fn().invoke(arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15,
	                   arg16, arg17, arg18, arg19, arg20, args);
}

public Object applyTo(ISeq arglist) throws Exception{
	return AFn.applyToHelper(this, arglist);
}

static IFn assoc = new AFn(){
    @Override
    public Object invoke(Object m, Object k, Object v) throws Exception {
        return RT.assoc(m, k, v);
    }
};
static IFn dissoc = new AFn() {
    @Override
    public Object invoke(Object c, Object k) throws Exception {
        return RT.dissoc(c, k);
    }
};
}
