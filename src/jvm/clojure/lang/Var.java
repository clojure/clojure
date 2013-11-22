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
	final static Frame TOP = new Frame(PersistentHashMap.EMPTY, null);
	//Var->TBox
	Associative bindings;
	//Var->val
//	Associative frameBindings;
	Frame prev;

	public Frame(Associative bindings, Frame prev){
//		this.frameBindings = frameBindings;
		this.bindings = bindings;
		this.prev = prev;
	}

    	protected Object clone() {
		return new Frame(this.bindings, null);
    	}

}

static final ThreadLocal<Frame> dvals = new ThreadLocal<Frame>(){

	protected Frame initialValue(){
		return Frame.TOP;
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
	return dvals.get();
}

public static Object cloneThreadBindingFrame(){
	return dvals.get().clone();
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

public Object alter(IFn fn, ISeq args) {
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

public Object doSet(Object val)  {
    return set(val);
    }

public Object doReset(Object val)  {
    bindRoot(val);
    return val;
    }

public void setMeta(IPersistentMap m) {
    //ensure these basis keys
    resetMeta(m.assoc(nameKey, sym).assoc(nsKey, ns));
}

public void setMacro() {
    alterMeta(assoc, RT.list(macroKey, RT.T));
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
    alterMeta(assoc, RT.list(RT.TAG_KEY, tag));
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
        alterMeta(dissoc, RT.list(macroKey));
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

synchronized public void commuteRoot(IFn fn) {
	Object newRoot = fn.invoke(root);
	validate(getValidator(), newRoot);
	Object oldroot = root;
	this.root = newRoot;
	++rev;
    notifyWatches(oldroot,newRoot);
}

synchronized public Object alterRoot(IFn fn, ISeq args) {
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
    Frame f = dvals.get().prev;
    if (f == null) {
        throw new IllegalStateException("Pop without matching push");
    } else if (f == Frame.TOP) {
        dvals.remove();
    } else {
        dvals.set(f);
    }
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

public Object call() {
	return invoke();
}

public void run(){
        invoke();
}

public Object invoke() {
	return fn().invoke();
}

public Object invoke(Object arg1) {
    return fn().invoke(Util.ret1(arg1,arg1=null));
}

public Object invoke(Object arg1, Object arg2) {
    return fn().invoke(Util.ret1(arg1,arg1=null),
                       Util.ret1(arg2,arg2=null));
}

public Object invoke(Object arg1, Object arg2, Object arg3) {
    return fn().invoke(Util.ret1(arg1,arg1=null),
                       Util.ret1(arg2,arg2=null),
                       Util.ret1(arg3,arg3=null));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4) {
    return fn().invoke(Util.ret1(arg1,arg1=null),
                       Util.ret1(arg2,arg2=null),
                       Util.ret1(arg3,arg3=null),
                       Util.ret1(arg4,arg4=null));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5) {
    return fn().invoke(Util.ret1(arg1,arg1=null),
                       Util.ret1(arg2,arg2=null),
                       Util.ret1(arg3,arg3=null),
                       Util.ret1(arg4,arg4=null),
                       Util.ret1(arg5,arg5=null));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6) {
    return fn().invoke(Util.ret1(arg1,arg1=null),
                       Util.ret1(arg2,arg2=null),
                       Util.ret1(arg3,arg3=null),
                       Util.ret1(arg4,arg4=null),
                       Util.ret1(arg5,arg5=null),
                       Util.ret1(arg6,arg6=null));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7)
		{
    return fn().invoke(Util.ret1(arg1,arg1=null),
                       Util.ret1(arg2,arg2=null),
                       Util.ret1(arg3,arg3=null),
                       Util.ret1(arg4,arg4=null),
                       Util.ret1(arg5,arg5=null),
                       Util.ret1(arg6,arg6=null),
                       Util.ret1(arg7,arg7=null));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8) {
    return fn().invoke(Util.ret1(arg1,arg1=null),
                       Util.ret1(arg2,arg2=null),
                       Util.ret1(arg3,arg3=null),
                       Util.ret1(arg4,arg4=null),
                       Util.ret1(arg5,arg5=null),
                       Util.ret1(arg6,arg6=null),
                       Util.ret1(arg7,arg7=null),
                       Util.ret1(arg8,arg8=null));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9) {
    return fn().invoke(Util.ret1(arg1,arg1=null),
                       Util.ret1(arg2,arg2=null),
                       Util.ret1(arg3,arg3=null),
                       Util.ret1(arg4,arg4=null),
                       Util.ret1(arg5,arg5=null),
                       Util.ret1(arg6,arg6=null),
                       Util.ret1(arg7,arg7=null),
                       Util.ret1(arg8,arg8=null),
                       Util.ret1(arg9,arg9=null));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10) {
    return fn().invoke(Util.ret1(arg1,arg1=null),
                       Util.ret1(arg2,arg2=null),
                       Util.ret1(arg3,arg3=null),
                       Util.ret1(arg4,arg4=null),
                       Util.ret1(arg5,arg5=null),
                       Util.ret1(arg6,arg6=null),
                       Util.ret1(arg7,arg7=null),
                       Util.ret1(arg8,arg8=null),
                       Util.ret1(arg9,arg9=null),
                       Util.ret1(arg10,arg10=null));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11) {
    return fn().invoke(Util.ret1(arg1,arg1=null),
                       Util.ret1(arg2,arg2=null),
                       Util.ret1(arg3,arg3=null),
                       Util.ret1(arg4,arg4=null),
                       Util.ret1(arg5,arg5=null),
                       Util.ret1(arg6,arg6=null),
                       Util.ret1(arg7,arg7=null),
                       Util.ret1(arg8,arg8=null),
                       Util.ret1(arg9,arg9=null),
                       Util.ret1(arg10,arg10=null),
                       Util.ret1(arg11,arg11=null));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12) {
    return fn().invoke(Util.ret1(arg1,arg1=null),
                       Util.ret1(arg2,arg2=null),
                       Util.ret1(arg3,arg3=null),
                       Util.ret1(arg4,arg4=null),
                       Util.ret1(arg5,arg5=null),
                       Util.ret1(arg6,arg6=null),
                       Util.ret1(arg7,arg7=null),
                       Util.ret1(arg8,arg8=null),
                       Util.ret1(arg9,arg9=null),
                       Util.ret1(arg10,arg10=null),
                       Util.ret1(arg11,arg11=null),
                       Util.ret1(arg12,arg12=null));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13)
		{
    return fn().invoke(Util.ret1(arg1,arg1=null),
                       Util.ret1(arg2,arg2=null),
                       Util.ret1(arg3,arg3=null),
                       Util.ret1(arg4,arg4=null),
                       Util.ret1(arg5,arg5=null),
                       Util.ret1(arg6,arg6=null),
                       Util.ret1(arg7,arg7=null),
                       Util.ret1(arg8,arg8=null),
                       Util.ret1(arg9,arg9=null),
                       Util.ret1(arg10,arg10=null),
                       Util.ret1(arg11,arg11=null),
                       Util.ret1(arg12,arg12=null),
                       Util.ret1(arg13,arg13=null));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14)
		{
    return fn().invoke(Util.ret1(arg1,arg1=null),
                       Util.ret1(arg2,arg2=null),
                       Util.ret1(arg3,arg3=null),
                       Util.ret1(arg4,arg4=null),
                       Util.ret1(arg5,arg5=null),
                       Util.ret1(arg6,arg6=null),
                       Util.ret1(arg7,arg7=null),
                       Util.ret1(arg8,arg8=null),
                       Util.ret1(arg9,arg9=null),
                       Util.ret1(arg10,arg10=null),
                       Util.ret1(arg11,arg11=null),
                       Util.ret1(arg12,arg12=null),
                       Util.ret1(arg13,arg13=null),
                       Util.ret1(arg14,arg14=null));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15) {
    return fn().invoke(Util.ret1(arg1,arg1=null),
                       Util.ret1(arg2,arg2=null),
                       Util.ret1(arg3,arg3=null),
                       Util.ret1(arg4,arg4=null),
                       Util.ret1(arg5,arg5=null),
                       Util.ret1(arg6,arg6=null),
                       Util.ret1(arg7,arg7=null),
                       Util.ret1(arg8,arg8=null),
                       Util.ret1(arg9,arg9=null),
                       Util.ret1(arg10,arg10=null),
                       Util.ret1(arg11,arg11=null),
                       Util.ret1(arg12,arg12=null),
                       Util.ret1(arg13,arg13=null),
                       Util.ret1(arg14,arg14=null),
                       Util.ret1(arg15,arg15=null));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16) {
    return fn().invoke(Util.ret1(arg1,arg1=null),
                       Util.ret1(arg2,arg2=null),
                       Util.ret1(arg3,arg3=null),
                       Util.ret1(arg4,arg4=null),
                       Util.ret1(arg5,arg5=null),
                       Util.ret1(arg6,arg6=null),
                       Util.ret1(arg7,arg7=null),
                       Util.ret1(arg8,arg8=null),
                       Util.ret1(arg9,arg9=null),
                       Util.ret1(arg10,arg10=null),
                       Util.ret1(arg11,arg11=null),
                       Util.ret1(arg12,arg12=null),
                       Util.ret1(arg13,arg13=null),
                       Util.ret1(arg14,arg14=null),
                       Util.ret1(arg15,arg15=null),
                       Util.ret1(arg16,arg16=null));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17) {
    return fn().invoke(Util.ret1(arg1,arg1=null),
                       Util.ret1(arg2,arg2=null),
                       Util.ret1(arg3,arg3=null),
                       Util.ret1(arg4,arg4=null),
                       Util.ret1(arg5,arg5=null),
                       Util.ret1(arg6,arg6=null),
                       Util.ret1(arg7,arg7=null),
                       Util.ret1(arg8,arg8=null),
                       Util.ret1(arg9,arg9=null),
                       Util.ret1(arg10,arg10=null),
                       Util.ret1(arg11,arg11=null),
                       Util.ret1(arg12,arg12=null),
                       Util.ret1(arg13,arg13=null),
                       Util.ret1(arg14,arg14=null),
                       Util.ret1(arg15,arg15=null),
                       Util.ret1(arg16,arg16=null),
                       Util.ret1(arg17,arg17=null));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18) {
    return fn().invoke(Util.ret1(arg1,arg1=null),
                       Util.ret1(arg2,arg2=null),
                       Util.ret1(arg3,arg3=null),
                       Util.ret1(arg4,arg4=null),
                       Util.ret1(arg5,arg5=null),
                       Util.ret1(arg6,arg6=null),
                       Util.ret1(arg7,arg7=null),
                       Util.ret1(arg8,arg8=null),
                       Util.ret1(arg9,arg9=null),
                       Util.ret1(arg10,arg10=null),
                       Util.ret1(arg11,arg11=null),
                       Util.ret1(arg12,arg12=null),
                       Util.ret1(arg13,arg13=null),
                       Util.ret1(arg14,arg14=null),
                       Util.ret1(arg15,arg15=null),
                       Util.ret1(arg16,arg16=null),
                       Util.ret1(arg17,arg17=null),
                       Util.ret1(arg18,arg18=null));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19) {
    return fn().invoke(Util.ret1(arg1,arg1=null),
                       Util.ret1(arg2,arg2=null),
                       Util.ret1(arg3,arg3=null),
                       Util.ret1(arg4,arg4=null),
                       Util.ret1(arg5,arg5=null),
                       Util.ret1(arg6,arg6=null),
                       Util.ret1(arg7,arg7=null),
                       Util.ret1(arg8,arg8=null),
                       Util.ret1(arg9,arg9=null),
                       Util.ret1(arg10,arg10=null),
                       Util.ret1(arg11,arg11=null),
                       Util.ret1(arg12,arg12=null),
                       Util.ret1(arg13,arg13=null),
                       Util.ret1(arg14,arg14=null),
                       Util.ret1(arg15,arg15=null),
                       Util.ret1(arg16,arg16=null),
                       Util.ret1(arg17,arg17=null),
                       Util.ret1(arg18,arg18=null),
                       Util.ret1(arg19,arg19=null));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20)
		{
    return fn().invoke(Util.ret1(arg1,arg1=null),
                       Util.ret1(arg2,arg2=null),
                       Util.ret1(arg3,arg3=null),
                       Util.ret1(arg4,arg4=null),
                       Util.ret1(arg5,arg5=null),
                       Util.ret1(arg6,arg6=null),
                       Util.ret1(arg7,arg7=null),
                       Util.ret1(arg8,arg8=null),
                       Util.ret1(arg9,arg9=null),
                       Util.ret1(arg10,arg10=null),
                       Util.ret1(arg11,arg11=null),
                       Util.ret1(arg12,arg12=null),
                       Util.ret1(arg13,arg13=null),
                       Util.ret1(arg14,arg14=null),
                       Util.ret1(arg15,arg15=null),
                       Util.ret1(arg16,arg16=null),
                       Util.ret1(arg17,arg17=null),
                       Util.ret1(arg18,arg18=null),
                       Util.ret1(arg19,arg19=null),
                       Util.ret1(arg20,arg20=null));
}

public Object invoke(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                     Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                     Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20,
                     Object... args)
		{
    return fn().invoke(Util.ret1(arg1,arg1=null),
                       Util.ret1(arg2,arg2=null),
                       Util.ret1(arg3,arg3=null),
                       Util.ret1(arg4,arg4=null),
                       Util.ret1(arg5,arg5=null),
                       Util.ret1(arg6,arg6=null),
                       Util.ret1(arg7,arg7=null),
                       Util.ret1(arg8,arg8=null),
                       Util.ret1(arg9,arg9=null),
                       Util.ret1(arg10,arg10=null),
                       Util.ret1(arg11,arg11=null),
                       Util.ret1(arg12,arg12=null),
                       Util.ret1(arg13,arg13=null),
                       Util.ret1(arg14,arg14=null),
                       Util.ret1(arg15,arg15=null),
                       Util.ret1(arg16,arg16=null),
                       Util.ret1(arg17,arg17=null),
                       Util.ret1(arg18,arg18=null),
                       Util.ret1(arg19,arg19=null),
                       Util.ret1(arg20,arg20=null),
                       (Object[])Util.ret1(args, args=null));
}

public Object applyTo(ISeq arglist) {
	return AFn.applyToHelper(this, arglist);
}

static IFn assoc = new AFn(){
    @Override
    public Object invoke(Object m, Object k, Object v)  {
        return RT.assoc(m, k, v);
    }
};
static IFn dissoc = new AFn() {
    @Override
    public Object invoke(Object c, Object k)  {
            return RT.dissoc(c, k);
    }
};
}
