/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Jul 31, 2007 */

package clojure.lang;

import java.util.concurrent.atomic.AtomicInteger;


public final class Var implements IFn, IRef, IObj{


static class Frame{
	//Var->Box
	Associative bindings;
	//Var->val
	Associative frameBindings;
	Frame prev;


	public Frame(){
		this(PersistentHashMap.EMPTY, PersistentHashMap.EMPTY, null);
	}

	public Frame(Associative frameBindings, Associative bindings, Frame prev){
		this.frameBindings = frameBindings;
		this.bindings = bindings;
		this.prev = prev;
	}
}

static ThreadLocal<Frame> dvals = new ThreadLocal<Frame>(){

	protected Frame initialValue(){
		return new Frame();
	}
};

static Keyword privateKey = Keyword.intern(null, "private");
static Keyword macroKey = Keyword.intern(null, "macro");
static Keyword nameKey = Keyword.intern(null, "name");
static Keyword nsKey = Keyword.intern(null, "ns");
//static Keyword tagKey = Keyword.intern(null, "tag");

volatile Object root;
transient final AtomicInteger count;
public final Symbol sym;
public final Namespace ns;
volatile IFn validator = null;

IPersistentMap _meta;

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
		return "#=(var " + ns.name + "/" + sym + ")";
	return "#<Var: " + (sym != null ? sym.toString() : "--unnamed--") + ">";
}

public static Var find(Symbol nsQualifiedSym){
	if(nsQualifiedSym.ns == null)
		throw new IllegalArgumentException("Symbol must be namespace-qualified");
	Namespace ns = Namespace.find(Symbol.create(nsQualifiedSym.ns));
	if(ns == null)
		throw new IllegalArgumentException("No such namespace: " + nsQualifiedSym.ns);
	return ns.findInternedVar(Symbol.create(nsQualifiedSym.name));
}

public static Var intern(Symbol nsName, Symbol sym){
	Namespace ns = Namespace.findOrCreate(nsName);
	return intern(ns, sym);
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
	this.count = new AtomicInteger();
	this.root = dvals;  //use dvals as magic not-bound value
	setMeta(PersistentHashMap.EMPTY);
}

Var(Namespace ns, Symbol sym, Object root){
	this(ns, sym);
	this.root = root;
}

public boolean isBound(){
	return hasRoot() || (count.get() > 0 && dvals.get().bindings.containsKey(this));
}

final public Object get(){
	Box b = getThreadBinding();
	if(b != null)
		return b.val;
	if(hasRoot())
		return root;
	throw new IllegalStateException(String.format("Var %s is unbound.", sym));
}

public void setValidator(IFn vf){
	if(isBound())
		validate(vf, getRoot());
	validator = vf;
}

public IFn getValidator(){
	return validator;
}

public Object alter(IFn fn, ISeq args) throws Exception{
	set(fn.applyTo(RT.cons(get(), args)));
	return this;
}

void validate(IFn vf, Object val){
	try
		{
		if(vf != null)
			vf.invoke(val);
		}
	catch(Exception e)
		{
		throw new IllegalStateException("Invalid var state", e);
		}
}

public Object set(Object val){
	validate(getValidator(), val);
	Box b = getThreadBinding();
	if(b != null)
		return (b.val = val);
	//jury still out on this
//	if(hasRoot())
//		{
//		bindRoot(val);
//		return val;
//		}
	throw new IllegalStateException(String.format("Can't change/establish root binding of: %s with set", sym));
}

public void setMeta(IPersistentMap m){
	//ensure these basis keys
	_meta = m.assoc(nameKey, sym).assoc(nsKey, ns);
}

public IPersistentMap meta(){
	return _meta;
}

public IObj withMeta(IPersistentMap meta){
	throw new UnsupportedOperationException("Vars are not values");
}

public void setMacro(){
	_meta = _meta.assoc(macroKey, RT.T);
}

public boolean isMacro(){
	return RT.booleanCast(_meta.valAt(macroKey));
}

//public void setExported(boolean state){
//	_meta = _meta.assoc(privateKey, state);
//}

public boolean isPublic(){
	return !RT.booleanCast(_meta.valAt(privateKey));
}

public Object getRoot(){
	return root;
}

public Object getTag(){
	return _meta.valAt(RT.TAG_KEY);
}

public void setTag(Symbol tag){
	_meta = _meta.assoc(RT.TAG_KEY, tag);
}

final public boolean hasRoot(){
	return root != dvals;
}

//binding root always clears macro flag
synchronized public void bindRoot(Object root){
	validate(getValidator(), root);
	this.root = root;
	_meta = _meta.assoc(macroKey, RT.F);
}

synchronized void swapRoot(Object root){
	validate(getValidator(), root);
	this.root = root;
}

synchronized public void unbindRoot(){
	this.root = dvals;
}

synchronized public void commuteRoot(IFn fn) throws Exception{
	Object newRoot = fn.invoke(root);
	validate(getValidator(), newRoot);
	this.root = newRoot;
}

synchronized public Object alterRoot(IFn fn, ISeq args) throws Exception{
	Object newRoot = fn.applyTo(RT.cons(root, args));
	validate(getValidator(), newRoot);
	this.root = newRoot;
	return newRoot;
}

public static void pushThreadBindings(Associative bindings){
	Frame f = dvals.get();
	Associative bmap = f.bindings;
	for(ISeq bs = bindings.seq(); bs != null; bs = bs.rest())
		{
		IMapEntry e = (IMapEntry) bs.first();
		Var v = (Var) e.key();
		v.validate(v.getValidator(), e.val());
		v.count.incrementAndGet();
		bmap = bmap.assoc(v, new Box(e.val()));
		}
	dvals.set(new Frame(bindings, bmap, f));
}

public static void popThreadBindings(){
	Frame f = dvals.get();
	if(f.prev == null)
		throw new IllegalStateException("Pop without matching push");
	for(ISeq bs = RT.keys(f.frameBindings); bs != null; bs = bs.rest())
		{
		Var v = (Var) bs.first();
		v.count.decrementAndGet();
		}
	dvals.set(f.prev);
}

public static void releaseThreadBindings(){
	Frame f = dvals.get();
	if(f.prev == null)
		throw new IllegalStateException("Release without full unwind");
	for(ISeq bs = RT.keys(f.bindings); bs != null; bs = bs.rest())
		{
		Var v = (Var) bs.first();
		v.count.decrementAndGet();
		}
	dvals.set(null);
}

final Box getThreadBinding(){
	if(count.get() > 0)
		{
		IMapEntry e = dvals.get().bindings.entryAt(this);
		if(e != null)
			return (Box) e.val();
		}
	return null;
}

final public IFn fn(){
	return (IFn) get();
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

}
