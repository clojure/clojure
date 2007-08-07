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

import java.util.concurrent.ConcurrentHashMap;

public class DynamicVar{
final InheritableThreadLocal<Binding> dvals;
Object root;
static ConcurrentHashMap<Symbol, DynamicVar> table = new ConcurrentHashMap<Symbol, DynamicVar>();


public static DynamicVar intern(Symbol sym, Object root, boolean replaceRoot){
	DynamicVar dvout = table.get(sym);
	boolean present = dvout != null;

	if(!present)
		{
		DynamicVar dvin = new DynamicVar(root);
		dvout = table.putIfAbsent(sym, dvin);
		present = dvout != dvin;   //might have snuck in
		}
	if(present)
		{
		synchronized(dvout)
			{
			if(dvout.root == dvout.dvals || replaceRoot)
				dvout.setRoot(root);
			}
		}
	return dvout;
}

public static DynamicVar intern(Symbol sym){
	DynamicVar dvout = table.get(sym);
	if(dvout != null)
		return dvout;

	return table.putIfAbsent(sym, new DynamicVar());
}

public static DynamicVar find(Symbol sym){
	return table.get(sym);
}

public DynamicVar(){
	this.dvals = new InheritableThreadLocal<Binding>();
	this.root = dvals;  //use dvals as magic not-bound value
}

public DynamicVar(Object root){
	this();
	this.root = root;
}

boolean isBound(){
	return root != dvals || dvals.get() != null;
}

public Object get(){
	Binding b = getThreadBinding();
	if(b != null)
		return b.val;
	if(root != dvals)
		return root;
	throw new IllegalStateException("Var is unbound.");
}

public Object set(Object val){
	Binding b = getThreadBinding();
	if(b != null)
		return (b.val = val);
	throw new IllegalStateException("Var is unbound.");
}

public Object getRoot(){
	return root;
}

synchronized public DynamicVar setRoot(Object root){
	this.root = root;
	return this;
}

public void pushThreadBinding(Object val){
	dvals.set(new Binding(val, dvals.get()));
}

public void popThreadBinding(){
	Binding b = dvals.get();
	if(b == null)
		throw new IllegalStateException("Can't pop unbound ref");
	dvals.set(b.rest);
}

final Binding getThreadBinding(){
	return dvals.get();
}

}
