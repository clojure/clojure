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

public class DynamicVar{
final InheritableThreadLocal<Binding> dvals;
Object root;

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

public Object get() throws Exception{
	Binding b = getThreadBinding();
	if(b != null)
		return b.val;
	if(root != dvals)
		return root;
	throw new IllegalStateException("Var is unbound.");
}

public Object set(Object val) throws Exception{
	Binding b = getThreadBinding();
	if(b != null)
		return (b.val = val);
	throw new IllegalStateException("Var is unbound.");
}

public Object getRoot(){
	return root;
}

public void setRoot(Object root){
	this.root = root;
}

public void pushThreadBinding(Object val){
	dvals.set(new Binding(val, dvals.get()));
}

public void popThreadBinding() throws Exception{
	Binding b = dvals.get();
	if(b == null)
		throw new Exception("Can't pop unbound ref");
	dvals.set(b.rest);
}

final Binding getThreadBinding(){
	return dvals.get();
}

}
