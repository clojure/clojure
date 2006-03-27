/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 25, 2006 11:42:47 AM */

package org.clojure.runtime;

public class Symbol extends AFn{

public final static Object UNBOUND = new Object();

public final String name;
public final Namespace namespace;
public Object val = UNBOUND;
public IFn fn;


/**
 * Used by Namespace.intern()
 * @param name
 * @param ns
 */
Symbol(String name, Namespace ns)
	{
	this.namespace = ns;
	this.name = name;
	}

public Object getValue(ThreadLocalData tld)  throws Exception
	{
	Cons binding = tld.getDynamicBinding(this);
	if(binding != null)
		return binding.first;
	if(val == UNBOUND)
		throw new Exception(name + " is unbound.");
	return val;
	}

public Object setValue(ThreadLocalData tld, Object val)
	{
	Cons binding = tld.getDynamicBinding(this);
	if(binding != null)
		return binding.first = val;
	//allow global set to create binding like this?
	if(val instanceof IFn)
		this.fn = (IFn) val;
	else
		this.fn = null; //todo, bind to throw stub?
	return this.val = val;
	}

/**
 *  Symbol implements IFn for attr access
 *  This single arg version is the getter
 * @param tld
 * @param obj - must be AMap
 * @return the value of the attr or nil if not found
 * @throws Exception
 */
public Object invoke(ThreadLocalData tld, Object obj) throws Exception
	{
	return ((AMap)obj).get(this);
	}

/**
 *  Symbol implements IFn for attr access
 *  This two arg version is the setter
 * @param tld
 * @param obj - must be AMap
 * @param val
 * @return val
 * @throws Exception
 */
public Object invoke(ThreadLocalData tld, Object obj, Object val) throws Exception
	{
	return ((AMap)obj).put(this,val);
	}
}
