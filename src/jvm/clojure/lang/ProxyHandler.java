/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Oct 4, 2007 */

package clojure.lang;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;

public class ProxyHandler implements InvocationHandler{
//method-name-string->fn
final IPersistentMap fns;


public ProxyHandler(IPersistentMap fns){
	this.fns = fns;
}

public Object invoke(Object proxy, Method method, Object[] args) throws Throwable{
	Class rt = method.getReturnType();
	IFn fn = (IFn) fns.valAt(method.getName());
	if(fn == null)
		{
		if(rt == Void.TYPE)
			return null;
		else if(method.getName().equals("equals"))
			{
			return proxy == args[0];
			}
		else if(method.getName().equals("hashCode"))
			{
			return System.identityHashCode(proxy);
			}
		else if(method.getName().equals("toString"))
			{
			return "Proxy: " + System.identityHashCode(proxy);
			}
		throw new UnsupportedOperationException();
		}
	Object ret = fn.applyTo(ArraySeq.create(args));
	if(rt == Void.TYPE)
		return null;
	else if(rt.isPrimitive())
		{
		if(rt == Character.TYPE)
			return ret;
		else if(rt == Integer.TYPE)
			return ((Number) ret).intValue();
		else if(rt == Long.TYPE)
			return ((Number) ret).longValue();
		else if(rt == Float.TYPE)
			return ((Number) ret).floatValue();
		else if(rt == Double.TYPE)
			return ((Number) ret).doubleValue();
		else if(rt == Boolean.TYPE && !(ret instanceof Boolean))
			return ret == null ? Boolean.FALSE : Boolean.TRUE;
		else if(rt == Byte.TYPE)
			return (byte) ((Number) ret).intValue();
		else if(rt == Short.TYPE)
			return (short) ((Number) ret).intValue();
		}
	return ret;
}
}
