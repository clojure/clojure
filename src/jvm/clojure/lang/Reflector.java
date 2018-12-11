/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Apr 19, 2006 */

package clojure.lang;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.invoke.MethodType;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.*;
import java.util.stream.Collectors;

public class Reflector{

private static final MethodHandle CAN_ACCESS_PRED;

// Java 8 is oldest JDK supported
private static boolean isJava8() {
	return System.getProperty("java.vm.specification.version").equals("1.8");
}

static {
	MethodHandle pred = null;
	try {
		if (! isJava8())
			pred = MethodHandles.lookup().findVirtual(Method.class, "canAccess", MethodType.methodType(boolean.class, Object.class));
	} catch (Throwable t) {
		Util.sneakyThrow(t);
	}
	CAN_ACCESS_PRED = pred;
}

private static boolean canAccess(Method m, Object target) {
	if (CAN_ACCESS_PRED != null) {
		// JDK9+ use j.l.r.AccessibleObject::canAccess, which respects module rules
		try {
			return (boolean) CAN_ACCESS_PRED.invoke(m, target);
		} catch (Throwable t) {
			throw Util.sneakyThrow(t);
		}
	} else {
		// JDK 8
		return true;
	}
}

private static Collection<Class> interfaces(Class c) {
	Set<Class> interfaces = new HashSet<Class>();
	Deque<Class> toWalk = new ArrayDeque<Class>();
	toWalk.addAll(Arrays.asList(c.getInterfaces()));
	Class iface = toWalk.poll();
	while (iface != null) {
		interfaces.add(iface);
		toWalk.addAll(Arrays.asList(iface.getInterfaces()));
		iface = toWalk.poll();
	}
	return interfaces;
}

private static Method tryFindMethod(Class c, Method m) {
	if(c == null) return null;
	try {
		return c.getMethod(m.getName(), m.getParameterTypes());
	} catch(NoSuchMethodException e) {
		return null;
	}
}

private static Method toAccessibleSuperMethod(Method m, Object target) {
	Method selected = m;
	while(selected != null) {
		if(canAccess(selected, target)) return selected;
		selected = tryFindMethod(selected.getDeclaringClass().getSuperclass(), m);
	}

	Collection<Class> interfaces = interfaces(m.getDeclaringClass());
	for(Class c : interfaces) {
		selected = tryFindMethod(c, m);
		if(selected != null) return selected;
	}
	return null;
}

public static Object invokeInstanceMethod(Object target, String methodName, Object[] args) {
	Class c = target.getClass();
	List methods = getMethods(c, args.length, methodName, false).stream()
					.map(method -> toAccessibleSuperMethod(method, target))
					.filter(method -> (method != null))
					.collect(Collectors.toList());
	return invokeMatchingMethod(methodName, methods, target, args);
}

private static Throwable getCauseOrElse(Exception e) {
	if (e.getCause() != null)
		return e.getCause();
	return e;
}

private static RuntimeException throwCauseOrElseException(Exception e) {
	if (e.getCause() != null)
		throw Util.sneakyThrow(e.getCause());
	throw Util.sneakyThrow(e);
}

private static String noMethodReport(String methodName, Object target, Object[] args){
	 return "No matching method " + methodName + " found taking " + args.length + " args"
			+ (target==null?"":" for " + target.getClass());
}
static Object invokeMatchingMethod(String methodName, List methods, Object target, Object[] args)
		{
	Method m = null;
	Object[] boxedArgs = null;
	if(methods.isEmpty())
		{
		throw new IllegalArgumentException(noMethodReport(methodName,target,args));
		}
	else if(methods.size() == 1)
		{
		m = (Method) methods.get(0);
		boxedArgs = boxArgs(m.getParameterTypes(), args);
		}
	else //overloaded w/same arity
		{
		Method foundm = null;
		for(Iterator i = methods.iterator(); i.hasNext();)
			{
			m = (Method) i.next();

			Class[] params = m.getParameterTypes();
			if(isCongruent(params, args))
				{
				if(foundm == null || Compiler.subsumes(params, foundm.getParameterTypes()))
					{
					foundm = m;
					boxedArgs = boxArgs(params, args);
					}
				}
			}
		m = foundm;
		}
	if(m == null)
		throw new IllegalArgumentException(noMethodReport(methodName,target,args));

	if(!Modifier.isPublic(m.getDeclaringClass().getModifiers()) || !canAccess(m, target))
		{
		//public method of non-public class, try to find it in hierarchy
		Method oldm = m;
		m = getAsMethodOfAccessibleBase(target.getClass(), m, target);
		if(m == null)
			throw new IllegalArgumentException("Can't call public method of non-public class: " +
			                                    oldm.toString());
		}
	try
		{
		return prepRet(m.getReturnType(), m.invoke(target, boxedArgs));
		}
	catch(Exception e)
		{
		throw Util.sneakyThrow(getCauseOrElse(e));
		}

}

// DEPRECATED - replaced by getAsMethodOfAccessibleBase()
public static Method getAsMethodOfPublicBase(Class c, Method m){
	for(Class iface : c.getInterfaces())
		{
		for(Method im : iface.getMethods())
			{
			if(isMatch(im, m))
				{
				return im;
				}
			}
		}
	Class sc = c.getSuperclass();
	if(sc == null)
		return null;
	for(Method scm : sc.getMethods())
		{
		if(isMatch(scm, m))
			{
			return scm;
			}
		}
	return getAsMethodOfPublicBase(sc, m);
}

// DEPRECATED - replaced by isAccessibleMatch()
public static boolean isMatch(Method lhs, Method rhs) {
	if(!lhs.getName().equals(rhs.getName())
			|| !Modifier.isPublic(lhs.getDeclaringClass().getModifiers()))
		{
		return false;
		}

		Class[] types1 = lhs.getParameterTypes();
		Class[] types2 = rhs.getParameterTypes();
		if(types1.length != types2.length)
			return false;

		boolean match = true;
		for (int i=0; i<types1.length; ++i)
			{
			if(!types1[i].isAssignableFrom(types2[i]))
				{
				match = false;
				break;
				}
			}
		return match;
}

public static Method getAsMethodOfAccessibleBase(Class c, Method m, Object target){
	for(Class iface : c.getInterfaces())
	{
		for(Method im : iface.getMethods())
		{
			if(isAccessibleMatch(im, m, target))
			{
				return im;
			}
		}
	}
	Class sc = c.getSuperclass();
	if(sc == null)
		return null;
	for(Method scm : sc.getMethods())
	{
		if(isAccessibleMatch(scm, m, target))
		{
			return scm;
		}
	}
	return getAsMethodOfAccessibleBase(sc, m, target);
}

public static boolean isAccessibleMatch(Method lhs, Method rhs, Object target) {
	if(!lhs.getName().equals(rhs.getName())
			|| !Modifier.isPublic(lhs.getDeclaringClass().getModifiers())
			|| !canAccess(lhs, target))
	{
		return false;
	}

	Class[] types1 = lhs.getParameterTypes();
	Class[] types2 = rhs.getParameterTypes();
	if(types1.length != types2.length)
		return false;

	boolean match = true;
	for (int i=0; i<types1.length; ++i)
	{
		if(!types1[i].isAssignableFrom(types2[i]))
		{
			match = false;
			break;
		}
	}
	return match;
}

public static Object invokeConstructor(Class c, Object[] args) {
	try
		{
		Constructor[] allctors = c.getConstructors();
		ArrayList ctors = new ArrayList();
		for(int i = 0; i < allctors.length; i++)
			{
			Constructor ctor = allctors[i];
			if(ctor.getParameterTypes().length == args.length)
				ctors.add(ctor);
			}
		if(ctors.isEmpty())
			{
			throw new IllegalArgumentException("No matching ctor found"
				+ " for " + c);
			}
		else if(ctors.size() == 1)
			{
			Constructor ctor = (Constructor) ctors.get(0);
			return ctor.newInstance(boxArgs(ctor.getParameterTypes(), args));
			}
		else //overloaded w/same arity
			{
			for(Iterator iterator = ctors.iterator(); iterator.hasNext();)
				{
				Constructor ctor = (Constructor) iterator.next();
				Class[] params = ctor.getParameterTypes();
				if(isCongruent(params, args))
					{
					Object[] boxedArgs = boxArgs(params, args);
					return ctor.newInstance(boxedArgs);
					}
				}
			throw new IllegalArgumentException("No matching ctor found"
				+ " for " + c);
			}
		}
	catch(Exception e)
		{
		throw Util.sneakyThrow(getCauseOrElse(e));
		}
}

public static Object invokeStaticMethodVariadic(String className, String methodName, Object... args) {
	return invokeStaticMethod(className, methodName, args);

}

public static Object invokeStaticMethod(String className, String methodName, Object[] args) {
	Class c = RT.classForName(className);
	return invokeStaticMethod(c, methodName, args);
}

public static Object invokeStaticMethod(Class c, String methodName, Object[] args) {
	if(methodName.equals("new"))
		return invokeConstructor(c, args);
	List methods = getMethods(c, args.length, methodName, true);
	return invokeMatchingMethod(methodName, methods, null, args);
}

public static Object getStaticField(String className, String fieldName) {
	Class c = RT.classForName(className);
	return getStaticField(c, fieldName);
}

public static Object getStaticField(Class c, String fieldName) {
//	if(fieldName.equals("class"))
//		return c;
	Field f = getField(c, fieldName, true);
	if(f != null)
		{
		try
			{
			return prepRet(f.getType(), f.get(null));
			}
		catch(IllegalAccessException e)
			{
			throw Util.sneakyThrow(e);
			}
		}
	throw new IllegalArgumentException("No matching field found: " + fieldName
		+ " for " + c);
}

public static Object setStaticField(String className, String fieldName, Object val) {
	Class c = RT.classForName(className);
	return setStaticField(c, fieldName, val);
}

public static Object setStaticField(Class c, String fieldName, Object val) {
	Field f = getField(c, fieldName, true);
	if(f != null)
		{
		try
			{
			f.set(null, boxArg(f.getType(), val));
			}
		catch(IllegalAccessException e)
			{
			throw Util.sneakyThrow(e);
			}
		return val;
		}
	throw new IllegalArgumentException("No matching field found: " + fieldName
		+ " for " + c);
}

public static Object getInstanceField(Object target, String fieldName) {
	Class c = target.getClass();
	Field f = getField(c, fieldName, false);
	if(f != null)
		{
		try
			{
			return prepRet(f.getType(), f.get(target));
			}
		catch(IllegalAccessException e)
			{
			throw Util.sneakyThrow(e);
			}
		}
	throw new IllegalArgumentException("No matching field found: " + fieldName
		+ " for " + target.getClass());
}

public static Object setInstanceField(Object target, String fieldName, Object val) {
	Class c = target.getClass();
	Field f = getField(c, fieldName, false);
	if(f != null)
		{
		try
			{
			f.set(target, boxArg(f.getType(), val));
			}
		catch(IllegalAccessException e)
			{
			throw Util.sneakyThrow(e);
			}
		return val;
		}
	throw new IllegalArgumentException("No matching field found: " + fieldName
		+ " for " + target.getClass());
}

// not used as of Clojure 1.6, but left for runtime compatibility with
// compiled bytecode from older versions
public static Object invokeNoArgInstanceMember(Object target, String name) {
	return invokeNoArgInstanceMember(target, name, false);
}

public static Object invokeNoArgInstanceMember(Object target, String name, boolean requireField) {
	Class c = target.getClass();

	if(requireField) {
		Field f = getField(c, name, false);
		if(f != null)
			return getInstanceField(target, name);
		else
			throw new IllegalArgumentException("No matching field found: " + name
					+ " for " + target.getClass());
	} else {
		List meths = getMethods(c, 0, name, false);
		if(meths.size() > 0)
			return invokeMatchingMethod(name, meths, target, RT.EMPTY_ARRAY);
		else
			return getInstanceField(target, name);
	}
}

public static Object invokeInstanceMember(Object target, String name) {
	//check for field first
	Class c = target.getClass();
	Field f = getField(c, name, false);
	if(f != null)  //field get
		{
		try
			{
			return prepRet(f.getType(), f.get(target));
			}
		catch(IllegalAccessException e)
			{
			throw Util.sneakyThrow(e);
			}
		}
	return invokeInstanceMethod(target, name, RT.EMPTY_ARRAY);
}

public static Object invokeInstanceMember(String name, Object target, Object arg1) {
	//check for field first
	Class c = target.getClass();
	Field f = getField(c, name, false);
	if(f != null)  //field set
		{
		try
			{
			f.set(target, boxArg(f.getType(), arg1));
			}
		catch(IllegalAccessException e)
			{
			throw Util.sneakyThrow(e);
			}
		return arg1;
		}
	return invokeInstanceMethod(target, name, new Object[]{arg1});
}

public static Object invokeInstanceMember(String name, Object target, Object... args) {
	return invokeInstanceMethod(target, name, args);
}


static public Field getField(Class c, String name, boolean getStatics){
	Field[] allfields = c.getFields();
	for(int i = 0; i < allfields.length; i++)
		{
		if(name.equals(allfields[i].getName())
		   && Modifier.isStatic(allfields[i].getModifiers()) == getStatics)
			return allfields[i];
		}
	return null;
}

static public List<Method> getMethods(Class c, int arity, String name, boolean getStatics){
	Method[] allmethods = c.getMethods();
	ArrayList methods = new ArrayList();
	ArrayList bridgeMethods = new ArrayList();
	for(int i = 0; i < allmethods.length; i++)
		{
		Method method = allmethods[i];
		if(name.equals(method.getName())
		   && Modifier.isStatic(method.getModifiers()) == getStatics
		   && method.getParameterTypes().length == arity)
			{
			try
				{
				if(method.isBridge()
				   && c.getMethod(method.getName(), method.getParameterTypes())
						.equals(method))
					bridgeMethods.add(method);
				else
					methods.add(method);
				}
			catch(NoSuchMethodException e)
				{
				}
			}
//			   && (!method.isBridge()
//			       || (c == StringBuilder.class &&
//			          c.getMethod(method.getName(), method.getParameterTypes())
//					.equals(method))))
//				{
//				methods.add(allmethods[i]);
//				}
		}

	if(methods.isEmpty())
		methods.addAll(bridgeMethods);
	
	if(!getStatics && c.isInterface())
		{
		allmethods = Object.class.getMethods();
		for(int i = 0; i < allmethods.length; i++)
			{
			if(name.equals(allmethods[i].getName())
			   && Modifier.isStatic(allmethods[i].getModifiers()) == getStatics
			   && allmethods[i].getParameterTypes().length == arity)
				{
				methods.add(allmethods[i]);
				}
			}
		}
	return methods;
}


static Object boxArg(Class paramType, Object arg){
	if(!paramType.isPrimitive())
		return paramType.cast(arg);
	else if(paramType == boolean.class)
		return Boolean.class.cast(arg);
	else if(paramType == char.class)
		return Character.class.cast(arg);
	else if(arg instanceof Number)
		{
		Number n = (Number) arg;
		if(paramType == int.class)
			return n.intValue();
		else if(paramType == float.class)
			return n.floatValue();
		else if(paramType == double.class)
			return n.doubleValue();
		else if(paramType == long.class)
			return n.longValue();
		else if(paramType == short.class)
			return n.shortValue();
		else if(paramType == byte.class)
			return n.byteValue();
		}
	throw new IllegalArgumentException("Unexpected param type, expected: " + paramType +
	                                   ", given: " + arg.getClass().getName());
}

static Object[] boxArgs(Class[] params, Object[] args){
	if(params.length == 0)
		return null;
	Object[] ret = new Object[params.length];
	for(int i = 0; i < params.length; i++)
		{
		Object arg = args[i];
		Class paramType = params[i];
		ret[i] = boxArg(paramType, arg);
		}
	return ret;
}

static public boolean paramArgTypeMatch(Class paramType, Class argType){
	if(argType == null)
		return !paramType.isPrimitive();
	if(paramType == argType || paramType.isAssignableFrom(argType))
		return true;
	if(paramType == int.class)
		return argType == Integer.class
		       || argType == long.class
				|| argType == Long.class
				|| argType == short.class
				|| argType == byte.class;// || argType == FixNum.class;
	else if(paramType == float.class)
		return argType == Float.class
				|| argType == double.class;
	else if(paramType == double.class)
		return argType == Double.class
				|| argType == float.class;// || argType == DoubleNum.class;
	else if(paramType == long.class)
		return argType == Long.class
				|| argType == int.class
				|| argType == short.class
				|| argType == byte.class;// || argType == BigNum.class;
	else if(paramType == char.class)
		return argType == Character.class;
	else if(paramType == short.class)
		return argType == Short.class;
	else if(paramType == byte.class)
		return argType == Byte.class;
	else if(paramType == boolean.class)
		return argType == Boolean.class;
	return false;
}

static boolean isCongruent(Class[] params, Object[] args){
	boolean ret = false;
	if(args == null)
		return params.length == 0;
	if(params.length == args.length)
		{
		ret = true;
		for(int i = 0; ret && i < params.length; i++)
			{
			Object arg = args[i];
			Class argType = (arg == null) ? null : arg.getClass();
			Class paramType = params[i];
			ret = paramArgTypeMatch(paramType, argType);
			}
		}
	return ret;
}

public static Object prepRet(Class c, Object x){
	if (!(c.isPrimitive() || c == Boolean.class))
		return x;
	if(x instanceof Boolean)
		return ((Boolean) x)?Boolean.TRUE:Boolean.FALSE;
//	else if(x instanceof Integer)
//		{
//		return ((Integer)x).longValue();
//		}
//	else if(x instanceof Float)
//			return Double.valueOf(((Float) x).doubleValue());
	return x;
}
}
