/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Apr 19, 2006 */

package clojure.lang;

import java.lang.reflect.*;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Arrays;

public class Reflector{

public static Object invokeInstanceMethod(Object target, String methodName, Object[] args) throws Exception{
	Class c = target.getClass();
	List methods = getMethods(c, args.length, methodName, false);
	return invokeMatchingMethod(methodName, methods, target, args);
}

static Object invokeMatchingMethod(String methodName, List methods, Object target, Object[] args)
		throws IllegalAccessException, InvocationTargetException, NoSuchMethodException{
	Method m = null;
	Object[] boxedArgs = null;
	if(methods.isEmpty())
		{
		throw new IllegalArgumentException("No matching method found: " + methodName);
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
				if(foundm == null || Compiler.subsumes(params,foundm.getParameterTypes()))
					{
					foundm = m;
					boxedArgs = boxArgs(params, args);
					}
				}
			}
		m = foundm;
		}
	if(m == null)
		throw new IllegalArgumentException("No matching method found: " + methodName);

	if(!Modifier.isPublic(m.getDeclaringClass().getModifiers()))
		{
		//public method of non-public class, try to find it in hierarchy
		m = getAsMethodOfPublicBase(m.getDeclaringClass(), m);
		}
	if(m == null)
		throw new IllegalArgumentException("No matching method found: " + methodName);
	return prepRet(m.getReturnType(), m.invoke(target, boxedArgs));
}

public static Method getAsMethodOfPublicBase(Class c, Method m){
	for(Class iface : c.getInterfaces())
		{
		for(Method im : iface.getMethods())
			{
			if(im.getName().equals(m.getName())
			   && Arrays.equals(m.getParameterTypes(), im.getParameterTypes()))
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
		if(scm.getName().equals(m.getName())
		   && Arrays.equals(m.getParameterTypes(), scm.getParameterTypes())
		   && Modifier.isPublic(scm.getDeclaringClass().getModifiers()))
			{
			return scm;
			}
		}
	return getAsMethodOfPublicBase(sc, m);
}

public static Object invokeConstructor(Class c, Object[] args) throws Exception{
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
		throw new IllegalArgumentException("No matching ctor found");
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
		throw new IllegalArgumentException("No matching ctor found");
		}
}

public static Object invokeStaticMethodVariadic(String className, String methodName, Object... args) throws Exception{
	return invokeStaticMethod(className, methodName, args);

}

public static Object invokeStaticMethod(String className, String methodName, Object[] args) throws Exception{
	Class c = Class.forName(className);
	return invokeStaticMethod(c, methodName, args);
}

public static Object invokeStaticMethod(Class c, String methodName, Object[] args) throws Exception{
	if(methodName.equals("new"))
		return invokeConstructor(c, args);
	List methods = getMethods(c, args.length, methodName, true);
	return invokeMatchingMethod(methodName, methods, null, args);
}

public static Object getStaticField(String className, String fieldName) throws Exception{
	Class c = Class.forName(className);
	return getStaticField(c, fieldName);
}

public static Object getStaticField(Class c, String fieldName) throws Exception{
//	if(fieldName.equals("class"))
//		return c;
	Field f = getField(c, fieldName, true);
	if(f != null)
		{
		return prepRet(f.getType(), f.get(null));
		}
	throw new IllegalArgumentException("No matching field found: " + fieldName);
}
public static Object setStaticField(String className, String fieldName, Object val) throws Exception{
	Class c = Class.forName(className);
	return setStaticField(c, fieldName, val);
}

public static Object setStaticField(Class c, String fieldName, Object val) throws Exception{
	Field f = getField(c, fieldName, true);
	if(f != null)
		{
		f.set(null, boxArg(f.getType(), val));
		return val;
		}
	throw new IllegalArgumentException("No matching field found: " + fieldName);
}

public static Object getInstanceField(Object target, String fieldName) throws Exception{
	Class c = target.getClass();
	Field f = getField(c, fieldName, false);
	if(f != null)
		{
		return prepRet(f.getType(), f.get(target));
		}
	throw new IllegalArgumentException("No matching field found: " + fieldName);
}

public static Object setInstanceField(Object target, String fieldName, Object val) throws Exception{
	Class c = target.getClass();
	Field f = getField(c, fieldName, false);
	if(f != null)
		{
		f.set(target, boxArg(f.getType(), val));
		return val;
		}
	throw new IllegalArgumentException("No matching field found: " + fieldName);
}

public static Object invokeInstanceMember(Object target, String name) throws Exception{
	//check for field first
	Class c = target.getClass();
	Field f = getField(c, name, false);
	if(f != null)  //field get
		{
		return prepRet(f.getType(), f.get(target));
		}
	return invokeInstanceMethod(target, name, RT.EMPTY_ARRAY);
}

public static Object invokeInstanceMember(String name, Object target, Object arg1) throws Exception{
	//check for field first
	Class c = target.getClass();
	Field f = getField(c, name, false);
	if(f != null)  //field set
		{
		f.set(target, boxArg(f.getType(), arg1));
		return arg1;
		}
	return invokeInstanceMethod(target, name, new Object[]{arg1});
}

public static Object invokeInstanceMember(String name, Object target, Object... args) throws Exception{
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

static public List getMethods(Class c, int arity, String name, boolean getStatics){
	Method[] allmethods = c.getMethods();
	ArrayList methods = new ArrayList();
	for(int i = 0; i < allmethods.length; i++)
		{
		if(name.equals(allmethods[i].getName())
		   && Modifier.isStatic(allmethods[i].getModifiers()) == getStatics
		   && allmethods[i].getParameterTypes().length == arity)
			{
			methods.add(allmethods[i]);
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
	throw new IllegalArgumentException("Unexpected param type");
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
		return argType == Integer.class || argType == FixNum.class;
	else if(paramType == float.class)
		return argType == Float.class;
	else if(paramType == double.class)
		return argType == Double.class || argType == DoubleNum.class;
	else if(paramType == long.class)
		return argType == Long.class || argType == BigNum.class;
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

static Object prepRet(Class c, Object x){
//	if(c == boolean.class)
//		return ((Boolean) x).booleanValue() ? RT.T : null;
	if(x instanceof Boolean && !((Boolean) x).booleanValue())
		return Boolean.FALSE;
	return x;
}
}
