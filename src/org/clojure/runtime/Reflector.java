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

package org.clojure.runtime;

import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class Reflector{

public static Object invokeInstanceMethod(String name, Object target, Object[] args) throws Exception
	{
	Class c = target.getClass();
	List methods = getMethods(c, args.length, name);
	if(methods.isEmpty())
		{
		throw new IllegalArgumentException("No matching field or method found");
		}
	else if(methods.size() == 1)
		{
		Method m = (Method) methods.get(0);
		return m.invoke(target, boxArgs(m.getParameterTypes(), args));
		}
	else //overloaded w/same arity
		{
		for(Iterator i = methods.iterator(); i.hasNext();)
			{
			Method m = (Method) i.next();

			Class[] params = m.getParameterTypes();
			if(isCongruent(params, args))
				{
				Object[] boxedArgs = boxArgs(params, args);
				return m.invoke(target, boxedArgs);
				}
			}
		throw new IllegalArgumentException("No matching field or method found");

		}
	}

public static Object invokeInstanceMember(String name, Object target) throws Exception
	{
	//check for field first
	Class c = target.getClass();
	Field f = getField(c, name);
	if(f != null)  //field get
		{
		return f.get(target);
		}
	return invokeInstanceMethod(name, target, RT.EMPTY_ARRAY);
	}

public static Object invokeInstanceMember(String name, Object target, Object arg1) throws Exception
	{
	//check for field first
	Class c = target.getClass();
	Field f = getField(c, name);
	if(f != null)  //field set
		{
		f.set(target, boxArg(f.getType(), arg1));
		return arg1;
		}
	return invokeInstanceMethod(name, target, new Object[]{arg1});
	}

public static Object invokeInstanceMember(String name, Object target, Object arg1, Object arg2) throws Exception
	{
	return invokeInstanceMethod(name, target, new Object[]{arg1, arg2});
	}

public static Object invokeInstanceMember(String name, Object target, Object arg1, Object arg2, Object arg3)
		throws Exception
	{
	return invokeInstanceMethod(name, target, new Object[]{arg1, arg2, arg3});
	}

public static Object invokeInstanceMember(String name, Object target, Object arg1, Object arg2, Object arg3,
                                          Object arg4)
		throws Exception
	{
	return invokeInstanceMethod(name, target, new Object[]{arg1, arg2, arg3, arg4});
	}

public static Object invokeInstanceMember(String name, Object target, Object arg1, Object arg2, Object arg3,
                                          Object arg4,
                                          Cons arglist)
		throws Exception
	{
	Object[] args = new Object[4 + RT.length(arglist)];
	args[0] = arg1;
	args[1] = arg2;
	args[2] = arg3;
	args[3] = arg4;
	for(int i = 4; arglist != null; i++, arglist = arglist.rest)
		args[i] = arglist.first;
	return invokeInstanceMethod(name, target, args);
	}


static public Field getField(Class c, String name)
	{
	Field[] allfields = c.getFields();
	for(int i = 0; i < allfields.length; i++)
		{
		if(name.equals(allfields[i].getName()))
			return allfields[i];
		}
	return null;
	}

static public List getMethods(Class c, int arity, String name)
	{
	Method[] allmethods = c.getMethods();
	ArrayList methods = new ArrayList();
	for(int i = 0; i < allmethods.length; i++)
		{
		if(name.equals(allmethods[i].getName())
		   && !Modifier.isStatic(allmethods[i].getModifiers())
		   && allmethods[i].getParameterTypes().length == arity)
			{
			methods.add(allmethods[i]);
			}
		}
	return methods;
	}


static Object boxArg(Class paramType, Object arg)
	{
	Class argType = arg.getClass();
	if(primBoxTypeMatch(paramType, argType))
		return arg;
	if(paramType == boolean.class)
		{
		if(arg == null)
			return Boolean.FALSE;
		return Boolean.TRUE;
		}
	else if(paramType.isPrimitive() && arg instanceof Num)
		{
		Num n = (Num) arg;
		if(paramType == int.class)
			return RT.box(n.intValue());
		else if(paramType == float.class)
			return RT.box(n.floatValue());
		else if(paramType == double.class)
			return RT.box(n.doubleValue());
		else if(paramType == long.class)
			return RT.box(n.longValue());
		else if(paramType == char.class)
			return RT.box((char) n.intValue());
		else if(paramType == short.class)
			return RT.box(n.shortValue());
		else
			return RT.box(n.byteValue());
		}
	else
		return arg;
	}

static Object[] boxArgs(Class[] params, Object[] args)
	{
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

static public boolean primBoxTypeMatch(Class primType, Class boxType)
	{
	if(primType == int.class)
		return boxType == Integer.class;
	else if(primType == float.class)
		return boxType == Float.class;
	else if(primType == double.class)
		return boxType == Double.class;
	else if(primType == long.class)
		return boxType == Long.class;
	else if(primType == char.class)
		return boxType == Character.class;
	else if(primType == short.class)
		return boxType == Short.class;
	else if(primType == byte.class)
		return boxType == Byte.class;
	return false;
	}

static boolean isCongruent(Class[] params, Object[] args)
	{
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
			if(paramType == boolean.class)
				{
				ret = arg == null || argType == Boolean.class;
				}
			else if(paramType.isPrimitive())
				{
				if(arg == null)
					ret = false;
				else
					ret = primBoxTypeMatch(paramType, argType);
				}
			else
				{
				ret = arg == null
				      || argType == paramType
				      || paramType.isAssignableFrom(argType);
				}
			}
		}
	return ret;
	}
}
