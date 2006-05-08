/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

using System;
using System.Reflection;
using System.Collections;

namespace org.clojure.runtime
{
public class Reflector{
    
public static Object invokeInstanceMethod(String name, Object target, Object[] args) //throws Exception	{	Type t = target.GetType();	IList methods = getMethods(t, args.Length, name);	if(methods.Count == 0)		{
        throw new InvalidOperationException("No matching field or method found");		}	else if(methods.Count == 1)		{		MethodInfo m = (MethodInfo) methods[0];		return m.Invoke(target, boxArgs(m.GetParameters(), args));		}	else //overloaded w/same arity, let reflection choose most specific match		{
        return t.InvokeMember(name, BindingFlags.Public | BindingFlags.Instance | BindingFlags.FlattenHierarchy | BindingFlags.InvokeMethod,                                 null, target, args);		}	}

public static Object invokeInstanceMember(String name, Object target) //throws Exception	{	//check for field first	Type t = target.GetType();	FieldInfo f = getField(t, name);	if(f != null)  //field get		{		return f.GetValue(target);		}
    PropertyInfo p = getProperty(t, name);
    if (p != null)
        {
        return p.GetValue(target, null);
        }	return invokeInstanceMethod(name, target, RT.EMPTY_ARRAY);	}public static Object invokeInstanceMember(String name, Object target, Object arg1) //throws Exception	{	//check for field first	Type t = target.GetType();    FieldInfo f = getField(t, name);
    if (f != null)  //field get
        {
        f.SetValue(target,boxArg(f.FieldType,arg1));
        return arg1;
        }
    PropertyInfo p = getProperty(t, name);
    if (p != null)
        {
        //could be indexed property, which we otherwise aren't dealing with yet
        if(p.GetIndexParameters() != null && p.GetIndexParameters().Length == 1)
            return p.GetValue(target, new Object[]{boxArg(p.GetIndexParameters()[0].ParameterType,arg1)});
        p.SetValue(target,boxArg(p.PropertyType,arg1),null);
        }	return invokeInstanceMethod(name, target, new Object[]{arg1});	}public static Object invokeInstanceMember(String name, Object target, Object arg1, Object arg2) //throws Exception	{	return invokeInstanceMethod(name, target, new Object[]{arg1, arg2});	}public static Object invokeInstanceMember(String name, Object target, Object arg1, Object arg2, Object arg3)		//throws Exception	{	return invokeInstanceMethod(name, target, new Object[]{arg1, arg2, arg3});	}public static Object invokeInstanceMember(String name, Object target, Object arg1, Object arg2, Object arg3,                                          Object arg4)		//throws Exception	{	return invokeInstanceMethod(name, target, new Object[]{arg1, arg2, arg3, arg4});	}public static Object invokeInstanceMember(String name, Object target, Object arg1, Object arg2, Object arg3,                                          Object arg4,                                          Cons arglist)		//throws Exception	{	Object[] args = new Object[4 + RT.length(arglist)];	args[0] = arg1;	args[1] = arg2;	args[2] = arg3;	args[3] = arg4;	for(int i = 4; arglist != null; i++, arglist = arglist.rest)		args[i] = arglist.first;	return invokeInstanceMethod(name, target, args);	}

    public static FieldInfo getField(Type t, string name)
        {
        return t.GetField(name, BindingFlags.Public | BindingFlags.Instance |BindingFlags.FlattenHierarchy);
        }
    public static PropertyInfo getProperty(Type t, string name)
        {
        return t.GetProperty(name, BindingFlags.Public | BindingFlags.Instance | BindingFlags.FlattenHierarchy);
        }

    static public IList getMethods(Type t, int arity, String name)
        {
        MethodInfo[] allmethods = t.GetMethods(BindingFlags.Public | BindingFlags.Instance | BindingFlags.FlattenHierarchy);
        ArrayList methods = new ArrayList();
        for (int i = 0; i < allmethods.Length; i++)
            {
            if (name.Equals(allmethods[i].Name)
               && allmethods[i].GetParameters().Length == arity)
                {
                methods.Add(allmethods[i]);
                }
            }
        return methods;
        }

static Object boxArg(Type paramType, Object arg)	{	Type argType = arg.GetType();	if(paramType == argType)		return arg;	if(paramType == typeof(bool))		{		return arg != null;		}	else if(paramType.IsPrimitive && arg is Num)		{		Num n = (Num) arg;		if(paramType == typeof(int))			return RT.box(n.intValue());		else if(paramType == typeof(float))			return RT.box(n.floatValue());		else if(paramType == typeof(double))			return RT.box(n.doubleValue());		else if(paramType == typeof(long))			return RT.box(n.longValue());		else if(paramType == typeof(char))			return RT.box((char) n.intValue());		else if(paramType == typeof(short))			return RT.box(n.shortValue());		else if(paramType == typeof(byte))			return RT.box(n.byteValue());        else            throw new ArgumentException("Cannot convert to primitive type: " + paramType.Name);		}	else		return arg;	}static Object[] boxArgs(ParameterInfo[] parms, Object[] args)	{	if(parms.Length == 0)		return null;	Object[] ret = new Object[parms.Length];	for(int i = 0; i < parms.Length; i++)		{		Object arg = args[i];		Type paramType = parms[i].ParameterType;		ret[i] = boxArg(paramType, arg);		}	return ret;	}



 }
}
