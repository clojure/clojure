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

namespace clojure.lang
{
public class Reflector{
    
public static Object invokeInstanceMethod(String name, Object target, Object[] args) //throws Exception	{	Type t = target.GetType();	IList methods = getMethods(t, args.Length, name,false);
    return invokeMatchingMethod(name, target, args, t, methods,false);
	}

    private static object invokeMatchingMethod(String name, Object target, Object[] args, Type t, IList methods, bool statics)
        {
        if (methods.Count == 0)
            {
            throw new InvalidOperationException("No matching field or method found");
            }
        else if (methods.Count == 1)
            {
            MethodInfo m = (MethodInfo)methods[0];
            return prepRet(m.Invoke(target, boxArgs(m.GetParameters(), args)));
            }
        else //overloaded w/same arity, let reflection choose most specific match
            {
            return prepRet(t.InvokeMember(name, BindingFlags.Public | (statics ? BindingFlags.Static : BindingFlags.Instance) 
                                        | BindingFlags.FlattenHierarchy | BindingFlags.InvokeMethod,
                                    null, target, args));
            }
        }

public static Object invokeConstructor(Type t, Object[] args)  //throws Exception
    {
    ConstructorInfo[] allctors = t.GetConstructors();
    ArrayList ctors = new ArrayList();
    foreach (ConstructorInfo ctor in allctors)
        {
        if (ctor.GetParameters().Length == args.Length)
            ctors.Add(ctor);
        }
    if (ctors.Count == 0)
        {
        throw new InvalidOperationException("No matching ctor found");
        }
    else if (ctors.Count == 1)
        {
        ConstructorInfo ctor = (ConstructorInfo)ctors[0];
        return ctor.Invoke(boxArgs(ctor.GetParameters(), args));
        }
    else //overloaded w/same arity, let reflection choose most specific match
        {
        return t.InvokeMember(null, BindingFlags.Public | BindingFlags.Instance | BindingFlags.CreateInstance,
                            null, null, args);
        }
    }
public static Object invokeStaticMethod(String name, String className, params Object[] args) //throws Exception
    {
    Type t = Type.GetType(className);
    if (name.Equals("new"))
        return invokeConstructor(t, args);
    IList methods = getMethods(t, args.Length, name, true);
    return invokeMatchingMethod(name, null, args, t, methods,true);
    }

public static Object getStaticField(String name, String className) //throws Exception
    {
    //check for field first
    Type t = Type.GetType(className);
    FieldInfo f = getField(t, name, true);
    if (f != null)  //field get
        {
        return prepRet(f.GetValue(null));
        }
    PropertyInfo p = getProperty(t, name, true);
    if (p != null)
        {
        return prepRet(p.GetValue(null, null));
        }
    throw new InvalidOperationException("No matching field or property found");
    }

public static Object setStaticField(String name, String className, Object arg1) //throws Exception
    {
    //check for field first
    Type t = Type.GetType(className);
    FieldInfo f = getField(t, name, true);
    if (f != null)  //field get
        {
        f.SetValue(null, boxArg(f.FieldType, arg1));
        return arg1;
        }
    PropertyInfo p = getProperty(t, name, true);
    if (p != null)
        {
        p.SetValue(null, boxArg(p.PropertyType, arg1), null);
        return arg1;
        }
    throw new InvalidOperationException("No matching field or property found");
    }

public static Object invokeInstanceMember(String name, Object target) //throws Exception	{	//check for field first	Type t = target.GetType();	FieldInfo f = getField(t, name,false);	if(f != null)  //field get		{		return prepRet(f.GetValue(target));		}
    PropertyInfo p = getProperty(t, name,false);
    if (p != null)
        {
        return prepRet(p.GetValue(target, null));
        }	return invokeInstanceMethod(name, target, RT.EMPTY_ARRAY);	}public static Object invokeInstanceMember(String name, Object target, Object arg1) //throws Exception	{	//check for field first	Type t = target.GetType();    FieldInfo f = getField(t, name,false);
    if (f != null)  //field get
        {
        f.SetValue(target,boxArg(f.FieldType,arg1));
        return arg1;
        }
    PropertyInfo p = getProperty(t, name,false);
    if (p != null)
        {
        //could be indexed property, which we otherwise aren't dealing with yet
        if(p.GetIndexParameters() != null && p.GetIndexParameters().Length == 1)
            return prepRet(p.GetValue(target, new Object[]{boxArg(p.GetIndexParameters()[0].ParameterType,arg1)}));
        p.SetValue(target,boxArg(p.PropertyType,arg1),null);
        return arg1;
        }	return invokeInstanceMethod(name, target, new Object[]{arg1});	}public static Object invokeInstanceMember(String name, Object target, params Object[] args) //throws Exception	{	return invokeInstanceMethod(name, target, args);	}

    public static FieldInfo getField(Type t, string name,bool statics)
        {
        return t.GetField(name, BindingFlags.Public | (statics ? BindingFlags.Static : BindingFlags.Instance) | BindingFlags.FlattenHierarchy);
        }
    public static PropertyInfo getProperty(Type t, string name, bool statics)
        {
        return t.GetProperty(name, BindingFlags.Public | (statics ? BindingFlags.Static : BindingFlags.Instance) | BindingFlags.FlattenHierarchy);
        }

    static public IList getMethods(Type t, int arity, String name, bool getStatics)
        {
        MethodInfo[] allmethods = t.GetMethods(BindingFlags.Public | (getStatics?BindingFlags.Static : BindingFlags.Instance) 
                                                | BindingFlags.FlattenHierarchy);
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

static Object prepRet(Object x)
   {
    if(x is Boolean)
        return ((Boolean)x)?RT.T:null;
    return x;
   }


 }
}
