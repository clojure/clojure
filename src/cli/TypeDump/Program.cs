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
using System.IO;
using System.Reflection;

namespace TypeDump
    {
    class Program
        {
        static void Main(string[] args)
            {
            if (args.Length < 2)
                {
                Console.Error.WriteLine("usage: typedump assembly namespace [namespace ...]");
                return;
                }
            Assembly a = Assembly.Load(new AssemblyName(args[0]));
            Console.WriteLine('(');
            foreach (Type t in a.GetExportedTypes())
                {
                if(Array.IndexOf(args,t.Namespace,1) >= 0
                    //we don't deal with generics
                    && !(t.IsGenericTypeDefinition || t.IsGenericType))
                    dumpType(t, Console.Out); 
                }
            Console.WriteLine(')');
            }

        static bool hasGenericOrRefParam(ParameterInfo[] args)
            {
            return  Array.Find(args, delegate(ParameterInfo pi)
                                    {
                                        Type pit = pi.ParameterType;
                                        return pit.IsGenericTypeDefinition || pit.IsGenericType ||pit.IsByRef;
                                    })
                                    != null;
            }

        static void dumpType(Type t, TextWriter p)
            {
            p.Write('(');
            p.Write("(:name \"" + t + "\")");
            if(t.BaseType != null)
                p.Write(" (:super \"" + t.BaseType + "\")");

            Type[] interfaces = t.GetInterfaces();
            if (interfaces.Length > 0)
                {
                p.WriteLine();
                p.Write(" (:interfaces");
                foreach (Type it in interfaces)
                    {
                    if (!(it.IsGenericTypeDefinition || it.IsGenericType))
                        p.Write(" \"" + it + "\"");

                    }
                p.Write(')');
                }
            foreach (ConstructorInfo ci in t.GetConstructors(BindingFlags.Public|BindingFlags.Instance))
                {
                //this should filter for pointer args etc
                Object[] clsattr = ci.GetCustomAttributes(typeof(CLSCompliantAttribute), false);
                if (clsattr.Length > 0 && !((CLSCompliantAttribute)clsattr[0]).IsCompliant)
                    continue;
                ParameterInfo[] args = ci.GetParameters();
                //we don't deal with generics
                if (hasGenericOrRefParam(args))
                    continue;
                p.WriteLine();
                p.Write(" (:ctor (:arity " + args.Length + ")");
                if (args.Length > 0
                    && args[args.Length - 1].GetCustomAttributes(typeof(ParamArrayAttribute), false).Length > 0)
                    {
                    p.Write("(:varargs t)");
                    }
                if (args.Length > 0)
                    {
                    p.WriteLine();
                    p.Write("  (:args");
                    foreach (ParameterInfo pi in args)
                        {
                        p.Write(" \"" + pi.ParameterType + "\"");
                        }
                    p.Write(')');
                    }
                p.Write(')');
                }
            foreach (MethodInfo mi in t.GetMethods(BindingFlags.Public | BindingFlags.Instance |BindingFlags.Static))
                {
                //this should filter for pointer args etc
                Object[] clsattr = mi.GetCustomAttributes(typeof(CLSCompliantAttribute), false);
                if (clsattr.Length > 0 && !((CLSCompliantAttribute)clsattr[0]).IsCompliant)
                    continue;
                ParameterInfo[] args = mi.GetParameters();
                //we don't deal with generics
                if (hasGenericOrRefParam(args) ||mi.ReturnType.IsGenericType)
                    continue;
                p.WriteLine();
                p.Write(" (:method (:name \"" + mi.Name + "\") (:arity " + args.Length + ")(:ret \"" + mi.ReturnType + "\")");
                if (mi.IsStatic)
                    {
                    p.Write("  (:static t)");
                    }
                if (args.Length > 0
                    && args[args.Length - 1].GetCustomAttributes(typeof(ParamArrayAttribute), false).Length > 0)
                    {
                    p.Write("  (:varargs t)");
                    }
                if (mi.IsSpecialName)
                    {
                    p.Write("  (:special t)");
                    }
                if (args.Length > 0)
                    {
                    p.WriteLine();
                    p.Write("  (:args");
                    foreach (ParameterInfo pi in args)
                        {
                        p.Write(" \"" + pi.ParameterType + "\"");
                        }
                    p.Write(')');
                    }
                p.Write(')');
                }
            foreach (FieldInfo fi in t.GetFields(BindingFlags.Public | BindingFlags.Instance | BindingFlags.Static))
                {
                p.WriteLine();
                p.Write(" (:field (:name \"");
                p.Write(fi.Name);
                p.Write("\") (:type \"");
                p.Write(fi.FieldType);
                p.Write("\")");
                if (fi.IsStatic)
                    {
                    p.Write(" (:static t)");
                    if (fi.IsLiteral)
                        {
                        Object v = fi.GetRawConstantValue();
                        p.Write(" (:const-value ");
                        if (v is String)
                            p.Write('"');
                        p.Write(v);
                        if (v is String)
                            p.Write('"');
                        p.Write(")");

                        }
                    }
                if (fi.IsSpecialName)
                    {
                    p.Write("  (:special t)");
                    }
                p.Write(")");

                }

            p.WriteLine(')');
            }
        }
    }
