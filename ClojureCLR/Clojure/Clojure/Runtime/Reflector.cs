/**
 *   Copyright (c) David Miller. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Reflection;

namespace clojure.runtime
{
    public sealed class Reflector
    {

        static public FieldInfo GetField(Type t, String name, bool getStatics)
        {
            return getStatics
                ? t.GetField(name,BindingFlags.Static)
                : t.GetField(name);
        }


        public static object CallStaticMethod(string methodName, Type t, params object[] args)
        {

            if (args.Length == 0)
            {
                FieldInfo f = t.GetField(methodName, BindingFlags.Static | BindingFlags.Public);
                if (f != null)
                    return f.GetValue(t);

                PropertyInfo p = t.GetProperty(methodName, BindingFlags.Static | BindingFlags.Public);
                if (p != null)
                    return p.GetValue(t, null);
            }

            BindingFlags flags = BindingFlags.Static | BindingFlags.Public | BindingFlags.FlattenHierarchy | BindingFlags.InvokeMethod | BindingFlags.GetField | BindingFlags.GetProperty;

            return t.InvokeMember(methodName, flags, Type.DefaultBinder, null, args);


            //IEnumerable<MethodInfo> einfo = t.GetMethods(flags).Where(mi => mi.Name == methodName && mi.GetParameters().Length == args.Length);
            //List<MethodInfo> infos = new List<MethodInfo>(einfo);

            //return InvokeMatchingMethod(methodName, infos, t, null, args);
        }




        public static object CallInstanceMethod(string methodName, object target, params object[] args)
        {
            if (args.Length == 0)
            {
                FieldInfo f = target.GetType().GetField(methodName, BindingFlags.Instance | BindingFlags.Public);
                if (f != null)
                    return f.GetValue(target);

                PropertyInfo p = target.GetType().GetProperty(methodName, BindingFlags.Instance | BindingFlags.Public);
                if (p != null)
                    return p.GetValue(target, null);
            }

            
            BindingFlags flags = BindingFlags.Instance | BindingFlags.Public | BindingFlags.FlattenHierarchy | BindingFlags.InvokeMethod;

            return target.GetType().InvokeMember(methodName, flags, Type.DefaultBinder, target, args);

            //IEnumerable<MethodInfo> einfo1 = target.GetType().GetMethods(flags);
            //List<MethodInfo> infos1 = new List<MethodInfo>(einfo1);


            //IEnumerable<MethodInfo> einfo = target.GetType().GetMethods(flags).Where(mi => mi.Name == methodName && mi.GetParameters().Length == args.Length);
            //List<MethodInfo> infos = new List<MethodInfo>(einfo);

 

            //return InvokeMatchingMethod(methodName, infos, null, target, args);
        }


        private static object InvokeMatchingMethod(string methodName, List<MethodInfo> infos, Type t, object target, object[] args)
        {

            Type targetType = t ?? target.GetType();

            if (infos.Count == 0)
                throw new InvalidOperationException(string.Format("Cannot find {0} method named: {1} for type: {2} with {3} arguments", (t == null ? "instance" : "static"), methodName, targetType.Name, args.Length));

            MethodInfo info;

            if (infos.Count == 1)
                info = infos[0];
            else
            {
                // More than one with correct arity.  Find best match.
                MethodInfo found = null;
                foreach (MethodInfo mi in infos)
                {
                    ParameterInfo[] pinfos = mi.GetParameters();
                    if (IsCongruent(pinfos, args))
                    {
                        if (found == null || Subsumes(pinfos, found.GetParameters()))
                            found = mi;
                    }
                }
                info = found;
            }

            if (info == null)
                throw new InvalidOperationException(string.Format("Cannot find static method named {0} for type: {1} with the correct argument type", methodName, t.Name));

            object[] boxedArgs = BoxArgs(info.GetParameters(), args);

            if (info.ReturnType == typeof(void))
            {
                info.Invoke(target, boxedArgs);
                return null;
            }
            else
                return info.Invoke(target, boxedArgs);
        }

        public static object InvokeConstructor(Type t, object[] args)
        {
            IEnumerable<ConstructorInfo> einfos = t.GetConstructors().Where(ci => ci.GetParameters().Length == args.Length);
            List<ConstructorInfo> infos = new List<ConstructorInfo>(einfos);

            if (infos.Count == 0)
                throw new ArgumentException("NO matching constructor found for " + t.Name);
            else if (infos.Count == 1)
            {
                ConstructorInfo info = infos[0];
                return info.Invoke(BoxArgs(info.GetParameters(), args));
            }
            else
            {
                ConstructorInfo info = null;

                // More than one with correct arity.  Find best match.
                ConstructorInfo found = null;
                foreach (ConstructorInfo ci in infos)
                {
                    ParameterInfo[] pinfos = ci.GetParameters();
                    if (IsCongruent(pinfos, args))
                    {
                        if (found == null || Subsumes(pinfos, found.GetParameters()))
                            found = ci;
                    }
                }
                info = found;


                if (info == null)
                    throw new InvalidOperationException(string.Format("Cannot find c-tor for type: {0} with the correct argument type", t.Name));

                return info.Invoke(BoxArgs(info.GetParameters(), args));
            }
        }


        private static bool Subsumes(ParameterInfo[] c1, ParameterInfo[] c2)
        {
            //presumes matching lengths
            Boolean better = false;
            for (int i = 0; i < c1.Length; i++)
            {
                Type t1 = c1[i].ParameterType;
                Type t2 = c2[i].ParameterType;
                if (t1 != t2)// || c2[i].isPrimitive() && c1[i] == Object.class))
                {
                    if (!t1.IsPrimitive && t2.IsPrimitive
                        //|| Number.class.isAssignableFrom(c1[i]) && c2[i].isPrimitive()
                       ||
                       t2.IsAssignableFrom(t1))
                        better = true;
                    else
                        return false;
                }
            }
            return better;
        }



        private static object[] BoxArgs(ParameterInfo[] pinfos, object[] args)
        {
            if (pinfos.Length == 0)
                return null;
            object[] ret = new object[pinfos.Length];
            for (int i = 0; i < pinfos.Length; i++)
                ret[i] = BoxArg(pinfos[i], args[i]);
            return ret;
        }

        // I can't remember what problem we are trying to solve.
        // Here is the code that solved the first problem.
        // However, it was messing up an Keyword -> IFn conversion, which should be possible.

        //private static object BoxArg(ParameterInfo pinfo, object arg)
        //{
        //    Type paramType = pinfo.ParameterType;

        //    if (!paramType.IsPrimitive)
        //        return Convert.ChangeType(arg, pinfo.ParameterType);

        //    return Convert.ChangeType(arg, pinfo.ParameterType);  // don't know yet what we need here
        //}

        // Here is an improved version, until we figure out the problem.

        private static object BoxArg(ParameterInfo pinfo, object arg)
        {
            Type paramType = pinfo.ParameterType;
            Type argType = arg.GetType();

            if (!paramType.IsPrimitive)
                return arg;

            return Convert.ChangeType(arg, pinfo.ParameterType);  // don't know yet what we need here
        }





        private static bool IsCongruent(ParameterInfo[] pinfos, object[] args)
        {
            bool ret = false;
            if (args == null)
                return pinfos.Length == 0;
            if (pinfos.Length == args.Length)
            {
                ret = true;
                for (int i = 0; ret && i < pinfos.Length; i++)
                {
                    object arg = args[i];
                    Type argType = (arg == null ? null : arg.GetType());
                    Type paramType = pinfos[i].ParameterType;
                    ret = ParamArgTypeMatch(paramType, argType);
                }
            }

            return ret;
        }

        private static bool ParamArgTypeMatch(Type paramType, Type argType)
        {
            if (argType == null)
                return !paramType.IsPrimitive;
            return AreAssignable(paramType, argType);
        }

        // Stolen from DLR TypeUtils
        internal static bool AreAssignable(Type dest, Type src)
        {
            if (dest == src)
            {
                return true;
            }
            if (dest.IsAssignableFrom(src))
            {
                return true;
            }
            if (dest.IsArray && src.IsArray && dest.GetArrayRank() == src.GetArrayRank() && AreReferenceAssignable(dest.GetElementType(), src.GetElementType()))
            {
                return true;
            }
            if (src.IsArray && dest.IsGenericType &&
                (dest.GetGenericTypeDefinition() == typeof(System.Collections.Generic.IEnumerable<>)
                || dest.GetGenericTypeDefinition() == typeof(System.Collections.Generic.IList<>)
                || dest.GetGenericTypeDefinition() == typeof(System.Collections.Generic.ICollection<>))
                && dest.GetGenericArguments()[0] == src.GetElementType())
            {
                return true;
            }
            return false;
        }

        // Stolen from DLR TypeUtils
        internal static bool AreReferenceAssignable(Type dest, Type src)
        {
            // WARNING: This actually implements "Is this identity assignable and/or reference assignable?"
            if (dest == src)
            {
                return true;
            }
            if (!dest.IsValueType && !src.IsValueType && AreAssignable(dest, src))
            {
                return true;
            }
            return false;
        }
    }
}
