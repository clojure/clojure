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
using java.math;

namespace clojure.lang
{
    public class Util
    {

        static public int Hash(object o)
        {
            return o == null ? 0 : o.GetHashCode();
        }

        static public int HashCombine(int seed, int hash)
        {
            //a la boost
            return (int)(seed ^ (hash + 0x9e3779b9 + (seed << 6) + (seed >> 2)));

        }


        static public bool equiv(object k1, object k2)
        {
            if (k1 == k2)
                return true;
            if (k1 != null)
            {
                if (IsNumeric(k1))
                    return Numbers.equiv(k1, k2);
                else if (k1 is IPersistentCollection && k2 is IPersistentCollection)
                    return ((IPersistentCollection)k1).equiv(k2);
                return k1.Equals(k2);
            }
            return false;
        }

        public static bool equals(object k1, object k2)
        {
            // Changed in Rev 1215
            //if(k1 == k2)
            //    return true;
	
            //if(k1 != null)
            //{
            //    if (IsNumeric(k1))
            //        return Numbers.equiv(k1, k2);

            //    return k1.Equals(k2);
            //}
	    
            //return false;
            if (k1 == k2)
                return true;
            return k1 != null && k1.Equals(k2);
        }

        public static int compare(object k1, object k2)
        {
            if (k1 == k2)
                return 0;
            if (k1 != null)
            {
                if (k2 == null)
                    return 1;
                if (IsNumeric(k1))
                    return Numbers.compare(k1, k2);
                return ((IComparable)k1).CompareTo(k2);
            }
            return -1;
        }


        public static int ConvertToInt(object o)
        {
            // ToInt32 rounds.  We need truncation.
            return (int)Convert.ToDouble(o);
        }

        public static long ConvertToLong(object o)
        {
            // ToInt64 rounds.  We need truncation.
            return (long)Convert.ToDouble(o);
        }

        public static float ConvertToFloat(object o)
        {
            return (float)Convert.ToDouble(o);
        }

        public static double ConvertToDouble(object o)
        {
            return ConvertToDouble(o);
        }

        public static bool IsNumeric(object o)
        {
            return o != null && IsNumeric(o.GetType());
        }


        public static int BitCount(int bitMask)
        { 
            bitMask -= ((bitMask >> 1) & 0x55555555);
            bitMask = (((bitMask >> 2) & 0x33333333) + (bitMask & 0x33333333));
            bitMask = (((bitMask >> 4) + bitMask) & 0x0f0f0f0f);
            return ((bitMask * 0x01010101) >> 24);
        }

        public static int Mask(int hash, int shift)
        {
        	return (hash >> shift) & 0x01f;
        }


        public static bool IsPrimitive(Type t)
        {
            return t != null && t.IsPrimitive && t != typeof(void);
        }

        #region core.clj compatibility

        public static int hash(object o)
        {
            return Hash(o);
        }

        #endregion

        #region Stolen code
        // The following code is from Microsoft's DLR..
        // It had the following notice:

        /* ****************************************************************************
         *
         * Copyright (c) Microsoft Corporation. 
         *
         * This source code is subject to terms and conditions of the Microsoft Public License. A 
         * copy of the license can be found in the License.html file at the root of this distribution. If 
         * you cannot locate the  Microsoft Public License, please send an email to 
         * dlr@microsoft.com. By using this source code in any fashion, you are agreeing to be bound 
         * by the terms of the Microsoft Public License.
         *
         * You must not remove this notice, or any other, from this software.
         *
         *
         * ***************************************************************************/



        internal static Type GetNonNullableType(Type type)
        {
            if (IsNullableType(type))
            {
                return type.GetGenericArguments()[0];
            }
            return type;
        }

        internal static bool IsNullableType(Type type)
        {
            return type.IsGenericType && type.GetGenericTypeDefinition() == typeof(Nullable<>);
        }


        internal static bool IsNumeric(Type type)
        {
            type = GetNonNullableType(type);
            if (!type.IsEnum)
            {
                switch (Type.GetTypeCode(type))
                {
                    case TypeCode.Char:
                    case TypeCode.SByte:
                    case TypeCode.Byte:
                    case TypeCode.Int16:
                    case TypeCode.Int32:
                    case TypeCode.Int64:
                    case TypeCode.Double:
                    case TypeCode.Single:
                    case TypeCode.UInt16:
                    case TypeCode.UInt32:
                    case TypeCode.UInt64:
                        return true;
                }
                if (type == typeof(BigInteger) || type == typeof(BigDecimal))
                    return true;
            }
            return false;
        }

        #endregion


        internal static Exception UnreachableCode()
        {
            return new InvalidOperationException("Invalid value in switch: default should not be reached.");
        }
    }
}
