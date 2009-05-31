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

namespace clojure.lang
{
    /// <summary>
    /// Adds methods that exist in Java Array class, for compatibility with core.clj.
    /// </summary>
    /// <remarks>The setters don't buy us much.  In the JVM version, they can prevent some boxing.  
    /// Here, we don't have type-specific setters in class Array, so we'll end up boxing anywa.</remarks>
    public static class ArrayHelper
    {
        //TODO: Rethink this.

        public static void setInt(int[] a, int index, int value)
        {
            a[index] = value;
        }

        public static void setShort(short[] a, int index, short value)
        {
            a[index] = value;
        }

        public static void setLong(long[] a, int index, long value)
        {
            a[index] = value;
        }

        public static void setFloat(float[] a, int index, float value)
        {
            a[index] = value;
        }

        public static void setDouble(double[] a, int index, double value)
        {
            a[index] = value;
        }

        public static void setBoolean(bool[] a, int index, bool value)
        {
            a[index] = value;
        }

        public static void setChar(char[] a, int index, char value)
        {
            a[index] = value;
        }

        public static void setByte(byte[] a, int index, byte value)
        {
            a[index] = value;
        }
       
    }
}
