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

    public abstract class ArrayStreamBase<T> : AFn
    {
        #region Data

        int _i = 0;
        readonly T[] _array;

        #endregion

        #region C-tors

        public ArrayStreamBase(T[] array)
        {
            _array = array;
        }

        #endregion

        #region Implementation

        public override object invoke()
        {
            if (_i < _array.Length)
                return _array[_i++];
            return RT.EOS;
        }
        #endregion
    }


    // TODO: ArrayStream needs some thought.
    public class ArrayStream : ArrayStreamBase<Object>
    {

        public ArrayStream(Object[] array)
            : base(array)
        {
        }

        public static Stream createFromObject(object array)
        {
            Type aType = array.GetType().GetElementType();

            if (!aType.IsPrimitive)
                return new Stream(new ArrayStream((Object[])array));

            switch (Type.GetTypeCode(aType))
            {
                case TypeCode.Char:
                    return new Stream(new ArrayStream_char((char[])array));
                case TypeCode.SByte:
                    return new Stream(new ArrayStream_sbyte((sbyte[])array));
                case TypeCode.Byte:
                    return new Stream(new ArrayStream_byte((byte[])array));
                case TypeCode.Int16:
                    return new Stream(new ArrayStream_short((short[])array));
                case TypeCode.Int32:
                    return new Stream(new ArrayStream_int((int[])array));
                case TypeCode.Int64:
                    return new Stream(new ArrayStream_long((long[])array));
                case TypeCode.Double:
                    return new Stream(new ArrayStream_double((double[])array));
                case TypeCode.Single:
                    return new Stream(new ArrayStream_float((float[])array));
                case TypeCode.UInt16:
                    return new Stream(new ArrayStream_ushort((ushort[])array));
                case TypeCode.UInt32:
                    return new Stream(new ArrayStream_uint((uint[])array));
                case TypeCode.UInt64:
                    return new Stream(new ArrayStream_ulong((ulong[])array));
                case TypeCode.Decimal:
                    return new Stream(new ArrayStream_decimal((decimal[])array));
                case TypeCode.Boolean:
                    return new Stream(new ArrayStream_bool((bool[])array));

            }


            // TODO: make sure we don't have a multi-dim array

            throw new ArgumentException(String.Format("Unsupported array type %s", array.GetType()));
        }

        class ArrayStream_char : ArrayStreamBase<char>
        {
            public ArrayStream_char(char[] array)
                : base(array)
            {
            }
        }

        class ArrayStream_sbyte : ArrayStreamBase<sbyte>
        {
            public ArrayStream_sbyte(sbyte[] array)
                : base(array)
            {
            }
        }
        class ArrayStream_byte : ArrayStreamBase<byte>
        {
            public ArrayStream_byte(byte[] array)
                : base(array)
            {
            }
        }
        class ArrayStream_short : ArrayStreamBase<short>
        {
            public ArrayStream_short(short[] array)
                : base(array)
            {
            }
        }
        class ArrayStream_int : ArrayStreamBase<int>
        {
            public ArrayStream_int(int[] array)
                : base(array)
            {
            }
        }
        class ArrayStream_long : ArrayStreamBase<long>
        {
            public ArrayStream_long(long[] array)
                : base(array)
            {
            }
        }
        class ArrayStream_double : ArrayStreamBase<double>
        {
            public ArrayStream_double(double[] array)
                : base(array)
            {
            }
        }
        class ArrayStream_float : ArrayStreamBase<float>
        {
            public ArrayStream_float(float[] array)
                : base(array)
            {
            }
        }
        class ArrayStream_ushort : ArrayStreamBase<ushort>
        {
            public ArrayStream_ushort(ushort[] array)
                : base(array)
            {
            }
        }
        class ArrayStream_uint : ArrayStreamBase<uint>
        {
            public ArrayStream_uint(uint[] array)
                : base(array)
            {
            }
        }
        class ArrayStream_ulong : ArrayStreamBase<ulong>
        {
            public ArrayStream_ulong(ulong[] array)
                : base(array)
            {
            }
        }

        class ArrayStream_decimal : ArrayStreamBase<decimal>
        {
            public ArrayStream_decimal(decimal[] array)
                : base(array)
            {
            }
        }

        class ArrayStream_bool : ArrayStreamBase<bool>
        {
            public ArrayStream_bool(bool[] array)
                : base(array)
            {
            }
        }
    }
}
