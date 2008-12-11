/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Dec 7, 2008 */

package clojure.lang;

import java.util.concurrent.atomic.AtomicInteger;
import java.lang.reflect.Array;

public class ArrayStream implements IStream {

    final AtomicInteger ai = new AtomicInteger(0);
    final Object[] array;

    public ArrayStream(Object[] array) {
        this.array = array;
    }

    public Object next() throws Exception {
        int i = ai.getAndIncrement();
        if (i < array.length)
            return array[i];
        return RT.eos();
    }

    static IStream createFromObject(Object array) {
        Class aclass = array.getClass().getComponentType();
        if (!aclass.isPrimitive())
            return new ArrayStream((Object[]) array);
        if (aclass == int.class)
            return new ArrayStream_int((int[]) array);
        if (aclass == long.class)
            return new ArrayStream_long((long[]) array);
        if (aclass == float.class)
            return new ArrayStream_float((float[]) array);
        if (aclass == double.class)
            return new ArrayStream_double((double[]) array);
        if (aclass == char.class)
            return new ArrayStream_char((char[]) array);
        if (aclass == byte.class)
            return new ArrayStream_byte((byte[]) array);
        if (aclass == short.class)
            return new ArrayStream_short((short[]) array);
        if (aclass == boolean.class)
            return new ArrayStream_boolean((boolean[]) array);
        throw new IllegalArgumentException(String.format("Unsupported array type %s", array));
    }

    static public class ArrayStream_int implements IStream {

        final AtomicInteger ai = new AtomicInteger(0);
        final int[] array;

        public ArrayStream_int(int[] array) {
            this.array = array;
        }

        public Object next() throws Exception {
            int i = ai.getAndIncrement();
            if (i < array.length)
                return array[i];
            return RT.eos();
        }
    }

    static public class ArrayStream_long implements IStream {

        final AtomicInteger ai = new AtomicInteger(0);
        final long[] array;

        public ArrayStream_long(long[] array) {
            this.array = array;
        }

        public Object next() throws Exception {
            int i = ai.getAndIncrement();
            if (i < array.length)
                return array[i];
            return RT.eos();
        }
    }

    static public class ArrayStream_float implements IStream {

        final AtomicInteger ai = new AtomicInteger(0);
        final float[] array;

        public ArrayStream_float(float[] array) {
            this.array = array;
        }

        public Object next() throws Exception {
            int i = ai.getAndIncrement();
            if (i < array.length)
                return array[i];
            return RT.eos();
        }
    }

    static public class ArrayStream_double implements IStream {

        final AtomicInteger ai = new AtomicInteger(0);
        final double[] array;

        public ArrayStream_double(double[] array) {
            this.array = array;
        }

        public Object next() throws Exception {
            int i = ai.getAndIncrement();
            if (i < array.length)
                return array[i];
            return RT.eos();
        }
    }

    static public class ArrayStream_char implements IStream {

        final AtomicInteger ai = new AtomicInteger(0);
        final char[] array;

        public ArrayStream_char(char[] array) {
            this.array = array;
        }

        public Object next() throws Exception {
            int i = ai.getAndIncrement();
            if (i < array.length)
                return array[i];
            return RT.eos();
        }
    }

    static public class ArrayStream_byte implements IStream {

        final AtomicInteger ai = new AtomicInteger(0);
        final byte[] array;

        public ArrayStream_byte(byte[] array) {
            this.array = array;
        }

        public Object next() throws Exception {
            int i = ai.getAndIncrement();
            if (i < array.length)
                return array[i];
            return RT.eos();
        }
    }

    static public class ArrayStream_short implements IStream {

        final AtomicInteger ai = new AtomicInteger(0);
        final short[] array;

        public ArrayStream_short(short[] array) {
            this.array = array;
        }

        public Object next() throws Exception {
            int i = ai.getAndIncrement();
            if (i < array.length)
                return array[i];
            return RT.eos();
        }
    }

    static public class ArrayStream_boolean implements IStream {

        final AtomicInteger ai = new AtomicInteger(0);
        final boolean[] array;

        public ArrayStream_boolean(boolean[] array) {
            this.array = array;
        }

        public Object next() throws Exception {
            int i = ai.getAndIncrement();
            if (i < array.length)
                return array[i];
            return RT.eos();
        }
    }


}
