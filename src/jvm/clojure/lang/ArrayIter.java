/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 *    the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

package clojure.lang;

import java.lang.reflect.Array;
import java.util.Iterator;

public class ArrayIter implements Iterator {
final Object[] array;
int i;

static public Iterator EMPTY_ITERATOR = new Iterator() {
  public boolean hasNext() { return false; }
  public Object next() { throw new java.util.NoSuchElementException(); }
  public void remove() { throw new UnsupportedOperationException("remove() not supported"); }
};

static public Iterator create(){
  return EMPTY_ITERATOR;
}

static public Iterator create(Object... array){
  if(array == null || array.length == 0)
    return EMPTY_ITERATOR;
  return new ArrayIter(array, 0);
}

static public Iterator createFromObject(Object array){
  if(array == null || Array.getLength(array) == 0)
    return EMPTY_ITERATOR;
  Class aclass = array.getClass();
  if(aclass == int[].class)
    return new ArrayIter_int((int[]) array, 0);
  if(aclass == float[].class)
    return new ArrayIter_float((float[]) array, 0);
  if(aclass == double[].class)
    return new ArrayIter_double((double[]) array, 0);
  if(aclass == long[].class)
    return new ArrayIter_long((long[]) array, 0);
  if(aclass == byte[].class)
    return new ArrayIter_byte((byte[]) array, 0);
  if(aclass == char[].class)
    return new ArrayIter_char((char[]) array, 0);
  if(aclass == short[].class)
    return new ArrayIter_short((short[]) array, 0);
  if(aclass == boolean[].class)
    return new ArrayIter_boolean((boolean[]) array, 0);
  return new ArrayIter(array, 0);
}

ArrayIter(Object array, int i){
  this.i = i;
  this.array = (Object[]) array;
}

public boolean hasNext() {
  return array != null && i < array.length;
}

public Object next() {
  if(array != null && i < array.length)
    return array[i++];
  throw new java.util.NoSuchElementException();
}

public void remove() {
  throw new UnsupportedOperationException("remove() not supported");
}

//////////////////////////////////// specialized primitive versions ///////////////////////////////

static public class ArrayIter_int implements Iterator<Long> {
  final int[] array;
  int i;

  ArrayIter_int(int[] array, int i){
    this.array = array;
    this.i = i;
  }

  public boolean hasNext() {
    return array != null && i < array.length;
  }

  public Long next() {
    if(array != null && i < array.length)
      return Long.valueOf(array[i++]);
    throw new java.util.NoSuchElementException();
  }

  public void remove() {
    throw new UnsupportedOperationException("remove() not supported");
  }
}

static public class ArrayIter_float implements Iterator<Double> {
  final float[] array;
  int i;

  ArrayIter_float(float[] array, int i){
    this.array = array;
    this.i = i;
  }

  public boolean hasNext() {
    return array != null && i < array.length;
  }

  public Double next() {
    if(array != null && i < array.length)
      return Double.valueOf(array[i++]);
    throw new java.util.NoSuchElementException();
  }

  public void remove() {
    throw new UnsupportedOperationException("remove() not supported");
  }
}

static public class ArrayIter_double implements Iterator<Double> {
  final double[] array;
  int i;

  ArrayIter_double(double[] array, int i){
    this.array = array;
    this.i = i;
  }

  public boolean hasNext() {
    return array != null && i < array.length;
  }

  public Double next() {
    if(array != null && i < array.length)
      return array[i++];
    throw new java.util.NoSuchElementException();
  }

  public void remove() {
    throw new UnsupportedOperationException("remove() not supported");
  }

}

static public class ArrayIter_long implements Iterator<Long> {
  final long[] array;
  int i;

  ArrayIter_long(long[] array, int i){
    this.array = array;
    this.i = i;
  }

  public boolean hasNext() {
    return array != null && i < array.length;
  }

  public Long next() {
    if(array != null && i < array.length)
      return Long.valueOf(array[i++]);
    throw new java.util.NoSuchElementException();
  }

  public void remove() {
    throw new UnsupportedOperationException("remove() not supported");
  }

}

static public class ArrayIter_byte implements Iterator<Byte> {
  final byte[] array;
  int i;

  ArrayIter_byte(byte[] array, int i){
    this.array = array;
    this.i = i;
  }

  public boolean hasNext() {
    return array != null && i < array.length;
  }

  public Byte next() {
    if(array != null && i < array.length)
      return array[i++];
    throw new java.util.NoSuchElementException();
  }

  public void remove() {
    throw new UnsupportedOperationException("remove() not supported");
  }

}

static public class ArrayIter_char implements Iterator<Character> {
  final char[] array;
  int i;

  ArrayIter_char(char[] array, int i){
    this.array = array;
    this.i = i;
  }

  public boolean hasNext() {
    return array != null && i < array.length;
  }

  public Character next() {
    if(array != null && i < array.length)
      return array[i++];
    throw new java.util.NoSuchElementException();
  }

  public void remove() {
    throw new UnsupportedOperationException("remove() not supported");
  }

}

static public class ArrayIter_short implements Iterator<Long> {
  final short[] array;
  int i;

  ArrayIter_short(short[] array, int i){
    this.array = array;
    this.i = i;
  }

  public boolean hasNext() {
    return array != null && i < array.length;
  }

  public Long next() {
    if(array != null && i < array.length)
      return Long.valueOf(array[i++]);
    throw new java.util.NoSuchElementException();
  }

  public void remove() {
    throw new UnsupportedOperationException("remove() not supported");
  }

}

static public class ArrayIter_boolean implements Iterator<Boolean> {
  final boolean[] array;
  int i;

  ArrayIter_boolean(boolean[] array, int i){
    this.array = array;
    this.i = i;
  }

  public boolean hasNext() {
    return array != null && i < array.length;
  }

  public Boolean next() {
    if(array != null && i < array.length)
      return Boolean.valueOf(array[i++]);
    throw new java.util.NoSuchElementException();
  }

  public void remove() {
    throw new UnsupportedOperationException("remove() not supported");
  }

}

}
