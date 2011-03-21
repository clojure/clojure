/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Jun 19, 2006 */

package clojure.lang;

import java.lang.reflect.Array;

public class ArraySeq extends ASeq implements IndexedSeq, IReduce{
public final Object array;
final int i;
final Object[] oa;
final Class ct;
//ISeq _rest;

static public ArraySeq create(){
	return null;
}

static public ArraySeq create(Object... array){
	if(array == null || array.length == 0)
		return null;
	return new ArraySeq(array, 0);
}

static ISeq createFromObject(Object array){
	if(array == null || Array.getLength(array) == 0)
		return null;
	Class aclass = array.getClass();
	if(aclass == int[].class)
		return new ArraySeq_int(null, (int[]) array, 0);
	if(aclass == float[].class)
		return new ArraySeq_float(null, (float[]) array, 0);
	if(aclass == double[].class)
		return new ArraySeq_double(null, (double[]) array, 0);
	if(aclass == long[].class)
		return new ArraySeq_long(null, (long[]) array, 0);
	if(aclass == byte[].class)
		return new ArraySeq_byte(null, (byte[]) array, 0);
	if(aclass == char[].class)
		return new ArraySeq_char(null, (char[]) array, 0);
	if(aclass == boolean[].class)
		return new ArraySeq_boolean(null, (boolean[]) array, 0);
	return new ArraySeq(array, 0);
}

ArraySeq(Object array, int i){
	this.array = array;
	this.ct = array.getClass().getComponentType();
	this.i = i;
	this.oa = (Object[]) (array instanceof Object[] ? array : null);
//    this._rest = this;
}

ArraySeq(IPersistentMap meta, Object array, int i){
	super(meta);
	this.array = array;
	this.ct = array.getClass().getComponentType();
	this.i = i;
	this.oa = (Object[]) (array instanceof Object[] ? array : null);
}

public Object first(){
	if(oa != null)
		return oa[i];
	return Reflector.prepRet(ct, Array.get(array, i));
}

public ISeq next(){
	if(oa != null)
		{
		if(i + 1 < oa.length)
			return new ArraySeq(array, i + 1);
		}
	else
		{
		if(i + 1 < Array.getLength(array))
			return new ArraySeq(array, i + 1);
		}
	return null;
}

public int count(){
	if(oa != null)
		return oa.length - i;
	return Array.getLength(array) - i;
}

public int index(){
	return i;
}

public ArraySeq withMeta(IPersistentMap meta){
	return new ArraySeq(meta, array, i);
}

public Object reduce(IFn f) {
	if(oa != null)
		{
		Object ret = oa[i];
		for(int x = i + 1; x < oa.length; x++)
			ret = f.invoke(ret, oa[x]);
		return ret;
		}

	Object ret = Reflector.prepRet(ct, Array.get(array, i));
	for(int x = i + 1; x < Array.getLength(array); x++)
		ret = f.invoke(ret, Reflector.prepRet(ct, Array.get(array, x)));
	return ret;
}

public Object reduce(IFn f, Object start) {
	if(oa != null)
		{
		Object ret = f.invoke(start, oa[i]);
		for(int x = i + 1; x < oa.length; x++)
			ret = f.invoke(ret, oa[x]);
		return ret;
		}
	Object ret = f.invoke(start, Reflector.prepRet(ct, Array.get(array, i)));
	for(int x = i + 1; x < Array.getLength(array); x++)
		ret = f.invoke(ret, Reflector.prepRet(ct, Array.get(array, x)));
	return ret;
}

public int indexOf(Object o) {
	if (oa != null) {
		for (int j = i; j < oa.length; j++)
			if (Util.equals(o, oa[j])) return j - i;
	} else {
		int n = Array.getLength(array); 
		for (int j = i; j < n; j++)
			if (Util.equals(o, Reflector.prepRet(ct, Array.get(array, j)))) return j - i;
	}
	return -1;
}

public int lastIndexOf(Object o) {
	if (oa != null) {
		if (o == null) {
			for (int j = oa.length - 1 ; j >= i; j--)
				if (oa[j] == null) return j - i;
		} else {
			for (int j = oa.length - 1 ; j >= i; j--)
				if (o.equals(oa[j])) return j - i;
		}
	} else {
		if (o == null) {
			for (int j = Array.getLength(array) - 1 ; j >= i; j--)
				if (Reflector.prepRet(ct, Array.get(array, j)) == null) return j - i;
		} else {
			for (int j = Array.getLength(array) - 1 ; j >= i; j--)
				if (o.equals(Reflector.prepRet(ct, Array.get(array, j)))) return j - i;
		}
	}
	return -1;
}

//////////////////////////////////// specialized primitive versions ///////////////////////////////

static public class ArraySeq_int extends ASeq implements IndexedSeq, IReduce{
	public final int[] array;
	final int i;

	ArraySeq_int(IPersistentMap meta, int[] array, int i){
		super(meta);
		this.array = array;
		this.i = i;
	}

	public Object first(){
		return array[i];
	}

	public ISeq next(){
		if(i + 1 < array.length)
			return new ArraySeq_int(meta(), array, i + 1);
		return null;
	}

	public int count(){
		return array.length - i;
	}

	public int index(){
		return i;
	}

	public ArraySeq_int withMeta(IPersistentMap meta){
		return new ArraySeq_int(meta, array, i);
	}

	public Object reduce(IFn f) {
		Object ret = array[i];
		for(int x = i + 1; x < array.length; x++)
			ret = f.invoke(ret, array[x]);
		return ret;
	}

	public Object reduce(IFn f, Object start) {
		Object ret = f.invoke(start, array[i]);
		for(int x = i + 1; x < array.length; x++)
			ret = f.invoke(ret, array[x]);
		return ret;
	}

	public int indexOf(Object o) {
		if (o instanceof Number) {
			int k = ((Number) o).intValue();
			for (int j = i; j < array.length; j++)
				if (k == array[j]) return j - i;
		}

		return -1;
	}
	
	public int lastIndexOf(Object o) {
		if (o instanceof Number) {
			int k = ((Number) o).intValue();
			for (int j = array.length - 1; j >= i; j--)
				if (k == array[j]) return j - i;
		}

		return -1;
	}
}


static public class ArraySeq_float extends ASeq implements IndexedSeq, IReduce{
	public final float[] array;
	final int i;

	ArraySeq_float(IPersistentMap meta, float[] array, int i){
		super(meta);
		this.array = array;
		this.i = i;
	}

	public Object first(){
		return Numbers.num(array[i]);
	}

	public ISeq next(){
		if(i + 1 < array.length)
			return new ArraySeq_float(meta(), array, i + 1);
		return null;
	}

	public int count(){
		return array.length - i;
	}

	public int index(){
		return i;
	}

	public ArraySeq_float withMeta(IPersistentMap meta){
		return new ArraySeq_float(meta, array, i);
	}

	public Object reduce(IFn f) {
		Object ret = Numbers.num(array[i]);
		for(int x = i + 1; x < array.length; x++)
			ret = f.invoke(ret, Numbers.num(array[x]));
		return ret;
	}

	public Object reduce(IFn f, Object start) {
		Object ret = f.invoke(start, Numbers.num(array[i]));
		for(int x = i + 1; x < array.length; x++)
			ret = f.invoke(ret, Numbers.num(array[x]));
		return ret;
	}

	public int indexOf(Object o) {
		if (o instanceof Number) {
			float f = ((Number) o).floatValue();
			for (int j = i; j < array.length; j++)
				if (f == array[j]) return j - i;
		}
		return -1;
	}
	
	public int lastIndexOf(Object o) {
		if (o instanceof Number) {
			float f = ((Number) o).floatValue();
			for (int j = array.length - 1; j >= i; j--)
				if (f == array[j]) return j - i;
		}
		return -1;
	}
}

static public class ArraySeq_double extends ASeq implements IndexedSeq, IReduce{
	public final double[] array;
	final int i;

	ArraySeq_double(IPersistentMap meta, double[] array, int i){
		super(meta);
		this.array = array;
		this.i = i;
	}

	public Object first(){
		return array[i];
	}

	public ISeq next(){
		if(i + 1 < array.length)
			return new ArraySeq_double(meta(), array, i + 1);
		return null;
	}

	public int count(){
		return array.length - i;
	}

	public int index(){
		return i;
	}

	public ArraySeq_double withMeta(IPersistentMap meta){
		return new ArraySeq_double(meta, array, i);
	}

	public Object reduce(IFn f) {
		Object ret = array[i];
		for(int x = i + 1; x < array.length; x++)
			ret = f.invoke(ret, array[x]);
		return ret;
	}

	public Object reduce(IFn f, Object start) {
		Object ret = f.invoke(start, array[i]);
		for(int x = i + 1; x < array.length; x++)
			ret = f.invoke(ret, array[x]);
		return ret;
	}

	public int indexOf(Object o) {
		if (o instanceof Number) {
			double d = ((Number) o).doubleValue();
			for (int j = i; j < array.length; j++)
				if (d == array[j]) return j - i;
		}

		return -1;
	}
	
	public int lastIndexOf(Object o) {
		if (o instanceof Number) {
			double d = ((Number) o).doubleValue();
			for (int j = array.length - 1; j >= i; j--)
				if (d == array[j]) return j - i;
		}

		return -1;
	}
}

static public class ArraySeq_long extends ASeq implements IndexedSeq, IReduce{
	public final long[] array;
	final int i;

	ArraySeq_long(IPersistentMap meta, long[] array, int i){
		super(meta);
		this.array = array;
		this.i = i;
	}

	public Object first(){
		return Numbers.num(array[i]);
	}

	public ISeq next(){
		if(i + 1 < array.length)
			return new ArraySeq_long(meta(), array, i + 1);
		return null;
	}

	public int count(){
		return array.length - i;
	}

	public int index(){
		return i;
	}

	public ArraySeq_long withMeta(IPersistentMap meta){
		return new ArraySeq_long(meta, array, i);
	}

	public Object reduce(IFn f) {
		Object ret = Numbers.num(array[i]);
		for(int x = i + 1; x < array.length; x++)
			ret = f.invoke(ret, Numbers.num(array[x]));
		return ret;
	}

	public Object reduce(IFn f, Object start) {
		Object ret = f.invoke(start, Numbers.num(array[i]));
		for(int x = i + 1; x < array.length; x++)
			ret = f.invoke(ret, Numbers.num(array[x]));
		return ret;
	}

	public int indexOf(Object o) {
		if (o instanceof Number) {
			long l = ((Number) o).longValue();
			for (int j = i; j < array.length; j++)
				if (l == array[j]) return j - i;
		}

		return -1;
	}
	
	public int lastIndexOf(Object o) {
		if (o instanceof Number) {
			long l = ((Number) o).longValue();
			for (int j = array.length - 1; j >= i; j--)
				if (l == array[j]) return j - i;
		}

		return -1;
	}
}

static public class ArraySeq_byte extends ASeq implements IndexedSeq, IReduce{
	public final byte[] array;
	final int i;

	ArraySeq_byte(IPersistentMap meta, byte[] array, int i){
		super(meta);
		this.array = array;
		this.i = i;
	}

	public Object first(){
		return array[i];
	}

	public ISeq next(){
		if(i + 1 < array.length)
			return new ArraySeq_byte(meta(), array, i + 1);
		return null;
	}

	public int count(){
		return array.length - i;
	}

	public int index(){
		return i;
	}

	public ArraySeq_byte withMeta(IPersistentMap meta){
		return new ArraySeq_byte(meta, array, i);
	}

	public Object reduce(IFn f) {
		Object ret = array[i];
		for(int x = i + 1; x < array.length; x++)
			ret = f.invoke(ret, array[x]);
		return ret;
	}

	public Object reduce(IFn f, Object start) {
		Object ret = f.invoke(start, array[i]);
		for(int x = i + 1; x < array.length; x++)
			ret = f.invoke(ret, array[x]);
		return ret;
	}

	public int indexOf(Object o) {
		if (o instanceof Byte) {
			byte b = ((Byte) o).byteValue();
			for (int j = i; j < array.length; j++)
				if (b == array[j]) return j - i;
		}
		if (o == null) {
			return -1;
		}
		for (int j = i; j < array.length; j++)
			if (o.equals(array[j])) return j - i;
		return -1;
	}
	
	public int lastIndexOf(Object o) {
		if (o instanceof Byte) {
			byte b = ((Byte) o).byteValue();
			for (int j = array.length - 1; j >= i; j--)
				if (b == array[j]) return j - i;
		}
		if (o == null) {
			return -1;
		}
		for (int j = array.length - 1; j >= i; j--)
			if (o.equals(array[j])) return j - i;
		return -1;
	}
}

static public class ArraySeq_char extends ASeq implements IndexedSeq, IReduce{
	public final char[] array;
	final int i;

	ArraySeq_char(IPersistentMap meta, char[] array, int i){
		super(meta);
		this.array = array;
		this.i = i;
	}

	public Object first(){
		return array[i];
	}

	public ISeq next(){
		if(i + 1 < array.length)
			return new ArraySeq_char(meta(), array, i + 1);
		return null;
	}

	public int count(){
		return array.length - i;
	}

	public int index(){
		return i;
	}

	public ArraySeq_char withMeta(IPersistentMap meta){
		return new ArraySeq_char(meta, array, i);
	}

	public Object reduce(IFn f) {
		Object ret = array[i];
		for(int x = i + 1; x < array.length; x++)
			ret = f.invoke(ret, array[x]);
		return ret;
	}

	public Object reduce(IFn f, Object start) {
		Object ret = f.invoke(start, array[i]);
		for(int x = i + 1; x < array.length; x++)
			ret = f.invoke(ret, array[x]);
		return ret;
	}
	
	public int indexOf(Object o) {
		if (o instanceof Character) {
			char c = ((Character) o).charValue();
			for (int j = i; j < array.length; j++)
				if (c == array[j]) return j - i;
		}
		if (o == null) {
			return -1;
		}
		for (int j = i; j < array.length; j++)
			if (o.equals(array[j])) return j - i;
		return -1;
	}
	
	public int lastIndexOf(Object o) {
		if (o instanceof Character) {
			char c = ((Character) o).charValue();
			for (int j = array.length - 1; j >= i; j--)
				if (c == array[j]) return j - i;
		}
		if (o == null) {
			return -1;
		}
		for (int j = array.length - 1; j >= i; j--)
			if (o.equals(array[j])) return j - i;
		return -1;
	}
}

static public class ArraySeq_boolean extends ASeq implements IndexedSeq, IReduce{
	public final boolean[] array;
	final int i;

	ArraySeq_boolean(IPersistentMap meta, boolean[] array, int i){
		super(meta);
		this.array = array;
		this.i = i;
	}

	public Object first(){
		return array[i];
	}

	public ISeq next(){
		if(i + 1 < array.length)
			return new ArraySeq_boolean(meta(), array, i + 1);
		return null;
	}

	public int count(){
		return array.length - i;
	}

	public int index(){
		return i;
	}

	public ArraySeq_boolean withMeta(IPersistentMap meta){
		return new ArraySeq_boolean(meta, array, i);
	}

	public Object reduce(IFn f) {
		Object ret = array[i];
		for(int x = i + 1; x < array.length; x++)
			ret = f.invoke(ret, array[x]);
		return ret;
	}

	public Object reduce(IFn f, Object start) {
		Object ret = f.invoke(start, array[i]);
		for(int x = i + 1; x < array.length; x++)
			ret = f.invoke(ret, array[x]);
		return ret;
	}
	
	public int indexOf(Object o) {
		if (o instanceof Boolean) {
			boolean b = ((Boolean) o).booleanValue();
			for (int j = i; j < array.length; j++)
				if (b == array[j]) return j - i;
		}
		if (o == null) {
			return -1;
		}
		for (int j = i; j < array.length; j++)
			if (o.equals(array[j])) return j - i;
		return -1;
	}
	
	public int lastIndexOf(Object o) {
		if (o instanceof Boolean) {
			boolean b = ((Boolean) o).booleanValue();
			for (int j = array.length - 1; j >= i; j--)
				if (b == array[j]) return j - i;
		}
		if (o == null) {
			return -1;
		}
		for (int j = array.length - 1; j >= i; j--)
			if (o.equals(array[j])) return j - i;
		return -1;
	}
}

}
