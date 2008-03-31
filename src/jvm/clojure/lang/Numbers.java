/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
 *   which can be found in the file CPL.TXT at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 31, 2008 */

package clojure.lang;

import java.math.BigInteger;
import java.math.BigDecimal;

public class Numbers{
static Object add(Object x, Object y){
	Class c = x.getClass();
	if(c == Integer.class)
		return add(((Integer) x).intValue(), y);
	else if(c == Double.class)
		return add(((Double) x).doubleValue(), y);
	else if(c == Long.class)
		return add(((Long) x).longValue(), y);
	else if(c == Float.class)
		return add(((Float) x).floatValue(), y);
	else if(c == BigInteger.class)
		return add((BigInteger) x, y);
	else if(c == BigDecimal.class)
		return add((BigDecimal) x, y);
	else if(c == Ratio.class)
		return add((Ratio) x, y);
	else if(c == double[].class)
		return add((double[]) x, y);
	else if(c == float[].class)
		return add((float[]) x, y);
	else
		return add(((Number) x).intValue(), y);
}

public static Object add(int x, Object y){
	Class c = y.getClass();
	if(c == Integer.class)
		return add(((Integer) y).intValue(), x);
	else if(c == Double.class)
		return x + (Double) y;
	else if(c == Long.class)
		return add(((Long) y).longValue(), x);
	else if(c == Float.class)
		return x + (Float) y;
	else if(c == BigInteger.class)
		return BigInteger.valueOf(x).add((BigInteger) y);
	else if(c == BigDecimal.class)
		return BigDecimal.valueOf(x).add((BigDecimal) y);
	else if(c == Ratio.class)
		return add((Ratio) y, x);
	else if(c == double[].class)
		{
		return add(((double[]) y), x);
		}
	else if(c == float[].class)
		{
		return add(((float[]) y), x);
		}
	else
		return add(((Number) y).intValue(), x);
}

public static double[] add(double[] x, double y){
	double[] ret = x.clone();
	for(int i = 0; i < ret.length; i++)
		ret[i] += y;
	return ret;
}

public static float[] add(float[] x, float y){
	float[] ret = x.clone();
	for(int i = 0; i < ret.length; i++)
		ret[i] += y;
	return ret;
}

public static Ratio add(Ratio x, int y){
	 return null;
}

public static Double add(double x, double y){
	return x + y;
}

public static Float add(float x, float y){
	return x + y;
}

public static Number add(int x, int y){
	long ret = (long) x + (long) y;
	if(ret <= Integer.MAX_VALUE && ret >= Integer.MIN_VALUE)
		return (int) ret;
	return ret;
}
}
