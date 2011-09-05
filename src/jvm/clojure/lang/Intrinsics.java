/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich 9/5/11 */

package clojure.lang;

import clojure.asm.Opcodes;

public class Intrinsics implements Opcodes{
private static Object[] oa(Object... arr){
	return arr;
}

static IPersistentMap ops = RT.map(
 "public static double clojure.lang.Numbers.add(double,double)", DADD,
 "public static long clojure.lang.Numbers.and(long,long)", LAND,
 "public static long clojure.lang.Numbers.or(long,long)", LOR,
 "public static long clojure.lang.Numbers.xor(long,long)", LXOR,
 "public static double clojure.lang.Numbers.multiply(double,double)", DMUL,
 "public static double clojure.lang.Numbers.divide(double,double)", DDIV,
 "public static long clojure.lang.Numbers.remainder(long,long)", LREM,
 "public static long clojure.lang.Numbers.shiftLeft(long,long)", oa(L2I, LSHL),
 "public static long clojure.lang.Numbers.shiftRight(long,long)", oa(L2I, LSHR),
 "public static double clojure.lang.Numbers.minus(double)", DNEG,
 "public static double clojure.lang.Numbers.minus(double,double)", DSUB,
 "public static double clojure.lang.Numbers.inc(double)", oa(DCONST_1, DADD),
 "public static double clojure.lang.Numbers.dec(double)", oa(DCONST_1, DSUB),
 "public static long clojure.lang.Numbers.quotient(long,long)", LDIV,
 "public static int clojure.lang.Numbers.shiftLeftInt(int,int)", ISHL,
 "public static int clojure.lang.Numbers.shiftRightInt(int,int)", ISHR,
 "public static int clojure.lang.Numbers.unchecked_int_add(int,int)", IADD,
 "public static int clojure.lang.Numbers.unchecked_int_subtract(int,int)", ISUB,
 "public static int clojure.lang.Numbers.unchecked_int_negate(int)", INEG,
 "public static int clojure.lang.Numbers.unchecked_int_inc(int)", oa(ICONST_1, IADD),
 "public static int clojure.lang.Numbers.unchecked_int_dec(int)", oa(ICONST_1, ISUB),
 "public static int clojure.lang.Numbers.unchecked_int_multiply(int,int)", IMUL,
 "public static int clojure.lang.Numbers.unchecked_int_divide(int,int)", IDIV,
 "public static int clojure.lang.Numbers.unchecked_int_remainder(int,int)", IREM,
 "public static long clojure.lang.Numbers.unchecked_add(long,long)", LADD,
 "public static double clojure.lang.Numbers.unchecked_add(double,double)", DADD,
 "public static long clojure.lang.Numbers.unchecked_minus(long)", LNEG,
 "public static double clojure.lang.Numbers.unchecked_minus(double)", DNEG,
 "public static double clojure.lang.Numbers.unchecked_minus(double,double)", DSUB,
 "public static long clojure.lang.Numbers.unchecked_minus(long,long)", LSUB,
 "public static long clojure.lang.Numbers.unchecked_multiply(long,long)", LMUL,
 "public static double clojure.lang.Numbers.unchecked_multiply(double,double)", DMUL,
 "public static double clojure.lang.Numbers.unchecked_inc(double)", oa(DCONST_1, DADD),
 "public static long clojure.lang.Numbers.unchecked_inc(long)", oa(LCONST_1, LADD),
 "public static double clojure.lang.Numbers.unchecked_dec(double)", oa(DCONST_1, DSUB),
 "public static long clojure.lang.Numbers.unchecked_dec(long)", oa(LCONST_1, LSUB),


  "public static short clojure.lang.RT.aget(short[],int)", SALOAD,
  "public static float clojure.lang.RT.aget(float[],int)", FALOAD,
  "public static double clojure.lang.RT.aget(double[],int)", DALOAD,
  "public static int clojure.lang.RT.aget(int[],int)", IALOAD,
  "public static long clojure.lang.RT.aget(long[],int)", LALOAD,
  "public static char clojure.lang.RT.aget(char[],int)", CALOAD,
  "public static byte clojure.lang.RT.aget(byte[],int)", BALOAD,
  "public static boolean clojure.lang.RT.aget(boolean[],int)", BALOAD,
  "public static java.lang.Object clojure.lang.RT.aget(java.lang.Object[],int)", AALOAD,
  "public static int clojure.lang.RT.alength(int[])", ARRAYLENGTH,
  "public static int clojure.lang.RT.alength(long[])", ARRAYLENGTH,
  "public static int clojure.lang.RT.alength(char[])", ARRAYLENGTH,
  "public static int clojure.lang.RT.alength(java.lang.Object[])", ARRAYLENGTH,
  "public static int clojure.lang.RT.alength(byte[])", ARRAYLENGTH,
  "public static int clojure.lang.RT.alength(float[])", ARRAYLENGTH,
  "public static int clojure.lang.RT.alength(short[])", ARRAYLENGTH,
  "public static int clojure.lang.RT.alength(boolean[])", ARRAYLENGTH,
  "public static int clojure.lang.RT.alength(double[])", ARRAYLENGTH,

 "public static double clojure.lang.RT.doubleCast(long)", L2D,
 "public static double clojure.lang.RT.doubleCast(double)", NOP,
 "public static double clojure.lang.RT.doubleCast(float)", F2D,
 "public static double clojure.lang.RT.doubleCast(int)", I2D,
 "public static double clojure.lang.RT.doubleCast(short)", I2D,
 "public static double clojure.lang.RT.doubleCast(byte)", I2D,
 "public static double clojure.lang.RT.uncheckedDoubleCast(double)", NOP,
 "public static double clojure.lang.RT.uncheckedDoubleCast(float)", F2D,
 "public static double clojure.lang.RT.uncheckedDoubleCast(long)", L2D,
 "public static double clojure.lang.RT.uncheckedDoubleCast(int)", I2D,
 "public static double clojure.lang.RT.uncheckedDoubleCast(short)", I2D,
 "public static double clojure.lang.RT.uncheckedDoubleCast(byte)", I2D,
 "public static long clojure.lang.RT.longCast(long)", NOP,
 "public static long clojure.lang.RT.longCast(short)", I2L,
 "public static long clojure.lang.RT.longCast(byte)", I2L,
 "public static long clojure.lang.RT.longCast(int)", I2L,
  "public static int clojure.lang.RT.uncheckedIntCast(long)", L2I,
  "public static int clojure.lang.RT.uncheckedIntCast(double)", D2I,
  "public static int clojure.lang.RT.uncheckedIntCast(byte)", NOP,
  "public static int clojure.lang.RT.uncheckedIntCast(short)", NOP,
  "public static int clojure.lang.RT.uncheckedIntCast(char)", NOP,
  "public static int clojure.lang.RT.uncheckedIntCast(int)", NOP,
  "public static int clojure.lang.RT.uncheckedIntCast(float)", F2I,
  "public static long clojure.lang.RT.uncheckedLongCast(short)", I2L,
  "public static long clojure.lang.RT.uncheckedLongCast(float)", F2L,
  "public static long clojure.lang.RT.uncheckedLongCast(double)", D2L,
  "public static long clojure.lang.RT.uncheckedLongCast(byte)", I2L,
  "public static long clojure.lang.RT.uncheckedLongCast(long)", NOP,
  "public static long clojure.lang.RT.uncheckedLongCast(int)", I2L
);

//map to instructions terminated with comparator for branch to false
static IPersistentMap preds = RT.map(
  "public static boolean clojure.lang.Numbers.lt(double,double)", oa(DCMPG, IFGE),
  "public static boolean clojure.lang.Numbers.lt(long,long)", oa(LCMP, IFGE),
  "public static boolean clojure.lang.Numbers.equiv(double,double)", oa(DCMPL, IFNE),
  "public static boolean clojure.lang.Numbers.equiv(long,long)", oa(LCMP, IFNE),
  "public static boolean clojure.lang.Numbers.lte(double,double)", oa(DCMPG, IFGT),
  "public static boolean clojure.lang.Numbers.lte(long,long)", oa(LCMP, IFGT),
  "public static boolean clojure.lang.Numbers.gt(long,long)", oa(LCMP, IFLE),
  "public static boolean clojure.lang.Numbers.gt(double,double)", oa(DCMPL, IFLE),
  "public static boolean clojure.lang.Numbers.gte(long,long)", oa(LCMP, IFLT),
  "public static boolean clojure.lang.Numbers.gte(double,double)", oa(DCMPL, IFLT),
  "public static boolean clojure.lang.Util.equiv(long,long)", oa(LCMP, IFNE),
  "public static boolean clojure.lang.Util.equiv(boolean,boolean)", oa(IF_ICMPNE),
  "public static boolean clojure.lang.Util.equiv(double,double)", oa(DCMPL, IFNE),

  "public static boolean clojure.lang.Numbers.isZero(double)", oa(DCONST_0, DCMPL, IFNE),
  "public static boolean clojure.lang.Numbers.isZero(long)", oa(LCONST_0, LCMP, IFNE),
  "public static boolean clojure.lang.Numbers.isPos(long)", oa(LCONST_0, LCMP, IFLE),
  "public static boolean clojure.lang.Numbers.isPos(double)", oa(DCONST_0, DCMPL, IFLE),
  "public static boolean clojure.lang.Numbers.isNeg(long)", oa(LCONST_0, LCMP, IFGE),
  "public static boolean clojure.lang.Numbers.isNeg(double)", oa(DCONST_0, DCMPG, IFGE)
);
}
