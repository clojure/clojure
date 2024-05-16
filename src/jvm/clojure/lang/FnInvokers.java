/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (https://opensource.org/license/epl-1-0)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

package clojure.lang;

public class FnInvokers {

    // Encode invoker param/return class to code for method name
    static char encodeInvokerType(Class c) {
        if(Long.TYPE.equals(c)) {
            return 'L';
        } else if(Double.TYPE.equals(c)) {
            return 'D';
        } else if(Integer.TYPE.equals(c)) {
            return 'I';
        } else if(Short.TYPE.equals(c)) {
            return 'S';
        } else if(Byte.TYPE.equals(c)) {
            return 'B';
        } else if(Float.TYPE.equals(c)) {
            return 'F';
        } else if(Boolean.TYPE.equals(c)) {
            return 'Z';
        } else {
            return 'O';
        }
    }

    public static long invokeLL(IFn f0, long a) {
        if(f0 instanceof IFn.LL) {
            return ((IFn.LL)f0).invokePrim(a);
        } else {
            return RT.longCast(f0.invoke(a));
        }
    }

    public static long invokeDL(IFn f0, double a) {
        if(f0 instanceof IFn.DL) {
            return ((IFn.DL)f0).invokePrim(a);
        } else {
            return RT.longCast(f0.invoke(a));
        }
    }

    public static long invokeOL(IFn f0, Object a) {
        if(f0 instanceof IFn.OL) {
            return ((IFn.OL)f0).invokePrim(a);
        } else {
            return RT.longCast(f0.invoke(a));
        }
    }

    public static int invokeLI(IFn f0, long a) {
        if(f0 instanceof IFn.LL) {
            return RT.intCast(((IFn.LL)f0).invokePrim(a));
        } else {
            return RT.intCast(f0.invoke(a));
        }
    }

    public static int invokeDI(IFn f0, double a) {
        if(f0 instanceof IFn.DL) {
            return RT.intCast(((IFn.DL)f0).invokePrim(a));
        } else {
            return RT.intCast(f0.invoke(a));
        }
    }

    public static int invokeOI(IFn f0, Object a) {
        if(f0 instanceof IFn.OL) {
            return RT.intCast(((IFn.OL)f0).invokePrim(a));
        } else {
            return RT.intCast(f0.invoke(a));
        }
    }

    public static short invokeLS(IFn f0, long a) {
        if(f0 instanceof IFn.LL) {
            return RT.shortCast(((IFn.LL)f0).invokePrim(a));
        } else {
            return RT.shortCast(f0.invoke(a));
        }
    }

    public static short invokeDS(IFn f0, double a) {
        if(f0 instanceof IFn.DL) {
            return RT.shortCast(((IFn.DL)f0).invokePrim(a));
        } else {
            return RT.shortCast(f0.invoke(a));
        }
    }

    public static short invokeOS(IFn f0, Object a) {
        if(f0 instanceof IFn.OL) {
            return RT.shortCast(((IFn.OL)f0).invokePrim(a));
        } else {
            return RT.shortCast(f0.invoke(a));
        }
    }

    public static byte invokeLB(IFn f0, long a) {
        if(f0 instanceof IFn.LL) {
            return RT.byteCast(((IFn.LL)f0).invokePrim(a));
        } else {
            return RT.byteCast(f0.invoke(a));
        }
    }

    public static byte invokeDB(IFn f0, double a) {
        if(f0 instanceof IFn.DL) {
            return RT.byteCast(((IFn.DL)f0).invokePrim(a));
        } else {
            return RT.byteCast(f0.invoke(a));
        }
    }

    public static byte invokeOB(IFn f0, Object a) {
        if(f0 instanceof IFn.OL) {
            return RT.byteCast(((IFn.OL)f0).invokePrim(a));
        } else {
            return RT.byteCast(f0.invoke(a));
        }
    }

    public static double invokeLD(IFn f0, long a) {
        if(f0 instanceof IFn.LD) {
            return ((IFn.LD)f0).invokePrim(a);
        } else {
            return RT.doubleCast(f0.invoke(a));
        }
    }

    public static double invokeDD(IFn f0, double a) {
        if(f0 instanceof IFn.DD) {
            return ((IFn.DD)f0).invokePrim(a);
        } else {
            return RT.doubleCast(f0.invoke(a));
        }
    }

    public static double invokeOD(IFn f0, Object a) {
        if(f0 instanceof IFn.OD) {
            return ((IFn.OD)f0).invokePrim(a);
        } else {
            return RT.doubleCast(f0.invoke(a));
        }
    }

    public static float invokeLF(IFn f0, long a) {
        if(f0 instanceof IFn.LD) {
            return RT.floatCast(((IFn.LD)f0).invokePrim(a));
        } else {
            return RT.floatCast(f0.invoke(a));
        }
    }

    public static float invokeDF(IFn f0, double a) {
        if(f0 instanceof IFn.DD) {
            return RT.floatCast(((IFn.DD)f0).invokePrim(a));
        } else {
            return RT.floatCast(f0.invoke(a));
        }
    }

    public static float invokeOF(IFn f0, Object a) {
        if(f0 instanceof IFn.OD) {
            return RT.floatCast(((IFn.OD)f0).invokePrim(a));
        } else {
            return RT.floatCast(f0.invoke(a));
        }
    }

    public static boolean invokeLZ(IFn f0, long a) {
        return RT.booleanCast(f0.invoke(a));
    }

    public static boolean invokeDZ(IFn f0, double a) {
        return RT.booleanCast(f0.invoke(a));
    }

    public static boolean invokeOZ(IFn f0, Object a) {
        return RT.booleanCast(f0.invoke(a));
    }

    public static Object invokeLO(IFn f0, long a) {
        if(f0 instanceof IFn.LO) {
            return ((IFn.LO)f0).invokePrim(a);
        } else {
            return f0.invoke(a);
        }
    }

    public static Object invokeDO(IFn f0, double a) {
        if(f0 instanceof IFn.DO) {
            return ((IFn.DO)f0).invokePrim(a);
        } else {
            return f0.invoke(a);
        }
    }

    public static Object invokeOO(IFn f0, Object a) {
        return f0.invoke(a);
    }

    public static long invokeLLL(IFn f0, long a, long b) {
        if(f0 instanceof IFn.LLL) {
            return ((IFn.LLL)f0).invokePrim(a, b);
        } else {
            return RT.longCast(f0.invoke(a, b));
        }
    }

    public static long invokeLOL(IFn f0, long a, Object b) {
        if(f0 instanceof IFn.LOL) {
            return ((IFn.LOL)f0).invokePrim(a, b);
        } else {
            return RT.longCast(f0.invoke(a, b));
        }
    }

    public static long invokeOLL(IFn f0, Object a, long b) {
        if(f0 instanceof IFn.OLL) {
            return ((IFn.OLL)f0).invokePrim(a, b);
        } else {
            return RT.longCast(f0.invoke(a, b));
        }
    }

    public static long invokeDDL(IFn f0, double a, double b) {
        if(f0 instanceof IFn.DDL) {
            return ((IFn.DDL)f0).invokePrim(a, b);
        } else {
            return RT.longCast(f0.invoke(a, b));
        }
    }

    public static long invokeLDL(IFn f0, long a, double b) {
        if(f0 instanceof IFn.LDL) {
            return ((IFn.LDL)f0).invokePrim(a, b);
        } else {
            return RT.longCast(f0.invoke(a, b));
        }
    }

    public static long invokeDLL(IFn f0, double a, long b) {
        if(f0 instanceof IFn.DLL) {
            return ((IFn.DLL)f0).invokePrim(a, b);
        } else {
            return RT.longCast(f0.invoke(a, b));
        }
    }

    public static long invokeOOL(IFn f0, Object a, Object b) {
        if(f0 instanceof IFn.OOL) {
            return ((IFn.OOL)f0).invokePrim(a, b);
        } else {
            return RT.longCast(f0.invoke(a, b));
        }
    }

    public static long invokeODL(IFn f0, Object a, double b) {
        if(f0 instanceof IFn.ODL) {
            return ((IFn.ODL)f0).invokePrim(a, b);
        } else {
            return RT.longCast(f0.invoke(a, b));
        }
    }

    public static long invokeDOL(IFn f0, double a, Object b) {
        if(f0 instanceof IFn.DOL) {
            return ((IFn.DOL)f0).invokePrim(a, b);
        } else {
            return RT.longCast(f0.invoke(a, b));
        }
    }

    public static int invokeLLI(IFn f0, long a, long b) {
        if(f0 instanceof IFn.LLL) {
            return RT.intCast(((IFn.LLL)f0).invokePrim(a, b));
        } else {
            return RT.intCast(f0.invoke(a, b));
        }
    }

    public static int invokeLOI(IFn f0, long a, Object b) {
        if(f0 instanceof IFn.LOL) {
            return RT.intCast(((IFn.LOL)f0).invokePrim(a, b));
        } else {
            return RT.intCast(f0.invoke(a, b));
        }
    }

    public static int invokeOLI(IFn f0, Object a, long b) {
        if(f0 instanceof IFn.OLL) {
            return RT.intCast(((IFn.OLL)f0).invokePrim(a, b));
        } else {
            return RT.intCast(f0.invoke(a, b));
        }
    }

    public static int invokeDDI(IFn f0, double a, double b) {
        if(f0 instanceof IFn.DDL) {
            return RT.intCast(((IFn.DDL)f0).invokePrim(a, b));
        } else {
            return RT.intCast(f0.invoke(a, b));
        }
    }

    public static int invokeLDI(IFn f0, long a, double b) {
        if(f0 instanceof IFn.LDL) {
            return RT.intCast(((IFn.LDL)f0).invokePrim(a, b));
        } else {
            return RT.intCast(f0.invoke(a, b));
        }
    }

    public static int invokeDLI(IFn f0, double a, long b) {
        if(f0 instanceof IFn.DLL) {
            return RT.intCast(((IFn.DLL)f0).invokePrim(a, b));
        } else {
            return RT.intCast(f0.invoke(a, b));
        }
    }

    public static int invokeOOI(IFn f0, Object a, Object b) {
        if(f0 instanceof IFn.OOL) {
            return RT.intCast(((IFn.OOL)f0).invokePrim(a, b));
        } else {
            return RT.intCast(f0.invoke(a, b));
        }
    }

    public static int invokeODI(IFn f0, Object a, double b) {
        if(f0 instanceof IFn.ODL) {
            return RT.intCast(((IFn.ODL)f0).invokePrim(a, b));
        } else {
            return RT.intCast(f0.invoke(a, b));
        }
    }

    public static int invokeDOI(IFn f0, double a, Object b) {
        if(f0 instanceof IFn.DOL) {
            return RT.intCast(((IFn.DOL)f0).invokePrim(a, b));
        } else {
            return RT.intCast(f0.invoke(a, b));
        }
    }

    public static short invokeLLS(IFn f0, long a, long b) {
        if(f0 instanceof IFn.LLL) {
            return RT.shortCast(((IFn.LLL)f0).invokePrim(a, b));
        } else {
            return RT.shortCast(f0.invoke(a, b));
        }
    }

    public static short invokeLOS(IFn f0, long a, Object b) {
        if(f0 instanceof IFn.LOL) {
            return RT.shortCast(((IFn.LOL)f0).invokePrim(a, b));
        } else {
            return RT.shortCast(f0.invoke(a, b));
        }
    }

    public static short invokeOLS(IFn f0, Object a, long b) {
        if(f0 instanceof IFn.OLL) {
            return RT.shortCast(((IFn.OLL)f0).invokePrim(a, b));
        } else {
            return RT.shortCast(f0.invoke(a, b));
        }
    }

    public static short invokeDDS(IFn f0, double a, double b) {
        if(f0 instanceof IFn.DDL) {
            return RT.shortCast(((IFn.DDL)f0).invokePrim(a, b));
        } else {
            return RT.shortCast(f0.invoke(a, b));
        }
    }

    public static short invokeLDS(IFn f0, long a, double b) {
        if(f0 instanceof IFn.LDL) {
            return RT.shortCast(((IFn.LDL)f0).invokePrim(a, b));
        } else {
            return RT.shortCast(f0.invoke(a, b));
        }
    }

    public static short invokeDLS(IFn f0, double a, long b) {
        if(f0 instanceof IFn.DLL) {
            return RT.shortCast(((IFn.DLL)f0).invokePrim(a, b));
        } else {
            return RT.shortCast(f0.invoke(a, b));
        }
    }

    public static short invokeOOS(IFn f0, Object a, Object b) {
        if(f0 instanceof IFn.OOL) {
            return RT.shortCast(((IFn.OOL)f0).invokePrim(a, b));
        } else {
            return RT.shortCast(f0.invoke(a, b));
        }
    }

    public static short invokeODS(IFn f0, Object a, double b) {
        if(f0 instanceof IFn.ODL) {
            return RT.shortCast(((IFn.ODL)f0).invokePrim(a, b));
        } else {
            return RT.shortCast(f0.invoke(a, b));
        }
    }

    public static short invokeDOS(IFn f0, double a, Object b) {
        if(f0 instanceof IFn.DOL) {
            return RT.shortCast(((IFn.DOL)f0).invokePrim(a, b));
        } else {
            return RT.shortCast(f0.invoke(a, b));
        }
    }

    public static byte invokeLLB(IFn f0, long a, long b) {
        if(f0 instanceof IFn.LLL) {
            return RT.byteCast(((IFn.LLL)f0).invokePrim(a, b));
        } else {
            return RT.byteCast(f0.invoke(a, b));
        }
    }

    public static byte invokeLOB(IFn f0, long a, Object b) {
        if(f0 instanceof IFn.LOL) {
            return RT.byteCast(((IFn.LOL)f0).invokePrim(a, b));
        } else {
            return RT.byteCast(f0.invoke(a, b));
        }
    }

    public static byte invokeOLB(IFn f0, Object a, long b) {
        if(f0 instanceof IFn.OLL) {
            return RT.byteCast(((IFn.OLL)f0).invokePrim(a, b));
        } else {
            return RT.byteCast(f0.invoke(a, b));
        }
    }

    public static byte invokeDDB(IFn f0, double a, double b) {
        if(f0 instanceof IFn.DDL) {
            return RT.byteCast(((IFn.DDL)f0).invokePrim(a, b));
        } else {
            return RT.byteCast(f0.invoke(a, b));
        }
    }

    public static byte invokeLDB(IFn f0, long a, double b) {
        if(f0 instanceof IFn.LDL) {
            return RT.byteCast(((IFn.LDL)f0).invokePrim(a, b));
        } else {
            return RT.byteCast(f0.invoke(a, b));
        }
    }

    public static byte invokeDLB(IFn f0, double a, long b) {
        if(f0 instanceof IFn.DLL) {
            return RT.byteCast(((IFn.DLL)f0).invokePrim(a, b));
        } else {
            return RT.byteCast(f0.invoke(a, b));
        }
    }

    public static byte invokeOOB(IFn f0, Object a, Object b) {
        if(f0 instanceof IFn.OOL) {
            return RT.byteCast(((IFn.OOL)f0).invokePrim(a, b));
        } else {
            return RT.byteCast(f0.invoke(a, b));
        }
    }

    public static byte invokeODB(IFn f0, Object a, double b) {
        if(f0 instanceof IFn.ODL) {
            return RT.byteCast(((IFn.ODL)f0).invokePrim(a, b));
        } else {
            return RT.byteCast(f0.invoke(a, b));
        }
    }

    public static byte invokeDOB(IFn f0, double a, Object b) {
        if(f0 instanceof IFn.DOL) {
            return RT.byteCast(((IFn.DOL)f0).invokePrim(a, b));
        } else {
            return RT.byteCast(f0.invoke(a, b));
        }
    }

    public static double invokeLLD(IFn f0, long a, long b) {
        if(f0 instanceof IFn.LLD) {
            return ((IFn.LLD)f0).invokePrim(a, b);
        } else {
            return RT.doubleCast(f0.invoke(a, b));
        }
    }

    public static double invokeLOD(IFn f0, long a, Object b) {
        if(f0 instanceof IFn.LOD) {
            return ((IFn.LOD)f0).invokePrim(a, b);
        } else {
            return RT.doubleCast(f0.invoke(a, b));
        }
    }

    public static double invokeOLD(IFn f0, Object a, long b) {
        if(f0 instanceof IFn.OLD) {
            return ((IFn.OLD)f0).invokePrim(a, b);
        } else {
            return RT.doubleCast(f0.invoke(a, b));
        }
    }

    public static double invokeDDD(IFn f0, double a, double b) {
        if(f0 instanceof IFn.DDD) {
            return ((IFn.DDD)f0).invokePrim(a, b);
        } else {
            return RT.doubleCast(f0.invoke(a, b));
        }
    }

    public static double invokeLDD(IFn f0, long a, double b) {
        if(f0 instanceof IFn.LDD) {
            return ((IFn.LDD)f0).invokePrim(a, b);
        } else {
            return RT.doubleCast(f0.invoke(a, b));
        }
    }

    public static double invokeDLD(IFn f0, double a, long b) {
        if(f0 instanceof IFn.DLD) {
            return ((IFn.DLD)f0).invokePrim(a, b);
        } else {
            return RT.doubleCast(f0.invoke(a, b));
        }
    }

    public static double invokeOOD(IFn f0, Object a, Object b) {
        if(f0 instanceof IFn.OOD) {
            return ((IFn.OOD)f0).invokePrim(a, b);
        } else {
            return RT.doubleCast(f0.invoke(a, b));
        }
    }

    public static double invokeODD(IFn f0, Object a, double b) {
        if(f0 instanceof IFn.ODD) {
            return ((IFn.ODD)f0).invokePrim(a, b);
        } else {
            return RT.doubleCast(f0.invoke(a, b));
        }
    }

    public static double invokeDOD(IFn f0, double a, Object b) {
        if(f0 instanceof IFn.DOD) {
            return ((IFn.DOD)f0).invokePrim(a, b);
        } else {
            return RT.doubleCast(f0.invoke(a, b));
        }
    }

    public static float invokeLLF(IFn f0, long a, long b) {
        if(f0 instanceof IFn.LLD) {
            return RT.floatCast(((IFn.LLD)f0).invokePrim(a, b));
        } else {
            return RT.floatCast(f0.invoke(a, b));
        }
    }

    public static float invokeLOF(IFn f0, long a, Object b) {
        if(f0 instanceof IFn.LOD) {
            return RT.floatCast(((IFn.LOD)f0).invokePrim(a, b));
        } else {
            return RT.floatCast(f0.invoke(a, b));
        }
    }

    public static float invokeOLF(IFn f0, Object a, long b) {
        if(f0 instanceof IFn.OLD) {
            return RT.floatCast(((IFn.OLD)f0).invokePrim(a, b));
        } else {
            return RT.floatCast(f0.invoke(a, b));
        }
    }

    public static float invokeDDF(IFn f0, double a, double b) {
        if(f0 instanceof IFn.DDD) {
            return RT.floatCast(((IFn.DDD)f0).invokePrim(a, b));
        } else {
            return RT.floatCast(f0.invoke(a, b));
        }
    }

    public static float invokeLDF(IFn f0, long a, double b) {
        if(f0 instanceof IFn.LDD) {
            return RT.floatCast(((IFn.LDD)f0).invokePrim(a, b));
        } else {
            return RT.floatCast(f0.invoke(a, b));
        }
    }

    public static float invokeDLF(IFn f0, double a, long b) {
        if(f0 instanceof IFn.DLD) {
            return RT.floatCast(((IFn.DLD)f0).invokePrim(a, b));
        } else {
            return RT.floatCast(f0.invoke(a, b));
        }
    }

    public static float invokeOOF(IFn f0, Object a, Object b) {
        if(f0 instanceof IFn.OOD) {
            return RT.floatCast(((IFn.OOD)f0).invokePrim(a, b));
        } else {
            return RT.floatCast(f0.invoke(a, b));
        }
    }

    public static float invokeODF(IFn f0, Object a, double b) {
        if(f0 instanceof IFn.ODD) {
            return RT.floatCast(((IFn.ODD)f0).invokePrim(a, b));
        } else {
            return RT.floatCast(f0.invoke(a, b));
        }
    }

    public static float invokeDOF(IFn f0, double a, Object b) {
        if(f0 instanceof IFn.DOD) {
            return RT.floatCast(((IFn.DOD)f0).invokePrim(a, b));
        } else {
            return RT.floatCast(f0.invoke(a, b));
        }
    }

    public static boolean invokeLLZ(IFn f0, long a, long b) {
        return RT.booleanCast(f0.invoke(a, b));
    }

    public static boolean invokeLOZ(IFn f0, long a, Object b) {
        return RT.booleanCast(f0.invoke(a, b));
    }

    public static boolean invokeOLZ(IFn f0, Object a, long b) {
        return RT.booleanCast(f0.invoke(a, b));
    }

    public static boolean invokeDDZ(IFn f0, double a, double b) {
        return RT.booleanCast(f0.invoke(a, b));
    }

    public static boolean invokeLDZ(IFn f0, long a, double b) {
        return RT.booleanCast(f0.invoke(a, b));
    }

    public static boolean invokeDLZ(IFn f0, double a, long b) {
        return RT.booleanCast(f0.invoke(a, b));
    }

    public static boolean invokeOOZ(IFn f0, Object a, Object b) {
        return RT.booleanCast(f0.invoke(a, b));
    }

    public static boolean invokeODZ(IFn f0, Object a, double b) {
        return RT.booleanCast(f0.invoke(a, b));
    }

    public static boolean invokeDOZ(IFn f0, double a, Object b) {
        return RT.booleanCast(f0.invoke(a, b));
    }

    public static Object invokeLLO(IFn f0, long a, long b) {
        if(f0 instanceof IFn.LLO) {
            return ((IFn.LLO)f0).invokePrim(a, b);
        } else {
            return f0.invoke(a, b);
        }
    }

    public static Object invokeLOO(IFn f0, long a, Object b) {
        if(f0 instanceof IFn.LOO) {
            return ((IFn.LOO)f0).invokePrim(a, b);
        } else {
            return f0.invoke(a, b);
        }
    }

    public static Object invokeOLO(IFn f0, Object a, long b) {
        if(f0 instanceof IFn.OLO) {
            return ((IFn.OLO)f0).invokePrim(a, b);
        } else {
            return f0.invoke(a, b);
        }
    }

    public static Object invokeDDO(IFn f0, double a, double b) {
        if(f0 instanceof IFn.DDO) {
            return ((IFn.DDO)f0).invokePrim(a, b);
        } else {
            return f0.invoke(a, b);
        }
    }

    public static Object invokeLDO(IFn f0, long a, double b) {
        if(f0 instanceof IFn.LDO) {
            return ((IFn.LDO)f0).invokePrim(a, b);
        } else {
            return f0.invoke(a, b);
        }
    }

    public static Object invokeDLO(IFn f0, double a, long b) {
        if(f0 instanceof IFn.DLO) {
            return ((IFn.DLO)f0).invokePrim(a, b);
        } else {
            return f0.invoke(a, b);
        }
    }

    public static Object invokeOOO(IFn f0, Object a, Object b) {
        return f0.invoke(a, b);
    }

    public static Object invokeODO(IFn f0, Object a, double b) {
        if(f0 instanceof IFn.ODO) {
            return ((IFn.ODO)f0).invokePrim(a, b);
        } else {
            return f0.invoke(a, b);
        }
    }

    public static Object invokeDOO(IFn f0, double a, Object b) {
        if(f0 instanceof IFn.DOO) {
            return ((IFn.DOO)f0).invokePrim(a, b);
        } else {
            return f0.invoke(a, b);
        }
    }

    public static boolean invokeOOOZ(IFn f0, Object a, Object b, Object c) {
        return RT.booleanCast(f0.invoke(a, b, c));
    }

    public static Object invokeOOOO(IFn f0, Object a, Object b, Object c) {
        return f0.invoke(a, b, c);
    }

    public static boolean invokeOOOOZ(IFn f0, Object a, Object b, Object c, Object d) {
        return RT.booleanCast(f0.invoke(a, b, c, d));
    }

    public static Object invokeOOOOO(IFn f0, Object a, Object b, Object c, Object d) {
        return f0.invoke(a, b, c, d);
    }

    public static boolean invokeOOOOOZ(IFn f0, Object a, Object b, Object c, Object d, Object e) {
        return RT.booleanCast(f0.invoke(a, b, c, d, e));
    }

    public static Object invokeOOOOOO(IFn f0, Object a, Object b, Object c, Object d, Object e) {
        return f0.invoke(a, b, c, d, e);
    }

    public static boolean invokeOOOOOOZ(IFn f0, Object a, Object b, Object c, Object d, Object e, Object f) {
        return RT.booleanCast(f0.invoke(a, b, c, d, e, f));
    }

    public static Object invokeOOOOOOO(IFn f0, Object a, Object b, Object c, Object d, Object e, Object f) {
        return f0.invoke(a, b, c, d, e, f);
    }

    public static boolean invokeOOOOOOOZ(IFn f0, Object a, Object b, Object c, Object d, Object e, Object f, Object g) {
        return RT.booleanCast(f0.invoke(a, b, c, d, e, f, g));
    }

    public static Object invokeOOOOOOOO(IFn f0, Object a, Object b, Object c, Object d, Object e, Object f, Object g) {
        return f0.invoke(a, b, c, d, e, f, g);
    }

    public static boolean invokeOOOOOOOOZ(IFn f0, Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h) {
        return RT.booleanCast(f0.invoke(a, b, c, d, e, f, g, h));
    }

    public static Object invokeOOOOOOOOO(IFn f0, Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h) {
        return f0.invoke(a, b, c, d, e, f, g, h);
    }

    public static boolean invokeOOOOOOOOOZ(IFn f0, Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h, Object i) {
        return RT.booleanCast(f0.invoke(a, b, c, d, e, f, g, h, i));
    }

    public static Object invokeOOOOOOOOOO(IFn f0, Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h, Object i) {
        return f0.invoke(a, b, c, d, e, f, g, h, i);
    }

    public static boolean invokeOOOOOOOOOOZ(IFn f0, Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h, Object i, Object j) {
        return RT.booleanCast(f0.invoke(a, b, c, d, e, f, g, h, i, j));
    }

    public static Object invokeOOOOOOOOOOO(IFn f0, Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h, Object i, Object j) {
        return f0.invoke(a, b, c, d, e, f, g, h, i, j);
    }

}