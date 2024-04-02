/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

package clojure.lang;

public class FnInvokers {

    private static RuntimeException notIFnError(Object f) {
        return new RuntimeException("Expected function, but found " + (f == null ? "null" : f.getClass().getName()));
    }

    public static long invokeLL(Object f0, long a) {
        if(f0 instanceof IFn.LL) {
            return ((IFn.LL)f0).invokePrim(a);
        } else if(f0 instanceof IFn) {
            return RT.longCast(((IFn)f0).invoke(a));
        } else {
            throw notIFnError(f0);
        }
    }

    public static long invokeDL(Object f0, double a) {
        if(f0 instanceof IFn.DL) {
            return ((IFn.DL)f0).invokePrim(a);
        } else if(f0 instanceof IFn) {
            return RT.longCast(((IFn)f0).invoke(a));
        } else {
            throw notIFnError(f0);
        }
    }

    public static long invokeOL(Object f0, Object a) {
        if(f0 instanceof IFn.OL) {
            return ((IFn.OL)f0).invokePrim(a);
        } else if(f0 instanceof IFn) {
            return RT.longCast(((IFn)f0).invoke(a));
        } else {
            throw notIFnError(f0);
        }
    }

    public static int invokeLI(Object f0, long a) {
        if(f0 instanceof IFn.LL) {
            return RT.intCast(((IFn.LL)f0).invokePrim(a));
        } else if(f0 instanceof IFn) {
            return RT.intCast(((IFn)f0).invoke(a));
        } else {
            throw notIFnError(f0);
        }
    }

    public static int invokeDI(Object f0, double a) {
        if(f0 instanceof IFn.DL) {
            return RT.intCast(((IFn.DL)f0).invokePrim(a));
        } else if(f0 instanceof IFn) {
            return RT.intCast(((IFn)f0).invoke(a));
        } else {
            throw notIFnError(f0);
        }
    }

    public static int invokeOI(Object f0, Object a) {
        if(f0 instanceof IFn.OL) {
            return RT.intCast(((IFn.OL)f0).invokePrim(a));
        } else if(f0 instanceof IFn) {
            return RT.intCast(((IFn)f0).invoke(a));
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean invokeLB(Object f0, long a) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a));
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean invokeDB(Object f0, double a) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a));
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean invokeOB(Object f0, Object a) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a));
        } else {
            throw notIFnError(f0);
        }
    }

    public static double invokeLD(Object f0, long a) {
        if(f0 instanceof IFn.LD) {
            return ((IFn.LD)f0).invokePrim(a);
        } else if(f0 instanceof IFn) {
            return RT.doubleCast(((IFn)f0).invoke(a));
        } else {
            throw notIFnError(f0);
        }
    }

    public static double invokeDD(Object f0, double a) {
        if(f0 instanceof IFn.DD) {
            return ((IFn.DD)f0).invokePrim(a);
        } else if(f0 instanceof IFn) {
            return RT.doubleCast(((IFn)f0).invoke(a));
        } else {
            throw notIFnError(f0);
        }
    }

    public static double invokeOD(Object f0, Object a) {
        if(f0 instanceof IFn.OD) {
            return ((IFn.OD)f0).invokePrim(a);
        } else if(f0 instanceof IFn) {
            return RT.doubleCast(((IFn)f0).invoke(a));
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object invokeLO(Object f0, long a) {
        if(f0 instanceof IFn.LO) {
            return ((IFn.LO)f0).invokePrim(a);
        } else if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a);
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object invokeDO(Object f0, double a) {
        if(f0 instanceof IFn.DO) {
            return ((IFn.DO)f0).invokePrim(a);
        } else if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a);
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object invokeOO(Object f0, Object a) {
        if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a);
        } else {
            throw notIFnError(f0);
        }
    }

    public static float invokeLF(Object f0, long a) {
        if(f0 instanceof IFn) {
            return RT.floatCast(((IFn)f0).invoke(a));
        } else {
            throw notIFnError(f0);
        }
    }

    public static float invokeDF(Object f0, double a) {
        if(f0 instanceof IFn) {
            return RT.floatCast(((IFn)f0).invoke(a));
        } else {
            throw notIFnError(f0);
        }
    }

    public static float invokeOF(Object f0, Object a) {
        if(f0 instanceof IFn) {
            return RT.floatCast(((IFn)f0).invoke(a));
        } else {
            throw notIFnError(f0);
        }
    }

    public static long invokeLLL(Object f0, long a, long b) {
        if(f0 instanceof IFn.LLL) {
            return ((IFn.LLL)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return RT.longCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static long invokeLOL(Object f0, long a, Object b) {
        if(f0 instanceof IFn.LOL) {
            return ((IFn.LOL)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return RT.longCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static long invokeOLL(Object f0, Object a, long b) {
        if(f0 instanceof IFn.OLL) {
            return ((IFn.OLL)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return RT.longCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static long invokeDDL(Object f0, double a, double b) {
        if(f0 instanceof IFn.DDL) {
            return ((IFn.DDL)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return RT.longCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static long invokeLDL(Object f0, long a, double b) {
        if(f0 instanceof IFn.LDL) {
            return ((IFn.LDL)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return RT.longCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static long invokeDLL(Object f0, double a, long b) {
        if(f0 instanceof IFn.DLL) {
            return ((IFn.DLL)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return RT.longCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static long invokeOOL(Object f0, Object a, Object b) {
        if(f0 instanceof IFn.OOL) {
            return ((IFn.OOL)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return RT.longCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static long invokeODL(Object f0, Object a, double b) {
        if(f0 instanceof IFn.ODL) {
            return ((IFn.ODL)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return RT.longCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static long invokeDOL(Object f0, double a, Object b) {
        if(f0 instanceof IFn.DOL) {
            return ((IFn.DOL)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return RT.longCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static int invokeLLI(Object f0, long a, long b) {
        if(f0 instanceof IFn.LLL) {
            return RT.intCast(((IFn.LLL)f0).invokePrim(a, b));
        } else if(f0 instanceof IFn) {
            return RT.intCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static int invokeLOI(Object f0, long a, Object b) {
        if(f0 instanceof IFn.LOL) {
            return RT.intCast(((IFn.LOL)f0).invokePrim(a, b));
        } else if(f0 instanceof IFn) {
            return RT.intCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static int invokeOLI(Object f0, Object a, long b) {
        if(f0 instanceof IFn.OLL) {
            return RT.intCast(((IFn.OLL)f0).invokePrim(a, b));
        } else if(f0 instanceof IFn) {
            return RT.intCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static int invokeDDI(Object f0, double a, double b) {
        if(f0 instanceof IFn.DDL) {
            return RT.intCast(((IFn.DDL)f0).invokePrim(a, b));
        } else if(f0 instanceof IFn) {
            return RT.intCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static int invokeLDI(Object f0, long a, double b) {
        if(f0 instanceof IFn.LDL) {
            return RT.intCast(((IFn.LDL)f0).invokePrim(a, b));
        } else if(f0 instanceof IFn) {
            return RT.intCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static int invokeDLI(Object f0, double a, long b) {
        if(f0 instanceof IFn.DLL) {
            return RT.intCast(((IFn.DLL)f0).invokePrim(a, b));
        } else if(f0 instanceof IFn) {
            return RT.intCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static int invokeOOI(Object f0, Object a, Object b) {
        if(f0 instanceof IFn.OOL) {
            return RT.intCast(((IFn.OOL)f0).invokePrim(a, b));
        } else if(f0 instanceof IFn) {
            return RT.intCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static int invokeODI(Object f0, Object a, double b) {
        if(f0 instanceof IFn.ODL) {
            return RT.intCast(((IFn.ODL)f0).invokePrim(a, b));
        } else if(f0 instanceof IFn) {
            return RT.intCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static int invokeDOI(Object f0, double a, Object b) {
        if(f0 instanceof IFn.DOL) {
            return RT.intCast(((IFn.DOL)f0).invokePrim(a, b));
        } else if(f0 instanceof IFn) {
            return RT.intCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean invokeLLB(Object f0, long a, long b) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean invokeLOB(Object f0, long a, Object b) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean invokeOLB(Object f0, Object a, long b) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean invokeDDB(Object f0, double a, double b) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean invokeLDB(Object f0, long a, double b) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean invokeDLB(Object f0, double a, long b) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean invokeOOB(Object f0, Object a, Object b) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean invokeODB(Object f0, Object a, double b) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean invokeDOB(Object f0, double a, Object b) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static double invokeLLD(Object f0, long a, long b) {
        if(f0 instanceof IFn.LLD) {
            return ((IFn.LLD)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return RT.doubleCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static double invokeLOD(Object f0, long a, Object b) {
        if(f0 instanceof IFn.LOD) {
            return ((IFn.LOD)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return RT.doubleCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static double invokeOLD(Object f0, Object a, long b) {
        if(f0 instanceof IFn.OLD) {
            return ((IFn.OLD)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return RT.doubleCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static double invokeDDD(Object f0, double a, double b) {
        if(f0 instanceof IFn.DDD) {
            return ((IFn.DDD)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return RT.doubleCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static double invokeLDD(Object f0, long a, double b) {
        if(f0 instanceof IFn.LDD) {
            return ((IFn.LDD)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return RT.doubleCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static double invokeDLD(Object f0, double a, long b) {
        if(f0 instanceof IFn.DLD) {
            return ((IFn.DLD)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return RT.doubleCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static double invokeOOD(Object f0, Object a, Object b) {
        if(f0 instanceof IFn.OOD) {
            return ((IFn.OOD)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return RT.doubleCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static double invokeODD(Object f0, Object a, double b) {
        if(f0 instanceof IFn.ODD) {
            return ((IFn.ODD)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return RT.doubleCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static double invokeDOD(Object f0, double a, Object b) {
        if(f0 instanceof IFn.DOD) {
            return ((IFn.DOD)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return RT.doubleCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object invokeLLO(Object f0, long a, long b) {
        if(f0 instanceof IFn.LLO) {
            return ((IFn.LLO)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a, b);
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object invokeLOO(Object f0, long a, Object b) {
        if(f0 instanceof IFn.LOO) {
            return ((IFn.LOO)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a, b);
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object invokeOLO(Object f0, Object a, long b) {
        if(f0 instanceof IFn.OLO) {
            return ((IFn.OLO)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a, b);
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object invokeDDO(Object f0, double a, double b) {
        if(f0 instanceof IFn.DDO) {
            return ((IFn.DDO)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a, b);
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object invokeLDO(Object f0, long a, double b) {
        if(f0 instanceof IFn.LDO) {
            return ((IFn.LDO)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a, b);
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object invokeDLO(Object f0, double a, long b) {
        if(f0 instanceof IFn.DLO) {
            return ((IFn.DLO)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a, b);
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object invokeOOO(Object f0, Object a, Object b) {
        if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a, b);
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object invokeODO(Object f0, Object a, double b) {
        if(f0 instanceof IFn.ODO) {
            return ((IFn.ODO)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a, b);
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object invokeDOO(Object f0, double a, Object b) {
        if(f0 instanceof IFn.DOO) {
            return ((IFn.DOO)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a, b);
        } else {
            throw notIFnError(f0);
        }
    }

    public static float invokeLLF(Object f0, long a, long b) {
        if(f0 instanceof IFn) {
            return RT.floatCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static float invokeLOF(Object f0, long a, Object b) {
        if(f0 instanceof IFn) {
            return RT.floatCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static float invokeOLF(Object f0, Object a, long b) {
        if(f0 instanceof IFn) {
            return RT.floatCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static float invokeDDF(Object f0, double a, double b) {
        if(f0 instanceof IFn) {
            return RT.floatCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static float invokeLDF(Object f0, long a, double b) {
        if(f0 instanceof IFn) {
            return RT.floatCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static float invokeDLF(Object f0, double a, long b) {
        if(f0 instanceof IFn) {
            return RT.floatCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static float invokeOOF(Object f0, Object a, Object b) {
        if(f0 instanceof IFn) {
            return RT.floatCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static float invokeODF(Object f0, Object a, double b) {
        if(f0 instanceof IFn) {
            return RT.floatCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static float invokeDOF(Object f0, double a, Object b) {
        if(f0 instanceof IFn) {
            return RT.floatCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean invokeOOOB(Object f0, Object a, Object b, Object c) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a, b, c));
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object invokeOOOO(Object f0, Object a, Object b, Object c) {
        if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a, b, c);
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean invokeOOOOB(Object f0, Object a, Object b, Object c, Object d) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a, b, c, d));
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object invokeOOOOO(Object f0, Object a, Object b, Object c, Object d) {
        if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a, b, c, d);
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean invokeOOOOOB(Object f0, Object a, Object b, Object c, Object d, Object e) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a, b, c, d, e));
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object invokeOOOOOO(Object f0, Object a, Object b, Object c, Object d, Object e) {
        if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a, b, c, d, e);
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean invokeOOOOOOB(Object f0, Object a, Object b, Object c, Object d, Object e, Object f) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a, b, c, d, e, f));
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object invokeOOOOOOO(Object f0, Object a, Object b, Object c, Object d, Object e, Object f) {
        if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a, b, c, d, e, f);
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean invokeOOOOOOOB(Object f0, Object a, Object b, Object c, Object d, Object e, Object f, Object g) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a, b, c, d, e, f, g));
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object invokeOOOOOOOO(Object f0, Object a, Object b, Object c, Object d, Object e, Object f, Object g) {
        if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a, b, c, d, e, f, g);
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean invokeOOOOOOOOB(Object f0, Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a, b, c, d, e, f, g, h));
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object invokeOOOOOOOOO(Object f0, Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h) {
        if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a, b, c, d, e, f, g, h);
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean invokeOOOOOOOOOB(Object f0, Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h, Object i) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a, b, c, d, e, f, g, h, i));
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object invokeOOOOOOOOOO(Object f0, Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h, Object i) {
        if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a, b, c, d, e, f, g, h, i);
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean invokeOOOOOOOOOOB(Object f0, Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h, Object i, Object j) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a, b, c, d, e, f, g, h, i, j));
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object invokeOOOOOOOOOOO(Object f0, Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h, Object i, Object j) {
        if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a, b, c, d, e, f, g, h, i, j);
        } else {
            throw notIFnError(f0);
        }
    }

}
