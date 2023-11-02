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

public class FnAdapters {

    private static RuntimeException notIFnError(Object f) {
        return new RuntimeException("Expected function, but found " + (f == null ? "null" : f.getClass().getName()));
    }

    public static long adaptLL(Object f0, long a) {
        if(f0 instanceof IFn.LL) {
            return ((IFn.LL)f0).invokePrim(a);
        } else if(f0 instanceof IFn) {
            return RT.longCast(((IFn)f0).invoke(a));
        } else {
            throw notIFnError(f0);
        }
    }

    public static long adaptDL(Object f0, double a) {
        if(f0 instanceof IFn.DL) {
            return ((IFn.DL)f0).invokePrim(a);
        } else if(f0 instanceof IFn) {
            return RT.longCast(((IFn)f0).invoke(a));
        } else {
            throw notIFnError(f0);
        }
    }

    public static long adaptOL(Object f0, Object a) {
        if(f0 instanceof IFn.OL) {
            return ((IFn.OL)f0).invokePrim(a);
        } else if(f0 instanceof IFn) {
            return RT.longCast(((IFn)f0).invoke(a));
        } else {
            throw notIFnError(f0);
        }
    }

    public static int adaptLI(Object f0, long a) {
        if(f0 instanceof IFn.LL) {
            return RT.intCast(((IFn.LL)f0).invokePrim(a));
        } else if(f0 instanceof IFn) {
            return RT.intCast(((IFn)f0).invoke(a));
        } else {
            throw notIFnError(f0);
        }
    }

    public static int adaptDI(Object f0, double a) {
        if(f0 instanceof IFn.DL) {
            return RT.intCast(((IFn.DL)f0).invokePrim(a));
        } else if(f0 instanceof IFn) {
            return RT.intCast(((IFn)f0).invoke(a));
        } else {
            throw notIFnError(f0);
        }
    }

    public static int adaptOI(Object f0, Object a) {
        if(f0 instanceof IFn.OL) {
            return RT.intCast(((IFn.OL)f0).invokePrim(a));
        } else if(f0 instanceof IFn) {
            return RT.intCast(((IFn)f0).invoke(a));
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean adaptLB(Object f0, long a) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a));
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean adaptDB(Object f0, double a) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a));
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean adaptOB(Object f0, Object a) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a));
        } else {
            throw notIFnError(f0);
        }
    }

    public static double adaptLD(Object f0, long a) {
        if(f0 instanceof IFn.LD) {
            return ((IFn.LD)f0).invokePrim(a);
        } else if(f0 instanceof IFn) {
            return RT.doubleCast(((IFn)f0).invoke(a));
        } else {
            throw notIFnError(f0);
        }
    }

    public static double adaptDD(Object f0, double a) {
        if(f0 instanceof IFn.DD) {
            return ((IFn.DD)f0).invokePrim(a);
        } else if(f0 instanceof IFn) {
            return RT.doubleCast(((IFn)f0).invoke(a));
        } else {
            throw notIFnError(f0);
        }
    }

    public static double adaptOD(Object f0, Object a) {
        if(f0 instanceof IFn.OD) {
            return ((IFn.OD)f0).invokePrim(a);
        } else if(f0 instanceof IFn) {
            return RT.doubleCast(((IFn)f0).invoke(a));
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object adaptLO(Object f0, long a) {
        if(f0 instanceof IFn.LO) {
            return ((IFn.LO)f0).invokePrim(a);
        } else if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a);
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object adaptDO(Object f0, double a) {
        if(f0 instanceof IFn.DO) {
            return ((IFn.DO)f0).invokePrim(a);
        } else if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a);
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object adaptOO(Object f0, Object a) {
        if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a);
        } else {
            throw notIFnError(f0);
        }
    }

    public static float adaptLF(Object f0, long a) {
        if(f0 instanceof IFn) {
            return RT.floatCast(((IFn)f0).invoke(a));
        } else {
            throw notIFnError(f0);
        }
    }

    public static float adaptDF(Object f0, double a) {
        if(f0 instanceof IFn) {
            return RT.floatCast(((IFn)f0).invoke(a));
        } else {
            throw notIFnError(f0);
        }
    }

    public static float adaptOF(Object f0, Object a) {
        if(f0 instanceof IFn) {
            return RT.floatCast(((IFn)f0).invoke(a));
        } else {
            throw notIFnError(f0);
        }
    }

    public static long adaptLLL(Object f0, long a, long b) {
        if(f0 instanceof IFn.LLL) {
            return ((IFn.LLL)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return RT.longCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static long adaptLOL(Object f0, long a, Object b) {
        if(f0 instanceof IFn.LOL) {
            return ((IFn.LOL)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return RT.longCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static long adaptOLL(Object f0, Object a, long b) {
        if(f0 instanceof IFn.OLL) {
            return ((IFn.OLL)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return RT.longCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static long adaptDDL(Object f0, double a, double b) {
        if(f0 instanceof IFn.DDL) {
            return ((IFn.DDL)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return RT.longCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static long adaptLDL(Object f0, long a, double b) {
        if(f0 instanceof IFn.LDL) {
            return ((IFn.LDL)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return RT.longCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static long adaptDLL(Object f0, double a, long b) {
        if(f0 instanceof IFn.DLL) {
            return ((IFn.DLL)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return RT.longCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static long adaptOOL(Object f0, Object a, Object b) {
        if(f0 instanceof IFn.OOL) {
            return ((IFn.OOL)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return RT.longCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static long adaptODL(Object f0, Object a, double b) {
        if(f0 instanceof IFn.ODL) {
            return ((IFn.ODL)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return RT.longCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static long adaptDOL(Object f0, double a, Object b) {
        if(f0 instanceof IFn.DOL) {
            return ((IFn.DOL)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return RT.longCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static int adaptLLI(Object f0, long a, long b) {
        if(f0 instanceof IFn.LLL) {
            return RT.intCast(((IFn.LLL)f0).invokePrim(a, b));
        } else if(f0 instanceof IFn) {
            return RT.intCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static int adaptLOI(Object f0, long a, Object b) {
        if(f0 instanceof IFn.LOL) {
            return RT.intCast(((IFn.LOL)f0).invokePrim(a, b));
        } else if(f0 instanceof IFn) {
            return RT.intCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static int adaptOLI(Object f0, Object a, long b) {
        if(f0 instanceof IFn.OLL) {
            return RT.intCast(((IFn.OLL)f0).invokePrim(a, b));
        } else if(f0 instanceof IFn) {
            return RT.intCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static int adaptDDI(Object f0, double a, double b) {
        if(f0 instanceof IFn.DDL) {
            return RT.intCast(((IFn.DDL)f0).invokePrim(a, b));
        } else if(f0 instanceof IFn) {
            return RT.intCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static int adaptLDI(Object f0, long a, double b) {
        if(f0 instanceof IFn.LDL) {
            return RT.intCast(((IFn.LDL)f0).invokePrim(a, b));
        } else if(f0 instanceof IFn) {
            return RT.intCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static int adaptDLI(Object f0, double a, long b) {
        if(f0 instanceof IFn.DLL) {
            return RT.intCast(((IFn.DLL)f0).invokePrim(a, b));
        } else if(f0 instanceof IFn) {
            return RT.intCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static int adaptOOI(Object f0, Object a, Object b) {
        if(f0 instanceof IFn.OOL) {
            return RT.intCast(((IFn.OOL)f0).invokePrim(a, b));
        } else if(f0 instanceof IFn) {
            return RT.intCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static int adaptODI(Object f0, Object a, double b) {
        if(f0 instanceof IFn.ODL) {
            return RT.intCast(((IFn.ODL)f0).invokePrim(a, b));
        } else if(f0 instanceof IFn) {
            return RT.intCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static int adaptDOI(Object f0, double a, Object b) {
        if(f0 instanceof IFn.DOL) {
            return RT.intCast(((IFn.DOL)f0).invokePrim(a, b));
        } else if(f0 instanceof IFn) {
            return RT.intCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean adaptLLB(Object f0, long a, long b) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean adaptLOB(Object f0, long a, Object b) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean adaptOLB(Object f0, Object a, long b) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean adaptDDB(Object f0, double a, double b) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean adaptLDB(Object f0, long a, double b) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean adaptDLB(Object f0, double a, long b) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean adaptOOB(Object f0, Object a, Object b) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean adaptODB(Object f0, Object a, double b) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean adaptDOB(Object f0, double a, Object b) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static double adaptLLD(Object f0, long a, long b) {
        if(f0 instanceof IFn.LLD) {
            return ((IFn.LLD)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return RT.doubleCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static double adaptLOD(Object f0, long a, Object b) {
        if(f0 instanceof IFn.LOD) {
            return ((IFn.LOD)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return RT.doubleCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static double adaptOLD(Object f0, Object a, long b) {
        if(f0 instanceof IFn.OLD) {
            return ((IFn.OLD)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return RT.doubleCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static double adaptDDD(Object f0, double a, double b) {
        if(f0 instanceof IFn.DDD) {
            return ((IFn.DDD)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return RT.doubleCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static double adaptLDD(Object f0, long a, double b) {
        if(f0 instanceof IFn.LDD) {
            return ((IFn.LDD)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return RT.doubleCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static double adaptDLD(Object f0, double a, long b) {
        if(f0 instanceof IFn.DLD) {
            return ((IFn.DLD)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return RT.doubleCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static double adaptOOD(Object f0, Object a, Object b) {
        if(f0 instanceof IFn.OOD) {
            return ((IFn.OOD)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return RT.doubleCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static double adaptODD(Object f0, Object a, double b) {
        if(f0 instanceof IFn.ODD) {
            return ((IFn.ODD)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return RT.doubleCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static double adaptDOD(Object f0, double a, Object b) {
        if(f0 instanceof IFn.DOD) {
            return ((IFn.DOD)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return RT.doubleCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object adaptLLO(Object f0, long a, long b) {
        if(f0 instanceof IFn.LLO) {
            return ((IFn.LLO)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a, b);
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object adaptLOO(Object f0, long a, Object b) {
        if(f0 instanceof IFn.LOO) {
            return ((IFn.LOO)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a, b);
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object adaptOLO(Object f0, Object a, long b) {
        if(f0 instanceof IFn.OLO) {
            return ((IFn.OLO)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a, b);
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object adaptDDO(Object f0, double a, double b) {
        if(f0 instanceof IFn.DDO) {
            return ((IFn.DDO)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a, b);
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object adaptLDO(Object f0, long a, double b) {
        if(f0 instanceof IFn.LDO) {
            return ((IFn.LDO)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a, b);
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object adaptDLO(Object f0, double a, long b) {
        if(f0 instanceof IFn.DLO) {
            return ((IFn.DLO)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a, b);
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object adaptOOO(Object f0, Object a, Object b) {
        if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a, b);
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object adaptODO(Object f0, Object a, double b) {
        if(f0 instanceof IFn.ODO) {
            return ((IFn.ODO)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a, b);
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object adaptDOO(Object f0, double a, Object b) {
        if(f0 instanceof IFn.DOO) {
            return ((IFn.DOO)f0).invokePrim(a, b);
        } else if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a, b);
        } else {
            throw notIFnError(f0);
        }
    }

    public static float adaptLLF(Object f0, long a, long b) {
        if(f0 instanceof IFn) {
            return RT.floatCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static float adaptLOF(Object f0, long a, Object b) {
        if(f0 instanceof IFn) {
            return RT.floatCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static float adaptOLF(Object f0, Object a, long b) {
        if(f0 instanceof IFn) {
            return RT.floatCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static float adaptDDF(Object f0, double a, double b) {
        if(f0 instanceof IFn) {
            return RT.floatCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static float adaptLDF(Object f0, long a, double b) {
        if(f0 instanceof IFn) {
            return RT.floatCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static float adaptDLF(Object f0, double a, long b) {
        if(f0 instanceof IFn) {
            return RT.floatCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static float adaptOOF(Object f0, Object a, Object b) {
        if(f0 instanceof IFn) {
            return RT.floatCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static float adaptODF(Object f0, Object a, double b) {
        if(f0 instanceof IFn) {
            return RT.floatCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static float adaptDOF(Object f0, double a, Object b) {
        if(f0 instanceof IFn) {
            return RT.floatCast(((IFn)f0).invoke(a, b));
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean adaptOOOB(Object f0, Object a, Object b, Object c) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a, b, c));
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object adaptOOOO(Object f0, Object a, Object b, Object c) {
        if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a, b, c);
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean adaptOOOOB(Object f0, Object a, Object b, Object c, Object d) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a, b, c, d));
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object adaptOOOOO(Object f0, Object a, Object b, Object c, Object d) {
        if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a, b, c, d);
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean adaptOOOOOB(Object f0, Object a, Object b, Object c, Object d, Object e) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a, b, c, d, e));
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object adaptOOOOOO(Object f0, Object a, Object b, Object c, Object d, Object e) {
        if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a, b, c, d, e);
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean adaptOOOOOOB(Object f0, Object a, Object b, Object c, Object d, Object e, Object f) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a, b, c, d, e, f));
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object adaptOOOOOOO(Object f0, Object a, Object b, Object c, Object d, Object e, Object f) {
        if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a, b, c, d, e, f);
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean adaptOOOOOOOB(Object f0, Object a, Object b, Object c, Object d, Object e, Object f, Object g) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a, b, c, d, e, f, g));
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object adaptOOOOOOOO(Object f0, Object a, Object b, Object c, Object d, Object e, Object f, Object g) {
        if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a, b, c, d, e, f, g);
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean adaptOOOOOOOOB(Object f0, Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a, b, c, d, e, f, g, h));
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object adaptOOOOOOOOO(Object f0, Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h) {
        if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a, b, c, d, e, f, g, h);
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean adaptOOOOOOOOOB(Object f0, Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h, Object i) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a, b, c, d, e, f, g, h, i));
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object adaptOOOOOOOOOO(Object f0, Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h, Object i) {
        if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a, b, c, d, e, f, g, h, i);
        } else {
            throw notIFnError(f0);
        }
    }

    public static boolean adaptOOOOOOOOOOB(Object f0, Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h, Object i, Object j) {
        if(f0 instanceof IFn) {
            return RT.booleanCast(((IFn)f0).invoke(a, b, c, d, e, f, g, h, i, j));
        } else {
            throw notIFnError(f0);
        }
    }

    public static Object adaptOOOOOOOOOOO(Object f0, Object a, Object b, Object c, Object d, Object e, Object f, Object g, Object h, Object i, Object j) {
        if(f0 instanceof IFn) {
            return ((IFn)f0).invoke(a, b, c, d, e, f, g, h, i, j);
        } else {
            throw notIFnError(f0);
        }
    }

}
