package clojure.lang;

public class FnAdapters {

    private static RuntimeException notIFnError(Object f) {
        return new RuntimeException("Expected function, but found " + (f == null ? "null" : f.getClass().getName()));
    }

    public static Object adaptO(Object f) {
        if(f instanceof IFn) {
            return ((IFn)f).invoke();
        } else {
            throw notIFnError(f);
        }
    }

    public static long adaptL(Object f) {
        if(f instanceof IFn.L) {
            return ((IFn.L)f).invokePrim();
        } else if(f instanceof IFn) {
            return RT.longCast(((IFn)f).invoke());
        } else {
            throw notIFnError(f);
        }
    }

    public static int adaptI(Object f) {
        if(f instanceof IFn.L) {
            return RT.intCast(((IFn.L)f).invokePrim());  // narrow
        } else if(f instanceof IFn) {
            return RT.intCast(((IFn)f).invoke()); // possibly narrow
        } else {
            throw notIFnError(f);
        }
    }

    public static double adaptD(Object f) {
        if(f instanceof IFn.D) {
            return ((IFn.D)f).invokePrim();
        } else if(f instanceof IFn) {
            return RT.doubleCast(((IFn)f).invoke());
        } else {
            throw notIFnError(f);
        }
    }

    public static boolean adaptB(Object f) {
        if(f instanceof IFn) {
            return RT.booleanCast(((IFn)f).invoke());
        } else {
            throw notIFnError(f);
        }
    }

    public static Object adaptOO(Object f, Object a) {
        if(f instanceof IFn) {
            return ((IFn)f).invoke(a);
        } else {
            throw notIFnError(f);
        }
    }

    public static boolean adaptOB(Object f, Object a) {
        if(f instanceof IFn) {
            return RT.booleanCast(((IFn)f).invoke(a));
        } else {
            throw notIFnError(f);
        }
    }

    public static long adaptLL(Object f, long a) {
        if(f instanceof IFn.LL) {
            return ((IFn.LL)f).invokePrim(a);
        } else if(f instanceof IFn) {
            return RT.longCast(((IFn)f).invoke(a));
        } else {
            throw notIFnError(f);
        }
    }

    public static boolean adaptLB(Object f, long a) {
        if(f instanceof IFn.LO) {
            return RT.booleanCast(((IFn.LO)f).invokePrim(a));
        } else if(f instanceof IFn) {
            return RT.booleanCast(((IFn)f).invoke(a));
        } else {
            throw notIFnError(f);
        }
    }

    public static Object adaptLO(Object f, long a) {
        if(f instanceof IFn.LO) {
            return ((IFn.LO)f).invokePrim(a);
        } else if(f instanceof IFn) {
            return ((IFn)f).invoke(a);
        } else {
            throw notIFnError(f);
        }
    }

    public static int adaptLLI(Object f, long a, long b) {
        if(f instanceof IFn.LLL) {
            return RT.intCast(((IFn.LLL)f).invokePrim(a, b));
        } else if(f instanceof IFn) {
            return RT.intCast(((IFn)f).invoke(a, b));
        } else {
            throw notIFnError(f);
        }
    }

    public static double adaptLD(Object f, long a) {
        if(f instanceof IFn.LD) {
            return ((IFn.LD)f).invokePrim(a);
        } else if(f instanceof IFn) {
            return RT.doubleCast(((IFn)f).invoke(a));
        } else {
            throw notIFnError(f);
        }
    }

    public static long adaptDL(Object f, double a) {
        if(f instanceof IFn.DL) {
            return ((IFn.DL)f).invokePrim(a);
        } else if(f instanceof IFn) {
            return RT.longCast(((IFn)f).invoke(a));
        } else {
            throw notIFnError(f);
        }
    }

    public static Object adaptOOO(Object f, Object a, Object b) {
        if(f instanceof IFn) {
            return ((IFn)f).invoke(a, b);
        } else {
            throw notIFnError(f);
        }
    }

    public static Object adaptODO(Object f, Object a, double b) {
        if(f instanceof IFn.ODO) {
            return ((IFn.ODO)f).invokePrim(a, b);
        } else if(f instanceof IFn) {
            return ((IFn)f).invoke(a, b);
        } else {
            throw notIFnError(f);
        }
    }

    public static boolean adaptOOB(Object f, Object a, Object b) {
        if(f instanceof IFn) {
            return RT.booleanCast(((IFn)f).invoke(a, b));
        } else {
            throw notIFnError(f);
        }
    }


    public static Object adaptOOOO(Object f, Object a, Object b, Object c) {
        if(f instanceof IFn) {
            return ((IFn)f).invoke(a, b, c);
        } else {
            throw notIFnError(f);
        }
    }

    public static Object adaptOOOOO(Object f, Object a, Object b, Object c, Object d) {
        if(f instanceof IFn) {
            return ((IFn)f).invoke(a, b, c, d);
        } else {
            throw notIFnError(f);
        }
    }

}
