package clojure.test;

import clojure.java.api.Clojure;
import clojure.lang.IFn;

public class SwissArmy {
    public static String doppelganger = "static-field";
    public String ctorId;
    public static IFn idFn = Clojure.var("clojure.core", "identity");

    public SwissArmy() {this.ctorId = "1";}
    public SwissArmy(int a, long b) {this.ctorId = "2";}
    public SwissArmy(long a, int b) {this.ctorId = "3";}
    public SwissArmy(boolean a, boolean b) {this.ctorId = "4";}
    public SwissArmy(long[] a, int b) {this.ctorId = "5";}
    public SwissArmy(String[] a, long b) {this.ctorId = "6";}
    public SwissArmy(int[][] a, long b) {this.ctorId = "7";}

    public String noArgs() {return "";}
    public String twoArgsIL(int a, long b) {return "int-long";}
    public String twoArgsLI(long a, int b) {return "long-int";}
    public String twoArgsBB(boolean a, boolean b) {return "boolean-boolean";}
//    public String twoArgsLAI(long[] a, int b) {return "long<>-int";}
    public String twoArgsSAL(String[] a, long b) {return "java.lang.String<>-long";}
//    public String twoArgsMDIL(int[][] a, long b) {return "int<><>-long";}
    public String arityOverloadMethod(int a) {return "int";}
    public String arityOverloadMethod(int a, int b) {return "int-int";}
    public String arityOverloadMethod(int a, int b, int c) {return "int-int-int";}
    public String doppelganger(int a, int b) {return "int-int";}

    public static String staticNoArgs() {return "";}
    public static String staticOneArg(boolean a) {return "boolean";}
    public static String staticTwoArgsIL(int a, long b) {return "int-long";}
    public static String staticTwoArgsLI(long a, int b) {return "long-int";}
    public static String staticTwoArgsBB(boolean a, boolean b) {return "boolean-boolean";}
    public static String staticTwoArgsSAL(String[] a, long b) {return "java.lang.String<>-long";}
//    public static String staticTwoArgsMDIL(int[][] a, long b) {return "int<><>-long";}
//    public static String couldReflect(long[] a, int b) {return "long<>-int";}
    public static String couldReflect(Object[] a, int b) {return "java.lang.Object<>-int";}
    public static String staticArityOverloadMethod(int a) {return "int";}
    public static String staticArityOverloadMethod(int a, int b) {return "int-int";}
    public static String staticArityOverloadMethod(int a, int b, int c) {return "int-int-int";}
    public static String doppelganger(int a, int b, long c) {return "int-int-long";}
    public static String doppelganger() {return "";}
}
