package clojure.test;

public class FunctionalTester {
    public char testVar;

    @FunctionalInterface
    public interface FI {
        char action(String str, int pos);
    }

    public FunctionalTester(String str, int pos, FI fi) {
        testVar = fi.action(str, pos);
    }

    public void instanceMethodWithFIArg(String str, int pos, FI fi) {
        testVar = fi.action(str, pos);
    }

    public static char staticMethodWithFIArg(String str, int pos, FI fi) {
        return fi.action(str, pos);
    }

    public static char getChar(String str, int pos) {
        return str.charAt(pos);
    }

    public static char getChar(int value, long n) {
        return "Fail".charAt(0);
    }

}


