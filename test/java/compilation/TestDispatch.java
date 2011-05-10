package compilation;

public class TestDispatch {
    public static String someMethod (int a, int b) {
        return "(int, int)";
    }
    
    public static String someMethod (int a, long b) {
        return "(int, long)";
    }
    
    public static String someMethod (long a, long b) {
        return "(long, long)";
    }
}