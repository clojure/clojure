package clojure.test;

import java.util.Arrays;
        import java.util.List;
        import java.util.function.Predicate;

public class FIStatic {

    public static List<Integer> numbers(Predicate<Integer> pred) {
        List<Integer> numbers = Arrays.asList(-2, -1, 0, 1, 2);
        return numbers.stream().filter(pred).toList();
    }
}
