package clojure.test;

import java.util.Arrays;
import java.util.List;
import java.util.function.Predicate;

public class FIStatic {

    public static List<Object> numbers(Predicate<Integer> pred) {
        List<Integer> numbers = Arrays.asList(-2, -1, 0, 1, 2);
        Object[] filteredNumbers =  numbers.stream().filter(pred).toArray();
        return Arrays.asList(filteredNumbers);
    }
}
