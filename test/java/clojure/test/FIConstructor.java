package clojure.test;

import java.util.Arrays;
import java.util.List;
import java.util.function.Predicate;

public class FIConstructor {

    public List<Integer> numbers;

    public FIConstructor(Predicate<Integer> pred) {
        List<Integer> numbers = Arrays.asList(-2, -1, 0, 1, 2);
        this.numbers = numbers.stream().filter(pred).toList();
    }

}
