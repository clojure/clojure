package clojure.test;

public class ConcreteClass implements GenericInterface<Integer> {
    @Override
    public Integer stampWidgets(Integer val) {
        return 42;
    }
}