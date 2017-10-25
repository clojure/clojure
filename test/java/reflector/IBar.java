package reflector;

public interface IBar {
    String stuff();

    class Factory {
        public static IBar get() {
            return new SubBar();
        }
    }
}

class Bar {
    public String stuff() {
        return "stuff";
    }
}

class SubBar extends Bar implements IBar {
}
