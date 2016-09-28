package compilation;

public class ClassWithFailingStaticInitialiser {
    static {
        // Static analysis refuses to compile a static initialiser
        // which will always throw, so we pretend to branch. This may
        // need updating if the static analysis gets cleverer in the
        // future
        if(true) {
            throw new AssertionError("Static Initialiser was run when it shouldn't have been");
        }
    }
}
