package clojure;

import clojure.lang.Compiler;
import clojure.lang.RT;
import clojure.lang.Var;

public class Demo {

  public static void main(String[] args) throws Exception {
    RT.var("clojure.core", "map");
    Var.pushThreadBindings(RT
        .mapUniqueKeys(Compiler.COMPILE_PATH, "target/gen", Compiler.COMPILE_FILES, Boolean.TRUE));
    RT.load("clojure/gal");
    Var.popThreadBindings();
  }
}
