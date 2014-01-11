package clojure;

import java.io.File;
import java.io.IOException;

import clojure.lang.Compiler;
import clojure.lang.IPersistentMap;
import clojure.lang.RT;
import clojure.lang.Var;

public class Demo {

  public static void main(String[] args) throws Exception {
    if (true) {
      long d = System.currentTimeMillis();

      loadAll();

      RT.var("clojure.test", "run-all-tests").invoke();

      System.out.println(System.currentTimeMillis() - d);
    } else {
      try {
        deleteDirectory(new File("target/src"));
        new File("target/src").mkdir();
        loadAll();
        Var.pushThreadBindings(RT.mapUniqueKeys(
            clojure.lang.Compiler.COMPILE_PATH, "target/gen",
            clojure.lang.Compiler.SOURCE_GEN_PATH, "target/src",
            clojure.lang.Compiler.COMPILE_FILES, Boolean.TRUE));
        IPersistentMap options = (IPersistentMap) Compiler.COMPILER_OPTIONS
            .deref();
        // RT.WARN_ON_REFLECTION.bindRoot(RT.T);
        Compiler.RUNTIME.bindRoot(Boolean.TRUE);
        for (String f : RT.loaded) {
          RT.load(f.replaceAll("[.]", "/").replaceAll("-", "_"));
        }

        Var.popThreadBindings();
      } catch (Exception e) {
        e.printStackTrace();
      } finally {
        deleteDirectory(new File("target/gen"));
        new File("target/gen").mkdir();
        System.exit(0);
      }
    }
  }

  private static void loadAll() throws IOException, ClassNotFoundException {
    // RT.load("clojure/core");
    // RT.load("clojure/main");
    // RT.load("clojure/test_clojure/control");
    // if (true) {
    // return;
    // }
    RT.load("clojure/core");
    RT.load("clojure/main");
    RT.load("clojure/test_clojure/agents");
    RT.load("clojure/test_clojure/atoms");
    RT.load("clojure/test_clojure/clojure_set");
    RT.load("clojure/test_clojure/clojure_walk");
    RT.load("clojure/test_clojure/control");
    RT.load("clojure/test_clojure/delays");
    RT.load("clojure/test_clojure/for");
    RT.load("clojure/test_clojure/keywords");
    // RT.load("clojure/test_clojure/vars");
    // RT.load("clojure/test_clojure/sequences");
    // RT.load("clojure/test_clojure/errors");
    // RT.load("clojure/test_clojure/logic");
    // RT.load("clojure/test_clojure/macros");
    // RT.load("clojure/test_clojure/string");
    // RT.load("clojure/test_clojure/fn");
    // RT.load("clojure/test_clojure/transients");

    // RT.load("clojure/test_clojure/api");
    // RT.load("clojure/test_clojure/clojure_xml");
    // RT.load("clojure/test_clojure/clojure_zip");

    // RT.load("clojure/test_clojure/edn");
    // RT.load("clojure/test_clojure/generators");
    // RT.load("clojure/test_clojure/main");
    // RT.load("clojure/test_clojure/metadata");
    // RT.load("clojure/test_clojure/java_interop");
    // RT.load("clojure/test_clojure/parallel");
    // RT.load("clojure/test_clojure/multimethods");
    // RT.load("clojure/test_clojure/reader");
    // RT.load("clojure/test_clojure/predicates");
    // RT.load("clojure/test_clojure/printer");
    // RT.load("clojure/test_clojure/data_structures");
    // RT.load("clojure/test_clojure/data");
    // RT.load("clojure/test_clojure/numbers");
    // RT.load("clojure/test_clojure/other_functions");
    // RT.load("clojure/test_clojure/special");
    // RT.load("clojure/test_clojure/repl");
    // RT.load("clojure/test_clojure/test_fixtures");
    // RT.load("clojure/test_clojure/rt");
    // RT.load("clojure/test_clojure/test");
    // RT.load("clojure/test_clojure/refs");

    // RT.load("clojure/test_clojure/reducers");
    // RT.load("clojure/test_clojure/serialization");
    // RT.load("clojure/test_clojure/pprint");
    // RT.load("clojure/test_clojure/reflect");
    // RT.load("clojure/test_clojure/evaluation");
    // RT.load("clojure/test_clojure/def");
    // RT.load("clojure/test_clojure/vectors");
    // RT.load("clojure/test_clojure/annotations");
    // RT.load("clojure/test_clojure/genclass");
    // RT.load("clojure/test_clojure/try_catch");
    // RT.load("clojure/test_clojure/protocols");
    // RT.load("clojure/test_clojure/ns_libs");
  }

  public static boolean deleteDirectory(File directory) {
    if (directory.exists()) {
      File[] files = directory.listFiles();
      if (null != files) {
        for (int i = 0; i < files.length; i++) {
          if (files[i].isDirectory()) {
            deleteDirectory(files[i]);
          } else {
            files[i].delete();
          }
        }
      }
    }
    return (directory.delete());
  }
}
