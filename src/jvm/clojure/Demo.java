package clojure;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import clojure.lang.Compiler;
import clojure.lang.IFn;
import clojure.lang.IPersistentMap;
import clojure.lang.RT;
import clojure.lang.Symbol;
import clojure.lang.Var;

public class Demo {

  static ArrayList<String> libs = new ArrayList<String>();

  public static void main(String[] args) throws Exception {
    init();
    if (false) {
      long d = System.currentTimeMillis();

      init();

      RT.var("clojure.test", "run-all-tests").invoke();

      System.out.println(System.currentTimeMillis() - d);
    } else {
      try {
        deleteDirectory(new File("target/src"));
        new File("target/src").mkdir();
        RT.load("clojure/core");
        IFn compile = RT.var("clojure.core", "compile");
        Var.pushThreadBindings(RT.mapUniqueKeys(
            clojure.lang.Compiler.COMPILE_PATH, "target/gen",
            clojure.lang.Compiler.SOURCE_GEN_PATH, "target/src",
            clojure.lang.Compiler.COMPILE_FILES, Boolean.TRUE));
        IPersistentMap options = (IPersistentMap) Compiler.COMPILER_OPTIONS
            .deref();
        for (String f : libs) {
          
          compile.invoke(Symbol.intern(f));
        }
        init();

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

  private static void init() throws IOException, ClassNotFoundException {
    // RT.load("clojure/core");
    // RT.load("clojure/main");
    // RT.load("clojure/test_clojure/control");
    // if (true) {
    // return;
    // }
    libs.add("clojure/core");
    libs.add("clojure/main");
    libs.add("clojure/test_clojure/vectors");
    libs.add("clojure/test_clojure/agents");
    libs.add("clojure/test_clojure/atoms");
    libs.add("clojure/test_clojure/clojure_set");
    libs.add("clojure/test_clojure/clojure_walk");
    libs.add("clojure/test_clojure/control");
    libs.add("clojure/test_clojure/delays");
    libs.add("clojure/test_clojure/for");
    libs.add("clojure/test_clojure/keywords");
    libs.add("clojure/test_clojure/refs");
    libs.add("clojure/test_clojure/vars");
    libs.add("clojure/test_clojure/sequences");
    libs.add("clojure/test_clojure/errors");
    libs.add("clojure/test_clojure/logic");
    libs.add("clojure/test_clojure/macros");
    libs.add("clojure/test_clojure/string");
    libs.add("clojure/test_clojure/fn");
    libs.add("clojure/test_clojure/transients");
    libs.add("clojure/test_clojure/clojure_xml");
    libs.add("clojure/test_clojure/clojure_zip");
    libs.add("clojure/test_clojure/edn");
    libs.add("clojure/test_clojure/numbers");
    libs.add("clojure/test_clojure/metadata");
    libs.add("clojure/test_clojure/multimethods");
    libs.add("clojure/test_clojure/other_functions");

    // libs.add("clojure/test_clojure/api");

    // libs.add("clojure/test_clojure/generators");
    // libs.add("clojure/test_clojure/main");
    // libs.add("clojure/test_clojure/java_interop");
    // libs.add("clojure/test_clojure/parallel");
    // libs.add("clojure/test_clojure/reader");
    // libs.add("clojure/test_clojure/predicates");
    // libs.add("clojure/test_clojure/printer");
    // libs.add("clojure/test_clojure/data_structures");
    // libs.add("clojure/test_clojure/data");
    // libs.add("clojure/test_clojure/special");
    // libs.add("clojure/test_clojure/repl");
    // libs.add("clojure/test_clojure/test_fixtures");
    // libs.add("clojure/test_clojure/rt");
    // libs.add("clojure/test_clojure/test");

    // libs.add("clojure/test_clojure/reducers");
    // libs.add("clojure/test_clojure/serialization");
    // libs.add("clojure/test_clojure/pprint");
    // libs.add("clojure/test_clojure/reflect");
    // libs.add("clojure/test_clojure/evaluation");
    // libs.add("clojure/test_clojure/def");
    // libs.add("clojure/test_clojure/annotations");
    // libs.add("clojure/test_clojure/genclass");
    // libs.add("clojure/test_clojure/try_catch");
    // libs.add("clojure/test_clojure/protocols");
    // libs.add("clojure/test_clojure/ns_libs");
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
