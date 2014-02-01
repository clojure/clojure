package clojure;

import java.io.IOException;
import java.util.ArrayList;

import clojure.lang.RemoteRef;
import clojure.lang.Namespace;
import clojure.lang.ObjCClass;
import clojure.lang.RT;
import clojure.lang.RemoteRepl;
import clojure.lang.Selector;
import clojure.lang.Symbol;
import clojure.lang.Var;

public class Demo {

  static ArrayList<String> libs = new ArrayList<String>();

  public static void main(String[] args) throws Exception {
    RT.load("clojure/core");
    RemoteRepl.listen();
    //runTests();
  }

  public static void runTests() throws IOException, ClassNotFoundException {
    init();
    RT.forceClass = true;
    long d = System.currentTimeMillis();

    for (String f : libs) {
      RT.load(f.replaceAll("\\.", "/").replaceAll("-", "_"));
    }
    init();

    Var.pushThreadBindings(RT.map(RT.CURRENT_NS,
        Namespace.find(Symbol.intern("clojure.core"))));
    RT.var("clojure.test", "run-all-tests").invoke();
    Var.popThreadBindings();

    System.out.println(System.currentTimeMillis() - d);
  }

  private static void init() throws IOException, ClassNotFoundException {
    libs.add("clojure/core");
    libs.add("clojure/main");
    libs.add("clojure.test-clojure.agents");
    libs.add("clojure.test-clojure.control");
    libs.add("clojure.test-clojure.multimethods");
    libs.add("clojure.test-clojure.protocols");
    libs.add("clojure.test-clojure.special");
    libs.add("clojure.test-clojure.data");
    libs.add("clojure.test-clojure.reader");
    libs.add("clojure.test-clojure.string");
    libs.add("clojure.test-clojure.data-structures");
    libs.add("clojure.test-clojure.generators");
    libs.add("clojure.test-clojure.numbers");
    libs.add("clojure.test-clojure.test");
    libs.add("clojure.test-clojure.api");
    libs.add("clojure.test-clojure.other-functions");
    libs.add("clojure.test-clojure.test-fixtures");
    libs.add("clojure.test-clojure.atoms");
    libs.add("clojure.test-clojure.delays");
    libs.add("clojure.test-clojure.parallel");
    libs.add("clojure.test-clojure.refs");
    libs.add("clojure.test-clojure.transients");
    libs.add("clojure.test-clojure.clojure-set");
    libs.add("clojure.test-clojure.edn");
    libs.add("clojure.test-clojure.keywords");
    libs.add("clojure.test-clojure.try-catch");
    libs.add("clojure.test-clojure.clojure-walk");
    libs.add("clojure.test-clojure.logic");
    libs.add("clojure.test-clojure.vars");
    libs.add("clojure.test-clojure.clojure-xml");
    libs.add("clojure.test-clojure.macros");
    libs.add("clojure.test-clojure.predicates");
    libs.add("clojure.test-clojure.vectors");
    libs.add("clojure.test-clojure.clojure-zip");
    libs.add("clojure.test-clojure.fn");
    libs.add("clojure.test-clojure.printer");
    libs.add("clojure.test-clojure.sequences");
    libs.add("clojure.test-clojure.for");
    //libs.add("clojure.test-clojure.evaluation");
    // libs.add("clojure.test-clojure.serialization");
    // libs.add("clojure.test-clojure.main");
    // libs.add("clojure.test-clojure.def");
    // libs.add("clojure.test-clojure.reflect");
    // libs.add("clojure.test-clojure.java-interop");
    // libs.add("clojure.test-clojure.errors");
    // libs.add("clojure.test-clojure.pprint");
    // libs.add("clojure.test-clojure.repl");
    // libs.add("clojure.test-clojure.rt");
    // libs.add("clojure.test-clojure.compilation");
    // libs.add("clojure.test-clojure.metadata");
    // libs.add("clojure.test_clojure.annotations");
    // libs.add("clojure.test-clojure.genclass");
    // libs.add("clojure.test-clojure.ns-libs");
  }
}
