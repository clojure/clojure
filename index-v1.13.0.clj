{:namespaces
 ({:doc "Fundamental library of the Clojure language",
   :name "clojure.core",
   :wiki-url "https://clojure.github.io/clojure/clojure.core-api.html",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj"}
  {:doc "Non-core data functions.",
   :author "Stuart Halloway",
   :name "clojure.data",
   :wiki-url "https://clojure.github.io/clojure/clojure.data-api.html",
   :source-url
   "https://github.com/clojure/clojure/blob/51c6d7a70912a8f65e81a8e11ae6f56c94920725/src/clj/clojure/data.clj"}
  {:doc
   "Functions to turn objects into data. Alpha, subject to change",
   :name "clojure.datafy",
   :wiki-url
   "https://clojure.github.io/clojure/clojure.datafy-api.html",
   :source-url
   "https://github.com/clojure/clojure/blob/b70db9639f9acddcabf7f760ea4bb050d6bfaa16/src/clj/clojure/datafy.clj"}
  {:doc "edn reading.",
   :author "Rich Hickey",
   :name "clojure.edn",
   :wiki-url "https://clojure.github.io/clojure/clojure.edn-api.html",
   :source-url
   "https://github.com/clojure/clojure/blob/c6756a8bab137128c8119add29a25b0a88509900/src/clj/clojure/edn.clj"}
  {:doc "Graphical object inspector for Clojure data structures.",
   :author "Rich Hickey",
   :name "clojure.inspector",
   :wiki-url
   "https://clojure.github.io/clojure/clojure.inspector-api.html",
   :source-url
   "https://github.com/clojure/clojure/blob/5da21b38470d175b3cecbf93b9cd145ca36940c1/src/clj/clojure/inspector.clj"}
  {:doc nil,
   :name "clojure.instant",
   :wiki-url
   "https://clojure.github.io/clojure/clojure.instant-api.html",
   :source-url
   "https://github.com/clojure/clojure/blob/ce1927e74b0c71c93375f1db2758d790a826f32c/src/clj/clojure/instant.clj"}
  {:doc
   "The lib basis includes which libraries and versions were loaded both\nfor direct dependencies and transitive dependencies, as well as the\nclasspath and possibly other information from the resolution process.\nThis basis will be known if the runtime was started by the Clojure CLI.\n\nThe Clojure CLI or tools.deps merge a set of deps maps (often from\ndeps.edn files). Additional runtime modifications are supplied via argmap\nkeys, provided via alias maps in the merged deps. Deps maps typically have\n:paths, :deps, and :aliases keys.\n\nThe basis is a superset of merged deps.edn files with the following\nadditional keys:\n  :basis-config - params used to configure basis deps sources, can be\n                  string path, deps map, nil, or :default\n    :root - default = loaded as a resource from tools.deps)\n    :user - default = ~/.clojure/deps.edn)\n    :project - default = ./deps.edn)\n    :extra - default = nil\n    :aliases - coll of keyword aliases to include during dep calculation\n  :argmap - effective argmap (after resolving and merging argmaps from aliases)\n  :libs - map of lib to coord for all included libraries\n  :classpath - classpath map, keys are paths (to directory or .jar), values\n               are maps with source identifier (either :lib-name or :path-key)\n  :classpath-roots - vector of paths in classpath order (keys of :classpath)",
   :name "clojure.java.basis",
   :wiki-url
   "https://clojure.github.io/clojure/clojure.java.basis-api.html",
   :source-url
   "https://github.com/clojure/clojure/blob/b7d87dcd729628a2e80038d05023eb7c7786bb11/src/clj/clojure/java/basis.clj"}
  {:doc "Start a web browser from Clojure",
   :author "Christophe Grand",
   :name "clojure.java.browse",
   :wiki-url
   "https://clojure.github.io/clojure/clojure.java.browse-api.html",
   :source-url
   "https://github.com/clojure/clojure/blob/19e3a2708def5ffb7f2be030d8e8e895464ce2d2/src/clj/clojure/java/browse.clj"}
  {:doc
   "This file defines polymorphic I/O utility functions for Clojure.",
   :author "Stuart Sierra, Chas Emerick, Stuart Halloway",
   :name "clojure.java.io",
   :wiki-url
   "https://clojure.github.io/clojure/clojure.java.io-api.html",
   :source-url
   "https://github.com/clojure/clojure/blob/089f27f9fc881674e565f1ffcbe2f8a616726c3b/src/clj/clojure/java/io.clj"}
  {:doc "A repl helper to quickly open javadocs.",
   :author "Christophe Grand, Stuart Sierra",
   :name "clojure.java.javadoc",
   :wiki-url
   "https://clojure.github.io/clojure/clojure.java.javadoc-api.html",
   :source-url
   "https://github.com/clojure/clojure/blob/3b6c31bc503afe8f25d01d6d7d05ebc960095abd/src/clj/clojure/java/javadoc.clj"}
  {:doc
   "A process invocation API wrapping the Java process API.\n\nThe primary function is 'start' which starts a process and handles the\nstreams as directed. It returns the Process object. Use 'exit-ref' to wait\nfor completion and receive the exit value, and ‘stdout', 'stderr', 'stdin'\nto access the process streams. The 'exec' function handles the common case\nto 'start' a process, wait for process exit, and return stdout.",
   :name "clojure.java.process",
   :wiki-url
   "https://clojure.github.io/clojure/clojure.java.process-api.html",
   :source-url
   "https://github.com/clojure/clojure/blob/afbd2098bf741234ec9efaa25480d02b14863e8c/src/clj/clojure/java/process.clj"}
  {:doc
   "Conveniently launch a sub-process providing its stdin and\ncollecting its stdout",
   :author "Chris Houser, Stuart Halloway",
   :name "clojure.java.shell",
   :wiki-url
   "https://clojure.github.io/clojure/clojure.java.shell-api.html",
   :source-url
   "https://github.com/clojure/clojure/blob/027d8ff2859442b222bf9cfa4c1be45567b788eb/src/clj/clojure/java/shell.clj"}
  {:doc "Top-level main function for Clojure REPL and scripts.",
   :author "Stephen C. Gilardi and Rich Hickey",
   :name "clojure.main",
   :wiki-url "https://clojure.github.io/clojure/clojure.main-api.html",
   :source-url
   "https://github.com/clojure/clojure/blob/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj"}
  {:doc
   "Clojure wrapper functions for java.lang.Math static methods.\n\nFunction calls are inlined for performance, and type hinted for primitive\nlong or double parameters where appropriate. In general, Math methods are\noptimized for performance and have bounds for error tolerance. If\ngreater precision is needed, use java.lang.StrictMath directly instead.\n\nFor more complete information, see:\nhttps://docs.oracle.com/javase/8/docs/api/java/lang/Math.html",
   :author "Alex Miller",
   :name "clojure.math",
   :wiki-url "https://clojure.github.io/clojure/clojure.math-api.html",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj"}
  {:doc
   "A Pretty Printer for Clojure\n\nclojure.pprint implements a flexible system for printing structured data\nin a pleasing, easy-to-understand format. Basic use of the pretty printer is \nsimple, just call pprint instead of println. More advanced users can use \nthe building blocks provided to create custom output formats. \n\nOut of the box, pprint supports a simple structured format for basic data \nand a specialized format for Clojure source code. More advanced formats, \nincluding formats that don't look like Clojure data at all like XML and \nJSON, can be rendered by creating custom dispatch functions. \n\nIn addition to the pprint function, this module contains cl-format, a text \nformatting function which is fully compatible with the format function in \nCommon Lisp. Because pretty printing directives are directly integrated with\ncl-format, it supports very concise custom dispatch. It also provides\na more powerful alternative to Clojure's standard format function.\n\nSee documentation for pprint and cl-format for more information or \ncomplete documentation on the Clojure web site on GitHub.",
   :author "Tom Faulhaber",
   :name "clojure.pprint",
   :wiki-url
   "https://clojure.github.io/clojure/clojure.pprint-api.html",
   :source-url
   "https://github.com/clojure/clojure/blob/6d08609c208ae49a3d411efbdc316ec102fdef1d/src/clj/clojure/pprint.clj"}
  {:doc
   "Reflection on Host Types\nAlpha - subject to change.\n\nTwo main entry points: \n\n* type-reflect reflects on something that implements TypeReference.\n* reflect (for REPL use) reflects on the class of an instance, or\n  on a class if passed a class\n\nKey features:\n\n* Exposes the read side of reflection as pure data. Reflecting\n  on a type returns a map with keys :bases, :flags, and :members.\n\n* Canonicalizes class names as Clojure symbols. Types can extend\n  to the TypeReference protocol to indicate that they can be\n  unambiguously resolved as a type name. The canonical format\n  requires one non-Java-ish convention: array brackets are <>\n  instead of [] so they can be part of a Clojure symbol.\n\n* Pluggable Reflectors for different implementations. The default\n  JavaReflector is good when you have a class in hand, or use\n  the AsmReflector for \"hands off\" reflection without forcing\n  classes to load.\n\nPlatform implementers must:\n\n* Create an implementation of Reflector.\n* Create one or more implementations of TypeReference.\n* def default-reflector to be an instance that satisfies Reflector.",
   :author "Stuart Halloway",
   :name "clojure.reflect",
   :wiki-url
   "https://clojure.github.io/clojure/clojure.reflect-api.html",
   :source-url
   "https://github.com/clojure/clojure/blob/ee00807bac64d55dbc7ec49442d6376352b77200/src/clj/clojure/reflect.clj"}
  {:doc "Utilities meant to be used interactively at the REPL",
   :author
   "Chris Houser, Christophe Grand, Stephen Gilardi, Michel Salim",
   :name "clojure.repl",
   :wiki-url "https://clojure.github.io/clojure/clojure.repl-api.html",
   :source-url
   "https://github.com/clojure/clojure/blob/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj"}
  {:doc "Set operations such as union/intersection.",
   :author "Rich Hickey",
   :name "clojure.set",
   :wiki-url "https://clojure.github.io/clojure/clojure.set-api.html",
   :source-url
   "https://github.com/clojure/clojure/blob/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj"}
  {:doc "Print stack traces oriented towards Clojure, not Java.",
   :author "Stuart Sierra",
   :name "clojure.stacktrace",
   :wiki-url
   "https://clojure.github.io/clojure/clojure.stacktrace-api.html",
   :source-url
   "https://github.com/clojure/clojure/blob/dbb448f7709b20c392558e7d7871d1e9b28c9440/src/clj/clojure/stacktrace.clj"}
  {:doc
   "Clojure String utilities\n\nIt is poor form to (:use clojure.string). Instead, use require\nwith :as to specify a prefix, e.g.\n\n(ns your.namespace.here\n  (:require [clojure.string :as str]))\n\nDesign notes for clojure.string:\n\n1. Strings are objects (as opposed to sequences). As such, the\n   string being manipulated is the first argument to a function;\n   passing nil will result in a NullPointerException unless\n   documented otherwise. If you want sequence-y behavior instead,\n   use a sequence.\n\n2. Functions are generally not lazy, and call straight to host\n   methods where those are available and efficient.\n\n3. Functions take advantage of String implementation details to\n   write high-performing loop/recurs instead of using higher-order\n   functions. (This is not idiomatic in general-purpose application\n   code.)\n\n4. When a function is documented to accept a string argument, it\n   will take any implementation of the correct *interface* on the\n   host platform. In Java, this is CharSequence, which is more\n   general than String. In ordinary usage you will almost always\n   pass concrete strings. If you are doing something unusual,\n   e.g. passing a mutable implementation of CharSequence, then\n   thread-safety is your responsibility.",
   :author "Stuart Sierra, Stuart Halloway, David Liebke",
   :name "clojure.string",
   :wiki-url
   "https://clojure.github.io/clojure/clojure.string-api.html",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj"}
  {:doc
   "Macros that expand to repeated copies of a template expression.",
   :author "Stuart Sierra",
   :name "clojure.template",
   :wiki-url
   "https://clojure.github.io/clojure/clojure.template-api.html",
   :source-url
   "https://github.com/clojure/clojure/blob/c4c0740a0696bc95b2184c0fef55ed7c3bb097f6/src/clj/clojure/template.clj"}
  {:doc
   "A unit testing framework.\n\nASSERTIONS\n\nThe core of the library is the \"is\" macro, which lets you make\nassertions of any arbitrary expression:\n\n(is (= 4 (+ 2 2)))\n(is (instance? Integer 256))\n(is (.startsWith \"abcde\" \"ab\"))\n\nYou can type an \"is\" expression directly at the REPL, which will\nprint a message if it fails.\n\n    user> (is (= 5 (+ 2 2)))\n\n    FAIL in  (:1)\n    expected: (= 5 (+ 2 2))\n      actual: (not (= 5 4))\n    false\n\nThe \"expected:\" line shows you the original expression, and the\n\"actual:\" shows you what actually happened.  In this case, it\nshows that (+ 2 2) returned 4, which is not = to 5.  Finally, the\n\"false\" on the last line is the value returned from the\nexpression.  The \"is\" macro always returns the result of the\ninner expression.\n\nThere are two special assertions for testing exceptions.  The\n\"(is (thrown? c ...))\" form tests if an exception of class c is\nthrown:\n\n(is (thrown? ArithmeticException (/ 1 0))) \n\n\"(is (thrown-with-msg? c re ...))\" does the same thing and also\ntests that the message on the exception matches the regular\nexpression re:\n\n(is (thrown-with-msg? ArithmeticException #\"Divide by zero\"\n                      (/ 1 0)))\n\nDOCUMENTING TESTS\n\n\"is\" takes an optional second argument, a string describing the\nassertion.  This message will be included in the error report.\n\n(is (= 5 (+ 2 2)) \"Crazy arithmetic\")\n\nIn addition, you can document groups of assertions with the\n\"testing\" macro, which takes a string followed by any number of\nassertions.  The string will be included in failure reports.\nCalls to \"testing\" may be nested, and all of the strings will be\njoined together with spaces in the final report, in a style\nsimilar to RSpec <http://rspec.info/>\n\n(testing \"Arithmetic\"\n  (testing \"with positive integers\"\n    (is (= 4 (+ 2 2)))\n    (is (= 7 (+ 3 4))))\n  (testing \"with negative integers\"\n    (is (= -4 (+ -2 -2)))\n    (is (= -1 (+ 3 -4)))))\n\nNote that, unlike RSpec, the \"testing\" macro may only be used\nINSIDE a \"deftest\" or \"with-test\" form (see below).\n\n\nDEFINING TESTS\n\nThere are two ways to define tests.  The \"with-test\" macro takes\na defn or def form as its first argument, followed by any number\nof assertions.  The tests will be stored as metadata on the\ndefinition.\n\n(with-test\n    (defn my-function [x y]\n      (+ x y))\n  (is (= 4 (my-function 2 2)))\n  (is (= 7 (my-function 3 4))))\n\nAs of Clojure SVN rev. 1221, this does not work with defmacro.\nSee http://code.google.com/p/clojure/issues/detail?id=51\n\nThe other way lets you define tests separately from the rest of\nyour code, even in a different namespace:\n\n(deftest addition\n  (is (= 4 (+ 2 2)))\n  (is (= 7 (+ 3 4))))\n\n(deftest subtraction\n  (is (= 1 (- 4 3)))\n  (is (= 3 (- 7 4))))\n\nThis creates functions named \"addition\" and \"subtraction\", which\ncan be called like any other function.  Therefore, tests can be\ngrouped and composed, in a style similar to the test framework in\nPeter Seibel's \"Practical Common Lisp\"\n<http://www.gigamonkeys.com/book/practical-building-a-unit-test-framework.html>\n\n(deftest arithmetic\n  (addition)\n  (subtraction))\n\nThe names of the nested tests will be joined in a list, like\n\"(arithmetic addition)\", in failure reports.  You can use nested\ntests to set up a context shared by several tests.\n\n\nRUNNING TESTS\n\nRun tests with the function \"(run-tests namespaces...)\":\n\n(run-tests 'your.namespace 'some.other.namespace)\n\nIf you don't specify any namespaces, the current namespace is\nused.  To run all tests in all namespaces, use \"(run-all-tests)\".\n\nBy default, these functions will search for all tests defined in\na namespace and run them in an undefined order.  However, if you\nare composing tests, as in the \"arithmetic\" example above, you\nprobably do not want the \"addition\" and \"subtraction\" tests run\nseparately.  In that case, you must define a special function\nnamed \"test-ns-hook\" that runs your tests in the correct order:\n\n(defn test-ns-hook []\n  (arithmetic))\n\nNote: test-ns-hook prevents execution of fixtures (see below).\n\n\nOMITTING TESTS FROM PRODUCTION CODE\n\nYou can bind the variable \"*load-tests*\" to false when loading or\ncompiling code in production.  This will prevent any tests from\nbeing created by \"with-test\" or \"deftest\".\n\n\nFIXTURES\n\nFixtures allow you to run code before and after tests, to set up\nthe context in which tests should be run.\n\nA fixture is just a function that calls another function passed as\nan argument.  It looks like this:\n\n(defn my-fixture [f]\n   Perform setup, establish bindings, whatever.\n  (f)  Then call the function we were passed.\n   Tear-down / clean-up code here.\n )\n\nFixtures are attached to namespaces in one of two ways.  \"each\"\nfixtures are run repeatedly, once for each test function created\nwith \"deftest\" or \"with-test\".  \"each\" fixtures are useful for\nestablishing a consistent before/after state for each test, like\nclearing out database tables.\n\n\"each\" fixtures can be attached to the current namespace like this:\n(use-fixtures :each fixture1 fixture2 ...)\nThe fixture1, fixture2 are just functions like the example above.\nThey can also be anonymous functions, like this:\n(use-fixtures :each (fn [f] setup... (f) cleanup...))\n\nThe other kind of fixture, a \"once\" fixture, is only run once,\naround ALL the tests in the namespace.  \"once\" fixtures are useful\nfor tasks that only need to be performed once, like establishing\ndatabase connections, or for time-consuming tasks.\n\nAttach \"once\" fixtures to the current namespace like this:\n(use-fixtures :once fixture1 fixture2 ...)\n\nNote: Fixtures and test-ns-hook are mutually incompatible.  If you\nare using test-ns-hook, fixture functions will *never* be run.\n\n\nSAVING TEST OUTPUT TO A FILE\n\nAll the test reporting functions write to the var *test-out*.  By\ndefault, this is the same as *out*, but you can rebind it to any\nPrintWriter.  For example, it could be a file opened with\nclojure.java.io/writer.\n\n\nEXTENDING TEST-IS (ADVANCED)\n\nYou can extend the behavior of the \"is\" macro by defining new\nmethods for the \"assert-expr\" multimethod.  These methods are\ncalled during expansion of the \"is\" macro, so they should return\nquoted forms to be evaluated.\n\nYou can plug in your own test-reporting framework by rebinding\nthe \"report\" function: (report event)\n\nThe 'event' argument is a map.  It will always have a :type key,\nwhose value will be a keyword signaling the type of event being\nreported.  Standard events with :type value of :pass, :fail, and\n:error are called when an assertion passes, fails, and throws an\nexception, respectively.  In that case, the event will also have\nthe following keys:\n\n  :expected   The form that was expected to be true\n  :actual     A form representing what actually occurred\n  :message    The string message given as an argument to 'is'\n\nThe \"testing\" strings will be a list in \"*testing-contexts*\", and\nthe vars being tested will be a list in \"*testing-vars*\".\n\nYour \"report\" function should wrap any printing calls in the\n\"with-test-out\" macro, which rebinds *out* to the current value\nof *test-out*.\n\nThe event types are:\n:default :pass :fail :error :summary :begin-test-ns :end-test-ns\n:begin-test-var :end-test-var",
   :author
   "Stuart Sierra, with contributions and suggestions by \n  Chas Emerick, Allen Rohner, and Stuart Halloway",
   :name "clojure.test",
   :wiki-url "https://clojure.github.io/clojure/clojure.test-api.html",
   :source-url
   "https://github.com/clojure/clojure/blob/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj"}
  {:doc
   "Functions for invoking Java processes and invoking tools via the Clojure CLI.",
   :name "clojure.tools.deps.interop",
   :wiki-url
   "https://clojure.github.io/clojure/clojure.tools.deps.interop-api.html",
   :source-url
   "https://github.com/clojure/clojure/blob/d1de868e66a8d46cae164202a0e9ca9e670df204/src/clj/clojure/tools/deps/interop.clj"}
  {:doc
   "This file defines a generic tree walker for Clojure data\nstructures.  It takes any data structure (list, vector, map, set,\nseq), calls a function on every element, and uses the return value\nof the function in place of the original.  This makes it fairly\neasy to write recursive search-and-replace functions, as shown in\nthe examples.\n\nNote: \"walk\" supports all Clojure data structures EXCEPT maps\ncreated with sorted-map-by.  There is no (obvious) way to retrieve\nthe sorting function.",
   :author "Stuart Sierra",
   :name "clojure.walk",
   :wiki-url "https://clojure.github.io/clojure/clojure.walk-api.html",
   :source-url
   "https://github.com/clojure/clojure/blob/4dbdcb1fe8128620162839a60192d899eb3c83f7/src/clj/clojure/walk.clj"}
  {:doc "XML reading/writing.",
   :author "Rich Hickey",
   :name "clojure.xml",
   :wiki-url "https://clojure.github.io/clojure/clojure.xml-api.html",
   :source-url
   "https://github.com/clojure/clojure/blob/4a4a6e7717d411679820c4a3ce735a77aef45cc3/src/clj/clojure/xml.clj"}
  {:doc
   "Functional hierarchical zipper, with navigation, editing,\nand enumeration.  See Huet",
   :author "Rich Hickey",
   :name "clojure.zip",
   :wiki-url "https://clojure.github.io/clojure/clojure.zip-api.html",
   :source-url
   "https://github.com/clojure/clojure/blob/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj"}
  {:doc nil,
   :name "clojure.core.protocols",
   :wiki-url
   "https://clojure.github.io/clojure/clojure.core-api.html#clojure.core.protocols",
   :source-url
   "https://github.com/clojure/clojure/blob/619b576022cdd5fae899a8418bc568ab1dac3472/src/clj/clojure/core/protocols.clj"}
  {:doc
   "A library for reduction and parallel folding. Alpha and subject\nto change.",
   :author "Rich Hickey",
   :name "clojure.core.reducers",
   :wiki-url
   "https://clojure.github.io/clojure/clojure.core-api.html#clojure.core.reducers",
   :source-url
   "https://github.com/clojure/clojure/blob/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj"}
  {:doc "Socket server support",
   :author "Alex Miller",
   :name "clojure.core.server",
   :wiki-url
   "https://clojure.github.io/clojure/clojure.core-api.html#clojure.core.server",
   :source-url
   "https://github.com/clojure/clojure/blob/3eab1a5a5b4cfed86c1d0c9c21bd1892e1fb44fd/src/clj/clojure/core/server.clj"}
  {:doc nil,
   :name "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure/clojure.core-api.html#clojure.core.specs.alpha",
   :source-url nil}
  {:doc nil,
   :name "clojure.java.basis.impl",
   :wiki-url
   "https://clojure.github.io/clojure/clojure.java.basis-api.html#clojure.java.basis.impl",
   :source-url
   "https://github.com/clojure/clojure/blob/b7d87dcd729628a2e80038d05023eb7c7786bb11/src/clj/clojure/java/basis/impl.clj"}
  {:doc
   "clojure.repl.deps provides facilities for dynamically modifying the available\nlibraries in the runtime when running at the REPL, without restarting",
   :name "clojure.repl.deps",
   :wiki-url
   "https://clojure.github.io/clojure/clojure.repl-api.html#clojure.repl.deps",
   :source-url
   "https://github.com/clojure/clojure/blob/be32516234961396581685b485ed8a88f8fe17e6/src/clj/clojure/repl/deps.clj"}
  {:doc
   "clojure.test extension for JUnit-compatible XML output.\n\nJUnit (http://junit.org/) is the most popular unit-testing library\nfor Java.  As such, tool support for JUnit output formats is\ncommon.  By producing compatible output from tests, this tool\nsupport can be exploited.\n\nTo use, wrap any calls to clojure.test/run-tests in the\nwith-junit-output macro, like this:\n\n  (use 'clojure.test)\n  (use 'clojure.test.junit)\n\n  (with-junit-output\n    (run-tests 'my.cool.library))\n\nTo write the output to a file, rebind clojure.test/*test-out* to\nyour own PrintWriter (perhaps opened using\nclojure.java.io/writer).",
   :author "Jason Sankey",
   :name "clojure.test.junit",
   :wiki-url
   "https://clojure.github.io/clojure/clojure.test-api.html#clojure.test.junit",
   :source-url
   "https://github.com/clojure/clojure/blob/d9f3f83182e146525a78cf638f0613487d7e18c6/src/clj/clojure/test/junit.clj"}
  {:doc
   "clojure.test extensions for the Test Anything Protocol (TAP)\n\nTAP is a simple text-based syntax for reporting test results.  TAP\nwas originally developed for Perl, and now has implementations in\nseveral languages.  For more information on TAP, see\nhttp://testanything.org/ and\nhttp://search.cpan.org/~petdance/TAP-1.0.0/TAP.pm\n\nTo use this library, wrap any calls to\nclojure.test/run-tests in the with-tap-output macro,\nlike this:\n\n  (use 'clojure.test)\n  (use 'clojure.test.tap)\n\n  (with-tap-output\n   (run-tests 'my.cool.library))",
   :author "Stuart Sierra",
   :name "clojure.test.tap",
   :wiki-url
   "https://clojure.github.io/clojure/clojure.test-api.html#clojure.test.tap",
   :source-url
   "https://github.com/clojure/clojure/blob/153a2d0f000ab2f254704e4970968fee6a0329a1/src/clj/clojure/test/tap.clj"}),
 :vars
 ({:name "&",
   :doc
   "Syntax for use with fn.\n\nPlease see https://clojure.org/reference/special_forms#fn",
   :var-type "special syntax",
   :added "1.0",
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/&",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "*",
   :doc
   "Returns the product of nums. (*) returns 1. Does not auto-promote\nlongs, will throw on overflow. See also: *'",
   :var-type "function",
   :line 1010,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([] [x] [x y] [x y & more]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1010",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "*'",
   :doc
   "Returns the product of nums. (*') returns 1. Supports arbitrary precision.\nSee also: *",
   :var-type "function",
   :line 998,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([] [x] [x y] [x y & more]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*'",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L998",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "*1",
   :doc "bound in a repl thread to the most recent value printed",
   :var-type "var",
   :line 6428,
   :added "1.0",
   :dynamic true,
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*1",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6428",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "*2",
   :doc
   "bound in a repl thread to the second most recent value printed",
   :var-type "var",
   :line 6433,
   :added "1.0",
   :dynamic true,
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*2",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6433",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "*3",
   :doc
   "bound in a repl thread to the third most recent value printed",
   :var-type "var",
   :line 6438,
   :added "1.0",
   :dynamic true,
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*3",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6438",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "*agent*",
   :doc
   "The agent currently running an action on this thread, else nil",
   :var-type "var",
   :added "1.0",
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*agent*",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "*assert*",
   :doc
   "When set to logical false, 'assert' will omit assertion checks in\ncompiled code. Defaults to true.",
   :var-type "var",
   :added "1.0",
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*assert*",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "*clojure-version*",
   :doc
   "The version info for Clojure core, as a map containing :major :minor \n:incremental and :qualifier keys. Feature releases may increment \n:minor and/or :major, bugfix releases will increment :incremental. \nPossible values of :qualifier include \"GA\", \"SNAPSHOT\", \"RC-x\" \"BETA-x\"",
   :var-type "var",
   :line 7299,
   :added "1.0",
   :dynamic true,
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*clojure-version*",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7299",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "*command-line-args*",
   :doc
   "A sequence of the supplied command line arguments, or nil if\nnone were supplied",
   :var-type "var",
   :added "1.0",
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*command-line-args*",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "*compile-files*",
   :doc "Set to true when compiling files, false otherwise.",
   :var-type "var",
   :added "1.0",
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*compile-files*",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "*compile-path*",
   :doc
   "Specifies the directory where 'compile' will write out .class\nfiles. This directory must be in the classpath for 'compile' to\nwork.\n\nDefaults to \"classes\"",
   :var-type "var",
   :added "1.0",
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*compile-path*",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "*compiler-options*",
   :doc
   "A map of keys to options.\nNote, when binding dynamically make sure to merge with previous value.\nSupported options:\n:elide-meta - a collection of metadata keys to elide during compilation.\n:disable-locals-clearing - set to true to disable clearing, useful for using a debugger\n:direct-linking - set to true to use direct static invocation of functions, rather than vars\n  Note that call sites compiled with direct linking will not be affected by var redefinition.\n  Use ^:redef (or ^:dynamic) on a var to prevent direct linking and allow redefinition.\nSee https://clojure.org/reference/compilation for more information.",
   :var-type "var",
   :added "1.4",
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*compiler-options*",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "*data-readers*",
   :doc
   "Map from reader tag symbols to data reader Vars.\n\nWhen Clojure starts, it searches for files named 'data_readers.clj'\nand 'data_readers.cljc' at the root of the classpath. Each such file\nmust contain a literal map of symbols, like this:\n\n    {foo/bar my.project.foo/bar\n     foo/baz my.project/baz}\n\nThe first symbol in each pair is a tag that will be recognized by\nthe Clojure reader. The second symbol in the pair is the\nfully-qualified name of a Var which will be invoked by the reader to\nparse the form following the tag. For example, given the\ndata_readers.clj file above, the Clojure reader would parse this\nform:\n\n    #foo/bar [1 2 3]\n\nby invoking the Var #'my.project.foo/bar on the vector [1 2 3]. The\ndata reader function is invoked on the form AFTER it has been read\nas a normal Clojure data structure by the reader.\n\nReader tags without namespace qualifiers are reserved for\nClojure. Default reader tags are defined in\nclojure.core/default-data-readers but may be overridden in\ndata_readers.clj, data_readers.cljc, or by rebinding this Var.",
   :var-type "var",
   :line 8078,
   :added "1.4",
   :dynamic true,
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*data-readers*",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L8078",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "*default-data-reader-fn*",
   :doc
   "When no data reader is found for a tag and *default-data-reader-fn*\nis non-nil, it will be called with two arguments,\nthe tag and the value.  If *default-data-reader-fn* is nil (the\ndefault), an exception will be thrown for the unknown tag.",
   :var-type "var",
   :line 8107,
   :added "1.5",
   :dynamic true,
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*default-data-reader-fn*",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L8107",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "*e",
   :doc
   "bound in a repl thread to the most recent exception caught by the repl",
   :var-type "var",
   :line 6443,
   :added "1.0",
   :dynamic true,
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*e",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6443",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "*err*",
   :doc
   "A java.io.Writer object representing standard error for print operations.\n\nDefaults to System/err, wrapped in a PrintWriter",
   :var-type "var",
   :added "1.0",
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*err*",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "*file*",
   :doc
   "The path of the file being evaluated, as a String.\n\nWhen there is no file, e.g. in the REPL, the value is not defined.",
   :var-type "var",
   :added "1.0",
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*file*",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "*flush-on-newline*",
   :doc
   "When set to true, output will be flushed whenever a newline is printed.\n\nDefaults to true.",
   :var-type "var",
   :added "1.0",
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*flush-on-newline*",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "*in*",
   :doc
   "A java.io.Reader object representing standard input for read operations.\n\nDefaults to System/in, wrapped in a LineNumberingPushbackReader",
   :var-type "var",
   :added "1.0",
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*in*",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "*ns*",
   :doc
   "A clojure.lang.Namespace object representing the current namespace.",
   :var-type "var",
   :added "1.0",
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*ns*",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "*out*",
   :doc
   "A java.io.Writer object representing standard output for print operations.\n\nDefaults to System/out, wrapped in an OutputStreamWriter",
   :var-type "var",
   :added "1.0",
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*out*",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "*print-dup*",
   :doc
   "When set to logical true, objects will be printed in a way that preserves\ntheir type when read in later.\n\nDefaults to false.",
   :var-type "var",
   :added "1.0",
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*print-dup*",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "*print-length*",
   :doc
   "*print-length* controls how many items of each collection the\nprinter will print. If it is bound to logical false, there is no\nlimit. Otherwise, it must be bound to an integer indicating the maximum\nnumber of items of each collection to print. If a collection contains\nmore items, the printer will print items up to the limit followed by\n'...' to represent the remaining items. The root binding is nil\nindicating no limit.",
   :var-type "var",
   :line 16,
   :added "1.0",
   :dynamic true,
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*print-length*",
   :source-url
   "https://github.com/clojure/clojure/blob/e6e7ca1fe859aeb74530ab18654815184e00f1f5/src/clj/clojure/core_print.clj#L16",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/e6e7ca1fe859aeb74530ab18654815184e00f1f5/src/clj/clojure/core_print.clj",
   :file "src/clj/clojure/core_print.clj"}
  {:name "*print-level*",
   :doc
   "*print-level* controls how many levels deep the printer will\nprint nested objects. If it is bound to logical false, there is no\nlimit. Otherwise, it must be bound to an integer indicating the maximum\nlevel to print. Each argument to print is at level 0; if an argument is a\ncollection, its items are at level 1; and so on. If an object is a\ncollection and is at a level greater than or equal to the value bound to\n*print-level*, the printer prints '#' to represent it. The root binding\nis nil indicating no limit.",
   :var-type "var",
   :line 27,
   :added "1.0",
   :dynamic true,
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*print-level*",
   :source-url
   "https://github.com/clojure/clojure/blob/e6e7ca1fe859aeb74530ab18654815184e00f1f5/src/clj/clojure/core_print.clj#L27",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/e6e7ca1fe859aeb74530ab18654815184e00f1f5/src/clj/clojure/core_print.clj",
   :file "src/clj/clojure/core_print.clj"}
  {:name "*print-meta*",
   :doc
   "If set to logical true, when printing an object, its metadata will also\nbe printed in a form that can be read back by the reader.\n\nDefaults to false.",
   :var-type "var",
   :added "1.0",
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*print-meta*",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "*print-namespace-maps*",
   :doc
   "*print-namespace-maps* controls whether the printer will print\nnamespace map literal syntax. It defaults to false, but the REPL binds\nto true.",
   :var-type "var",
   :line 41,
   :added "1.9",
   :dynamic true,
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*print-namespace-maps*",
   :source-url
   "https://github.com/clojure/clojure/blob/e6e7ca1fe859aeb74530ab18654815184e00f1f5/src/clj/clojure/core_print.clj#L41",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/e6e7ca1fe859aeb74530ab18654815184e00f1f5/src/clj/clojure/core_print.clj",
   :file "src/clj/clojure/core_print.clj"}
  {:name "*print-readably*",
   :doc
   "When set to logical false, strings and characters will be printed with\nnon-alphanumeric characters converted to the appropriate escape sequences.\n\nDefaults to true",
   :var-type "var",
   :added "1.0",
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*print-readably*",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "*read-eval*",
   :doc
   "Defaults to true (or value specified by system property, see below)\n***This setting implies that the full power of the reader is in play,\nincluding syntax that can cause code to execute. It should never be\nused with untrusted sources. See also: clojure.edn/read.***\n\nWhen set to logical false in the thread-local binding,\nthe eval reader (#=) and record/type literal syntax are disabled in read/load.\nExample (will fail): (binding [*read-eval* false] (read-string \"#=(* 2 21)\"))\n\nThe default binding can be controlled by the system property\n'clojure.read.eval' System properties can be set on the command line\nlike this:\n\njava -Dclojure.read.eval=false ...\n\nThe system property can also be set to 'unknown' via\n-Dclojure.read.eval=unknown, in which case the default binding\nis :unknown and all reads will fail in contexts where *read-eval*\nhas not been explicitly bound to either true or false. This setting\ncan be a useful diagnostic tool to ensure that all of your reads\noccur in considered contexts. You can also accomplish this in a\nparticular scope by binding *read-eval* to :unknown\n",
   :var-type "var",
   :added "1.0",
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*read-eval*",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "*repl*",
   :doc "Bound to true in a repl thread",
   :var-type "var",
   :line 6448,
   :added "1.12",
   :dynamic true,
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*repl*",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6448",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "*unchecked-math*",
   :doc
   "While bound to true, compilations of +, -, *, inc, dec and the\ncoercions will be done without overflow checks. While bound\nto :warn-on-boxed, same behavior as true, and a warning is emitted\nwhen compilation uses boxed math. Default: false.",
   :var-type "var",
   :added "1.3",
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*unchecked-math*",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "*warn-on-reflection*",
   :doc
   "When set to true, the compiler will emit warnings when reflection is\nneeded to resolve Java method calls or field accesses.\n\nDefaults to false.",
   :var-type "var",
   :added "1.0",
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*warn-on-reflection*",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "+",
   :doc
   "Returns the sum of nums. (+) returns 0. Does not auto-promote\nlongs, will throw on overflow. See also: +'",
   :var-type "function",
   :line 986,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([] [x] [x y] [x y & more]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/+",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L986",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "+'",
   :doc
   "Returns the sum of nums. (+') returns 0. Supports arbitrary precision.\nSee also: +",
   :var-type "function",
   :line 974,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([] [x] [x y] [x y & more]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/+'",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L974",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "-",
   :doc
   "If no ys are supplied, returns the negation of x, else subtracts\nthe ys from x and returns the result. Does not auto-promote\nlongs, will throw on overflow. See also: -'",
   :var-type "function",
   :line 1045,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([x] [x y] [x y & more]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/-",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1045",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "-'",
   :doc
   "If no ys are supplied, returns the negation of x, else subtracts\nthe ys from x and returns the result. Supports arbitrary precision.\nSee also: -",
   :var-type "function",
   :line 1033,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x] [x y] [x y & more]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/-'",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1033",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "->",
   :doc
   "Threads the expr through the forms. Inserts x as the\nsecond item in the first form, making a list of it if it is not a\nlist already. If there are more forms, inserts the first form as the\nsecond item in second form, etc.",
   :var-type "macro",
   :line 1691,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x & forms]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/->",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1691",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "->>",
   :doc
   "Threads the expr through the forms. Inserts x as the\nlast item in the first form, making a list of it if it is not a\nlist already. If there are more forms, inserts the first form as the\nlast item in second form, etc.",
   :var-type "macro",
   :line 1707,
   :added "1.1",
   :namespace "clojure.core",
   :arglists ([x & forms]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/->>",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1707",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "->ArrayChunk",
   :doc
   "Positional factory function for class clojure.core.ArrayChunk.",
   :var-type "function",
   :line 37,
   :namespace "clojure.core",
   :arglists ([am arr off end]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/->ArrayChunk",
   :source-url
   "https://github.com/clojure/clojure/blob/e0efe77eb39f3868ba02c5951f036a48262e80ff/src/clj/clojure/gvec.clj#L37",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/e0efe77eb39f3868ba02c5951f036a48262e80ff/src/clj/clojure/gvec.clj",
   :file "src/clj/clojure/gvec.clj"}
  {:name "->Eduction",
   :doc "Positional factory function for class clojure.core.Eduction.",
   :var-type "function",
   :line 7956,
   :namespace "clojure.core",
   :arglists ([xform coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/->Eduction",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7956",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "->Vec",
   :doc "Positional factory function for class clojure.core.Vec.",
   :var-type "function",
   :line 170,
   :namespace "clojure.core",
   :arglists ([am cnt shift root tail _meta]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/->Vec",
   :source-url
   "https://github.com/clojure/clojure/blob/e0efe77eb39f3868ba02c5951f036a48262e80ff/src/clj/clojure/gvec.clj#L170",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/e0efe77eb39f3868ba02c5951f036a48262e80ff/src/clj/clojure/gvec.clj",
   :file "src/clj/clojure/gvec.clj"}
  {:name "->VecNode",
   :doc "Positional factory function for class clojure.core.VecNode.",
   :var-type "function",
   :line 18,
   :namespace "clojure.core",
   :arglists ([edit arr]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/->VecNode",
   :source-url
   "https://github.com/clojure/clojure/blob/e0efe77eb39f3868ba02c5951f036a48262e80ff/src/clj/clojure/gvec.clj#L18",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/e0efe77eb39f3868ba02c5951f036a48262e80ff/src/clj/clojure/gvec.clj",
   :file "src/clj/clojure/gvec.clj"}
  {:name "->VecSeq",
   :doc "Positional factory function for class clojure.core.VecSeq.",
   :var-type "function",
   :line 59,
   :namespace "clojure.core",
   :arglists ([am vec anode i offset _meta]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/->VecSeq",
   :source-url
   "https://github.com/clojure/clojure/blob/e0efe77eb39f3868ba02c5951f036a48262e80ff/src/clj/clojure/gvec.clj#L59",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/e0efe77eb39f3868ba02c5951f036a48262e80ff/src/clj/clojure/gvec.clj",
   :file "src/clj/clojure/gvec.clj"}
  {:name ".",
   :doc
   "The instance member form works for both fields and methods.\nThey all expand into calls to the dot operator at macroexpansion time.\n\nPlease see https://clojure.org/java_interop#dot",
   :var-type "special form",
   :added "1.0",
   :forms
   [(.instanceMember instance args*)
    (.instanceMember Classname args*)
    (Classname/staticMethod args*)
    Classname/staticField],
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/.",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "..",
   :doc
   "form => fieldName-symbol or (instanceMethodName-symbol args*)\n\nExpands into a member access (.) of the first member on the first\nargument, followed by the next member on the result, etc. For\ninstance:\n\n(.. System (getProperties) (get \"os.name\"))\n\nexpands to:\n\n(. (. System (getProperties)) (get \"os.name\"))\n\nbut is easier to write, read, and understand.",
   :var-type "macro",
   :line 1673,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x form] [x form & more]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/..",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1673",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "/",
   :doc
   "If no denominators are supplied, returns 1/numerator,\nelse returns numerator divided by all of the denominators.",
   :var-type "function",
   :line 1022,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x] [x y] [x y & more]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core//",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1022",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "<",
   :doc
   "Returns non-nil if nums are in monotonically increasing order,\notherwise false.",
   :var-type "function",
   :line 902,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x] [x y] [x y & more]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/<",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L902",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "<=",
   :doc
   "Returns non-nil if nums are in monotonically non-decreasing order,\notherwise false.",
   :var-type "function",
   :line 1057,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x] [x y] [x y & more]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/<=",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1057",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "=",
   :doc
   "Equality. Returns true if x equals y, false if not. Same as\nJava x.equals(y) except it also works for nil, and compares\nnumbers and collections in a type-independent manner.  Clojure's immutable data\nstructures define equals() (and thus =) as a value, not an identity,\ncomparison.",
   :var-type "function",
   :line 785,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x] [x y] [x y & more]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/=",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L785",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "==",
   :doc
   "Returns non-nil if nums all have the equivalent\nvalue (type-independent), otherwise false",
   :var-type "function",
   :line 1102,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x] [x y] [x y & more]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/==",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1102",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name ">",
   :doc
   "Returns non-nil if nums are in monotonically decreasing order,\notherwise false.",
   :var-type "function",
   :line 1072,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x] [x y] [x y & more]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/>",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1072",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name ">=",
   :doc
   "Returns non-nil if nums are in monotonically non-increasing order,\notherwise false.",
   :var-type "function",
   :line 1087,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x] [x y] [x y & more]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/>=",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1087",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "NaN?",
   :doc "Returns true if num is NaN, else false",
   :var-type "function",
   :line 8296,
   :added "1.11",
   :namespace "clojure.core",
   :arglists ([num]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/NaN?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L8296",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "PrintWriter-on",
   :doc
   "implements java.io.PrintWriter given flush-fn, which will be called\nwhen .flush() is called, with a string built up since the last call to .flush().\nif not nil, close-fn will be called with no arguments when .close is called.\nautoflush? determines if the PrintWriter will autoflush, false by default.",
   :var-type "function",
   :line 563,
   :added "1.10",
   :namespace "clojure.core",
   :arglists ([flush-fn close-fn] [flush-fn close-fn autoflush?]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/PrintWriter-on",
   :source-url
   "https://github.com/clojure/clojure/blob/e6e7ca1fe859aeb74530ab18654815184e00f1f5/src/clj/clojure/core_print.clj#L563",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/e6e7ca1fe859aeb74530ab18654815184e00f1f5/src/clj/clojure/core_print.clj",
   :file "src/clj/clojure/core_print.clj"}
  {:name "StackTraceElement->vec",
   :doc
   "Constructs a data representation for a StackTraceElement: [class method file line]",
   :var-type "function",
   :line 467,
   :added "1.9",
   :namespace "clojure.core",
   :arglists ([o]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/StackTraceElement->vec",
   :source-url
   "https://github.com/clojure/clojure/blob/e6e7ca1fe859aeb74530ab18654815184e00f1f5/src/clj/clojure/core_print.clj#L467",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/e6e7ca1fe859aeb74530ab18654815184e00f1f5/src/clj/clojure/core_print.clj",
   :file "src/clj/clojure/core_print.clj"}
  {:name "Throwable->map",
   :doc
   "Constructs a data representation for a Throwable with keys:\n:cause - root cause message\n:phase - error phase\n:via - cause chain, with cause keys:\n         :type - exception class symbol\n         :message - exception message\n         :data - ex-data\n         :at - top stack element\n:trace - root cause stack elements",
   :var-type "function",
   :line 473,
   :added "1.7",
   :namespace "clojure.core",
   :arglists ([o]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/Throwable->map",
   :source-url
   "https://github.com/clojure/clojure/blob/e6e7ca1fe859aeb74530ab18654815184e00f1f5/src/clj/clojure/core_print.clj#L473",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/e6e7ca1fe859aeb74530ab18654815184e00f1f5/src/clj/clojure/core_print.clj",
   :file "src/clj/clojure/core_print.clj"}
  {:name "abs",
   :doc
   "Returns the absolute value of a.\nIf a is Long/MIN_VALUE => Long/MIN_VALUE\nIf a is a double and zero => +0.0\nIf a is a double and ##Inf or ##-Inf => ##Inf\nIf a is a double and ##NaN => ##NaN",
   :var-type "function",
   :line 1137,
   :added "1.11",
   :namespace "clojure.core",
   :arglists ([a]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/abs",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1137",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "accessor",
   :doc
   "Returns a fn that, given an instance of a structmap with the basis,\nreturns the value at the key.  The key must be in the basis. The\nreturned function should be (slightly) more efficient than using\nget, but such use of accessors should be limited to known\nperformance-critical areas.",
   :var-type "function",
   :line 4094,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([s key]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/accessor",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4094",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "aclone",
   :doc
   "Returns a clone of the Java array. Works on arrays of known\ntypes.",
   :var-type "function",
   :line 3928,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([array]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/aclone",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3928",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "add-classpath",
   :doc
   "DEPRECATED \n\nAdds the url (String or URL object) to the classpath per\nURLClassLoader.addURL",
   :var-type "function",
   :line 5288,
   :added "1.0",
   :deprecated "1.1",
   :namespace "clojure.core",
   :arglists ([url]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/add-classpath",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5288",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "add-tap",
   :doc
   "adds f, a fn of one argument, to the tap set. This function will be called with anything sent via tap>.\nThis function may (briefly) block (e.g. for streams), and will never impede calls to tap>,\nbut blocking indefinitely may cause tap values to be dropped.\nRemember f in order to remove-tap",
   :var-type "function",
   :line 8188,
   :added "1.10",
   :namespace "clojure.core",
   :arglists ([f]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/add-tap",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L8188",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "add-watch",
   :doc
   "Adds a watch function to an agent/atom/var/ref reference. The watch\nfn must be a fn of 4 args: a key, the reference, its old-state, its\nnew-state. Whenever the reference's state might have been changed,\nany registered watches will have their functions called. The watch fn\nwill be called synchronously, on the agent's thread if an agent,\nbefore any pending sends if agent or ref. Note that an atom's or\nref's state may have changed again prior to the fn call, so use\nold/new-state rather than derefing the reference. Note also that watch\nfns may be called from multiple threads simultaneously. Var watchers\nare triggered only by root binding changes, not thread-local\nset!s. Keys must be unique per reference, and can be used to remove\nthe watch with remove-watch, but are otherwise considered opaque by\nthe watch mechanism.",
   :var-type "function",
   :line 2158,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([reference key fn]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/add-watch",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2158",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "agent",
   :doc
   "Creates and returns an agent with an initial value of state and\nzero or more options (in any order):\n\n:meta metadata-map\n\n:validator validate-fn\n\n:error-handler handler-fn\n\n:error-mode mode-keyword\n\nIf metadata-map is supplied, it will become the metadata on the\nagent. validate-fn must be nil or a side-effect-free fn of one\nargument, which will be passed the intended new state on any state\nchange. If the new state is unacceptable, the validate-fn should\nreturn false or throw an exception.  handler-fn is called if an\naction throws an exception or if validate-fn rejects a new state --\nsee set-error-handler! for details.  The mode-keyword may be either\n:continue (the default if an error-handler is given) or :fail (the\ndefault if no error-handler is given) -- see set-error-mode! for\ndetails.",
   :var-type "function",
   :line 2068,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([state & options]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/agent",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2068",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "agent-error",
   :doc
   "Returns the exception thrown during an asynchronous action of the\nagent if the agent is failed.  Returns nil if the agent is not\nfailed.",
   :var-type "function",
   :line 2183,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([a]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/agent-error",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2183",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "agent-errors",
   :doc
   "DEPRECATED: Use 'agent-error' instead.\nReturns a sequence of the exceptions thrown during asynchronous\nactions of the agent.",
   :var-type "function",
   :line 2250,
   :added "1.0",
   :deprecated "1.2",
   :namespace "clojure.core",
   :arglists ([a]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/agent-errors",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2250",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "aget",
   :doc
   "Returns the value at the index/indices. Works on Java arrays of all\ntypes.",
   :var-type "function",
   :line 3935,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([array idx] [array idx & idxs]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/aget",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3935",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "alength",
   :doc
   "Returns the length of the Java array. Works on arrays of all\ntypes.",
   :var-type "function",
   :line 3921,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([array]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/alength",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3921",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "alias",
   :doc
   "Add an alias in the current namespace to another\nnamespace. Arguments are two symbols: the alias to be used, and\nthe symbolic name of the target namespace. Use :as in the ns macro in preference\nto calling this directly.",
   :var-type "function",
   :line 4287,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([alias namespace-sym]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/alias",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4287",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "all-ns",
   :doc "Returns a sequence of all namespaces.",
   :var-type "function",
   :line 4170,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/all-ns",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4170",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "alter",
   :doc
   "Must be called in a transaction. Sets the in-transaction-value of\nref to:\n\n(apply fun in-transaction-value-of-ref args)\n\nand returns the in-transaction-value of ref.",
   :var-type "function",
   :line 2457,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([ref fun & args]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/alter",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2457",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "alter-meta!",
   :doc
   "Atomically sets the metadata for a namespace/var/ref/agent/atom to be:\n\n(apply f its-current-meta args)\n\nf must be free of side-effects",
   :var-type "function",
   :line 2420,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([iref f & args]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/alter-meta!",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2420",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "alter-var-root",
   :doc
   "Atomically alters the root binding of var v by applying f to its\ncurrent value plus any args",
   :var-type "function",
   :line 5641,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([v f & args]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/alter-var-root",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5641",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "amap",
   :doc
   "Maps an expression across an array a, using an index named idx, and\nreturn value named ret, initialized to a clone of a, then setting \neach element of ret to the evaluation of expr, returning the new \narray ret.",
   :var-type "macro",
   :line 5385,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([a idx ret expr]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/amap",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5385",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "ancestors",
   :doc
   "Returns the immediate and indirect parents of tag, either via a Java type\ninheritance relationship or a relationship established via derive. h\nmust be a hierarchy obtained from make-hierarchy, if not supplied\ndefaults to the global hierarchy",
   :var-type "function",
   :line 5734,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([tag] [h tag]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ancestors",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5734",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "and",
   :doc
   "Evaluates exprs one at a time, from left to right. If a form\nreturns logical false (nil or false), and returns that value and\ndoesn't evaluate any of the other expressions, otherwise it returns\nthe value of the last expr. (and) returns true.",
   :var-type "macro",
   :line 844,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([] [x] [x & next]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/and",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L844",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "any?",
   :doc "Returns true given any argument.",
   :var-type "function",
   :line 540,
   :added "1.9",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/any?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L540",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "apply",
   :doc
   "Applies fn f to the argument list formed by prepending intervening arguments to args.",
   :var-type "function",
   :line 662,
   :added "1.0",
   :namespace "clojure.core",
   :arglists
   ([f args]
    [f x args]
    [f x y args]
    [f x y z args]
    [f a b c d & args]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/apply",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L662",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "areduce",
   :doc
   "Reduces an expression across an array a, using an index named idx,\nand return value named ret, initialized to init, setting ret to the \nevaluation of expr at each step, returning ret.",
   :var-type "macro",
   :line 5401,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([a idx ret init expr]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/areduce",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5401",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "array-map",
   :doc
   "Constructs an array-map. If any keys are equal, they are handled as\nif by repeated uses of assoc.",
   :var-type "function",
   :line 4402,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([] [& keyvals]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/array-map",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4402",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "as->",
   :doc
   "Binds name to expr, evaluates the first form in the lexical context\nof that binding, then binds name to that result, repeating for each\nsuccessive form, returning the result of the last form.",
   :var-type "macro",
   :line 7846,
   :added "1.5",
   :namespace "clojure.core",
   :arglists ([expr name & forms]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/as->",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7846",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "aset",
   :doc
   "Sets the value at the index/indices. Works on Java arrays of\nreference types. Returns val.",
   :var-type "function",
   :line 3946,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([array idx val] [array idx idx2 & idxv]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/aset",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3946",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "aset-boolean",
   :doc
   "Sets the value at the index/indices. Works on arrays of boolean. Returns val.",
   :var-type "function",
   :line 3979,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([array idx val] [array idx idx2 & idxv]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/aset-boolean",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3979",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "aset-byte",
   :doc
   "Sets the value at the index/indices. Works on arrays of byte. Returns val.",
   :var-type "function",
   :line 3999,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([array idx val] [array idx idx2 & idxv]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/aset-byte",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3999",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "aset-char",
   :doc
   "Sets the value at the index/indices. Works on arrays of char. Returns val.",
   :var-type "function",
   :line 4004,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([array idx val] [array idx idx2 & idxv]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/aset-char",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4004",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "aset-double",
   :doc
   "Sets the value at the index/indices. Works on arrays of double. Returns val.",
   :var-type "function",
   :line 3989,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([array idx val] [array idx idx2 & idxv]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/aset-double",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3989",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "aset-float",
   :doc
   "Sets the value at the index/indices. Works on arrays of float. Returns val.",
   :var-type "function",
   :line 3984,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([array idx val] [array idx idx2 & idxv]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/aset-float",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3984",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "aset-int",
   :doc
   "Sets the value at the index/indices. Works on arrays of int. Returns val.",
   :var-type "function",
   :line 3969,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([array idx val] [array idx idx2 & idxv]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/aset-int",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3969",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "aset-long",
   :doc
   "Sets the value at the index/indices. Works on arrays of long. Returns val.",
   :var-type "function",
   :line 3974,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([array idx val] [array idx idx2 & idxv]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/aset-long",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3974",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "aset-short",
   :doc
   "Sets the value at the index/indices. Works on arrays of short. Returns val.",
   :var-type "function",
   :line 3994,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([array idx val] [array idx idx2 & idxv]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/aset-short",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3994",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "assert",
   :doc
   "Evaluates expression x and throws an AssertionError with optional\nmessage if x does not evaluate to logical true.\n\nAssertion checks are omitted from compiled code if '*assert*' is\nfalse.",
   :var-type "macro",
   :line 4958,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x] [x message]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/assert",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4958",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "assoc",
   :doc
   "assoc[iate]. When applied to a map, returns a new map of the\nsame (hashed/sorted) type, that contains the mapping of key(s) to\nval(s). When applied to a vector, returns a new vector that\ncontains val at index. Note - index must be <= (count vector).",
   :var-type "function",
   :line 183,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([map key val] [map key val & kvs]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/assoc",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L183",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "assoc!",
   :doc
   "When applied to a transient map, adds mapping of key(s) to\nval(s). When applied to a transient vector, sets the val at index.\nNote - index must be <= (count vector). Returns coll.",
   :var-type "function",
   :line 3391,
   :added "1.1",
   :namespace "clojure.core",
   :arglists ([coll key val] [coll key val & kvs]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/assoc!",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3391",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "assoc-in",
   :doc
   "Associates a value in a nested associative structure, where ks is a\nsequence of keys and v is the new value and returns a new nested structure.\nIf any levels do not exist, hash-maps will be created.",
   :var-type "function",
   :line 6307,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([m [k & ks] v]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/assoc-in",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6307",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "associative?",
   :doc "Returns true if coll implements Associative",
   :var-type "function",
   :line 6383,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/associative?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6383",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "atom",
   :doc
   "Creates and returns an Atom with an initial value of x and zero or\nmore options (in any order):\n\n:meta metadata-map\n\n:validator validate-fn\n\nIf metadata-map is supplied, it will become the metadata on the\natom. validate-fn must be nil or a side-effect-free fn of one\nargument, which will be passed the intended new state on any state\nchange. If the new state is unacceptable, the validate-fn should\nreturn false or throw an exception.",
   :var-type "function",
   :line 2341,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x] [x & options]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/atom",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2341",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "await",
   :doc
   "Blocks the current thread (indefinitely!) until all actions\ndispatched thus far, from this thread or agent, to the agent(s) have\noccurred.  Will block on failed agents.  Will never return if\na failed agent is restarted with :clear-actions true or shutdown-agents was called.",
   :var-type "function",
   :line 3289,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([& agents]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/await",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3289",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "await-for",
   :doc
   "Blocks the current thread until all actions dispatched thus\nfar (from this thread or agent) to the agents have occurred, or the\ntimeout (in milliseconds) has elapsed. Returns logical false if\nreturning due to timeout, logical true otherwise.",
   :var-type "function",
   :line 3311,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([timeout-ms & agents]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/await-for",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3311",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "bases",
   :doc
   "Returns the immediate superclass and direct interfaces of c, if any",
   :var-type "function",
   :line 5679,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([c]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bases",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5679",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "bean",
   :doc
   "Takes a Java object and returns a read-only implementation of the\nmap abstraction based upon its JavaBean properties.",
   :var-type "function",
   :line 403,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bean",
   :source-url
   "https://github.com/clojure/clojure/blob/658693f6cf97e6ab0ff789e096c9eb6654e4d3ab/src/clj/clojure/core_proxy.clj#L403",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/658693f6cf97e6ab0ff789e096c9eb6654e4d3ab/src/clj/clojure/core_proxy.clj",
   :file "src/clj/clojure/core_proxy.clj"}
  {:name "bigdec",
   :doc "Coerce to BigDecimal",
   :var-type "function",
   :line 3670,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bigdec",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3670",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "bigint",
   :doc "Coerce to BigInt",
   :var-type "function",
   :line 3642,
   :added "1.3",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bigint",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3642",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "biginteger",
   :doc "Coerce to BigInteger",
   :var-type "function",
   :line 3656,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/biginteger",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3656",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "binding",
   :doc
   "binding => var-symbol init-expr\n\nCreates new bindings for the (already-existing) vars, with the\nsupplied initial values, executes the exprs in an implicit do, then\nre-establishes the bindings that existed before.  The new bindings\nare made in parallel (unlike let); all init-exprs are evaluated\nbefore the vars are bound to their new values.",
   :var-type "macro",
   :line 1961,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([bindings & body]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/binding",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1961",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "bit-and",
   :doc "Bitwise and",
   :var-type "function",
   :line 1307,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x y] [x y & more]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bit-and",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1307",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "bit-and-not",
   :doc "Bitwise and with complement",
   :var-type "function",
   :line 1334,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x y] [x y & more]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bit-and-not",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1334",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "bit-clear",
   :doc "Clear bit at index n",
   :var-type "function",
   :line 1345,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x n]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bit-clear",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1345",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "bit-flip",
   :doc "Flip bit at index n",
   :var-type "function",
   :line 1357,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x n]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bit-flip",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1357",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "bit-not",
   :doc "Bitwise complement",
   :var-type "function",
   :line 1300,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bit-not",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1300",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "bit-or",
   :doc "Bitwise or",
   :var-type "function",
   :line 1316,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x y] [x y & more]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bit-or",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1316",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "bit-set",
   :doc "Set bit at index n",
   :var-type "function",
   :line 1351,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x n]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bit-set",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1351",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "bit-shift-left",
   :doc "Bitwise shift left",
   :var-type "function",
   :line 1370,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x n]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bit-shift-left",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1370",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "bit-shift-right",
   :doc "Bitwise shift right",
   :var-type "function",
   :line 1376,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x n]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bit-shift-right",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1376",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "bit-test",
   :doc "Test bit at index n",
   :var-type "function",
   :line 1363,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x n]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bit-test",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1363",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "bit-xor",
   :doc "Bitwise exclusive or",
   :var-type "function",
   :line 1325,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x y] [x y & more]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bit-xor",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1325",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "boolean",
   :doc "Coerce to boolean",
   :var-type "function",
   :line 1617,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/boolean",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1617",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "boolean-array",
   :doc "Creates an array of booleans",
   :var-type "function",
   :line 5421,
   :added "1.1",
   :namespace "clojure.core",
   :arglists ([size-or-seq] [size init-val-or-seq]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/boolean-array",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5421",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "boolean?",
   :doc "Return true if x is a Boolean",
   :var-type "function",
   :line 521,
   :added "1.9",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/boolean?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L521",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "booleans",
   :doc "Casts to boolean[]",
   :var-type "function",
   :line 5484,
   :added "1.1",
   :namespace "clojure.core",
   :arglists ([xs]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/booleans",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5484",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "bound-fn",
   :doc
   "Returns a function defined by the given fntail, which will install the\nsame bindings in effect as in the thread at the time bound-fn was called.\nThis may be used to define a helper function which runs on a different\nthread, but needs the same bindings in place.",
   :var-type "macro",
   :line 2020,
   :added "1.1",
   :namespace "clojure.core",
   :arglists ([& fntail]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bound-fn",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2020",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "bound-fn*",
   :doc
   "Returns a function, which will install the same bindings in effect as in\nthe thread at the time bound-fn* was called and then call f with any given\narguments. This may be used to define a helper function which runs on a\ndifferent thread, but needs the same bindings in place.",
   :var-type "function",
   :line 2008,
   :added "1.1",
   :namespace "clojure.core",
   :arglists ([f]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bound-fn*",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2008",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "bound?",
   :doc
   "Returns true if all of the vars provided as arguments have any bound value, root or thread-local.\nImplies that deref'ing the provided vars will succeed. Returns true if no vars are provided.",
   :var-type "function",
   :line 5648,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([& vars]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bound?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5648",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "bounded-count",
   :doc
   "If coll is counted? returns its count, else will count at most the first n\nelements of coll using its seq",
   :var-type "function",
   :line 7658,
   :added "1.9",
   :namespace "clojure.core",
   :arglists ([n coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bounded-count",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7658",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "butlast",
   :doc
   "Return a seq of all but the last item in coll, in linear time",
   :var-type "function",
   :line 274,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/butlast",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L274",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "byte",
   :doc "Coerce to byte",
   :var-type "function",
   :line 3527,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/byte",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3527",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "byte-array",
   :doc "Creates an array of bytes",
   :var-type "function",
   :line 5429,
   :added "1.1",
   :namespace "clojure.core",
   :arglists ([size-or-seq] [size init-val-or-seq]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/byte-array",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5429",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "bytes",
   :doc "Casts to byte[]",
   :var-type "function",
   :line 5489,
   :added "1.1",
   :namespace "clojure.core",
   :arglists ([xs]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bytes",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5489",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "bytes?",
   :doc "Return true if x is a byte array",
   :var-type "function",
   :line 5524,
   :added "1.9",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bytes?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5524",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "case",
   :doc
   "Takes an expression, and a set of clauses.\n\nEach clause can take the form of either:\n\ntest-constant result-expr\n\n(test-constant1 ... test-constantN)  result-expr\n\nThe test-constants are not evaluated. They must be compile-time\nliterals, and need not be quoted.  If the expression is equal to a\ntest-constant, the corresponding result-expr is returned. A single\ndefault expression can follow the clauses, and its value will be\nreturned if no clause matches. If no default expression is provided\nand no clause matches, an IllegalArgumentException is thrown.\n\nUnlike cond and condp, case does a constant-time dispatch, the\nclauses are not considered sequentially.  All manner of constant\nexpressions are acceptable in case, including numbers, strings,\nsymbols, keywords, and (Clojure) composites thereof. Note that since\nlists are used to group multiple constants that map to the same\nexpression, a vector can be used to match a list if needed. The\ntest-constants need not be all of the same type.",
   :var-type "macro",
   :line 6858,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([e & clauses]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/case",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6858",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "cast",
   :doc "Throws a ClassCastException if x is not a c, else returns x.",
   :var-type "function",
   :line 348,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([c x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/cast",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L348",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "cat",
   :doc
   "A transducer which concatenates the contents of each input, which must be a\ncollection, into the reduction.",
   :var-type "function",
   :line 7893,
   :added "1.7",
   :namespace "clojure.core",
   :arglists ([rf]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/cat",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7893",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "catch",
   :doc
   "Syntax for use with try.\n\nPlease see https://clojure.org/reference/special_forms#try",
   :var-type "special syntax",
   :added "1.0",
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/catch",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "char",
   :doc "Coerce to char",
   :var-type "function",
   :line 3533,
   :added "1.1",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/char",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3533",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "char-array",
   :doc "Creates an array of chars",
   :var-type "function",
   :line 5437,
   :added "1.1",
   :namespace "clojure.core",
   :arglists ([size-or-seq] [size init-val-or-seq]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/char-array",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5437",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "char-escape-string",
   :doc "Returns escape string for char or nil if none",
   :var-type "var",
   :line 200,
   :added "1.0",
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/char-escape-string",
   :source-url
   "https://github.com/clojure/clojure/blob/e6e7ca1fe859aeb74530ab18654815184e00f1f5/src/clj/clojure/core_print.clj#L200",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/e6e7ca1fe859aeb74530ab18654815184e00f1f5/src/clj/clojure/core_print.clj",
   :file "src/clj/clojure/core_print.clj"}
  {:name "char-name-string",
   :doc "Returns name string for char or nil if none",
   :var-type "var",
   :line 342,
   :added "1.0",
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/char-name-string",
   :source-url
   "https://github.com/clojure/clojure/blob/e6e7ca1fe859aeb74530ab18654815184e00f1f5/src/clj/clojure/core_print.clj#L342",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/e6e7ca1fe859aeb74530ab18654815184e00f1f5/src/clj/clojure/core_print.clj",
   :file "src/clj/clojure/core_print.clj"}
  {:name "char?",
   :doc "Return true if x is a Character",
   :var-type "function",
   :line 155,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/char?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L155",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "chars",
   :doc "Casts to char[]",
   :var-type "function",
   :line 5494,
   :added "1.1",
   :namespace "clojure.core",
   :arglists ([xs]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/chars",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5494",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "class",
   :doc "Returns the Class of x",
   :var-type "function",
   :line 3483,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/class",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3483",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "class?",
   :doc "Returns true if x is an instance of Class",
   :var-type "function",
   :line 5579,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/class?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5579",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "clear-agent-errors",
   :doc
   "DEPRECATED: Use 'restart-agent' instead.\nClears any exceptions thrown during asynchronous actions of the\nagent, allowing subsequent actions to occur.",
   :var-type "function",
   :line 2260,
   :added "1.0",
   :deprecated "1.2",
   :namespace "clojure.core",
   :arglists ([a]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/clear-agent-errors",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2260",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "clojure-version",
   :doc "Returns clojure version as a printable string.",
   :var-type "function",
   :line 7311,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/clojure-version",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7311",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "coll?",
   :doc "Returns true if x implements IPersistentCollection",
   :var-type "function",
   :line 6352,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/coll?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6352",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "comment",
   :doc "Ignores body, yields nil",
   :var-type "macro",
   :line 4852,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([& body]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/comment",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4852",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "commute",
   :doc
   "Must be called in a transaction. Sets the in-transaction-value of\nref to:\n\n(apply fun in-transaction-value-of-ref args)\n\nand returns the in-transaction-value of ref.\n\nAt the commit point of the transaction, sets the value of ref to be:\n\n(apply fun most-recently-committed-value-of-ref args)\n\nThus fun should be commutative, or, failing that, you must accept\nlast-one-in-wins behavior.  commute allows for more concurrency than\nref-set.",
   :var-type "function",
   :line 2436,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([ref fun & args]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/commute",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2436",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "comp",
   :doc
   "Takes a set of functions and returns a fn that is the composition\nof those fns.  The returned fn takes a variable number of args,\napplies the rightmost of fns to the args, the next\nfn (right-to-left) to the result, etc.",
   :var-type "function",
   :line 2571,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([] [f] [f g] [f g & fs]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/comp",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2571",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "comparator",
   :doc
   "Returns an implementation of java.util.Comparator based upon pred.",
   :var-type "function",
   :line 3099,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([pred]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/comparator",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3099",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "compare",
   :doc
   "Comparator. Returns a negative number, zero, or a positive number\nwhen x is logically 'less than', 'equal to', or 'greater than'\ny. Same as Java x.compareTo(y) except it also works for nil, and\ncompares numbers and collections in a type-independent manner. x\nmust implement Comparable",
   :var-type "function",
   :line 833,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x y]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/compare",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L833",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "compare-and-set!",
   :doc
   "Atomically sets the value of atom to newval if and only if the\ncurrent value of the atom is identical to oldval. Returns true if\nset happened, else false",
   :var-type "function",
   :line 2382,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([atom oldval newval]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/compare-and-set!",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2382",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "compile",
   :doc
   "Compiles the namespace named by the symbol lib into a set of\nclassfiles. The source for the lib must be in a proper\nclasspath-relative directory. The output files will go into the\ndirectory specified by *compile-path*, and that directory too must\nbe in the classpath.",
   :var-type "function",
   :line 6274,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([lib]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/compile",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6274",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "complement",
   :doc
   "Takes a fn f and returns a fn that takes the same arguments as f,\nhas the same effects, if any, and returns the opposite truth value.",
   :var-type "function",
   :line 1447,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([f]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/complement",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1447",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "completing",
   :doc
   "Takes a reducing function f of 2 args and returns a fn suitable for\ntransduce by adding an arity-1 signature that calls cf (default -\nidentity) on the result argument.",
   :var-type "function",
   :line 7069,
   :added "1.7",
   :namespace "clojure.core",
   :arglists ([f] [f cf]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/completing",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7069",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "concat",
   :doc
   "Returns a lazy seq representing the concatenation of the elements in the supplied colls.",
   :var-type "function",
   :line 720,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([] [x] [x y] [x y & zs]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/concat",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L720",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "cond",
   :doc
   "Takes a set of test/expr pairs. It evaluates each test one at a\ntime.  If a test returns logical true, cond evaluates and returns\nthe value of the corresponding expr and doesn't evaluate any of the\nother tests or exprs. (cond) returns nil.",
   :var-type "macro",
   :line 576,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([& clauses]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/cond",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L576",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "cond->",
   :doc
   "Takes an expression and a set of test/form pairs. Threads expr (via ->)\nthrough each form for which the corresponding test\nexpression is true. Note that, unlike cond branching, cond-> threading does\nnot short circuit after the first true test expression.",
   :var-type "macro",
   :line 7812,
   :added "1.5",
   :namespace "clojure.core",
   :arglists ([expr & clauses]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/cond->",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7812",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "cond->>",
   :doc
   "Takes an expression and a set of test/form pairs. Threads expr (via ->>)\nthrough each form for which the corresponding test expression\nis true.  Note that, unlike cond branching, cond->> threading does not short circuit\nafter the first true test expression.",
   :var-type "macro",
   :line 7829,
   :added "1.5",
   :namespace "clojure.core",
   :arglists ([expr & clauses]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/cond->>",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7829",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "condp",
   :doc
   "Takes a binary predicate, an expression, and a set of clauses.\nEach clause can take the form of either:\n\ntest-expr result-expr\n\ntest-expr :>> result-fn\n\nNote :>> is an ordinary keyword.\n\nFor each clause, (pred test-expr expr) is evaluated. If it returns\nlogical true, the clause is a match. If a binary clause matches, the\nresult-expr is returned, if a ternary clause matches, its result-fn,\nwhich must be a unary function, is called with the result of the\npredicate as its argument, the result of that call being the return\nvalue of condp. A single default expression can follow the clauses,\nand its value will be returned if no clause matches. If no default\nexpression is provided and no clause matches, an\nIllegalArgumentException is thrown.",
   :var-type "macro",
   :line 6513,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([pred expr & clauses]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/condp",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6513",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "conj",
   :doc
   "conj[oin]. Returns a new collection with the xs\n'added'. (conj nil item) returns (item).\n(conj coll) returns coll. (conj) returns [].\nThe 'addition' may happen at different 'places' depending\non the concrete type.",
   :var-type "function",
   :line 75,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([] [coll] [coll x] [coll x & xs]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/conj",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L75",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "conj!",
   :doc
   "Adds x to the transient collection, and return coll. The 'addition'\nmay happen at different 'places' depending on the concrete type.",
   :var-type "function",
   :line 3381,
   :added "1.1",
   :namespace "clojure.core",
   :arglists ([] [coll] [coll x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/conj!",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3381",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "cons",
   :doc
   "Returns a new seq where x is the first element and seq is\nthe rest.",
   :var-type "function",
   :line 22,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x seq]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/cons",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L22",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "constantly",
   :doc
   "Returns a function that takes any number of arguments and returns x.",
   :var-type "function",
   :line 1459,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/constantly",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1459",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "construct-proxy",
   :doc
   "Takes a proxy class and any arguments for its superclass ctor and\ncreates and returns an instance of the proxy.",
   :var-type "function",
   :line 295,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([c & ctor-args]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/construct-proxy",
   :source-url
   "https://github.com/clojure/clojure/blob/658693f6cf97e6ab0ff789e096c9eb6654e4d3ab/src/clj/clojure/core_proxy.clj#L295",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/658693f6cf97e6ab0ff789e096c9eb6654e4d3ab/src/clj/clojure/core_proxy.clj",
   :file "src/clj/clojure/core_proxy.clj"}
  {:name "contains?",
   :doc
   "Returns true if key is present in the given collection, otherwise\nreturns false.  Note that for numerically indexed collections like\nvectors and Java arrays, this tests if the numeric key is within the\nrange of indexes. 'contains?' operates constant or logarithmic time;\nit will not perform a linear search for a value.  See also 'some'.",
   :var-type "function",
   :line 1502,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([coll key]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/contains?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1502",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "count",
   :doc
   "Returns the number of items in the collection. (count nil) returns\n0.  Also works on strings, arrays, and Java Collections and Maps",
   :var-type "function",
   :line 876,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/count",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L876",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "counted?",
   :doc "Returns true if coll implements count in constant time",
   :var-type "function",
   :line 6401,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/counted?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6401",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "create-ns",
   :doc
   "Create a new namespace named by the symbol if one doesn't already\nexist, returns it or the already-existing namespace of the same\nname.",
   :var-type "function",
   :line 4155,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([sym]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/create-ns",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4155",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "create-struct",
   :doc "Returns a structure basis object.",
   :var-type "function",
   :line 4061,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([& keys]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/create-struct",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4061",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "cycle",
   :doc
   "Returns a lazy (infinite!) sequence of repetitions of the items in coll.",
   :var-type "function",
   :line 2999,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/cycle",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2999",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "dec",
   :doc
   "Returns a number one less than num. Does not auto-promote\nlongs, will throw on overflow. See also: dec'",
   :var-type "function",
   :line 1156,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/dec",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1156",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "dec'",
   :doc
   "Returns a number one less than num. Supports arbitrary precision.\nSee also: dec",
   :var-type "function",
   :line 1149,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/dec'",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1149",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "decimal?",
   :doc "Returns true if n is a BigDecimal",
   :var-type "function",
   :line 3621,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([n]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/decimal?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3621",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "declare",
   :doc
   "defs the supplied var names with no bindings, useful for making forward declarations.",
   :var-type "macro",
   :line 2790,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([& names]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/declare",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2790",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "dedupe",
   :doc
   "Returns a lazy sequence removing consecutive duplicates in coll.\nReturns a transducer when no collection is provided.",
   :var-type "function",
   :line 7929,
   :added "1.7",
   :namespace "clojure.core",
   :arglists ([] [coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/dedupe",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7929",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "def",
   :doc
   "Creates and interns a global var with the name\nof symbol in the current namespace (*ns*) or locates such a var if\nit already exists.  If init is supplied, it is evaluated, and the\nroot binding of the var is set to the resulting value.  If init is\nnot supplied, the root binding of the var is unaffected.\n\nPlease see https://clojure.org/reference/special_forms#def",
   :var-type "special form",
   :added "1.0",
   :forms [(def symbol doc-string? init?)],
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/def",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "default-data-readers",
   :doc
   "Default map of data reader functions provided by Clojure. May be\noverridden by binding *data-readers*.",
   :var-type "var",
   :line 8070,
   :added "1.4",
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/default-data-readers",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L8070",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "definline",
   :doc
   "Experimental - like defmacro, except defines a named function whose\nbody is the expansion, calls to which may be expanded inline as if\nit were a macro. Cannot be used with variadic (&) args.",
   :var-type "macro",
   :line 5365,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([name & decl]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/definline",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5365",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "definterface",
   :doc
   "Creates a new Java interface with the given name and method sigs.\nThe method return types and parameter types may be specified with type hints,\ndefaulting to Object if omitted.\n\n(definterface MyInterface\n  (^int method1 [x])\n  (^Bar method2 [^Baz b ^Quux q]))",
   :var-type "macro",
   :line 33,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([name & sigs]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/definterface",
   :source-url
   "https://github.com/clojure/clojure/blob/c1ce24118af6e596839be6decf2bdb0da53dc8ed/src/clj/clojure/core_deftype.clj#L33",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/c1ce24118af6e596839be6decf2bdb0da53dc8ed/src/clj/clojure/core_deftype.clj",
   :file "src/clj/clojure/core_deftype.clj"}
  {:name "defmacro",
   :doc
   "Like defn, but the resulting function name is declared as a\nmacro and will be used as a macro by the compiler when it is\ncalled.",
   :var-type "macro",
   :line 446,
   :added "1.0",
   :namespace "clojure.core",
   :arglists
   ([name doc-string? attr-map? [params*] body]
    [name doc-string? attr-map? ([params*] body) + attr-map?]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/defmacro",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L446",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "defmethod",
   :doc
   "Creates and installs a new method of multimethod associated with dispatch-value. ",
   :var-type "macro",
   :line 1797,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([multifn dispatch-val & fn-tail]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/defmethod",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1797",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "defmulti",
   :doc
   "Creates a new multimethod with the associated dispatch function.\nThe docstring and attr-map are optional.\n\nOptions are key-value pairs and may be one of:\n\n:default\n\nThe default dispatch value, defaults to :default\n\n:hierarchy\n\nThe value used for hierarchical dispatch (e.g. ::square is-a ::shape)\n\nHierarchies are type-like relationships that do not depend upon type\ninheritance. By default Clojure's multimethods dispatch off of a\nglobal hierarchy map.  However, a hierarchy relationship can be\ncreated with the derive function used to augment the root ancestor\ncreated with make-hierarchy.\n\nMultimethods expect the value of the hierarchy option to be supplied as\na reference type e.g. a var (i.e. via the Var-quote dispatch macro #'\nor the var special form).",
   :var-type "macro",
   :line 1739,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([name docstring? attr-map? dispatch-fn & options]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/defmulti",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1739",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "defn",
   :doc
   "Same as (def name (fn [params* ] exprs*)) or (def\nname (fn ([params* ] exprs*)+)) with any doc-string or attrs added\nto the var metadata. prepost-map defines a map with optional keys\n:pre and :post that contain collections of pre or post conditions.",
   :var-type "macro",
   :line 285,
   :added "1.0",
   :namespace "clojure.core",
   :arglists
   ([name doc-string? attr-map? [params*] prepost-map? body]
    [name
     doc-string?
     attr-map?
     ([params*] prepost-map? body)
     +
     attr-map?]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/defn",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L285",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "defn-",
   :doc "same as defn, yielding non-public def",
   :var-type "macro",
   :line 5070,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([name & decls]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/defn-",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5070",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "defonce",
   :doc
   "defs name to have the root value of the expr iff the named var has no root value,\nelse expr is unevaluated",
   :var-type "macro",
   :line 5964,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([name expr]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/defonce",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5964",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "defprotocol",
   :doc
   "A protocol is a named set of named methods and their signatures:\n(defprotocol AProtocolName\n\n  ;optional doc string\n  \"A doc string for AProtocol abstraction\"\n\n ;options\n :extend-via-metadata true\n\n;method signatures\n  (bar [this a b] \"bar docs\")\n  (baz [this a] [this a b] [this a b c] \"baz docs\"))\n\nNo implementations are provided. Docs can be specified for the\nprotocol overall and for each method. The above yields a set of\npolymorphic functions and a protocol object. All are\nnamespace-qualified by the ns enclosing the definition The resulting\nfunctions dispatch on the type of their first argument, which is\nrequired and corresponds to the implicit target object ('this' in \nJava parlance). defprotocol is dynamic, has no special compile-time \neffect, and defines no new types or classes. Implementations of \nthe protocol methods can be provided using extend.\n\nWhen :extend-via-metadata is true, values can extend protocols by\nadding metadata where keys are fully-qualified protocol function\nsymbols and values are function implementations. Protocol\nimplementations are checked first for direct definitions (defrecord,\ndeftype, reify), then metadata definitions, then external\nextensions (extend, extend-type, extend-protocol)\n\ndefprotocol will automatically generate a corresponding interface,\nwith the same name as the protocol, i.e. given a protocol:\nmy.ns/Protocol, an interface: my.ns.Protocol. The interface will\nhave methods corresponding to the protocol functions, and the\nprotocol will automatically work with instances of the interface.\n\nNote that you should not use this interface with deftype or\nreify, as they support the protocol directly:\n\n(defprotocol P \n  (foo [this]) \n  (bar-me [this] [this y]))\n\n(deftype Foo [a b c] \n P\n  (foo [this] a)\n  (bar-me [this] b)\n  (bar-me [this y] (+ c y)))\n\n(bar-me (Foo. 1 2 3) 42)\n=> 45\n\n(foo \n  (let [x 42]\n    (reify P \n      (foo [this] 17)\n      (bar-me [this] x)\n      (bar-me [this y] x))))\n=> 17",
   :var-type "macro",
   :line 723,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([name & opts+sigs]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/defprotocol",
   :source-url
   "https://github.com/clojure/clojure/blob/c1ce24118af6e596839be6decf2bdb0da53dc8ed/src/clj/clojure/core_deftype.clj#L723",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/c1ce24118af6e596839be6decf2bdb0da53dc8ed/src/clj/clojure/core_deftype.clj",
   :file "src/clj/clojure/core_deftype.clj"}
  {:name "defrecord",
   :doc
   "(defrecord name [fields*]  options* specs*)\n\nOptions are expressed as sequential keywords and arguments (in any order).\n\nSupported options:\n:load-ns - if true, importing the record class will cause the\n           namespace in which the record was defined to be loaded.\n           Defaults to false.\n\nEach spec consists of a protocol or interface name followed by zero\nor more method bodies:\n\nprotocol-or-interface-or-Object\n(methodName [args*] body)*\n\nDynamically generates compiled bytecode for class with the given\nname, in a package with the same name as the current namespace, the\ngiven fields, and, optionally, methods for protocols and/or\ninterfaces.\n\nThe class will have the (immutable) fields named by\nfields, which can have type hints. Protocols/interfaces and methods\nare optional. The only methods that can be supplied are those\ndeclared in the protocols/interfaces.  Note that method bodies are\nnot closures, the local environment includes only the named fields,\nand those fields can be accessed directly.\n\nMethod definitions take the form:\n\n(methodname [args*] body)\n\nThe argument and return types can be hinted on the arg and\nmethodname symbols. If not supplied, they will be inferred, so type\nhints should be reserved for disambiguation.\n\nMethods should be supplied for all methods of the desired\nprotocol(s) and interface(s). You can also define overrides for\nmethods of Object. Note that a parameter must be supplied to\ncorrespond to the target object ('this' in Java parlance). Thus\nmethods for interfaces will take one more argument than do the\ninterface declarations. Note also that recur calls to the method\nhead should *not* pass the target object, it will be supplied\nautomatically and can not be substituted.\n\nIn the method bodies, the (unqualified) name can be used to name the\nclass (for calls to new, instance? etc).\n\nThe class will have implementations of several (clojure.lang)\ninterfaces generated automatically: IObj (metadata support) and\nIPersistentMap, and all of their superinterfaces.\n\nIn addition, defrecord will define type-and-value-based =,\nand will defined Java .hashCode and .equals consistent with the\ncontract for java.util.Map.\n\nWhen AOT compiling, generates compiled bytecode for a class with the\ngiven name (a symbol), prepends the current ns as the package, and\nwrites the .class file to the *compile-path* directory.\n\nTwo constructors will be defined, one taking the designated fields\nfollowed by a metadata map (nil for none) and an extension field\nmap (nil for none), and one taking only the fields (using nil for\nmeta and extension fields). Note that the field names __meta,\n__extmap, __hash and __hasheq are currently reserved and should not\nbe used when defining your own records.\n\nGiven (defrecord TypeName ...), two factory functions will be\ndefined: ->TypeName, taking positional parameters for the fields,\nand map->TypeName, taking a map of keywords to field values.",
   :var-type "macro",
   :line 327,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([name [& fields] & opts+specs]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/defrecord",
   :source-url
   "https://github.com/clojure/clojure/blob/c1ce24118af6e596839be6decf2bdb0da53dc8ed/src/clj/clojure/core_deftype.clj#L327",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/c1ce24118af6e596839be6decf2bdb0da53dc8ed/src/clj/clojure/core_deftype.clj",
   :file "src/clj/clojure/core_deftype.clj"}
  {:name "defstruct",
   :doc "Same as (def name (create-struct keys...))",
   :var-type "macro",
   :line 4068,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([name & keys]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/defstruct",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4068",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "deftype",
   :doc
   "(deftype name [fields*]  options* specs*)\n\nOptions are expressed as sequential keywords and arguments (in any order).\n\nSupported options:\n:load-ns - if true, importing the type class will cause the\n           namespace in which the type was defined to be loaded.\n           Defaults to false.\n\nEach spec consists of a protocol or interface name followed by zero\nor more method bodies:\n\nprotocol-or-interface-or-Object\n(methodName [args*] body)*\n\nDynamically generates compiled bytecode for class with the given\nname, in a package with the same name as the current namespace, the\ngiven fields, and, optionally, methods for protocols and/or\ninterfaces. \n\nThe class will have the (by default, immutable) fields named by\nfields, which can have type hints. Protocols/interfaces and methods\nare optional. The only methods that can be supplied are those\ndeclared in the protocols/interfaces.  Note that method bodies are\nnot closures, the local environment includes only the named fields,\nand those fields can be accessed directly. Fields can be qualified\nwith the metadata :volatile-mutable true or :unsynchronized-mutable\ntrue, at which point (set! afield aval) will be supported in method\nbodies. Note well that mutable fields are extremely difficult to use\ncorrectly, and are present only to facilitate the building of higher\nlevel constructs, such as Clojure's reference types, in Clojure\nitself. They are for experts only - if the semantics and\nimplications of :volatile-mutable or :unsynchronized-mutable are not\nimmediately apparent to you, you should not be using them.\n\nMethod definitions take the form:\n\n(methodname [args*] body)\n\nThe argument and return types can be hinted on the arg and\nmethodname symbols. If not supplied, they will be inferred, so type\nhints should be reserved for disambiguation.\n\nMethods should be supplied for all methods of the desired\nprotocol(s) and interface(s). You can also define overrides for\nmethods of Object. Note that a parameter must be supplied to\ncorrespond to the target object ('this' in Java parlance). Thus\nmethods for interfaces will take one more argument than do the\ninterface declarations. Note also that recur calls to the method\nhead should *not* pass the target object, it will be supplied\nautomatically and can not be substituted.\n\nIn the method bodies, the (unqualified) name can be used to name the\nclass (for calls to new, instance? etc).\n\nWhen AOT compiling, generates compiled bytecode for a class with the\ngiven name (a symbol), prepends the current ns as the package, and\nwrites the .class file to the *compile-path* directory.\n\nOne constructor will be defined, taking the designated fields.  Note\nthat the field names __meta, __extmap, __hash and __hasheq are currently\nreserved and should not be used when defining your own types.\n\nGiven (deftype TypeName ...), a factory function called ->TypeName\nwill be defined, taking positional parameters for the fields",
   :var-type "macro",
   :line 437,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([name [& fields] & opts+specs]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/deftype",
   :source-url
   "https://github.com/clojure/clojure/blob/c1ce24118af6e596839be6decf2bdb0da53dc8ed/src/clj/clojure/core_deftype.clj#L437",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/c1ce24118af6e596839be6decf2bdb0da53dc8ed/src/clj/clojure/core_deftype.clj",
   :file "src/clj/clojure/core_deftype.clj"}
  {:name "delay",
   :doc
   "Takes a body of expressions and yields a Delay object that will\ninvoke the body only the first time it is forced (with force or deref/@), and\nwill cache the result and return it on all subsequent force\ncalls. See also - realized?",
   :var-type "macro",
   :line 748,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([& body]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/delay",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L748",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "delay?",
   :doc "returns true if x is a Delay created with delay",
   :var-type "function",
   :line 757,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/delay?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L757",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "deliver",
   :doc
   "Delivers the supplied value to the promise, releasing any pending\nderefs. A subsequent call to deliver on a promise will have no effect.",
   :var-type "function",
   :line 7357,
   :added "1.1",
   :namespace "clojure.core",
   :arglists ([promise val]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/deliver",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7357",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "denominator",
   :doc "Returns the denominator part of a Ratio.",
   :var-type "function",
   :line 3613,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([r]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/denominator",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3613",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "deref",
   :doc
   "Also reader macro: @ref/@agent/@var/@atom/@delay/@future/@promise. Within a transaction,\nreturns the in-transaction-value of ref, else returns the\nmost-recently-committed value of ref. When applied to a var, agent\nor atom, returns its current state. When applied to a delay, forces\nit if not already forced. When applied to a future, will block if\ncomputation not complete. When applied to a promise, will block\nuntil a value is delivered.  The variant taking a timeout can be\nused for blocking references (futures and promises), and will return\ntimeout-val if the timeout (in milliseconds) is reached before a\nvalue is available. See also - realized?.",
   :var-type "function",
   :line 2320,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([ref] [ref timeout-ms timeout-val]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/deref",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2320",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "derive",
   :doc
   "Establishes a parent/child relationship between parent and\ntag. Parent must be a namespace-qualified symbol or keyword and\nchild can be either a namespace-qualified symbol or keyword or a\nclass. h must be a hierarchy obtained from make-hierarchy, if not\nsupplied defaults to, and modifies, the global hierarchy.",
   :var-type "function",
   :line 5762,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([tag parent] [h tag parent]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/derive",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5762",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "descendants",
   :doc
   "Returns the immediate and indirect children of tag, through a\nrelationship established via derive. h must be a hierarchy obtained\nfrom make-hierarchy, if not supplied defaults to the global\nhierarchy. Note: does not work on Java type inheritance\nrelationships.",
   :var-type "function",
   :line 5750,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([tag] [h tag]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/descendants",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5750",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "disj",
   :doc
   "disj[oin]. Returns a new set of the same (hashed/sorted) type, that\ndoes not contain key(s).",
   :var-type "function",
   :line 1545,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([set] [set key] [set key & ks]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/disj",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1545",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "disj!",
   :doc
   "disj[oin]. Returns a transient set of the same (hashed/sorted) type, that\ndoes not contain key(s).",
   :var-type "function",
   :line 3423,
   :added "1.1",
   :namespace "clojure.core",
   :arglists ([set] [set key] [set key & ks]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/disj!",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3423",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "dissoc",
   :doc
   "dissoc[iate]. Returns a new map of the same (hashed/sorted) type,\nthat does not contain a mapping for key(s).",
   :var-type "function",
   :line 1531,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([map] [map key] [map key & ks]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/dissoc",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1531",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "dissoc!",
   :doc
   "Returns a transient map that doesn't contain a mapping for key(s).",
   :var-type "function",
   :line 3404,
   :added "1.1",
   :namespace "clojure.core",
   :arglists ([map key] [map key & ks]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/dissoc!",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3404",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "distinct",
   :doc
   "Returns a lazy sequence of the elements of coll with duplicates removed.\nReturns a stateful transducer when no collection is provided.",
   :var-type "function",
   :line 5174,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([] [coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/distinct",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5174",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "distinct?",
   :doc "Returns true if no two of the arguments are =",
   :var-type "function",
   :line 5821,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x] [x y] [x y & more]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/distinct?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5821",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "do",
   :doc
   "Evaluates the expressions in order and returns the value of\nthe last. If no expressions are supplied, returns nil.\n\nPlease see https://clojure.org/reference/special_forms#do",
   :var-type "special form",
   :added "1.0",
   :forms [(do exprs*)],
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/do",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "doall",
   :doc
   "When lazy sequences are produced via functions that have side\neffects, any effects other than those needed to produce the first\nelement in the seq do not occur until the seq is consumed. doall can\nbe used to force any effects. Walks through the successive nexts of\nthe seq, retains the head and returns it, thus causing the entire\nseq to reside in memory at one time.",
   :var-type "function",
   :line 3153,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([coll] [n coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/doall",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3153",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "dorun",
   :doc
   "When lazy sequences are produced via functions that have side\neffects, any effects other than those needed to produce the first\nelement in the seq do not occur until the seq is consumed. dorun can\nbe used to force any effects. Walks through the successive nexts of\nthe seq, does not retain the head and returns nil.",
   :var-type "function",
   :line 3138,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([coll] [n coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/dorun",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3138",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "doseq",
   :doc
   "Repeatedly executes body (presumably for side-effects) with\nbindings and filtering as provided by \"for\".  Does not retain\nthe head of the sequence. Returns nil.",
   :var-type "macro",
   :line 3231,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([seq-exprs & body]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/doseq",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3231",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "dosync",
   :doc
   "Runs the exprs (in an implicit do) in a transaction that encompasses\nexprs and any nested calls.  Starts a transaction if none is already\nrunning on this thread. Any uncaught exception will abort the\ntransaction and flow out of dosync. The exprs may be run more than\nonce, but any effects on Refs will be atomic.",
   :var-type "macro",
   :line 5221,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([& exprs]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/dosync",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5221",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "dotimes",
   :doc
   "bindings => name n\n\nRepeatedly executes body (presumably for side-effects) with name\nbound to integers from 0 through n-1.",
   :var-type "macro",
   :line 3328,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([bindings & body]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/dotimes",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3328",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "doto",
   :doc
   "Evaluates x then calls all of the methods and functions with the\nvalue of x supplied at the front of the given arguments.  The forms\nare evaluated in order.  Returns x.\n\n(doto (new java.util.HashMap) (.put \"a\" 1) (.put \"b\" 2))",
   :var-type "macro",
   :line 3875,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x & forms]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/doto",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3875",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "double",
   :doc "Coerce to double",
   :var-type "function",
   :line 3515,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/double",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3515",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "double-array",
   :doc "Creates an array of doubles",
   :var-type "function",
   :line 5453,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([size-or-seq] [size init-val-or-seq]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/double-array",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5453",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "double?",
   :doc "Return true if x is a Double",
   :var-type "function",
   :line 1440,
   :added "1.9",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/double?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1440",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "doubles",
   :doc "Casts to double[]",
   :var-type "function",
   :line 5514,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([xs]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/doubles",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5514",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "drop",
   :doc
   "Returns a laziness-preserving sequence of all but the first n items in coll.\nReturns a stateful transducer when no collection is provided.",
   :var-type "function",
   :line 2923,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([n] [n coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/drop",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2923",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "drop-last",
   :doc
   "Return a lazy sequence of all but the last n (default 1) items in coll",
   :var-type "function",
   :line 2954,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([coll] [n coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/drop-last",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2954",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "drop-while",
   :doc
   "Returns a lazy sequence of the items in coll starting from the\nfirst item for which (pred item) returns logical false.  Returns a\nstateful transducer when no collection is provided.",
   :var-type "function",
   :line 2972,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([pred] [pred coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/drop-while",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2972",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "eduction",
   :doc
   "Returns a reducible/iterable application of the transducers\nto the items in coll. Transducers are applied in order as if\ncombined with comp. Note that these applications will be\nperformed every time reduce/iterator is called.",
   :var-type "function",
   :line 7968,
   :added "1.7",
   :namespace "clojure.core",
   :arglists ([xform* coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/eduction",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7968",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "empty",
   :doc
   "Returns an empty collection of the same category as coll, or nil",
   :var-type "function",
   :line 5377,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/empty",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5377",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "empty?",
   :doc
   "Returns true if coll has no items. To check the emptiness of a seq,\nplease use the idiom (seq x) rather than (not (empty? x))",
   :var-type "function",
   :line 6407,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/empty?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6407",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "ensure",
   :doc
   "Must be called in a transaction. Protects the ref from modification\nby other transactions.  Returns the in-transaction-value of\nref. Allows for more concurrency than (ref-set ref @ref)",
   :var-type "function",
   :line 2502,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([ref]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ensure",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2502",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "ensure-reduced",
   :doc
   "If x is already reduced?, returns it, else returns (reduced x)",
   :var-type "function",
   :line 2863,
   :added "1.7",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ensure-reduced",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2863",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "enumeration-seq",
   :doc "Returns a seq on a java.util.Enumeration",
   :var-type "function",
   :line 5867,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([e]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/enumeration-seq",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5867",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "error-handler",
   :doc
   "Returns the error-handler of agent a, or nil if there is none.\nSee set-error-handler!",
   :var-type "function",
   :line 2218,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([a]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/error-handler",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2218",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "error-mode",
   :doc "Returns the error-mode of agent a.  See set-error-mode!",
   :var-type "function",
   :line 2243,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([a]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/error-mode",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2243",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "eval",
   :doc
   "Evaluates the form data structure (not text!) and returns the result.",
   :var-type "function",
   :line 3225,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([form]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/eval",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3225",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "even?",
   :doc
   "Returns true if n is even, throws an exception if n is not an integer",
   :var-type "function",
   :line 1400,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([n]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/even?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1400",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "every-pred",
   :doc
   "Takes a set of predicates and returns a function f that returns true if all of its\ncomposing predicates return a logical true value against all of its arguments, else it returns\nfalse. Note that f is short-circuiting in that it will stop execution on the first\nargument that triggers a logical false result against the original predicates.",
   :var-type "function",
   :line 7670,
   :added "1.3",
   :namespace "clojure.core",
   :arglists ([p] [p1 p2] [p1 p2 p3] [p1 p2 p3 & ps]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/every-pred",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7670",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "every?",
   :doc
   "Returns true if (pred x) is logical true for every x in coll, else\nfalse.",
   :var-type "function",
   :line 2686,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([pred coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/every?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2686",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "ex-cause",
   :doc
   "Returns the cause of ex if ex is a Throwable.\nOtherwise returns nil.",
   :var-type "function",
   :line 4949,
   :added "1.10",
   :namespace "clojure.core",
   :arglists ([ex]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ex-cause",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4949",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "ex-data",
   :doc
   "Returns exception data (a map) if ex is an IExceptionInfo.\nOtherwise returns nil.",
   :var-type "function",
   :line 4933,
   :added "1.4",
   :namespace "clojure.core",
   :arglists ([ex]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ex-data",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4933",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "ex-info",
   :doc
   "Create an instance of ExceptionInfo, a RuntimeException subclass\nthat carries a map of additional data.",
   :var-type "function",
   :line 4924,
   :added "1.4",
   :namespace "clojure.core",
   :arglists ([msg map] [msg map cause]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ex-info",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4924",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "ex-message",
   :doc
   "Returns the message attached to ex if ex is a Throwable.\nOtherwise returns nil.",
   :var-type "function",
   :line 4941,
   :added "1.10",
   :namespace "clojure.core",
   :arglists ([ex]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ex-message",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4941",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "extend",
   :doc
   "Implementations of protocol methods can be provided using the extend construct:\n\n (extend AType\n   AProtocol\n    {:foo an-existing-fn\n     :bar (fn [a b] ...)\n     :baz (fn ([a]...) ([a b] ...)...)}\n   BProtocol \n     {...} \n   ...)\n\n extend takes a type/class (or interface, see below), and one or more\n protocol + method map pairs. It will extend the polymorphism of the\n protocol's methods to call the supplied methods when an AType is\n provided as the first argument. \n\n Method maps are maps of the keyword-ized method names to ordinary\n fns. This facilitates easy reuse of existing fns and fn maps, for\n code reuse/mixins without derivation or composition. You can extend\n an interface to a protocol. This is primarily to facilitate interop\n with the host (e.g. Java) but opens the door to incidental multiple\n inheritance of implementation since a class can inherit from more\n than one interface, both of which extend the protocol. It is TBD how\n to specify which impl to use. You can extend a protocol on nil.\n\n If you are supplying the definitions explicitly (i.e. not reusing\n exsting functions or mixin maps), you may find it more convenient to\n use the extend-type or extend-protocol macros.\n\n Note that multiple independent extend clauses can exist for the same\n type, not all protocols need be defined in a single extend call.\n\n See also:\n extends?, satisfies?, extenders",
   :var-type "function",
   :line 787,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([atype & proto+mmaps]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/extend",
   :source-url
   "https://github.com/clojure/clojure/blob/c1ce24118af6e596839be6decf2bdb0da53dc8ed/src/clj/clojure/core_deftype.clj#L787",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/c1ce24118af6e596839be6decf2bdb0da53dc8ed/src/clj/clojure/core_deftype.clj",
   :file "src/clj/clojure/core_deftype.clj"}
  {:name "extend-protocol",
   :doc
   "Useful when you want to provide several implementations of the same\nprotocol all at once. Takes a single protocol and the implementation\nof that protocol for one or more types. Expands into calls to\nextend-type:\n\n(extend-protocol Protocol\n  AType\n    (foo [x] ...)\n    (bar [x y] ...)\n  BType\n    (foo [x] ...)\n    (bar [x y] ...)\n  AClass\n    (foo [x] ...)\n    (bar [x y] ...)\n  nil\n    (foo [x] ...)\n    (bar [x y] ...))\n\nexpands into:\n\n(do\n (clojure.core/extend-type AType Protocol \n   (foo [x] ...) \n   (bar [x y] ...))\n (clojure.core/extend-type BType Protocol \n   (foo [x] ...) \n   (bar [x y] ...))\n (clojure.core/extend-type AClass Protocol \n   (foo [x] ...) \n   (bar [x y] ...))\n (clojure.core/extend-type nil Protocol \n   (foo [x] ...) \n   (bar [x y] ...)))",
   :var-type "macro",
   :line 887,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([p & specs]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/extend-protocol",
   :source-url
   "https://github.com/clojure/clojure/blob/c1ce24118af6e596839be6decf2bdb0da53dc8ed/src/clj/clojure/core_deftype.clj#L887",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/c1ce24118af6e596839be6decf2bdb0da53dc8ed/src/clj/clojure/core_deftype.clj",
   :file "src/clj/clojure/core_deftype.clj"}
  {:name "extend-type",
   :doc
   "A macro that expands into an extend call. Useful when you are\nsupplying the definitions explicitly inline, extend-type\nautomatically creates the maps required by extend.  Propagates the\nclass as a type hint on the first argument of all fns.\n\n(extend-type MyType \n  Countable\n    (cnt [c] ...)\n  Foo\n    (bar [x y] ...)\n    (baz ([x] ...) ([x y & zs] ...)))\n\nexpands into:\n\n(extend MyType\n Countable\n   {:cnt (fn [c] ...)}\n Foo\n   {:baz (fn ([x] ...) ([x y & zs] ...))\n    :bar (fn [x y] ...)})",
   :var-type "macro",
   :line 855,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([t & specs]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/extend-type",
   :source-url
   "https://github.com/clojure/clojure/blob/c1ce24118af6e596839be6decf2bdb0da53dc8ed/src/clj/clojure/core_deftype.clj#L855",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/c1ce24118af6e596839be6decf2bdb0da53dc8ed/src/clj/clojure/core_deftype.clj",
   :file "src/clj/clojure/core_deftype.clj"}
  {:name "extenders",
   :doc
   "Returns a collection of the types explicitly extending protocol",
   :var-type "function",
   :line 578,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([protocol]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/extenders",
   :source-url
   "https://github.com/clojure/clojure/blob/c1ce24118af6e596839be6decf2bdb0da53dc8ed/src/clj/clojure/core_deftype.clj#L578",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/c1ce24118af6e596839be6decf2bdb0da53dc8ed/src/clj/clojure/core_deftype.clj",
   :file "src/clj/clojure/core_deftype.clj"}
  {:name "extends?",
   :doc "Returns true if atype extends protocol",
   :var-type "function",
   :line 571,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([protocol atype]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/extends?",
   :source-url
   "https://github.com/clojure/clojure/blob/c1ce24118af6e596839be6decf2bdb0da53dc8ed/src/clj/clojure/core_deftype.clj#L571",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/c1ce24118af6e596839be6decf2bdb0da53dc8ed/src/clj/clojure/core_deftype.clj",
   :file "src/clj/clojure/core_deftype.clj"}
  {:name "false?",
   :doc "Returns true if x is the value false, false otherwise.",
   :var-type "function",
   :line 507,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/false?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L507",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "ffirst",
   :doc "Same as (first (first x))",
   :var-type "function",
   :line 100,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ffirst",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L100",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "file-seq",
   :doc "A tree seq on java.io.Files",
   :var-type "function",
   :line 5093,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([dir]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/file-seq",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5093",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "filter",
   :doc
   "Returns a lazy sequence of the items in coll for which\n(pred item) returns logical true. pred must be free of side-effects.\nReturns a transducer when no collection is provided.",
   :var-type "function",
   :line 2807,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([pred] [pred coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/filter",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2807",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "filterv",
   :doc
   "Returns a vector of the items in coll for which\n(pred item) returns logical true. pred must be free of side-effects.",
   :var-type "function",
   :line 7137,
   :added "1.4",
   :namespace "clojure.core",
   :arglists ([pred coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/filterv",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7137",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "finally",
   :doc
   "Syntax for use with try.\n\nPlease see https://clojure.org/reference/special_forms#try",
   :var-type "special syntax",
   :added "1.0",
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/finally",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "find",
   :doc "Returns the map entry for key, or nil if key not present.",
   :var-type "function",
   :line 1561,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([map key]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/find",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1561",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "find-keyword",
   :doc
   "Returns a Keyword with the given namespace and name if one already\nexists.  This function will not intern a new keyword. If the keyword\nhas not already been interned, it will return nil.  Do not use :\nin the keyword strings, it will be added automatically.",
   :var-type "function",
   :line 627,
   :added "1.3",
   :namespace "clojure.core",
   :arglists ([name] [ns name]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/find-keyword",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L627",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "find-ns",
   :doc
   "Returns the namespace named by the symbol or nil if it doesn't exist.",
   :var-type "function",
   :line 4149,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([sym]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/find-ns",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4149",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "find-var",
   :doc
   "Returns the global var named by the namespace-qualified symbol, or\nnil if no var with that name.",
   :var-type "function",
   :line 2029,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([sym]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/find-var",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2029",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "first",
   :doc
   "Returns the first item in the collection. Calls seq on its\nargument. If coll is nil, returns nil.",
   :var-type "function",
   :line 49,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/first",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L49",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "flatten",
   :doc
   "Takes any nested combination of sequential things (lists, vectors,\netc.) and returns their contents as a single, flat lazy sequence.\n(flatten nil) returns an empty sequence.",
   :var-type "function",
   :line 7366,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/flatten",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7366",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "float",
   :doc "Coerce to float",
   :var-type "function",
   :line 3509,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/float",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3509",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "float-array",
   :doc "Creates an array of floats",
   :var-type "function",
   :line 5413,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([size-or-seq] [size init-val-or-seq]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/float-array",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5413",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "float?",
   :doc "Returns true if n is a floating point number",
   :var-type "function",
   :line 3627,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([n]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/float?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3627",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "floats",
   :doc "Casts to float[]",
   :var-type "function",
   :line 5504,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([xs]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/floats",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5504",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "flush",
   :doc
   "Flushes the output stream that is the current value of\n*out*",
   :var-type "function",
   :line 3728,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/flush",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3728",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "fn",
   :doc
   "params => positional-params*, or positional-params* & rest-param\npositional-param => binding-form\nrest-param => binding-form\nbinding-form => name, or destructuring-form\n\nDefines a function.\n\nSee https://clojure.org/reference/special_forms#fn for more information",
   :var-type "special form",
   :line 4652,
   :added "1.0",
   :forms
   [(fn name? [params*] exprs*) (fn name? ([params*] exprs*) +)],
   :namespace "clojure.core",
   :arglists ([& sigs]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/fn",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4652",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "fn?",
   :doc
   "Returns true if x implements Fn, i.e. is an object created via fn.",
   :var-type "function",
   :line 6376,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/fn?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6376",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "fnext",
   :doc "Same as (first (next x))",
   :var-type "function",
   :line 114,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/fnext",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L114",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "fnil",
   :doc
   "Takes a function f, and returns a function that calls f, replacing\na nil first argument to f with the supplied value x. Higher arity\nversions can replace arguments in the second and third\npositions (y, z). Note that the function f can take any number of\narguments, not just the one(s) being nil-patched.",
   :var-type "function",
   :line 6718,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([f x] [f x y] [f x y z]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/fnil",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6718",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "for",
   :doc
   "List comprehension. Takes a vector of one or more\n binding-form/collection-expr pairs, each followed by zero or more\n modifiers, and yields a lazy sequence of evaluations of expr.\n Collections are iterated in a nested fashion, rightmost fastest,\n and nested coll-exprs can refer to bindings created in prior\n binding-forms.  Supported modifiers are: :let [binding-form expr ...],\n :while test, :when test.\n\n(take 100 (for [x (range 100000000) y (range 1000000) :while (< y x)] [x y]))",
   :var-type "macro",
   :line 4765,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([seq-exprs body-expr]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/for",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4765",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "force",
   :doc
   "If x is a Delay, returns the (possibly cached) value of its expression, else returns x",
   :var-type "function",
   :line 763,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/force",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L763",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "format",
   :doc
   "Formats a string using java.lang.String.format, see java.util.Formatter for format\nstring syntax",
   :var-type "function",
   :line 5874,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([fmt & args]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/format",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5874",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "frequencies",
   :doc
   "Returns a map from distinct items in coll to the number of times\nthey appear.",
   :var-type "function",
   :line 7433,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/frequencies",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7433",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "future",
   :doc
   "Takes a body of expressions and yields a future object that will\ninvoke the body in another thread, and will cache the result and\nreturn it on all subsequent calls to deref/@. If the computation has\nnot yet finished, calls to deref/@ will block, unless the variant of\nderef with timeout is used. See also - realized?.",
   :var-type "macro",
   :line 7219,
   :added "1.1",
   :namespace "clojure.core",
   :arglists ([& body]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/future",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7219",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "future-call",
   :doc
   "Takes a function of no args and yields a future object that will\ninvoke the function in another thread, and will cache the result and\nreturn it on all subsequent calls to deref/@. If the computation has\nnot yet finished, calls to deref/@ will block, unless the variant\nof deref with timeout is used. See also - realized?.",
   :var-type "function",
   :line 7192,
   :added "1.1",
   :namespace "clojure.core",
   :arglists ([f]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/future-call",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7192",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "future-cancel",
   :doc "Cancels the future, if possible.",
   :var-type "function",
   :line 7229,
   :added "1.1",
   :namespace "clojure.core",
   :arglists ([f]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/future-cancel",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7229",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "future-cancelled?",
   :doc "Returns true if future f is cancelled",
   :var-type "function",
   :line 7235,
   :added "1.1",
   :namespace "clojure.core",
   :arglists ([f]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/future-cancelled?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7235",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "future-done?",
   :doc "Returns true if future f is done",
   :var-type "function",
   :line 6698,
   :added "1.1",
   :namespace "clojure.core",
   :arglists ([f]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/future-done?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6698",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "future?",
   :doc "Returns true if x is a future",
   :var-type "function",
   :line 6692,
   :added "1.1",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/future?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6692",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "gen-class",
   :doc
   "When compiling, generates compiled bytecode for a class with the\ngiven package-qualified :name (which, as all names in these\nparameters, can be a string or symbol), and writes the .class file\nto the *compile-path* directory.  When not compiling, does\nnothing. The gen-class construct contains no implementation, as the\nimplementation will be dynamically sought by the generated class in\nfunctions in an implementing Clojure namespace. Given a generated\nclass org.mydomain.MyClass with a method named mymethod, gen-class\nwill generate an implementation that looks for a function named by \n(str prefix mymethod) (default prefix: \"-\") in a\nClojure namespace specified by :impl-ns\n(defaults to the current namespace). All inherited methods,\ngenerated methods, and init and main functions (see :methods, :init,\nand :main below) will be found similarly prefixed. By default, the\nstatic initializer for the generated class will attempt to load the\nClojure support code for the class as a resource from the classpath,\ne.g. in the example case, ``org/mydomain/MyClass__init.class``. This\nbehavior can be controlled by :load-impl-ns\n\nNote that methods with a maximum of 18 parameters are supported.\n\nIn all subsequent sections taking types, the primitive types can be\nreferred to by their Java names (int, float etc), and classes in the\njava.lang package can be used without a package qualifier. All other\nclasses must be fully qualified.\n\nOptions should be a set of key/value pairs, all except for :name are optional:\n\n:name aname\n\nThe package-qualified name of the class to be generated\n\n:extends aclass\n\nSpecifies the superclass, the non-private methods of which will be\noverridden by the class. If not provided, defaults to Object.\n\n:implements [interface ...]\n\nOne or more interfaces, the methods of which will be implemented by the class.\n\n:init name\n\nIf supplied, names a function that will be called with the arguments\nto the constructor. Must return [ [superclass-constructor-args] state] \nIf not supplied, the constructor args are passed directly to\nthe superclass constructor and the state will be nil\n\n:constructors {[param-types] [super-param-types], ...}\n\nBy default, constructors are created for the generated class which\nmatch the signature(s) of the constructors for the superclass. This\nparameter may be used to explicitly specify constructors, each entry\nproviding a mapping from a constructor signature to a superclass\nconstructor signature. When you supply this, you must supply an :init\nspecifier. \n\n:post-init name\n\nIf supplied, names a function that will be called with the object as\nthe first argument, followed by the arguments to the constructor.\nIt will be called every time an object of this class is created,\nimmediately after all the inherited constructors have completed.\nIts return value is ignored.\n\n:methods [ [name [param-types] return-type], ...]\n\nThe generated class automatically defines all of the non-private\nmethods of its superclasses/interfaces. This parameter can be used\nto specify the signatures of additional methods of the generated\nclass. Static methods can be specified with ^{:static true} in the\nsignature's metadata. Do not repeat superclass/interface signatures\nhere.\n\n:main boolean\n\nIf supplied and true, a static public main function will be generated. It will\npass each string of the String[] argument as a separate argument to\na function called (str prefix main).\n\n:factory name\n\nIf supplied, a (set of) public static factory function(s) will be\ncreated with the given name, and the same signature(s) as the\nconstructor(s).\n\n:state name\n\nIf supplied, a public final instance field with the given name will be\ncreated. You must supply an :init function in order to provide a\nvalue for the state. Note that, though final, the state can be a ref\nor agent, supporting the creation of Java objects with transactional\nor asynchronous mutation semantics.\n\n:exposes {protected-field-name {:get name :set name}, ...}\n\nSince the implementations of the methods of the generated class\noccur in Clojure functions, they have no access to the inherited\nprotected fields of the superclass. This parameter can be used to\ngenerate public getter/setter methods exposing the protected field(s)\nfor use in the implementation.\n\n:exposes-methods {super-method-name exposed-name, ...}\n\nIt is sometimes necessary to call the superclass' implementation of an\noverridden method.  Those methods may be exposed and referred in \nthe new method implementation by a local name.\n\n:prefix string\n\nDefault: \"-\" Methods called e.g. Foo will be looked up in vars called\nprefixFoo in the implementing ns.\n\n:impl-ns name\n\nDefault: the name of the current ns. Implementations of methods will be \nlooked up in this namespace.\n\n:load-impl-ns boolean\n\nDefault: true. Causes the static initializer for the generated class\nto reference the load code for the implementing namespace. Should be\ntrue when implementing-ns is the default, false if you intend to\nload the code via some other method.",
   :var-type "macro",
   :line 518,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([& options]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/gen-class",
   :source-url
   "https://github.com/clojure/clojure/blob/641933ea85ea83d475aaeaa2303779bd29223fa1/src/clj/clojure/genclass.clj#L518",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/641933ea85ea83d475aaeaa2303779bd29223fa1/src/clj/clojure/genclass.clj",
   :file "src/clj/clojure/genclass.clj"}
  {:name "gen-interface",
   :doc
   "When compiling, generates compiled bytecode for an interface with\n the given package-qualified :name (which, as all names in these\n parameters, can be a string or symbol), and writes the .class file\n to the *compile-path* directory.  When not compiling, does nothing.\n\n In all subsequent sections taking types, the primitive types can be\n referred to by their Java names (int, float etc), and classes in the\n java.lang package can be used without a package qualifier. All other\n classes must be fully qualified.\n\n Options should be a set of key/value pairs, all except for :name are\n optional:\n\n :name aname\n\n The package-qualified name of the class to be generated\n\n :extends [interface ...]\n\n One or more interfaces, which will be extended by this interface.\n\n :methods [ [name [param-types] return-type], ...]\n\n This parameter is used to specify the signatures of the methods of\n the generated interface.  Do not repeat superinterface signatures\n here.",
   :var-type "macro",
   :line 700,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([& options]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/gen-interface",
   :source-url
   "https://github.com/clojure/clojure/blob/641933ea85ea83d475aaeaa2303779bd29223fa1/src/clj/clojure/genclass.clj#L700",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/641933ea85ea83d475aaeaa2303779bd29223fa1/src/clj/clojure/genclass.clj",
   :file "src/clj/clojure/genclass.clj"}
  {:name "gensym",
   :doc
   "Returns a new symbol with a unique name. If a prefix string is\nsupplied, the name is prefix# where # is some unique number. If\nprefix is not supplied, the prefix is 'G__'.",
   :var-type "function",
   :line 606,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([] [prefix-string]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/gensym",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L606",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "get",
   :doc
   "Returns the value mapped to key, not-found or nil if key not present\nin associative collection, set, string, array, or ILookup instance.",
   :var-type "function",
   :line 1512,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([map key] [map key not-found]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/get",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1512",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "get-in",
   :doc
   "Returns the value in a nested associative structure,\nwhere ks is a sequence of keys. Returns nil if the key\nis not present, or the not-found value if supplied.",
   :var-type "function",
   :line 6288,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([m ks] [m ks not-found]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/get-in",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6288",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "get-method",
   :doc
   "Given a multimethod and a dispatch value, returns the dispatch fn\nthat would apply to that value, or nil if none apply and no default",
   :var-type "function",
   :line 1831,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([multifn dispatch-val]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/get-method",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1831",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "get-proxy-class",
   :doc
   "Takes an optional single class followed by zero or more\ninterfaces. If not supplied class defaults to Object.  Creates an\nreturns an instance of a proxy class derived from the supplied\nclasses. The resulting value is cached and used for any subsequent\nrequests for the same class set. Returns a Class object.",
   :var-type "function",
   :line 281,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([& bases]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/get-proxy-class",
   :source-url
   "https://github.com/clojure/clojure/blob/658693f6cf97e6ab0ff789e096c9eb6654e4d3ab/src/clj/clojure/core_proxy.clj#L281",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/658693f6cf97e6ab0ff789e096c9eb6654e4d3ab/src/clj/clojure/core_proxy.clj",
   :file "src/clj/clojure/core_proxy.clj"}
  {:name "get-thread-bindings",
   :doc
   "Get a map with the Var/value pairs which is currently in effect for the\ncurrent thread.",
   :var-type "function",
   :line 1953,
   :added "1.1",
   :namespace "clojure.core",
   :arglists ([]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/get-thread-bindings",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1953",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "get-validator",
   :doc "Gets the validator-fn for a var/ref/agent/atom.",
   :var-type "function",
   :line 2414,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([iref]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/get-validator",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2414",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "group-by",
   :doc
   "Returns a map of the elements of coll keyed by the result of\nf on each element. The value at each key will be a vector of the\ncorresponding elements, in the order they appeared in coll.",
   :var-type "function",
   :line 7376,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([f coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/group-by",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7376",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "halt-when",
   :doc
   "Returns a transducer that ends transduction when pred returns true\nfor an input. When retf is supplied it must be a fn of 2 arguments -\nit will be passed the (completed) result so far and the input that\ntriggered the predicate, and its return value (if it does not throw\nan exception) will be the return value of the transducer. If retf\nis not supplied, the input that triggered the predicate will be\nreturned. If the predicate never returns true the transduction is\nunaffected.",
   :var-type "function",
   :line 7905,
   :added "1.9",
   :namespace "clojure.core",
   :arglists ([pred] [pred retf]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/halt-when",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7905",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "hash",
   :doc
   "Returns the hash code of its argument. Note this is the hash code\nconsistent with =, and thus is different than .hashCode for Integer,\nShort, Byte and Clojure collections.",
   :var-type "function",
   :line 5301,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/hash",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5301",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "hash-map",
   :doc
   "keyval => key val\nReturns a new hash map with supplied mappings.  If any keys are\nequal, they are handled as if by repeated uses of assoc.",
   :var-type "function",
   :line 381,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([] [& keyvals]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/hash-map",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L381",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "hash-ordered-coll",
   :doc
   "Returns the hash code, consistent with =, for an external ordered\ncollection implementing Iterable.\nSee http://clojure.org/data_structures#hash for full algorithms.",
   :var-type "function",
   :line 5322,
   :added "1.6",
   :namespace "clojure.core",
   :arglists ([coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/hash-ordered-coll",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5322",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "hash-set",
   :doc
   "Returns a new hash set with supplied keys.  Any equal keys are\nhandled as if by repeated uses of conj.",
   :var-type "function",
   :line 391,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([] [& keys]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/hash-set",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L391",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "hash-unordered-coll",
   :doc
   "Returns the hash code, consistent with =, for an external unordered\ncollection implementing Iterable. For maps, the iterator should\nreturn map entries whose hash is computed as\n  (hash-ordered-coll [k v]).\nSee http://clojure.org/data_structures#hash for full algorithms.",
   :var-type "function",
   :line 5331,
   :added "1.6",
   :namespace "clojure.core",
   :arglists ([coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/hash-unordered-coll",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5331",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "ident?",
   :doc "Return true if x is a symbol or keyword",
   :var-type "function",
   :line 1624,
   :added "1.9",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ident?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1624",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "identical?",
   :doc "Tests if 2 arguments are the same object",
   :var-type "function",
   :line 777,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x y]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/identical?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L777",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "identity",
   :doc "Returns its argument.",
   :var-type "function",
   :line 1469,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/identity",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1469",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "if",
   :doc
   "Evaluates test. If not the singular values nil or false,\nevaluates and yields then, otherwise, evaluates and yields else. If\nelse is not supplied it defaults to nil.\n\nPlease see https://clojure.org/reference/special_forms#if",
   :var-type "special form",
   :added "1.0",
   :forms [(if test then else?)],
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/if",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "if-let",
   :doc
   "bindings => binding-form test\n\nIf test is true, evaluates then with binding-form bound to the value of \ntest, if not, yields else",
   :var-type "macro",
   :line 1855,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([bindings then] [bindings then else & oldform]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/if-let",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1855",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "if-not",
   :doc
   "Evaluates test. If logical false, evaluates and returns then expr, \notherwise else expr, if supplied, else nil.",
   :var-type "macro",
   :line 769,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([test then] [test then else]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/if-not",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L769",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "if-some",
   :doc
   "bindings => binding-form test\n\nIf test is not nil, evaluates then with binding-form bound to the\nvalue of test, if not, yields else",
   :var-type "macro",
   :line 1890,
   :added "1.6",
   :namespace "clojure.core",
   :arglists ([bindings then] [bindings then else & oldform]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/if-some",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1890",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "ifn?",
   :doc
   "Returns true if x implements IFn. Note that many data structures\n(e.g. sets and maps) implement IFn",
   :var-type "function",
   :line 6369,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ifn?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6369",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "import",
   :doc
   "import-list => (package-symbol class-name-symbols*)\n\nFor each name in class-name-symbols, adds a mapping from name to the\nclass named by package.name to the current namespace. Use :import in the ns\nmacro in preference to calling this directly.",
   :var-type "macro",
   :line 3448,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([& import-symbols-or-lists]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/import",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3448",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "in-ns",
   :doc
   "Sets *ns* to the namespace named by the symbol, creating it if needed.",
   :var-type "function",
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([name]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/in-ns",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "inc",
   :doc
   "Returns a number one greater than num. Does not auto-promote\nlongs, will throw on overflow. See also: inc'",
   :var-type "function",
   :line 924,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/inc",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L924",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "inc'",
   :doc
   "Returns a number one greater than num. Supports arbitrary precision.\nSee also: inc",
   :var-type "function",
   :line 917,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/inc'",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L917",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "indexed?",
   :doc
   "Return true if coll implements Indexed, indicating efficient lookup by index",
   :var-type "function",
   :line 6423,
   :added "1.9",
   :namespace "clojure.core",
   :arglists ([coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/indexed?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6423",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "infinite?",
   :doc
   "Returns true if num is negative or positive infinity, else false",
   :var-type "function",
   :line 8305,
   :added "1.11",
   :namespace "clojure.core",
   :arglists ([num]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/infinite?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L8305",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "init-proxy",
   :doc
   "Takes a proxy instance and a map of strings (which must\ncorrespond to methods of the proxy superclass/superinterfaces) to\nfns (which must take arguments matching the corresponding method,\nplus an additional (explicit) first arg corresponding to this, and\nsets the proxy's fn map.  Returns the proxy.",
   :var-type "function",
   :line 302,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([proxy mappings]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/init-proxy",
   :source-url
   "https://github.com/clojure/clojure/blob/658693f6cf97e6ab0ff789e096c9eb6654e4d3ab/src/clj/clojure/core_proxy.clj#L302",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/658693f6cf97e6ab0ff789e096c9eb6654e4d3ab/src/clj/clojure/core_proxy.clj",
   :file "src/clj/clojure/core_proxy.clj"}
  {:name "inst-ms",
   :doc
   "Return the number of milliseconds since January 1, 1970, 00:00:00 GMT",
   :var-type "function",
   :line 6985,
   :added "1.9",
   :namespace "clojure.core",
   :arglists ([inst]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/inst-ms",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6985",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "inst?",
   :doc "Return true if x satisfies Inst",
   :var-type "function",
   :line 6991,
   :added "1.9",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/inst?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6991",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "instance?",
   :doc
   "Evaluates x and tests if it is an instance of the class\nc. Returns true or false",
   :var-type "function",
   :line 141,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([c x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/instance?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L141",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "int",
   :doc "Coerce to int",
   :var-type "function",
   :line 884,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/int",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L884",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "int-array",
   :doc "Creates an array of ints",
   :var-type "function",
   :line 5468,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([size-or-seq] [size init-val-or-seq]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/int-array",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5468",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "int?",
   :doc "Return true if x is a fixed precision integer",
   :var-type "function",
   :line 1414,
   :added "1.9",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/int?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1414",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "integer?",
   :doc "Returns true if n is an integer",
   :var-type "function",
   :line 1388,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([n]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/integer?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1388",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "interleave",
   :doc
   "Returns a lazy seq of the first item in each coll, then the second etc.",
   :var-type "function",
   :line 4332,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([] [c1] [c1 c2] [c1 c2 & colls]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/interleave",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4332",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "intern",
   :doc
   "Finds or creates a var named by the symbol name in the namespace\nns (which can be a symbol or a namespace), setting its root binding\nto val if supplied. The namespace must exist. The var will adopt any\nmetadata from the name symbol.  Returns the var.",
   :var-type "function",
   :line 6471,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([ns name] [ns name val]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/intern",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6471",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "interpose",
   :doc
   "Returns a lazy seq of the elements of coll separated by sep.\nReturns a stateful transducer when no collection is provided.",
   :var-type "function",
   :line 5342,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([sep] [sep coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/interpose",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5342",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "into",
   :doc
   "Returns a new coll consisting of to with all of the items of\nfrom conjoined. A transducer may be supplied.\n(into x) returns x. (into) returns [].",
   :var-type "function",
   :line 7098,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([] [to] [to from] [to xform from]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/into",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7098",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "into-array",
   :doc
   "Returns an array with components set to the values in aseq. The array's\ncomponent type is type if provided, or the type of the first value in\naseq if present, or Object. All values in aseq must be compatible with\nthe component type. Class objects for the primitive types can be obtained\nusing, e.g., Integer/TYPE.",
   :var-type "function",
   :line 3466,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([aseq] [type aseq]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/into-array",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3466",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "ints",
   :doc "Casts to int[]",
   :var-type "function",
   :line 5509,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([xs]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ints",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5509",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "io!",
   :doc
   "If an io! block occurs in a transaction, throws an\nIllegalStateException, else runs body in an implicit do. If the\nfirst expression in body is a literal string, will use that as the\nexception message.",
   :var-type "macro",
   :line 2526,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([& body]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/io!",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2526",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "isa?",
   :doc
   "Returns true if (= child parent), or child is directly or indirectly derived from\nparent, either via a Java type inheritance relationship or a\nrelationship established via derive. h must be a hierarchy obtained\nfrom make-hierarchy, if not supplied defaults to the global\nhierarchy",
   :var-type "function",
   :line 5700,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([child parent] [h child parent]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/isa?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5700",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "iterate",
   :doc
   "Returns a lazy (infinite!) sequence of x, (f x), (f (f x)) etc.\nf must be free of side-effects",
   :var-type "function",
   :line 3033,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([f x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/iterate",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3033",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "iteration",
   :doc
   "Creates a seqable/reducible via repeated calls to step,\na function of some (continuation token) 'k'. The first call to step\nwill be passed initk, returning 'ret'. Iff (somef ret) is true,\n(vf ret) will be included in the iteration, else iteration will\nterminate and vf/kf will not be called. If (kf ret) is non-nil it\nwill be passed to the next step call, else iteration will terminate.\n\nThis can be used e.g. to consume APIs that return paginated or batched data.\n\n step - (possibly impure) fn of 'k' -> 'ret'\n\n :somef - fn of 'ret' -> logical true/false, default 'some?'\n :vf - fn of 'ret' -> 'v', a value produced by the iteration, default 'identity'\n :kf - fn of 'ret' -> 'next-k' or nil (signaling 'do not continue'), default 'identity'\n :initk - the first value passed to step, default 'nil'\n\nIt is presumed that step with non-initk is unreproducible/non-idempotent.\nIf step with initk is unreproducible it is on the consumer to not consume twice.",
   :var-type "function",
   :line 7992,
   :added "1.11",
   :namespace "clojure.core",
   :arglists
   ([step
     &
     {:keys [somef vf kf initk],
      :or {vf identity, kf identity, somef some?, initk nil}}]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/iteration",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7992",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "iterator-seq",
   :doc
   "Returns a seq on a java.util.Iterator. Note that most collections\nproviding iterators implement Iterable and thus support seq directly.\nSeqs cache values, thus iterator-seq should not be used on any\niterator that repeatedly returns the same mutable object.",
   :var-type "function",
   :line 5857,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([iter]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/iterator-seq",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5857",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "juxt",
   :doc
   "Takes a set of functions and returns a fn that is the juxtaposition\nof those fns.  The returned fn takes a variable number of args, and\nreturns a vector containing the result of applying each fn to the\nargs (left-to-right).\n((juxt a b c) x) => [(a x) (b x) (c x)]",
   :var-type "function",
   :line 2590,
   :added "1.1",
   :namespace "clojure.core",
   :arglists ([f] [f g] [f g h] [f g h & fs]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/juxt",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2590",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "keep",
   :doc
   "Returns a lazy sequence of the non-nil results of (f item). Note,\nthis means false return values will be included.  f must be free of\nside-effects.  Returns a transducer when no collection is provided.",
   :var-type "function",
   :line 7587,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([f] [f coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/keep",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7587",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "keep-indexed",
   :doc
   "Returns a lazy sequence of the non-nil results of (f index item). Note,\nthis means false return values will be included.  f must be free of\nside-effects.  Returns a stateful transducer when no collection is\nprovided.",
   :var-type "function",
   :line 7620,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([f] [f coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/keep-indexed",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7620",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "key",
   :doc "Returns the key of the map entry.",
   :var-type "function",
   :line 1579,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([e]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/key",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1579",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "keys",
   :doc
   "Returns a sequence of the map's keys, in the same order as (seq map).",
   :var-type "function",
   :line 1567,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([map]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/keys",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1567",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "keyword",
   :doc
   "Returns a Keyword with the given namespace and name.  Do not use :\nin the keyword strings, it will be added automatically.",
   :var-type "function",
   :line 616,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([name] [ns name]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/keyword",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L616",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "keyword?",
   :doc "Return true if x is a Keyword",
   :var-type "function",
   :line 570,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/keyword?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L570",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "last",
   :doc "Return the last item in coll, in linear time",
   :var-type "function",
   :line 264,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/last",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L264",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "lazy-cat",
   :doc
   "Expands to code which yields a lazy sequence of the concatenation\nof the supplied colls.  Each coll expr is not evaluated until it is\nneeded. \n\n(lazy-cat xs ys zs) === (concat (lazy-seq xs) (lazy-seq ys) (lazy-seq zs))",
   :var-type "macro",
   :line 4755,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([& colls]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/lazy-cat",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4755",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "lazy-seq",
   :doc
   "Takes a body of expressions that returns an ISeq or nil, and yields\na Seqable object that will invoke the body only the first time seq\nis called, and will cache the result and return it on all subsequent\nseq calls. See also - realized?",
   :var-type "macro",
   :line 685,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([& body]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/lazy-seq",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L685",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "let",
   :doc
   "binding => binding-form init-expr\nbinding-form => name, or destructuring-form\ndestructuring-form => map-destructure-form, or seq-destructure-form\n\nEvaluates the exprs in a lexical context in which the symbols in\nthe binding-forms are bound to their respective init-exprs or parts\ntherein.\n\nSee https://clojure.org/reference/special_forms#binding-forms for\nmore information about destructuring.",
   :var-type "special form",
   :line 4615,
   :added "1.0",
   :forms [(let [bindings*] exprs*)],
   :namespace "clojure.core",
   :arglists ([bindings & body]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/let",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4615",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "letfn",
   :doc
   "fnspec ==> (fname [params*] exprs) or (fname ([params*] exprs)+)\n\nTakes a vector of function specs and a body, and generates a set of\nbindings of functions to their names. All of the names are available\nin all of the definitions of the functions, as well as the body.",
   :var-type "special form",
   :line 6705,
   :added "1.0",
   :forms [(letfn [fnspecs*] exprs*)],
   :namespace "clojure.core",
   :arglists ([fnspecs & body]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/letfn",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6705",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "line-seq",
   :doc
   "Returns the lines of text from rdr as a lazy sequence of strings.\nrdr must implement java.io.BufferedReader.",
   :var-type "function",
   :line 3090,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([rdr]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/line-seq",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3090",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "list",
   :doc "Creates a new list containing the items.",
   :var-type "function",
   :line 16,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([& items]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/list",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L16",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "list*",
   :doc
   "Creates a new seq containing the items prepended to the rest, the\nlast of which will be treated as a sequence.",
   :var-type "function",
   :line 650,
   :added "1.0",
   :namespace "clojure.core",
   :arglists
   ([args] [a args] [a b args] [a b c args] [a b c d & more]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/list*",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L650",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "list?",
   :doc "Returns true if x implements IPersistentList",
   :var-type "function",
   :line 6358,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/list?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6358",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "load",
   :doc
   "Loads Clojure code from resources in classpath. A path is interpreted as\nclasspath-relative if it begins with a slash or relative to the root\ndirectory for the current namespace otherwise.",
   :var-type "function",
   :line 6255,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([& paths]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/load",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6255",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "load-file",
   :doc
   "Sequentially read and evaluate the set of forms contained in the file.",
   :var-type "function",
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([name]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/load-file",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "load-reader",
   :doc
   "Sequentially read and evaluate the set of forms contained in the\nstream/file",
   :var-type "function",
   :line 4105,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([rdr]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/load-reader",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4105",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "load-string",
   :doc
   "Sequentially read and evaluate the set of forms contained in the\nstring",
   :var-type "function",
   :line 4112,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([s]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/load-string",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4112",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "loaded-libs",
   :doc
   "Returns a sorted set of symbols naming the currently loaded libs",
   :var-type "function",
   :line 6250,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/loaded-libs",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6250",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "locking",
   :doc
   "Executes exprs in an implicit do, while holding the monitor of x.\nWill release the monitor of x in all circumstances.",
   :var-type "macro",
   :line 1659,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x & body]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/locking",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1659",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "long",
   :doc "Coerce to long",
   :var-type "function",
   :line 3503,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/long",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3503",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "long-array",
   :doc "Creates an array of longs",
   :var-type "function",
   :line 5476,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([size-or-seq] [size init-val-or-seq]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/long-array",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5476",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "longs",
   :doc "Casts to long[]",
   :var-type "function",
   :line 5519,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([xs]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/longs",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5519",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "loop",
   :doc
   "Evaluates the exprs in a lexical context in which the symbols in\nthe binding-forms are bound to their respective init-exprs or parts\ntherein. Acts as a recur target.",
   :var-type "special form",
   :line 4716,
   :added "1.0",
   :forms [(loop [bindings*] exprs*)],
   :namespace "clojure.core",
   :arglists ([bindings & body]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/loop",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4716",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "macroexpand",
   :doc
   "Repeatedly calls macroexpand-1 on form until it no longer\nrepresents a macro form, then returns it.  Note neither\nmacroexpand-1 nor macroexpand expand macros in subforms.",
   :var-type "function",
   :line 4049,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([form]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/macroexpand",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4049",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "macroexpand-1",
   :doc
   "If form represents a macro form, returns its expansion,\nelse returns form.",
   :var-type "function",
   :line 4041,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([form]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/macroexpand-1",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4041",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "make-array",
   :doc
   "Creates and returns an array of instances of the specified class of\nthe specified dimension(s).  Note that a class object is required.\nClass objects can be obtained by using their imported or\nfully-qualified name.  Class objects for the primitive types can be\nobtained using, e.g., Integer/TYPE.",
   :var-type "function",
   :line 4009,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([type len] [type dim & more-dims]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/make-array",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4009",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "make-hierarchy",
   :doc "Creates a hierarchy object for use with derive, isa? etc.",
   :var-type "function",
   :line 5664,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/make-hierarchy",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5664",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "map",
   :doc
   "Returns a lazy sequence consisting of the result of applying f to\nthe set of first items of each coll, followed by applying f to the\nset of second items in each coll, until any one of the colls is\nexhausted.  Any remaining items in other colls are ignored. Function\nf should accept number-of-colls arguments. Returns a transducer when\nno collection is provided.",
   :var-type "function",
   :line 2741,
   :added "1.0",
   :namespace "clojure.core",
   :arglists
   ([f] [f coll] [f c1 c2] [f c1 c2 c3] [f c1 c2 c3 & colls]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/map",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2741",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "map-entry?",
   :doc "Return true if x is a map entry",
   :var-type "function",
   :line 1496,
   :added "1.8",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/map-entry?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1496",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "map-indexed",
   :doc
   "Returns a lazy sequence consisting of the result of applying f to 0\nand the first item of coll, followed by applying f to 1 and the second\nitem in coll, etc, until coll is exhausted. Thus function f should\naccept 2 arguments, index and item. Returns a stateful transducer when\nno collection is provided.",
   :var-type "function",
   :line 7557,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([f] [f coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/map-indexed",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7557",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "map?",
   :doc "Return true if x implements IPersistentMap",
   :var-type "function",
   :line 169,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/map?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L169",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "mapcat",
   :doc
   "Returns the result of applying concat to the result of applying map\nto f and colls.  Thus function f should return a collection. Returns\na transducer when no collections are provided",
   :var-type "function",
   :line 2797,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([f] [f & colls]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/mapcat",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2797",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "mapv",
   :doc
   "Returns a vector consisting of the result of applying f to the\nset of first items of each coll, followed by applying f to the set\nof second items in each coll, until any one of the colls is\nexhausted.  Any remaining items in other colls are ignored. Function\nf should accept number-of-colls arguments.",
   :var-type "function",
   :line 7119,
   :added "1.4",
   :namespace "clojure.core",
   :arglists ([f coll] [f c1 c2] [f c1 c2 c3] [f c1 c2 c3 & colls]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/mapv",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7119",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "max",
   :doc "Returns the greatest of the nums.",
   :var-type "function",
   :line 1117,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x] [x y] [x y & more]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/max",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1117",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "max-key",
   :doc
   "Returns the x for which (k x), a number, is greatest.\n\nIf there are multiple such xs, the last one is returned.",
   :var-type "function",
   :line 5134,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([k x] [k x y] [k x y & more]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/max-key",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5134",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "memfn",
   :doc
   "Expands into code that creates a fn that expects to be passed an\nobject and any args and calls the named instance method on the\nobject passing the args. Use when you want to treat a Java method as\na first-class fn. name may be type-hinted with the method receiver's\ntype in order to avoid reflective calls.",
   :var-type "macro",
   :line 3894,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([name & args]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/memfn",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3894",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "memoize",
   :doc
   "Returns a memoized version of a referentially transparent function. The\nmemoized version of the function keeps a cache of the mapping from arguments\nto results and, when calls with the same arguments are repeated often, has\nhigher performance at the expense of higher memory use.",
   :var-type "function",
   :line 6497,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([f]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/memoize",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6497",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "merge",
   :doc
   "Returns a map that consists of the rest of the maps conj-ed onto\nthe first.  If a key occurs in more than one map, the mapping from\nthe latter (left-to-right) will be the mapping in the result.",
   :var-type "function",
   :line 3062,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([& maps]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/merge",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3062",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "merge-with",
   :doc
   "Returns a map that consists of the rest of the maps conj-ed onto\nthe first.  If a key occurs in more than one map, the mapping(s)\nfrom the latter (left-to-right) will be combined with the mapping in\nthe result by calling (f val-in-result val-in-latter).",
   :var-type "function",
   :line 3072,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([f & maps]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/merge-with",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3072",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "meta",
   :doc
   "Returns the metadata of obj, returns nil if there is no metadata.",
   :var-type "function",
   :line 204,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([obj]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/meta",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L204",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "methods",
   :doc
   "Given a multimethod, returns a map of dispatch values -> dispatch fns",
   :var-type "function",
   :line 1825,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([multifn]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/methods",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1825",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "min",
   :doc "Returns the least of the nums.",
   :var-type "function",
   :line 1127,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x] [x y] [x y & more]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/min",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1127",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "min-key",
   :doc
   "Returns the x for which (k x), a number, is least.\n\nIf there are multiple such xs, the last one is returned.",
   :var-type "function",
   :line 5154,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([k x] [k x y] [k x y & more]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/min-key",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5154",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "mix-collection-hash",
   :doc
   "Mix final collection hash for ordered or unordered collections.\nhash-basis is the combined collection hash, count is the number\nof elements included in the basis. Note this is the hash code\nconsistent with =, different from .hashCode.\nSee http://clojure.org/data_structures#hash for full algorithms.",
   :var-type "function",
   :line 5311,
   :added "1.6",
   :namespace "clojure.core",
   :arglists ([hash-basis count]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/mix-collection-hash",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5311",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "mod",
   :doc "Modulus of num and div. Truncates toward negative infinity.",
   :var-type "function",
   :line 3589,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([num div]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/mod",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3589",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "monitor-enter",
   :doc
   "Synchronization primitive that should be avoided\nin user code. Use the 'locking' macro.\n\nPlease see https://clojure.org/reference/special_forms#monitor-enter",
   :var-type "special form",
   :added "1.0",
   :forms [(monitor-enter x)],
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/monitor-enter",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "monitor-exit",
   :doc
   "Synchronization primitive that should be avoided\nin user code. Use the 'locking' macro.\n\nPlease see https://clojure.org/reference/special_forms#monitor-exit",
   :var-type "special form",
   :added "1.0",
   :forms [(monitor-exit x)],
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/monitor-exit",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "name",
   :doc "Returns the name String of a string, symbol or keyword.",
   :var-type "function",
   :line 1601,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/name",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1601",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "namespace",
   :doc
   "Returns the namespace String of a symbol or keyword, or nil if not present.",
   :var-type "function",
   :line 1609,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/namespace",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1609",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "namespace-munge",
   :doc
   "Convert a Clojure namespace name to a legal Java package name.",
   :var-type "function",
   :line 13,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([ns]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/namespace-munge",
   :source-url
   "https://github.com/clojure/clojure/blob/c1ce24118af6e596839be6decf2bdb0da53dc8ed/src/clj/clojure/core_deftype.clj#L13",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/c1ce24118af6e596839be6decf2bdb0da53dc8ed/src/clj/clojure/core_deftype.clj",
   :file "src/clj/clojure/core_deftype.clj"}
  {:name "nat-int?",
   :doc "Return true if x is a non-negative fixed precision integer",
   :var-type "function",
   :line 1434,
   :added "1.9",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/nat-int?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1434",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "neg-int?",
   :doc "Return true if x is a negative fixed precision integer",
   :var-type "function",
   :line 1428,
   :added "1.9",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/neg-int?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1428",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "neg?",
   :doc "Returns true if num is less than zero, else false",
   :var-type "function",
   :line 1268,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([num]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/neg?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1268",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "new",
   :doc
   "The args, if any, are evaluated from left to right, and\npassed to the constructor of the class named by Classname. The\nconstructed object is returned.\n\nPlease see https://clojure.org/java_interop#new",
   :var-type "special form",
   :added "1.0",
   :forms [(Classname. args*) (new Classname args*)],
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/new",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "newline",
   :doc "Writes a platform-specific newline to *out*",
   :var-type "function",
   :line 3720,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/newline",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3720",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "next",
   :doc
   "Returns a seq of the items after the first. Calls seq on its\nargument.  If there are no more items, returns nil.",
   :var-type "function",
   :line 57,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/next",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L57",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "nfirst",
   :doc "Same as (next (first x))",
   :var-type "function",
   :line 107,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/nfirst",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L107",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "nil?",
   :doc "Returns true if x is nil, false otherwise.",
   :var-type "function",
   :line 438,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/nil?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L438",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "nnext",
   :doc "Same as (next (next x))",
   :var-type "function",
   :line 121,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/nnext",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L121",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "not",
   :doc "Returns true if x is logical false, false otherwise.",
   :var-type "function",
   :line 526,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/not",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L526",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "not-any?",
   :doc
   "Returns false if (pred x) is logical true for any x in coll,\nelse true.",
   :var-type "function",
   :line 2717,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([pred coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/not-any?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2717",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "not-empty",
   :doc "If coll is empty, returns nil, else coll",
   :var-type "function",
   :line 5673,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/not-empty",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5673",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "not-every?",
   :doc
   "Returns false if (pred x) is logical true for every x in\ncoll, else true.",
   :var-type "function",
   :line 2698,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([pred coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/not-every?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2698",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "not=",
   :doc "Same as (not (= obj1 obj2))",
   :var-type "function",
   :line 821,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x] [x y] [x y & more]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/not=",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L821",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "ns",
   :doc
   "Sets *ns* to the namespace named by name (unevaluated), creating it\nif needed.  references can be zero or more of: (:refer-clojure ...)\n(:require ...) (:use ...) (:import ...) (:load ...) (:gen-class)\nwith the syntax of refer-clojure/require/use/import/load/gen-class\nrespectively, except the arguments are unevaluated and need not be\nquoted. (:gen-class ...), when supplied, defaults to :name\ncorresponding to the ns name, :main true, :impl-ns same as ns, and\n:init-impl-ns true. All options of gen-class are\nsupported. The :gen-class directive is ignored when not\ncompiling. If :gen-class is not supplied, when compiled only an\nnsname__init.class will be generated. If :refer-clojure is not used, a\ndefault (refer 'clojure.core) is used.  Use of ns is preferred to\nindividual calls to in-ns/require/use/import:\n\n(ns foo.bar\n  (:refer-clojure :exclude [ancestors printf])\n  (:require (clojure.contrib sql combinatorics))\n  (:use (my.lib this that))\n  (:import (java.util Date Timer Random)\n           (java.sql Connection Statement)))",
   :var-type "macro",
   :line 5900,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([name docstring? attr-map? references*]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ns",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5900",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "ns-aliases",
   :doc "Returns a map of the aliases for the namespace.",
   :var-type "function",
   :line 4297,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([ns]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ns-aliases",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4297",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "ns-imports",
   :doc "Returns a map of the import mappings for the namespace.",
   :var-type "function",
   :line 4223,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([ns]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ns-imports",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4223",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "ns-interns",
   :doc "Returns a map of the intern mappings for the namespace.",
   :var-type "function",
   :line 4230,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([ns]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ns-interns",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4230",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "ns-map",
   :doc "Returns a map of all the mappings for the namespace.",
   :var-type "function",
   :line 4194,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([ns]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ns-map",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4194",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "ns-name",
   :doc "Returns the name of the namespace, a symbol.",
   :var-type "function",
   :line 4187,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([ns]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ns-name",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4187",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "ns-publics",
   :doc
   "Returns a map of the public intern mappings for the namespace.",
   :var-type "function",
   :line 4212,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([ns]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ns-publics",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4212",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "ns-refers",
   :doc "Returns a map of the refer mappings for the namespace.",
   :var-type "function",
   :line 4277,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([ns]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ns-refers",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4277",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "ns-resolve",
   :doc
   "Returns the var or Class to which a symbol will be resolved in the\nnamespace (unless found in the environment), else nil.  Note that\nif the symbol is fully qualified, the var/Class to which it resolves\nneed not be present in the namespace.",
   :var-type "function",
   :line 4382,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([ns sym] [ns env sym]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ns-resolve",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4382",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "ns-unalias",
   :doc "Removes the alias for the symbol from the namespace.",
   :var-type "function",
   :line 4304,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([ns sym]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ns-unalias",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4304",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "ns-unmap",
   :doc "Removes the mappings for the symbol from the namespace.",
   :var-type "function",
   :line 4201,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([ns sym]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ns-unmap",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4201",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "nth",
   :doc
   "Returns the value at the index. get returns nil if index out of\nbounds, nth throws an exception unless not-found is supplied.  nth\nalso works for strings, Java arrays, regex Matchers and Lists, and,\nin O(n) time, for sequences.",
   :var-type "function",
   :line 891,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([coll index] [coll index not-found]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/nth",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L891",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "nthnext",
   :doc "Returns the nth next of coll, (seq coll) when n is 0.",
   :var-type "function",
   :line 3169,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([coll n]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/nthnext",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3169",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "nthrest",
   :doc "Returns the nth rest of coll, coll when n is 0.",
   :var-type "function",
   :line 3183,
   :added "1.3",
   :namespace "clojure.core",
   :arglists ([coll n]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/nthrest",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3183",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "num",
   :doc "Coerce to Number",
   :var-type "function",
   :line 3496,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/num",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3496",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "number?",
   :doc "Returns true if x is a Number",
   :var-type "function",
   :line 3582,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/number?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3582",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "numerator",
   :doc "Returns the numerator part of a Ratio.",
   :var-type "function",
   :line 3605,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([r]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/numerator",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3605",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "object-array",
   :doc "Creates an array of objects",
   :var-type "function",
   :line 5461,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([size-or-seq]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/object-array",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5461",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "odd?",
   :doc
   "Returns true if n is odd, throws an exception if n is not an integer",
   :var-type "function",
   :line 1408,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([n]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/odd?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1408",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "or",
   :doc
   "Evaluates exprs one at a time, from left to right. If a form\nreturns a logical true value, or returns that value and doesn't\nevaluate any of the other expressions, otherwise it returns the\nvalue of the last expression. (or) returns nil.",
   :var-type "macro",
   :line 856,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([] [x] [x & next]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/or",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L856",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "parents",
   :doc
   "Returns the immediate parents of tag, either via a Java type\ninheritance relationship or a relationship established via derive. h\nmust be a hierarchy obtained from make-hierarchy, if not supplied\ndefaults to the global hierarchy",
   :var-type "function",
   :line 5721,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([tag] [h tag]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/parents",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5721",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "parse-boolean",
   :doc
   "Parse strings \"true\" or \"false\" and return a boolean, or nil if invalid",
   :var-type "function",
   :line 8285,
   :added "1.11",
   :namespace "clojure.core",
   :arglists ([s]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/parse-boolean",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L8285",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "parse-double",
   :doc
   "Parse string with floating point components and return a Double value,\nor nil if parse fails.\n\nGrammar: https://docs.oracle.com/javase/8/docs/api/java/lang/Double.html#valueOf-java.lang.String-",
   :var-type "function",
   :line 8261,
   :added "1.11",
   :namespace "clojure.core",
   :arglists ([s]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/parse-double",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L8261",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "parse-long",
   :doc
   "Parse string of decimal digits with optional leading -/+ and return a\nLong value, or nil if parse fails",
   :var-type "function",
   :line 8250,
   :added "1.11",
   :namespace "clojure.core",
   :arglists ([s]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/parse-long",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L8250",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "parse-uuid",
   :doc
   "Parse a string representing a UUID and return a java.util.UUID instance,\nor nil if parse fails.\n\nGrammar: https://docs.oracle.com/javase/8/docs/api/java/util/UUID.html#toString--",
   :var-type "function",
   :line 8274,
   :added "1.11",
   :namespace "clojure.core",
   :arglists ([s]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/parse-uuid",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L8274",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "partial",
   :doc
   "Takes a function f and fewer than the normal arguments to f, and\nreturns a fn that takes a variable number of additional args. When\ncalled, the returned function calls f with args + additional args.",
   :var-type "function",
   :line 2628,
   :added "1.0",
   :namespace "clojure.core",
   :arglists
   ([f]
    [f arg1]
    [f arg1 arg2]
    [f arg1 arg2 arg3]
    [f arg1 arg2 arg3 & more]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/partial",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2628",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "partition",
   :doc
   "Returns a lazy sequence of lists of n items each, at offsets step\napart. If step is not supplied, defaults to n, i.e. the partitions\ndo not overlap. If a pad collection is supplied, use its elements as\nnecessary to complete last partition upto n items. In case there are\nnot enough padding elements, return a partition with less than n items.",
   :var-type "function",
   :line 3199,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([n coll] [n step coll] [n step pad coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/partition",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3199",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "partition-all",
   :doc
   "Returns a lazy sequence of lists like partition, but may include\npartitions with fewer than n items at the end.  Returns a stateful\ntransducer when no collection is provided.",
   :var-type "function",
   :line 7470,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([n] [n coll] [n step coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/partition-all",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7470",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "partition-by",
   :doc
   "Applies f to each value in coll, splitting it each time f returns a\nnew value.  Returns a lazy seq of partitions.  Returns a stateful\ntransducer when no collection is provided.",
   :var-type "function",
   :line 7390,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([f] [f coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/partition-by",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7390",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "partitionv",
   :doc
   "Returns a lazy sequence of vectors of n items each, at offsets step\napart. If step is not supplied, defaults to n, i.e. the partitions\ndo not overlap. If a pad collection is supplied, use its elements as\nnecessary to complete last partition upto n items. In case there are\nnot enough padding elements, return a partition with less than n items.",
   :var-type "function",
   :line 7510,
   :added "1.12",
   :namespace "clojure.core",
   :arglists ([n coll] [n step coll] [n step pad coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/partitionv",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7510",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "partitionv-all",
   :doc
   "Returns a lazy sequence of vector partitions, but may include\npartitions with fewer than n items at the end.\nReturns a stateful transducer when no collection is provided.",
   :var-type "function",
   :line 7533,
   :added "1.12",
   :namespace "clojure.core",
   :arglists ([n] [n coll] [n step coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/partitionv-all",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7533",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "pcalls",
   :doc
   "Executes the no-arg fns in parallel, returning a lazy sequence of\ntheir values",
   :var-type "function",
   :line 7266,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([& fns]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/pcalls",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7266",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "peek",
   :doc
   "For a list or queue, same as first, for a vector, same as, but much\nmore efficient than, last. If the collection is empty, returns nil.",
   :var-type "function",
   :line 1478,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/peek",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1478",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "persistent!",
   :doc
   "Returns a new, persistent version of the transient collection, in\nconstant time. The transient collection cannot be used after this\ncall, any such use will throw an exception.",
   :var-type "function",
   :line 3372,
   :added "1.1",
   :namespace "clojure.core",
   :arglists ([coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/persistent!",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3372",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "pmap",
   :doc
   "Like map, except f is applied in parallel. Semi-lazy in that the\nparallel computation stays ahead of the consumption, but doesn't\nrealize the entire result unless required. Only useful for\ncomputationally intensive functions where the time of f dominates\nthe coordination overhead.",
   :var-type "function",
   :line 7241,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([f coll] [f coll & colls]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/pmap",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7241",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "pop",
   :doc
   "For a list or queue, returns a new list/queue without the first\nitem, for a vector, returns a new vector without the last item. If\nthe collection is empty, throws an exception.  Note - not the same\nas next/butlast.",
   :var-type "function",
   :line 1485,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/pop",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1485",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "pop!",
   :doc
   "Removes the last item from a transient vector. If\nthe collection is empty, throws an exception. Returns coll",
   :var-type "function",
   :line 3415,
   :added "1.1",
   :namespace "clojure.core",
   :arglists ([coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/pop!",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3415",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "pop-thread-bindings",
   :doc
   "Pop one set of bindings pushed with push-binding before. It is an error to\npop bindings without pushing before.",
   :var-type "function",
   :line 1945,
   :added "1.1",
   :namespace "clojure.core",
   :arglists ([]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/pop-thread-bindings",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1945",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "pos-int?",
   :doc "Return true if x is a positive fixed precision integer",
   :var-type "function",
   :line 1422,
   :added "1.9",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/pos-int?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1422",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "pos?",
   :doc "Returns true if num is greater than zero, else false",
   :var-type "function",
   :line 1261,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([num]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/pos?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1261",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "pr",
   :doc
   "Prints the object(s) to the output stream that is the current value\nof *out*.  Prints the object(s), separated by spaces if there is\nmore than one.  By default, pr and prn print in a way that objects\ncan be read by the reader",
   :var-type "function",
   :line 3700,
   :added "1.0",
   :dynamic true,
   :namespace "clojure.core",
   :arglists ([] [x] [x & more]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/pr",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3700",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "pr-str",
   :doc "pr to a string, returning it",
   :var-type "function",
   :line 4877,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([& xs]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/pr-str",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4877",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "prefer-method",
   :doc
   "Causes the multimethod to prefer matches of dispatch-val-x over dispatch-val-y \nwhen there is a conflict",
   :var-type "function",
   :line 1817,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([multifn dispatch-val-x dispatch-val-y]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/prefer-method",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1817",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "prefers",
   :doc
   "Given a multimethod, returns a map of preferred value -> set of other values",
   :var-type "function",
   :line 1838,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([multifn]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/prefers",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1838",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "print",
   :doc
   "Prints the object(s) to the output stream that is the current value\nof *out*.  print and println produce output for human consumption.",
   :var-type "function",
   :line 3747,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([& more]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/print",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3747",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "print-str",
   :doc "print to a string, returning it",
   :var-type "function",
   :line 4895,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([& xs]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/print-str",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4895",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "printf",
   :doc "Prints formatted output, as per format",
   :var-type "function",
   :line 5882,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([fmt & args]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/printf",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5882",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "println",
   :doc "Same as print followed by (newline)",
   :var-type "function",
   :line 3756,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([& more]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/println",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3756",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "println-str",
   :doc "println to a string, returning it",
   :var-type "function",
   :line 4904,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([& xs]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/println-str",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4904",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "prn",
   :doc
   "Same as pr followed by (newline). Observes *flush-on-newline*",
   :var-type "function",
   :line 3737,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([& more]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/prn",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3737",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "prn-str",
   :doc "prn to a string, returning it",
   :var-type "function",
   :line 4886,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([& xs]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/prn-str",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4886",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "promise",
   :doc
   "Returns a promise object that can be read with deref/@, and set,\nonce only, with deliver. Calls to deref/@ prior to delivery will\nblock, unless the variant of deref with timeout is used. All\nsubsequent derefs will return the same delivered value without\nblocking. See also - realized?.",
   :var-type "function",
   :line 7326,
   :added "1.1",
   :namespace "clojure.core",
   :arglists ([]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/promise",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7326",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "proxy",
   :doc
   "class-and-interfaces - a vector of class names\n\nargs - a (possibly empty) vector of arguments to the superclass\nconstructor.\n\nf => (name [params*] body) or\n(name ([params*] body) ([params+] body) ...)\n\nExpands to code which creates a instance of a proxy class that\nimplements the named class/interface(s) by calling the supplied\nfns. A single class, if provided, must be first. If not provided it\ndefaults to Object.\n\nThe interfaces names must be valid interface types. If a method fn\nis not provided for a class method, the superclass method will be\ncalled. If a method fn is not provided for an interface method, an\nUnsupportedOperationException will be thrown should it be\ncalled. Method fns are closures and can capture the environment in\nwhich proxy is called. Each method fn takes an additional implicit\nfirst arg, which is bound to 'this. Note that while method fns can\nbe provided to override protected methods, they have no other access\nto protected members, nor to super, as these capabilities cannot be\nproxied.",
   :var-type "macro",
   :line 334,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([class-and-interfaces args & fs]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/proxy",
   :source-url
   "https://github.com/clojure/clojure/blob/658693f6cf97e6ab0ff789e096c9eb6654e4d3ab/src/clj/clojure/core_proxy.clj#L334",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/658693f6cf97e6ab0ff789e096c9eb6654e4d3ab/src/clj/clojure/core_proxy.clj",
   :file "src/clj/clojure/core_proxy.clj"}
  {:name "proxy-mappings",
   :doc "Takes a proxy instance and returns the proxy's fn map.",
   :var-type "function",
   :line 328,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([proxy]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/proxy-mappings",
   :source-url
   "https://github.com/clojure/clojure/blob/658693f6cf97e6ab0ff789e096c9eb6654e4d3ab/src/clj/clojure/core_proxy.clj#L328",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/658693f6cf97e6ab0ff789e096c9eb6654e4d3ab/src/clj/clojure/core_proxy.clj",
   :file "src/clj/clojure/core_proxy.clj"}
  {:name "proxy-super",
   :doc
   "Use to call a superclass method in the body of a proxy method. \nNote, expansion captures 'this",
   :var-type "macro",
   :line 396,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([meth & args]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/proxy-super",
   :source-url
   "https://github.com/clojure/clojure/blob/658693f6cf97e6ab0ff789e096c9eb6654e4d3ab/src/clj/clojure/core_proxy.clj#L396",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/658693f6cf97e6ab0ff789e096c9eb6654e4d3ab/src/clj/clojure/core_proxy.clj",
   :file "src/clj/clojure/core_proxy.clj"}
  {:name "push-thread-bindings",
   :doc
   "WARNING: This is a low-level function. Prefer high-level macros like\nbinding where ever possible.\n\nTakes a map of Var/value pairs. Binds each Var to the associated value for\nthe current thread. Each call *MUST* be accompanied by a matching call to\npop-thread-bindings wrapped in a try-finally!\n\n    (push-thread-bindings bindings)\n    (try\n      ...\n      (finally\n        (pop-thread-bindings)))",
   :var-type "function",
   :line 1927,
   :added "1.1",
   :namespace "clojure.core",
   :arglists ([bindings]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/push-thread-bindings",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1927",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "pvalues",
   :doc
   "Returns a lazy sequence of the values of the exprs, which are\nevaluated in parallel",
   :var-type "macro",
   :line 7273,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([& exprs]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/pvalues",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7273",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "qualified-ident?",
   :doc "Return true if x is a symbol or keyword with a namespace",
   :var-type "function",
   :line 1634,
   :added "1.9",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/qualified-ident?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1634",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "qualified-keyword?",
   :doc "Return true if x is a keyword with a namespace",
   :var-type "function",
   :line 1654,
   :added "1.9",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/qualified-keyword?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1654",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "qualified-symbol?",
   :doc "Return true if x is a symbol with a namespace",
   :var-type "function",
   :line 1644,
   :added "1.9",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/qualified-symbol?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1644",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "quot",
   :doc "quot[ient] of dividing numerator by denominator.",
   :var-type "function",
   :line 1275,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([num div]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/quot",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1275",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "quote",
   :doc
   "Yields the unevaluated form.\n\nPlease see https://clojure.org/reference/special_forms#quote",
   :var-type "special form",
   :added "1.0",
   :forms ['form],
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/quote",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "rand",
   :doc
   "Returns a random floating point number between 0 (inclusive) and\nn (default 1) (exclusive).",
   :var-type "function",
   :line 5056,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([] [n]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/rand",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5056",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "rand-int",
   :doc
   "Returns a random integer between 0 (inclusive) and n (exclusive).",
   :var-type "function",
   :line 5064,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([n]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/rand-int",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5064",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "rand-nth",
   :doc
   "Return a random element of the (sequential) collection. Will have\nthe same performance characteristics as nth for the given\ncollection.",
   :var-type "function",
   :line 7461,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/rand-nth",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7461",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "random-sample",
   :doc
   "Returns items from coll with random probability of prob (0.0 -\n1.0).  Returns a transducer when no collection is provided.",
   :var-type "function",
   :line 7947,
   :added "1.7",
   :namespace "clojure.core",
   :arglists ([prob] [prob coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/random-sample",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7947",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "random-uuid",
   :doc
   "Returns a pseudo-randomly generated java.util.UUID instance (i.e. type 4).\n\nSee: https://docs.oracle.com/javase/8/docs/api/java/util/UUID.html#randomUUID--",
   :var-type "function",
   :line 7008,
   :added "1.11",
   :namespace "clojure.core",
   :arglists ([]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/random-uuid",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7008",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "range",
   :doc
   "Returns a lazy seq of nums from start (inclusive) to end\n(exclusive), by step, where start defaults to 0, step to 1, and end to\ninfinity. When step is equal to 0, returns an infinite sequence of\nstart. When start is equal to end, returns empty list.",
   :var-type "function",
   :line 3040,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([] [end] [start end] [start end step]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/range",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3040",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "ratio?",
   :doc "Returns true if n is a Ratio",
   :var-type "function",
   :line 3599,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([n]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ratio?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3599",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "rational?",
   :doc "Returns true if n is a rational number",
   :var-type "function",
   :line 3635,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([n]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/rational?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3635",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "rationalize",
   :doc "returns the rational value of num",
   :var-type "function",
   :line 1291,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([num]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/rationalize",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1291",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "re-find",
   :doc
   "Returns the next regex match, if any, of string to pattern, using\njava.util.regex.Matcher.find().  Uses re-groups to return the\ngroups.",
   :var-type "function",
   :line 5043,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([m] [re s]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/re-find",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5043",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "re-groups",
   :doc
   "Returns the groups from the most recent match/find. If there are no\nnested groups, returns a string of the entire match. If there are\nnested groups, returns a vector of the groups, the first element\nbeing the entire match.",
   :var-type "function",
   :line 5003,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([m]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/re-groups",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5003",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "re-matcher",
   :doc
   "Returns an instance of java.util.regex.Matcher, for use, e.g. in\nre-find.",
   :var-type "function",
   :line 4994,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([re s]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/re-matcher",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4994",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "re-matches",
   :doc
   "Returns the match, if any, of string to pattern, using\njava.util.regex.Matcher.matches().  Uses re-groups to return the\ngroups.",
   :var-type "function",
   :line 5031,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([re s]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/re-matches",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5031",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "re-pattern",
   :doc
   "Returns an instance of java.util.regex.Pattern, for use, e.g. in\nre-matcher.",
   :var-type "function",
   :line 4984,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([s]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/re-pattern",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4984",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "re-seq",
   :doc
   "Returns a lazy sequence of successive matches of pattern in string,\nusing java.util.regex.Matcher.find(), each such match processed with\nre-groups.",
   :var-type "function",
   :line 5019,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([re s]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/re-seq",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5019",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "read",
   :doc
   "Reads the next object from stream, which must be an instance of\njava.io.PushbackReader or some derivee.  stream defaults to the\ncurrent value of *in*.\n\nOpts is a persistent map with valid keys:\n  :read-cond - :allow to process reader conditionals, or\n               :preserve to keep all branches\n  :features - persistent set of feature keywords for reader conditionals\n  :eof - on eof, return value unless :eofthrow, then throw.\n         if not specified, will throw\n\nNote that read can execute code (controlled by *read-eval*),\nand as such should be used only with trusted sources.\n\nFor data structure interop use clojure.edn/read",
   :var-type "function",
   :line 3764,
   :added "1.0",
   :namespace "clojure.core",
   :arglists
   ([]
    [stream]
    [stream eof-error? eof-value]
    [stream eof-error? eof-value recursive?]
    [opts stream]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/read",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3764",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "read+string",
   :doc
   "Like read, and taking the same args. stream must be a LineNumberingPushbackReader.\nReturns a vector containing the object read and the (whitespace-trimmed) string read.",
   :var-type "function",
   :line 3793,
   :added "1.10",
   :namespace "clojure.core",
   :arglists
   ([]
    [stream]
    [stream eof-error? eof-value]
    [stream eof-error? eof-value recursive?]
    [opts stream]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/read+string",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3793",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "read-line",
   :doc
   "Reads the next line from stream that is the current value of *in* .",
   :var-type "function",
   :line 3819,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/read-line",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3819",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "read-string",
   :doc
   "Reads one object from the string s. Optionally include reader\noptions, as specified in read.\n\nNote that read-string can execute code (controlled by *read-eval*),\nand as such should be used only with trusted sources.\n\nFor data structure interop use clojure.edn/read-string",
   :var-type "function",
   :line 3828,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([s] [opts s]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/read-string",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3828",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "reader-conditional",
   :doc
   "Construct a data representation of a reader conditional.\nIf true, splicing? indicates read-cond-splicing.",
   :var-type "function",
   :line 8058,
   :added "1.7",
   :namespace "clojure.core",
   :arglists ([form splicing?]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/reader-conditional",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L8058",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "reader-conditional?",
   :doc
   "Return true if the value is the data representation of a reader conditional",
   :var-type "function",
   :line 8052,
   :added "1.7",
   :namespace "clojure.core",
   :arglists ([value]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/reader-conditional?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L8052",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "realized?",
   :doc
   "Returns true if a value has been produced for a promise, delay, future or lazy sequence.",
   :var-type "function",
   :line 7807,
   :added "1.3",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/realized?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7807",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "record?",
   :doc "Returns true if x is a record",
   :var-type "function",
   :line 420,
   :added "1.6",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/record?",
   :source-url
   "https://github.com/clojure/clojure/blob/c1ce24118af6e596839be6decf2bdb0da53dc8ed/src/clj/clojure/core_deftype.clj#L420",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/c1ce24118af6e596839be6decf2bdb0da53dc8ed/src/clj/clojure/core_deftype.clj",
   :file "src/clj/clojure/core_deftype.clj"}
  {:name "recur",
   :doc
   "Evaluates the exprs in order, then, in parallel, rebinds\nthe bindings of the recursion point to the values of the exprs.\nExecution then jumps back to the recursion point, a loop or fn method.\n\nPlease see https://clojure.org/reference/special_forms#recur",
   :var-type "special form",
   :added "1.0",
   :forms [(recur exprs*)],
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/recur",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "reduce",
   :doc
   "f should be a function of 2 arguments. If val is not supplied,\nreturns the result of applying f to the first 2 items in coll, then\napplying f to that result and the 3rd item, etc. If coll contains no\nitems, f must accept no arguments as well, and reduce returns the\nresult of calling f with no arguments.  If coll has only 1 item, it\nis returned and f is not called.  If val is supplied, returns the\nresult of applying f to val and the first item in coll, then\napplying f to that result and the 2nd item, etc. If coll contains no\nitems, returns val and f is not called.",
   :var-type "function",
   :line 7016,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([f coll] [f val coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/reduce",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7016",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "reduce-kv",
   :doc
   "Reduces an associative collection. f should be a function of 3\narguments. Returns the result of applying f to init, the first key\nand the first value in coll, then applying f to that result and the\n2nd key and value, etc. If coll contains no entries, returns init\nand f is not called. Note that reduce-kv is supported on vectors,\nwhere the keys will be the ordinals.",
   :var-type "function",
   :line 7058,
   :added "1.4",
   :namespace "clojure.core",
   :arglists ([f init coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/reduce-kv",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7058",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "reduced",
   :doc
   "Wraps x in a way such that a reduce will terminate with the value x",
   :var-type "function",
   :line 2850,
   :added "1.5",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/reduced",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2850",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "reduced?",
   :doc "Returns true if x is the result of a call to reduced",
   :var-type "function",
   :line 2856,
   :added "1.5",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/reduced?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2856",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "reductions",
   :doc
   "Returns a lazy seq of the intermediate values of the reduction (as\nper reduce) of coll by f, starting with init.",
   :var-type "function",
   :line 7444,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([f coll] [f init coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/reductions",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7444",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "ref",
   :doc
   "Creates and returns a Ref with an initial value of x and zero or\nmore options (in any order):\n\n:meta metadata-map\n\n:validator validate-fn\n\n:min-history (default 0)\n:max-history (default 10)\n\nIf metadata-map is supplied, it will become the metadata on the\nref. validate-fn must be nil or a side-effect-free fn of one\nargument, which will be passed the intended new state on any state\nchange. If the new state is unacceptable, the validate-fn should\nreturn false or throw an exception. validate-fn will be called on\ntransaction commit, when all refs have their final values.\n\nNormally refs accumulate history dynamically as needed to deal with\nread demands. If you know in advance you will need history you can\nset :min-history to ensure it will be available when first needed (instead\nof after a read fault). History is limited, and the limit can be set\nwith :max-history.",
   :var-type "function",
   :line 2276,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x] [x & options]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ref",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2276",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "ref-history-count",
   :doc "Returns the history count of a ref",
   :var-type "function",
   :line 2477,
   :added "1.1",
   :namespace "clojure.core",
   :arglists ([ref]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ref-history-count",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2477",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "ref-max-history",
   :doc
   "Gets the max-history of a ref, or sets it and returns the ref",
   :var-type "function",
   :line 2493,
   :added "1.1",
   :namespace "clojure.core",
   :arglists ([ref] [ref n]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ref-max-history",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2493",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "ref-min-history",
   :doc
   "Gets the min-history of a ref, or sets it and returns the ref",
   :var-type "function",
   :line 2484,
   :added "1.1",
   :namespace "clojure.core",
   :arglists ([ref] [ref n]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ref-min-history",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2484",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "ref-set",
   :doc
   "Must be called in a transaction. Sets the value of ref.\nReturns val.",
   :var-type "function",
   :line 2469,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([ref val]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ref-set",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2469",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "refer",
   :doc
   "refers to all public vars of ns, subject to filters.\nfilters can include at most one each of:\n\n:exclude list-of-symbols\n:only list-of-symbols\n:rename map-of-fromsymbol-tosymbol\n\nFor each public interned var in the namespace named by the symbol,\nadds a mapping from the name of the var to the var to the current\nnamespace.  Throws an exception if name is already mapped to\nsomething else in the current namespace. Filters can be used to\nselect a subset, via inclusion or exclusion, or to provide a mapping\nto a symbol different from the var's name, in order to prevent\nclashes. Use :use in the ns macro in preference to calling this directly.",
   :var-type "function",
   :line 4240,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([ns-sym & filters]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/refer",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4240",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "refer-clojure",
   :doc "Same as (refer 'clojure.core <filters>)",
   :var-type "macro",
   :line 5958,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([& filters]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/refer-clojure",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5958",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "reify",
   :doc
   "reify creates an object implementing a protocol or interface.\n reify is a macro with the following structure:\n\n(reify options* specs*)\n \n Currently there are no options.\n\n Each spec consists of the protocol or interface name followed by zero\n or more method bodies:\n\n protocol-or-interface-or-Object\n (methodName [args+] body)*\n\n Methods should be supplied for all methods of the desired\n protocol(s) and interface(s). You can also define overrides for\n methods of Object. Note that the first parameter must be supplied to\n correspond to the target object ('this' in Java parlance). Thus\n methods for interfaces will take one more argument than do the\n interface declarations.  Note also that recur calls to the method\n head should *not* pass the target object, it will be supplied\n automatically and can not be substituted.\n\n The return type can be indicated by a type hint on the method name,\n and arg types can be indicated by a type hint on arg names. If you\n leave out all hints, reify will try to match on same name/arity\n method in the protocol(s)/interface(s) - this is preferred. If you\n supply any hints at all, no inference is done, so all hints (or\n default of Object) must be correct, for both arguments and return\n type. If a method is overloaded in a protocol/interface, multiple\n independent method definitions must be supplied.  If overloaded with\n same arity in an interface you must specify complete hints to\n disambiguate - a missing hint implies Object.\n\n Method heads are recursion points for recur, as in a fn. The method\n bodies of reify are lexical closures, and can refer to the surrounding\n local scope:\n \n (str (let [f \"foo\"] \n        (reify Object\n          (toString [this] f))))\n == \"foo\"\n\n (seq (let [f \"foo\"] \n        (reify clojure.lang.Seqable\n          (seq [this] (seq f)))))\n == (\\f \\o \\o)\n \n reify always implements clojure.lang.IObj and transfers meta\n data of the form to the created object.\n \n (meta ^{:k :v} (reify Object (toString [this] \"foo\")))\n == {:k :v}",
   :var-type "macro",
   :line 83,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([& opts+specs]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/reify",
   :source-url
   "https://github.com/clojure/clojure/blob/c1ce24118af6e596839be6decf2bdb0da53dc8ed/src/clj/clojure/core_deftype.clj#L83",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/c1ce24118af6e596839be6decf2bdb0da53dc8ed/src/clj/clojure/core_deftype.clj",
   :file "src/clj/clojure/core_deftype.clj"}
  {:name "release-pending-sends",
   :doc
   "Normally, actions sent directly or indirectly during another action\nare held until the action completes (changes the agent's\nstate). This function can be used to dispatch any pending sent\nactions immediately. This has no impact on actions sent during a\ntransaction, which are still held until commit. If no action is\noccurring, does nothing. Returns the number of actions dispatched.",
   :var-type "function",
   :line 2147,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/release-pending-sends",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2147",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "rem",
   :doc "remainder of dividing numerator by denominator.",
   :var-type "function",
   :line 1283,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([num div]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/rem",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1283",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "remove",
   :doc
   "Returns a lazy sequence of the items in coll for which\n(pred item) returns logical false. pred must be free of side-effects.\nReturns a transducer when no collection is provided.",
   :var-type "function",
   :line 2840,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([pred] [pred coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/remove",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2840",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "remove-all-methods",
   :doc "Removes all of the methods of multimethod.",
   :var-type "function",
   :line 1803,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([multifn]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/remove-all-methods",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1803",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "remove-method",
   :doc
   "Removes the method of multimethod associated with dispatch-value.",
   :var-type "function",
   :line 1810,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([multifn dispatch-val]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/remove-method",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1810",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "remove-ns",
   :doc
   "Removes the namespace named by the symbol. Use with caution.\nCannot be used to remove the clojure namespace.",
   :var-type "function",
   :line 4163,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([sym]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/remove-ns",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4163",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "remove-tap",
   :doc "Remove f from the tap set.",
   :var-type "function",
   :line 8199,
   :added "1.10",
   :namespace "clojure.core",
   :arglists ([f]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/remove-tap",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L8199",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "remove-watch",
   :doc "Removes a watch (set by add-watch) from a reference",
   :var-type "function",
   :line 2176,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([reference key]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/remove-watch",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2176",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "repeat",
   :doc
   "Returns a lazy (infinite!, or length n if supplied) sequence of xs.",
   :var-type "function",
   :line 3019,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x] [n x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/repeat",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3019",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "repeatedly",
   :doc
   "Takes a function of no args, presumably with side effects, and\nreturns an infinite (or length n if supplied) lazy sequence of calls\nto it",
   :var-type "function",
   :line 4428,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([f] [n f]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/repeatedly",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4428",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "replace",
   :doc
   "Given a map of replacement pairs and a vector/collection, returns a\nvector/seq with any elements = a key in smap replaced with the\ncorresponding val in smap.  Returns a transducer when no collection\nis provided.",
   :var-type "function",
   :line 5203,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([smap] [smap coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/replace",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5203",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "replicate",
   :doc
   "DEPRECATED: Use 'repeat' instead.\nReturns a lazy seq of n xs.",
   :var-type "function",
   :line 3026,
   :added "1.0",
   :deprecated "1.3",
   :namespace "clojure.core",
   :arglists ([n x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/replicate",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3026",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "req!",
   :doc "Like arity-2 'get', but throws if key not present.",
   :var-type "function",
   :line 1523,
   :added "1.13",
   :namespace "clojure.core",
   :arglists ([m k]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/req!",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1523",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "require",
   :doc
   "Loads libs, skipping any that are already loaded. Each argument is\neither a libspec that identifies a lib, a prefix list that identifies\nmultiple libs whose names share a common prefix, or a flag that modifies\nhow all the identified libs are loaded. Use :require in the ns macro\nin preference to calling this directly.\n\nLibs\n\nA 'lib' is a named set of resources in classpath whose contents define a\nlibrary of Clojure code. Lib names are symbols and each lib is associated\nwith a Clojure namespace and a Java package that share its name. A lib's\nname also locates its root directory within classpath using Java's\npackage name to classpath-relative path mapping. All resources in a lib\nshould be contained in the directory structure under its root directory.\nAll definitions a lib makes should be in its associated namespace.\n\n'require loads a lib by loading its root resource. The root resource path\nis derived from the lib name in the following manner:\nConsider a lib named by the symbol 'x.y.z; it has the root directory\n<classpath>/x/y/, and its root resource is <classpath>/x/y/z.clj, or\n<classpath>/x/y/z.cljc if <classpath>/x/y/z.clj does not exist. The\nroot resource should contain code to create the lib's\nnamespace (usually by using the ns macro) and load any additional\nlib resources.\n\nLibspecs\n\nA libspec is a lib name or a vector containing a lib name followed by\noptions expressed as sequential keywords and arguments.\n\nRecognized options:\n:as takes a symbol as its argument and makes that symbol an alias to the\n  lib's namespace in the current namespace.\n:as-alias takes a symbol as its argument and aliases like :as, however\n  the lib will not be loaded. If the lib has not been loaded, a new\n  empty namespace will be created (as with create-ns).\n:refer takes a list of symbols to refer from the namespace or the :all\n  keyword to bring in all public vars.\n\nPrefix Lists\n\nIt's common for Clojure code to depend on several libs whose names have\nthe same prefix. When specifying libs, prefix lists can be used to reduce\nrepetition. A prefix list contains the shared prefix followed by libspecs\nwith the shared prefix removed from the lib names. After removing the\nprefix, the names that remain must not contain any periods.\n\nFlags\n\nA flag is a keyword.\nRecognized flags: :reload, :reload-all, :verbose\n:reload forces loading of all the identified libs even if they are\n  already loaded (has no effect on libspecs using :as-alias)\n:reload-all implies :reload and also forces loading of all libs that the\n  identified libs directly or indirectly load via require or use\n  (has no effect on libspecs using :as-alias)\n:verbose triggers printing information about each load, alias, and refer\n\nExample:\n\nThe following would load the libraries clojure.zip and clojure.set\nabbreviated as 's'.\n\n(require '(clojure zip [set :as s]))",
   :var-type "function",
   :line 6149,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([& args]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/require",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6149",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "requiring-resolve",
   :doc
   "Resolves namespace-qualified sym per 'resolve'. If initial resolve\nfails, attempts to require sym's namespace and retries.",
   :var-type "function",
   :line 6228,
   :added "1.10",
   :namespace "clojure.core",
   :arglists ([sym]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/requiring-resolve",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6228",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "reset!",
   :doc
   "Sets the value of atom to newval without regard for the\ncurrent value. Returns newval.",
   :var-type "function",
   :line 2390,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([atom newval]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/reset!",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2390",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "reset-meta!",
   :doc
   "Atomically resets the metadata for a namespace/var/ref/agent/atom",
   :var-type "function",
   :line 2430,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([iref metadata-map]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/reset-meta!",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2430",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "reset-vals!",
   :doc
   "Sets the value of atom to newval. Returns [old new], the value of the\natom before and after the reset.",
   :var-type "function",
   :line 2397,
   :added "1.9",
   :namespace "clojure.core",
   :arglists ([atom newval]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/reset-vals!",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2397",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "resolve",
   :doc
   "same as (ns-resolve *ns* symbol) or (ns-resolve *ns* &env symbol)",
   :var-type "function",
   :line 4395,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([sym] [env sym]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/resolve",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4395",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "rest",
   :doc
   "Returns a possibly empty seq of the items after the first. Calls seq on its\nargument.",
   :var-type "function",
   :line 66,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/rest",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L66",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "restart-agent",
   :doc
   "When an agent is failed, changes the agent state to new-state and\nthen un-fails the agent so that sends are allowed again.  If\na :clear-actions true option is given, any actions queued on the\nagent that were being held while it was failed will be discarded,\notherwise those held actions will proceed.  The new-state must pass\nthe validator if any, or restart will throw an exception and the\nagent will remain failed with its old state and error.  Watchers, if\nany, will NOT be notified of the new state.  Throws an exception if\nthe agent is not failed.",
   :var-type "function",
   :line 2191,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([a new-state & options]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/restart-agent",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2191",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "resultset-seq",
   :doc
   "Creates and returns a lazy sequence of structmaps corresponding to\nthe rows in the java.sql.ResultSet rs",
   :var-type "function",
   :line 5838,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([rs]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/resultset-seq",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5838",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "reverse",
   :doc
   "Returns a seq of the items in coll in reverse order. Not lazy.",
   :var-type "function",
   :line 949,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/reverse",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L949",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "reversible?",
   :doc "Returns true if coll implements Reversible",
   :var-type "function",
   :line 6417,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/reversible?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6417",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "rseq",
   :doc
   "Returns, in constant time, a seq of the items in rev (which\ncan be a vector or sorted-map), in reverse order. If rev is empty returns nil",
   :var-type "function",
   :line 1593,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([rev]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/rseq",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1593",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "rsubseq",
   :doc
   "sc must be a sorted collection, test(s) one of <, <=, > or\n>=. Returns a reverse seq of those entries with keys ek for\nwhich (test (.. sc comparator (compare ek key)) 0) is true",
   :var-type "function",
   :line 5271,
   :added "1.0",
   :namespace "clojure.core",
   :arglists
   ([sc test key] [sc start-test start-key end-test end-key]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/rsubseq",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5271",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "run!",
   :doc
   "Runs the supplied procedure (via reduce), for purposes of side\neffects, on successive items in the collection. Returns nil",
   :var-type "function",
   :line 7984,
   :added "1.7",
   :namespace "clojure.core",
   :arglists ([proc coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/run!",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7984",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "satisfies?",
   :doc "Returns true if x satisfies the protocol",
   :var-type "function",
   :line 584,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([protocol x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/satisfies?",
   :source-url
   "https://github.com/clojure/clojure/blob/c1ce24118af6e596839be6decf2bdb0da53dc8ed/src/clj/clojure/core_deftype.clj#L584",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/c1ce24118af6e596839be6decf2bdb0da53dc8ed/src/clj/clojure/core_deftype.clj",
   :file "src/clj/clojure/core_deftype.clj"}
  {:name "second",
   :doc "Same as (first (next x))",
   :var-type "function",
   :line 93,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/second",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L93",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "select-keys",
   :doc
   "Returns a map containing only those entries in map whose key is in keyseq",
   :var-type "function",
   :line 7148,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([map keyseq]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/select-keys",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7148",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "send",
   :doc
   "Dispatch an action to an agent. Returns the agent immediately.\nSubsequently, in a thread from a thread pool, the state of the agent\nwill be set to the value of:\n\n(apply action-fn state-of-agent args)",
   :var-type "function",
   :line 2125,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([a f & args]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/send",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2125",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "send-off",
   :doc
   "Dispatch a potentially blocking action to an agent. Returns the\nagent immediately. Subsequently, in a separate thread, the state of\nthe agent will be set to the value of:\n\n(apply action-fn state-of-agent args)",
   :var-type "function",
   :line 2136,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([a f & args]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/send-off",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2136",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "send-via",
   :doc
   "Dispatch an action to an agent. Returns the agent immediately.\nSubsequently, in a thread supplied by executor, the state of the agent\nwill be set to the value of:\n\n(apply action-fn state-of-agent args)",
   :var-type "function",
   :line 2115,
   :added "1.5",
   :namespace "clojure.core",
   :arglists ([executor a f & args]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/send-via",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2115",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "seq",
   :doc
   "Returns a seq on the collection. If the collection is\nempty, returns nil.  (seq nil) returns nil. seq also works on\nStrings, native Java arrays (of reference types) and any objects\nthat implement Iterable. Note that seqs cache values, thus seq\nshould not be used on any Iterable whose iterator repeatedly\nreturns the same mutable object.",
   :var-type "function",
   :line 128,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/seq",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L128",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "seq-to-map-for-destructuring",
   :doc
   "Builds a map from a seq as described in\nhttps://clojure.org/reference/special_forms#keyword-arguments",
   :var-type "function",
   :line 4449,
   :added "1.11",
   :namespace "clojure.core",
   :arglists ([s]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/seq-to-map-for-destructuring",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4449",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "seq?",
   :doc "Return true if x implements ISeq",
   :var-type "function",
   :line 148,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/seq?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L148",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "seqable?",
   :doc "Return true if the seq function is supported for x",
   :var-type "function",
   :line 6364,
   :added "1.9",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/seqable?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6364",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "seque",
   :doc
   "Creates a queued seq on another (presumably lazy) seq s. The queued\nseq will produce a concrete seq in the background, and can get up to\nn items ahead of the consumer. n-or-q can be an integer n buffer\nsize, or an instance of java.util.concurrent BlockingQueue. Note\nthat reading from a seque can block if the reader gets ahead of the\nproducer.",
   :var-type "function",
   :line 5533,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([s] [n-or-q s]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/seque",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5533",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "sequence",
   :doc
   "Coerces coll to a (possibly empty) sequence, if it is not already\none. Will not force a lazy seq. (sequence nil) yields (), When a\ntransducer is supplied, returns a lazy sequence of applications of\nthe transform to the items in coll(s), i.e. to the set of first\nitems of each coll, followed by the set of second\nitems in each coll, until any one of the colls is exhausted.  Any\nremaining items in other colls are ignored. The transform should accept\nnumber-of-colls arguments",
   :var-type "function",
   :line 2661,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([coll] [xform coll] [xform coll & colls]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/sequence",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2661",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "sequential?",
   :doc "Returns true if coll implements Sequential",
   :var-type "function",
   :line 6389,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/sequential?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6389",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "set",
   :doc "Returns a set of the distinct elements of coll.",
   :var-type "function",
   :line 4128,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/set",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4128",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "set!",
   :doc
   "Used to set thread-local-bound vars, Java object instance\nfields, and Java class static fields.\n\nPlease see https://clojure.org/vars#set",
   :var-type "special form",
   :added "1.0",
   :forms
   [(set! var-symbol expr)
    (set! (. instance-expr instanceFieldName-symbol) expr)
    (set! (. Classname-symbol staticFieldName-symbol) expr)],
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/set!",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "set-agent-send-executor!",
   :doc "Sets the ExecutorService to be used by send",
   :var-type "function",
   :line 2103,
   :added "1.5",
   :namespace "clojure.core",
   :arglists ([executor]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/set-agent-send-executor!",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2103",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "set-agent-send-off-executor!",
   :doc "Sets the ExecutorService to be used by send-off",
   :var-type "function",
   :line 2109,
   :added "1.5",
   :namespace "clojure.core",
   :arglists ([executor]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/set-agent-send-off-executor!",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2109",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "set-error-handler!",
   :doc
   "Sets the error-handler of agent a to handler-fn.  If an action\nbeing run by the agent throws an exception or doesn't pass the\nvalidator fn, handler-fn will be called with two arguments: the\nagent and the exception.",
   :var-type "function",
   :line 2208,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([a handler-fn]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/set-error-handler!",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2208",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "set-error-mode!",
   :doc
   "Sets the error-mode of agent a to mode-keyword, which must be\neither :fail or :continue.  If an action being run by the agent\nthrows an exception or doesn't pass the validator fn, an\nerror-handler may be called (see set-error-handler!), after which,\nif the mode is :continue, the agent will continue as if neither the\naction that caused the error nor the error itself ever happened.\n\nIf the mode is :fail, the agent will become failed and will stop\naccepting new 'send' and 'send-off' actions, and any previously\nqueued actions will be held until a 'restart-agent'.  Deref will\nstill work, returning the state of the agent before the error.",
   :var-type "function",
   :line 2226,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([a mode-keyword]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/set-error-mode!",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2226",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "set-validator!",
   :doc
   "Sets the validator-fn for a var/ref/agent/atom. validator-fn must be nil or a\nside-effect-free fn of one argument, which will be passed the intended\nnew state on any state change. If the new state is unacceptable, the\nvalidator-fn should return false or throw an exception. If the current state (root\nvalue if var) is not acceptable to the new validator, an exception\nwill be thrown and the validator will not be changed.",
   :var-type "function",
   :line 2403,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([iref validator-fn]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/set-validator!",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2403",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "set?",
   :doc "Returns true if x implements IPersistentSet",
   :var-type "function",
   :line 4122,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/set?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4122",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "short",
   :doc "Coerce to short",
   :var-type "function",
   :line 3521,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/short",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3521",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "short-array",
   :doc "Creates an array of shorts",
   :var-type "function",
   :line 5445,
   :added "1.1",
   :namespace "clojure.core",
   :arglists ([size-or-seq] [size init-val-or-seq]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/short-array",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5445",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "shorts",
   :doc "Casts to short[]",
   :var-type "function",
   :line 5499,
   :added "1.1",
   :namespace "clojure.core",
   :arglists ([xs]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/shorts",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5499",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "shuffle",
   :doc "Return a random permutation of coll",
   :var-type "function",
   :line 7548,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/shuffle",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7548",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "shutdown-agents",
   :doc
   "Initiates a shutdown of the thread pools that back the agent\nsystem. Running actions will complete, but no new actions will be\naccepted",
   :var-type "function",
   :line 2268,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/shutdown-agents",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2268",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "simple-ident?",
   :doc "Return true if x is a symbol or keyword without a namespace",
   :var-type "function",
   :line 1629,
   :added "1.9",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/simple-ident?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1629",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "simple-keyword?",
   :doc "Return true if x is a keyword without a namespace",
   :var-type "function",
   :line 1649,
   :added "1.9",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/simple-keyword?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1649",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "simple-symbol?",
   :doc "Return true if x is a symbol without a namespace",
   :var-type "function",
   :line 1639,
   :added "1.9",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/simple-symbol?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1639",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "slurp",
   :doc
   "Opens a reader on f and reads all its contents, returning a string.\nSee clojure.java.io/reader for a complete list of supported arguments.",
   :var-type "function",
   :line 7171,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([f & opts]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/slurp",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7171",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "some",
   :doc
   "Returns the first logical true value of (pred x) for any x in coll,\nelse nil.  One common idiom is to use a set as pred, for example\nthis will return :fred if :fred is in the sequence, otherwise nil:\n(some #{:fred} coll)",
   :var-type "function",
   :line 2706,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([pred coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/some",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2706",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "some->",
   :doc
   "When expr is not nil, threads it into the first form (via ->),\nand when that result is not nil, through the next etc",
   :var-type "macro",
   :line 7858,
   :added "1.5",
   :namespace "clojure.core",
   :arglists ([expr & forms]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/some->",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7858",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "some->>",
   :doc
   "When expr is not nil, threads it into the first form (via ->>),\nand when that result is not nil, through the next etc",
   :var-type "macro",
   :line 7872,
   :added "1.5",
   :namespace "clojure.core",
   :arglists ([expr & forms]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/some->>",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7872",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "some-fn",
   :doc
   "Takes a set of predicates and returns a function f that returns the first logical true value\nreturned by one of its composing predicates against any of its arguments, else it returns\nlogical false. Note that f is short-circuiting in that it will stop execution on the first\nargument that triggers a logical true result against the original predicates.",
   :var-type "function",
   :line 7710,
   :added "1.3",
   :namespace "clojure.core",
   :arglists ([p] [p1 p2] [p1 p2 p3] [p1 p2 p3 & ps]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/some-fn",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7710",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "some-vals",
   :doc
   "Returns a map with only the non-nil values of map m. Returns nil if\nm has no non-nil vals.",
   :var-type "function",
   :line 4439,
   :added "1.13",
   :namespace "clojure.core",
   :arglists ([m]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/some-vals",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4439",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "some?",
   :doc "Returns true if x is not nil, false otherwise.",
   :var-type "function",
   :line 533,
   :added "1.6",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/some?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L533",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "sort",
   :doc
   "Returns a sorted sequence of the items in coll. If no comparator is\nsupplied, uses compare.  comparator must implement\njava.util.Comparator.  Guaranteed to be stable: equal elements will\nnot be reordered.  If coll is a Java array, it will be modified.  To\navoid this, sort a copy of the array.",
   :var-type "function",
   :line 3107,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([coll] [comp coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/sort",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3107",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "sort-by",
   :doc
   "Returns a sorted sequence of the items in coll, where the sort\norder is determined by comparing (keyfn item).  If no comparator is\nsupplied, uses compare.  comparator must implement\njava.util.Comparator.  Guaranteed to be stable: equal elements will\nnot be reordered.  If coll is a Java array, it will be modified.  To\navoid this, sort a copy of the array.",
   :var-type "function",
   :line 3124,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([keyfn coll] [keyfn comp coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/sort-by",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3124",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "sorted-map",
   :doc
   "keyval => key val\nReturns a new sorted map with supplied mappings.  If any keys are\nequal, they are handled as if by repeated uses of assoc.",
   :var-type "function",
   :line 400,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([& keyvals]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/sorted-map",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L400",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "sorted-map-by",
   :doc
   "keyval => key val\nReturns a new sorted map with supplied mappings, using the supplied\ncomparator.  If any keys are equal, they are handled as if by\nrepeated uses of assoc.",
   :var-type "function",
   :line 409,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([comparator & keyvals]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/sorted-map-by",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L409",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "sorted-set",
   :doc
   "Returns a new sorted set with supplied keys.  Any equal keys are\nhandled as if by repeated uses of conj.",
   :var-type "function",
   :line 419,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([& keys]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/sorted-set",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L419",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "sorted-set-by",
   :doc
   "Returns a new sorted set with supplied keys, using the supplied\ncomparator.  Any equal keys are handled as if by repeated uses of\nconj.",
   :var-type "function",
   :line 427,
   :added "1.1",
   :namespace "clojure.core",
   :arglists ([comparator & keys]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/sorted-set-by",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L427",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "sorted?",
   :doc "Returns true if coll implements Sorted",
   :var-type "function",
   :line 6395,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/sorted?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6395",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "special-symbol?",
   :doc "Returns true if s names a special form",
   :var-type "function",
   :line 5113,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([s]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/special-symbol?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5113",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "spit",
   :doc
   "Opposite of slurp.  Opens f with writer, writes content, then\ncloses f. Options passed to clojure.java.io/writer.",
   :var-type "function",
   :line 7183,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([f content & options]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/spit",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7183",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "split-at",
   :doc "Returns a vector of [(take n coll) (drop n coll)]",
   :var-type "function",
   :line 3005,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([n coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/split-at",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3005",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "split-with",
   :doc
   "Returns a vector of [(take-while pred coll) (drop-while pred coll)]",
   :var-type "function",
   :line 3012,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([pred coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/split-with",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3012",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "splitv-at",
   :doc "Returns a vector of [(into [] (take n) coll) (drop n coll)]",
   :var-type "function",
   :line 7504,
   :added "1.12",
   :namespace "clojure.core",
   :arglists ([n coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/splitv-at",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7504",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "str",
   :doc
   "With no args, returns the empty string. With one arg x, returns\nx.toString().  (str nil) returns the empty string. With more than\none arg, returns the concatenation of the str values of the args.",
   :var-type "function",
   :line 546,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([] [x] [x & ys]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/str",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L546",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "stream-into!",
   :doc
   "Returns a new coll consisting of coll with all of the items of the\nstream conjoined. This is a terminal operation on the stream.",
   :var-type "function",
   :line 6956,
   :added "1.12",
   :namespace "clojure.core",
   :arglists ([to stream] [to xform stream]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/stream-into!",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6956",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "stream-reduce!",
   :doc
   "Works like reduce but takes a java.util.stream.BaseStream as its source.\nHonors 'reduced', is a terminal operation on the stream",
   :var-type "function",
   :line 6930,
   :added "1.12",
   :namespace "clojure.core",
   :arglists ([f s] [f init s]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/stream-reduce!",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6930",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "stream-seq!",
   :doc
   "Takes a java.util.stream.BaseStream instance s and returns a seq of its\ncontents. This is a terminal operation on the stream.",
   :var-type "function",
   :line 6939,
   :added "1.12",
   :namespace "clojure.core",
   :arglists ([stream]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/stream-seq!",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6939",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "stream-transduce!",
   :doc
   "Works like transduce but takes a java.util.stream.BaseStream as its source.\nThis is a terminal operation on the stream.",
   :var-type "function",
   :line 6946,
   :added "1.12",
   :namespace "clojure.core",
   :arglists ([xform f stream] [xform f init stream]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/stream-transduce!",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6946",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "string?",
   :doc "Return true if x is a String",
   :var-type "function",
   :line 162,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/string?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L162",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "struct",
   :doc
   "Returns a new structmap instance with the keys of the\nstructure-basis. vals must be supplied for basis keys in order -\nwhere values are not supplied they will default to nil.",
   :var-type "function",
   :line 4085,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([s & vals]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/struct",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4085",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "struct-map",
   :doc
   "Returns a new structmap instance with the keys of the\nstructure-basis. keyvals may contain all, some or none of the basis\nkeys - where values are not supplied they will default to nil.\nkeyvals can also contain keys not in the basis.",
   :var-type "function",
   :line 4075,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([s & inits]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/struct-map",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4075",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "subs",
   :doc
   "Returns the substring of s beginning at start inclusive, and ending\nat end (defaults to length of string), exclusive.",
   :var-type "function",
   :line 5126,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([s start] [s start end]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/subs",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5126",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "subseq",
   :doc
   "sc must be a sorted collection, test(s) one of <, <=, > or\n>=. Returns a seq of those entries with keys ek for\nwhich (test (.. sc comparator (compare ek key)) 0) is true",
   :var-type "function",
   :line 5254,
   :added "1.0",
   :namespace "clojure.core",
   :arglists
   ([sc test key] [sc start-test start-key end-test end-key]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/subseq",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5254",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "subvec",
   :doc
   "Returns a persistent vector of the items in vector from\nstart (inclusive) to end (exclusive).  If end is not supplied,\ndefaults to (count vector). This operation is O(1) and very fast, as\nthe resulting vector shares structure with the original and no\ntrimming is done.",
   :var-type "function",
   :line 3841,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([v start] [v start end]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/subvec",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3841",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "supers",
   :doc
   "Returns the immediate and indirect superclasses and interfaces of c, if any",
   :var-type "function",
   :line 5689,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([class]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/supers",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5689",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "swap!",
   :doc
   "Atomically swaps the value of atom to be:\n(apply f current-value-of-atom args). Note that f may be called\nmultiple times, and thus should be free of side effects.  Returns\nthe value that was swapped in.",
   :var-type "function",
   :line 2359,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([atom f] [atom f x] [atom f x y] [atom f x y & args]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/swap!",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2359",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "swap-vals!",
   :doc
   "Atomically swaps the value of atom to be:\n(apply f current-value-of-atom args). Note that f may be called\nmultiple times, and thus should be free of side effects.\nReturns [old new], the value of the atom before and after the swap.",
   :var-type "function",
   :line 2371,
   :added "1.9",
   :namespace "clojure.core",
   :arglists ([atom f] [atom f x] [atom f x y] [atom f x y & args]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/swap-vals!",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2371",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "symbol",
   :doc
   "Returns a Symbol with the given namespace and name. Arity-1 works\non strings, keywords, and vars.",
   :var-type "function",
   :line 591,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([name] [ns name]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/symbol",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L591",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "symbol?",
   :doc "Return true if x is a Symbol",
   :var-type "function",
   :line 564,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/symbol?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L564",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "sync",
   :doc
   "transaction-flags => TBD, pass nil for now\n\nRuns the exprs (in an implicit do) in a transaction that encompasses\nexprs and any nested calls.  Starts a transaction if none is already\nrunning on this thread. Any uncaught exception will abort the\ntransaction and flow out of sync. The exprs may be run more than\nonce, but any effects on Refs will be atomic.",
   :var-type "macro",
   :line 2512,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([flags-ignored-for-now & body]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/sync",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2512",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "tagged-literal",
   :doc
   "Construct a data representation of a tagged literal from a\ntag symbol and a form.",
   :var-type "function",
   :line 8045,
   :added "1.7",
   :namespace "clojure.core",
   :arglists ([tag form]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/tagged-literal",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L8045",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "tagged-literal?",
   :doc
   "Return true if the value is the data representation of a tagged literal",
   :var-type "function",
   :line 8039,
   :added "1.7",
   :namespace "clojure.core",
   :arglists ([value]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/tagged-literal?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L8039",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "take",
   :doc
   "Returns a lazy sequence of the first n items in coll, or all items if\nthere are fewer than n.  Returns a stateful transducer when\nno collection is provided.",
   :var-type "function",
   :line 2875,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([n] [n coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/take",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2875",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "take-last",
   :doc
   "Returns a seq of the last n items in coll.  Depending on the type\nof coll may be no better than linear time.  For vectors, see also subvec.",
   :var-type "function",
   :line 2961,
   :added "1.1",
   :namespace "clojure.core",
   :arglists ([n coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/take-last",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2961",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "take-nth",
   :doc
   "Returns a lazy seq of every nth item in coll.  Returns a stateful\ntransducer when no collection is provided.",
   :var-type "function",
   :line 4311,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([n] [n coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/take-nth",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4311",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "take-while",
   :doc
   "Returns a lazy sequence of successive items from coll while\n(pred item) returns logical true. pred must be free of side-effects.\nReturns a transducer when no collection is provided.",
   :var-type "function",
   :line 2902,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([pred] [pred coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/take-while",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2902",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "tap>",
   :doc
   "sends x to any taps. Will not block. Returns true if there was room in the queue,\nfalse if not (dropped).",
   :var-type "function",
   :line 8206,
   :added "1.10",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/tap>",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L8206",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "test",
   :doc
   "test [v] finds fn at key :test in var metadata and calls it,\npresuming failure will throw exception",
   :var-type "function",
   :line 4974,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([v]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/test",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4974",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "the-ns",
   :doc
   "If passed a namespace, returns it. Else, when passed a symbol,\nreturns the namespace named by it, throwing an exception if not\nfound.",
   :var-type "function",
   :line 4176,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/the-ns",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4176",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "thread-bound?",
   :doc
   "Returns true if all of the vars provided as arguments have thread-local bindings.\nImplies that set!'ing the provided vars will succeed.  Returns true if no vars are provided.",
   :var-type "function",
   :line 5656,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([& vars]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/thread-bound?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5656",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "throw",
   :doc
   "The expr is evaluated and thrown, therefore it should\nyield an instance of some derivee of Throwable.\n\nPlease see https://clojure.org/reference/special_forms#throw",
   :var-type "special form",
   :added "1.0",
   :forms [(throw expr)],
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/throw",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "time",
   :doc
   "Evaluates expr and prints the time it took.  Returns the value of\nexpr.",
   :var-type "macro",
   :line 3907,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([expr]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/time",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3907",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "to-array",
   :doc
   "Returns an array of Objects containing the contents of coll, which\ncan be any Collection.  Maps to java.util.Collection.toArray().",
   :var-type "function",
   :line 340,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/to-array",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L340",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "to-array-2d",
   :doc
   "Returns a (potentially-ragged) 2-dimensional array of Objects\ncontaining the contents of coll, which can be any Collection of any\nCollection.",
   :var-type "function",
   :line 4026,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/to-array-2d",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4026",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "trampoline",
   :doc
   "trampoline can be used to convert algorithms requiring mutual\nrecursion without stack consumption. Calls f with supplied args, if\nany. If f returns a fn, calls that fn with no arguments, and\ncontinues to repeat, until the return value is not a fn, then\nreturns that non-fn value. Note that if you want to return a fn as a\nfinal value, you must wrap it in some data structure and unpack it\nafter trampoline returns.",
   :var-type "function",
   :line 6453,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([f] [f & args]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/trampoline",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6453",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "transduce",
   :doc
   "reduce with a transformation of f (xf). If init is not\nsupplied, (f) will be called to produce it. f should be a reducing\nstep function that accepts both 1 and 2 arguments, if it accepts\nonly 2 you can add the arity-1 with 'completing'. Returns the result\nof applying (the transformed) xf to init and the first item in coll,\nthen applying xf to that result and the 2nd item, etc. If coll\ncontains no items, returns init and f is not called. Note that\ncertain transforms may inject or skip items.",
   :var-type "function",
   :line 7081,
   :added "1.7",
   :namespace "clojure.core",
   :arglists ([xform f coll] [xform f init coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/transduce",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7081",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "transient",
   :doc
   "Returns a new, transient version of the collection, in constant time.\n\nTransients support a parallel set of 'changing' operations, with similar names\nfollowed by ! - assoc!, conj! etc. These do the same things as their persistent\ncounterparts except the return values are themselves transient.\n\nNote in particular that transients are not designed to be bashed in-place. You\nmust capture and use the return value in the next call. In this way, they support\nthe same code structure as the functional persistent code they replace.",
   :var-type "function",
   :line 3357,
   :added "1.1",
   :namespace "clojure.core",
   :arglists ([coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/transient",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3357",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "tree-seq",
   :doc
   "Returns a lazy sequence of the nodes in a tree, via a depth-first walk.\n branch? must be a fn of one arg that returns true if passed a node\n that can have children (but may not).  children must be a fn of one\n arg that returns a sequence of the children. Will only be called on\n nodes for which branch? returns true. Root is the root node of the\ntree.",
   :var-type "function",
   :line 5076,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([branch? children root]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/tree-seq",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5076",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "true?",
   :doc "Returns true if x is the value true, false otherwise.",
   :var-type "function",
   :line 514,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/true?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L514",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "try",
   :doc
   "catch-clause => (catch classname name expr*)\nfinally-clause => (finally expr*)\n\nCatches and handles Java exceptions.\n\nPlease see https://clojure.org/reference/special_forms#try",
   :var-type "special form",
   :added "1.0",
   :forms [(try expr* catch-clause* finally-clause?)],
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/try",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "type",
   :doc "Returns the :type metadata of x, or its Class if none",
   :var-type "function",
   :line 3489,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/type",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3489",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "unchecked-add",
   :doc
   "Returns the sum of x and y, both long.\nNote - uses a primitive operator subject to overflow.",
   :var-type "function",
   :line 1212,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x y]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-add",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1212",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "unchecked-add-int",
   :doc
   "Returns the sum of x and y, both int.\nNote - uses a primitive operator subject to overflow.",
   :var-type "function",
   :line 1205,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x y]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-add-int",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1205",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "unchecked-byte",
   :doc "Coerce to byte. Subject to rounding or truncation.",
   :var-type "function",
   :line 3539,
   :added "1.3",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-byte",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3539",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "unchecked-char",
   :doc "Coerce to char. Subject to rounding or truncation.",
   :var-type "function",
   :line 3551,
   :added "1.3",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-char",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3551",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "unchecked-dec",
   :doc
   "Returns a number one less than x, a long.\nNote - uses a primitive operator subject to overflow.",
   :var-type "function",
   :line 1184,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-dec",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1184",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "unchecked-dec-int",
   :doc
   "Returns a number one less than x, an int.\nNote - uses a primitive operator subject to overflow.",
   :var-type "function",
   :line 1177,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-dec-int",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1177",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "unchecked-divide-int",
   :doc
   "Returns the division of x by y, both int.\nNote - uses a primitive operator subject to truncation.",
   :var-type "function",
   :line 1247,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x y]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-divide-int",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1247",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "unchecked-double",
   :doc "Coerce to double. Subject to rounding.",
   :var-type "function",
   :line 3575,
   :added "1.3",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-double",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3575",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "unchecked-float",
   :doc "Coerce to float. Subject to rounding.",
   :var-type "function",
   :line 3569,
   :added "1.3",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-float",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3569",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "unchecked-inc",
   :doc
   "Returns a number one greater than x, a long.\nNote - uses a primitive operator subject to overflow.",
   :var-type "function",
   :line 1170,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-inc",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1170",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "unchecked-inc-int",
   :doc
   "Returns a number one greater than x, an int.\nNote - uses a primitive operator subject to overflow.",
   :var-type "function",
   :line 1163,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-inc-int",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1163",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "unchecked-int",
   :doc "Coerce to int. Subject to rounding or truncation.",
   :var-type "function",
   :line 3557,
   :added "1.3",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-int",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3557",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "unchecked-long",
   :doc "Coerce to long. Subject to rounding or truncation.",
   :var-type "function",
   :line 3563,
   :added "1.3",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-long",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3563",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "unchecked-multiply",
   :doc
   "Returns the product of x and y, both long.\nNote - uses a primitive operator subject to overflow.",
   :var-type "function",
   :line 1240,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x y]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-multiply",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1240",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "unchecked-multiply-int",
   :doc
   "Returns the product of x and y, both int.\nNote - uses a primitive operator subject to overflow.",
   :var-type "function",
   :line 1233,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x y]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-multiply-int",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1233",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "unchecked-negate",
   :doc
   "Returns the negation of x, a long.\nNote - uses a primitive operator subject to overflow.",
   :var-type "function",
   :line 1198,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-negate",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1198",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "unchecked-negate-int",
   :doc
   "Returns the negation of x, an int.\nNote - uses a primitive operator subject to overflow.",
   :var-type "function",
   :line 1191,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-negate-int",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1191",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "unchecked-remainder-int",
   :doc
   "Returns the remainder of division of x by y, both int.\nNote - uses a primitive operator subject to truncation.",
   :var-type "function",
   :line 1254,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x y]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-remainder-int",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1254",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "unchecked-short",
   :doc "Coerce to short. Subject to rounding or truncation.",
   :var-type "function",
   :line 3545,
   :added "1.3",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-short",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3545",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "unchecked-subtract",
   :doc
   "Returns the difference of x and y, both long.\nNote - uses a primitive operator subject to overflow.",
   :var-type "function",
   :line 1226,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x y]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-subtract",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1226",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "unchecked-subtract-int",
   :doc
   "Returns the difference of x and y, both int.\nNote - uses a primitive operator subject to overflow.",
   :var-type "function",
   :line 1219,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x y]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-subtract-int",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1219",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "underive",
   :doc
   "Removes a parent/child relationship between parent and\ntag. h must be a hierarchy obtained from make-hierarchy, if not\nsupplied defaults to, and modifies, the global hierarchy.",
   :var-type "function",
   :line 5800,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([tag parent] [h tag parent]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/underive",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5800",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "unreduced",
   :doc "If x is reduced?, returns (deref x), else returns x",
   :var-type "function",
   :line 2869,
   :added "1.7",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unreduced",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2869",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "unsigned-bit-shift-right",
   :doc "Bitwise shift right, without sign-extension.",
   :var-type "function",
   :line 1382,
   :added "1.6",
   :namespace "clojure.core",
   :arglists ([x n]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unsigned-bit-shift-right",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1382",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "update",
   :doc
   "'Updates' a value in an associative structure, where k is a\nkey and f is a function that will take the old value\nand any supplied args and return the new value, and returns a new\nstructure.  If the key does not exist, nil is passed as the old value.",
   :var-type "function",
   :line 6334,
   :added "1.7",
   :namespace "clojure.core",
   :arglists
   ([m k f] [m k f x] [m k f x y] [m k f x y z] [m k f x y z & more]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/update",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6334",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "update-in",
   :doc
   "'Updates' a value in a nested associative structure, where ks is a\nsequence of keys and f is a function that will take the old value\nand any supplied args and return the new value, and returns a new\nnested structure.  If any levels do not exist, hash-maps will be\ncreated.",
   :var-type "function",
   :line 6318,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([m ks f & args]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/update-in",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6318",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "update-keys",
   :doc
   "m f => {(f k) v ...}\n\nGiven a map m and a function f of 1-argument, returns a new map whose\nkeys are the result of applying f to the keys of m, mapped to the\ncorresponding values of m.\nf must return a unique key for each key of m, else the behavior is undefined.",
   :var-type "function",
   :line 8230,
   :added "1.11",
   :namespace "clojure.core",
   :arglists ([m f]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/update-keys",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L8230",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "update-proxy",
   :doc
   "Takes a proxy instance and a map of strings (which must\ncorrespond to methods of the proxy superclass/superinterfaces) to\nfns (which must take arguments matching the corresponding method,\nplus an additional (explicit) first arg corresponding to this, and\nupdates (via assoc) the proxy's fn map. nil can be passed instead of\na fn, in which case the corresponding method will revert to the\ndefault behavior. Note that this function can be used to update the\nbehavior of an existing instance without changing its identity.\nReturns the proxy.",
   :var-type "function",
   :line 313,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([proxy mappings]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/update-proxy",
   :source-url
   "https://github.com/clojure/clojure/blob/658693f6cf97e6ab0ff789e096c9eb6654e4d3ab/src/clj/clojure/core_proxy.clj#L313",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/658693f6cf97e6ab0ff789e096c9eb6654e4d3ab/src/clj/clojure/core_proxy.clj",
   :file "src/clj/clojure/core_proxy.clj"}
  {:name "update-vals",
   :doc
   "m f => {k (f v) ...}\n\nGiven a map m and a function f of 1-argument, returns a new map where the keys of m\nare mapped to result of applying f to the corresponding values of m.",
   :var-type "function",
   :line 8214,
   :added "1.11",
   :namespace "clojure.core",
   :arglists ([m f]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/update-vals",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L8214",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "uri?",
   :doc "Return true if x is a java.net.URI",
   :var-type "function",
   :line 8165,
   :added "1.9",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/uri?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L8165",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "use",
   :doc
   "Like 'require, but also refers to each lib's namespace using\nclojure.core/refer. Use :use in the ns macro in preference to calling\nthis directly.\n\n'use accepts additional options in libspecs: :exclude, :only, :rename.\nThe arguments and semantics for :exclude, :only, and :rename are the same\nas those documented for clojure.core/refer.",
   :var-type "function",
   :line 6239,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([& args]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/use",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6239",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "uuid?",
   :doc "Return true if x is a java.util.UUID",
   :var-type "function",
   :line 7003,
   :added "1.9",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/uuid?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7003",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "val",
   :doc "Returns the value in the map entry.",
   :var-type "function",
   :line 1586,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([e]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/val",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1586",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "vals",
   :doc
   "Returns a sequence of the map's values, in the same order as (seq map).",
   :var-type "function",
   :line 1573,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([map]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/vals",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1573",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "var",
   :doc
   "The symbol must resolve to a var, and the Var object\nitself (not its value) is returned. The reader macro #'x\nexpands to (var x).\n\nPlease see https://clojure.org/reference/special_forms#var",
   :var-type "special form",
   :added "1.0",
   :forms [#'symbol],
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/var",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "var-get",
   :doc "Gets the value in the var object",
   :var-type "function",
   :line 4350,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/var-get",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4350",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "var-set",
   :doc
   "Sets the value in the var object to val. The var must be\nthread-locally bound.",
   :var-type "function",
   :line 4356,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x val]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/var-set",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4356",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "var?",
   :doc "Returns true if v is of type clojure.lang.Var",
   :var-type "function",
   :line 5120,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([v]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/var?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5120",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "vary-meta",
   :doc
   "Returns an object of the same type and value as obj, with\n(apply f (meta obj) args) as its metadata.",
   :var-type "function",
   :line 677,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([obj f & args]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/vary-meta",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L677",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "vec",
   :doc
   "Creates a new vector containing the contents of coll. Java arrays\nwill be aliased and should not be modified.",
   :var-type "function",
   :line 369,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/vec",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L369",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "vector",
   :doc "Creates a new vector containing the args.",
   :var-type "function",
   :line 355,
   :added "1.0",
   :namespace "clojure.core",
   :arglists
   ([]
    [a]
    [a b]
    [a b c]
    [a b c d]
    [a b c d e]
    [a b c d e f]
    [a b c d e f & args]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/vector",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L355",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "vector-of",
   :doc
   "Creates a new vector of a single primitive type t, where t is one\nof :int :long :float :double :byte :short :char or :boolean. The\nresulting vector complies with the interface of vectors in general,\nbut stores the values unboxed internally.\n\nOptionally takes one or more elements to populate the vector.",
   :var-type "function",
   :line 523,
   :added "1.2",
   :namespace "clojure.core",
   :arglists ([t] [t & elements]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/vector-of",
   :source-url
   "https://github.com/clojure/clojure/blob/e0efe77eb39f3868ba02c5951f036a48262e80ff/src/clj/clojure/gvec.clj#L523",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/e0efe77eb39f3868ba02c5951f036a48262e80ff/src/clj/clojure/gvec.clj",
   :file "src/clj/clojure/gvec.clj"}
  {:name "vector?",
   :doc "Return true if x implements IPersistentVector",
   :var-type "function",
   :line 176,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/vector?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L176",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "volatile!",
   :doc "Creates and returns a Volatile with an initial value of val.",
   :var-type "function",
   :line 2539,
   :added "1.7",
   :namespace "clojure.core",
   :arglists ([val]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/volatile!",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2539",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "volatile?",
   :doc "Returns true if x is a volatile.",
   :var-type "function",
   :line 2562,
   :added "1.7",
   :namespace "clojure.core",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/volatile?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2562",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "vreset!",
   :doc
   "Sets the value of volatile to newval without regard for the\ncurrent value. Returns newval.",
   :var-type "function",
   :line 2546,
   :added "1.7",
   :namespace "clojure.core",
   :arglists ([vol newval]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/vreset!",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2546",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "vswap!",
   :doc
   "Non-atomically swaps the value of the volatile as if:\n(apply f current-value-of-vol args). Returns the value that\nwas swapped in.",
   :var-type "macro",
   :line 2553,
   :added "1.7",
   :namespace "clojure.core",
   :arglists ([vol f & args]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/vswap!",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2553",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "when",
   :doc
   "Evaluates test. If logical true, evaluates body in an implicit do.",
   :var-type "macro",
   :line 495,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([test & body]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/when",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L495",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "when-first",
   :doc
   "bindings => x xs\n\nRoughly the same as (when (seq xs) (let [x (first xs)] body)) but xs is evaluated only once",
   :var-type "macro",
   :line 4741,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([bindings & body]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/when-first",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4741",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "when-let",
   :doc
   "bindings => binding-form test\n\nWhen test is true, evaluates body with binding-form bound to the value of test",
   :var-type "macro",
   :line 1875,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([bindings & body]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/when-let",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1875",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "when-not",
   :doc
   "Evaluates test. If logical false, evaluates body in an implicit do.",
   :var-type "macro",
   :line 501,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([test & body]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/when-not",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L501",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "when-some",
   :doc
   "bindings => binding-form test\n\nWhen test is not nil, evaluates body with binding-form bound to the\nvalue of test",
   :var-type "macro",
   :line 1910,
   :added "1.6",
   :namespace "clojure.core",
   :arglists ([bindings & body]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/when-some",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1910",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "while",
   :doc
   "Repeatedly executes body while test expression is true. Presumes\nsome side-effect will cause test to become false/nil. Returns nil",
   :var-type "macro",
   :line 6487,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([test & body]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/while",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L6487",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "with-bindings",
   :doc
   "Takes a map of Var/value pairs. Installs for the given Vars the associated\nvalues as thread-local bindings. Then executes body. Pops the installed\nbindings after body was evaluated. Returns the value of body.",
   :var-type "macro",
   :line 2000,
   :added "1.1",
   :namespace "clojure.core",
   :arglists ([binding-map & body]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/with-bindings",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L2000",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "with-bindings*",
   :doc
   "Takes a map of Var/value pairs. Installs for the given Vars the associated\nvalues as thread-local bindings. Then calls f with the supplied arguments.\nPops the installed bindings after f returned. Returns whatever f returns.",
   :var-type "function",
   :line 1987,
   :added "1.1",
   :namespace "clojure.core",
   :arglists ([binding-map f & args]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/with-bindings*",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L1987",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "with-in-str",
   :doc
   "Evaluates body in a context in which *in* is bound to a fresh\nStringReader initialized with the string s.",
   :var-type "macro",
   :line 4868,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([s & body]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/with-in-str",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4868",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "with-local-vars",
   :doc
   "varbinding=> symbol init-expr\n\nExecutes the exprs in a context in which the symbols are bound to\nvars with per-thread bindings to the init-exprs.  The symbols refer\nto the var objects themselves, and must be accessed with var-get and\nvar-set",
   :var-type "macro",
   :line 4363,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([name-vals-vec & body]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/with-local-vars",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4363",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "with-meta",
   :doc
   "Returns an object of the same type and value as obj, with\nmap m as its metadata.",
   :var-type "function",
   :line 213,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([obj m]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/with-meta",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L213",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "with-open",
   :doc
   "bindings => [name init ...]\n\nEvaluates body in a try expression with names bound to the values\nof the inits, and a finally clause that calls (.close name) on each\nname in reverse order.",
   :var-type "macro",
   :line 3854,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([bindings & body]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/with-open",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L3854",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "with-out-str",
   :doc
   "Evaluates exprs in a context in which *out* is bound to a fresh\nStringWriter.  Returns the string created by any nested printing\ncalls.",
   :var-type "macro",
   :line 4857,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([& body]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/with-out-str",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4857",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "with-precision",
   :doc
   "Sets the precision and rounding mode to be used for BigDecimal operations.\n\nUsage: (with-precision 10 (/ 1M 3))\nor:    (with-precision 10 :rounding HALF_DOWN (/ 1M 3))\n\nThe rounding mode is one of CEILING, FLOOR, HALF_UP, HALF_DOWN,\nHALF_EVEN, UP, DOWN and UNNECESSARY; it defaults to HALF_UP.",
   :var-type "macro",
   :line 5231,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([precision & exprs]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/with-precision",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5231",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "with-redefs",
   :doc
   "binding => var-symbol temp-value-expr\n\nTemporarily redefines Vars while executing the body.  The\ntemp-value-exprs will be evaluated and each resulting value will\nreplace in parallel the root value of its Var.  After the body is\nexecuted, the root values of all the Vars will be set back to their\nold values.  These temporary changes will be visible in all threads.\nUseful for mocking out functions during testing.",
   :var-type "macro",
   :line 7792,
   :added "1.3",
   :namespace "clojure.core",
   :arglists ([bindings & body]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/with-redefs",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7792",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "with-redefs-fn",
   :doc
   "Temporarily redefines Vars during a call to func.  Each val of\nbinding-map will replace the root value of its key which must be\na Var.  After func is called with no args, the root values of all\nthe Vars will be set back to their old values.  These temporary\nchanges will be visible in all threads.  Useful for mocking out\nfunctions during testing.",
   :var-type "function",
   :line 7772,
   :added "1.3",
   :namespace "clojure.core",
   :arglists ([binding-map func]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/with-redefs-fn",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L7772",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "xml-seq",
   :doc "A tree seq on the xml elements as per xml/parse",
   :var-type "function",
   :line 5103,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([root]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/xml-seq",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L5103",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "zero?",
   :doc "Returns true if num is zero, else false",
   :var-type "function",
   :line 869,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([num]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/zero?",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L869",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "zipmap",
   :doc
   "Returns a map with the keys mapped to the corresponding vals.",
   :var-type "function",
   :line 4414,
   :added "1.0",
   :namespace "clojure.core",
   :arglists ([keys vals]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/zipmap",
   :source-url
   "https://github.com/clojure/clojure/blob/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj#L4414",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/bca9d8287d6d34547b0834e5072f53743c9f15b6/src/clj/clojure/core.clj",
   :file "src/clj/clojure/core.clj"}
  {:name "ArrayChunk",
   :var-type "type",
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ArrayChunk",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "Eduction",
   :var-type "type",
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/Eduction",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "Vec",
   :var-type "type",
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/Vec",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "VecNode",
   :var-type "type",
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/VecNode",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "VecSeq",
   :var-type "type",
   :namespace "clojure.core",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/VecSeq",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "diff",
   :doc
   "Recursively compares a and b, returning a tuple of\n[things-only-in-a things-only-in-b things-in-both].\nComparison rules:\n\n* For equal a and b, return [nil nil a].\n* Maps are subdiffed where keys match and values differ.\n* Sets are never subdiffed.\n* All sequential things are treated as associative collections\n  by their indexes, with results returned as vectors.\n* Everything else (including strings!) is treated as\n  an atom and compared for equality.",
   :var-type "function",
   :line 124,
   :added "1.3",
   :namespace "clojure.data",
   :arglists ([a b]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.data-api.html#clojure.data/diff",
   :source-url
   "https://github.com/clojure/clojure/blob/51c6d7a70912a8f65e81a8e11ae6f56c94920725/src/clj/clojure/data.clj#L124",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/51c6d7a70912a8f65e81a8e11ae6f56c94920725/src/clj/clojure/data.clj",
   :file "src/clj/clojure/data.clj"}
  {:name "Diff",
   :doc "Implementation detail. Subject to change.",
   :var-type "protocol",
   :line 73,
   :added "1.3",
   :namespace "clojure.data",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.data-api.html#clojure.data/Diff",
   :source-url
   "https://github.com/clojure/clojure/blob/51c6d7a70912a8f65e81a8e11ae6f56c94920725/src/clj/clojure/data.clj#L73",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/51c6d7a70912a8f65e81a8e11ae6f56c94920725/src/clj/clojure/data.clj",
   :file "src/clj/clojure/data.clj"}
  {:name "EqualityPartition",
   :doc "Implementation detail. Subject to change.",
   :var-type "protocol",
   :line 69,
   :added "1.3",
   :namespace "clojure.data",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.data-api.html#clojure.data/EqualityPartition",
   :source-url
   "https://github.com/clojure/clojure/blob/51c6d7a70912a8f65e81a8e11ae6f56c94920725/src/clj/clojure/data.clj#L69",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/51c6d7a70912a8f65e81a8e11ae6f56c94920725/src/clj/clojure/data.clj",
   :file "src/clj/clojure/data.clj"}
  {:name "diff-similar",
   :doc "Implementation detail. Subject to change.",
   :var-type "function",
   :added "1.3",
   :namespace "clojure.data",
   :arglists ([a b]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.data-api.html#clojure.data/diff-similar",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "equality-partition",
   :doc "Implementation detail. Subject to change.",
   :var-type "function",
   :added "1.3",
   :namespace "clojure.data",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.data-api.html#clojure.data/equality-partition",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "datafy",
   :doc
   "Attempts to return x as data.\ndatafy will return the value of clojure.core.protocols/datafy. If\nthe value has been transformed and the result supports\nmetadata, :clojure.datafy/obj will be set on the metadata to the\noriginal value of x, and :clojure.datafy/class to the name of the\nclass of x, as a symbol.",
   :var-type "function",
   :line 15,
   :namespace "clojure.datafy",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.datafy-api.html#clojure.datafy/datafy",
   :source-url
   "https://github.com/clojure/clojure/blob/b70db9639f9acddcabf7f760ea4bb050d6bfaa16/src/clj/clojure/datafy.clj#L15",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/b70db9639f9acddcabf7f760ea4bb050d6bfaa16/src/clj/clojure/datafy.clj",
   :file "src/clj/clojure/datafy.clj"}
  {:name "nav",
   :doc
   "Returns (possibly transformed) v in the context of coll and k (a\nkey/index or nil). Callers should attempt to provide the key/index\ncontext k for Indexed/Associative/ILookup colls if possible, but not\nto fabricate one e.g. for sequences (pass nil). nav returns the\nvalue of clojure.core.protocols/nav.",
   :var-type "function",
   :line 30,
   :namespace "clojure.datafy",
   :arglists ([coll k v]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.datafy-api.html#clojure.datafy/nav",
   :source-url
   "https://github.com/clojure/clojure/blob/b70db9639f9acddcabf7f760ea4bb050d6bfaa16/src/clj/clojure/datafy.clj#L30",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/b70db9639f9acddcabf7f760ea4bb050d6bfaa16/src/clj/clojure/datafy.clj",
   :file "src/clj/clojure/datafy.clj"}
  {:name "read",
   :doc
   "Reads the next object from stream, which must be an instance of\njava.io.PushbackReader or some derivee.  stream defaults to the\ncurrent value of *in*.\n\nReads data in the edn format (subset of Clojure data):\nhttp://edn-format.org\n\nopts is a map that can include the following keys:\n:eof - value to return on end-of-file. When not supplied, eof throws an exception.\n:readers  - a map of tag symbols to data-reader functions to be considered before default-data-readers.\n            When not supplied, only the default-data-readers will be used.\n:default - A function of two args, that will, if present and no reader is found for a tag,\n           be called with the tag and the value.",
   :var-type "function",
   :line 14,
   :added "1.5",
   :namespace "clojure.edn",
   :arglists ([] [stream] [opts stream]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.edn-api.html#clojure.edn/read",
   :source-url
   "https://github.com/clojure/clojure/blob/c6756a8bab137128c8119add29a25b0a88509900/src/clj/clojure/edn.clj#L14",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/c6756a8bab137128c8119add29a25b0a88509900/src/clj/clojure/edn.clj",
   :file "src/clj/clojure/edn.clj"}
  {:name "read-string",
   :doc
   "Reads one object from the string s. Returns nil when s is nil or empty.\n\nReads data in the edn format (subset of Clojure data):\nhttp://edn-format.org\n\nopts is a map as per clojure.edn/read",
   :var-type "function",
   :line 37,
   :added "1.5",
   :namespace "clojure.edn",
   :arglists ([s] [opts s]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.edn-api.html#clojure.edn/read-string",
   :source-url
   "https://github.com/clojure/clojure/blob/c6756a8bab137128c8119add29a25b0a88509900/src/clj/clojure/edn.clj#L37",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/c6756a8bab137128c8119add29a25b0a88509900/src/clj/clojure/edn.clj",
   :file "src/clj/clojure/edn.clj"}
  {:name "inspect",
   :doc "creates a graphical (Swing) inspector on the supplied object",
   :var-type "function",
   :line 154,
   :added "1.0",
   :namespace "clojure.inspector",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.inspector-api.html#clojure.inspector/inspect",
   :source-url
   "https://github.com/clojure/clojure/blob/5da21b38470d175b3cecbf93b9cd145ca36940c1/src/clj/clojure/inspector.clj#L154",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/5da21b38470d175b3cecbf93b9cd145ca36940c1/src/clj/clojure/inspector.clj",
   :file "src/clj/clojure/inspector.clj"}
  {:name "inspect-table",
   :doc
   "creates a graphical (Swing) inspector on the supplied regular\ndata, which must be a sequential data structure of data structures\nof equal length",
   :var-type "function",
   :line 100,
   :added "1.0",
   :namespace "clojure.inspector",
   :arglists ([data]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.inspector-api.html#clojure.inspector/inspect-table",
   :source-url
   "https://github.com/clojure/clojure/blob/5da21b38470d175b3cecbf93b9cd145ca36940c1/src/clj/clojure/inspector.clj#L100",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/5da21b38470d175b3cecbf93b9cd145ca36940c1/src/clj/clojure/inspector.clj",
   :file "src/clj/clojure/inspector.clj"}
  {:name "inspect-tree",
   :doc
   "creates a graphical (Swing) inspector on the supplied hierarchical data",
   :var-type "function",
   :line 91,
   :added "1.0",
   :namespace "clojure.inspector",
   :arglists ([data]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.inspector-api.html#clojure.inspector/inspect-tree",
   :source-url
   "https://github.com/clojure/clojure/blob/5da21b38470d175b3cecbf93b9cd145ca36940c1/src/clj/clojure/inspector.clj#L91",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/5da21b38470d175b3cecbf93b9cd145ca36940c1/src/clj/clojure/inspector.clj",
   :file "src/clj/clojure/inspector.clj"}
  {:name "parse-timestamp",
   :doc
   "Parse a string containing an RFC3339-like like timestamp.\n\nThe function new-instant is called with the following arguments.\n\n                min  max           default\n                ---  ------------  -------\n  years          0           9999      N/A (s must provide years)\n  months         1             12        1\n  days           1             31        1 (actual max days depends\n  hours          0             23        0  on month and year)\n  minutes        0             59        0\n  seconds        0             60        0 (though 60 is only valid\n  nanoseconds    0      999999999        0  when minutes is 59)\n  offset-sign   -1              1        0\n  offset-hours   0             23        0\n  offset-minutes 0             59        0\n\nThese are all integers and will be non-nil. (The listed defaults\nwill be passed if the corresponding field is not present in s.)\n\nGrammar (of s):\n\n  date-fullyear   = 4DIGIT\n  date-month      = 2DIGIT  ; 01-12\n  date-mday       = 2DIGIT  ; 01-28, 01-29, 01-30, 01-31 based on\n                            ; month/year\n  time-hour       = 2DIGIT  ; 00-23\n  time-minute     = 2DIGIT  ; 00-59\n  time-second     = 2DIGIT  ; 00-58, 00-59, 00-60 based on leap second\n                            ; rules\n  time-secfrac    = '.' 1*DIGIT\n  time-numoffset  = ('+' / '-') time-hour ':' time-minute\n  time-offset     = 'Z' / time-numoffset\n\n  time-part       = time-hour [ ':' time-minute [ ':' time-second\n                    [time-secfrac] [time-offset] ] ]\n\n  timestamp       = date-year [ '-' date-month [ '-' date-mday\n                    [ 'T' time-part ] ] ]\n\nUnlike RFC3339:\n\n  - we only parse the timestamp format\n  - timestamp can elide trailing components\n  - time-offset is optional (defaults to +00:00)\n\nThough time-offset is syntactically optional, a missing time-offset\nwill be treated as if the time-offset zero (+00:00) had been\nspecified.",
   :var-type "function",
   :line 53,
   :namespace "clojure.instant",
   :arglists ([new-instant cs]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.instant-api.html#clojure.instant/parse-timestamp",
   :source-url
   "https://github.com/clojure/clojure/blob/ce1927e74b0c71c93375f1db2758d790a826f32c/src/clj/clojure/instant.clj#L53",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/ce1927e74b0c71c93375f1db2758d790a826f32c/src/clj/clojure/instant.clj",
   :file "src/clj/clojure/instant.clj"}
  {:name "read-instant-calendar",
   :doc
   "To read an instant as a java.util.Calendar, bind *data-readers* to a map with\nthis var as the value for the 'inst key.  Calendar preserves the timezone\noffset.",
   :var-type "function",
   :line 281,
   :namespace "clojure.instant",
   :arglists ([cs]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.instant-api.html#clojure.instant/read-instant-calendar",
   :source-url
   "https://github.com/clojure/clojure/blob/ce1927e74b0c71c93375f1db2758d790a826f32c/src/clj/clojure/instant.clj#L281",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/ce1927e74b0c71c93375f1db2758d790a826f32c/src/clj/clojure/instant.clj",
   :file "src/clj/clojure/instant.clj"}
  {:name "read-instant-date",
   :doc
   "To read an instant as a java.util.Date, bind *data-readers* to a map with\nthis var as the value for the 'inst key. The timezone offset will be used\nto convert into UTC.",
   :var-type "function",
   :line 274,
   :namespace "clojure.instant",
   :arglists ([cs]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.instant-api.html#clojure.instant/read-instant-date",
   :source-url
   "https://github.com/clojure/clojure/blob/ce1927e74b0c71c93375f1db2758d790a826f32c/src/clj/clojure/instant.clj#L274",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/ce1927e74b0c71c93375f1db2758d790a826f32c/src/clj/clojure/instant.clj",
   :file "src/clj/clojure/instant.clj"}
  {:name "read-instant-timestamp",
   :doc
   "To read an instant as a java.sql.Timestamp, bind *data-readers* to a\nmap with this var as the value for the 'inst key. Timestamp preserves\nfractional seconds with nanosecond precision. The timezone offset will\nbe used to convert into UTC.",
   :var-type "function",
   :line 288,
   :namespace "clojure.instant",
   :arglists ([cs]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.instant-api.html#clojure.instant/read-instant-timestamp",
   :source-url
   "https://github.com/clojure/clojure/blob/ce1927e74b0c71c93375f1db2758d790a826f32c/src/clj/clojure/instant.clj#L288",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/ce1927e74b0c71c93375f1db2758d790a826f32c/src/clj/clojure/instant.clj",
   :file "src/clj/clojure/instant.clj"}
  {:name "validated",
   :doc
   "Return a function which constructs an instant by calling constructor\nafter first validating that those arguments are in range and otherwise\nplausible. The resulting function will throw an exception if called\nwith invalid arguments.",
   :var-type "function",
   :line 139,
   :namespace "clojure.instant",
   :arglists ([new-instance]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.instant-api.html#clojure.instant/validated",
   :source-url
   "https://github.com/clojure/clojure/blob/ce1927e74b0c71c93375f1db2758d790a826f32c/src/clj/clojure/instant.clj#L139",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/ce1927e74b0c71c93375f1db2758d790a826f32c/src/clj/clojure/instant.clj",
   :file "src/clj/clojure/instant.clj"}
  {:name "current-basis",
   :doc
   "Return the current basis, which may have been modified since runtime launch.",
   :var-type "function",
   :line 43,
   :added "1.12",
   :namespace "clojure.java.basis",
   :arglists ([]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.basis-api.html#clojure.java.basis/current-basis",
   :source-url
   "https://github.com/clojure/clojure/blob/b7d87dcd729628a2e80038d05023eb7c7786bb11/src/clj/clojure/java/basis.clj#L43",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/b7d87dcd729628a2e80038d05023eb7c7786bb11/src/clj/clojure/java/basis.clj",
   :file "src/clj/clojure/java/basis.clj"}
  {:name "initial-basis",
   :doc
   "Initial runtime basis at launch, nil if unknown (process not started by CLI)",
   :var-type "function",
   :line 37,
   :added "1.12",
   :namespace "clojure.java.basis",
   :arglists ([]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.basis-api.html#clojure.java.basis/initial-basis",
   :source-url
   "https://github.com/clojure/clojure/blob/b7d87dcd729628a2e80038d05023eb7c7786bb11/src/clj/clojure/java/basis.clj#L37",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/b7d87dcd729628a2e80038d05023eb7c7786bb11/src/clj/clojure/java/basis.clj",
   :file "src/clj/clojure/java/basis.clj"}
  {:name "browse-url",
   :doc "Open url in a browser",
   :var-type "function",
   :line 68,
   :added "1.2",
   :namespace "clojure.java.browse",
   :arglists ([url]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.browse-api.html#clojure.java.browse/browse-url",
   :source-url
   "https://github.com/clojure/clojure/blob/19e3a2708def5ffb7f2be030d8e8e895464ce2d2/src/clj/clojure/java/browse.clj#L68",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/19e3a2708def5ffb7f2be030d8e8e895464ce2d2/src/clj/clojure/java/browse.clj",
   :file "src/clj/clojure/java/browse.clj"}
  {:name "as-relative-path",
   :doc
   "Take an as-file-able thing and return a string if it is\na relative path, else IllegalArgumentException.",
   :var-type "function",
   :line 411,
   :added "1.2",
   :namespace "clojure.java.io",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.io-api.html#clojure.java.io/as-relative-path",
   :source-url
   "https://github.com/clojure/clojure/blob/089f27f9fc881674e565f1ffcbe2f8a616726c3b/src/clj/clojure/java/io.clj#L411",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/089f27f9fc881674e565f1ffcbe2f8a616726c3b/src/clj/clojure/java/io.clj",
   :file "src/clj/clojure/java/io.clj"}
  {:name "copy",
   :doc
   "Copies input to output.  Returns nil or throws IOException.\nInput may be an InputStream, Reader, File, byte[], char[], or String.\nOutput may be an OutputStream, Writer, or File.\n\nOptions are key/value pairs and may be one of\n\n  :buffer-size  buffer size to use, default is 1024.\n  :encoding     encoding to use if converting between\n                byte and char streams.   \n\nDoes not close any streams except those it opens itself \n(on a File).",
   :var-type "function",
   :line 394,
   :added "1.2",
   :namespace "clojure.java.io",
   :arglists ([input output & opts]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.io-api.html#clojure.java.io/copy",
   :source-url
   "https://github.com/clojure/clojure/blob/089f27f9fc881674e565f1ffcbe2f8a616726c3b/src/clj/clojure/java/io.clj#L394",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/089f27f9fc881674e565f1ffcbe2f8a616726c3b/src/clj/clojure/java/io.clj",
   :file "src/clj/clojure/java/io.clj"}
  {:name "delete-file",
   :doc
   "Delete file f. If silently is nil or false, raise an exception on failure, else return the value of silently.",
   :var-type "function",
   :line 433,
   :added "1.2",
   :namespace "clojure.java.io",
   :arglists ([f & [silently]]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.io-api.html#clojure.java.io/delete-file",
   :source-url
   "https://github.com/clojure/clojure/blob/089f27f9fc881674e565f1ffcbe2f8a616726c3b/src/clj/clojure/java/io.clj#L433",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/089f27f9fc881674e565f1ffcbe2f8a616726c3b/src/clj/clojure/java/io.clj",
   :file "src/clj/clojure/java/io.clj"}
  {:name "file",
   :doc
   "Returns a java.io.File, passing each arg to as-file.  Multiple-arg\nversions treat the first argument as parent and subsequent args as\nchildren relative to the parent.",
   :var-type "function",
   :line 421,
   :added "1.2",
   :namespace "clojure.java.io",
   :arglists ([arg] [parent child] [parent child & more]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.io-api.html#clojure.java.io/file",
   :source-url
   "https://github.com/clojure/clojure/blob/089f27f9fc881674e565f1ffcbe2f8a616726c3b/src/clj/clojure/java/io.clj#L421",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/089f27f9fc881674e565f1ffcbe2f8a616726c3b/src/clj/clojure/java/io.clj",
   :file "src/clj/clojure/java/io.clj"}
  {:name "input-stream",
   :doc
   "Attempts to coerce its argument into an open java.io.InputStream.\nDefault implementations always return a java.io.BufferedInputStream.\n\nDefault implementations are defined for InputStream, File, URI, URL,\nSocket, byte array, and String arguments.\n\nIf the argument is a String, it tries to resolve it first as a URI, then\nas a local file name.  URIs with a 'file' protocol are converted to\nlocal file names.\n\nShould be used inside with-open to ensure the InputStream is properly\nclosed.",
   :var-type "function",
   :line 124,
   :added "1.2",
   :namespace "clojure.java.io",
   :arglists ([x & opts]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.io-api.html#clojure.java.io/input-stream",
   :source-url
   "https://github.com/clojure/clojure/blob/089f27f9fc881674e565f1ffcbe2f8a616726c3b/src/clj/clojure/java/io.clj#L124",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/089f27f9fc881674e565f1ffcbe2f8a616726c3b/src/clj/clojure/java/io.clj",
   :file "src/clj/clojure/java/io.clj"}
  {:name "make-parents",
   :doc
   "Given the same arg(s) as for file, creates all parent directories of\nthe file they represent.",
   :var-type "function",
   :line 441,
   :added "1.2",
   :namespace "clojure.java.io",
   :arglists ([f & more]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.io-api.html#clojure.java.io/make-parents",
   :source-url
   "https://github.com/clojure/clojure/blob/089f27f9fc881674e565f1ffcbe2f8a616726c3b/src/clj/clojure/java/io.clj#L441",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/089f27f9fc881674e565f1ffcbe2f8a616726c3b/src/clj/clojure/java/io.clj",
   :file "src/clj/clojure/java/io.clj"}
  {:name "output-stream",
   :doc
   "Attempts to coerce its argument into an open java.io.OutputStream.\nDefault implementations always return a java.io.BufferedOutputStream.\n\nDefault implementations are defined for OutputStream, File, URI, URL,\nSocket, and String arguments.\n\nIf the argument is a String, it tries to resolve it first as a URI, then\nas a local file name.  URIs with a 'file' protocol are converted to\nlocal file names.\n\nShould be used inside with-open to ensure the OutputStream is\nproperly closed.",
   :var-type "function",
   :line 141,
   :added "1.2",
   :namespace "clojure.java.io",
   :arglists ([x & opts]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.io-api.html#clojure.java.io/output-stream",
   :source-url
   "https://github.com/clojure/clojure/blob/089f27f9fc881674e565f1ffcbe2f8a616726c3b/src/clj/clojure/java/io.clj#L141",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/089f27f9fc881674e565f1ffcbe2f8a616726c3b/src/clj/clojure/java/io.clj",
   :file "src/clj/clojure/java/io.clj"}
  {:name "reader",
   :doc
   "Attempts to coerce its argument into an open java.io.Reader.\nDefault implementations always return a java.io.BufferedReader.\n\nDefault implementations are provided for Reader, BufferedReader,\nInputStream, File, URI, URL, Socket, byte arrays, character arrays,\nand String.\n\nIf argument is a String, it tries to resolve it first as a URI, then\nas a local file name.  URIs with a 'file' protocol are converted to\nlocal file names.\n\nShould be used inside with-open to ensure the Reader is properly\nclosed.",
   :var-type "function",
   :line 89,
   :added "1.2",
   :namespace "clojure.java.io",
   :arglists ([x & opts]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.io-api.html#clojure.java.io/reader",
   :source-url
   "https://github.com/clojure/clojure/blob/089f27f9fc881674e565f1ffcbe2f8a616726c3b/src/clj/clojure/java/io.clj#L89",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/089f27f9fc881674e565f1ffcbe2f8a616726c3b/src/clj/clojure/java/io.clj",
   :file "src/clj/clojure/java/io.clj"}
  {:name "resource",
   :doc
   "Returns the URL for a named resource. Use the context class loader\nif no loader is specified.",
   :var-type "function",
   :line 449,
   :added "1.2",
   :namespace "clojure.java.io",
   :arglists ([n] [n loader]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.io-api.html#clojure.java.io/resource",
   :source-url
   "https://github.com/clojure/clojure/blob/089f27f9fc881674e565f1ffcbe2f8a616726c3b/src/clj/clojure/java/io.clj#L449",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/089f27f9fc881674e565f1ffcbe2f8a616726c3b/src/clj/clojure/java/io.clj",
   :file "src/clj/clojure/java/io.clj"}
  {:name "writer",
   :doc
   "Attempts to coerce its argument into an open java.io.Writer.\nDefault implementations always return a java.io.BufferedWriter.\n\nDefault implementations are provided for Writer, BufferedWriter,\nOutputStream, File, URI, URL, Socket, and String.\n\nIf the argument is a String, it tries to resolve it first as a URI, then\nas a local file name.  URIs with a 'file' protocol are converted to\nlocal file names.\n\nShould be used inside with-open to ensure the Writer is properly\nclosed.",
   :var-type "function",
   :line 107,
   :added "1.2",
   :namespace "clojure.java.io",
   :arglists ([x & opts]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.io-api.html#clojure.java.io/writer",
   :source-url
   "https://github.com/clojure/clojure/blob/089f27f9fc881674e565f1ffcbe2f8a616726c3b/src/clj/clojure/java/io.clj#L107",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/089f27f9fc881674e565f1ffcbe2f8a616726c3b/src/clj/clojure/java/io.clj",
   :file "src/clj/clojure/java/io.clj"}
  {:name "Coercions",
   :doc "Coerce between various 'resource-namish' things.",
   :var-type "protocol",
   :line 38,
   :added "1.2",
   :namespace "clojure.java.io",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.io-api.html#clojure.java.io/Coercions",
   :source-url
   "https://github.com/clojure/clojure/blob/089f27f9fc881674e565f1ffcbe2f8a616726c3b/src/clj/clojure/java/io.clj#L38",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/089f27f9fc881674e565f1ffcbe2f8a616726c3b/src/clj/clojure/java/io.clj",
   :file "src/clj/clojure/java/io.clj"}
  {:name "IOFactory",
   :doc
   "Factory functions that create ready-to-use, buffered versions of\nthe various Java I/O stream types, on top of anything that can\nbe unequivocally converted to the requested kind of stream.\n\nCommon options include\n\n  :append    true to open stream in append mode\n  :encoding  string name of encoding to use, e.g. \"UTF-8\".\n\nCallers should generally prefer the higher level API provided by\nreader, writer, input-stream, and output-stream.",
   :var-type "protocol",
   :line 72,
   :added "1.2",
   :namespace "clojure.java.io",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.io-api.html#clojure.java.io/IOFactory",
   :source-url
   "https://github.com/clojure/clojure/blob/089f27f9fc881674e565f1ffcbe2f8a616726c3b/src/clj/clojure/java/io.clj#L72",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/089f27f9fc881674e565f1ffcbe2f8a616726c3b/src/clj/clojure/java/io.clj",
   :file "src/clj/clojure/java/io.clj"}
  {:name "as-file",
   :doc "Coerce argument to a file.",
   :var-type "function",
   :added "1.2",
   :namespace "clojure.java.io",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.io-api.html#clojure.java.io/as-file",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "as-url",
   :doc "Coerce argument to a URL.",
   :var-type "function",
   :added "1.2",
   :namespace "clojure.java.io",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.io-api.html#clojure.java.io/as-url",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "make-input-stream",
   :doc "Creates a BufferedInputStream. See also IOFactory docs.",
   :var-type "function",
   :added "1.2",
   :namespace "clojure.java.io",
   :arglists ([x opts]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.io-api.html#clojure.java.io/make-input-stream",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "make-output-stream",
   :doc "Creates a BufferedOutputStream. See also IOFactory docs.",
   :var-type "function",
   :added "1.2",
   :namespace "clojure.java.io",
   :arglists ([x opts]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.io-api.html#clojure.java.io/make-output-stream",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "make-reader",
   :doc "Creates a BufferedReader. See also IOFactory docs.",
   :var-type "function",
   :added "1.2",
   :namespace "clojure.java.io",
   :arglists ([x opts]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.io-api.html#clojure.java.io/make-reader",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "make-writer",
   :doc "Creates a BufferedWriter. See also IOFactory docs.",
   :var-type "function",
   :added "1.2",
   :namespace "clojure.java.io",
   :arglists ([x opts]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.io-api.html#clojure.java.io/make-writer",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "add-local-javadoc",
   :doc "Adds to the list of local Javadoc paths.",
   :var-type "function",
   :line 47,
   :added "1.2",
   :namespace "clojure.java.javadoc",
   :arglists ([path]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.javadoc-api.html#clojure.java.javadoc/add-local-javadoc",
   :source-url
   "https://github.com/clojure/clojure/blob/3b6c31bc503afe8f25d01d6d7d05ebc960095abd/src/clj/clojure/java/javadoc.clj#L47",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3b6c31bc503afe8f25d01d6d7d05ebc960095abd/src/clj/clojure/java/javadoc.clj",
   :file "src/clj/clojure/java/javadoc.clj"}
  {:name "add-remote-javadoc",
   :doc
   "Adds to the list of remote Javadoc URLs.  package-prefix is the\nbeginning of the package name that has docs at this URL.",
   :var-type "function",
   :line 53,
   :added "1.2",
   :namespace "clojure.java.javadoc",
   :arglists ([package-prefix url]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.javadoc-api.html#clojure.java.javadoc/add-remote-javadoc",
   :source-url
   "https://github.com/clojure/clojure/blob/3b6c31bc503afe8f25d01d6d7d05ebc960095abd/src/clj/clojure/java/javadoc.clj#L53",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3b6c31bc503afe8f25d01d6d7d05ebc960095abd/src/clj/clojure/java/javadoc.clj",
   :file "src/clj/clojure/java/javadoc.clj"}
  {:name "javadoc",
   :doc
   "Opens a browser window displaying the javadoc for the argument.\nTries *local-javadocs* first, then *remote-javadocs*.",
   :var-type "function",
   :line 92,
   :added "1.2",
   :namespace "clojure.java.javadoc",
   :arglists ([class-or-object]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.javadoc-api.html#clojure.java.javadoc/javadoc",
   :source-url
   "https://github.com/clojure/clojure/blob/3b6c31bc503afe8f25d01d6d7d05ebc960095abd/src/clj/clojure/java/javadoc.clj#L92",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3b6c31bc503afe8f25d01d6d7d05ebc960095abd/src/clj/clojure/java/javadoc.clj",
   :file "src/clj/clojure/java/javadoc.clj"}
  {:name "exec",
   :doc
   "Execute a command and on successful exit, return the captured output,\nelse throw RuntimeException. Args are the same as 'start' and options\nif supplied override the default 'exec' settings.",
   :var-type "function",
   :line 163,
   :added "1.12",
   :namespace "clojure.java.process",
   :arglists ([& opts+args]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.process-api.html#clojure.java.process/exec",
   :source-url
   "https://github.com/clojure/clojure/blob/afbd2098bf741234ec9efaa25480d02b14863e8c/src/clj/clojure/java/process.clj#L163",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/afbd2098bf741234ec9efaa25480d02b14863e8c/src/clj/clojure/java/process.clj",
   :file "src/clj/clojure/java/process.clj"}
  {:name "exit-ref",
   :doc
   "Given a Process (the output of 'start'), return a reference that can be\nused to wait for process completion then returns the exit value.",
   :var-type "function",
   :line 114,
   :added "1.12",
   :namespace "clojure.java.process",
   :arglists ([process]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.process-api.html#clojure.java.process/exit-ref",
   :source-url
   "https://github.com/clojure/clojure/blob/afbd2098bf741234ec9efaa25480d02b14863e8c/src/clj/clojure/java/process.clj#L114",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/afbd2098bf741234ec9efaa25480d02b14863e8c/src/clj/clojure/java/process.clj",
   :file "src/clj/clojure/java/process.clj"}
  {:name "from-file",
   :doc
   "Coerce f to a file per clojure.java.io/file and return a ProcessBuilder.Redirect reading from the file.\nThis can be passed to 'start' in :in.",
   :var-type "function",
   :line 46,
   :added "1.12",
   :namespace "clojure.java.process",
   :arglists ([f]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.process-api.html#clojure.java.process/from-file",
   :source-url
   "https://github.com/clojure/clojure/blob/afbd2098bf741234ec9efaa25480d02b14863e8c/src/clj/clojure/java/process.clj#L46",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/afbd2098bf741234ec9efaa25480d02b14863e8c/src/clj/clojure/java/process.clj",
   :file "src/clj/clojure/java/process.clj"}
  {:name "start",
   :doc
   "Start an external command, defined in args.\nThe process environment vars are inherited from the parent by\ndefault (use :clear-env to clear them).\n\nIf needed, provide options in map as first arg:\n  :in - a ProcessBuilder.Redirect (default = :pipe) or :inherit\n  :out - a ProcessBuilder.Redirect (default = :pipe) or :inherit :discard\n  :err - a ProcessBuilder.Redirect (default = :pipe) or :inherit :discard :stdout\n  :dir - current directory when the process runs (default=\".\")\n  :clear-env - if true, remove all inherited parent env vars\n  :env - {env-var value} of environment variables to set (all strings)\n\nReturns the java.lang.Process.",
   :var-type "function",
   :line 53,
   :added "1.12",
   :namespace "clojure.java.process",
   :arglists ([& opts+args]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.process-api.html#clojure.java.process/start",
   :source-url
   "https://github.com/clojure/clojure/blob/afbd2098bf741234ec9efaa25480d02b14863e8c/src/clj/clojure/java/process.clj#L53",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/afbd2098bf741234ec9efaa25480d02b14863e8c/src/clj/clojure/java/process.clj",
   :file "src/clj/clojure/java/process.clj"}
  {:name "stderr",
   :doc
   "Given a process, return the stderr of the external process (an InputStream)",
   :var-type "function",
   :line 108,
   :added "1.12",
   :namespace "clojure.java.process",
   :arglists ([process]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.process-api.html#clojure.java.process/stderr",
   :source-url
   "https://github.com/clojure/clojure/blob/afbd2098bf741234ec9efaa25480d02b14863e8c/src/clj/clojure/java/process.clj#L108",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/afbd2098bf741234ec9efaa25480d02b14863e8c/src/clj/clojure/java/process.clj",
   :file "src/clj/clojure/java/process.clj"}
  {:name "stdin",
   :doc
   "Given a process, return the stdin of the external process (an OutputStream)",
   :var-type "function",
   :line 96,
   :added "1.12",
   :namespace "clojure.java.process",
   :arglists ([process]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.process-api.html#clojure.java.process/stdin",
   :source-url
   "https://github.com/clojure/clojure/blob/afbd2098bf741234ec9efaa25480d02b14863e8c/src/clj/clojure/java/process.clj#L96",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/afbd2098bf741234ec9efaa25480d02b14863e8c/src/clj/clojure/java/process.clj",
   :file "src/clj/clojure/java/process.clj"}
  {:name "stdout",
   :doc
   "Given a process, return the stdout of the external process (an InputStream)",
   :var-type "function",
   :line 102,
   :added "1.12",
   :namespace "clojure.java.process",
   :arglists ([process]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.process-api.html#clojure.java.process/stdout",
   :source-url
   "https://github.com/clojure/clojure/blob/afbd2098bf741234ec9efaa25480d02b14863e8c/src/clj/clojure/java/process.clj#L102",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/afbd2098bf741234ec9efaa25480d02b14863e8c/src/clj/clojure/java/process.clj",
   :file "src/clj/clojure/java/process.clj"}
  {:name "to-file",
   :doc
   "Coerce f to a file per clojure.java.io/file and return a ProcessBuilder.Redirect writing to the file.\nSet ':append' in opts to append. This can be passed to 'start' in :out or :err.",
   :var-type "function",
   :line 36,
   :added "1.12",
   :namespace "clojure.java.process",
   :arglists ([f & {:keys [append], :as opts}]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.process-api.html#clojure.java.process/to-file",
   :source-url
   "https://github.com/clojure/clojure/blob/afbd2098bf741234ec9efaa25480d02b14863e8c/src/clj/clojure/java/process.clj#L36",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/afbd2098bf741234ec9efaa25480d02b14863e8c/src/clj/clojure/java/process.clj",
   :file "src/clj/clojure/java/process.clj"}
  {:name "sh",
   :doc
   "Passes the given strings to Runtime.exec() to launch a sub-process.\n\nOptions are\n\n:in      may be given followed by any legal input source for\n         clojure.java.io/copy, e.g. InputStream, Reader, File, byte[],\n         or String, to be fed to the sub-process's stdin.\n:in-enc  option may be given followed by a String, used as a character\n         encoding name (for example \"UTF-8\" or \"ISO-8859-1\") to\n         convert the input string specified by the :in option to the\n         sub-process's stdin.  Defaults to UTF-8.\n         If the :in option provides a byte array, then the bytes are passed\n         unencoded, and this option is ignored.\n:out-enc option may be given followed by :bytes or a String. If a\n         String is given, it will be used as a character encoding\n         name (for example \"UTF-8\" or \"ISO-8859-1\") to convert\n         the sub-process's stdout to a String which is returned.\n         If :bytes is given, the sub-process's stdout will be stored\n         in a byte array and returned.  Defaults to UTF-8.\n:env     override the process env with a map (or the underlying Java\n         String[] if you are a masochist).\n:dir     override the process dir with a String or java.io.File.\n\nYou can bind :env or :dir for multiple operations using with-sh-env\nand with-sh-dir.\n\nsh returns a map of\n  :exit => sub-process's exit code\n  :out  => sub-process's stdout (as byte[] or String)\n  :err  => sub-process's stderr (String via platform default encoding)",
   :var-type "function",
   :line 79,
   :added "1.2",
   :namespace "clojure.java.shell",
   :arglists ([& args]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.shell-api.html#clojure.java.shell/sh",
   :source-url
   "https://github.com/clojure/clojure/blob/027d8ff2859442b222bf9cfa4c1be45567b788eb/src/clj/clojure/java/shell.clj#L79",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/027d8ff2859442b222bf9cfa4c1be45567b788eb/src/clj/clojure/java/shell.clj",
   :file "src/clj/clojure/java/shell.clj"}
  {:name "with-sh-dir",
   :doc "Sets the directory for use with sh, see sh for details.",
   :var-type "macro",
   :line 21,
   :added "1.2",
   :namespace "clojure.java.shell",
   :arglists ([dir & forms]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.shell-api.html#clojure.java.shell/with-sh-dir",
   :source-url
   "https://github.com/clojure/clojure/blob/027d8ff2859442b222bf9cfa4c1be45567b788eb/src/clj/clojure/java/shell.clj#L21",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/027d8ff2859442b222bf9cfa4c1be45567b788eb/src/clj/clojure/java/shell.clj",
   :file "src/clj/clojure/java/shell.clj"}
  {:name "with-sh-env",
   :doc "Sets the environment for use with sh, see sh for details.",
   :var-type "macro",
   :line 28,
   :added "1.2",
   :namespace "clojure.java.shell",
   :arglists ([env & forms]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.shell-api.html#clojure.java.shell/with-sh-env",
   :source-url
   "https://github.com/clojure/clojure/blob/027d8ff2859442b222bf9cfa4c1be45567b788eb/src/clj/clojure/java/shell.clj#L28",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/027d8ff2859442b222bf9cfa4c1be45567b788eb/src/clj/clojure/java/shell.clj",
   :file "src/clj/clojure/java/shell.clj"}
  {:name "demunge",
   :doc
   "Given a string representation of a fn class,\nas in a stack trace element, returns a readable version.",
   :var-type "function",
   :line 28,
   :added "1.3",
   :namespace "clojure.main",
   :arglists ([fn-name]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/demunge",
   :source-url
   "https://github.com/clojure/clojure/blob/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj#L28",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj",
   :file "src/clj/clojure/main.clj"}
  {:name "err->msg",
   :doc "Helper to return an error message string from an exception.",
   :var-type "function",
   :line 342,
   :namespace "clojure.main",
   :arglists ([e]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/err->msg",
   :source-url
   "https://github.com/clojure/clojure/blob/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj#L342",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj",
   :file "src/clj/clojure/main.clj"}
  {:name "ex-str",
   :doc
   "Returns a string from exception data, as produced by ex-triage.\nThe first line summarizes the exception phase and location.\nThe subsequent lines describe the cause.",
   :var-type "function",
   :line 268,
   :added "1.10",
   :namespace "clojure.main",
   :arglists
   ([{:clojure.error/keys
      [phase source path line column symbol class cause spec],
      :as triage-data}]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/ex-str",
   :source-url
   "https://github.com/clojure/clojure/blob/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj#L268",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj",
   :file "src/clj/clojure/main.clj"}
  {:name "ex-triage",
   :doc
   "Returns an analysis of the phase, error, cause, and location of an error that occurred\nbased on Throwable data, as returned by Throwable->map. All attributes other than phase\nare optional:\n  :clojure.error/phase - keyword phase indicator, one of:\n    :read-source :compile-syntax-check :compilation :macro-syntax-check :macroexpansion\n    :execution :read-eval-result :print-eval-result\n  :clojure.error/source - file name (no path)\n  :clojure.error/path - source path\n  :clojure.error/line - integer line number\n  :clojure.error/column - integer column number\n  :clojure.error/symbol - symbol being expanded/compiled/invoked\n  :clojure.error/class - cause exception class symbol\n  :clojure.error/cause - cause exception message\n  :clojure.error/spec - explain-data for spec error",
   :var-type "function",
   :line 207,
   :added "1.10",
   :namespace "clojure.main",
   :arglists ([datafied-throwable]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/ex-triage",
   :source-url
   "https://github.com/clojure/clojure/blob/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj#L207",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj",
   :file "src/clj/clojure/main.clj"}
  {:name "load-script",
   :doc
   "Loads Clojure source from a file or resource given its path. Paths\nbeginning with @ or @/ are considered relative to classpath.",
   :var-type "function",
   :line 469,
   :namespace "clojure.main",
   :arglists ([path]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/load-script",
   :source-url
   "https://github.com/clojure/clojure/blob/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj#L469",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj",
   :file "src/clj/clojure/main.clj"}
  {:name "main",
   :doc
   "Usage: java -cp clojure.jar clojure.main [init-opt*] [main-opt] [arg*]\n\nWith no options or args, runs an interactive Read-Eval-Print Loop\n\ninit options:\n  -i, --init path     Load a file or resource\n  -e, --eval string   Evaluate expressions in string; print non-nil values\n  --report target     Report uncaught exception to \"file\" (default), \"stderr\",\n                      or \"none\", overrides System property clojure.main.report\n\nmain options:\n  -m, --main ns-name  Call the -main function from a namespace with args\n  -r, --repl          Run a repl\n  path                Run a script from a file or resource\n  -                   Run a script from standard input\n  -h, -?, --help      Print this help message and exit\n\noperation:\n\n  - Establishes thread-local bindings for commonly set!-able vars\n  - Enters the user namespace\n  - Binds *command-line-args* to a seq of strings containing command line\n    args that appear after any main option\n  - Runs all init options in order\n  - Calls a -main function or runs a repl or script if requested\n\nThe init options may be repeated and mixed freely, but must appear before\nany main option. The appearance of any eval option before running a repl\nsuppresses the usual repl greeting message: \"Clojure ~(clojure-version)\".\n\nPaths may be absolute or relative in the filesystem or relative to\nclasspath. Classpath-relative paths have prefix of @ or @/",
   :var-type "function",
   :line 617,
   :namespace "clojure.main",
   :arglists ([& args]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/main",
   :source-url
   "https://github.com/clojure/clojure/blob/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj#L617",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj",
   :file "src/clj/clojure/main.clj"}
  {:name "renumbering-read",
   :doc
   "Reads from reader, which must be a LineNumberingPushbackReader, while capturing\nthe read string. If the read is successful, reset the line number and re-read.\nThe line number on re-read is the passed line-number unless :line or\n:clojure.core/eval-file meta are explicitly set on the read value.",
   :var-type "function",
   :line 139,
   :added "1.10",
   :namespace "clojure.main",
   :arglists ([opts reader line-number]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/renumbering-read",
   :source-url
   "https://github.com/clojure/clojure/blob/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj#L139",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj",
   :file "src/clj/clojure/main.clj"}
  {:name "repl",
   :doc
   "Generic, reusable, read-eval-print loop. By default, reads from *in*,\nwrites to *out*, and prints exception summaries to *err*. If you use the\ndefault :read hook, *in* must either be an instance of\nLineNumberingPushbackReader or duplicate its behavior of both supporting\n.unread and collapsing CR, LF, and CRLF into a single \\newline. Options\nare sequential keyword-value pairs. Available options and their defaults:\n\n   - :init, function of no arguments, initialization hook called with\n     bindings for set!-able vars in place.\n     default: #()\n\n   - :need-prompt, function of no arguments, called before each\n     read-eval-print except the first, the user will be prompted if it\n     returns true.\n     default: (if (instance? LineNumberingPushbackReader *in*)\n                #(.atLineStart *in*)\n                #(identity true))\n\n   - :prompt, function of no arguments, prompts for more input.\n     default: repl-prompt\n\n   - :flush, function of no arguments, flushes output\n     default: flush\n\n   - :read, function of two arguments, reads from *in*:\n       - returns its first argument to request a fresh prompt\n         - depending on need-prompt, this may cause the repl to prompt\n           before reading again\n       - returns its second argument to request an exit from the repl\n       - else returns the next object read from the input stream\n     default: repl-read\n\n   - :eval, function of one argument, returns the evaluation of its\n     argument\n     default: eval\n\n   - :print, function of one argument, prints its argument to the output\n     default: prn\n\n   - :caught, function of one argument, a throwable, called when\n     read, eval, or print throws an exception or error\n     default: repl-caught",
   :var-type "function",
   :line 368,
   :namespace "clojure.main",
   :arglists ([& options]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/repl",
   :source-url
   "https://github.com/clojure/clojure/blob/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj#L368",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj",
   :file "src/clj/clojure/main.clj"}
  {:name "repl-caught",
   :doc "Default :caught hook for repl",
   :var-type "function",
   :line 347,
   :namespace "clojure.main",
   :arglists ([e]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/repl-caught",
   :source-url
   "https://github.com/clojure/clojure/blob/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj#L347",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj",
   :file "src/clj/clojure/main.clj"}
  {:name "repl-exception",
   :doc "Returns the root cause of throwables",
   :var-type "function",
   :line 171,
   :namespace "clojure.main",
   :arglists ([throwable]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/repl-exception",
   :source-url
   "https://github.com/clojure/clojure/blob/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj#L171",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj",
   :file "src/clj/clojure/main.clj"}
  {:name "repl-prompt",
   :doc "Default :prompt hook for repl",
   :var-type "function",
   :line 102,
   :namespace "clojure.main",
   :arglists ([]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/repl-prompt",
   :source-url
   "https://github.com/clojure/clojure/blob/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj#L102",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj",
   :file "src/clj/clojure/main.clj"}
  {:name "repl-read",
   :doc
   "Default :read hook for repl. Reads from *in* which must either be an\ninstance of LineNumberingPushbackReader or duplicate its behavior of both\nsupporting .unread and collapsing all of CR, LF, and CRLF into a single\n\\newline. repl-read:\n  - skips whitespace, then\n    - returns request-prompt on start of line, or\n    - returns request-exit on end of stream, or\n    - reads an object from the input stream, then\n      - skips the next input character if it's end of line, then\n      - returns the object.",
   :var-type "function",
   :line 153,
   :namespace "clojure.main",
   :arglists ([request-prompt request-exit]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/repl-read",
   :source-url
   "https://github.com/clojure/clojure/blob/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj#L153",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj",
   :file "src/clj/clojure/main.clj"}
  {:name "repl-requires",
   :doc
   "A sequence of lib specs that are applied to `require`\nby default when a new command-line REPL is started.",
   :var-type "var",
   :line 354,
   :namespace "clojure.main",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/repl-requires",
   :source-url
   "https://github.com/clojure/clojure/blob/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj#L354",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj",
   :file "src/clj/clojure/main.clj"}
  {:name "report-error",
   :doc
   "Create and output an exception report for a Throwable to target.\n\nOptions:\n  :target - \"file\" (default), \"stderr\", \"none\"\n\nIf file is specified but cannot be written, falls back to stderr.",
   :var-type "function",
   :line 585,
   :namespace "clojure.main",
   :arglists ([t & {:keys [target], :or {target "file"}, :as opts}]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/report-error",
   :source-url
   "https://github.com/clojure/clojure/blob/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj#L585",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj",
   :file "src/clj/clojure/main.clj"}
  {:name "root-cause",
   :doc
   "Returns the initial cause of an exception or error by peeling off all of\nits wrappers",
   :var-type "function",
   :line 35,
   :added "1.3",
   :namespace "clojure.main",
   :arglists ([t]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/root-cause",
   :source-url
   "https://github.com/clojure/clojure/blob/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj#L35",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj",
   :file "src/clj/clojure/main.clj"}
  {:name "skip-if-eol",
   :doc
   "If the next character on stream s is a newline, skips it, otherwise\nleaves the stream untouched. Returns :line-start, :stream-end, or :body\nto indicate the relative location of the next character on s. The stream\nmust either be an instance of LineNumberingPushbackReader or duplicate\nits behavior of both supporting .unread and collapsing all of CR, LF, and\nCRLF to a single \\newline.",
   :var-type "function",
   :line 107,
   :namespace "clojure.main",
   :arglists ([s]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/skip-if-eol",
   :source-url
   "https://github.com/clojure/clojure/blob/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj#L107",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj",
   :file "src/clj/clojure/main.clj"}
  {:name "skip-whitespace",
   :doc
   "Skips whitespace characters on stream s. Returns :line-start, :stream-end,\nor :body to indicate the relative location of the next character on s.\nInterprets comma as whitespace and semicolon as comment to end of line.\nDoes not interpret #! as comment to end of line because only one\ncharacter of lookahead is available. The stream must either be an\ninstance of LineNumberingPushbackReader or duplicate its behavior of both\nsupporting .unread and collapsing all of CR, LF, and CRLF to a single\n\\newline.",
   :var-type "function",
   :line 121,
   :namespace "clojure.main",
   :arglists ([s]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/skip-whitespace",
   :source-url
   "https://github.com/clojure/clojure/blob/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj#L121",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj",
   :file "src/clj/clojure/main.clj"}
  {:name "stack-element-str",
   :doc
   "Returns a (possibly unmunged) string representation of a StackTraceElement",
   :var-type "function",
   :line 62,
   :added "1.3",
   :namespace "clojure.main",
   :arglists ([el]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/stack-element-str",
   :source-url
   "https://github.com/clojure/clojure/blob/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj#L62",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj",
   :file "src/clj/clojure/main.clj"}
  {:name "with-bindings",
   :doc
   "Executes body in the context of thread-local bindings for several vars\nthat often need to be set!: *ns* *warn-on-reflection* *math-context*\n*print-meta* *print-length* *print-level* *compile-path*\n*command-line-args* *1 *2 *3 *e",
   :var-type "macro",
   :line 76,
   :namespace "clojure.main",
   :arglists ([& body]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/with-bindings",
   :source-url
   "https://github.com/clojure/clojure/blob/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj#L76",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj",
   :file "src/clj/clojure/main.clj"}
  {:name "with-read-known",
   :doc
   "Evaluates body with *read-eval* set to a \"known\" value,\ni.e. substituting true for :unknown if necessary.",
   :var-type "macro",
   :line 361,
   :namespace "clojure.main",
   :arglists ([& body]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/with-read-known",
   :source-url
   "https://github.com/clojure/clojure/blob/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj#L361",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/b81660ee9cc50d830c33f943dbda8606ebac3806/src/clj/clojure/main.clj",
   :file "src/clj/clojure/main.clj"}
  {:name "E",
   :doc
   "Constant for e, the base for natural logarithms.\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#E",
   :var-type "var",
   :line 24,
   :added "1.11",
   :namespace "clojure.math",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/E",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L24",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "IEEE-remainder",
   :doc
   "Returns the remainder per IEEE 754 such that\n  remainder = dividend - divisor * n\nwhere n is the integer closest to the exact value of dividend / divisor.\nIf two integers are equally close, then n is the even one.\nIf the remainder is zero, sign will match dividend.\nIf dividend or divisor is ##NaN, or dividend is ##Inf or ##-Inf, or divisor is zero => ##NaN\nIf dividend is finite and divisor is infinite => dividend\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#IEEEremainder-double-double-",
   :var-type "function",
   :line 184,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([dividend divisor]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/IEEE-remainder",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L184",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "PI",
   :doc
   "Constant for pi, the ratio of the circumference of a circle to its diameter.\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#PI",
   :var-type "var",
   :line 33,
   :added "1.11",
   :namespace "clojure.math",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/PI",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L33",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "acos",
   :doc
   "Returns the arc cosine of a, in the range 0.0 to pi.\nIf a is ##NaN or |a|>1 => ##NaN\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#acos-double-",
   :var-type "function",
   :line 85,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([a]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/acos",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L85",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "add-exact",
   :doc
   "Returns the sum of x and y, throws ArithmeticException on overflow.\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#addExact-long-long-",
   :var-type "function",
   :line 277,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([x y]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/add-exact",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L277",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "asin",
   :doc
   "Returns the arc sine of an angle, in the range -pi/2 to pi/2.\nIf a is ##NaN or |a|>1 => ##NaN\nIf a is zero => zero with the same sign as a\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#asin-double-",
   :var-type "function",
   :line 74,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([a]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/asin",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L74",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "atan",
   :doc
   "Returns the arc tangent of a, in the range of -pi/2 to pi/2.\nIf a is ##NaN => ##NaN\nIf a is zero => zero with the same sign as a\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#atan-double-",
   :var-type "function",
   :line 95,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([a]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/atan",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L95",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "atan2",
   :doc
   "Returns the angle theta from the conversion of rectangular coordinates (x, y) to polar coordinates (r, theta).\nComputes the phase theta by computing an arc tangent of y/x in the range of -pi to pi.\nFor more details on special cases, see:\nhttps://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#atan2-double-double-",
   :var-type "function",
   :line 233,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([y x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/atan2",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L233",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "cbrt",
   :doc
   "Returns the cube root of a.\nIf a is ##NaN => ##NaN\nIf a is ##Inf or ##-Inf => a\nIf a is zero => zero with sign matching a\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#cbrt-double-",
   :var-type "function",
   :line 172,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([a]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/cbrt",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L172",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "ceil",
   :doc
   "Returns the smallest double greater than or equal to a, and equal to a\nmathematical integer.\nIf a is ##NaN or ##Inf or ##-Inf or already equal to an integer => a\nIf a is less than zero but greater than -1.0 => -0.0\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#ceil-double-",
   :var-type "function",
   :line 199,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([a]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/ceil",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L199",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "copy-sign",
   :doc
   "Returns a double with the magnitude of the first argument and the sign of\nthe second.\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#copySign-double-double-",
   :var-type "function",
   :line 448,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([magnitude sign]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/copy-sign",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L448",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "cos",
   :doc
   "Returns the cosine of an angle.\nIf a is ##NaN, ##-Inf, ##Inf => ##NaN\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#cos-double-",
   :var-type "function",
   :line 53,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([a]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/cos",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L53",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "cosh",
   :doc
   "Returns the hyperbolic cosine of x, (e^x + e^-x)/2.\nIf x is ##NaN => ##NaN\nIf x is ##Inf or ##-Inf => ##Inf\nIf x is zero => 1.0\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#cosh-double-",
   :var-type "function",
   :line 385,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/cosh",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L385",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "decrement-exact",
   :doc
   "Returns a decremented by 1, throws ArithmeticException on overflow.\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#decrementExact-long-",
   :var-type "function",
   :line 313,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([a]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/decrement-exact",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L313",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "exp",
   :doc
   "Returns Euler's number e raised to the power of a.\nIf a is ##NaN => ##NaN\nIf a is ##Inf => ##Inf\nIf a is ##-Inf => +0.0\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#exp-double-",
   :var-type "function",
   :line 124,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([a]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/exp",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L124",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "expm1",
   :doc
   "Returns e^x - 1. Near 0, expm1(x)+1 is more accurate to e^x than exp(x).\nIf x is ##NaN => ##NaN\nIf x is ##Inf => #Inf\nIf x is ##-Inf => -1.0\nIf x is zero => x\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#expm1-double-",
   :var-type "function",
   :line 421,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/expm1",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L421",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "floor",
   :doc
   "Returns the largest double less than or equal to a, and equal to a\nmathematical integer.\nIf a is ##NaN or ##Inf or ##-Inf or already equal to an integer => a\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#floor-double-",
   :var-type "function",
   :line 211,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([a]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/floor",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L211",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "floor-div",
   :doc
   "Integer division that rounds to negative infinity (as opposed to zero).\nThe special case (floorDiv Long/MIN_VALUE -1) overflows and returns Long/MIN_VALUE.\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#floorDiv-long-long-",
   :var-type "function",
   :line 331,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([x y]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/floor-div",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L331",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "floor-mod",
   :doc
   "Integer modulus x - (floorDiv(x, y) * y). Sign matches y and is in the\nrange -|y| < r < |y|.\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#floorMod-long-long-",
   :var-type "function",
   :line 341,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([x y]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/floor-mod",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L341",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "get-exponent",
   :doc
   "Returns the exponent of d.\nIf d is ##NaN, ##Inf, ##-Inf => Double/MAX_EXPONENT + 1\nIf d is zero or subnormal => Double/MIN_EXPONENT - 1\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#getExponent-double-",
   :var-type "function",
   :line 458,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([d]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/get-exponent",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L458",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "hypot",
   :doc
   "Returns sqrt(x^2 + y^2) without intermediate underflow or overflow.\nIf x or y is ##Inf or ##-Inf => ##Inf\nIf x or y is ##NaN and neither is ##Inf or ##-Inf => ##NaN\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#hypot-double-double-",
   :var-type "function",
   :line 410,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([x y]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/hypot",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L410",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "increment-exact",
   :doc
   "Returns a incremented by 1, throws ArithmeticException on overflow.\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#incrementExact-long-",
   :var-type "function",
   :line 304,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([a]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/increment-exact",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L304",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "log",
   :doc
   "Returns the natural logarithm (base e) of a.\nIf a is ##NaN or negative => ##NaN\nIf a is ##Inf => ##Inf\nIf a is zero => ##-Inf\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#log-double-",
   :var-type "function",
   :line 136,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([a]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/log",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L136",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "log10",
   :doc
   "Returns the logarithm (base 10) of a.\nIf a is ##NaN or negative => ##NaN\nIf a is ##Inf => ##Inf\nIf a is zero => ##-Inf\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#log10-double-",
   :var-type "function",
   :line 148,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([a]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/log10",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L148",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "log1p",
   :doc
   "Returns ln(1+x). For small values of x, log1p(x) is more accurate than\nlog(1.0+x).\nIf x is ##NaN or < -1 => ##NaN\nIf x is ##Inf => ##Inf\nIf x is -1 => ##-Inf\nIf x is 0 => 0 with sign matching x\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#log1p-double-",
   :var-type "function",
   :line 434,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/log1p",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L434",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "multiply-exact",
   :doc
   "Returns the product of x and y, throws ArithmeticException on overflow.\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#multiplyExact-long-long-",
   :var-type "function",
   :line 295,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([x y]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/multiply-exact",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L295",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "negate-exact",
   :doc
   "Returns the negation of a, throws ArithmeticException on overflow.\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#negateExact-long-",
   :var-type "function",
   :line 322,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([a]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/negate-exact",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L322",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "next-after",
   :doc
   "Returns the adjacent floating point number to start in the direction of\nthe second argument. If the arguments are equal, the second is returned.\nIf either arg is #NaN => #NaN\nIf both arguments are signed zeros => direction\nIf start is +-Double/MIN_VALUE and direction would cause a smaller magnitude\n  => zero with sign matching start\nIf start is ##Inf or ##-Inf and direction would cause a smaller magnitude\n  => Double/MAX_VALUE with same sign as start\nIf start is equal to +=Double/MAX_VALUE and direction would cause a larger magnitude\n  => ##Inf or ##-Inf with sign matching start\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#nextAfter-double-double-",
   :var-type "function",
   :line 469,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([start direction]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/next-after",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L469",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "next-down",
   :doc
   "Returns the adjacent double of d in the direction of ##-Inf.\nIf d is ##NaN => ##NaN\nIf d is ##-Inf => ##-Inf\nIf d is zero => -Double/MIN_VALUE\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#nextDown-double-",
   :var-type "function",
   :line 499,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([d]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/next-down",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L499",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "next-up",
   :doc
   "Returns the adjacent double of d in the direction of ##Inf.\nIf d is ##NaN => ##NaN\nIf d is ##Inf => ##Inf\nIf d is zero => Double/MIN_VALUE\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#nextUp-double-",
   :var-type "function",
   :line 487,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([d]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/next-up",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L487",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "pow",
   :doc
   "Returns the value of a raised to the power of b.\nFor more details on special cases, see:\nhttps://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#pow-double-double-",
   :var-type "function",
   :line 244,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([a b]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/pow",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L244",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "random",
   :doc
   "Returns a positive double between 0.0 and 1.0, chosen pseudorandomly with\napproximately random distribution.\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#random--",
   :var-type "function",
   :line 267,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/random",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L267",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "rint",
   :doc
   "Returns the double closest to a and equal to a mathematical integer.\nIf two values are equally close, return the even one.\nIf a is ##NaN or ##Inf or ##-Inf or zero => a\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#rint-double-",
   :var-type "function",
   :line 222,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([a]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/rint",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L222",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "round",
   :doc
   "Returns the closest long to a. If equally close to two values, return the one\ncloser to ##Inf.\nIf a is ##NaN => 0\nIf a is ##-Inf or < Long/MIN_VALUE => Long/MIN_VALUE\nIf a is ##Inf or > Long/MAX_VALUE => Long/MAX_VALUE\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#round-double-",
   :var-type "function",
   :line 254,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([a]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/round",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L254",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "scalb",
   :doc
   "Returns d * 2^scaleFactor, scaling by a factor of 2. If the exponent\nis between Double/MIN_EXPONENT and Double/MAX_EXPONENT, the answer is exact.\nIf d is ##NaN => ##NaN\nIf d is ##Inf or ##-Inf => ##Inf or ##-Inf respectively\nIf d is zero => zero of same sign as d\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#scalb-double-int-",
   :var-type "function",
   :line 511,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([d scaleFactor]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/scalb",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L511",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "signum",
   :doc
   "Returns the signum function of d - zero for zero, 1.0 if >0, -1.0 if <0.\nIf d is ##NaN => ##NaN\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#signum-double-",
   :var-type "function",
   :line 364,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([d]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/signum",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L364",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "sin",
   :doc
   "Returns the sine of an angle.\nIf a is ##NaN, ##-Inf, ##Inf => ##NaN\nIf a is zero => zero with the same sign as a\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#sin-double-",
   :var-type "function",
   :line 42,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([a]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/sin",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L42",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "sinh",
   :doc
   "Returns the hyperbolic sine of x, (e^x - e^-x)/2.\nIf x is ##NaN => ##NaN\nIf x is ##Inf or ##-Inf or zero => x\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#sinh-double-",
   :var-type "function",
   :line 374,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/sinh",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L374",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "sqrt",
   :doc
   "Returns the positive square root of a.\nIf a is ##NaN or negative => ##NaN\nIf a is ##Inf => ##Inf\nIf a is zero => a\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#sqrt-double-",
   :var-type "function",
   :line 160,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([a]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/sqrt",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L160",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "subtract-exact",
   :doc
   "Returns the difference of x and y, throws ArithmeticException on overflow.\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#subtractExact-long-long-",
   :var-type "function",
   :line 286,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([x y]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/subtract-exact",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L286",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "tan",
   :doc
   "Returns the tangent of an angle.\nIf a is ##NaN, ##-Inf, ##Inf => ##NaN\nIf a is zero => zero with the same sign as a\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#tan-double-",
   :var-type "function",
   :line 63,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([a]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/tan",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L63",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "tanh",
   :doc
   "Returns the hyperbolic tangent of x, sinh(x)/cosh(x).\nIf x is ##NaN => ##NaN\nIf x is zero => zero, with same sign\nIf x is ##Inf => +1.0\nIf x is ##-Inf => -1.0\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#tanh-double-",
   :var-type "function",
   :line 397,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/tanh",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L397",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "to-degrees",
   :doc
   "Converts an angle in radians to an approximate equivalent angle in degrees.\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#toDegrees-double-",
   :var-type "function",
   :line 115,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([r]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/to-degrees",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L115",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "to-radians",
   :doc
   "Converts an angle in degrees to an approximate equivalent angle in radians.\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#toRadians-double-",
   :var-type "function",
   :line 106,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([deg]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/to-radians",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L106",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "ulp",
   :doc
   "Returns the size of an ulp (unit in last place) for d.\nIf d is ##NaN => ##NaN\nIf d is ##Inf or ##-Inf => ##Inf\nIf d is zero => Double/MIN_VALUE\nIf d is +/- Double/MAX_VALUE => 2^971\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#ulp-double-",
   :var-type "function",
   :line 351,
   :added "1.11",
   :namespace "clojure.math",
   :arglists ([d]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/ulp",
   :source-url
   "https://github.com/clojure/clojure/blob/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj#L351",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3bc3500c725402f5d54914612fbdbf8691a1b669/src/clj/clojure/math.clj",
   :file "src/clj/clojure/math.clj"}
  {:name "*print-base*",
   :doc "The base to use for printing integers and rationals.",
   :var-type "var",
   :line 87,
   :added "1.2",
   :dynamic true,
   :namespace "clojure.pprint",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/*print-base*",
   :source-url
   "https://github.com/clojure/clojure/blob/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj#L87",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj",
   :file "src/clj/clojure/pprint/pprint_base.clj"}
  {:name "*print-miser-width*",
   :doc
   "The column at which to enter miser style. Depending on the dispatch table, \nmiser style add newlines in more places to try to keep lines short allowing for further \nlevels of nesting.",
   :var-type "var",
   :line 47,
   :added "1.2",
   :dynamic true,
   :namespace "clojure.pprint",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/*print-miser-width*",
   :source-url
   "https://github.com/clojure/clojure/blob/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj#L47",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj",
   :file "src/clj/clojure/pprint/pprint_base.clj"}
  {:name "*print-pprint-dispatch*",
   :doc
   "The pretty print dispatch function. Use with-pprint-dispatch or set-pprint-dispatch\nto modify.",
   :var-type "multimethod",
   :line 34,
   :added "1.2",
   :dynamic true,
   :namespace "clojure.pprint",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/*print-pprint-dispatch*",
   :source-url
   "https://github.com/clojure/clojure/blob/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj#L34",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj",
   :file "src/clj/clojure/pprint/pprint_base.clj"}
  {:name "*print-pretty*",
   :doc "Bind to true if you want write to use pretty printing",
   :var-type "var",
   :line 30,
   :added "1.2",
   :dynamic true,
   :namespace "clojure.pprint",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/*print-pretty*",
   :source-url
   "https://github.com/clojure/clojure/blob/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj#L30",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj",
   :file "src/clj/clojure/pprint/pprint_base.clj"}
  {:name "*print-radix*",
   :doc
   "Print a radix specifier in front of integers and rationals. If *print-base* is 2, 8, \nor 16, then the radix specifier used is #b, #o, or #x, respectively. Otherwise the \nradix specifier is in the form #XXr where XX is the decimal value of *print-base* ",
   :var-type "var",
   :line 80,
   :added "1.2",
   :dynamic true,
   :namespace "clojure.pprint",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/*print-radix*",
   :source-url
   "https://github.com/clojure/clojure/blob/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj#L80",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj",
   :file "src/clj/clojure/pprint/pprint_base.clj"}
  {:name "*print-right-margin*",
   :doc
   "Pretty printing will try to avoid anything going beyond this column.\nSet it to nil to have pprint let the line be arbitrarily long. This will ignore all \nnon-mandatory newlines.",
   :var-type "var",
   :line 40,
   :added "1.2",
   :dynamic true,
   :namespace "clojure.pprint",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/*print-right-margin*",
   :source-url
   "https://github.com/clojure/clojure/blob/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj#L40",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj",
   :file "src/clj/clojure/pprint/pprint_base.clj"}
  {:name "*print-suppress-namespaces*",
   :doc
   "Don't print namespaces with symbols. This is particularly useful when \npretty printing the results of macro expansions",
   :var-type "var",
   :line 72,
   :added "1.2",
   :dynamic true,
   :namespace "clojure.pprint",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/*print-suppress-namespaces*",
   :source-url
   "https://github.com/clojure/clojure/blob/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj#L72",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj",
   :file "src/clj/clojure/pprint/pprint_base.clj"}
  {:name "cl-format",
   :doc
   "An implementation of a Common Lisp compatible format function. cl-format formats its\narguments to an output stream or string based on the format control string given. It \nsupports sophisticated formatting of structured data.\n\nWriter is an instance of java.io.Writer, true to output to *out* or nil to output \nto a string, format-in is the format control string and the remaining arguments \nare the data to be formatted.\n\nThe format control string is a string to be output with embedded 'format directives' \ndescribing how to format the various arguments passed in.\n\nIf writer is nil, cl-format returns the formatted result string. Otherwise, cl-format \nreturns nil.\n\nFor example:\n (let [results [46 38 22]]\n        (cl-format true \"There ~[are~;is~:;are~]~:* ~d result~:p: ~{~d~^, ~}~%\" \n                   (count results) results))\n\nPrints to *out*:\n There are 3 results: 46, 38, 22\n\nDetailed documentation on format control strings is available in the \"Common Lisp the \nLanguage, 2nd edition\", Chapter 22 (available online at:\nhttp://www.cs.cmu.edu/afs/cs.cmu.edu/project/ai-repository/ai/html/cltl/clm/node200.html#SECTION002633000000000000000) \nand in the Common Lisp HyperSpec at \nhttp://www.lispworks.com/documentation/HyperSpec/Body/22_c.htm",
   :var-type "function",
   :line 27,
   :added "1.2",
   :namespace "clojure.pprint",
   :arglists ([writer format-in & args]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/cl-format",
   :source-url
   "https://github.com/clojure/clojure/blob/fb916808669ef65dce5dfe58e23d4a902253ca55/src/clj/clojure/pprint/cl_format.clj#L27",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/fb916808669ef65dce5dfe58e23d4a902253ca55/src/clj/clojure/pprint/cl_format.clj",
   :file "src/clj/clojure/pprint/cl_format.clj"}
  {:name "code-dispatch",
   :doc
   "The pretty print dispatch function for pretty printing Clojure code.",
   :var-type "multimethod",
   :line 454,
   :added "1.2",
   :namespace "clojure.pprint",
   :arglists [[object]],
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/code-dispatch",
   :source-url
   "https://github.com/clojure/clojure/blob/1786de6d79fee27b641b50120a432da3dcdd5058/src/clj/clojure/pprint/dispatch.clj#L454",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/1786de6d79fee27b641b50120a432da3dcdd5058/src/clj/clojure/pprint/dispatch.clj",
   :file "src/clj/clojure/pprint/dispatch.clj"}
  {:name "formatter",
   :doc
   "Makes a function which can directly run format-in. The function is\nfn [stream & args] ... and returns nil unless the stream is nil (meaning \noutput to a string) in which case it returns the resulting string.\n\nformat-in can be either a control string or a previously compiled format.",
   :var-type "macro",
   :line 1916,
   :added "1.2",
   :namespace "clojure.pprint",
   :arglists ([format-in]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/formatter",
   :source-url
   "https://github.com/clojure/clojure/blob/fb916808669ef65dce5dfe58e23d4a902253ca55/src/clj/clojure/pprint/cl_format.clj#L1916",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/fb916808669ef65dce5dfe58e23d4a902253ca55/src/clj/clojure/pprint/cl_format.clj",
   :file "src/clj/clojure/pprint/cl_format.clj"}
  {:name "formatter-out",
   :doc
   "Makes a function which can directly run format-in. The function is\nfn [& args] ... and returns nil. This version of the formatter macro is\ndesigned to be used with *out* set to an appropriate Writer. In particular,\nthis is meant to be used as part of a pretty printer dispatch method.\n\nformat-in can be either a control string or a previously compiled format.",
   :var-type "macro",
   :line 1936,
   :added "1.2",
   :namespace "clojure.pprint",
   :arglists ([format-in]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/formatter-out",
   :source-url
   "https://github.com/clojure/clojure/blob/fb916808669ef65dce5dfe58e23d4a902253ca55/src/clj/clojure/pprint/cl_format.clj#L1936",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/fb916808669ef65dce5dfe58e23d4a902253ca55/src/clj/clojure/pprint/cl_format.clj",
   :file "src/clj/clojure/pprint/cl_format.clj"}
  {:name "fresh-line",
   :doc
   "Make a newline if *out* is not already at the beginning of the line. If *out* is\nnot a pretty writer (which keeps track of columns), this function always outputs a newline.",
   :var-type "function",
   :line 1245,
   :added "1.2",
   :namespace "clojure.pprint",
   :arglists ([]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/fresh-line",
   :source-url
   "https://github.com/clojure/clojure/blob/fb916808669ef65dce5dfe58e23d4a902253ca55/src/clj/clojure/pprint/cl_format.clj#L1245",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/fb916808669ef65dce5dfe58e23d4a902253ca55/src/clj/clojure/pprint/cl_format.clj",
   :file "src/clj/clojure/pprint/cl_format.clj"}
  {:name "get-pretty-writer",
   :doc
   "Returns the java.io.Writer passed in wrapped in a pretty writer proxy, unless it's \nalready a pretty writer. Generally, it is unnecessary to call this function, since pprint,\nwrite, and cl-format all call it if they need to. However if you want the state to be \npreserved across calls, you will want to wrap them with this. \n\nFor example, when you want to generate column-aware output with multiple calls to cl-format, \ndo it like in this example:\n\n    (defn print-table [aseq column-width]\n      (binding [*out* (get-pretty-writer *out*)]\n        (doseq [row aseq]\n          (doseq [col row]\n            (cl-format true \"~4D~7,vT\" col column-width))\n          (prn))))\n\nNow when you run:\n\n    user> (print-table (map #(vector % (* % %) (* % % %)) (range 1 11)) 8)\n\nIt prints a table of squares and cubes for the numbers from 1 to 10:\n\n       1      1       1    \n       2      4       8    \n       3      9      27    \n       4     16      64    \n       5     25     125    \n       6     36     216    \n       7     49     343    \n       8     64     512    \n       9     81     729    \n      10    100    1000",
   :var-type "function",
   :line 1203,
   :added "1.2",
   :namespace "clojure.pprint",
   :arglists ([writer]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/get-pretty-writer",
   :source-url
   "https://github.com/clojure/clojure/blob/fb916808669ef65dce5dfe58e23d4a902253ca55/src/clj/clojure/pprint/cl_format.clj#L1203",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/fb916808669ef65dce5dfe58e23d4a902253ca55/src/clj/clojure/pprint/cl_format.clj",
   :file "src/clj/clojure/pprint/cl_format.clj"}
  {:name "pp",
   :doc
   "A convenience macro that pretty prints the last thing output. This is\nexactly equivalent to (pprint *1).",
   :var-type "macro",
   :line 254,
   :added "1.2",
   :namespace "clojure.pprint",
   :arglists ([]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/pp",
   :source-url
   "https://github.com/clojure/clojure/blob/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj#L254",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj",
   :file "src/clj/clojure/pprint/pprint_base.clj"}
  {:name "pprint",
   :doc
   "Pretty print object to the optional output writer. If the writer is not provided, \nprint the object to the currently bound value of *out*.",
   :var-type "function",
   :line 241,
   :added "1.2",
   :namespace "clojure.pprint",
   :arglists ([object] [object writer]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/pprint",
   :source-url
   "https://github.com/clojure/clojure/blob/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj#L241",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj",
   :file "src/clj/clojure/pprint/pprint_base.clj"}
  {:name "pprint-indent",
   :doc
   "Create an indent at this point in the pretty printing stream. This defines how \nfollowing lines are indented. relative-to can be either :block or :current depending \nwhether the indent should be computed relative to the start of the logical block or\nthe current column position. n is an offset. \n\nThis function is intended for use when writing custom dispatch functions.\n\nOutput is sent to *out* which must be a pretty printing writer.",
   :var-type "function",
   :line 341,
   :added "1.2",
   :namespace "clojure.pprint",
   :arglists ([relative-to n]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/pprint-indent",
   :source-url
   "https://github.com/clojure/clojure/blob/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj#L341",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj",
   :file "src/clj/clojure/pprint/pprint_base.clj"}
  {:name "pprint-logical-block",
   :doc
   "Execute the body as a pretty printing logical block with output to *out* which \nmust be a pretty printing writer. When used from pprint or cl-format, this can be \nassumed. \n\nThis function is intended for use when writing custom dispatch functions.\n\nBefore the body, the caller can optionally specify options: :prefix, :per-line-prefix, \nand :suffix.",
   :var-type "macro",
   :line 302,
   :added "1.2",
   :namespace "clojure.pprint",
   :arglists [[options* body]],
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/pprint-logical-block",
   :source-url
   "https://github.com/clojure/clojure/blob/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj#L302",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj",
   :file "src/clj/clojure/pprint/pprint_base.clj"}
  {:name "pprint-newline",
   :doc
   "Print a conditional newline to a pretty printing stream. kind specifies if the \nnewline is :linear, :miser, :fill, or :mandatory. \n\nThis function is intended for use when writing custom dispatch functions.\n\nOutput is sent to *out* which must be a pretty printing writer.",
   :var-type "function",
   :line 329,
   :added "1.2",
   :namespace "clojure.pprint",
   :arglists ([kind]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/pprint-newline",
   :source-url
   "https://github.com/clojure/clojure/blob/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj#L329",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj",
   :file "src/clj/clojure/pprint/pprint_base.clj"}
  {:name "pprint-tab",
   :doc
   "Tab at this point in the pretty printing stream. kind specifies whether the tab\nis :line, :section, :line-relative, or :section-relative. \n\nColnum and colinc specify the target column and the increment to move the target\nforward if the output is already past the original target.\n\nThis function is intended for use when writing custom dispatch functions.\n\nOutput is sent to *out* which must be a pretty printing writer.\n\nTHIS FUNCTION IS NOT YET IMPLEMENTED.",
   :var-type "function",
   :line 356,
   :added "1.2",
   :namespace "clojure.pprint",
   :arglists ([kind colnum colinc]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/pprint-tab",
   :source-url
   "https://github.com/clojure/clojure/blob/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj#L356",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj",
   :file "src/clj/clojure/pprint/pprint_base.clj"}
  {:name "print-length-loop",
   :doc
   "A version of loop that iterates at most *print-length* times. This is designed \nfor use in pretty-printer dispatch functions.",
   :var-type "macro",
   :line 391,
   :added "1.3",
   :namespace "clojure.pprint",
   :arglists ([bindings & body]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/print-length-loop",
   :source-url
   "https://github.com/clojure/clojure/blob/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj#L391",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj",
   :file "src/clj/clojure/pprint/pprint_base.clj"}
  {:name "print-table",
   :doc
   "Prints a collection of maps in a textual table. Prints table headings\nks, and then a line of output for each row, corresponding to the keys\nin ks. If ks are not specified, use the keys of the first item in rows.",
   :var-type "function",
   :line 11,
   :added "1.3",
   :namespace "clojure.pprint",
   :arglists ([ks rows] [rows]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/print-table",
   :source-url
   "https://github.com/clojure/clojure/blob/93d13d0c0671130b329863570080c72799563ac7/src/clj/clojure/pprint/print_table.clj#L11",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/93d13d0c0671130b329863570080c72799563ac7/src/clj/clojure/pprint/print_table.clj",
   :file "src/clj/clojure/pprint/print_table.clj"}
  {:name "set-pprint-dispatch",
   :doc
   "Set the pretty print dispatch function to a function matching (fn [obj] ...)\nwhere obj is the object to pretty print. That function will be called with *out* set\nto a pretty printing writer to which it should do its printing.\n\nFor example functions, see simple-dispatch and code-dispatch in \nclojure.pprint.dispatch.clj.",
   :var-type "function",
   :line 260,
   :added "1.2",
   :namespace "clojure.pprint",
   :arglists ([function]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/set-pprint-dispatch",
   :source-url
   "https://github.com/clojure/clojure/blob/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj#L260",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj",
   :file "src/clj/clojure/pprint/pprint_base.clj"}
  {:name "simple-dispatch",
   :doc
   "The pretty print dispatch function for simple data structure format.",
   :var-type "multimethod",
   :line 152,
   :added "1.2",
   :namespace "clojure.pprint",
   :arglists [[object]],
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/simple-dispatch",
   :source-url
   "https://github.com/clojure/clojure/blob/1786de6d79fee27b641b50120a432da3dcdd5058/src/clj/clojure/pprint/dispatch.clj#L152",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/1786de6d79fee27b641b50120a432da3dcdd5058/src/clj/clojure/pprint/dispatch.clj",
   :file "src/clj/clojure/pprint/dispatch.clj"}
  {:name "with-pprint-dispatch",
   :doc
   "Execute body with the pretty print dispatch function bound to function.",
   :var-type "macro",
   :line 274,
   :added "1.2",
   :namespace "clojure.pprint",
   :arglists ([function & body]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/with-pprint-dispatch",
   :source-url
   "https://github.com/clojure/clojure/blob/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj#L274",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj",
   :file "src/clj/clojure/pprint/pprint_base.clj"}
  {:name "write",
   :doc
   "Write an object subject to the current bindings of the printer control variables.\nUse the kw-args argument to override individual variables for this call (and any \nrecursive calls). Returns the string result if :stream is nil or nil otherwise.\n\nThe following keyword arguments can be passed with values:\n  Keyword              Meaning                              Default value\n  :stream              Writer for output or nil             true (indicates *out*)\n  :base                Base to use for writing rationals    Current value of *print-base*\n  :circle*             If true, mark circular structures    Current value of *print-circle*\n  :length              Maximum elements to show in sublists Current value of *print-length*\n  :level               Maximum depth                        Current value of *print-level*\n  :lines*              Maximum lines of output              Current value of *print-lines*\n  :miser-width         Width to enter miser mode            Current value of *print-miser-width*\n  :dispatch            The pretty print dispatch function   Current value of *print-pprint-dispatch*\n  :pretty              If true, do pretty printing          Current value of *print-pretty*\n  :radix               If true, prepend a radix specifier   Current value of *print-radix*\n  :readably*           If true, print readably              Current value of *print-readably*\n  :right-margin        The column for the right margin      Current value of *print-right-margin*\n  :suppress-namespaces If true, no namespaces in symbols    Current value of *print-suppress-namespaces*\n\n  * = not yet supported",
   :var-type "function",
   :line 197,
   :added "1.2",
   :namespace "clojure.pprint",
   :arglists ([object & kw-args]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/write",
   :source-url
   "https://github.com/clojure/clojure/blob/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj#L197",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj",
   :file "src/clj/clojure/pprint/pprint_base.clj"}
  {:name "write-out",
   :doc
   "Write an object to *out* subject to the current bindings of the printer control \nvariables. Use the kw-args argument to override individual variables for this call (and \nany recursive calls).\n\n*out* must be a PrettyWriter if pretty printing is enabled. This is the responsibility\nof the caller.\n\nThis method is primarily intended for use by pretty print dispatch functions that \nalready know that the pretty printer will have set up their environment appropriately.\nNormal library clients should use the standard \"write\" interface. ",
   :var-type "function",
   :line 171,
   :added "1.2",
   :namespace "clojure.pprint",
   :arglists ([object]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/write-out",
   :source-url
   "https://github.com/clojure/clojure/blob/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj#L171",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj",
   :file "src/clj/clojure/pprint/pprint_base.clj"}
  {:name "->AsmReflector",
   :doc
   "Positional factory function for class clojure.reflect.AsmReflector.",
   :var-type "function",
   :line 208,
   :namespace "clojure.reflect",
   :arglists ([class-resolver]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/->AsmReflector",
   :source-url
   "https://github.com/clojure/clojure/blob/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj#L208",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj",
   :file "src/clj/clojure/reflect/java.clj"}
  {:name "->Constructor",
   :doc
   "Positional factory function for class clojure.reflect.Constructor.",
   :var-type "function",
   :line 115,
   :namespace "clojure.reflect",
   :arglists
   ([name declaring-class parameter-types exception-types flags]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/->Constructor",
   :source-url
   "https://github.com/clojure/clojure/blob/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj#L115",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj",
   :file "src/clj/clojure/reflect/java.clj"}
  {:name "->Field",
   :doc "Positional factory function for class clojure.reflect.Field.",
   :var-type "function",
   :line 154,
   :namespace "clojure.reflect",
   :arglists ([name type declaring-class flags]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/->Field",
   :source-url
   "https://github.com/clojure/clojure/blob/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj#L154",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj",
   :file "src/clj/clojure/reflect/java.clj"}
  {:name "->JavaReflector",
   :doc
   "Positional factory function for class clojure.reflect.JavaReflector.",
   :var-type "function",
   :line 178,
   :namespace "clojure.reflect",
   :arglists ([classloader]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/->JavaReflector",
   :source-url
   "https://github.com/clojure/clojure/blob/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj#L178",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj",
   :file "src/clj/clojure/reflect/java.clj"}
  {:name "->Method",
   :doc
   "Positional factory function for class clojure.reflect.Method.",
   :var-type "function",
   :line 134,
   :namespace "clojure.reflect",
   :arglists
   ([name
     return-type
     declaring-class
     parameter-types
     exception-types
     flags]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/->Method",
   :source-url
   "https://github.com/clojure/clojure/blob/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj#L134",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj",
   :file "src/clj/clojure/reflect/java.clj"}
  {:name "flag-descriptors",
   :doc
   "The Java access bitflags, along with their friendly names and\nthe kinds of objects to which they can apply.",
   :var-type "var",
   :line 77,
   :namespace "clojure.reflect",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/flag-descriptors",
   :source-url
   "https://github.com/clojure/clojure/blob/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj#L77",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj",
   :file "src/clj/clojure/reflect/java.clj"}
  {:name "map->Constructor",
   :doc
   "Factory function for class clojure.reflect.Constructor, taking a map of keywords to field values.",
   :var-type "function",
   :line 115,
   :namespace "clojure.reflect",
   :arglists ([m#]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/map->Constructor",
   :source-url
   "https://github.com/clojure/clojure/blob/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj#L115",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj",
   :file "src/clj/clojure/reflect/java.clj"}
  {:name "map->Field",
   :doc
   "Factory function for class clojure.reflect.Field, taking a map of keywords to field values.",
   :var-type "function",
   :line 154,
   :namespace "clojure.reflect",
   :arglists ([m#]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/map->Field",
   :source-url
   "https://github.com/clojure/clojure/blob/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj#L154",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj",
   :file "src/clj/clojure/reflect/java.clj"}
  {:name "map->Method",
   :doc
   "Factory function for class clojure.reflect.Method, taking a map of keywords to field values.",
   :var-type "function",
   :line 134,
   :namespace "clojure.reflect",
   :arglists ([m#]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/map->Method",
   :source-url
   "https://github.com/clojure/clojure/blob/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj#L134",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj",
   :file "src/clj/clojure/reflect/java.clj"}
  {:name "reflect",
   :doc
   "Alpha - subject to change.\nReflect on the type of obj (or obj itself if obj is a class).\nReturn value and options are the same as for type-reflect. ",
   :var-type "function",
   :line 115,
   :added "1.3",
   :namespace "clojure.reflect",
   :arglists ([obj & options]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/reflect",
   :source-url
   "https://github.com/clojure/clojure/blob/ee00807bac64d55dbc7ec49442d6376352b77200/src/clj/clojure/reflect.clj#L115",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/ee00807bac64d55dbc7ec49442d6376352b77200/src/clj/clojure/reflect.clj",
   :file "src/clj/clojure/reflect.clj"}
  {:name "type-reflect",
   :doc
   "Alpha - subject to change.\n Reflect on a typeref, returning a map with :bases, :flags, and\n:members. In the discussion below, names are always Clojure symbols.\n\n :bases            a set of names of the type's bases\n :flags            a set of keywords naming the boolean attributes\n                   of the type.\n :members          a set of the type's members. Each member is a map\n                   and can be a constructor, method, or field.\n\n Keys common to all members:\n :name             name of the type \n :declaring-class  name of the declarer\n :flags            keyword naming boolean attributes of the member\n\n Keys specific to constructors:\n :parameter-types  vector of parameter type names\n :exception-types  vector of exception type names\n\n Key specific to methods:\n :parameter-types  vector of parameter type names\n :exception-types  vector of exception type names\n :return-type      return type name\n\n Keys specific to fields:\n :type             type name\n\n Options:\n\n   :ancestors     in addition to the keys described above, also\n                  include an :ancestors key with the entire set of\n                  ancestors, and add all ancestor members to\n                  :members.\n   :reflector     implementation to use. Defaults to JavaReflector,\n                  AsmReflector is also an option.",
   :var-type "function",
   :line 58,
   :added "1.3",
   :namespace "clojure.reflect",
   :arglists ([typeref & options]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/type-reflect",
   :source-url
   "https://github.com/clojure/clojure/blob/ee00807bac64d55dbc7ec49442d6376352b77200/src/clj/clojure/reflect.clj#L58",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/ee00807bac64d55dbc7ec49442d6376352b77200/src/clj/clojure/reflect.clj",
   :file "src/clj/clojure/reflect.clj"}
  {:name "AsmReflector",
   :var-type "type",
   :namespace "clojure.reflect",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/AsmReflector",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "Constructor",
   :var-type "record",
   :namespace "clojure.reflect",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/Constructor",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "Field",
   :var-type "record",
   :namespace "clojure.reflect",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/Field",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "JavaReflector",
   :var-type "type",
   :namespace "clojure.reflect",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/JavaReflector",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "Method",
   :var-type "record",
   :namespace "clojure.reflect",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/Method",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "ClassResolver",
   :doc nil,
   :var-type "protocol",
   :line 196,
   :namespace "clojure.reflect",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/ClassResolver",
   :source-url
   "https://github.com/clojure/clojure/blob/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj#L196",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj",
   :file "src/clj/clojure/reflect/java.clj"}
  {:name "Reflector",
   :doc "Protocol for reflection implementers.",
   :var-type "protocol",
   :line 44,
   :namespace "clojure.reflect",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/Reflector",
   :source-url
   "https://github.com/clojure/clojure/blob/ee00807bac64d55dbc7ec49442d6376352b77200/src/clj/clojure/reflect.clj#L44",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/ee00807bac64d55dbc7ec49442d6376352b77200/src/clj/clojure/reflect.clj",
   :file "src/clj/clojure/reflect.clj"}
  {:name "TypeReference",
   :doc
   "A TypeReference can be unambiguously converted to a type name on\nthe host platform.\n\nAll typerefs are normalized into symbols. If you need to\nnormalize a typeref yourself, call typesym.",
   :var-type "protocol",
   :line 48,
   :namespace "clojure.reflect",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/TypeReference",
   :source-url
   "https://github.com/clojure/clojure/blob/ee00807bac64d55dbc7ec49442d6376352b77200/src/clj/clojure/reflect.clj#L48",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/ee00807bac64d55dbc7ec49442d6376352b77200/src/clj/clojure/reflect.clj",
   :file "src/clj/clojure/reflect.clj"}
  {:name "resolve-class",
   :doc
   "Given a class name, return that typeref's class bytes as an InputStream.",
   :var-type "function",
   :namespace "clojure.reflect",
   :arglists ([this name]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/resolve-class",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "do-reflect",
   :doc nil,
   :var-type "function",
   :namespace "clojure.reflect",
   :arglists ([reflector typeref]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/do-reflect",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "typename",
   :doc
   "Returns Java name as returned by ASM getClassName, e.g. byte[], java.lang.String[]",
   :var-type "function",
   :namespace "clojure.reflect",
   :arglists ([o]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/typename",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "apropos",
   :doc
   "Given a regular expression or stringable thing, return a seq of all\npublic definitions in all currently-loaded namespaces that match the\nstr-or-pattern.",
   :var-type "function",
   :line 181,
   :namespace "clojure.repl",
   :arglists ([str-or-pattern]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.repl-api.html#clojure.repl/apropos",
   :source-url
   "https://github.com/clojure/clojure/blob/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj#L181",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj",
   :file "src/clj/clojure/repl.clj"}
  {:name "demunge",
   :doc
   "Given a string representation of a fn class,\nas in a stack trace element, returns a readable version.",
   :var-type "function",
   :line 207,
   :added "1.3",
   :namespace "clojure.repl",
   :arglists ([fn-name]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.repl-api.html#clojure.repl/demunge",
   :source-url
   "https://github.com/clojure/clojure/blob/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj#L207",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj",
   :file "src/clj/clojure/repl.clj"}
  {:name "dir",
   :doc "Prints a sorted directory of public vars in a namespace",
   :var-type "macro",
   :line 201,
   :namespace "clojure.repl",
   :arglists ([nsname]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.repl-api.html#clojure.repl/dir",
   :source-url
   "https://github.com/clojure/clojure/blob/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj#L201",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj",
   :file "src/clj/clojure/repl.clj"}
  {:name "dir-fn",
   :doc
   "Returns a sorted seq of symbols naming public vars in\na namespace or namespace alias. Looks for aliases in *ns*",
   :var-type "function",
   :line 195,
   :namespace "clojure.repl",
   :arglists ([ns]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.repl-api.html#clojure.repl/dir-fn",
   :source-url
   "https://github.com/clojure/clojure/blob/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj#L195",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj",
   :file "src/clj/clojure/repl.clj"}
  {:name "doc",
   :doc
   "Prints documentation for a var or special form given its name,\nor for a spec if given a keyword",
   :var-type "macro",
   :line 131,
   :added "1.0",
   :namespace "clojure.repl",
   :arglists ([name]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.repl-api.html#clojure.repl/doc",
   :source-url
   "https://github.com/clojure/clojure/blob/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj#L131",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj",
   :file "src/clj/clojure/repl.clj"}
  {:name "find-doc",
   :doc
   "Prints documentation for any var whose documentation or name\ncontains a match for re-string-or-pattern",
   :var-type "function",
   :line 115,
   :added "1.0",
   :namespace "clojure.repl",
   :arglists ([re-string-or-pattern]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.repl-api.html#clojure.repl/find-doc",
   :source-url
   "https://github.com/clojure/clojure/blob/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj#L115",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj",
   :file "src/clj/clojure/repl.clj"}
  {:name "pst",
   :doc
   "Prints a stack trace of the exception, to the depth requested. If none supplied, uses the root cause of the\nmost recent repl exception (*e), and a depth of 12.",
   :var-type "function",
   :line 240,
   :added "1.3",
   :namespace "clojure.repl",
   :arglists ([] [e-or-depth] [e depth]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.repl-api.html#clojure.repl/pst",
   :source-url
   "https://github.com/clojure/clojure/blob/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj#L240",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj",
   :file "src/clj/clojure/repl.clj"}
  {:name "root-cause",
   :doc
   "Returns the initial cause of an exception or error by peeling off all of\nits wrappers",
   :var-type "function",
   :line 214,
   :added "1.3",
   :namespace "clojure.repl",
   :arglists ([t]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.repl-api.html#clojure.repl/root-cause",
   :source-url
   "https://github.com/clojure/clojure/blob/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj#L214",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj",
   :file "src/clj/clojure/repl.clj"}
  {:name "set-break-handler!",
   :doc
   "Register INT signal handler.  After calling this, Ctrl-C will cause\nthe given function f to be called with a single argument, the signal.\nUses thread-stopper if no function given.",
   :var-type "function",
   :line 279,
   :namespace "clojure.repl",
   :arglists ([] [f]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.repl-api.html#clojure.repl/set-break-handler!",
   :source-url
   "https://github.com/clojure/clojure/blob/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj#L279",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj",
   :file "src/clj/clojure/repl.clj"}
  {:name "source",
   :doc
   "Prints the source code for the given symbol, if it can find it.\nThis requires that the symbol resolve to a Var defined in a\nnamespace for which the .clj is in the classpath.\n\nExample: (source filter)",
   :var-type "macro",
   :line 172,
   :namespace "clojure.repl",
   :arglists ([n]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.repl-api.html#clojure.repl/source",
   :source-url
   "https://github.com/clojure/clojure/blob/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj#L172",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj",
   :file "src/clj/clojure/repl.clj"}
  {:name "source-fn",
   :doc
   "Returns a string of the source code for the given symbol, if it can\nfind it.  This requires that the symbol resolve to a Var defined in\na namespace for which the .clj is in the classpath.  Returns nil if\nit can't find the source.  For most REPL usage, 'source' is more\nconvenient.\n\nExample: (source-fn 'filter)",
   :var-type "function",
   :line 147,
   :namespace "clojure.repl",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.repl-api.html#clojure.repl/source-fn",
   :source-url
   "https://github.com/clojure/clojure/blob/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj#L147",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj",
   :file "src/clj/clojure/repl.clj"}
  {:name "stack-element-str",
   :doc
   "Returns a (possibly unmunged) string representation of a StackTraceElement",
   :var-type "function",
   :line 227,
   :added "1.3",
   :namespace "clojure.repl",
   :arglists ([el]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.repl-api.html#clojure.repl/stack-element-str",
   :source-url
   "https://github.com/clojure/clojure/blob/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj#L227",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj",
   :file "src/clj/clojure/repl.clj"}
  {:name "thread-stopper",
   :doc
   "Returns a function that takes one arg and uses that as an exception message\nto stop the given thread.  Defaults to the current thread",
   :var-type "function",
   :line 273,
   :namespace "clojure.repl",
   :arglists ([] [thread]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.repl-api.html#clojure.repl/thread-stopper",
   :source-url
   "https://github.com/clojure/clojure/blob/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj#L273",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj",
   :file "src/clj/clojure/repl.clj"}
  {:name "difference",
   :doc
   "Return a set that is the first set without elements of the remaining sets",
   :var-type "function",
   :line 49,
   :added "1.0",
   :namespace "clojure.set",
   :arglists ([s1] [s1 s2] [s1 s2 & sets]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.set-api.html#clojure.set/difference",
   :source-url
   "https://github.com/clojure/clojure/blob/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj#L49",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj",
   :file "src/clj/clojure/set.clj"}
  {:name "index",
   :doc
   "Returns a map of the distinct values of ks in the xrel mapped to a\nset of the maps in xrel with the corresponding values of ks.",
   :var-type "function",
   :line 95,
   :added "1.0",
   :namespace "clojure.set",
   :arglists ([xrel ks]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.set-api.html#clojure.set/index",
   :source-url
   "https://github.com/clojure/clojure/blob/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj#L95",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj",
   :file "src/clj/clojure/set.clj"}
  {:name "intersection",
   :doc "Return a set that is the intersection of the input sets",
   :var-type "function",
   :line 33,
   :added "1.0",
   :namespace "clojure.set",
   :arglists ([s1] [s1 s2] [s1 s2 & sets]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.set-api.html#clojure.set/intersection",
   :source-url
   "https://github.com/clojure/clojure/blob/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj#L33",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj",
   :file "src/clj/clojure/set.clj"}
  {:name "join",
   :doc
   "When passed 2 rels, returns the rel corresponding to the natural\njoin. When passed an additional keymap, joins on the corresponding\nkeys.",
   :var-type "function",
   :line 115,
   :added "1.0",
   :namespace "clojure.set",
   :arglists ([xrel yrel] [xrel yrel km]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.set-api.html#clojure.set/join",
   :source-url
   "https://github.com/clojure/clojure/blob/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj#L115",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj",
   :file "src/clj/clojure/set.clj"}
  {:name "map-invert",
   :doc "Returns the map with the vals mapped to the keys.",
   :var-type "function",
   :line 106,
   :added "1.0",
   :namespace "clojure.set",
   :arglists ([m]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.set-api.html#clojure.set/map-invert",
   :source-url
   "https://github.com/clojure/clojure/blob/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj#L106",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj",
   :file "src/clj/clojure/set.clj"}
  {:name "project",
   :doc
   "Returns a rel of the elements of xrel with only the keys in ks",
   :var-type "function",
   :line 72,
   :added "1.0",
   :namespace "clojure.set",
   :arglists ([xrel ks]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.set-api.html#clojure.set/project",
   :source-url
   "https://github.com/clojure/clojure/blob/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj#L72",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj",
   :file "src/clj/clojure/set.clj"}
  {:name "rename",
   :doc
   "Returns a rel of the maps in xrel with the keys in kmap renamed to the vals in kmap",
   :var-type "function",
   :line 89,
   :added "1.0",
   :namespace "clojure.set",
   :arglists ([xrel kmap]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.set-api.html#clojure.set/rename",
   :source-url
   "https://github.com/clojure/clojure/blob/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj#L89",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj",
   :file "src/clj/clojure/set.clj"}
  {:name "rename-keys",
   :doc
   "Returns the map with the keys in kmap renamed to the vals in kmap",
   :var-type "function",
   :line 78,
   :added "1.0",
   :namespace "clojure.set",
   :arglists ([map kmap]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.set-api.html#clojure.set/rename-keys",
   :source-url
   "https://github.com/clojure/clojure/blob/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj#L78",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj",
   :file "src/clj/clojure/set.clj"}
  {:name "select",
   :doc "Returns a set of the elements for which pred is true",
   :var-type "function",
   :line 65,
   :added "1.0",
   :namespace "clojure.set",
   :arglists ([pred xset]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.set-api.html#clojure.set/select",
   :source-url
   "https://github.com/clojure/clojure/blob/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj#L65",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj",
   :file "src/clj/clojure/set.clj"}
  {:name "subset?",
   :doc "Is set1 a subset of set2?",
   :var-type "function",
   :line 146,
   :added "1.2",
   :namespace "clojure.set",
   :arglists ([set1 set2]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.set-api.html#clojure.set/subset?",
   :source-url
   "https://github.com/clojure/clojure/blob/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj#L146",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj",
   :file "src/clj/clojure/set.clj"}
  {:name "superset?",
   :doc "Is set1 a superset of set2?",
   :var-type "function",
   :line 154,
   :added "1.2",
   :namespace "clojure.set",
   :arglists ([set1 set2]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.set-api.html#clojure.set/superset?",
   :source-url
   "https://github.com/clojure/clojure/blob/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj#L154",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj",
   :file "src/clj/clojure/set.clj"}
  {:name "union",
   :doc "Return a set that is the union of the input sets",
   :var-type "function",
   :line 20,
   :added "1.0",
   :namespace "clojure.set",
   :arglists ([] [s1] [s1 s2] [s1 s2 & sets]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.set-api.html#clojure.set/union",
   :source-url
   "https://github.com/clojure/clojure/blob/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj#L20",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj",
   :file "src/clj/clojure/set.clj"}
  {:name "e",
   :doc
   "REPL utility.  Prints a brief stack trace for the root cause of the\nmost recent exception.",
   :var-type "function",
   :line 82,
   :added "1.1",
   :namespace "clojure.stacktrace",
   :arglists ([]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.stacktrace-api.html#clojure.stacktrace/e",
   :source-url
   "https://github.com/clojure/clojure/blob/dbb448f7709b20c392558e7d7871d1e9b28c9440/src/clj/clojure/stacktrace.clj#L82",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/dbb448f7709b20c392558e7d7871d1e9b28c9440/src/clj/clojure/stacktrace.clj",
   :file "src/clj/clojure/stacktrace.clj"}
  {:name "print-cause-trace",
   :doc
   "Like print-stack-trace but prints chained exceptions (causes).",
   :var-type "function",
   :line 72,
   :added "1.1",
   :namespace "clojure.stacktrace",
   :arglists ([tr] [tr n]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.stacktrace-api.html#clojure.stacktrace/print-cause-trace",
   :source-url
   "https://github.com/clojure/clojure/blob/dbb448f7709b20c392558e7d7871d1e9b28c9440/src/clj/clojure/stacktrace.clj#L72",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/dbb448f7709b20c392558e7d7871d1e9b28c9440/src/clj/clojure/stacktrace.clj",
   :file "src/clj/clojure/stacktrace.clj"}
  {:name "print-stack-trace",
   :doc
   "Prints a Clojure-oriented stack trace of tr, a Throwable.\nPrints a maximum of n stack frames (default: unlimited).\nDoes not print chained exceptions (causes).",
   :var-type "function",
   :line 50,
   :added "1.1",
   :namespace "clojure.stacktrace",
   :arglists ([tr] [tr n]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.stacktrace-api.html#clojure.stacktrace/print-stack-trace",
   :source-url
   "https://github.com/clojure/clojure/blob/dbb448f7709b20c392558e7d7871d1e9b28c9440/src/clj/clojure/stacktrace.clj#L50",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/dbb448f7709b20c392558e7d7871d1e9b28c9440/src/clj/clojure/stacktrace.clj",
   :file "src/clj/clojure/stacktrace.clj"}
  {:name "print-throwable",
   :doc
   "Prints the class and message of a Throwable. Prints the ex-data map\nif present.",
   :var-type "function",
   :line 40,
   :added "1.1",
   :namespace "clojure.stacktrace",
   :arglists ([tr]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.stacktrace-api.html#clojure.stacktrace/print-throwable",
   :source-url
   "https://github.com/clojure/clojure/blob/dbb448f7709b20c392558e7d7871d1e9b28c9440/src/clj/clojure/stacktrace.clj#L40",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/dbb448f7709b20c392558e7d7871d1e9b28c9440/src/clj/clojure/stacktrace.clj",
   :file "src/clj/clojure/stacktrace.clj"}
  {:name "print-trace-element",
   :doc
   "Prints a Clojure-oriented view of one element in a stack trace.",
   :var-type "function",
   :line 28,
   :added "1.1",
   :namespace "clojure.stacktrace",
   :arglists ([e]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.stacktrace-api.html#clojure.stacktrace/print-trace-element",
   :source-url
   "https://github.com/clojure/clojure/blob/dbb448f7709b20c392558e7d7871d1e9b28c9440/src/clj/clojure/stacktrace.clj#L28",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/dbb448f7709b20c392558e7d7871d1e9b28c9440/src/clj/clojure/stacktrace.clj",
   :file "src/clj/clojure/stacktrace.clj"}
  {:name "root-cause",
   :doc "Returns the last 'cause' Throwable in a chain of Throwables.",
   :var-type "function",
   :line 20,
   :added "1.1",
   :namespace "clojure.stacktrace",
   :arglists ([tr]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.stacktrace-api.html#clojure.stacktrace/root-cause",
   :source-url
   "https://github.com/clojure/clojure/blob/dbb448f7709b20c392558e7d7871d1e9b28c9440/src/clj/clojure/stacktrace.clj#L20",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/dbb448f7709b20c392558e7d7871d1e9b28c9440/src/clj/clojure/stacktrace.clj",
   :file "src/clj/clojure/stacktrace.clj"}
  {:name "blank?",
   :doc "True if s is nil, empty, or contains only whitespace.",
   :var-type "function",
   :line 288,
   :added "1.2",
   :namespace "clojure.string",
   :arglists ([s]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/blank?",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L288",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :file "src/clj/clojure/string.clj"}
  {:name "capitalize",
   :doc
   "Converts first character of the string to upper-case, all other\ncharacters to lower-case.",
   :var-type "function",
   :line 196,
   :added "1.2",
   :namespace "clojure.string",
   :arglists ([s]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/capitalize",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L196",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :file "src/clj/clojure/string.clj"}
  {:name "ends-with?",
   :doc "True if s ends with substr.",
   :var-type "function",
   :line 367,
   :added "1.8",
   :namespace "clojure.string",
   :arglists ([s substr]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/ends-with?",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L367",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :file "src/clj/clojure/string.clj"}
  {:name "escape",
   :doc
   "Return a new string, using cmap to escape each character ch\nfrom s as follows:\n\nIf (cmap ch) is nil, append ch to the new string.\nIf (cmap ch) is non-nil, append (str (cmap ch)) instead.",
   :var-type "function",
   :line 301,
   :added "1.2",
   :namespace "clojure.string",
   :arglists ([s cmap]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/escape",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L301",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :file "src/clj/clojure/string.clj"}
  {:name "includes?",
   :doc "True if s includes substr.",
   :var-type "function",
   :line 373,
   :added "1.8",
   :namespace "clojure.string",
   :arglists ([s substr]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/includes?",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L373",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :file "src/clj/clojure/string.clj"}
  {:name "index-of",
   :doc
   "Return index of value (string or char) in s, optionally searching\nforward from from-index. Return nil if value not found.",
   :var-type "function",
   :line 319,
   :added "1.8",
   :namespace "clojure.string",
   :arglists ([s value] [s value from-index]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/index-of",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L319",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :file "src/clj/clojure/string.clj"}
  {:name "join",
   :doc
   "Returns a string of all elements in coll, as returned by (seq coll),\nseparated by an optional separator.",
   :var-type "function",
   :line 180,
   :added "1.2",
   :namespace "clojure.string",
   :arglists ([coll] [separator coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/join",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L180",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :file "src/clj/clojure/string.clj"}
  {:name "last-index-of",
   :doc
   "Return last index of value (string or char) in s, optionally\nsearching backward from from-index. Return nil if value not found.",
   :var-type "function",
   :line 340,
   :added "1.8",
   :namespace "clojure.string",
   :arglists ([s value] [s value from-index]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/last-index-of",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L340",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :file "src/clj/clojure/string.clj"}
  {:name "lower-case",
   :doc "Converts string to all lower-case.",
   :var-type "function",
   :line 213,
   :added "1.2",
   :namespace "clojure.string",
   :arglists ([s]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/lower-case",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L213",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :file "src/clj/clojure/string.clj"}
  {:name "re-quote-replacement",
   :doc
   "Given a replacement string that you wish to be a literal\nreplacement for a pattern match in replace or replace-first, do the\nnecessary escaping of special characters in the replacement.",
   :var-type "function",
   :line 54,
   :added "1.5",
   :namespace "clojure.string",
   :arglists ([replacement]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/re-quote-replacement",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L54",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :file "src/clj/clojure/string.clj"}
  {:name "replace",
   :doc
   "Replaces all instance of match with replacement in s.\n\nmatch/replacement can be:\n\nstring / string\nchar / char\npattern / (string or function of match).\n\nSee also replace-first.\n\nThe replacement is literal (i.e. none of its characters are treated\nspecially) for all cases above except pattern / string.\n\nFor pattern / string, $1, $2, etc. in the replacement string are\nsubstituted with the string that matched the corresponding\nparenthesized group in the pattern.  If you wish your replacement\nstring r to be used literally, use (re-quote-replacement r) as the\nreplacement argument.  See also documentation for\njava.util.regex.Matcher's appendReplacement method.\n\nExample:\n(clojure.string/replace \"Almost Pig Latin\" #\"\\b(\\w)(\\w+)\\b\" \"$2$1ay\")\n-> \"lmostAay igPay atinLay\"",
   :var-type "function",
   :line 75,
   :added "1.2",
   :namespace "clojure.string",
   :arglists ([s match replacement]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/replace",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L75",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :file "src/clj/clojure/string.clj"}
  {:name "replace-first",
   :doc
   "Replaces the first instance of match with replacement in s.\n\nmatch/replacement can be:\n\nchar / char\nstring / string\npattern / (string or function of match).\n\nSee also replace.\n\nThe replacement is literal (i.e. none of its characters are treated\nspecially) for all cases above except pattern / string.\n\nFor pattern / string, $1, $2, etc. in the replacement string are\nsubstituted with the string that matched the corresponding\nparenthesized group in the pattern.  If you wish your replacement\nstring r to be used literally, use (re-quote-replacement r) as the\nreplacement argument.  See also documentation for\njava.util.regex.Matcher's appendReplacement method.\n\nExample:\n(clojure.string/replace-first \"swap first two words\"\n                              #\"(\\w+)(\\s+)(\\w+)\" \"$3$2$1\")\n-> \"first swap two words\"",
   :var-type "function",
   :line 138,
   :added "1.2",
   :namespace "clojure.string",
   :arglists ([s match replacement]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/replace-first",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L138",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :file "src/clj/clojure/string.clj"}
  {:name "reverse",
   :doc "Returns s with its characters reversed.",
   :var-type "function",
   :line 48,
   :added "1.2",
   :namespace "clojure.string",
   :arglists ([s]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/reverse",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L48",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :file "src/clj/clojure/string.clj"}
  {:name "split",
   :doc
   "Splits string on a regular expression.  Optional argument limit is\nthe maximum number of parts. Not lazy. Returns vector of the parts.\nTrailing empty strings are not returned - pass limit of -1 to return all.",
   :var-type "function",
   :line 219,
   :added "1.2",
   :namespace "clojure.string",
   :arglists ([s re] [s re limit]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/split",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L219",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :file "src/clj/clojure/string.clj"}
  {:name "split-lines",
   :doc
   "Splits s on \\n or \\r\\n. Trailing empty lines are not returned.",
   :var-type "function",
   :line 229,
   :added "1.2",
   :namespace "clojure.string",
   :arglists ([s]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/split-lines",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L229",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :file "src/clj/clojure/string.clj"}
  {:name "starts-with?",
   :doc "True if s starts with substr.",
   :var-type "function",
   :line 361,
   :added "1.8",
   :namespace "clojure.string",
   :arglists ([s substr]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/starts-with?",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L361",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :file "src/clj/clojure/string.clj"}
  {:name "trim",
   :doc "Removes whitespace from both ends of string.",
   :var-type "function",
   :line 235,
   :added "1.2",
   :namespace "clojure.string",
   :arglists ([s]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/trim",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L235",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :file "src/clj/clojure/string.clj"}
  {:name "trim-newline",
   :doc
   "Removes all trailing newline \\n or return \\r characters from\nstring.  Similar to Perl's chomp.",
   :var-type "function",
   :line 275,
   :added "1.2",
   :namespace "clojure.string",
   :arglists ([s]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/trim-newline",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L275",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :file "src/clj/clojure/string.clj"}
  {:name "triml",
   :doc "Removes whitespace from the left side of string.",
   :var-type "function",
   :line 252,
   :added "1.2",
   :namespace "clojure.string",
   :arglists ([s]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/triml",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L252",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :file "src/clj/clojure/string.clj"}
  {:name "trimr",
   :doc "Removes whitespace from the right side of string.",
   :var-type "function",
   :line 264,
   :added "1.2",
   :namespace "clojure.string",
   :arglists ([s]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/trimr",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L264",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :file "src/clj/clojure/string.clj"}
  {:name "upper-case",
   :doc "Converts string to all upper-case.",
   :var-type "function",
   :line 207,
   :added "1.2",
   :namespace "clojure.string",
   :arglists ([s]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/upper-case",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L207",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :file "src/clj/clojure/string.clj"}
  {:name "apply-template",
   :doc
   "For use in macros.  argv is an argument list, as in defn.  expr is\na quoted expression using the symbols in argv.  values is a sequence\nof values to be used for the arguments.\n\napply-template will recursively replace argument symbols in expr\nwith their corresponding values, returning a modified expr.\n\nExample: (apply-template '[x] '(+ x x) '[2])\n         ;=> (+ 2 2)",
   :var-type "function",
   :line 30,
   :namespace "clojure.template",
   :arglists ([argv expr values]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.template-api.html#clojure.template/apply-template",
   :source-url
   "https://github.com/clojure/clojure/blob/c4c0740a0696bc95b2184c0fef55ed7c3bb097f6/src/clj/clojure/template.clj#L30",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/c4c0740a0696bc95b2184c0fef55ed7c3bb097f6/src/clj/clojure/template.clj",
   :file "src/clj/clojure/template.clj"}
  {:name "do-template",
   :doc
   "Repeatedly copies expr (in a do block) for each group of arguments\nin values.  values are automatically partitioned by the number of\narguments in argv, an argument vector as in defn.\n\nExample: (macroexpand '(do-template [x y] (+ y x) 2 4 3 5))\n         ;=> (do (+ 4 2) (+ 5 3))",
   :var-type "macro",
   :line 45,
   :namespace "clojure.template",
   :arglists ([argv expr & values]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.template-api.html#clojure.template/do-template",
   :source-url
   "https://github.com/clojure/clojure/blob/c4c0740a0696bc95b2184c0fef55ed7c3bb097f6/src/clj/clojure/template.clj#L45",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/c4c0740a0696bc95b2184c0fef55ed7c3bb097f6/src/clj/clojure/template.clj",
   :file "src/clj/clojure/template.clj"}
  {:name "*load-tests*",
   :doc
   "True by default.  If set to false, no test functions will\nbe created by deftest, set-test, or with-test.  Use this to omit\ntests when compiling or loading production code.",
   :var-type "var",
   :line 247,
   :added "1.1",
   :dynamic true,
   :namespace "clojure.test",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/*load-tests*",
   :source-url
   "https://github.com/clojure/clojure/blob/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj#L247",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj",
   :file "src/clj/clojure/test.clj"}
  {:name "*stack-trace-depth*",
   :doc
   "The maximum depth of stack traces to print when an Exception\nis thrown during a test.  Defaults to nil, which means print the \ncomplete stack trace.",
   :var-type "var",
   :line 254,
   :added "1.1",
   :dynamic true,
   :namespace "clojure.test",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/*stack-trace-depth*",
   :source-url
   "https://github.com/clojure/clojure/blob/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj#L254",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj",
   :file "src/clj/clojure/test.clj"}
  {:name "are",
   :doc
   "Checks multiple assertions with a template expression.\nSee clojure.template/do-template for an explanation of\ntemplates.\n\nExample: (are [x y] (= x y)  \n              2 (+ 1 1)\n              4 (* 2 2))\nExpands to: \n         (do (is (= 2 (+ 1 1)))\n             (is (= 4 (* 2 2))))\n\nNote: This breaks some reporting features, such as line numbers.",
   :var-type "macro",
   :line 574,
   :added "1.1",
   :namespace "clojure.test",
   :arglists ([argv expr & args]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/are",
   :source-url
   "https://github.com/clojure/clojure/blob/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj#L574",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj",
   :file "src/clj/clojure/test.clj"}
  {:name "assert-any",
   :doc
   "Returns generic assertion code for any test, including macros, Java\nmethod calls, or isolated symbols.",
   :var-type "function",
   :line 457,
   :added "1.1",
   :namespace "clojure.test",
   :arglists ([msg form]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/assert-any",
   :source-url
   "https://github.com/clojure/clojure/blob/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj#L457",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj",
   :file "src/clj/clojure/test.clj"}
  {:name "assert-predicate",
   :doc
   "Returns generic assertion code for any functional predicate.  The\n'expected' argument to 'report' will contains the original form, the\n'actual' argument will contain the form with all its sub-forms\nevaluated.  If the predicate returns false, the 'actual' form will\nbe wrapped in (not...).",
   :var-type "function",
   :line 438,
   :added "1.1",
   :namespace "clojure.test",
   :arglists ([msg form]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/assert-predicate",
   :source-url
   "https://github.com/clojure/clojure/blob/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj#L438",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj",
   :file "src/clj/clojure/test.clj"}
  {:name "compose-fixtures",
   :doc
   "Composes two fixture functions, creating a new fixture function\nthat combines their behavior.",
   :var-type "function",
   :line 691,
   :added "1.1",
   :namespace "clojure.test",
   :arglists ([f1 f2]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/compose-fixtures",
   :source-url
   "https://github.com/clojure/clojure/blob/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj#L691",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj",
   :file "src/clj/clojure/test.clj"}
  {:name "deftest",
   :doc
   "Defines a test function with no arguments.  Test functions may call\nother tests, so tests may be composed.  If you compose tests, you\nshould also define a function named test-ns-hook; run-tests will\ncall test-ns-hook instead of testing all vars.\n\nNote: Actually, the test body goes in the :test metadata on the var,\nand the real function (the value of the var) calls test-var on\nitself.\n\nWhen *load-tests* is false, deftest is ignored.",
   :var-type "macro",
   :line 624,
   :added "1.1",
   :namespace "clojure.test",
   :arglists ([name & body]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/deftest",
   :source-url
   "https://github.com/clojure/clojure/blob/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj#L624",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj",
   :file "src/clj/clojure/test.clj"}
  {:name "deftest-",
   :doc "Like deftest but creates a private var.",
   :var-type "macro",
   :line 641,
   :added "1.1",
   :namespace "clojure.test",
   :arglists ([name & body]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/deftest-",
   :source-url
   "https://github.com/clojure/clojure/blob/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj#L641",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj",
   :file "src/clj/clojure/test.clj"}
  {:name "do-report",
   :doc
   "Add file and line information to a test result and call report.\nIf you are writing a custom assert-expr method, call this function\nto pass test results to report.",
   :var-type "function",
   :line 353,
   :added "1.2",
   :namespace "clojure.test",
   :arglists ([m]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/do-report",
   :source-url
   "https://github.com/clojure/clojure/blob/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj#L353",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj",
   :file "src/clj/clojure/test.clj"}
  {:name "file-position",
   :doc
   "Returns a vector [filename line-number] for the nth call up the\nstack.\n\nDeprecated in 1.2: The information needed for test reporting is\nnow on :file and :line keys in the result map.",
   :var-type "function",
   :line 284,
   :added "1.1",
   :deprecated "1.2",
   :namespace "clojure.test",
   :arglists ([n]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/file-position",
   :source-url
   "https://github.com/clojure/clojure/blob/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj#L284",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj",
   :file "src/clj/clojure/test.clj"}
  {:name "function?",
   :doc
   "Returns true if argument is a function or a symbol that resolves to\na function (not a macro).",
   :var-type "function",
   :line 426,
   :added "1.1",
   :namespace "clojure.test",
   :arglists ([x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/function?",
   :source-url
   "https://github.com/clojure/clojure/blob/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj#L426",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj",
   :file "src/clj/clojure/test.clj"}
  {:name "get-possibly-unbound-var",
   :doc "Like var-get but returns nil if the var is unbound.",
   :var-type "function",
   :line 418,
   :added "1.1",
   :namespace "clojure.test",
   :arglists ([v]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/get-possibly-unbound-var",
   :source-url
   "https://github.com/clojure/clojure/blob/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj#L418",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj",
   :file "src/clj/clojure/test.clj"}
  {:name "inc-report-counter",
   :doc
   "Increments the named counter in *report-counters*, a ref to a map.\nDoes nothing if *report-counters* is nil.",
   :var-type "function",
   :line 316,
   :added "1.1",
   :namespace "clojure.test",
   :arglists ([name]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/inc-report-counter",
   :source-url
   "https://github.com/clojure/clojure/blob/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj#L316",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj",
   :file "src/clj/clojure/test.clj"}
  {:name "is",
   :doc
   "Generic assertion macro.  'form' is any predicate test.\n'msg' is an optional message to attach to the assertion.\n\nExample: (is (= 4 (+ 2 2)) \"Two plus two should be 4\")\n\nSpecial forms:\n\n(is (thrown? c body)) checks that an instance of c is thrown from\nbody, fails if not; then returns the thing thrown.\n\n(is (thrown-with-msg? c re body)) checks that an instance of c is\nthrown AND that the message on the exception matches (with\nre-find) the regular expression re.",
   :var-type "macro",
   :line 556,
   :added "1.1",
   :namespace "clojure.test",
   :arglists ([form] [form msg]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/is",
   :source-url
   "https://github.com/clojure/clojure/blob/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj#L556",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj",
   :file "src/clj/clojure/test.clj"}
  {:name "join-fixtures",
   :doc
   "Composes a collection of fixtures, in order.  Always returns a valid\nfixture function, even if the collection is empty.",
   :var-type "function",
   :line 698,
   :added "1.1",
   :namespace "clojure.test",
   :arglists ([fixtures]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/join-fixtures",
   :source-url
   "https://github.com/clojure/clojure/blob/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj#L698",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj",
   :file "src/clj/clojure/test.clj"}
  {:name "report",
   :doc
   "Generic reporting function, may be overridden to plug in\ndifferent report formats (e.g., TAP, JUnit).  Assertions such as\n'is' call 'report' to indicate results. The argument given to\n'report' will be a map with a :type key. See (doc clojure.test)\ndocs under 'EXTENDING TEST-IS' for types of arguments for\n'report'.",
   :var-type "multimethod",
   :line 326,
   :added "1.1",
   :dynamic true,
   :namespace "clojure.test",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/report",
   :source-url
   "https://github.com/clojure/clojure/blob/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj#L326",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj",
   :file "src/clj/clojure/test.clj"}
  {:name "run-all-tests",
   :doc
   "Runs all tests in all namespaces; prints results.\nOptional argument is a regular expression; only namespaces with\nnames matching the regular expression (with re-matches) will be\ntested.",
   :var-type "function",
   :line 782,
   :added "1.1",
   :namespace "clojure.test",
   :arglists ([] [re]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/run-all-tests",
   :source-url
   "https://github.com/clojure/clojure/blob/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj#L782",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj",
   :file "src/clj/clojure/test.clj"}
  {:name "run-test",
   :doc
   "Runs a single test.\n\nBecause the intent is to run a single test, there is no check for the namespace test-ns-hook.",
   :var-type "macro",
   :line 815,
   :added "1.11",
   :namespace "clojure.test",
   :arglists ([test-symbol]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/run-test",
   :source-url
   "https://github.com/clojure/clojure/blob/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj#L815",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj",
   :file "src/clj/clojure/test.clj"}
  {:name "run-test-var",
   :doc
   "Runs the tests for a single Var, with fixtures executed around the test, and summary output after.",
   :var-type "function",
   :line 799,
   :added "1.11",
   :namespace "clojure.test",
   :arglists ([v]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/run-test-var",
   :source-url
   "https://github.com/clojure/clojure/blob/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj#L799",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj",
   :file "src/clj/clojure/test.clj"}
  {:name "run-tests",
   :doc
   "Runs all tests in the given namespaces; prints results.\nDefaults to current namespace if none given.  Returns a map\nsummarizing test results.",
   :var-type "function",
   :line 770,
   :added "1.1",
   :namespace "clojure.test",
   :arglists ([] [& namespaces]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/run-tests",
   :source-url
   "https://github.com/clojure/clojure/blob/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj#L770",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj",
   :file "src/clj/clojure/test.clj"}
  {:name "set-test",
   :doc
   "Experimental.\nSets :test metadata of the named var to a fn with the given body.\nThe var must already exist.  Does not modify the value of the var.\n\nWhen *load-tests* is false, set-test is ignored.",
   :var-type "macro",
   :line 650,
   :added "1.1",
   :namespace "clojure.test",
   :arglists ([name & body]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/set-test",
   :source-url
   "https://github.com/clojure/clojure/blob/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj#L650",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj",
   :file "src/clj/clojure/test.clj"}
  {:name "successful?",
   :doc
   "Returns true if the given test summary indicates all tests\nwere successful, false otherwise.",
   :var-type "function",
   :line 791,
   :added "1.1",
   :namespace "clojure.test",
   :arglists ([summary]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/successful?",
   :source-url
   "https://github.com/clojure/clojure/blob/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj#L791",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj",
   :file "src/clj/clojure/test.clj"}
  {:name "test-all-vars",
   :doc
   "Calls test-vars on every var interned in the namespace, with fixtures.",
   :var-type "function",
   :line 739,
   :added "1.1",
   :namespace "clojure.test",
   :arglists ([ns]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/test-all-vars",
   :source-url
   "https://github.com/clojure/clojure/blob/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj#L739",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj",
   :file "src/clj/clojure/test.clj"}
  {:name "test-ns",
   :doc
   "If the namespace defines a function named test-ns-hook, calls that.\nOtherwise, calls test-all-vars on the namespace.  'ns' is a\nnamespace object or a symbol.\n\nInternally binds *report-counters* to a ref initialized to\n*initial-report-counters*.  Returns the final, dereferenced state of\n*report-counters*.",
   :var-type "function",
   :line 745,
   :added "1.1",
   :namespace "clojure.test",
   :arglists ([ns]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/test-ns",
   :source-url
   "https://github.com/clojure/clojure/blob/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj#L745",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj",
   :file "src/clj/clojure/test.clj"}
  {:name "test-var",
   :doc
   "If v has a function in its :test metadata, calls that function,\nwith *testing-vars* bound to (conj *testing-vars* v).",
   :var-type "function",
   :line 710,
   :added "1.1",
   :dynamic true,
   :namespace "clojure.test",
   :arglists ([v]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/test-var",
   :source-url
   "https://github.com/clojure/clojure/blob/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj#L710",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj",
   :file "src/clj/clojure/test.clj"}
  {:name "test-vars",
   :doc
   "Groups vars by their namespace and runs test-var on them with\nappropriate fixtures applied.",
   :var-type "function",
   :line 725,
   :added "1.6",
   :namespace "clojure.test",
   :arglists ([vars]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/test-vars",
   :source-url
   "https://github.com/clojure/clojure/blob/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj#L725",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj",
   :file "src/clj/clojure/test.clj"}
  {:name "testing",
   :doc
   "Adds a new string to the list of testing contexts.  May be nested,\nbut must occur inside a test function (deftest).",
   :var-type "macro",
   :line 599,
   :added "1.1",
   :namespace "clojure.test",
   :arglists ([string & body]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/testing",
   :source-url
   "https://github.com/clojure/clojure/blob/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj#L599",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj",
   :file "src/clj/clojure/test.clj"}
  {:name "testing-contexts-str",
   :doc
   "Returns a string representation of the current test context. Joins\nstrings in *testing-contexts* with spaces.",
   :var-type "function",
   :line 309,
   :added "1.1",
   :namespace "clojure.test",
   :arglists ([]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/testing-contexts-str",
   :source-url
   "https://github.com/clojure/clojure/blob/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj#L309",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj",
   :file "src/clj/clojure/test.clj"}
  {:name "testing-vars-str",
   :doc
   "Returns a string representation of the current test.  Renders names\nin *testing-vars* as a list, then the source file and line of\ncurrent assertion.",
   :var-type "function",
   :line 296,
   :added "1.1",
   :namespace "clojure.test",
   :arglists ([m]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/testing-vars-str",
   :source-url
   "https://github.com/clojure/clojure/blob/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj#L296",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj",
   :file "src/clj/clojure/test.clj"}
  {:name "try-expr",
   :doc
   "Used by the 'is' macro to catch unexpected exceptions.\nYou don't call this.",
   :var-type "macro",
   :line 540,
   :added "1.1",
   :namespace "clojure.test",
   :arglists ([msg form]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/try-expr",
   :source-url
   "https://github.com/clojure/clojure/blob/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj#L540",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj",
   :file "src/clj/clojure/test.clj"}
  {:name "use-fixtures",
   :doc
   "Wrap test runs in a fixture function to perform setup and\nteardown. Using a fixture-type of :each wraps every test\nindividually, while :once wraps the whole run in a single function.",
   :var-type "multimethod",
   :line 672,
   :added "1.1",
   :namespace "clojure.test",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/use-fixtures",
   :source-url
   "https://github.com/clojure/clojure/blob/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj#L672",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj",
   :file "src/clj/clojure/test.clj"}
  {:name "with-test",
   :doc
   "Takes any definition form (that returns a Var) as the first argument.\nRemaining body goes in the :test metadata function for that Var.\n\nWhen *load-tests* is false, only evaluates the definition, ignoring\nthe tests.",
   :var-type "macro",
   :line 611,
   :added "1.1",
   :namespace "clojure.test",
   :arglists ([definition & body]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/with-test",
   :source-url
   "https://github.com/clojure/clojure/blob/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj#L611",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj",
   :file "src/clj/clojure/test.clj"}
  {:name "with-test-out",
   :doc "Runs body with *out* bound to the value of *test-out*.",
   :var-type "macro",
   :line 275,
   :added "1.1",
   :namespace "clojure.test",
   :arglists ([& body]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/with-test-out",
   :source-url
   "https://github.com/clojure/clojure/blob/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj#L275",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/cd3e9f6e5f9bbeda02c0b66a592578277769fad3/src/clj/clojure/test.clj",
   :file "src/clj/clojure/test.clj"}
  {:name "invoke-tool",
   :doc
   "Invoke tool using Clojure CLI. Args (one of :tool-alias or :tool-name, and :fn\nare required):\n  :tool-alias - Tool alias to invoke (keyword)\n  :tool-name - Name of installed tool to invoke (string or symbol)\n  :fn - Function (symbol)\n  :args - map of args to pass to function\n\nOptions:\n  :preserve-envelope - if true, return the full invocation envelope, default=false",
   :var-type "function",
   :line 41,
   :added "1.12",
   :dynamic true,
   :namespace "clojure.tools.deps.interop",
   :arglists
   ([{:keys [tool-name tool-alias fn args preserve-envelope],
      :or {preserve-envelope false},
      :as opts}]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.tools.deps.interop-api.html#clojure.tools.deps.interop/invoke-tool",
   :source-url
   "https://github.com/clojure/clojure/blob/d1de868e66a8d46cae164202a0e9ca9e670df204/src/clj/clojure/tools/deps/interop.clj#L41",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/d1de868e66a8d46cae164202a0e9ca9e670df204/src/clj/clojure/tools/deps/interop.clj",
   :file "src/clj/clojure/tools/deps/interop.clj"}
  {:name "keywordize-keys",
   :doc
   "Recursively transforms all map keys from strings to keywords.",
   :var-type "function",
   :line 94,
   :added "1.1",
   :namespace "clojure.walk",
   :arglists ([m]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.walk-api.html#clojure.walk/keywordize-keys",
   :source-url
   "https://github.com/clojure/clojure/blob/4dbdcb1fe8128620162839a60192d899eb3c83f7/src/clj/clojure/walk.clj#L94",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/4dbdcb1fe8128620162839a60192d899eb3c83f7/src/clj/clojure/walk.clj",
   :file "src/clj/clojure/walk.clj"}
  {:name "macroexpand-all",
   :doc "Recursively performs all possible macroexpansions in form.",
   :var-type "function",
   :line 126,
   :added "1.1",
   :namespace "clojure.walk",
   :arglists ([form]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.walk-api.html#clojure.walk/macroexpand-all",
   :source-url
   "https://github.com/clojure/clojure/blob/4dbdcb1fe8128620162839a60192d899eb3c83f7/src/clj/clojure/walk.clj#L126",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/4dbdcb1fe8128620162839a60192d899eb3c83f7/src/clj/clojure/walk.clj",
   :file "src/clj/clojure/walk.clj"}
  {:name "postwalk",
   :doc
   "Performs a depth-first, post-order traversal of form.  Calls f on\neach sub-form, uses f's return value in place of the original.\nRecognizes all Clojure data structures. Consumes seqs as with doall.",
   :var-type "function",
   :line 53,
   :added "1.1",
   :namespace "clojure.walk",
   :arglists ([f form]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.walk-api.html#clojure.walk/postwalk",
   :source-url
   "https://github.com/clojure/clojure/blob/4dbdcb1fe8128620162839a60192d899eb3c83f7/src/clj/clojure/walk.clj#L53",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/4dbdcb1fe8128620162839a60192d899eb3c83f7/src/clj/clojure/walk.clj",
   :file "src/clj/clojure/walk.clj"}
  {:name "postwalk-demo",
   :doc
   "Demonstrates the behavior of postwalk by printing each form as it is\nwalked.  Returns form.",
   :var-type "function",
   :line 80,
   :added "1.1",
   :namespace "clojure.walk",
   :arglists ([form]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.walk-api.html#clojure.walk/postwalk-demo",
   :source-url
   "https://github.com/clojure/clojure/blob/4dbdcb1fe8128620162839a60192d899eb3c83f7/src/clj/clojure/walk.clj#L80",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/4dbdcb1fe8128620162839a60192d899eb3c83f7/src/clj/clojure/walk.clj",
   :file "src/clj/clojure/walk.clj"}
  {:name "postwalk-replace",
   :doc
   "Recursively transforms form by replacing keys in smap with their\nvalues.  Like clojure/replace but works on any data structure.  Does\nreplacement at the leaves of the tree first.",
   :var-type "function",
   :line 118,
   :added "1.1",
   :namespace "clojure.walk",
   :arglists ([smap form]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.walk-api.html#clojure.walk/postwalk-replace",
   :source-url
   "https://github.com/clojure/clojure/blob/4dbdcb1fe8128620162839a60192d899eb3c83f7/src/clj/clojure/walk.clj#L118",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/4dbdcb1fe8128620162839a60192d899eb3c83f7/src/clj/clojure/walk.clj",
   :file "src/clj/clojure/walk.clj"}
  {:name "prewalk",
   :doc "Like postwalk, but does pre-order traversal.",
   :var-type "function",
   :line 61,
   :added "1.1",
   :namespace "clojure.walk",
   :arglists ([f form]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.walk-api.html#clojure.walk/prewalk",
   :source-url
   "https://github.com/clojure/clojure/blob/4dbdcb1fe8128620162839a60192d899eb3c83f7/src/clj/clojure/walk.clj#L61",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/4dbdcb1fe8128620162839a60192d899eb3c83f7/src/clj/clojure/walk.clj",
   :file "src/clj/clojure/walk.clj"}
  {:name "prewalk-demo",
   :doc
   "Demonstrates the behavior of prewalk by printing each form as it is\nwalked.  Returns form.",
   :var-type "function",
   :line 87,
   :added "1.1",
   :namespace "clojure.walk",
   :arglists ([form]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.walk-api.html#clojure.walk/prewalk-demo",
   :source-url
   "https://github.com/clojure/clojure/blob/4dbdcb1fe8128620162839a60192d899eb3c83f7/src/clj/clojure/walk.clj#L87",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/4dbdcb1fe8128620162839a60192d899eb3c83f7/src/clj/clojure/walk.clj",
   :file "src/clj/clojure/walk.clj"}
  {:name "prewalk-replace",
   :doc
   "Recursively transforms form by replacing keys in smap with their\nvalues.  Like clojure/replace but works on any data structure.  Does\nreplacement at the root of the tree first.",
   :var-type "function",
   :line 110,
   :added "1.1",
   :namespace "clojure.walk",
   :arglists ([smap form]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.walk-api.html#clojure.walk/prewalk-replace",
   :source-url
   "https://github.com/clojure/clojure/blob/4dbdcb1fe8128620162839a60192d899eb3c83f7/src/clj/clojure/walk.clj#L110",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/4dbdcb1fe8128620162839a60192d899eb3c83f7/src/clj/clojure/walk.clj",
   :file "src/clj/clojure/walk.clj"}
  {:name "stringify-keys",
   :doc
   "Recursively transforms all map keys from keywords to strings.",
   :var-type "function",
   :line 102,
   :added "1.1",
   :namespace "clojure.walk",
   :arglists ([m]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.walk-api.html#clojure.walk/stringify-keys",
   :source-url
   "https://github.com/clojure/clojure/blob/4dbdcb1fe8128620162839a60192d899eb3c83f7/src/clj/clojure/walk.clj#L102",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/4dbdcb1fe8128620162839a60192d899eb3c83f7/src/clj/clojure/walk.clj",
   :file "src/clj/clojure/walk.clj"}
  {:name "walk",
   :doc
   "Traverses form, an arbitrary data structure.  inner and outer are\nfunctions.  Applies inner to each element of form, building up a\ndata structure of the same type, then applies outer to the result.\nRecognizes all Clojure data structures. Consumes seqs as with doall.",
   :var-type "function",
   :line 35,
   :added "1.1",
   :namespace "clojure.walk",
   :arglists ([inner outer form]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.walk-api.html#clojure.walk/walk",
   :source-url
   "https://github.com/clojure/clojure/blob/4dbdcb1fe8128620162839a60192d899eb3c83f7/src/clj/clojure/walk.clj#L35",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/4dbdcb1fe8128620162839a60192d899eb3c83f7/src/clj/clojure/walk.clj",
   :file "src/clj/clojure/walk.clj"}
  {:name "disable-external-entities",
   :doc
   "Modifies a SAXParser to disable external entity resolution to prevent XXE attacks",
   :var-type "function",
   :line 81,
   :added "1.11",
   :namespace "clojure.xml",
   :arglists ([parser]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.xml-api.html#clojure.xml/disable-external-entities",
   :source-url
   "https://github.com/clojure/clojure/blob/4a4a6e7717d411679820c4a3ce735a77aef45cc3/src/clj/clojure/xml.clj#L81",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/4a4a6e7717d411679820c4a3ce735a77aef45cc3/src/clj/clojure/xml.clj",
   :file "src/clj/clojure/xml.clj"}
  {:name "parse",
   :doc
   "Parses and loads the source s, which can be a File, InputStream or\nString naming a URI. Returns a tree of the xml/element struct-map,\nwhich has the keys :tag, :attrs, and :content. and accessor fns tag,\nattrs, and content. Other parsers can be supplied by passing\nstartparse, a fn taking a source and a ContentHandler and returning\na parser.\n\nPrior to 1.11, used startparse-sax by default. As of 1.11, uses\nstartparse-sax-safe, which disables XXE (XML External Entity)\nprocessing. Pass startparse-sax to revert to prior behavior.",
   :var-type "function",
   :line 106,
   :added "1.0",
   :namespace "clojure.xml",
   :arglists ([s] [s startparse]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.xml-api.html#clojure.xml/parse",
   :source-url
   "https://github.com/clojure/clojure/blob/4a4a6e7717d411679820c4a3ce735a77aef45cc3/src/clj/clojure/xml.clj#L106",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/4a4a6e7717d411679820c4a3ce735a77aef45cc3/src/clj/clojure/xml.clj",
   :file "src/clj/clojure/xml.clj"}
  {:name "sax-parser",
   :doc "Create a new SAXParser",
   :var-type "function",
   :line 75,
   :added "1.11",
   :namespace "clojure.xml",
   :arglists ([]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.xml-api.html#clojure.xml/sax-parser",
   :source-url
   "https://github.com/clojure/clojure/blob/4a4a6e7717d411679820c4a3ce735a77aef45cc3/src/clj/clojure/xml.clj#L75",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/4a4a6e7717d411679820c4a3ce735a77aef45cc3/src/clj/clojure/xml.clj",
   :file "src/clj/clojure/xml.clj"}
  {:name "startparse-sax",
   :doc
   "A startparse function suitable for use with clojure.xml/parse.\nNote that this function is open to XXE entity attacks, see startparse-sax-safe.",
   :var-type "function",
   :line 92,
   :added "1.0",
   :namespace "clojure.xml",
   :arglists ([s ch]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.xml-api.html#clojure.xml/startparse-sax",
   :source-url
   "https://github.com/clojure/clojure/blob/4a4a6e7717d411679820c4a3ce735a77aef45cc3/src/clj/clojure/xml.clj#L92",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/4a4a6e7717d411679820c4a3ce735a77aef45cc3/src/clj/clojure/xml.clj",
   :file "src/clj/clojure/xml.clj"}
  {:name "startparse-sax-safe",
   :doc
   "A startparse function suitable for use with clojure.xml/parse.\nExternal entity resolution is disabled to prevent XXE entity attacks.",
   :var-type "function",
   :line 99,
   :added "1.11",
   :namespace "clojure.xml",
   :arglists ([s ch]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.xml-api.html#clojure.xml/startparse-sax-safe",
   :source-url
   "https://github.com/clojure/clojure/blob/4a4a6e7717d411679820c4a3ce735a77aef45cc3/src/clj/clojure/xml.clj#L99",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/4a4a6e7717d411679820c4a3ce735a77aef45cc3/src/clj/clojure/xml.clj",
   :file "src/clj/clojure/xml.clj"}
  {:name "append-child",
   :doc
   "Inserts the item as the rightmost child of the node at this loc,\nwithout moving",
   :var-type "function",
   :line 223,
   :added "1.0",
   :namespace "clojure.zip",
   :arglists ([loc item]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/append-child",
   :source-url
   "https://github.com/clojure/clojure/blob/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj#L223",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj",
   :file "src/clj/clojure/zip.clj"}
  {:name "branch?",
   :doc "Returns true if the node at loc is a branch",
   :var-type "function",
   :line 69,
   :added "1.0",
   :namespace "clojure.zip",
   :arglists ([loc]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/branch?",
   :source-url
   "https://github.com/clojure/clojure/blob/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj#L69",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj",
   :file "src/clj/clojure/zip.clj"}
  {:name "children",
   :doc
   "Returns a seq of the children of node at loc, which must be a branch",
   :var-type "function",
   :line 75,
   :added "1.0",
   :namespace "clojure.zip",
   :arglists ([loc]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/children",
   :source-url
   "https://github.com/clojure/clojure/blob/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj#L75",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj",
   :file "src/clj/clojure/zip.clj"}
  {:name "down",
   :doc
   "Returns the loc of the leftmost child of the node at this loc, or\nnil if no children",
   :var-type "function",
   :line 109,
   :added "1.0",
   :namespace "clojure.zip",
   :arglists ([loc]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/down",
   :source-url
   "https://github.com/clojure/clojure/blob/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj#L109",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj",
   :file "src/clj/clojure/zip.clj"}
  {:name "edit",
   :doc
   "Replaces the node at this loc with the value of (f node args)",
   :var-type "function",
   :line 210,
   :added "1.0",
   :namespace "clojure.zip",
   :arglists ([loc f & args]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/edit",
   :source-url
   "https://github.com/clojure/clojure/blob/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj#L210",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj",
   :file "src/clj/clojure/zip.clj"}
  {:name "end?",
   :doc "Returns true if loc represents the end of a depth-first walk",
   :var-type "function",
   :line 258,
   :added "1.0",
   :namespace "clojure.zip",
   :arglists ([loc]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/end?",
   :source-url
   "https://github.com/clojure/clojure/blob/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj#L258",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj",
   :file "src/clj/clojure/zip.clj"}
  {:name "insert-child",
   :doc
   "Inserts the item as the leftmost child of the node at this loc,\nwithout moving",
   :var-type "function",
   :line 216,
   :added "1.0",
   :namespace "clojure.zip",
   :arglists ([loc item]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/insert-child",
   :source-url
   "https://github.com/clojure/clojure/blob/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj#L216",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj",
   :file "src/clj/clojure/zip.clj"}
  {:name "insert-left",
   :doc
   "Inserts the item as the left sibling of the node at this loc,\nwithout moving",
   :var-type "function",
   :line 183,
   :added "1.0",
   :namespace "clojure.zip",
   :arglists ([loc item]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/insert-left",
   :source-url
   "https://github.com/clojure/clojure/blob/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj#L183",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj",
   :file "src/clj/clojure/zip.clj"}
  {:name "insert-right",
   :doc
   "Inserts the item as the right sibling of the node at this loc,\nwithout moving",
   :var-type "function",
   :line 193,
   :added "1.0",
   :namespace "clojure.zip",
   :arglists ([loc item]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/insert-right",
   :source-url
   "https://github.com/clojure/clojure/blob/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj#L193",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj",
   :file "src/clj/clojure/zip.clj"}
  {:name "left",
   :doc
   "Returns the loc of the left sibling of the node at this loc, or nil",
   :var-type "function",
   :line 166,
   :added "1.0",
   :namespace "clojure.zip",
   :arglists ([loc]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/left",
   :source-url
   "https://github.com/clojure/clojure/blob/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj#L166",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj",
   :file "src/clj/clojure/zip.clj"}
  {:name "leftmost",
   :doc
   "Returns the loc of the leftmost sibling of the node at this loc, or self",
   :var-type "function",
   :line 174,
   :added "1.0",
   :namespace "clojure.zip",
   :arglists ([loc]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/leftmost",
   :source-url
   "https://github.com/clojure/clojure/blob/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj#L174",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj",
   :file "src/clj/clojure/zip.clj"}
  {:name "lefts",
   :doc "Returns a seq of the left siblings of this loc",
   :var-type "function",
   :line 96,
   :added "1.0",
   :namespace "clojure.zip",
   :arglists ([loc]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/lefts",
   :source-url
   "https://github.com/clojure/clojure/blob/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj#L96",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj",
   :file "src/clj/clojure/zip.clj"}
  {:name "make-node",
   :doc
   "Returns a new branch node, given an existing node and new\nchildren. The loc is only used to supply the constructor.",
   :var-type "function",
   :line 83,
   :added "1.0",
   :namespace "clojure.zip",
   :arglists ([loc node children]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/make-node",
   :source-url
   "https://github.com/clojure/clojure/blob/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj#L83",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj",
   :file "src/clj/clojure/zip.clj"}
  {:name "next",
   :doc
   "Moves to the next loc in the hierarchy, depth-first. When reaching\nthe end, returns a distinguished loc detectable via end?. If already\nat the end, stays there.",
   :var-type "function",
   :line 230,
   :added "1.0",
   :namespace "clojure.zip",
   :arglists ([loc]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/next",
   :source-url
   "https://github.com/clojure/clojure/blob/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj#L230",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj",
   :file "src/clj/clojure/zip.clj"}
  {:name "node",
   :doc "Returns the node at loc",
   :var-type "function",
   :line 64,
   :added "1.0",
   :namespace "clojure.zip",
   :arglists ([loc]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/node",
   :source-url
   "https://github.com/clojure/clojure/blob/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj#L64",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj",
   :file "src/clj/clojure/zip.clj"}
  {:name "path",
   :doc "Returns a seq of nodes leading to this loc",
   :var-type "function",
   :line 90,
   :added "1.0",
   :namespace "clojure.zip",
   :arglists ([loc]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/path",
   :source-url
   "https://github.com/clojure/clojure/blob/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj#L90",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj",
   :file "src/clj/clojure/zip.clj"}
  {:name "prev",
   :doc
   "Moves to the previous loc in the hierarchy, depth-first. If already\nat the root, returns nil.",
   :var-type "function",
   :line 246,
   :added "1.0",
   :namespace "clojure.zip",
   :arglists ([loc]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/prev",
   :source-url
   "https://github.com/clojure/clojure/blob/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj#L246",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj",
   :file "src/clj/clojure/zip.clj"}
  {:name "remove",
   :doc
   "Removes the node at loc, returning the loc that would have preceded\nit in a depth-first walk.",
   :var-type "function",
   :line 264,
   :added "1.0",
   :namespace "clojure.zip",
   :arglists ([loc]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/remove",
   :source-url
   "https://github.com/clojure/clojure/blob/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj#L264",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj",
   :file "src/clj/clojure/zip.clj"}
  {:name "replace",
   :doc "Replaces the node at this loc, without moving",
   :var-type "function",
   :line 203,
   :added "1.0",
   :namespace "clojure.zip",
   :arglists ([loc node]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/replace",
   :source-url
   "https://github.com/clojure/clojure/blob/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj#L203",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj",
   :file "src/clj/clojure/zip.clj"}
  {:name "right",
   :doc
   "Returns the loc of the right sibling of the node at this loc, or nil",
   :var-type "function",
   :line 149,
   :added "1.0",
   :namespace "clojure.zip",
   :arglists ([loc]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/right",
   :source-url
   "https://github.com/clojure/clojure/blob/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj#L149",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj",
   :file "src/clj/clojure/zip.clj"}
  {:name "rightmost",
   :doc
   "Returns the loc of the rightmost sibling of the node at this loc, or self",
   :var-type "function",
   :line 157,
   :added "1.0",
   :namespace "clojure.zip",
   :arglists ([loc]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/rightmost",
   :source-url
   "https://github.com/clojure/clojure/blob/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj#L157",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj",
   :file "src/clj/clojure/zip.clj"}
  {:name "rights",
   :doc "Returns a seq of the right siblings of this loc",
   :var-type "function",
   :line 102,
   :added "1.0",
   :namespace "clojure.zip",
   :arglists ([loc]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/rights",
   :source-url
   "https://github.com/clojure/clojure/blob/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj#L102",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj",
   :file "src/clj/clojure/zip.clj"}
  {:name "root",
   :doc
   "zips all the way up and returns the root node, reflecting any\nchanges.",
   :var-type "function",
   :line 137,
   :added "1.0",
   :namespace "clojure.zip",
   :arglists ([loc]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/root",
   :source-url
   "https://github.com/clojure/clojure/blob/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj#L137",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj",
   :file "src/clj/clojure/zip.clj"}
  {:name "seq-zip",
   :doc "Returns a zipper for nested sequences, given a root sequence",
   :var-type "function",
   :line 35,
   :added "1.0",
   :namespace "clojure.zip",
   :arglists ([root]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/seq-zip",
   :source-url
   "https://github.com/clojure/clojure/blob/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj#L35",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj",
   :file "src/clj/clojure/zip.clj"}
  {:name "up",
   :doc
   "Returns the loc of the parent of the node at this loc, or nil if at\nthe top",
   :var-type "function",
   :line 123,
   :added "1.0",
   :namespace "clojure.zip",
   :arglists ([loc]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/up",
   :source-url
   "https://github.com/clojure/clojure/blob/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj#L123",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj",
   :file "src/clj/clojure/zip.clj"}
  {:name "vector-zip",
   :doc "Returns a zipper for nested vectors, given a root vector",
   :var-type "function",
   :line 44,
   :added "1.0",
   :namespace "clojure.zip",
   :arglists ([root]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/vector-zip",
   :source-url
   "https://github.com/clojure/clojure/blob/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj#L44",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj",
   :file "src/clj/clojure/zip.clj"}
  {:name "xml-zip",
   :doc
   "Returns a zipper for xml elements (as from xml/parse),\ngiven a root element",
   :var-type "function",
   :line 53,
   :added "1.0",
   :namespace "clojure.zip",
   :arglists ([root]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/xml-zip",
   :source-url
   "https://github.com/clojure/clojure/blob/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj#L53",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj",
   :file "src/clj/clojure/zip.clj"}
  {:name "zipper",
   :doc
   "Creates a new zipper structure. \n\nbranch? is a fn that, given a node, returns true if it can have\nchildren, even if it currently doesn't.\n\nchildren is a fn that, given a branch node, returns a seq of its\nchildren.\n\nmake-node is a fn that, given an existing node and a seq of\nchildren, returns a new branch node with the supplied children.\nroot is the root node.",
   :var-type "function",
   :line 18,
   :added "1.0",
   :namespace "clojure.zip",
   :arglists ([branch? children make-node root]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/zipper",
   :source-url
   "https://github.com/clojure/clojure/blob/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj#L18",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/13b2a2014fda5d4d1c5a8a9d52129813d11bf95d/src/clj/clojure/zip.clj",
   :file "src/clj/clojure/zip.clj"}
  {:name "CollReduce",
   :doc
   "Protocol for collection types that can implement reduce faster than\nfirst/next recursion. Called by clojure.core/reduce. Baseline\nimplementation defined in terms of Iterable.",
   :var-type "protocol",
   :line 13,
   :namespace "clojure.core.protocols",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.protocols/CollReduce",
   :source-url
   "https://github.com/clojure/clojure/blob/619b576022cdd5fae899a8418bc568ab1dac3472/src/clj/clojure/core/protocols.clj#L13",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/619b576022cdd5fae899a8418bc568ab1dac3472/src/clj/clojure/core/protocols.clj",
   :file "src/clj/clojure/core/protocols.clj"}
  {:name "Datafiable",
   :doc nil,
   :var-type "protocol",
   :line 181,
   :namespace "clojure.core.protocols",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.protocols/Datafiable",
   :source-url
   "https://github.com/clojure/clojure/blob/619b576022cdd5fae899a8418bc568ab1dac3472/src/clj/clojure/core/protocols.clj#L181",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/619b576022cdd5fae899a8418bc568ab1dac3472/src/clj/clojure/core/protocols.clj",
   :file "src/clj/clojure/core/protocols.clj"}
  {:name "IKVReduce",
   :doc
   "Protocol for concrete associative types that can reduce themselves\nvia a function of key and val faster than first/next recursion over map\nentries. Called by clojure.core/reduce-kv, and has same\nsemantics (just different arg order).",
   :var-type "protocol",
   :line 174,
   :namespace "clojure.core.protocols",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.protocols/IKVReduce",
   :source-url
   "https://github.com/clojure/clojure/blob/619b576022cdd5fae899a8418bc568ab1dac3472/src/clj/clojure/core/protocols.clj#L174",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/619b576022cdd5fae899a8418bc568ab1dac3472/src/clj/clojure/core/protocols.clj",
   :file "src/clj/clojure/core/protocols.clj"}
  {:name "InternalReduce",
   :doc
   "Protocol for concrete seq types that can reduce themselves\nfaster than first/next recursion. Called by clojure.core/reduce.",
   :var-type "protocol",
   :line 19,
   :namespace "clojure.core.protocols",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.protocols/InternalReduce",
   :source-url
   "https://github.com/clojure/clojure/blob/619b576022cdd5fae899a8418bc568ab1dac3472/src/clj/clojure/core/protocols.clj#L19",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/619b576022cdd5fae899a8418bc568ab1dac3472/src/clj/clojure/core/protocols.clj",
   :file "src/clj/clojure/core/protocols.clj"}
  {:name "Navigable",
   :doc nil,
   :var-type "protocol",
   :line 193,
   :namespace "clojure.core.protocols",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.protocols/Navigable",
   :source-url
   "https://github.com/clojure/clojure/blob/619b576022cdd5fae899a8418bc568ab1dac3472/src/clj/clojure/core/protocols.clj#L193",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/619b576022cdd5fae899a8418bc568ab1dac3472/src/clj/clojure/core/protocols.clj",
   :file "src/clj/clojure/core/protocols.clj"}
  {:name "coll-reduce",
   :doc nil,
   :var-type "function",
   :namespace "clojure.core.protocols",
   :arglists ([coll f] [coll f val]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.protocols/coll-reduce",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "datafy",
   :doc "return a representation of o as data (default identity)",
   :var-type "function",
   :namespace "clojure.core.protocols",
   :arglists ([o]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.protocols/datafy",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "kv-reduce",
   :doc nil,
   :var-type "function",
   :namespace "clojure.core.protocols",
   :arglists ([amap f init]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.protocols/kv-reduce",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "internal-reduce",
   :doc nil,
   :var-type "function",
   :namespace "clojure.core.protocols",
   :arglists ([seq f start]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.protocols/internal-reduce",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "nav",
   :doc
   "return (possibly transformed) v in the context of coll and k (a key/index or nil),\ndefaults to returning v.",
   :var-type "function",
   :namespace "clojure.core.protocols",
   :arglists ([coll k v]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.protocols/nav",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "->Cat",
   :doc
   "Positional factory function for class clojure.core.reducers.Cat.",
   :var-type "function",
   :line 230,
   :namespace "clojure.core.reducers",
   :arglists ([cnt left right]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.reducers/->Cat",
   :source-url
   "https://github.com/clojure/clojure/blob/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj#L230",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj",
   :file "src/clj/clojure/core/reducers.clj"}
  {:name "append!",
   :doc ".adds x to acc and returns acc",
   :var-type "function",
   :line 275,
   :added "1.5",
   :namespace "clojure.core.reducers",
   :arglists ([acc x]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.reducers/append!",
   :source-url
   "https://github.com/clojure/clojure/blob/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj#L275",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj",
   :file "src/clj/clojure/core/reducers.clj"}
  {:name "cat",
   :doc
   "A high-performance combining fn that yields the catenation of the\nreduced values. The result is reducible, foldable, seqable and\ncounted, providing the identity collections are reducible, seqable\nand counted. The single argument version will build a combining fn\nwith the supplied identity constructor. Tests for identity\nwith (zero? (count x)). See also foldcat.",
   :var-type "function",
   :line 255,
   :added "1.5",
   :namespace "clojure.core.reducers",
   :arglists ([] [ctor] [left right]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.reducers/cat",
   :source-url
   "https://github.com/clojure/clojure/blob/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj#L255",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj",
   :file "src/clj/clojure/core/reducers.clj"}
  {:name "drop",
   :doc "Elides the first n values from the reduction of coll.",
   :var-type "function",
   :line 215,
   :added "1.5",
   :namespace "clojure.core.reducers",
   :arglists ([n] [n coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.reducers/drop",
   :source-url
   "https://github.com/clojure/clojure/blob/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj#L215",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj",
   :file "src/clj/clojure/core/reducers.clj"}
  {:name "filter",
   :doc
   "Retains values in the reduction of coll for which (pred val)\nreturns logical true. Foldable.",
   :var-type "function",
   :line 154,
   :added "1.5",
   :namespace "clojure.core.reducers",
   :arglists ([pred] [pred coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.reducers/filter",
   :source-url
   "https://github.com/clojure/clojure/blob/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj#L154",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj",
   :file "src/clj/clojure/core/reducers.clj"}
  {:name "flatten",
   :doc
   "Takes any nested combination of sequential things (lists, vectors,\netc.) and returns their contents as a single, flat foldable\ncollection.",
   :var-type "function",
   :line 174,
   :added "1.5",
   :namespace "clojure.core.reducers",
   :arglists ([] [coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.reducers/flatten",
   :source-url
   "https://github.com/clojure/clojure/blob/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj#L174",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj",
   :file "src/clj/clojure/core/reducers.clj"}
  {:name "fold",
   :doc
   "Reduces a collection using a (potentially parallel) reduce-combine\nstrategy. The collection is partitioned into groups of approximately\nn (default 512), each of which is reduced with reducef (with a seed\nvalue obtained by calling (combinef) with no arguments). The results\nof these reductions are then reduced with combinef (default\nreducef). combinef must be associative, and, when called with no\narguments, (combinef) must produce its identity element. These\noperations may be performed in parallel, but the results will\npreserve order.",
   :var-type "function",
   :line 51,
   :added "1.5",
   :namespace "clojure.core.reducers",
   :arglists
   ([reducef coll] [combinef reducef coll] [n combinef reducef coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.reducers/fold",
   :source-url
   "https://github.com/clojure/clojure/blob/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj#L51",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj",
   :file "src/clj/clojure/core/reducers.clj"}
  {:name "foldcat",
   :doc "Equivalent to (fold cat append! coll)",
   :var-type "function",
   :line 281,
   :added "1.5",
   :namespace "clojure.core.reducers",
   :arglists ([coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.reducers/foldcat",
   :source-url
   "https://github.com/clojure/clojure/blob/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj#L281",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj",
   :file "src/clj/clojure/core/reducers.clj"}
  {:name "folder",
   :doc
   "Given a foldable collection, and a transformation function xf,\nreturns a foldable collection, where any supplied reducing\nfn will be transformed by xf. xf is a function of reducing fn to\nreducing fn.",
   :var-type "function",
   :line 81,
   :added "1.5",
   :namespace "clojure.core.reducers",
   :arglists ([coll xf]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.reducers/folder",
   :source-url
   "https://github.com/clojure/clojure/blob/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj#L81",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj",
   :file "src/clj/clojure/core/reducers.clj"}
  {:name "map",
   :doc "Applies f to every value in the reduction of coll. Foldable.",
   :var-type "function",
   :line 128,
   :added "1.5",
   :namespace "clojure.core.reducers",
   :arglists ([f] [f coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.reducers/map",
   :source-url
   "https://github.com/clojure/clojure/blob/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj#L128",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj",
   :file "src/clj/clojure/core/reducers.clj"}
  {:name "mapcat",
   :doc
   "Applies f to every value in the reduction of coll, concatenating the result\ncolls of (f val). Foldable.",
   :var-type "function",
   :line 138,
   :added "1.5",
   :namespace "clojure.core.reducers",
   :arglists ([f] [f coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.reducers/mapcat",
   :source-url
   "https://github.com/clojure/clojure/blob/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj#L138",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj",
   :file "src/clj/clojure/core/reducers.clj"}
  {:name "monoid",
   :doc
   "Builds a combining fn out of the supplied operator and identity\nconstructor. op must be associative and ctor called with no args\nmust return an identity value for it.",
   :var-type "function",
   :line 287,
   :added "1.5",
   :namespace "clojure.core.reducers",
   :arglists ([op ctor]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.reducers/monoid",
   :source-url
   "https://github.com/clojure/clojure/blob/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj#L287",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj",
   :file "src/clj/clojure/core/reducers.clj"}
  {:name "reduce",
   :doc
   "Like core/reduce except:\nWhen init is not provided, (f) is used.\nMaps are reduced with reduce-kv",
   :var-type "function",
   :line 38,
   :namespace "clojure.core.reducers",
   :arglists ([f coll] [f init coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.reducers/reduce",
   :source-url
   "https://github.com/clojure/clojure/blob/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj#L38",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj",
   :file "src/clj/clojure/core/reducers.clj"}
  {:name "reducer",
   :doc
   "Given a reducible collection, and a transformation function xf,\nreturns a reducible collection, where any supplied reducing\nfn will be transformed by xf. xf is a function of reducing fn to\nreducing fn.",
   :var-type "function",
   :line 67,
   :added "1.5",
   :namespace "clojure.core.reducers",
   :arglists ([coll xf]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.reducers/reducer",
   :source-url
   "https://github.com/clojure/clojure/blob/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj#L67",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj",
   :file "src/clj/clojure/core/reducers.clj"}
  {:name "remove",
   :doc
   "Removes values in the reduction of coll for which (pred val)\nreturns logical true. Foldable.",
   :var-type "function",
   :line 167,
   :added "1.5",
   :namespace "clojure.core.reducers",
   :arglists ([pred] [pred coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.reducers/remove",
   :source-url
   "https://github.com/clojure/clojure/blob/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj#L167",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj",
   :file "src/clj/clojure/core/reducers.clj"}
  {:name "take",
   :doc "Ends the reduction of coll after consuming n values.",
   :var-type "function",
   :line 201,
   :added "1.5",
   :namespace "clojure.core.reducers",
   :arglists ([n] [n coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.reducers/take",
   :source-url
   "https://github.com/clojure/clojure/blob/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj#L201",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj",
   :file "src/clj/clojure/core/reducers.clj"}
  {:name "take-while",
   :doc
   "Ends the reduction of coll when (pred val) returns logical false.",
   :var-type "function",
   :line 189,
   :added "1.5",
   :namespace "clojure.core.reducers",
   :arglists ([pred] [pred coll]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.reducers/take-while",
   :source-url
   "https://github.com/clojure/clojure/blob/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj#L189",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj",
   :file "src/clj/clojure/core/reducers.clj"}
  {:name "Cat",
   :var-type "type",
   :namespace "clojure.core.reducers",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.reducers/Cat",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:name "io-prepl",
   :doc
   "prepl bound to *in* and *out*, suitable for use with e.g. server/repl (socket-repl).\n:ret and :tap vals will be processed by valf, a fn of one argument\nor a symbol naming same (default pr-str)\n\nAlpha, subject to change.",
   :var-type "function",
   :line 275,
   :added "1.10",
   :namespace "clojure.core.server",
   :arglists ([& {:keys [valf], :or {valf pr-str}}]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.server/io-prepl",
   :source-url
   "https://github.com/clojure/clojure/blob/3eab1a5a5b4cfed86c1d0c9c21bd1892e1fb44fd/src/clj/clojure/core/server.clj#L275",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3eab1a5a5b4cfed86c1d0c9c21bd1892e1fb44fd/src/clj/clojure/core/server.clj",
   :file "src/clj/clojure/core/server.clj"}
  {:name "prepl",
   :doc
   "a REPL with structured output (for programs)\nreads forms to eval from in-reader (a LineNumberingPushbackReader)\nClosing the input or passing the form :repl/quit will cause it to return\n\nCalls out-fn with data, one of:\n{:tag :ret\n :val val ;;eval result, or Throwable->map data if exception thrown\n :ns ns-name-string\n :ms long ;;eval time in milliseconds\n :form string ;;iff successfully read\n :exception true ;;iff exception thrown\n}\n{:tag :out\n :val string} ;chars from during-eval *out*\n{:tag :err\n :val string} ;chars from during-eval *err*\n{:tag :tap\n :val val} ;values from tap>\n\nYou might get more than one :out or :err per eval, but exactly one :ret\ntap output can happen at any time (i.e. between evals)\nIf during eval an attempt is made to read *in* it will read from in-reader unless :stdin is supplied\n\nAlpha, subject to change.",
   :var-type "function",
   :line 194,
   :added "1.10",
   :namespace "clojure.core.server",
   :arglists ([in-reader out-fn & {:keys [stdin]}]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.server/prepl",
   :source-url
   "https://github.com/clojure/clojure/blob/3eab1a5a5b4cfed86c1d0c9c21bd1892e1fb44fd/src/clj/clojure/core/server.clj#L194",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3eab1a5a5b4cfed86c1d0c9c21bd1892e1fb44fd/src/clj/clojure/core/server.clj",
   :file "src/clj/clojure/core/server.clj"}
  {:name "remote-prepl",
   :doc
   "Implements a prepl on in-reader and out-fn by forwarding to a\nremote [io-]prepl over a socket.  Messages will be read by readf, a\nfn of a LineNumberingPushbackReader and EOF value or a symbol naming\nsame (default #(read %1 false %2)),\n:ret and :tap vals will be processed by valf, a fn of one argument\nor a symbol naming same (default read-string). If that function\nthrows, :val will be unprocessed.\n\nAlpha, subject to change.",
   :var-type "function",
   :line 298,
   :added "1.10",
   :namespace "clojure.core.server",
   :arglists
   ([host
     port
     in-reader
     out-fn
     &
     {:keys [valf readf],
      :or
      {valf read-string,
       readf
       (fn*
        [p1__6883# p2__6884#]
        (read p1__6883# false p2__6884#))}}]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.server/remote-prepl",
   :source-url
   "https://github.com/clojure/clojure/blob/3eab1a5a5b4cfed86c1d0c9c21bd1892e1fb44fd/src/clj/clojure/core/server.clj#L298",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3eab1a5a5b4cfed86c1d0c9c21bd1892e1fb44fd/src/clj/clojure/core/server.clj",
   :file "src/clj/clojure/core/server.clj"}
  {:name "repl",
   :doc "REPL with predefined hooks for attachable socket server.",
   :var-type "function",
   :line 183,
   :namespace "clojure.core.server",
   :arglists ([]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.server/repl",
   :source-url
   "https://github.com/clojure/clojure/blob/3eab1a5a5b4cfed86c1d0c9c21bd1892e1fb44fd/src/clj/clojure/core/server.clj#L183",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3eab1a5a5b4cfed86c1d0c9c21bd1892e1fb44fd/src/clj/clojure/core/server.clj",
   :file "src/clj/clojure/core/server.clj"}
  {:name "repl-init",
   :doc
   "Initialize repl in user namespace and make standard repl requires.",
   :var-type "function",
   :line 166,
   :namespace "clojure.core.server",
   :arglists ([]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.server/repl-init",
   :source-url
   "https://github.com/clojure/clojure/blob/3eab1a5a5b4cfed86c1d0c9c21bd1892e1fb44fd/src/clj/clojure/core/server.clj#L166",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3eab1a5a5b4cfed86c1d0c9c21bd1892e1fb44fd/src/clj/clojure/core/server.clj",
   :file "src/clj/clojure/core/server.clj"}
  {:name "repl-read",
   :doc "Enhanced :read hook for repl supporting :repl/quit.",
   :var-type "function",
   :line 172,
   :namespace "clojure.core.server",
   :arglists ([request-prompt request-exit]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.server/repl-read",
   :source-url
   "https://github.com/clojure/clojure/blob/3eab1a5a5b4cfed86c1d0c9c21bd1892e1fb44fd/src/clj/clojure/core/server.clj#L172",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3eab1a5a5b4cfed86c1d0c9c21bd1892e1fb44fd/src/clj/clojure/core/server.clj",
   :file "src/clj/clojure/core/server.clj"}
  {:name "start-server",
   :doc
   "Start a socket server given the specified opts:\n :address Host or address, string, defaults to loopback address\n :port Port, integer, required\n :name Name, required\n :accept Namespaced symbol of the accept function to invoke, required\n :args Vector of args to pass to accept function\n :bind-err Bind *err* to socket out stream?, defaults to true\n :server-daemon Is server thread a daemon?, defaults to true\n :client-daemon Are client threads daemons?, defaults to true\nReturns server socket.",
   :var-type "function",
   :line 85,
   :namespace "clojure.core.server",
   :arglists ([opts]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.server/start-server",
   :source-url
   "https://github.com/clojure/clojure/blob/3eab1a5a5b4cfed86c1d0c9c21bd1892e1fb44fd/src/clj/clojure/core/server.clj#L85",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3eab1a5a5b4cfed86c1d0c9c21bd1892e1fb44fd/src/clj/clojure/core/server.clj",
   :file "src/clj/clojure/core/server.clj"}
  {:name "start-servers",
   :doc "Start all servers specified in the system properties.",
   :var-type "function",
   :line 160,
   :namespace "clojure.core.server",
   :arglists ([system-props]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.server/start-servers",
   :source-url
   "https://github.com/clojure/clojure/blob/3eab1a5a5b4cfed86c1d0c9c21bd1892e1fb44fd/src/clj/clojure/core/server.clj#L160",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3eab1a5a5b4cfed86c1d0c9c21bd1892e1fb44fd/src/clj/clojure/core/server.clj",
   :file "src/clj/clojure/core/server.clj"}
  {:name "stop-server",
   :doc
   "Stop server with name or use the server-name from *session* if none supplied.\nReturns true if server stopped successfully, nil if not found, or throws if\nthere is an error closing the socket.",
   :var-type "function",
   :line 126,
   :namespace "clojure.core.server",
   :arglists ([] [name]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.server/stop-server",
   :source-url
   "https://github.com/clojure/clojure/blob/3eab1a5a5b4cfed86c1d0c9c21bd1892e1fb44fd/src/clj/clojure/core/server.clj#L126",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3eab1a5a5b4cfed86c1d0c9c21bd1892e1fb44fd/src/clj/clojure/core/server.clj",
   :file "src/clj/clojure/core/server.clj"}
  {:name "stop-servers",
   :doc "Stop all servers ignores all errors, and returns nil.",
   :var-type "function",
   :line 140,
   :namespace "clojure.core.server",
   :arglists ([]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.server/stop-servers",
   :source-url
   "https://github.com/clojure/clojure/blob/3eab1a5a5b4cfed86c1d0c9c21bd1892e1fb44fd/src/clj/clojure/core/server.clj#L140",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/3eab1a5a5b4cfed86c1d0c9c21bd1892e1fb44fd/src/clj/clojure/core/server.clj",
   :file "src/clj/clojure/core/server.clj"}
  {:name "even-number-of-forms?",
   :doc
   "Returns true if there are an even number of forms in a binding vector",
   :var-type "function",
   :line 59,
   :namespace "clojure.core.specs.alpha",
   :arglists ([forms]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.specs.alpha/even-number-of-forms?",
   :source-url nil,
   :raw-source-url nil,
   :file "src/clj/clojure/core/specs/alpha.clj"}
  {:keyword :clojure.core.specs.alpha/as,
   :spec (and simple-symbol? (not= '& %)),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/as"}
  {:keyword :clojure.core.specs.alpha/as-alias,
   :spec simple-symbol?,
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/as-alias"}
  {:keyword :clojure.core.specs.alpha/binding,
   :spec
   (cat :form :clojure.core.specs.alpha/binding-form :init-expr any?),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/binding"}
  {:keyword :clojure.core.specs.alpha/binding-form,
   :spec
   (or
    :local-symbol
    :clojure.core.specs.alpha/local-name
    :seq-destructure
    :clojure.core.specs.alpha/seq-binding-form
    :map-destructure
    :clojure.core.specs.alpha/map-binding-form),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/binding-form"}
  {:keyword :clojure.core.specs.alpha/bindings,
   :spec
   (and
    vector?
    even-number-of-forms?
    (* :clojure.core.specs.alpha/binding)),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/bindings"}
  {:keyword :clojure.core.specs.alpha/class-ident,
   :spec (or :class simple-symbol? :class-name string?),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/class-ident"}
  {:keyword :clojure.core.specs.alpha/constructors,
   :spec
   (map-of
    :clojure.core.specs.alpha/signature
    :clojure.core.specs.alpha/signature),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/constructors"}
  {:keyword :clojure.core.specs.alpha/defaults,
   :spec (and simple-symbol? (not= '& %)),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/defaults"}
  {:keyword :clojure.core.specs.alpha/defn-args,
   :spec
   (cat
    :fn-name
    simple-symbol?
    :docstring
    (? string?)
    :meta
    (? map?)
    :fn-tail
    (alt
     :arity-1
     :clojure.core.specs.alpha/params+body
     :arity-n
     (cat
      :bodies
      (+ (spec :clojure.core.specs.alpha/params+body))
      :attr-map
      (? map?)))),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/defn-args"}
  {:keyword :clojure.core.specs.alpha/exclude,
   :spec (coll-of simple-symbol?),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/exclude"}
  {:keyword :clojure.core.specs.alpha/expose,
   :spec
   (keys
    :opt-un
    [:clojure.core.specs.alpha/get :clojure.core.specs.alpha/set]),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/expose"}
  {:keyword :clojure.core.specs.alpha/exposes,
   :spec (map-of simple-symbol? :clojure.core.specs.alpha/expose),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/exposes"}
  {:keyword :clojure.core.specs.alpha/extends,
   :spec simple-symbol?,
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/extends"}
  {:keyword :clojure.core.specs.alpha/factory,
   :spec simple-symbol?,
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/factory"}
  {:keyword :clojure.core.specs.alpha/filters,
   :spec
   (&
    (* (cat :clojure.spec.alpha/k keyword? :clojure.spec.alpha/v any?))
    :clojure.spec.alpha/kvs->map
    mspec__2531__auto__),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/filters"}
  {:keyword :clojure.core.specs.alpha/get,
   :spec simple-symbol?,
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/get"}
  {:keyword :clojure.core.specs.alpha/impl-ns,
   :spec simple-symbol?,
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/impl-ns"}
  {:keyword :clojure.core.specs.alpha/implements,
   :spec (coll-of simple-symbol? :kind vector?),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/implements"}
  {:keyword :clojure.core.specs.alpha/import-list,
   :spec
   (*
    (alt
     :class
     simple-symbol?
     :package-list
     :clojure.core.specs.alpha/package-list)),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/import-list"}
  {:keyword :clojure.core.specs.alpha/init,
   :spec symbol?,
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/init"}
  {:keyword :clojure.core.specs.alpha/keys,
   :spec
   (and
    vector?
    (cat
     :bindings
     (* ident?)
     :nonbinding
     (? :clojure.core.specs.alpha/non-binding-elements))),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/keys"}
  {:keyword :clojure.core.specs.alpha/keys!,
   :spec
   (and
    vector?
    (cat
     :bindings
     (* ident?)
     :nonbinding
     (? :clojure.core.specs.alpha/non-binding-elements))),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/keys!"}
  {:keyword :clojure.core.specs.alpha/keys-form,
   :spec
   (and
    vector?
    (cat
     :bindings
     (* ident?)
     :nonbinding
     (? :clojure.core.specs.alpha/non-binding-elements))),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/keys-form"}
  {:keyword :clojure.core.specs.alpha/libspec,
   :spec
   (alt
    :lib
    simple-symbol?
    :lib+opts
    (spec
     (cat
      :lib
      simple-symbol?
      :options
      (keys*
       :opt-un
       [:clojure.core.specs.alpha/as
        :clojure.core.specs.alpha/refer
        :clojure.core.specs.alpha/as-alias])))),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/libspec"}
  {:keyword :clojure.core.specs.alpha/load-impl-ns,
   :spec boolean?,
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/load-impl-ns"}
  {:keyword :clojure.core.specs.alpha/local-name,
   :spec (and simple-symbol? (not= '& %)),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/local-name"}
  {:keyword :clojure.core.specs.alpha/main,
   :spec boolean?,
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/main"}
  {:keyword :clojure.core.specs.alpha/map-binding,
   :spec (tuple :clojure.core.specs.alpha/binding-form any?),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/map-binding"}
  {:keyword :clojure.core.specs.alpha/map-binding-form,
   :spec
   (merge
    :clojure.core.specs.alpha/map-bindings
    :clojure.core.specs.alpha/map-special-binding),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/map-binding-form"}
  {:keyword :clojure.core.specs.alpha/map-bindings,
   :spec
   (every
    (or
     :map-binding
     :clojure.core.specs.alpha/map-binding
     :qualified-keys-or-syms
     :clojure.core.specs.alpha/ns-keys
     :special-binding
     (tuple
      #{:syms!
        :as
        :or
        :syms
        :defaults
        :keys
        :strs!
        :keys!
        :strs
        :select}
      any?))
    :kind
    map?),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/map-bindings"}
  {:keyword :clojure.core.specs.alpha/map-special-binding,
   :spec
   (keys
    :opt-un
    [:clojure.core.specs.alpha/as
     :clojure.core.specs.alpha/or
     :clojure.core.specs.alpha/keys
     :clojure.core.specs.alpha/syms
     :clojure.core.specs.alpha/strs
     :clojure.core.specs.alpha/keys!
     :clojure.core.specs.alpha/syms!
     :clojure.core.specs.alpha/strs!
     :clojure.core.specs.alpha/select
     :clojure.core.specs.alpha/defaults]),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/map-special-binding"}
  {:keyword :clojure.core.specs.alpha/method,
   :spec
   (and
    vector?
    (cat
     :method-name
     simple-symbol?
     :param-types
     :clojure.core.specs.alpha/signature
     :return-type
     :clojure.core.specs.alpha/class-ident)),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/method"}
  {:keyword :clojure.core.specs.alpha/methods,
   :spec (coll-of :clojure.core.specs.alpha/method :kind vector?),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/methods"}
  {:keyword :clojure.core.specs.alpha/name,
   :spec simple-symbol?,
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/name"}
  {:keyword :clojure.core.specs.alpha/non-binding-elements,
   :spec
   (cat
    :ampersand
    #{'&}
    :clojure.core.specs.alpha/non-binding-element
    (+ any?)),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/non-binding-elements"}
  {:keyword :clojure.core.specs.alpha/ns-clauses,
   :spec
   (*
    (alt
     :refer-clojure
     :clojure.core.specs.alpha/ns-refer-clojure
     :require
     :clojure.core.specs.alpha/ns-require
     :import
     :clojure.core.specs.alpha/ns-import
     :use
     :clojure.core.specs.alpha/ns-use
     :refer
     :clojure.core.specs.alpha/ns-refer
     :load
     :clojure.core.specs.alpha/ns-load
     :gen-class
     :clojure.core.specs.alpha/ns-gen-class)),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/ns-clauses"}
  {:keyword :clojure.core.specs.alpha/ns-form,
   :spec
   (cat
    :ns-name
    simple-symbol?
    :docstring
    (? string?)
    :attr-map
    (? map?)
    :ns-clauses
    :clojure.core.specs.alpha/ns-clauses),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/ns-form"}
  {:keyword :clojure.core.specs.alpha/ns-gen-class,
   :spec
   (cat
    :clause
    #{:gen-class}
    :options
    (keys*
     :opt-un
     [:clojure.core.specs.alpha/name
      :clojure.core.specs.alpha/extends
      :clojure.core.specs.alpha/implements
      :clojure.core.specs.alpha/init
      :clojure.core.specs.alpha/constructors
      :clojure.core.specs.alpha/post-init
      :clojure.core.specs.alpha/methods
      :clojure.core.specs.alpha/main
      :clojure.core.specs.alpha/factory
      :clojure.core.specs.alpha/state
      :clojure.core.specs.alpha/exposes
      :clojure.core.specs.alpha/prefix
      :clojure.core.specs.alpha/impl-ns
      :clojure.core.specs.alpha/load-impl-ns])),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/ns-gen-class"}
  {:keyword :clojure.core.specs.alpha/ns-import,
   :spec
   (cat
    :clause
    #{:import}
    :classes
    :clojure.core.specs.alpha/import-list),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/ns-import"}
  {:keyword :clojure.core.specs.alpha/ns-keys,
   :spec
   (tuple
    (and
     qualified-keyword?
     (fn*
      [p1__4868#]
      (-> p1__4868# name #{"syms" "keys" "syms!" "keys!"})))
    :clojure.core.specs.alpha/syms-form),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/ns-keys"}
  {:keyword :clojure.core.specs.alpha/ns-load,
   :spec (cat :clause #{:load} :libs (* string?)),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/ns-load"}
  {:keyword :clojure.core.specs.alpha/ns-refer,
   :spec
   (cat
    :clause
    #{:refer}
    :lib
    simple-symbol?
    :refer-filters
    :clojure.core.specs.alpha/filters),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/ns-refer"}
  {:keyword :clojure.core.specs.alpha/ns-refer-clojure,
   :spec
   (cat
    :clause
    #{:refer-clojure}
    :refer-filters
    :clojure.core.specs.alpha/filters),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/ns-refer-clojure"}
  {:keyword :clojure.core.specs.alpha/ns-require,
   :spec
   (cat
    :clause
    #{:require}
    :body
    (+
     (alt
      :libspec
      :clojure.core.specs.alpha/libspec
      :prefix-list
      :clojure.core.specs.alpha/prefix-list
      :flag
      #{:verbose :reload :reload-all}))),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/ns-require"}
  {:keyword :clojure.core.specs.alpha/ns-use,
   :spec
   (cat
    :clause
    #{:use}
    :libs
    (+
     (alt
      :libspec
      :clojure.core.specs.alpha/use-libspec
      :prefix-list
      :clojure.core.specs.alpha/use-prefix-list
      :flag
      #{:verbose :reload :reload-all}))),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/ns-use"}
  {:keyword :clojure.core.specs.alpha/only,
   :spec (coll-of simple-symbol?),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/only"}
  {:keyword :clojure.core.specs.alpha/or,
   :spec map?,
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/or"}
  {:keyword :clojure.core.specs.alpha/package-list,
   :spec (cat :package simple-symbol? :classes (+ simple-symbol?)),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/package-list"}
  {:keyword :clojure.core.specs.alpha/param-list,
   :spec
   (and
    vector?
    (cat
     :params
     (* :clojure.core.specs.alpha/binding-form)
     :var-params
     (?
      (cat
       :ampersand
       #{'&}
       :var-form
       :clojure.core.specs.alpha/binding-form)))),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/param-list"}
  {:keyword :clojure.core.specs.alpha/params+body,
   :spec
   (cat
    :params
    :clojure.core.specs.alpha/param-list
    :body
    (alt
     :prepost+body
     (cat :prepost map? :body (+ any?))
     :body
     (* any?))),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/params+body"}
  {:keyword :clojure.core.specs.alpha/post-init,
   :spec symbol?,
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/post-init"}
  {:keyword :clojure.core.specs.alpha/prefix,
   :spec string?,
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/prefix"}
  {:keyword :clojure.core.specs.alpha/prefix-list,
   :spec
   (cat
    :prefix
    simple-symbol?
    :libspecs
    (+ :clojure.core.specs.alpha/libspec)),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/prefix-list"}
  {:keyword :clojure.core.specs.alpha/quotable-import-list,
   :spec
   (*
    (alt
     :class
     (quotable simple-symbol?)
     :package-list
     (quotable :clojure.core.specs.alpha/package-list))),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/quotable-import-list"}
  {:keyword :clojure.core.specs.alpha/refer,
   :spec (or :all #{:all} :syms (coll-of simple-symbol?)),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/refer"}
  {:keyword :clojure.core.specs.alpha/rename,
   :spec (map-of simple-symbol? simple-symbol?),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/rename"}
  {:keyword :clojure.core.specs.alpha/select,
   :spec (and simple-symbol? (not= '& %)),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/select"}
  {:keyword :clojure.core.specs.alpha/seq-binding-form,
   :spec
   (and
    vector?
    (cat
     :forms
     (* :clojure.core.specs.alpha/binding-form)
     :rest-forms
     (?
      (cat
       :ampersand
       #{'&}
       :form
       :clojure.core.specs.alpha/binding-form))
     :as-form
     (?
      (cat :as #{:as} :as-sym :clojure.core.specs.alpha/local-name)))),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/seq-binding-form"}
  {:keyword :clojure.core.specs.alpha/set,
   :spec simple-symbol?,
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/set"}
  {:keyword :clojure.core.specs.alpha/signature,
   :spec (coll-of :clojure.core.specs.alpha/class-ident :kind vector?),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/signature"}
  {:keyword :clojure.core.specs.alpha/state,
   :spec simple-symbol?,
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/state"}
  {:keyword :clojure.core.specs.alpha/strs,
   :spec
   (and
    vector?
    (cat
     :bindings
     (* simple-symbol?)
     :nonbinding
     (? :clojure.core.specs.alpha/non-binding-elements))),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/strs"}
  {:keyword :clojure.core.specs.alpha/strs!,
   :spec
   (and
    vector?
    (cat
     :bindings
     (* simple-symbol?)
     :nonbinding
     (? :clojure.core.specs.alpha/non-binding-elements))),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/strs!"}
  {:keyword :clojure.core.specs.alpha/strs-form,
   :spec
   (and
    vector?
    (cat
     :bindings
     (* simple-symbol?)
     :nonbinding
     (? :clojure.core.specs.alpha/non-binding-elements))),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/strs-form"}
  {:keyword :clojure.core.specs.alpha/syms,
   :spec
   (and
    vector?
    (cat
     :bindings
     (* symbol?)
     :nonbinding
     (? :clojure.core.specs.alpha/non-binding-elements))),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/syms"}
  {:keyword :clojure.core.specs.alpha/syms!,
   :spec
   (and
    vector?
    (cat
     :bindings
     (* symbol?)
     :nonbinding
     (? :clojure.core.specs.alpha/non-binding-elements))),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/syms!"}
  {:keyword :clojure.core.specs.alpha/syms-form,
   :spec
   (and
    vector?
    (cat
     :bindings
     (* symbol?)
     :nonbinding
     (? :clojure.core.specs.alpha/non-binding-elements))),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/syms-form"}
  {:keyword :clojure.core.specs.alpha/use-libspec,
   :spec
   (alt
    :lib
    simple-symbol?
    :lib+opts
    (spec
     (cat
      :lib
      simple-symbol?
      :options
      (keys*
       :opt-un
       [:clojure.core.specs.alpha/as
        :clojure.core.specs.alpha/refer
        :clojure.core.specs.alpha/exclude
        :clojure.core.specs.alpha/only
        :clojure.core.specs.alpha/rename])))),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/use-libspec"}
  {:keyword :clojure.core.specs.alpha/use-prefix-list,
   :spec
   (cat
    :prefix
    simple-symbol?
    :libspecs
    (+ :clojure.core.specs.alpha/use-libspec)),
   :var-type "spec",
   :namespace "clojure.core.specs.alpha",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#:clojure.core.specs.alpha/use-prefix-list"}
  {:name "update-basis!",
   :doc "Update the runtime basis by applying f with args",
   :var-type "function",
   :line 48,
   :namespace "clojure.java.basis.impl",
   :arglists ([f & args]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.basis-api.html#clojure.java.basis.impl/update-basis!",
   :source-url
   "https://github.com/clojure/clojure/blob/b7d87dcd729628a2e80038d05023eb7c7786bb11/src/clj/clojure/java/basis/impl.clj#L48",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/b7d87dcd729628a2e80038d05023eb7c7786bb11/src/clj/clojure/java/basis/impl.clj",
   :file "src/clj/clojure/java/basis/impl.clj"}
  {:name "add-lib",
   :doc
   "Given a lib that is not yet on the repl classpath, make it available by\ndownloading the library if necessary and adding it to the classloader.\nLibs already on the classpath are not updated. Requires a valid parent\nDynamicClassLoader.\n\n lib - symbol identifying a library, for Maven: groupId/artifactId\n coord - optional map of location information specific to the procurer,\n         or latest if not supplied\n\nReturns coll of libs loaded, including transitive (or nil if none).\n\nFor info on libs, coords, and versions, see:\n https://clojure.org/reference/deps_and_cli",
   :var-type "function",
   :line 59,
   :added "1.12",
   :namespace "clojure.repl.deps",
   :arglists ([lib coord] [lib]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.repl-api.html#clojure.repl.deps/add-lib",
   :source-url
   "https://github.com/clojure/clojure/blob/be32516234961396581685b485ed8a88f8fe17e6/src/clj/clojure/repl/deps.clj#L59",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/be32516234961396581685b485ed8a88f8fe17e6/src/clj/clojure/repl/deps.clj",
   :file "src/clj/clojure/repl/deps.clj"}
  {:name "add-libs",
   :doc
   "Given lib-coords, a map of lib to coord, will resolve all transitive deps for the libs\ntogether and add them to the repl classpath, unlike separate calls to add-lib.",
   :var-type "function",
   :line 35,
   :added "1.12",
   :namespace "clojure.repl.deps",
   :arglists ([lib-coords]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.repl-api.html#clojure.repl.deps/add-libs",
   :source-url
   "https://github.com/clojure/clojure/blob/be32516234961396581685b485ed8a88f8fe17e6/src/clj/clojure/repl/deps.clj#L35",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/be32516234961396581685b485ed8a88f8fe17e6/src/clj/clojure/repl/deps.clj",
   :file "src/clj/clojure/repl/deps.clj"}
  {:name "sync-deps",
   :doc
   "Calls add-libs with any libs present in deps.edn but not yet present on the classpath.\n\n:aliases - coll of alias keywords to use during the sync",
   :var-type "function",
   :line 85,
   :added "1.12",
   :namespace "clojure.repl.deps",
   :arglists ([& {:as opts}]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.repl-api.html#clojure.repl.deps/sync-deps",
   :source-url
   "https://github.com/clojure/clojure/blob/be32516234961396581685b485ed8a88f8fe17e6/src/clj/clojure/repl/deps.clj#L85",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/be32516234961396581685b485ed8a88f8fe17e6/src/clj/clojure/repl/deps.clj",
   :file "src/clj/clojure/repl/deps.clj"}
  {:name "with-junit-output",
   :doc
   "Execute body with modified test-is reporting functions that write\nJUnit-compatible XML output.",
   :var-type "macro",
   :line 182,
   :added "1.1",
   :namespace "clojure.test.junit",
   :arglists ([& body]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test.junit/with-junit-output",
   :source-url
   "https://github.com/clojure/clojure/blob/d9f3f83182e146525a78cf638f0613487d7e18c6/src/clj/clojure/test/junit.clj#L182",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/d9f3f83182e146525a78cf638f0613487d7e18c6/src/clj/clojure/test/junit.clj",
   :file "src/clj/clojure/test/junit.clj"}
  {:name "print-tap-diagnostic",
   :doc
   "Prints a TAP diagnostic line.  data is a (possibly multi-line)\nstring.",
   :var-type "function",
   :line 51,
   :added "1.1",
   :namespace "clojure.test.tap",
   :arglists ([data]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test.tap/print-tap-diagnostic",
   :source-url
   "https://github.com/clojure/clojure/blob/153a2d0f000ab2f254704e4970968fee6a0329a1/src/clj/clojure/test/tap.clj#L51",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/153a2d0f000ab2f254704e4970968fee6a0329a1/src/clj/clojure/test/tap.clj",
   :file "src/clj/clojure/test/tap.clj"}
  {:name "print-tap-fail",
   :doc
   "Prints a TAP 'not ok' line.  msg is a string, with no line breaks",
   :var-type "function",
   :line 65,
   :added "1.1",
   :namespace "clojure.test.tap",
   :arglists ([msg]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test.tap/print-tap-fail",
   :source-url
   "https://github.com/clojure/clojure/blob/153a2d0f000ab2f254704e4970968fee6a0329a1/src/clj/clojure/test/tap.clj#L65",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/153a2d0f000ab2f254704e4970968fee6a0329a1/src/clj/clojure/test/tap.clj",
   :file "src/clj/clojure/test/tap.clj"}
  {:name "print-tap-pass",
   :doc
   "Prints a TAP 'ok' line.  msg is a string, with no line breaks",
   :var-type "function",
   :line 59,
   :added "1.1",
   :namespace "clojure.test.tap",
   :arglists ([msg]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test.tap/print-tap-pass",
   :source-url
   "https://github.com/clojure/clojure/blob/153a2d0f000ab2f254704e4970968fee6a0329a1/src/clj/clojure/test/tap.clj#L59",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/153a2d0f000ab2f254704e4970968fee6a0329a1/src/clj/clojure/test/tap.clj",
   :file "src/clj/clojure/test/tap.clj"}
  {:name "print-tap-plan",
   :doc
   "Prints a TAP plan line like '1..n'.  n is the number of tests",
   :var-type "function",
   :line 45,
   :added "1.1",
   :namespace "clojure.test.tap",
   :arglists ([n]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test.tap/print-tap-plan",
   :source-url
   "https://github.com/clojure/clojure/blob/153a2d0f000ab2f254704e4970968fee6a0329a1/src/clj/clojure/test/tap.clj#L45",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/153a2d0f000ab2f254704e4970968fee6a0329a1/src/clj/clojure/test/tap.clj",
   :file "src/clj/clojure/test/tap.clj"}
  {:name "with-tap-output",
   :doc
   "Execute body with modified test reporting functions that produce\nTAP output",
   :var-type "macro",
   :line 117,
   :added "1.1",
   :namespace "clojure.test.tap",
   :arglists ([& body]),
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test.tap/with-tap-output",
   :source-url
   "https://github.com/clojure/clojure/blob/153a2d0f000ab2f254704e4970968fee6a0329a1/src/clj/clojure/test/tap.clj#L117",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/153a2d0f000ab2f254704e4970968fee6a0329a1/src/clj/clojure/test/tap.clj",
   :file "src/clj/clojure/test/tap.clj"})}
