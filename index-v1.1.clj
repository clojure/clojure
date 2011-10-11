{:namespaces
 ({:source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url "http://clojure.github.com/clojure/clojure.core-api.html",
   :name "clojure.core",
   :doc "Fundamental library of the Clojure language"}
  {:source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/inspector.clj",
   :wiki-url
   "http://clojure.github.com/clojure/clojure.inspector-api.html",
   :name "clojure.inspector",
   :author "Rich Hickey",
   :doc "Graphical object inspector for Clojure data structures."}
  {:source-url
   "https://github.com/clojure/clojure/blob/2cc710e7aeaab08e0739debe21e2cc6b7020e1b1/src/clj/clojure/main.clj",
   :wiki-url "http://clojure.github.com/clojure/clojure.main-api.html",
   :name "clojure.main",
   :doc nil}
  {:source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/set.clj",
   :wiki-url "http://clojure.github.com/clojure/clojure.set-api.html",
   :name "clojure.set",
   :author "Rich Hickey",
   :doc "Set operations such as union/intersection."}
  {:source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/stacktrace.clj",
   :wiki-url
   "http://clojure.github.com/clojure/clojure.stacktrace-api.html",
   :name "clojure.stacktrace",
   :author "Stuart Sierra",
   :doc "Print stack traces oriented towards Clojure, not Java."}
  {:source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/template.clj",
   :wiki-url
   "http://clojure.github.com/clojure/clojure.template-api.html",
   :name "clojure.template",
   :author "Stuart Sierra",
   :doc
   "Macros that expand to repeated copies of a template expression."}
  {:source-url
   "https://github.com/clojure/clojure/blob/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj",
   :wiki-url "http://clojure.github.com/clojure/clojure.test-api.html",
   :name "clojure.test",
   :author
   "Stuart Sierra, with contributions and suggestions by \n  Chas Emerick, Allen Rohner, and Stuart Halloway",
   :doc
   "A unit testing framework.\n\nASSERTIONS\n\nThe core of the library is the \"is\" macro, which lets you make\nassertions of any arbitrary expression:\n\n(is (= 4 (+ 2 2)))\n(is (instance? Integer 256))\n(is (.startsWith \"abcde\" \"ab\"))\n\nYou can type an \"is\" expression directly at the REPL, which will\nprint a message if it fails.\n\n    user> (is (= 5 (+ 2 2)))\n\n    FAIL in  (:1)\n    expected: (= 5 (+ 2 2))\n      actual: (not (= 5 4))\n    false\n\nThe \"expected:\" line shows you the original expression, and the\n\"actual:\" shows you what actually happened.  In this case, it\nshows that (+ 2 2) returned 4, which is not = to 5.  Finally, the\n\"false\" on the last line is the value returned from the\nexpression.  The \"is\" macro always returns the result of the\ninner expression.\n\nThere are two special assertions for testing exceptions.  The\n\"(is (thrown? c ...))\" form tests if an exception of class c is\nthrown:\n\n(is (thrown? ArithmeticException (/ 1 0))) \n\n\"(is (thrown-with-msg? c re ...))\" does the same thing and also\ntests that the message on the exception matches the regular\nexpression re:\n\n(is (thrown-with-msg? ArithmeticException #\"Divide by zero\"\n                      (/ 1 0)))\n\nDOCUMENTING TESTS\n\n\"is\" takes an optional second argument, a string describing the\nassertion.  This message will be included in the error report.\n\n(is (= 5 (+ 2 2)) \"Crazy arithmetic\")\n\nIn addition, you can document groups of assertions with the\n\"testing\" macro, which takes a string followed by any number of\nassertions.  The string will be included in failure reports.\nCalls to \"testing\" may be nested, and all of the strings will be\njoined together with spaces in the final report, in a style\nsimilar to RSpec <http://rspec.info/>\n\n(testing \"Arithmetic\"\n  (testing \"with positive integers\"\n    (is (= 4 (+ 2 2)))\n    (is (= 7 (+ 3 4))))\n  (testing \"with negative integers\"\n    (is (= -4 (+ -2 -2)))\n    (is (= -1 (+ 3 -4)))))\n\nNote that, unlike RSpec, the \"testing\" macro may only be used\nINSIDE a \"deftest\" or \"with-test\" form (see below).\n\n\nDEFINING TESTS\n\nThere are two ways to define tests.  The \"with-test\" macro takes\na defn or def form as its first argument, followed by any number\nof assertions.  The tests will be stored as metadata on the\ndefinition.\n\n(with-test\n    (defn my-function [x y]\n      (+ x y))\n  (is (= 4 (my-function 2 2)))\n  (is (= 7 (my-function 3 4))))\n\nAs of Clojure SVN rev. 1221, this does not work with defmacro.\nSee http://code.google.com/p/clojure/issues/detail?id=51\n\nThe other way lets you define tests separately from the rest of\nyour code, even in a different namespace:\n\n(deftest addition\n  (is (= 4 (+ 2 2)))\n  (is (= 7 (+ 3 4))))\n\n(deftest subtraction\n  (is (= 1 (- 4 3)))\n  (is (= 3 (- 7 4))))\n\nThis creates functions named \"addition\" and \"subtraction\", which\ncan be called like any other function.  Therefore, tests can be\ngrouped and composed, in a style similar to the test framework in\nPeter Seibel's \"Practical Common Lisp\"\n<http://www.gigamonkeys.com/book/practical-building-a-unit-test-framework.html>\n\n(deftest arithmetic\n  (addition)\n  (subtraction))\n\nThe names of the nested tests will be joined in a list, like\n\"(arithmetic addition)\", in failure reports.  You can use nested\ntests to set up a context shared by several tests.\n\n\nRUNNING TESTS\n\nRun tests with the function \"(run-tests namespaces...)\":\n\n(run-tests 'your.namespace 'some.other.namespace)\n\nIf you don't specify any namespaces, the current namespace is\nused.  To run all tests in all namespaces, use \"(run-all-tests)\".\n\nBy default, these functions will search for all tests defined in\na namespace and run them in an undefined order.  However, if you\nare composing tests, as in the \"arithmetic\" example above, you\nprobably do not want the \"addition\" and \"subtraction\" tests run\nseparately.  In that case, you must define a special function\nnamed \"test-ns-hook\" that runs your tests in the correct order:\n\n(defn test-ns-hook []\n  (arithmetic))\n\n\nOMITTING TESTS FROM PRODUCTION CODE\n\nYou can bind the variable \"*load-tests*\" to false when loading or\ncompiling code in production.  This will prevent any tests from\nbeing created by \"with-test\" or \"deftest\".\n\n\nFIXTURES (new)\n\nFixtures allow you to run code before and after tests, to set up\nthe context in which tests should be run.\n\nA fixture is just a function that calls another function passed as\nan argument.  It looks like this:\n\n(defn my-fixture [f]\n   Perform setup, establish bindings, whatever.\n  (f)  Then call the function we were passed.\n   Tear-down / clean-up code here.\n )\n\nFixtures are attached to namespaces in one of two ways.  \"each\"\nfixtures are run repeatedly, once for each test function created\nwith \"deftest\" or \"with-test\".  \"each\" fixtures are useful for\nestablishing a consistent before/after state for each test, like\nclearing out database tables.\n\n\"each\" fixtures can be attached to the current namespace like this:\n(use-fixtures :each fixture1 fixture2 ...)\nThe fixture1, fixture2 are just functions like the example above.\nThey can also be anonymous functions, like this:\n(use-fixtures :each (fn [f] setup... (f) cleanup...))\n\nThe other kind of fixture, a \"once\" fixture, is only run once,\naround ALL the tests in the namespace.  \"once\" fixtures are useful\nfor tasks that only need to be performed once, like establishing\ndatabase connections, or for time-consuming tasks.\n\nAttach \"once\" fixtures to the current namespace like this:\n(use-fixtures :once fixture1 fixture2 ...)\n\n\nSAVING TEST OUTPUT TO A FILE\n\nAll the test reporting functions write to the var *test-out*.  By\ndefault, this is the same as *out*, but you can rebind it to any\nPrintWriter.  For example, it could be a file opened with\nclojure.contrib.duck-streams/writer.\n\n\nEXTENDING TEST-IS (ADVANCED)\n\nYou can extend the behavior of the \"is\" macro by defining new\nmethods for the \"assert-expr\" multimethod.  These methods are\ncalled during expansion of the \"is\" macro, so they should return\nquoted forms to be evaluated.\n\nYou can plug in your own test-reporting framework by rebinding\nthe \"report\" function: (report event)\n\nThe 'event' argument is a map.  It will always have a :type key,\nwhose value will be a keyword signaling the type of event being\nreported.  Standard events with :type value of :pass, :fail, and\n:error are called when an assertion passes, fails, and throws an\nexception, respectively.  In that case, the event will also have\nthe following keys:\n\n  :expected   The form that was expected to be true\n  :actual     A form representing what actually occurred\n  :message    The string message given as an argument to 'is'\n\nThe \"testing\" strings will be a list in \"*testing-contexts*\", and\nthe vars being tested will be a list in \"*testing-vars*\".\n\nYour \"report\" function should wrap any printing calls in the\n\"with-test-out\" macro, which rebinds *out* to the current value\nof *test-out*.\n\nFor additional event types, see the examples in the code."}
  {:source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/walk.clj",
   :wiki-url "http://clojure.github.com/clojure/clojure.walk-api.html",
   :name "clojure.walk",
   :author "Stuart Sierra",
   :doc
   "This file defines a generic tree walker for Clojure data\nstructures.  It takes any data structure (list, vector, map, set,\nseq), calls a function on every element, and uses the return value\nof the function in place of the original.  This makes it fairly\neasy to write recursive search-and-replace functions, as shown in\nthe examples.\n\nNote: \"walk\" supports all Clojure data structures EXCEPT maps\ncreated with sorted-map-by.  There is no (obvious) way to retrieve\nthe sorting function."}
  {:source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/xml.clj",
   :wiki-url "http://clojure.github.com/clojure/clojure.xml-api.html",
   :name "clojure.xml",
   :author "Rich Hickey",
   :doc "XML reading/writing."}
  {:source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj",
   :wiki-url "http://clojure.github.com/clojure/clojure.zip-api.html",
   :name "clojure.zip",
   :author "Rich Hickey",
   :doc
   "Functional hierarchical zipper, with navigation, editing, \nand enumeration.  See Huet"}
  {:source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/test/junit.clj",
   :wiki-url
   "http://clojure.github.com/clojure/clojure.test.junit-api.html",
   :name "clojure.test.junit",
   :author "Jason Sankey",
   :doc
   "clojure.test extension for JUnit-compatible XML output.\n\nJUnit (http://junit.org/) is the most popular unit-testing library\nfor Java.  As such, tool support for JUnit output formats is\ncommon.  By producing compatible output from tests, this tool\nsupport can be exploited.\n\nTo use, wrap any calls to clojure.test/run-tests in the\nwith-junit-output macro, like this:\n\n  (use 'clojure.test)\n  (use 'clojure.contrib.test.junit)\n\n  (with-junit-output\n    (run-tests 'my.cool.library))\n\nTo write the output to a file, rebind clojure.test/*test-out* to\nyour own PrintWriter (perhaps opened using\nclojure.contrib.duck-streams/writer)."}
  {:source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/test/tap.clj",
   :wiki-url
   "http://clojure.github.com/clojure/clojure.test.tap-api.html",
   :name "clojure.test.tap",
   :author "Stuart Sierra",
   :doc
   "clojure.test extensions for the Test Anything Protocol (TAP)\n\nTAP is a simple text-based syntax for reporting test results.  TAP\nwas originally develped for Perl, and now has implementations in\nseveral languages.  For more information on TAP, see\nhttp://testanything.org/ and\nhttp://search.cpan.org/~petdance/TAP-1.0.0/TAP.pm\n\nTo use this library, wrap any calls to\nclojure.test/run-tests in the with-tap-output macro,\nlike this:\n\n  (use 'clojure.test)\n  (use 'clojure.test.tap)\n\n  (with-tap-output\n   (run-tests 'my.cool.library))"}),
 :vars
 ({:arglists ([] [x] [x y] [x y & more]),
   :name "*",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L687",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/*",
   :doc "Returns the product of nums. (*) returns 1.",
   :var-type "function",
   :line 687,
   :file "src/clj/clojure/core.clj"}
  {:file "src/clj/clojure/core.clj",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4259",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/*1",
   :namespace "clojure.core",
   :line 4259,
   :var-type "var",
   :doc "bound in a repl thread to the most recent value printed",
   :name "*1"}
  {:file "src/clj/clojure/core.clj",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4263",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/*2",
   :namespace "clojure.core",
   :line 4263,
   :var-type "var",
   :doc
   "bound in a repl thread to the second most recent value printed",
   :name "*2"}
  {:file "src/clj/clojure/core.clj",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4267",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/*3",
   :namespace "clojure.core",
   :line 4267,
   :var-type "var",
   :doc
   "bound in a repl thread to the third most recent value printed",
   :name "*3"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/*agent*",
   :namespace "clojure.core",
   :var-type "var",
   :doc
   "The agent currently running an action on this thread, else nil",
   :name "*agent*"}
  {:file "src/clj/clojure/core.clj",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4552",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/*clojure-version*",
   :namespace "clojure.core",
   :line 4552,
   :var-type "var",
   :doc
   "The version info for Clojure core, as a map containing :major :minor \n:incremental and :qualifier keys. Feature releases may increment \n:minor and/or :major, bugfix releases will increment :incremental. \nPossible values of :qualifier include \"GA\", \"SNAPSHOT\", \"RC-x\" \"BETA-x\"",
   :name "*clojure-version*"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/*command-line-args*",
   :namespace "clojure.core",
   :var-type "var",
   :doc
   "A sequence of the supplied command line arguments, or nil if\nnone were supplied",
   :name "*command-line-args*"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/*compile-files*",
   :namespace "clojure.core",
   :var-type "var",
   :doc "Set to true when compiling files, false otherwise.",
   :name "*compile-files*"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/*compile-path*",
   :namespace "clojure.core",
   :var-type "var",
   :doc
   "Specifies the directory where 'compile' will write out .class\nfiles. This directory must be in the classpath for 'compile' to\nwork.\n\nDefaults to \"classes\"",
   :name "*compile-path*"}
  {:file "src/clj/clojure/core.clj",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4271",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/*e",
   :namespace "clojure.core",
   :line 4271,
   :var-type "var",
   :doc
   "bound in a repl thread to the most recent exception caught by the repl",
   :name "*e"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/*err*",
   :namespace "clojure.core",
   :var-type "var",
   :doc
   "A java.io.Writer object representing standard error for print operations.\n\nDefaults to System/err, wrapped in a PrintWriter",
   :name "*err*"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/*file*",
   :namespace "clojure.core",
   :var-type "var",
   :doc
   "The path of the file being evaluated, as a String.\n\nEvaluates to nil when there is no file, eg. in the REPL.",
   :name "*file*"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/*flush-on-newline*",
   :namespace "clojure.core",
   :var-type "var",
   :doc
   "When set to true, output will be flushed whenever a newline is printed.\n\nDefaults to true.",
   :name "*flush-on-newline*"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/*in*",
   :namespace "clojure.core",
   :var-type "var",
   :doc
   "A java.io.Reader object representing standard input for read operations.\n\nDefaults to System/in, wrapped in a LineNumberingPushbackReader",
   :name "*in*"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/*ns*",
   :namespace "clojure.core",
   :var-type "var",
   :doc
   "A clojure.lang.Namespace object representing the current namespace.",
   :name "*ns*"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/*out*",
   :namespace "clojure.core",
   :var-type "var",
   :doc
   "A java.io.Writer object representing standard output for print operations.\n\nDefaults to System/out",
   :name "*out*"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/*print-dup*",
   :namespace "clojure.core",
   :var-type "var",
   :doc
   "When set to logical true, objects will be printed in a way that preserves\ntheir type when read in later.\n\nDefaults to false.",
   :name "*print-dup*"}
  {:file "src/clj/clojure/core_print.clj",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/1a0e23d0e78ef3d3a3a6267a68adcfc414d3fb56/src/clj/clojure/core_print.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/1a0e23d0e78ef3d3a3a6267a68adcfc414d3fb56/src/clj/clojure/core_print.clj#L15",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/*print-length*",
   :namespace "clojure.core",
   :line 15,
   :var-type "var",
   :doc
   "*print-length* controls how many items of each collection the\nprinter will print. If it is bound to logical false, there is no\nlimit. Otherwise, it must be bound to an integer indicating the maximum\nnumber of items of each collection to print. If a collection contains\nmore items, the printer will print items up to the limit followed by\n'...' to represent the remaining items. The root binding is nil\nindicating no limit.",
   :name "*print-length*"}
  {:file "src/clj/clojure/core_print.clj",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/1a0e23d0e78ef3d3a3a6267a68adcfc414d3fb56/src/clj/clojure/core_print.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/1a0e23d0e78ef3d3a3a6267a68adcfc414d3fb56/src/clj/clojure/core_print.clj#L25",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/*print-level*",
   :namespace "clojure.core",
   :line 25,
   :var-type "var",
   :doc
   "*print-level* controls how many levels deep the printer will\nprint nested objects. If it is bound to logical false, there is no\nlimit. Otherwise, it must be bound to an integer indicating the maximum\nlevel to print. Each argument to print is at level 0; if an argument is a\ncollection, its items are at level 1; and so on. If an object is a\ncollection and is at a level greater than or equal to the value bound to\n*print-level*, the printer prints '#' to represent it. The root binding\nis nil indicating no limit.",
   :name "*print-level*"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/*print-meta*",
   :namespace "clojure.core",
   :var-type "var",
   :doc
   "If set to logical true, when printing an object, its metadata will also\nbe printed in a form that can be read back by the reader.\n\nDefaults to false.",
   :name "*print-meta*"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/*print-readably*",
   :namespace "clojure.core",
   :var-type "var",
   :doc
   "When set to logical false, strings and characters will be printed with\nnon-alphanumeric characters converted to the appropriate escape sequences.\n\nDefaults to true",
   :name "*print-readably*"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/*read-eval*",
   :namespace "clojure.core",
   :var-type "var",
   :doc
   "When set to logical false, the EvalReader (#=(...)) is disabled in the \nread/load in the thread-local binding.\nExample: (binding [*read-eval* false] (read-string \"#=(eval (def x 3))\"))\n\nDefaults to true",
   :name "*read-eval*"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/*warn-on-reflection*",
   :namespace "clojure.core",
   :var-type "var",
   :doc
   "When set to true, the compiler will emit warnings when reflection is\nneeded to resolve Java method calls or field accesses.\n\nDefaults to false.",
   :name "*warn-on-reflection*"}
  {:arglists ([] [x] [x y] [x y & more]),
   :name "+",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L677",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/+",
   :doc "Returns the sum of nums. (+) returns 0.",
   :var-type "function",
   :line 677,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x] [x y] [x y & more]),
   :name "-",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L707",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/-",
   :doc
   "If no ys are supplied, returns the negation of x, else subtracts\nthe ys from x and returns the result.",
   :var-type "function",
   :line 707,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x] [x form] [x form & more]),
   :name "->",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1089",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/->",
   :doc
   "Threads the expr through the forms. Inserts x as the\nsecond item in the first form, making a list of it if it is not a\nlist already. If there are more forms, inserts the first form as the\nsecond item in second form, etc.",
   :var-type "macro",
   :line 1089,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x form] [x form & more]),
   :name "->>",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1100",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/->>",
   :doc
   "Threads the expr through the forms. Inserts x as the\nlast item in the first form, making a list of it if it is not a\nlist already. If there are more forms, inserts the first form as the\nlast item in second form, etc.",
   :var-type "macro",
   :line 1100,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x form] [x form & more]),
   :name "..",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1072",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/..",
   :doc
   "form => fieldName-symbol or (instanceMethodName-symbol args*)\n\nExpands into a member access (.) of the first member on the first\nargument, followed by the next member on the result, etc. For\ninstance:\n\n(.. System (getProperties) (get \"os.name\"))\n\nexpands to:\n\n(. (. System (getProperties)) (get \"os.name\"))\n\nbut is easier to write, read, and understand.",
   :var-type "macro",
   :line 1072,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x] [x y] [x y & more]),
   :name "/",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L697",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core//",
   :doc
   "If no denominators are supplied, returns 1/numerator,\nelse returns numerator divided by all of the denominators.",
   :var-type "function",
   :line 697,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x] [x y] [x y & more]),
   :name "<",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L627",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/<",
   :doc
   "Returns non-nil if nums are in monotonically increasing order,\notherwise false.",
   :var-type "function",
   :line 627,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x] [x y] [x y & more]),
   :name "<=",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L717",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/<=",
   :doc
   "Returns non-nil if nums are in monotonically non-decreasing order,\notherwise false.",
   :var-type "function",
   :line 717,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x] [x y] [x y & more]),
   :name "=",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L539",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/=",
   :doc
   "Equality. Returns true if x equals y, false if not. Same as\nJava x.equals(y) except it also works for nil, and compares\nnumbers and collections in a type-independent manner.  Clojure's immutable data\nstructures define equals() (and thus =) as a value, not an identity,\ncomparison.",
   :var-type "function",
   :line 539,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x] [x y] [x y & more]),
   :name "==",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L759",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/==",
   :doc
   "Returns non-nil if nums all have the same value, otherwise false",
   :var-type "function",
   :line 759,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x] [x y] [x y & more]),
   :name ">",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L731",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/>",
   :doc
   "Returns non-nil if nums are in monotonically decreasing order,\notherwise false.",
   :var-type "function",
   :line 731,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x] [x y] [x y & more]),
   :name ">=",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L745",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/>=",
   :doc
   "Returns non-nil if nums are in monotonically non-increasing order,\notherwise false.",
   :var-type "function",
   :line 745,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([s key]),
   :name "accessor",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2612",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/accessor",
   :doc
   "Returns a fn that, given an instance of a structmap with the basis,\nreturns the value at the key.  The key must be in the basis. The\nreturned function should be (slightly) more efficient than using\nget, but such use of accessors should be limited to known\nperformance-critical areas.",
   :var-type "function",
   :line 2612,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([array]),
   :name "aclone",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2473",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/aclone",
   :doc
   "Returns a clone of the Java array. Works on arrays of known\ntypes.",
   :var-type "function",
   :line 2473,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([url]),
   :name "add-classpath",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3472",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/add-classpath",
   :doc
   "DEPRECATED \n\nAdds the url (String or URL object) to the classpath per\nURLClassLoader.addURL",
   :var-type "function",
   :line 3472,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([reference key fn]),
   :name "add-watch",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1371",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/add-watch",
   :doc
   "Alpha - subject to change.\nAdds a watch function to an agent/atom/var/ref reference. The watch\nfn must be a fn of 4 args: a key, the reference, its old-state, its\nnew-state. Whenever the reference's state might have been changed,\nany registered watches will have their functions called. The watch fn\nwill be called synchronously, on the agent's thread if an agent,\nbefore any pending sends if agent or ref. Note that an atom's or\nref's state may have changed again prior to the fn call, so use\nold/new-state rather than derefing the reference. Note also that watch\nfns may be called from multiple threads simultaneously. Var watchers\nare triggered only by root binding changes, not thread-local\nset!s. Keys must be unique per reference, and can be used to remove\nthe watch with remove-watch, but are otherwise considered opaque by\nthe watch mechanism.",
   :var-type "function",
   :line 1371,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([state] [state & options]),
   :name "agent",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1327",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/agent",
   :doc
   "Creates and returns an agent with an initial value of state and\nzero or more options (in any order):\n\n:meta metadata-map\n\n:validator validate-fn\n\nIf metadata-map is supplied, it will be come the metadata on the\nagent. validate-fn must be nil or a side-effect-free fn of one\nargument, which will be passed the intended new state on any state\nchange. If the new state is unacceptable, the validate-fn should\nreturn false or throw an exception.",
   :var-type "function",
   :line 1327,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([a]),
   :name "agent-errors",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1395",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/agent-errors",
   :doc
   "Returns a sequence of the exceptions thrown during asynchronous\nactions of the agent.",
   :var-type "function",
   :line 1395,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([array idx] [array idx & idxs]),
   :name "aget",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2479",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/aget",
   :doc
   "Returns the value at the index/indices. Works on Java arrays of all\ntypes.",
   :var-type "function",
   :line 2479,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([array]),
   :name "alength",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2467",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/alength",
   :doc
   "Returns the length of the Java array. Works on arrays of all\ntypes.",
   :var-type "function",
   :line 2467,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([alias namespace-sym]),
   :name "alias",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2753",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/alias",
   :doc
   "Add an alias in the current namespace to another\nnamespace. Arguments are two symbols: the alias to be used, and\nthe symbolic name of the target namespace. Use :as in the ns macro in preference\nto calling this directly.",
   :var-type "function",
   :line 2753,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([]),
   :name "all-ns",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2662",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/all-ns",
   :doc "Returns a sequence of all namespaces.",
   :var-type "function",
   :line 2662,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([ref fun & args]),
   :name "alter",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1534",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/alter",
   :doc
   "Must be called in a transaction. Sets the in-transaction-value of\nref to:\n\n(apply fun in-transaction-value-of-ref args)\n\nand returns the in-transaction-value of ref.",
   :var-type "function",
   :line 1534,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([iref f & args]),
   :name "alter-meta!",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1503",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/alter-meta!",
   :doc
   "Atomically sets the metadata for a namespace/var/ref/agent/atom to be:\n\n(apply f its-current-meta args)\n\nf must be free of side-effects",
   :var-type "function",
   :line 1503,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([v f & args]),
   :name "alter-var-root",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3664",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/alter-var-root",
   :doc
   "Atomically alters the root binding of var v by applying f to its\ncurrent value plus any args",
   :var-type "function",
   :line 3664,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([a idx ret expr]),
   :name "amap",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3508",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/amap",
   :doc
   "Maps an expression across an array a, using an index named idx, and\nreturn value named ret, initialized to a clone of a, then setting \neach element of ret to the evaluation of expr, returning the new \narray ret.",
   :var-type "macro",
   :line 3508,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([tag] [h tag]),
   :name "ancestors",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3729",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/ancestors",
   :doc
   "Returns the immediate and indirect parents of tag, either via a Java type\ninheritance relationship or a relationship established via derive. h\nmust be a hierarchy obtained from make-hierarchy, if not supplied\ndefaults to the global hierarchy",
   :var-type "function",
   :line 3729,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([] [x] [x & next]),
   :name "and",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L577",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/and",
   :doc
   "Evaluates exprs one at a time, from left to right. If a form\nreturns logical false (nil or false), and returns that value and\ndoesn't evaluate any of the other expressions, otherwise it returns\nthe value of the last expr. (and) returns true.",
   :var-type "macro",
   :line 577,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([f args* argseq]),
   :name "apply",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L432",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/apply",
   :doc
   "Applies fn f to the argument list formed by prepending args to argseq.",
   :var-type "function",
   :line 432,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([a idx ret init expr]),
   :name "areduce",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3523",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/areduce",
   :doc
   "Reduces an expression across an array a, using an index named idx,\nand return value named ret, initialized to init, setting ret to the \nevaluation of expr at each step, returning ret.",
   :var-type "macro",
   :line 3523,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([] [& keyvals]),
   :name "array-map",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2831",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/array-map",
   :doc "Constructs an array-map.",
   :var-type "function",
   :line 2831,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([array idx val] [array idx idx2 & idxv]),
   :name "aset",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2489",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/aset",
   :doc
   "Sets the value at the index/indices. Works on Java arrays of\nreference types. Returns val.",
   :var-type "function",
   :line 2489,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([array idx val] [array idx idx2 & idxv]),
   :name "aset-boolean",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2519",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/aset-boolean",
   :doc
   "Sets the value at the index/indices. Works on arrays of boolean. Returns val.",
   :var-type "function",
   :line 2519,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([array idx val] [array idx idx2 & idxv]),
   :name "aset-byte",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2535",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/aset-byte",
   :doc
   "Sets the value at the index/indices. Works on arrays of byte. Returns val.",
   :var-type "function",
   :line 2535,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([array idx val] [array idx idx2 & idxv]),
   :name "aset-char",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2539",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/aset-char",
   :doc
   "Sets the value at the index/indices. Works on arrays of char. Returns val.",
   :var-type "function",
   :line 2539,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([array idx val] [array idx idx2 & idxv]),
   :name "aset-double",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2527",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/aset-double",
   :doc
   "Sets the value at the index/indices. Works on arrays of double. Returns val.",
   :var-type "function",
   :line 2527,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([array idx val] [array idx idx2 & idxv]),
   :name "aset-float",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2523",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/aset-float",
   :doc
   "Sets the value at the index/indices. Works on arrays of float. Returns val.",
   :var-type "function",
   :line 2523,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([array idx val] [array idx idx2 & idxv]),
   :name "aset-int",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2511",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/aset-int",
   :doc
   "Sets the value at the index/indices. Works on arrays of int. Returns val.",
   :var-type "function",
   :line 2511,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([array idx val] [array idx idx2 & idxv]),
   :name "aset-long",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2515",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/aset-long",
   :doc
   "Sets the value at the index/indices. Works on arrays of long. Returns val.",
   :var-type "function",
   :line 2515,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([array idx val] [array idx idx2 & idxv]),
   :name "aset-short",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2531",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/aset-short",
   :doc
   "Sets the value at the index/indices. Works on arrays of short. Returns val.",
   :var-type "function",
   :line 2531,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "assert",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3149",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/assert",
   :doc
   "Evaluates expr and throws an exception if it does not evaluate to\nlogical true.",
   :var-type "macro",
   :line 3149,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([map key val] [map key val & kvs]),
   :name "assoc",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L138",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/assoc",
   :doc
   "assoc[iate]. When applied to a map, returns a new map of the\nsame (hashed/sorted) type, that contains the mapping of key(s) to\nval(s). When applied to a vector, returns a new vector that\ncontains val at index. Note - index must be <= (count vector).",
   :var-type "function",
   :line 138,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([coll key val] [coll key val & kvs]),
   :name "assoc!",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4624",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/assoc!",
   :doc
   "Alpha - subject to change.\nWhen applied to a transient map, adds mapping of key(s) to\nval(s). When applied to a transient vector, sets the val at index.\nNote - index must be <= (count vector). Returns coll.",
   :var-type "function",
   :line 4624,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([m [k & ks] v]),
   :name "assoc-in",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4191",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/assoc-in",
   :doc
   "Associates a value in a nested associative structure, where ks is a\nsequence of keys and v is the new value and returns a new nested structure.\nIf any levels do not exist, hash-maps will be created.",
   :var-type "function",
   :line 4191,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([coll]),
   :name "associative?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4239",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/associative?",
   :doc "Returns true if coll implements Associative",
   :var-type "function",
   :line 4239,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x] [x & options]),
   :name "atom",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1453",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/atom",
   :doc
   "Creates and returns an Atom with an initial value of x and zero or\nmore options (in any order):\n\n:meta metadata-map\n\n:validator validate-fn\n\nIf metadata-map is supplied, it will be come the metadata on the\natom. validate-fn must be nil or a side-effect-free fn of one\nargument, which will be passed the intended new state on any state\nchange. If the new state is unacceptable, the validate-fn should\nreturn false or throw an exception.",
   :var-type "function",
   :line 1453,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([& agents]),
   :name "await",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2103",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/await",
   :doc
   "Blocks the current thread (indefinitely!) until all actions\ndispatched thus far, from this thread or agent, to the agent(s) have\noccurred.",
   :var-type "function",
   :line 2103,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([timeout-ms & agents]),
   :name "await-for",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2122",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/await-for",
   :doc
   "Blocks the current thread until all actions dispatched thus\nfar (from this thread or agent) to the agents have occurred, or the\ntimeout (in milliseconds) has elapsed. Returns nil if returning due\nto timeout, non-nil otherwise.",
   :var-type "function",
   :line 2122,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([c]),
   :name "bases",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3680",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/bases",
   :doc
   "Returns the immediate superclass and direct interfaces of c, if any",
   :var-type "function",
   :line 3680,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "bean",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/040f083efc16dd830a4508a35a04465e3e5677d3/src/clj/clojure/core_proxy.clj#L360",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/040f083efc16dd830a4508a35a04465e3e5677d3/src/clj/clojure/core_proxy.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/bean",
   :doc
   "Takes a Java object and returns a read-only implementation of the\nmap abstraction based upon its JavaBean properties.",
   :var-type "function",
   :line 360,
   :file "src/clj/clojure/core_proxy.clj"}
  {:arglists ([x]),
   :name "bigdec",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2301",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/bigdec",
   :doc "Coerce to BigDecimal",
   :var-type "function",
   :line 2301,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "bigint",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2292",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/bigint",
   :doc "Coerce to BigInteger",
   :var-type "function",
   :line 2292,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([bindings & body]),
   :name "binding",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1251",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/binding",
   :doc
   "binding => var-symbol init-expr\n\nCreates new bindings for the (already-existing) vars, with the\nsupplied initial values, executes the exprs in an implicit do, then\nre-establishes the bindings that existed before.  The new bindings\nare made in parallel (unlike let); all init-exprs are evaluated\nbefore the vars are bound to their new values.",
   :var-type "macro",
   :line 1251,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x y]),
   :name "bit-and",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L874",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/bit-and",
   :doc "Bitwise and",
   :var-type "function",
   :line 874,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x y]),
   :name "bit-and-not",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L889",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/bit-and-not",
   :doc "Bitwise and with complement",
   :var-type "function",
   :line 889,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x n]),
   :name "bit-clear",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L894",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/bit-clear",
   :doc "Clear bit at index n",
   :var-type "function",
   :line 894,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x n]),
   :name "bit-flip",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L902",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/bit-flip",
   :doc "Flip bit at index n",
   :var-type "function",
   :line 902,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "bit-not",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L868",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/bit-not",
   :doc "Bitwise complement",
   :var-type "function",
   :line 868,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x y]),
   :name "bit-or",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L879",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/bit-or",
   :doc "Bitwise or",
   :var-type "function",
   :line 879,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x n]),
   :name "bit-set",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L898",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/bit-set",
   :doc "Set bit at index n",
   :var-type "function",
   :line 898,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x n]),
   :name "bit-shift-left",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L911",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/bit-shift-left",
   :doc "Bitwise shift left",
   :var-type "function",
   :line 911,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x n]),
   :name "bit-shift-right",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L915",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/bit-shift-right",
   :doc "Bitwise shift right",
   :var-type "function",
   :line 915,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x n]),
   :name "bit-test",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L906",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/bit-test",
   :doc "Test bit at index n",
   :var-type "function",
   :line 906,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x y]),
   :name "bit-xor",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L884",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/bit-xor",
   :doc "Bitwise exclusive or",
   :var-type "function",
   :line 884,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "boolean",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2246",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/boolean",
   :doc "Coerce to boolean",
   :var-type "function",
   :line 2246,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([size-or-seq] [size init-val-or-seq]),
   :name "boolean-array",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3541",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/boolean-array",
   :doc "Creates an array of booleans",
   :var-type "function",
   :line 3541,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([xs]),
   :name "booleans",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3590",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/booleans",
   :doc "Casts to boolean[]",
   :var-type "function",
   :line 3590,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([& fntail]),
   :name "bound-fn",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1304",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/bound-fn",
   :doc
   "Returns a function defined by the given fntail, which will install the\nsame bindings in effect as in the thread at the time bound-fn was called.\nThis may be used to define a helper function which runs on a different\nthread, but needs the same bindings in place.",
   :var-type "macro",
   :line 1304,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([f]),
   :name "bound-fn*",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1294",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/bound-fn*",
   :doc
   "Returns a function, which will install the same bindings in effect as in\nthe thread at the time bound-fn* was called and then call f with any given\narguments. This may be used to define a helper function which runs on a\ndifferent thread, but needs the same bindings in place.",
   :var-type "function",
   :line 1294,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([coll]),
   :name "butlast",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L197",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/butlast",
   :doc
   "Return a seq of all but the last item in coll, in linear time",
   :var-type "function",
   :line 197,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "byte",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2234",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/byte",
   :doc "Coerce to byte",
   :var-type "function",
   :line 2234,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([size-or-seq] [size init-val-or-seq]),
   :name "byte-array",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3548",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/byte-array",
   :doc "Creates an array of bytes",
   :var-type "function",
   :line 3548,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([xs]),
   :name "bytes",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3594",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/bytes",
   :doc "Casts to bytes[]",
   :var-type "function",
   :line 3594,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([c x]),
   :name "cast",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L250",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/cast",
   :doc "Throws a ClassCastException if x is not a c, else returns x.",
   :var-type "function",
   :line 250,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "char",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2240",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/char",
   :doc "Coerce to char",
   :var-type "function",
   :line 2240,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([size-or-seq] [size init-val-or-seq]),
   :name "char-array",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3555",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/char-array",
   :doc "Creates an array of chars",
   :var-type "function",
   :line 3555,
   :file "src/clj/clojure/core.clj"}
  {:file "src/clj/clojure/core_print.clj",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/1a0e23d0e78ef3d3a3a6267a68adcfc414d3fb56/src/clj/clojure/core_print.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/1a0e23d0e78ef3d3a3a6267a68adcfc414d3fb56/src/clj/clojure/core_print.clj#L165",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/char-escape-string",
   :namespace "clojure.core",
   :line 165,
   :var-type "var",
   :doc "Returns escape string for char or nil if none",
   :name "char-escape-string"}
  {:file "src/clj/clojure/core_print.clj",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/1a0e23d0e78ef3d3a3a6267a68adcfc414d3fb56/src/clj/clojure/core_print.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/1a0e23d0e78ef3d3a3a6267a68adcfc414d3fb56/src/clj/clojure/core_print.clj#L223",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/char-name-string",
   :namespace "clojure.core",
   :line 223,
   :var-type "var",
   :doc "Returns name string for char or nil if none",
   :name "char-name-string"}
  {:arglists ([x]),
   :name "char?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L118",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/char?",
   :doc "Return true if x is a Character",
   :var-type "function",
   :line 118,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([xs]),
   :name "chars",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3598",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/chars",
   :doc "Casts to chars[]",
   :var-type "function",
   :line 3598,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "class",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2195",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/class",
   :doc "Returns the Class of x",
   :var-type "function",
   :line 2195,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "class?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3660",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/class?",
   :doc "Returns true if x is an instance of Class",
   :var-type "function",
   :line 3660,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([a]),
   :name "clear-agent-errors",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1400",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/clear-agent-errors",
   :doc
   "Clears any exceptions thrown during asynchronous actions of the\nagent, allowing subsequent actions to occur.",
   :var-type "function",
   :line 1400,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([]),
   :name "clojure-version",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4563",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/clojure-version",
   :doc "Returns clojure version as a printable string.",
   :var-type "function",
   :line 4563,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "coll?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4217",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/coll?",
   :doc "Returns true if x implements IPersistentCollection",
   :var-type "function",
   :line 4217,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([& body]),
   :name "comment",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3099",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/comment",
   :doc "Ignores body, yields nil",
   :var-type "macro",
   :line 3099,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([ref fun & args]),
   :name "commute",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1515",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/commute",
   :doc
   "Must be called in a transaction. Sets the in-transaction-value of\nref to:\n\n(apply fun in-transaction-value-of-ref args)\n\nand returns the in-transaction-value of ref.\n\nAt the commit point of the transaction, sets the value of ref to be:\n\n(apply fun most-recently-committed-value-of-ref args)\n\nThus fun should be commutative, or, failing that, you must accept\nlast-one-in-wins behavior.  commute allows for more concurrency than\nref-set.",
   :var-type "function",
   :line 1515,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([f] [f g] [f g h] [f1 f2 f3 & fs]),
   :name "comp",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1605",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/comp",
   :doc
   "Takes a set of functions and returns a fn that is the composition\nof those fns.  The returned fn takes a variable number of args,\napplies the rightmost of fns to the args, the next\nfn (right-to-left) to the result, etc.",
   :var-type "function",
   :line 1605,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([pred]),
   :name "comparator",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1962",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/comparator",
   :doc
   "Returns an implementation of java.util.Comparator based upon pred.",
   :var-type "function",
   :line 1962,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x y]),
   :name "compare",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L567",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/compare",
   :doc
   "Comparator. Returns a negative number, zero, or a positive number\nwhen x is logically 'less than', 'equal to', or 'greater than'\ny. Same as Java x.compareTo(y) except it also works for nil, and\ncompares numbers and collections in a type-independent manner. x\nmust implement Comparable",
   :var-type "function",
   :line 567,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([atom oldval newval]),
   :name "compare-and-set!",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1479",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/compare-and-set!",
   :doc
   "Atomically sets the value of atom to newval if and only if the\ncurrent value of the atom is identical to oldval. Returns true if\nset happened, else false",
   :var-type "function",
   :line 1479,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([lib]),
   :name "compile",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4173",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/compile",
   :doc
   "Compiles the namespace named by the symbol lib into a set of\nclassfiles. The source for the lib must be in a proper\nclasspath-relative directory. The output files will go into the\ndirectory specified by *compile-path*, and that directory too must\nbe in the classpath.",
   :var-type "function",
   :line 4173,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([f]),
   :name "complement",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L930",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/complement",
   :doc
   "Takes a fn f and returns a fn that takes the same arguments as f,\nhas the same effects, if any, and returns the opposite truth value.",
   :var-type "function",
   :line 930,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([] [x] [x y] [x y & zs]),
   :name "concat",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L488",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/concat",
   :doc
   "Returns a lazy seq representing the concatenation of the elements in the supplied colls.",
   :var-type "function",
   :line 488,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([& clauses]),
   :name "cond",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L400",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/cond",
   :doc
   "Takes a set of test/expr pairs. It evaluates each test one at a\ntime.  If a test returns logical true, cond evaluates and returns\nthe value of the corresponding expr and doesn't evaluate any of the\nother tests or exprs. (cond) returns nil.",
   :var-type "macro",
   :line 400,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([pred expr & clauses]),
   :name "condp",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4332",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/condp",
   :doc
   "Takes a binary predicate, an expression, and a set of clauses.\nEach clause can take the form of either:\n\ntest-expr result-expr\n\ntest-expr :>> result-fn\n\nNote :>> is an ordinary keyword.\n\nFor each clause, (pred test-expr expr) is evaluated. If it returns\nlogical true, the clause is a match. If a binary clause matches, the\nresult-expr is returned, if a ternary clause matches, its result-fn,\nwhich must be a unary function, is called with the result of the\npredicate as its argument, the result of that call being the return\nvalue of condp. A single default expression can follow the clauses,\nand its value will be returned if no clause matches. If no default\nexpression is provided and no clause matches, an\nIllegalArgumentException is thrown.",
   :var-type "macro",
   :line 4332,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([coll x] [coll x & xs]),
   :name "conj",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L61",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/conj",
   :doc
   "conj[oin]. Returns a new collection with the xs\n'added'. (conj nil item) returns (item).  The 'addition' may\nhappen at different 'places' depending on the concrete type.",
   :var-type "function",
   :line 61,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([coll x]),
   :name "conj!",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4617",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/conj!",
   :doc
   "Alpha - subject to change.\nAdds x to the transient collection, and return coll. The 'addition'\nmay happen at different 'places' depending on the concrete type.",
   :var-type "function",
   :line 4617,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x seq]),
   :name "cons",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L21",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/cons",
   :doc
   "Returns a new seq where x is the first element and seq is\nthe rest.",
   :var-type "function",
   :line 21,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "constantly",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L940",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/constantly",
   :doc
   "Returns a function that takes any number of arguments and returns x.",
   :var-type "function",
   :line 940,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([c & ctor-args]),
   :name "construct-proxy",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/040f083efc16dd830a4508a35a04465e3e5677d3/src/clj/clojure/core_proxy.clj#L263",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/040f083efc16dd830a4508a35a04465e3e5677d3/src/clj/clojure/core_proxy.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/construct-proxy",
   :doc
   "Takes a proxy class and any arguments for its superclass ctor and\ncreates and returns an instance of the proxy.",
   :var-type "function",
   :line 263,
   :file "src/clj/clojure/core_proxy.clj"}
  {:arglists ([coll key]),
   :name "contains?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L969",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/contains?",
   :doc
   "Returns true if key is present in the given collection, otherwise\nreturns false.  Note that for numerically indexed collections like\nvectors and Java arrays, this tests if the numeric key is within the\nrange of indexes. 'contains?' operates constant or logarithmic time;\nit will not perform a linear search for a value.  See also 'some'.",
   :var-type "function",
   :line 969,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([coll]),
   :name "count",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L606",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/count",
   :doc
   "Returns the number of items in the collection. (count nil) returns\n0.  Also works on strings, arrays, and Java Collections and Maps",
   :var-type "function",
   :line 606,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([coll]),
   :name "counted?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4251",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/counted?",
   :doc "Returns true if coll implements count in constant time",
   :var-type "function",
   :line 4251,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([sym]),
   :name "create-ns",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2651",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/create-ns",
   :doc
   "Create a new namespace named by the symbol if one doesn't already\nexist, returns it or the already-existing namespace of the same\nname.",
   :var-type "function",
   :line 2651,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([& keys]),
   :name "create-struct",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2587",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/create-struct",
   :doc "Returns a structure basis object.",
   :var-type "function",
   :line 2587,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([coll]),
   :name "cycle",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1868",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/cycle",
   :doc
   "Returns a lazy (infinite!) sequence of repetitions of the items in coll.",
   :var-type "function",
   :line 1868,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "dec",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L786",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/dec",
   :doc "Returns a number one less than num.",
   :var-type "function",
   :line 786,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([n]),
   :name "decimal?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2278",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/decimal?",
   :doc "Returns true if n is a BigDecimal",
   :var-type "function",
   :line 2278,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([& names]),
   :name "declare",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4275",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/declare",
   :doc
   "defs the supplied var names with no bindings, useful for making forward declarations.",
   :var-type "macro",
   :line 4275,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([name & decl]),
   :name "definline",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3491",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/definline",
   :doc
   "Experimental - like defmacro, except defines a named function whose\nbody is the expansion, calls to which may be expanded inline as if\nit were a macro. Cannot be used with variadic (&) args.",
   :var-type "macro",
   :line 3491,
   :file "src/clj/clojure/core.clj"}
  {:arglists
   ([name doc-string? attr-map? [params*] body]
    [name doc-string? attr-map? ([params*] body) + attr-map?]),
   :name "defmacro",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L311",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/defmacro",
   :doc
   "Like defn, but the resulting function name is declared as a\nmacro and will be used as a macro by the compiler when it is\ncalled.",
   :var-type "macro",
   :line 311,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([multifn dispatch-val & fn-tail]),
   :name "defmethod",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1152",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/defmethod",
   :doc
   "Creates and installs a new method of multimethod associated with dispatch-value. ",
   :var-type "macro",
   :line 1152,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([name docstring? attr-map? dispatch-fn & options]),
   :name "defmulti",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1113",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/defmulti",
   :doc
   "Creates a new multimethod with the associated dispatch function.\nThe docstring and attribute-map are optional.\n\nOptions are key-value pairs and may be one of:\n  :default    the default dispatch value, defaults to :default\n  :hierarchy  the isa? hierarchy to use for dispatching\n              defaults to the global hierarchy",
   :var-type "macro",
   :line 1113,
   :file "src/clj/clojure/core.clj"}
  {:arglists
   ([name doc-string? attr-map? [params*] body]
    [name doc-string? attr-map? ([params*] body) + attr-map?]),
   :name "defn",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L206",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/defn",
   :doc
   "Same as (def name (fn [params* ] exprs*)) or (def\nname (fn ([params* ] exprs*)+)) with any doc-string or attrs added\nto the var metadata",
   :var-type "macro",
   :line 206,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([name & decls]),
   :name "defn-",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3236",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/defn-",
   :doc "same as defn, yielding non-public def",
   :var-type "macro",
   :line 3236,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([name expr]),
   :name "defonce",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3932",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/defonce",
   :doc
   "defs name to have the root value of the expr iff the named var has no root value,\nelse expr is unevaluated",
   :var-type "macro",
   :line 3932,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([name & keys]),
   :name "defstruct",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2592",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/defstruct",
   :doc "Same as (def name (create-struct keys...))",
   :var-type "macro",
   :line 2592,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([& body]),
   :name "delay",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L516",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/delay",
   :doc
   "Takes a body of expressions and yields a Delay object that will\ninvoke the body only the first time it is forced (with force), and\nwill cache the result and return it on all subsequent force\ncalls.",
   :var-type "macro",
   :line 516,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "delay?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L524",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/delay?",
   :doc "returns true if x is a Delay created with delay",
   :var-type "function",
   :line 524,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([promise val]),
   :name "deliver",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4596",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/deliver",
   :doc
   "Alpha - subject to change.\nDelivers the supplied value to the promise, releasing any pending\nderefs. A subsequent call to deliver on a promise will throw an exception.",
   :var-type "function",
   :line 4596,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([ref]),
   :name "deref",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1444",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/deref",
   :doc
   "Also reader macro: @ref/@agent/@var/@atom/@delay/@future. Within a transaction,\nreturns the in-transaction-value of ref, else returns the\nmost-recently-committed value of ref. When applied to a var, agent\nor atom, returns its current state. When applied to a delay, forces\nit if not already forced. When applied to a future, will block if\ncomputation not complete",
   :var-type "function",
   :line 1444,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([tag parent] [h tag parent]),
   :name "derive",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3755",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/derive",
   :doc
   "Establishes a parent/child relationship between parent and\ntag. Parent must be a namespace-qualified symbol or keyword and\nchild can be either a namespace-qualified symbol or keyword or a\nclass. h must be a hierarchy obtained from make-hierarchy, if not\nsupplied defaults to, and modifies, the global hierarchy.",
   :var-type "function",
   :line 3755,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([tag] [h tag]),
   :name "descendants",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3744",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/descendants",
   :doc
   "Returns the immediate and indirect children of tag, through a\nrelationship established via derive. h must be a hierarchy obtained\nfrom make-hierarchy, if not supplied defaults to the global\nhierarchy. Note: does not work on Java type inheritance\nrelationships.",
   :var-type "function",
   :line 3744,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([set] [set key] [set key & ks]),
   :name "disj",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L996",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/disj",
   :doc
   "disj[oin]. Returns a new set of the same (hashed/sorted) type, that\ndoes not contain key(s).",
   :var-type "function",
   :line 996,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([set] [set key] [set key & ks]),
   :name "disj!",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4653",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/disj!",
   :doc
   "Alpha - subject to change.\ndisj[oin]. Returns a transient set of the same (hashed/sorted) type, that\ndoes not contain key(s).",
   :var-type "function",
   :line 4653,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([map] [map key] [map key & ks]),
   :name "dissoc",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L984",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/dissoc",
   :doc
   "dissoc[iate]. Returns a new map of the same (hashed/sorted) type,\nthat does not contain a mapping for key(s).",
   :var-type "function",
   :line 984,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([map key] [map key & ks]),
   :name "dissoc!",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4636",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/dissoc!",
   :doc
   "Alpha - subject to change.\nReturns a transient map that doesn't contain a mapping for key(s).",
   :var-type "function",
   :line 4636,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([coll]),
   :name "distinct",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3378",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/distinct",
   :doc
   "Returns a lazy sequence of the elements of coll with duplicates removed",
   :var-type "function",
   :line 3378,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x] [x y] [x y & more]),
   :name "distinct?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3812",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/distinct?",
   :doc "Returns true if no two of the arguments are =",
   :var-type "function",
   :line 3812,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([coll] [n coll]),
   :name "doall",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2089",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/doall",
   :doc
   "When lazy sequences are produced via functions that have side\neffects, any effects other than those needed to produce the first\nelement in the seq do not occur until the seq is consumed. doall can\nbe used to force any effects. Walks through the successive nexts of\nthe seq, retains the head and returns it, thus causing the entire\nseq to reside in memory at one time.",
   :var-type "function",
   :line 2089,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([name]),
   :name "doc",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3288",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/doc",
   :doc
   "Prints documentation for a var or special form given its name",
   :var-type "macro",
   :line 3288,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([coll] [n coll]),
   :name "dorun",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2076",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/dorun",
   :doc
   "When lazy sequences are produced via functions that have side\neffects, any effects other than those needed to produce the first\nelement in the seq do not occur until the seq is consumed. dorun can\nbe used to force any effects. Walks through the successive nexts of\nthe seq, does not retain the head and returns nil.",
   :var-type "function",
   :line 2076,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([seq-exprs & body]),
   :name "doseq",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2019",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/doseq",
   :doc
   "Repeatedly executes body (presumably for side-effects) with\nbindings and filtering as provided by \"for\".  Does not retain\nthe head of the sequence. Returns nil.",
   :var-type "macro",
   :line 2019,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([& exprs]),
   :name "dosync",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3406",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/dosync",
   :doc
   "Runs the exprs (in an implicit do) in a transaction that encompasses\nexprs and any nested calls.  Starts a transaction if none is already\nrunning on this thread. Any uncaught exception will abort the\ntransaction and flow out of dosync. The exprs may be run more than\nonce, but any effects on Refs will be atomic.",
   :var-type "macro",
   :line 3406,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([bindings & body]),
   :name "dotimes",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2137",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/dotimes",
   :doc
   "bindings => name n\n\nRepeatedly executes body (presumably for side-effects) with name\nbound to integers from 0 through n-1.",
   :var-type "macro",
   :line 2137,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x & forms]),
   :name "doto",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2429",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/doto",
   :doc
   "Evaluates x then calls all of the methods and functions with the\nvalue of x supplied at the from of the given arguments.  The forms\nare evaluated in order.  Returns x.\n\n(doto (new java.util.HashMap) (.put \"a\" 1) (.put \"b\" 2))",
   :var-type "macro",
   :line 2429,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "double",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2222",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/double",
   :doc "Coerce to double",
   :var-type "function",
   :line 2222,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([size-or-seq] [size init-val-or-seq]),
   :name "double-array",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3569",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/double-array",
   :doc "Creates an array of doubles",
   :var-type "function",
   :line 3569,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([xs]),
   :name "doubles",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3614",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/doubles",
   :doc "Casts to double[]",
   :var-type "function",
   :line 3614,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([n coll]),
   :name "drop",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1833",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/drop",
   :doc
   "Returns a lazy sequence of all but the first n items in coll.",
   :var-type "function",
   :line 1833,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([s] [n s]),
   :name "drop-last",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1843",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/drop-last",
   :doc
   "Return a lazy sequence of all but the last n (default 1) items in coll",
   :var-type "function",
   :line 1843,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([pred coll]),
   :name "drop-while",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1857",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/drop-while",
   :doc
   "Returns a lazy sequence of the items in coll starting from the first\nitem for which (pred item) returns nil.",
   :var-type "function",
   :line 1857,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([coll]),
   :name "empty",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3502",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/empty",
   :doc
   "Returns an empty collection of the same category as coll, or nil",
   :var-type "function",
   :line 3502,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([coll]),
   :name "empty?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4212",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/empty?",
   :doc
   "Returns true if coll has no items - same as (not (seq coll)).\nPlease use the idiom (seq x) rather than (not (empty? x))",
   :var-type "function",
   :line 4212,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([ref]),
   :name "ensure",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1569",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/ensure",
   :doc
   "Must be called in a transaction. Protects the ref from modification\nby other transactions.  Returns the in-transaction-value of\nref. Allows for more concurrency than (ref-set ref @ref)",
   :var-type "function",
   :line 1569,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([e]),
   :name "enumeration-seq",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3851",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/enumeration-seq",
   :doc "Returns a seq on a java.util.Enumeration",
   :var-type "function",
   :line 3851,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([form]),
   :name "eval",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2015",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/eval",
   :doc
   "Evaluates the form data structure (not text!) and returns the result.",
   :var-type "function",
   :line 2015,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([n]),
   :name "even?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L919",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/even?",
   :doc
   "Returns true if n is even, throws an exception if n is not an integer",
   :var-type "function",
   :line 919,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([pred coll]),
   :name "every?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1698",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/every?",
   :doc
   "Returns true if (pred x) is logical true for every x in coll, else\nfalse.",
   :var-type "function",
   :line 1698,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "false?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L341",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/false?",
   :doc "Returns true if x is the value false, false otherwise.",
   :var-type "function",
   :line 341,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "ffirst",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L78",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/ffirst",
   :doc "Same as (first (first x))",
   :var-type "function",
   :line 78,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([dir]),
   :name "file-seq",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3317",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/file-seq",
   :doc "A tree seq on java.io.Files",
   :var-type "function",
   :line 3317,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([pred coll]),
   :name "filter",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1789",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/filter",
   :doc
   "Returns a lazy sequence of the items in coll for which\n(pred item) returns true. pred must be free of side-effects.",
   :var-type "function",
   :line 1789,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([map key]),
   :name "find",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1008",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/find",
   :doc "Returns the map entry for key, or nil if key not present.",
   :var-type "function",
   :line 1008,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([re-string-or-pattern]),
   :name "find-doc",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3249",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/find-doc",
   :doc
   "Prints documentation for any var whose documentation or name\ncontains a match for re-string-or-pattern",
   :var-type "function",
   :line 3249,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([sym]),
   :name "find-ns",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2647",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/find-ns",
   :doc
   "Returns the namespace named by the symbol or nil if it doesn't exist.",
   :var-type "function",
   :line 2647,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([sym]),
   :name "find-var",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1312",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/find-var",
   :doc
   "Returns the global var named by the namespace-qualified symbol, or\nnil if no var with that name.",
   :var-type "function",
   :line 1312,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([coll]),
   :name "first",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L41",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/first",
   :doc
   "Returns the first item in the collection. Calls seq on its\nargument. If coll is nil, returns nil.",
   :var-type "function",
   :line 41,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "float",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2216",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/float",
   :doc "Coerce to float",
   :var-type "function",
   :line 2216,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([size-or-seq] [size init-val-or-seq]),
   :name "float-array",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3534",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/float-array",
   :doc "Creates an array of floats",
   :var-type "function",
   :line 3534,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([n]),
   :name "float?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2282",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/float?",
   :doc "Returns true if n is a floating point number",
   :var-type "function",
   :line 2282,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([xs]),
   :name "floats",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3606",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/floats",
   :doc "Casts to float[]",
   :var-type "function",
   :line 3606,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([]),
   :name "flush",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2345",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/flush",
   :doc
   "Flushes the output stream that is the current value of\n*out*",
   :var-type "function",
   :line 2345,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([& sigs]),
   :name "fn",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2913",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/fn",
   :doc
   "(fn name? [params* ] exprs*)\n(fn name? ([params* ] exprs*)+)\n\nparams => positional-params* , or positional-params* & next-param\npositional-param => binding-form\nnext-param => binding-form\nname => symbol\n\nDefines a function",
   :var-type "macro",
   :line 2913,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "fn?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4234",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/fn?",
   :doc
   "Returns true if x implements Fn, i.e. is an object created via fn.",
   :var-type "function",
   :line 4234,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "fnext",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L88",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/fnext",
   :doc "Same as (first (next x))",
   :var-type "function",
   :line 88,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([seq-exprs body-expr]),
   :name "for",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3013",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/for",
   :doc
   "List comprehension. Takes a vector of one or more\n binding-form/collection-expr pairs, each followed by zero or more\n modifiers, and yields a lazy sequence of evaluations of expr.\n Collections are iterated in a nested fashion, rightmost fastest,\n and nested coll-exprs can refer to bindings created in prior\n binding-forms.  Supported modifiers are: :let [binding-form expr ...],\n :while test, :when test.\n\n(take 100 (for [x (range 100000000) y (range 1000000) :while (< y x)] [x y]))",
   :var-type "macro",
   :line 3013,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "force",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L528",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/force",
   :doc
   "If x is a Delay, returns the (possibly cached) value of its expression, else returns x",
   :var-type "function",
   :line 528,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([fmt & args]),
   :name "format",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3856",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/format",
   :doc
   "Formats a string using java.lang.String.format, see java.util.Formatter for format\nstring syntax",
   :var-type "function",
   :line 3856,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([& body]),
   :name "future",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4481",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/future",
   :doc
   "Takes a body of expressions and yields a future object that will\ninvoke the body in another thread, and will cache the result and\nreturn it on all subsequent calls to deref/@. If the computation has\nnot yet finished, calls to deref/@ will block.",
   :var-type "macro",
   :line 4481,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([f]),
   :name "future-call",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4466",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/future-call",
   :doc
   "Takes a function of no args and yields a future object that will\ninvoke the function in another thread, and will cache the result and\nreturn it on all subsequent calls to deref/@. If the computation has\nnot yet finished, calls to deref/@ will block.",
   :var-type "function",
   :line 4466,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([f]),
   :name "future-cancel",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4489",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/future-cancel",
   :doc "Cancels the future, if possible.",
   :var-type "function",
   :line 4489,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([f]),
   :name "future-cancelled?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4493",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/future-cancelled?",
   :doc "Returns true if future f is cancelled",
   :var-type "function",
   :line 4493,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([f]),
   :name "future-done?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4455",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/future-done?",
   :doc "Returns true if future f is done",
   :var-type "function",
   :line 4455,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "future?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4451",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/future?",
   :doc "Returns true if x is a future",
   :var-type "function",
   :line 4451,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([& options]),
   :name "gen-class",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/6109d41a975bf24b17681342591116a9897e4a27/src/clj/clojure/genclass.clj#L464",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/6109d41a975bf24b17681342591116a9897e4a27/src/clj/clojure/genclass.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/gen-class",
   :doc
   "When compiling, generates compiled bytecode for a class with the\ngiven package-qualified :name (which, as all names in these\nparameters, can be a string or symbol), and writes the .class file\nto the *compile-path* directory.  When not compiling, does\nnothing. The gen-class construct contains no implementation, as the\nimplementation will be dynamically sought by the generated class in\nfunctions in an implementing Clojure namespace. Given a generated\nclass org.mydomain.MyClass with a method named mymethod, gen-class\nwill generate an implementation that looks for a function named by \n(str prefix mymethod) (default prefix: \"-\") in a\nClojure namespace specified by :impl-ns\n(defaults to the current namespace). All inherited methods,\ngenerated methods, and init and main functions (see :methods, :init,\nand :main below) will be found similarly prefixed. By default, the\nstatic initializer for the generated class will attempt to load the\nClojure support code for the class as a resource from the classpath,\ne.g. in the example case, ``org/mydomain/MyClass__init.class``. This\nbehavior can be controlled by :load-impl-ns\n\nNote that methods with a maximum of 18 parameters are supported.\n\nIn all subsequent sections taking types, the primitive types can be\nreferred to by their Java names (int, float etc), and classes in the\njava.lang package can be used without a package qualifier. All other\nclasses must be fully qualified.\n\nOptions should be a set of key/value pairs, all except for :name are optional:\n\n:name aname\n\nThe package-qualified name of the class to be generated\n\n:extends aclass\n\nSpecifies the superclass, the non-private methods of which will be\noverridden by the class. If not provided, defaults to Object.\n\n:implements [interface ...]\n\nOne or more interfaces, the methods of which will be implemented by the class.\n\n:init name\n\nIf supplied, names a function that will be called with the arguments\nto the constructor. Must return [ [superclass-constructor-args] state] \nIf not supplied, the constructor args are passed directly to\nthe superclass constructor and the state will be nil\n\n:constructors {[param-types] [super-param-types], ...}\n\nBy default, constructors are created for the generated class which\nmatch the signature(s) of the constructors for the superclass. This\nparameter may be used to explicitly specify constructors, each entry\nproviding a mapping from a constructor signature to a superclass\nconstructor signature. When you supply this, you must supply an :init\nspecifier. \n\n:post-init name\n\nIf supplied, names a function that will be called with the object as\nthe first argument, followed by the arguments to the constructor.\nIt will be called every time an object of this class is created,\nimmediately after all the inherited constructors have completed.\nIt's return value is ignored.\n\n:methods [ [name [param-types] return-type], ...]\n\nThe generated class automatically defines all of the non-private\nmethods of its superclasses/interfaces. This parameter can be used\nto specify the signatures of additional methods of the generated\nclass. Static methods can be specified with #^{:static true} in the\nsignature's metadata. Do not repeat superclass/interface signatures\nhere.\n\n:main boolean\n\nIf supplied and true, a static public main function will be generated. It will\npass each string of the String[] argument as a separate argument to\na function called (str prefix main).\n\n:factory name\n\nIf supplied, a (set of) public static factory function(s) will be\ncreated with the given name, and the same signature(s) as the\nconstructor(s).\n\n:state name\n\nIf supplied, a public final instance field with the given name will be\ncreated. You must supply an :init function in order to provide a\nvalue for the state. Note that, though final, the state can be a ref\nor agent, supporting the creation of Java objects with transactional\nor asynchronous mutation semantics.\n\n:exposes {protected-field-name {:get name :set name}, ...}\n\nSince the implementations of the methods of the generated class\noccur in Clojure functions, they have no access to the inherited\nprotected fields of the superclass. This parameter can be used to\ngenerate public getter/setter methods exposing the protected field(s)\nfor use in the implementation.\n\n:exposes-methods {super-method-name exposed-name, ...}\n\nIt is sometimes necessary to call the superclass' implementation of an\noverridden method.  Those methods may be exposed and referred in \nthe new method implementation by a local name.\n\n:prefix string\n\nDefault: \"-\" Methods called e.g. Foo will be looked up in vars called\nprefixFoo in the implementing ns.\n\n:impl-ns name\n\nDefault: the name of the current ns. Implementations of methods will be \nlooked up in this namespace.\n\n:load-impl-ns boolean\n\nDefault: true. Causes the static initializer for the generated class\nto reference the load code for the implementing namespace. Should be\ntrue when implementing-ns is the default, false if you intend to\nload the code via some other method.",
   :var-type "macro",
   :line 464,
   :file "src/clj/clojure/genclass.clj"}
  {:arglists ([& options]),
   :name "gen-interface",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/6109d41a975bf24b17681342591116a9897e4a27/src/clj/clojure/genclass.clj#L635",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/6109d41a975bf24b17681342591116a9897e4a27/src/clj/clojure/genclass.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/gen-interface",
   :doc
   "When compiling, generates compiled bytecode for an interface with\n the given package-qualified :name (which, as all names in these\n parameters, can be a string or symbol), and writes the .class file\n to the *compile-path* directory.  When not compiling, does nothing.\n\n In all subsequent sections taking types, the primitive types can be\n referred to by their Java names (int, float etc), and classes in the\n java.lang package can be used without a package qualifier. All other\n classes must be fully qualified.\n\n Options should be a set of key/value pairs, all except for :name are\n optional:\n\n :name aname\n\n The package-qualified name of the class to be generated\n\n :extends [interface ...]\n\n One or more interfaces, which will be extended by this interface.\n\n :methods [ [name [param-types] return-type], ...]\n\n This parameter is used to specify the signatures of the methods of\n the generated interface.  Do not repeat superinterface signatures\n here.",
   :var-type "macro",
   :line 635,
   :file "src/clj/clojure/genclass.clj"}
  {:arglists ([] [prefix-string]),
   :name "gensym",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L393",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/gensym",
   :doc
   "Returns a new symbol with a unique name. If a prefix string is\nsupplied, the name is prefix# where # is some unique number. If\nprefix is not supplied, the prefix is 'G__'.",
   :var-type "function",
   :line 393,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([map key] [map key not-found]),
   :name "get",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L977",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/get",
   :doc
   "Returns the value mapped to key, not-found or nil if key not present.",
   :var-type "function",
   :line 977,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([m ks]),
   :name "get-in",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4186",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/get-in",
   :doc
   "returns the value in a nested associative structure, where ks is a sequence of keys",
   :var-type "function",
   :line 4186,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([multifn dispatch-val]),
   :name "get-method",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1172",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/get-method",
   :doc
   "Given a multimethod and a dispatch value, returns the dispatch fn\nthat would apply to that value, or nil if none apply and no default",
   :var-type "function",
   :line 1172,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([& bases]),
   :name "get-proxy-class",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/040f083efc16dd830a4508a35a04465e3e5677d3/src/clj/clojure/core_proxy.clj#L250",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/040f083efc16dd830a4508a35a04465e3e5677d3/src/clj/clojure/core_proxy.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/get-proxy-class",
   :doc
   "Takes an optional single class followed by zero or more\ninterfaces. If not supplied class defaults to Object.  Creates an\nreturns an instance of a proxy class derived from the supplied\nclasses. The resulting value is cached and used for any subsequent\nrequests for the same class set. Returns a Class object.",
   :var-type "function",
   :line 250,
   :file "src/clj/clojure/core_proxy.clj"}
  {:arglists ([]),
   :name "get-thread-bindings",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1245",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/get-thread-bindings",
   :doc
   "Get a map with the Var/value pairs which is currently in effect for the\ncurrent thread.",
   :var-type "function",
   :line 1245,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([iref]),
   :name "get-validator",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1499",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/get-validator",
   :doc "Gets the validator-fn for a var/ref/agent/atom.",
   :var-type "function",
   :line 1499,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "hash",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3483",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/hash",
   :doc "Returns the hash code of its argument",
   :var-type "function",
   :line 3483,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([] [& keyvals]),
   :name "hash-map",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L274",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/hash-map",
   :doc
   "keyval => key val\nReturns a new hash map with supplied mappings.",
   :var-type "function",
   :line 274,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([] [& keys]),
   :name "hash-set",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L281",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/hash-set",
   :doc "Returns a new hash set with supplied keys.",
   :var-type "function",
   :line 281,
   :file "src/clj/clojure/core.clj"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/identical?",
   :namespace "clojure.core",
   :var-type "function",
   :arglists ([x y]),
   :doc "Tests if 2 arguments are the same object",
   :name "identical?"}
  {:arglists ([x]),
   :name "identity",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L944",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/identity",
   :doc "Returns its argument.",
   :var-type "function",
   :line 944,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([bindings then] [bindings then else & oldform]),
   :name "if-let",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1191",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/if-let",
   :doc
   "bindings => binding-form test\n\nIf test is true, evaluates then with binding-form bound to the value of \ntest, if not, yields else",
   :var-type "macro",
   :line 1191,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([test then] [test then else]),
   :name "if-not",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L532",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/if-not",
   :doc
   "Evaluates test. If logical false, evaluates and returns then expr, \notherwise else expr, if supplied, else nil.",
   :var-type "macro",
   :line 532,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "ifn?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4229",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/ifn?",
   :doc
   "Returns true if x implements IFn. Note that many data structures\n(e.g. sets and maps) implement IFn",
   :var-type "function",
   :line 4229,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([& import-symbols-or-lists]),
   :name "import",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2163",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/import",
   :doc
   "import-list => (package-symbol class-name-symbols*)\n\nFor each name in class-name-symbols, adds a mapping from name to the\nclass named by package.name to the current namespace. Use :import in the ns\nmacro in preference to calling this directly.",
   :var-type "macro",
   :line 2163,
   :file "src/clj/clojure/core.clj"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/in-ns",
   :namespace "clojure.core",
   :var-type "function",
   :arglists ([name]),
   :doc
   "Sets *ns* to the namespace named by the symbol, creating it if needed.",
   :name "in-ns"}
  {:arglists ([x]),
   :name "inc",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L641",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/inc",
   :doc "Returns a number one greater than num.",
   :var-type "function",
   :line 641,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([proxy mappings]),
   :name "init-proxy",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/040f083efc16dd830a4508a35a04465e3e5677d3/src/clj/clojure/core_proxy.clj#L269",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/040f083efc16dd830a4508a35a04465e3e5677d3/src/clj/clojure/core_proxy.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/init-proxy",
   :doc
   "Takes a proxy instance and a map of strings (which must\ncorrespond to methods of the proxy superclass/superinterfaces) to\nfns (which must take arguments matching the corresponding method,\nplus an additional (explicit) first arg corresponding to this, and\nsets the proxy's fn map.",
   :var-type "function",
   :line 269,
   :file "src/clj/clojure/core_proxy.clj"}
  {:arglists ([c x]),
   :name "instance?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L107",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/instance?",
   :doc
   "Evaluates x and tests if it is an instance of the class\nc. Returns true or false",
   :var-type "function",
   :line 107,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "int",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L611",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/int",
   :doc "Coerce to int",
   :var-type "function",
   :line 611,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([size-or-seq] [size init-val-or-seq]),
   :name "int-array",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3576",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/int-array",
   :doc "Creates an array of ints",
   :var-type "function",
   :line 3576,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([n]),
   :name "integer?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2257",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/integer?",
   :doc "Returns true if n is an integer",
   :var-type "function",
   :line 2257,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([c1 c2] [c1 c2 & colls]),
   :name "interleave",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2778",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/interleave",
   :doc
   "Returns a lazy seq of the first item in each coll, then the second etc.",
   :var-type "function",
   :line 2778,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([ns name] [ns name val]),
   :name "intern",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4295",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/intern",
   :doc
   "Finds or creates a var named by the symbol name in the namespace\nns (which can be a symbol or a namespace), setting its root binding\nto val if supplied. The namespace must exist. The var will adopt any\nmetadata from the name symbol.  Returns the var.",
   :var-type "function",
   :line 4295,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([sep coll]),
   :name "interpose",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3487",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/interpose",
   :doc "Returns a lazy seq of the elements of coll separated by sep",
   :var-type "function",
   :line 3487,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([to from]),
   :name "into",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4667",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/into",
   :doc
   "Returns a new coll consisting of to-coll with all of the items of\nfrom-coll conjoined.",
   :var-type "function",
   :line 4667,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([aseq] [type aseq]),
   :name "into-array",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2180",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/into-array",
   :doc
   "Returns an array with components set to the values in aseq. The array's\ncomponent type is type if provided, or the type of the first value in\naseq if present, or Object. All values in aseq must be compatible with\nthe component type. Class objects for the primitive types can be obtained\nusing, e.g., Integer/TYPE.",
   :var-type "function",
   :line 2180,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([xs]),
   :name "ints",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3610",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/ints",
   :doc "Casts to int[]",
   :var-type "function",
   :line 3610,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([& body]),
   :name "io!",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1590",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/io!",
   :doc
   "If an io! block occurs in a transaction, throws an\nIllegalStateException, else runs body in an implicit do. If the\nfirst expression in body is a literal string, will use that as the\nexception message.",
   :var-type "macro",
   :line 1590,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([child parent] [h child parent]),
   :name "isa?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3697",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/isa?",
   :doc
   "Returns true if (= child parent), or child is directly or indirectly derived from\nparent, either via a Java type inheritance relationship or a\nrelationship established via derive. h must be a hierarchy obtained\nfrom make-hierarchy, if not supplied defaults to the global\nhierarchy",
   :var-type "function",
   :line 3697,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([f x]),
   :name "iterate",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1893",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/iterate",
   :doc
   "Returns a lazy sequence of x, (f x), (f (f x)) etc. f must be free of side-effects",
   :var-type "function",
   :line 1893,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([iter]),
   :name "iterator-seq",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3845",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/iterator-seq",
   :doc
   "Returns a seq on a java.util.Iterator. Note that most collections\nproviding iterators implement Iterable and thus support seq directly.",
   :var-type "function",
   :line 3845,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([f] [f g] [f g h] [f g h & fs]),
   :name "juxt",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1633",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/juxt",
   :doc
   "Alpha - name subject to change.\nTakes a set of functions and returns a fn that is the juxtaposition\nof those fns.  The returned fn takes a variable number of args, and\nreturns a vector containing the result of applying each fn to the\nargs (left-to-right).\n((juxt a b c) x) => [(a x) (b x) (c x)]",
   :var-type "function",
   :line 1633,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([e]),
   :name "key",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1033",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/key",
   :doc "Returns the key of the map entry.",
   :var-type "function",
   :line 1033,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([map]),
   :name "keys",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1025",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/keys",
   :doc "Returns a sequence of the map's keys.",
   :var-type "function",
   :line 1025,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([name] [ns name]),
   :name "keyword",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L386",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/keyword",
   :doc
   "Returns a Keyword with the given namespace and name.  Do not use :\nin the keyword strings, it will be added automatically.",
   :var-type "function",
   :line 386,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "keyword?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L376",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/keyword?",
   :doc "Return true if x is a Keyword",
   :var-type "function",
   :line 376,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([coll]),
   :name "last",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L189",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/last",
   :doc "Return the last item in coll, in linear time",
   :var-type "function",
   :line 189,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([& colls]),
   :name "lazy-cat",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3004",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/lazy-cat",
   :doc
   "Expands to code which yields a lazy sequence of the concatenation\nof the supplied colls.  Each coll expr is not evaluated until it is\nneeded. \n\n(lazy-cat xs ys zs) === (concat (lazy-seq xs) (lazy-seq ys) (lazy-seq zs))",
   :var-type "macro",
   :line 3004,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([& body]),
   :name "lazy-seq",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L454",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/lazy-seq",
   :doc
   "Takes a body of expressions that returns an ISeq or nil, and yields\na Seqable object that will invoke the body only the first time seq\nis called, and will cache the result and return it on all subsequent\nseq calls.",
   :var-type "macro",
   :line 454,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([bindings & body]),
   :name "let",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2902",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/let",
   :doc
   "Evaluates the exprs in a lexical context in which the symbols in\nthe binding-forms are bound to their respective init-exprs or parts\ntherein.",
   :var-type "macro",
   :line 2902,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([fnspecs & body]),
   :name "letfn",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4531",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/letfn",
   :doc
   "Takes a vector of function specs and a body, and generates a set of\nbindings of functions to their names. All of the names are available\nin all of the definitions of the functions, as well as the body.\n\nfnspec ==> (fname [params*] exprs) or (fname ([params*] exprs)+)",
   :var-type "macro",
   :line 4531,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([rdr]),
   :name "line-seq",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1954",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/line-seq",
   :doc
   "Returns the lines of text from rdr as a lazy sequence of strings.\nrdr must implement java.io.BufferedReader.",
   :var-type "function",
   :line 1954,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([& items]),
   :name "list",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L16",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/list",
   :doc "Creates a new list containing the items.",
   :var-type "function",
   :line 16,
   :file "src/clj/clojure/core.clj"}
  {:arglists
   ([args] [a args] [a b args] [a b c args] [a b c d & more]),
   :name "list*",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L422",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/list*",
   :doc
   "Creates a new list containing the items prepended to the rest, the\nlast of which will be treated as a sequence.",
   :var-type "function",
   :line 422,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "list?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4221",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/list?",
   :doc "Returns true if x implements IPersistentList",
   :var-type "function",
   :line 4221,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([& paths]),
   :name "load",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4154",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/load",
   :doc
   "Loads Clojure code from resources in classpath. A path is interpreted as\nclasspath-relative if it begins with a slash or relative to the root\ndirectory for the current namespace otherwise.",
   :var-type "function",
   :line 4154,
   :file "src/clj/clojure/core.clj"}
  {:file nil,
   :raw-source-url nil,
   :source-url nil,
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/load-file",
   :namespace "clojure.core",
   :var-type "function",
   :arglists ([name]),
   :doc
   "Sequentially read and evaluate the set of forms contained in the file.",
   :name "load-file"}
  {:arglists ([rdr]),
   :name "load-reader",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2621",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/load-reader",
   :doc
   "Sequentially read and evaluate the set of forms contained in the\nstream/file",
   :var-type "function",
   :line 2621,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([s]),
   :name "load-string",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2626",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/load-string",
   :doc
   "Sequentially read and evaluate the set of forms contained in the\nstring",
   :var-type "function",
   :line 2626,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([]),
   :name "loaded-libs",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4150",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/loaded-libs",
   :doc
   "Returns a sorted set of symbols naming the currently loaded libs",
   :var-type "function",
   :line 4150,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x & body]),
   :name "locking",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1061",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/locking",
   :doc
   "Executes exprs in an implicit do, while holding the monitor of x.\nWill release the monitor of x in all circumstances.",
   :var-type "macro",
   :line 1061,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "long",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2210",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/long",
   :doc "Coerce to long",
   :var-type "function",
   :line 2210,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([size-or-seq] [size init-val-or-seq]),
   :name "long-array",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3583",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/long-array",
   :doc "Creates an array of longs",
   :var-type "function",
   :line 3583,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([xs]),
   :name "longs",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3618",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/longs",
   :doc "Casts to long[]",
   :var-type "function",
   :line 3618,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([bindings & body]),
   :name "loop",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2967",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/loop",
   :doc
   "Evaluates the exprs in a lexical context in which the symbols in\nthe binding-forms are bound to their respective init-exprs or parts\ntherein. Acts as a recur target.",
   :var-type "macro",
   :line 2967,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([form]),
   :name "macroexpand",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2577",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/macroexpand",
   :doc
   "Repeatedly calls macroexpand-1 on form until it no longer\nrepresents a macro form, then returns it.  Note neither\nmacroexpand-1 nor macroexpand expand macros in subforms.",
   :var-type "function",
   :line 2577,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([form]),
   :name "macroexpand-1",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2571",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/macroexpand-1",
   :doc
   "If form represents a macro form, returns its expansion,\nelse returns form.",
   :var-type "function",
   :line 2571,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([type len] [type dim & more-dims]),
   :name "make-array",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2543",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/make-array",
   :doc
   "Creates and returns an array of instances of the specified class of\nthe specified dimension(s).  Note that a class object is required.\nClass objects can be obtained by using their imported or\nfully-qualified name.  Class objects for the primitive types can be\nobtained using, e.g., Integer/TYPE.",
   :var-type "function",
   :line 2543,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([]),
   :name "make-hierarchy",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3669",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/make-hierarchy",
   :doc "Creates a hierarchy object for use with derive, isa? etc.",
   :var-type "function",
   :line 3669,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([f coll] [f c1 c2] [f c1 c2 c3] [f c1 c2 c3 & colls]),
   :name "map",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1746",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/map",
   :doc
   "Returns a lazy sequence consisting of the result of applying f to the\nset of first items of each coll, followed by applying f to the set\nof second items in each coll, until any one of the colls is\nexhausted.  Any remaining items in other colls are ignored. Function\nf should accept number-of-colls arguments.",
   :var-type "function",
   :line 1746,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "map?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L128",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/map?",
   :doc "Return true if x implements IPersistentMap",
   :var-type "function",
   :line 128,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([f & colls]),
   :name "mapcat",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1783",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/mapcat",
   :doc
   "Returns the result of applying concat to the result of applying map\nto f and colls.  Thus function f should return a collection.",
   :var-type "function",
   :line 1783,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x] [x y] [x y & more]),
   :name "max",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L772",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/max",
   :doc "Returns the greatest of the nums.",
   :var-type "function",
   :line 772,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([k x] [k x y] [k x y & more]),
   :name "max-key",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3364",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/max-key",
   :doc "Returns the x for which (k x), a number, is greatest.",
   :var-type "function",
   :line 3364,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([name & args]),
   :name "memfn",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2445",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/memfn",
   :doc
   "Expands into code that creates a fn that expects to be passed an\nobject and any args and calls the named instance method on the\nobject passing the args. Use when you want to treat a Java method as\na first-class fn.",
   :var-type "macro",
   :line 2445,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([f]),
   :name "memoize",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4318",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/memoize",
   :doc
   "Returns a memoized version of a referentially transparent function. The\nmemoized version of the function keeps a cache of the mapping from arguments\nto results and, when calls with the same arguments are repeated often, has\nhigher performance at the expense of higher memory use.",
   :var-type "function",
   :line 4318,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([& maps]),
   :name "merge",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1916",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/merge",
   :doc
   "Returns a map that consists of the rest of the maps conj-ed onto\nthe first.  If a key occurs in more than one map, the mapping from\nthe latter (left-to-right) will be the mapping in the result.",
   :var-type "function",
   :line 1916,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([f & maps]),
   :name "merge-with",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1924",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/merge-with",
   :doc
   "Returns a map that consists of the rest of the maps conj-ed onto\nthe first.  If a key occurs in more than one map, the mapping(s)\nfrom the latter (left-to-right) will be combined with the mapping in\nthe result by calling (f val-in-result val-in-latter).",
   :var-type "function",
   :line 1924,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([obj]),
   :name "meta",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L154",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/meta",
   :doc
   "Returns the metadata of obj, returns nil if there is no metadata.",
   :var-type "function",
   :line 154,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([multifn]),
   :name "methods",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1168",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/methods",
   :doc
   "Given a multimethod, returns a map of dispatch values -> dispatch fns",
   :var-type "function",
   :line 1168,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x] [x y] [x y & more]),
   :name "min",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L779",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/min",
   :doc "Returns the least of the nums.",
   :var-type "function",
   :line 779,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([k x] [k x y] [k x y & more]),
   :name "min-key",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3371",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/min-key",
   :doc "Returns the x for which (k x), a number, is least.",
   :var-type "function",
   :line 3371,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([num div]),
   :name "mod",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2266",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/mod",
   :doc "Modulus of num and div. Truncates toward negative infinity.",
   :var-type "function",
   :line 2266,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "name",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1049",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/name",
   :doc "Returns the name String of a symbol or keyword.",
   :var-type "function",
   :line 1049,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "namespace",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1055",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/namespace",
   :doc
   "Returns the namespace String of a symbol or keyword, or nil if not present.",
   :var-type "function",
   :line 1055,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "neg?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L845",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/neg?",
   :doc "Returns true if num is less than zero, else false",
   :var-type "function",
   :line 845,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([]),
   :name "newline",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2338",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/newline",
   :doc
   "Writes a newline to the output stream that is the current value of\n*out*",
   :var-type "function",
   :line 2338,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([coll]),
   :name "next",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L47",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/next",
   :doc
   "Returns a seq of the items after the first. Calls seq on its\nargument.  If there are no more items, returns nil.",
   :var-type "function",
   :line 47,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "nfirst",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L83",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/nfirst",
   :doc "Same as (next (first x))",
   :var-type "function",
   :line 83,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "nil?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L336",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/nil?",
   :doc "Returns true if x is nil, false otherwise.",
   :var-type "function",
   :line 336,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "nnext",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L93",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/nnext",
   :doc "Same as (next (next x))",
   :var-type "function",
   :line 93,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "not",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L351",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/not",
   :doc "Returns true if x is logical false, false otherwise.",
   :var-type "function",
   :line 351,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([pred coll]),
   :name "not-any?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1724",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/not-any?",
   :doc
   "Returns false if (pred x) is logical true for any x in coll,\nelse true.",
   :var-type "function",
   :line 1724,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([coll]),
   :name "not-empty",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3676",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/not-empty",
   :doc "If coll is empty, returns nil, else coll",
   :var-type "function",
   :line 3676,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([pred coll]),
   :name "not-every?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1708",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/not-every?",
   :doc
   "Returns false if (pred x) is logical true for every x in\ncoll, else true.",
   :var-type "function",
   :line 1708,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x] [x y] [x y & more]),
   :name "not=",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L557",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/not=",
   :doc "Same as (not (= obj1 obj2))",
   :var-type "function",
   :line 557,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([name & references]),
   :name "ns",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3879",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/ns",
   :doc
   "Sets *ns* to the namespace named by name (unevaluated), creating it\nif needed.  references can be zero or more of: (:refer-clojure ...)\n(:require ...) (:use ...) (:import ...) (:load ...) (:gen-class)\nwith the syntax of refer-clojure/require/use/import/load/gen-class\nrespectively, except the arguments are unevaluated and need not be\nquoted. (:gen-class ...), when supplied, defaults to :name\ncorresponding to the ns name, :main true, :impl-ns same as ns, and\n:init-impl-ns true. All options of gen-class are\nsupported. The :gen-class directive is ignored when not\ncompiling. If :gen-class is not supplied, when compiled only an\nnsname__init.class will be generated. If :refer-clojure is not used, a\ndefault (refer 'clojure) is used.  Use of ns is preferred to\nindividual calls to in-ns/require/use/import:\n\n(ns foo.bar\n  (:refer-clojure :exclude [ancestors printf])\n  (:require (clojure.contrib sql sql.tests))\n  (:use (my.lib this that))\n  (:import (java.util Date Timer Random)\n            (java.sql Connection Statement)))",
   :var-type "macro",
   :line 3879,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([ns]),
   :name "ns-aliases",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2761",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/ns-aliases",
   :doc "Returns a map of the aliases for the namespace.",
   :var-type "function",
   :line 2761,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([ns]),
   :name "ns-imports",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2703",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/ns-imports",
   :doc "Returns a map of the import mappings for the namespace.",
   :var-type "function",
   :line 2703,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([ns]),
   :name "ns-interns",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2745",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/ns-interns",
   :doc "Returns a map of the intern mappings for the namespace.",
   :var-type "function",
   :line 2745,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([ns]),
   :name "ns-map",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2680",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/ns-map",
   :doc "Returns a map of all the mappings for the namespace.",
   :var-type "function",
   :line 2680,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([ns]),
   :name "ns-name",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2675",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/ns-name",
   :doc "Returns the name of the namespace, a symbol.",
   :var-type "function",
   :line 2675,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([ns]),
   :name "ns-publics",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2694",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/ns-publics",
   :doc
   "Returns a map of the public intern mappings for the namespace.",
   :var-type "function",
   :line 2694,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([ns]),
   :name "ns-refers",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2737",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/ns-refers",
   :doc "Returns a map of the refer mappings for the namespace.",
   :var-type "function",
   :line 2737,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([ns sym]),
   :name "ns-resolve",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2819",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/ns-resolve",
   :doc
   "Returns the var or Class to which a symbol will be resolved in the\nnamespace, else nil.  Note that if the symbol is fully qualified,\nthe var/Class to which it resolves need not be present in the\nnamespace.",
   :var-type "function",
   :line 2819,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([ns sym]),
   :name "ns-unalias",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2766",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/ns-unalias",
   :doc "Removes the alias for the symbol from the namespace.",
   :var-type "function",
   :line 2766,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([ns sym]),
   :name "ns-unmap",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2685",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/ns-unmap",
   :doc "Removes the mappings for the symbol from the namespace.",
   :var-type "function",
   :line 2685,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([coll index] [coll index not-found]),
   :name "nth",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L617",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/nth",
   :doc
   "Returns the value at the index. get returns nil if index out of\nbounds, nth throws an exception unless not-found is supplied.  nth\nalso works for strings, Java arrays, regex Matchers and Lists, and,\nin O(n) time, for sequences.",
   :var-type "function",
   :line 617,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([coll n]),
   :name "nthnext",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2836",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/nthnext",
   :doc "Returns the nth next of coll, (seq coll) when n is 0.",
   :var-type "function",
   :line 2836,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "num",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2204",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/num",
   :doc "Coerce to Number",
   :var-type "function",
   :line 2204,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "number?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2252",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/number?",
   :doc "Returns true if x is a Number",
   :var-type "function",
   :line 2252,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([n]),
   :name "odd?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L923",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/odd?",
   :doc
   "Returns true if n is odd, throws an exception if n is not an integer",
   :var-type "function",
   :line 923,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([] [x] [x & next]),
   :name "or",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L588",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/or",
   :doc
   "Evaluates exprs one at a time, from left to right. If a form\nreturns a logical true value, or returns that value and doesn't\nevaluate any of the other expressions, otherwise it returns the\nvalue of the last expression. (or) returns nil.",
   :var-type "macro",
   :line 588,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([tag] [h tag]),
   :name "parents",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3717",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/parents",
   :doc
   "Returns the immediate parents of tag, either via a Java type\ninheritance relationship or a relationship established via derive. h\nmust be a hierarchy obtained from make-hierarchy, if not supplied\ndefaults to the global hierarchy",
   :var-type "function",
   :line 3717,
   :file "src/clj/clojure/core.clj"}
  {:arglists
   ([f arg1]
    [f arg1 arg2]
    [f arg1 arg2 arg3]
    [f arg1 arg2 arg3 & more]),
   :name "partial",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1670",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/partial",
   :doc
   "Takes a function f and fewer than the normal arguments to f, and\nreturns a fn that takes a variable number of additional args. When\ncalled, the returned function calls f with args + additional args.",
   :var-type "function",
   :line 1670,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([n coll] [n step coll] [n step pad coll]),
   :name "partition",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1991",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/partition",
   :doc
   "Returns a lazy sequence of lists of n items each, at offsets step\napart. If step is not supplied, defaults to n, i.e. the partitions\ndo not overlap. If a pad collection is supplied, use its elements as\nnecessary to complete last partition upto n items. In case there are\nnot enough padding elements, return a partition with less than n items.",
   :var-type "function",
   :line 1991,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([& fns]),
   :name "pcalls",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4520",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/pcalls",
   :doc
   "Executes the no-arg fns in parallel, returning a lazy sequence of\ntheir values",
   :var-type "function",
   :line 4520,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([coll]),
   :name "peek",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L955",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/peek",
   :doc
   "For a list or queue, same as first, for a vector, same as, but much\nmore efficient than, last. If the collection is empty, returns nil.",
   :var-type "function",
   :line 955,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([coll]),
   :name "persistent!",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4609",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/persistent!",
   :doc
   "Alpha - subject to change.\nReturns a new, persistent version of the transient collection, in\nconstant time. The transient collection cannot be used after this\ncall, any such use will throw an exception.",
   :var-type "function",
   :line 4609,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([f coll] [f coll & colls]),
   :name "pmap",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4497",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/pmap",
   :doc
   "Like map, except f is applied in parallel. Semi-lazy in that the\nparallel computation stays ahead of the consumption, but doesn't\nrealize the entire result unless required. Only useful for\ncomputationally intensive functions where the time of f dominates\nthe coordination overhead.",
   :var-type "function",
   :line 4497,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([coll]),
   :name "pop",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L960",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/pop",
   :doc
   "For a list or queue, returns a new list/queue without the first\nitem, for a vector, returns a new vector without the last item. If\nthe collection is empty, throws an exception.  Note - not the same\nas next/butlast.",
   :var-type "function",
   :line 960,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([coll]),
   :name "pop!",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4646",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/pop!",
   :doc
   "Alpha - subject to change.\nRemoves the last item from a transient vector. If\nthe collection is empty, throws an exception. Returns coll",
   :var-type "function",
   :line 4646,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([]),
   :name "pop-thread-bindings",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1239",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/pop-thread-bindings",
   :doc
   "Pop one set of bindings pushed with push-binding before. It is an error to\npop bindings without pushing before.",
   :var-type "function",
   :line 1239,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "pos?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L839",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/pos?",
   :doc "Returns true if num is greater than zero, else false",
   :var-type "function",
   :line 839,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([] [x] [x & more]),
   :name "pr",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2325",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/pr",
   :doc
   "Prints the object(s) to the output stream that is the current value\nof *out*.  Prints the object(s), separated by spaces if there is\nmore than one.  By default, pr and prn print in a way that objects\ncan be read by the reader",
   :var-type "function",
   :line 2325,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([& xs]),
   :name "pr-str",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3121",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/pr-str",
   :doc "pr to a string, returning it",
   :var-type "function",
   :line 3121,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([multifn dispatch-val-x dispatch-val-y]),
   :name "prefer-method",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1162",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/prefer-method",
   :doc
   "Causes the multimethod to prefer matches of dispatch-val-x over dispatch-val-y \nwhen there is a conflict",
   :var-type "function",
   :line 1162,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([multifn]),
   :name "prefers",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1177",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/prefers",
   :doc
   "Given a multimethod, returns a map of preferred value -> set of other values",
   :var-type "function",
   :line 1177,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([& more]),
   :name "print",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2360",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/print",
   :doc
   "Prints the object(s) to the output stream that is the current value\nof *out*.  print and println produce output for human consumption.",
   :var-type "function",
   :line 2360,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([nspace]),
   :name "print-namespace-doc",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3281",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/print-namespace-doc",
   :doc "Print the documentation string of a Namespace.",
   :var-type "function",
   :line 3281,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([& xs]),
   :name "print-str",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3135",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/print-str",
   :doc "print to a string, returning it",
   :var-type "function",
   :line 3135,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([fmt & args]),
   :name "printf",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3863",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/printf",
   :doc "Prints formatted output, as per format",
   :var-type "function",
   :line 3863,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([& more]),
   :name "println",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2367",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/println",
   :doc "Same as print followed by (newline)",
   :var-type "function",
   :line 2367,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([& xs]),
   :name "println-str",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3142",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/println-str",
   :doc "println to a string, returning it",
   :var-type "function",
   :line 3142,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([& more]),
   :name "prn",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2352",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/prn",
   :doc
   "Same as pr followed by (newline). Observes *flush-on-newline*",
   :var-type "function",
   :line 2352,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([& xs]),
   :name "prn-str",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3128",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/prn-str",
   :doc "prn to a string, returning it",
   :var-type "function",
   :line 3128,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([]),
   :name "promise",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4577",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/promise",
   :doc
   "Alpha - subject to change.\nReturns a promise object that can be read with deref/@, and set,\nonce only, with deliver. Calls to deref/@ prior to delivery will\nblock. All subsequent derefs will return the same delivered value\nwithout blocking.",
   :var-type "function",
   :line 4577,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([class-and-interfaces args & fs]),
   :name "proxy",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/040f083efc16dd830a4508a35a04465e3e5677d3/src/clj/clojure/core_proxy.clj#L295",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/040f083efc16dd830a4508a35a04465e3e5677d3/src/clj/clojure/core_proxy.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/proxy",
   :doc
   "class-and-interfaces - a vector of class names\n\nargs - a (possibly empty) vector of arguments to the superclass\nconstructor.\n\nf => (name [params*] body) or\n(name ([params*] body) ([params+] body) ...)\n\nExpands to code which creates a instance of a proxy class that\nimplements the named class/interface(s) by calling the supplied\nfns. A single class, if provided, must be first. If not provided it\ndefaults to Object.\n\nThe interfaces names must be valid interface types. If a method fn\nis not provided for a class method, the superclass methd will be\ncalled. If a method fn is not provided for an interface method, an\nUnsupportedOperationException will be thrown should it be\ncalled. Method fns are closures and can capture the environment in\nwhich proxy is called. Each method fn takes an additional implicit\nfirst arg, which is bound to 'this. Note that while method fns can\nbe provided to override protected methods, they have no other access\nto protected members, nor to super, as these capabilities cannot be\nproxied.",
   :var-type "macro",
   :line 295,
   :file "src/clj/clojure/core_proxy.clj"}
  {:arglists ([proxy]),
   :name "proxy-mappings",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/040f083efc16dd830a4508a35a04465e3e5677d3/src/clj/clojure/core_proxy.clj#L290",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/040f083efc16dd830a4508a35a04465e3e5677d3/src/clj/clojure/core_proxy.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/proxy-mappings",
   :doc "Takes a proxy instance and returns the proxy's fn map.",
   :var-type "function",
   :line 290,
   :file "src/clj/clojure/core_proxy.clj"}
  {:arglists ([meth & args]),
   :name "proxy-super",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/040f083efc16dd830a4508a35a04465e3e5677d3/src/clj/clojure/core_proxy.clj#L354",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/040f083efc16dd830a4508a35a04465e3e5677d3/src/clj/clojure/core_proxy.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/proxy-super",
   :doc
   "Use to call a superclass method in the body of a proxy method. \nNote, expansion captures 'this",
   :var-type "macro",
   :line 354,
   :file "src/clj/clojure/core_proxy.clj"}
  {:arglists ([bindings]),
   :name "push-thread-bindings",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1223",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/push-thread-bindings",
   :doc
   "WARNING: This is a low-level function. Prefer high-level macros like\nbinding where ever possible.\n\nTakes a map of Var/value pairs. Binds each Var to the associated value for\nthe current thread. Each call *MUST* be accompanied by a matching call to\npop-thread-bindings wrapped in a try-finally!\n\n    (push-thread-bindings bindings)\n    (try\n      ...\n      (finally\n        (pop-thread-bindings)))",
   :var-type "function",
   :line 1223,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([& exprs]),
   :name "pvalues",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4525",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/pvalues",
   :doc
   "Returns a lazy sequence of the values of the exprs, which are\nevaluated in parallel",
   :var-type "macro",
   :line 4525,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([num div]),
   :name "quot",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L851",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/quot",
   :doc "quot[ient] of dividing numerator by denominator.",
   :var-type "function",
   :line 851,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([] [n]),
   :name "rand",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3226",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/rand",
   :doc
   "Returns a random floating point number between 0 (inclusive) and\nn (default 1) (exclusive).",
   :var-type "function",
   :line 3226,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([n]),
   :name "rand-int",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3232",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/rand-int",
   :doc
   "Returns a random integer between 0 (inclusive) and n (exclusive).",
   :var-type "function",
   :line 3232,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([end] [start end] [start end step]),
   :name "range",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1897",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/range",
   :doc
   "Returns a lazy seq of nums from start (inclusive) to end\n(exclusive), by step, where start defaults to 0 and step to 1.",
   :var-type "function",
   :line 1897,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([n]),
   :name "ratio?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2274",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/ratio?",
   :doc "Returns true if n is a Ratio",
   :var-type "function",
   :line 2274,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([num]),
   :name "rationalize",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L861",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/rationalize",
   :doc "returns the rational value of num",
   :var-type "function",
   :line 861,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([m] [re s]),
   :name "re-find",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3215",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/re-find",
   :doc
   "Returns the next regex match, if any, of string to pattern, using\njava.util.regex.Matcher.find().  Uses re-groups to return the\ngroups.",
   :var-type "function",
   :line 3215,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([m]),
   :name "re-groups",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3181",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/re-groups",
   :doc
   "Returns the groups from the most recent match/find. If there are no\nnested groups, returns a string of the entire match. If there are\nnested groups, returns a vector of the groups, the first element\nbeing the entire match.",
   :var-type "function",
   :line 3181,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([re s]),
   :name "re-matcher",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3174",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/re-matcher",
   :doc
   "Returns an instance of java.util.regex.Matcher, for use, e.g. in\nre-find.",
   :var-type "function",
   :line 3174,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([re s]),
   :name "re-matches",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3205",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/re-matches",
   :doc
   "Returns the match, if any, of string to pattern, using\njava.util.regex.Matcher.matches().  Uses re-groups to return the\ngroups.",
   :var-type "function",
   :line 3205,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([s]),
   :name "re-pattern",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3166",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/re-pattern",
   :doc
   "Returns an instance of java.util.regex.Pattern, for use, e.g. in\nre-matcher.",
   :var-type "function",
   :line 3166,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([re s]),
   :name "re-seq",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3195",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/re-seq",
   :doc
   "Returns a lazy sequence of successive matches of pattern in string,\nusing java.util.regex.Matcher.find(), each such match processed with\nre-groups.",
   :var-type "function",
   :line 3195,
   :file "src/clj/clojure/core.clj"}
  {:arglists
   ([]
    [stream]
    [stream eof-error? eof-value]
    [stream eof-error? eof-value recursive?]),
   :name "read",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2374",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/read",
   :doc
   "Reads the next object from stream, which must be an instance of\njava.io.PushbackReader or some derivee.  stream defaults to the\ncurrent value of *in* .",
   :var-type "function",
   :line 2374,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([]),
   :name "read-line",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2387",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/read-line",
   :doc
   "Reads the next line from stream that is the current value of *in* .",
   :var-type "function",
   :line 2387,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([s]),
   :name "read-string",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2394",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/read-string",
   :doc "Reads one object from the string s",
   :var-type "function",
   :line 2394,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([f coll] [f val coll]),
   :name "reduce",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L646",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/reduce",
   :doc
   "f should be a function of 2 arguments. If val is not supplied,\nreturns the result of applying f to the first 2 items in coll, then\napplying f to that result and the 3rd item, etc. If coll contains no\nitems, f must accept no arguments as well, and reduce returns the\nresult of calling f with no arguments.  If coll has only 1 item, it\nis returned and f is not called.  If val is supplied, returns the\nresult of applying f to val and the first item in coll, then\napplying f to that result and the 2nd item, etc. If coll contains no\nitems, returns val and f is not called.",
   :var-type "function",
   :line 646,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x] [x & options]),
   :name "ref",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1411",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/ref",
   :doc
   "Creates and returns a Ref with an initial value of x and zero or\nmore options (in any order):\n\n:meta metadata-map\n\n:validator validate-fn\n\n:min-history (default 0)\n:max-history (default 10)\n\nIf metadata-map is supplied, it will be come the metadata on the\nref. validate-fn must be nil or a side-effect-free fn of one\nargument, which will be passed the intended new state on any state\nchange. If the new state is unacceptable, the validate-fn should\nreturn false or throw an exception. validate-fn will be called on\ntransaction commit, when all refs have their final values.\n\nNormally refs accumulate history dynamically as needed to deal with\nread demands. If you know in advance you will need history you can\nset :min-history to ensure it will be available when first needed (instead\nof after a read fault). History is limited, and the limit can be set\nwith :max-history.",
   :var-type "function",
   :line 1411,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([ref]),
   :name "ref-history-count",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1550",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/ref-history-count",
   :doc "Returns the history count of a ref",
   :var-type "function",
   :line 1550,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([ref] [ref n]),
   :name "ref-max-history",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1562",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/ref-max-history",
   :doc
   "Gets the max-history of a ref, or sets it and returns the ref",
   :var-type "function",
   :line 1562,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([ref] [ref n]),
   :name "ref-min-history",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1555",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/ref-min-history",
   :doc
   "Gets the min-history of a ref, or sets it and returns the ref",
   :var-type "function",
   :line 1555,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([ref val]),
   :name "ref-set",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1544",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/ref-set",
   :doc
   "Must be called in a transaction. Sets the value of ref.\nReturns val.",
   :var-type "function",
   :line 1544,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([ns-sym & filters]),
   :name "refer",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2708",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/refer",
   :doc
   "refers to all public vars of ns, subject to filters.\nfilters can include at most one each of:\n\n:exclude list-of-symbols\n:only list-of-symbols\n:rename map-of-fromsymbol-tosymbol\n\nFor each public interned var in the namespace named by the symbol,\nadds a mapping from the name of the var to the var to the current\nnamespace.  Throws an exception if name is already mapped to\nsomething else in the current namespace. Filters can be used to\nselect a subset, via inclusion or exclusion, or to provide a mapping\nto a symbol different from the var's name, in order to prevent\nclashes. Use :use in the ns macro in preference to calling this directly.",
   :var-type "function",
   :line 2708,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([& filters]),
   :name "refer-clojure",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3927",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/refer-clojure",
   :doc "Same as (refer 'clojure.core <filters>)",
   :var-type "macro",
   :line 3927,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([]),
   :name "release-pending-sends",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1362",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/release-pending-sends",
   :doc
   "Normally, actions sent directly or indirectly during another action\nare held until the action completes (changes the agent's\nstate). This function can be used to dispatch any pending sent\nactions immediately. This has no impact on actions sent during a\ntransaction, which are still held until commit. If no action is\noccurring, does nothing. Returns the number of actions dispatched.",
   :var-type "function",
   :line 1362,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([num div]),
   :name "rem",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L856",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/rem",
   :doc "remainder of dividing numerator by denominator.",
   :var-type "function",
   :line 856,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([pred coll]),
   :name "remove",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1809",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/remove",
   :doc
   "Returns a lazy sequence of the items in coll for which\n(pred item) returns false. pred must be free of side-effects.",
   :var-type "function",
   :line 1809,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([multifn dispatch-val]),
   :name "remove-method",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1157",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/remove-method",
   :doc
   "Removes the method of multimethod associated with dispatch-value.",
   :var-type "function",
   :line 1157,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([sym]),
   :name "remove-ns",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2657",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/remove-ns",
   :doc
   "Removes the namespace named by the symbol. Use with caution.\nCannot be used to remove the clojure namespace.",
   :var-type "function",
   :line 2657,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([reference key]),
   :name "remove-watch",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1388",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/remove-watch",
   :doc
   "Alpha - subject to change.\nRemoves a watch (set by add-watch) from a reference",
   :var-type "function",
   :line 1388,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x] [n x]),
   :name "repeat",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1884",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/repeat",
   :doc
   "Returns a lazy (infinite!, or length n if supplied) sequence of xs.",
   :var-type "function",
   :line 1884,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([f]),
   :name "repeatedly",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3467",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/repeatedly",
   :doc
   "Takes a function of no args, presumably with side effects, and returns an infinite\nlazy sequence of calls to it",
   :var-type "function",
   :line 3467,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([smap coll]),
   :name "replace",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3393",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/replace",
   :doc
   "Given a map of replacement pairs and a vector/collection, returns a\nvector/seq with any elements = a key in smap replaced with the\ncorresponding val in smap",
   :var-type "function",
   :line 3393,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([n x]),
   :name "replicate",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1889",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/replicate",
   :doc "Returns a lazy seq of n xs.",
   :var-type "function",
   :line 1889,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([& args]),
   :name "require",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4079",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/require",
   :doc
   "Loads libs, skipping any that are already loaded. Each argument is\neither a libspec that identifies a lib, a prefix list that identifies\nmultiple libs whose names share a common prefix, or a flag that modifies\nhow all the identified libs are loaded. Use :require in the ns macro\nin preference to calling this directly.\n\nLibs\n\nA 'lib' is a named set of resources in classpath whose contents define a\nlibrary of Clojure code. Lib names are symbols and each lib is associated\nwith a Clojure namespace and a Java package that share its name. A lib's\nname also locates its root directory within classpath using Java's\npackage name to classpath-relative path mapping. All resources in a lib\nshould be contained in the directory structure under its root directory.\nAll definitions a lib makes should be in its associated namespace.\n\n'require loads a lib by loading its root resource. The root resource path\nis derived from the lib name in the following manner:\nConsider a lib named by the symbol 'x.y.z; it has the root directory\n<classpath>/x/y/, and its root resource is <classpath>/x/y/z.clj. The root\nresource should contain code to create the lib's namespace (usually by using\nthe ns macro) and load any additional lib resources.\n\nLibspecs\n\nA libspec is a lib name or a vector containing a lib name followed by\noptions expressed as sequential keywords and arguments.\n\nRecognized options: :as\n:as takes a symbol as its argument and makes that symbol an alias to the\n  lib's namespace in the current namespace.\n\nPrefix Lists\n\nIt's common for Clojure code to depend on several libs whose names have\nthe same prefix. When specifying libs, prefix lists can be used to reduce\nrepetition. A prefix list contains the shared prefix followed by libspecs\nwith the shared prefix removed from the lib names. After removing the\nprefix, the names that remain must not contain any periods.\n\nFlags\n\nA flag is a keyword.\nRecognized flags: :reload, :reload-all, :verbose\n:reload forces loading of all the identified libs even if they are\n  already loaded\n:reload-all implies :reload and also forces loading of all libs that the\n  identified libs directly or indirectly load via require or use\n:verbose triggers printing information about each load, alias, and refer\n\nExample:\n\nThe following would load the libraries clojure.zip and clojure.set\nabbreviated as 's'.\n\n(require '(clojure zip [set :as s]))",
   :var-type "function",
   :line 4079,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([atom newval]),
   :name "reset!",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1485",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/reset!",
   :doc
   "Sets the value of atom to newval without regard for the\ncurrent value. Returns newval.",
   :var-type "function",
   :line 1485,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([iref metadata-map]),
   :name "reset-meta!",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1511",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/reset-meta!",
   :doc
   "Atomically resets the metadata for a namespace/var/ref/agent/atom",
   :var-type "function",
   :line 1511,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([sym]),
   :name "resolve",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2827",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/resolve",
   :doc "same as (ns-resolve *ns* symbol)",
   :var-type "function",
   :line 2827,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([coll]),
   :name "rest",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L54",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/rest",
   :doc
   "Returns a possibly empty seq of the items after the first. Calls seq on its\nargument.",
   :var-type "function",
   :line 54,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([rs]),
   :name "resultset-seq",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3827",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/resultset-seq",
   :doc
   "Creates and returns a lazy sequence of structmaps corresponding to\nthe rows in the java.sql.ResultSet rs",
   :var-type "function",
   :line 3827,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([coll]),
   :name "reverse",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L671",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/reverse",
   :doc
   "Returns a seq of the items in coll in reverse order. Not lazy.",
   :var-type "function",
   :line 671,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([coll]),
   :name "reversible?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4255",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/reversible?",
   :doc "Returns true if coll implements Reversible",
   :var-type "function",
   :line 4255,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([rev]),
   :name "rseq",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1043",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/rseq",
   :doc
   "Returns, in constant time, a seq of the items in rev (which\ncan be a vector or sorted-map), in reverse order. If rev is empty returns nil",
   :var-type "function",
   :line 1043,
   :file "src/clj/clojure/core.clj"}
  {:arglists
   ([sc test key] [sc start-test start-key end-test end-key]),
   :name "rsubseq",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3452",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/rsubseq",
   :doc
   "sc must be a sorted collection, test(s) one of <, <=, > or\n>=. Returns a reverse seq of those entries with keys ek for\nwhich (test (.. sc comparator (compare ek key)) 0) is true",
   :var-type "function",
   :line 3452,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "second",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L73",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/second",
   :doc "Same as (first (next x))",
   :var-type "function",
   :line 73,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([map keyseq]),
   :name "select-keys",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1012",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/select-keys",
   :doc
   "Returns a map containing only those entries in map whose key is in keys",
   :var-type "function",
   :line 1012,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([a f & args]),
   :name "send",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1344",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/send",
   :doc
   "Dispatch an action to an agent. Returns the agent immediately.\nSubsequently, in a thread from a thread pool, the state of the agent\nwill be set to the value of:\n\n(apply action-fn state-of-agent args)",
   :var-type "function",
   :line 1344,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([a f & args]),
   :name "send-off",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1353",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/send-off",
   :doc
   "Dispatch a potentially blocking action to an agent. Returns the\nagent immediately. Subsequently, in a separate thread, the state of\nthe agent will be set to the value of:\n\n(apply action-fn state-of-agent args)",
   :var-type "function",
   :line 1353,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([coll]),
   :name "seq",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L98",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/seq",
   :doc
   "Returns a seq on the collection. If the collection is\nempty, returns nil.  (seq nil) returns nil. seq also works on\nStrings, native Java arrays (of reference types) and any objects\nthat implement Iterable.",
   :var-type "function",
   :line 98,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "seq?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L113",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/seq?",
   :doc "Return true if x implements ISeq",
   :var-type "function",
   :line 113,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([s] [n-or-q s]),
   :name "seque",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3624",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/seque",
   :doc
   "Creates a queued seq on another (presumably lazy) seq s. The queued\nseq will produce a concrete seq in the background, and can get up to\nn items ahead of the consumer. n-or-q can be an integer n buffer\nsize, or an instance of java.util.concurrent BlockingQueue. Note\nthat reading from a seque can block if the reader gets ahead of the\nproducer.",
   :var-type "function",
   :line 3624,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([coll]),
   :name "sequence",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1689",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/sequence",
   :doc
   "Coerces coll to a (possibly empty) sequence, if it is not already\none. Will not force a lazy seq. (sequence nil) yields ()",
   :var-type "function",
   :line 1689,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([coll]),
   :name "sequential?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4243",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/sequential?",
   :doc "Returns true if coll implements Sequential",
   :var-type "function",
   :line 4243,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([coll]),
   :name "set",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2634",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/set",
   :doc "Returns a set of the distinct elements of coll.",
   :var-type "function",
   :line 2634,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([iref validator-fn]),
   :name "set-validator!",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1490",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/set-validator!",
   :doc
   "Sets the validator-fn for a var/ref/agent/atom. validator-fn must be nil or a\nside-effect-free fn of one argument, which will be passed the intended\nnew state on any state change. If the new state is unacceptable, the\nvalidator-fn should return false or throw an exception. If the current state (root\nvalue if var) is not acceptable to the new validator, an exception\nwill be thrown and the validator will not be changed.",
   :var-type "function",
   :line 1490,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "set?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4225",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/set?",
   :doc "Returns true if x implements IPersistentSet",
   :var-type "function",
   :line 4225,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "short",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2228",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/short",
   :doc "Coerce to short",
   :var-type "function",
   :line 2228,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([size-or-seq] [size init-val-or-seq]),
   :name "short-array",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3562",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/short-array",
   :doc "Creates an array of shorts",
   :var-type "function",
   :line 3562,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([xs]),
   :name "shorts",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3602",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/shorts",
   :doc "Casts to shorts[]",
   :var-type "function",
   :line 3602,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([]),
   :name "shutdown-agents",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1405",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/shutdown-agents",
   :doc
   "Initiates a shutdown of the thread pools that back the agent\nsystem. Running actions will complete, but no new actions will be\naccepted",
   :var-type "function",
   :line 1405,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([f] [f enc]),
   :name "slurp",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3342",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/slurp",
   :doc
   "Reads the file named by f using the encoding enc into a string\nand returns it.",
   :var-type "function",
   :line 3342,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([pred coll]),
   :name "some",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1715",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/some",
   :doc
   "Returns the first logical true value of (pred x) for any x in coll,\nelse nil.  One common idiom is to use a set as pred, for example\nthis will return :fred if :fred is in the sequence, otherwise nil:\n(some #{:fred} coll)",
   :var-type "function",
   :line 1715,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([coll] [comp coll]),
   :name "sort",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1968",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/sort",
   :doc
   "Returns a sorted sequence of the items in coll. If no comparator is\nsupplied, uses compare. comparator must\nimplement java.util.Comparator.",
   :var-type "function",
   :line 1968,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([keyfn coll] [keyfn comp coll]),
   :name "sort-by",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1981",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/sort-by",
   :doc
   "Returns a sorted sequence of the items in coll, where the sort\norder is determined by comparing (keyfn item).  If no comparator is\nsupplied, uses compare. comparator must\nimplement java.util.Comparator.",
   :var-type "function",
   :line 1981,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([& keyvals]),
   :name "sorted-map",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L287",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/sorted-map",
   :doc
   "keyval => key val\nReturns a new sorted map with supplied mappings.",
   :var-type "function",
   :line 287,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([comparator & keyvals]),
   :name "sorted-map-by",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L293",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/sorted-map-by",
   :doc
   "keyval => key val\nReturns a new sorted map with supplied mappings, using the supplied comparator.",
   :var-type "function",
   :line 293,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([& keys]),
   :name "sorted-set",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L299",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/sorted-set",
   :doc "Returns a new sorted set with supplied keys.",
   :var-type "function",
   :line 299,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([comparator & keys]),
   :name "sorted-set-by",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L304",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/sorted-set-by",
   :doc
   "Returns a new sorted set with supplied keys, using the supplied comparator.",
   :var-type "function",
   :line 304,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([coll]),
   :name "sorted?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4247",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/sorted?",
   :doc "Returns true if coll implements Sorted",
   :var-type "function",
   :line 4247,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "special-form-anchor",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3261",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/special-form-anchor",
   :doc
   "Returns the anchor tag on http://clojure.org/special_forms for the\nspecial form x, or nil",
   :var-type "function",
   :line 3261,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([s]),
   :name "special-symbol?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3333",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/special-symbol?",
   :doc "Returns true if s names a special form",
   :var-type "function",
   :line 3333,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([n coll]),
   :name "split-at",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1874",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/split-at",
   :doc "Returns a vector of [(take n coll) (drop n coll)]",
   :var-type "function",
   :line 1874,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([pred coll]),
   :name "split-with",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1879",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/split-with",
   :doc
   "Returns a vector of [(take-while pred coll) (drop-while pred coll)]",
   :var-type "function",
   :line 1879,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([] [x] [x & ys]),
   :name "str",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L356",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/str",
   :doc
   "With no args, returns the empty string. With one arg x, returns\nx.toString().  (str nil) returns the empty string. With more than\none arg, returns the concatenation of the str values of the args.",
   :var-type "function",
   :line 356,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "stream?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1684",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/stream?",
   :doc "Returns true if x is an instance of Stream",
   :var-type "function",
   :line 1684,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "string?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L123",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/string?",
   :doc "Return true if x is a String",
   :var-type "function",
   :line 123,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([s & vals]),
   :name "struct",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2605",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/struct",
   :doc
   "Returns a new structmap instance with the keys of the\nstructure-basis. vals must be supplied for basis keys in order -\nwhere values are not supplied they will default to nil.",
   :var-type "function",
   :line 2605,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([s & inits]),
   :name "struct-map",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2597",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/struct-map",
   :doc
   "Returns a new structmap instance with the keys of the\nstructure-basis. keyvals may contain all, some or none of the basis\nkeys - where values are not supplied they will default to nil.\nkeyvals can also contain keys not in the basis.",
   :var-type "function",
   :line 2597,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([s start] [s start end]),
   :name "subs",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3358",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/subs",
   :doc
   "Returns the substring of s beginning at start inclusive, and ending\nat end (defaults to length of string), exclusive.",
   :var-type "function",
   :line 3358,
   :file "src/clj/clojure/core.clj"}
  {:arglists
   ([sc test key] [sc start-test start-key end-test end-key]),
   :name "subseq",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3437",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/subseq",
   :doc
   "sc must be a sorted collection, test(s) one of <, <=, > or\n>=. Returns a seq of those entries with keys ek for\nwhich (test (.. sc comparator (compare ek key)) 0) is true",
   :var-type "function",
   :line 3437,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([v start] [v start end]),
   :name "subvec",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2398",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/subvec",
   :doc
   "Returns a persistent vector of the items in vector from\nstart (inclusive) to end (exclusive).  If end is not supplied,\ndefaults to (count vector). This operation is O(1) and very fast, as\nthe resulting vector shares structure with the original and no\ntrimming is done.",
   :var-type "function",
   :line 2398,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([class]),
   :name "supers",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3688",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/supers",
   :doc
   "Returns the immediate and indirect superclasses and interfaces of c, if any",
   :var-type "function",
   :line 3688,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([atom f] [atom f x] [atom f x y] [atom f x y & args]),
   :name "swap!",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1469",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/swap!",
   :doc
   "Atomically swaps the value of atom to be:\n(apply f current-value-of-atom args). Note that f may be called\nmultiple times, and thus should be free of side effects.  Returns\nthe value that was swapped in.",
   :var-type "function",
   :line 1469,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([name] [ns name]),
   :name "symbol",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L380",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/symbol",
   :doc "Returns a Symbol with the given namespace and name.",
   :var-type "function",
   :line 380,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "symbol?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L372",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/symbol?",
   :doc "Return true if x is a Symbol",
   :var-type "function",
   :line 372,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([flags-ignored-for-now & body]),
   :name "sync",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1577",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/sync",
   :doc
   "transaction-flags => TBD, pass nil for now\n\nRuns the exprs (in an implicit do) in a transaction that encompasses\nexprs and any nested calls.  Starts a transaction if none is already\nrunning on this thread. Any uncaught exception will abort the\ntransaction and flow out of sync. The exprs may be run more than\nonce, but any effects on Refs will be atomic.",
   :var-type "macro",
   :line 1577,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "syntax-symbol-anchor",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3268",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/syntax-symbol-anchor",
   :doc
   "Returns the anchor tag on http://clojure.org/special_forms for the\nspecial form that uses syntax symbol x, or nil",
   :var-type "function",
   :line 3268,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([n coll]),
   :name "take",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1815",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/take",
   :doc
   "Returns a lazy sequence of the first n items in coll, or all items if\nthere are fewer than n.",
   :var-type "function",
   :line 1815,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([n coll]),
   :name "take-last",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1848",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/take-last",
   :doc
   "Returns a seq of the last n items in coll.  Depending on the type\nof coll may be no better than linear time.  For vectors, see also subvec.",
   :var-type "function",
   :line 1848,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([n coll]),
   :name "take-nth",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2771",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/take-nth",
   :doc "Returns a lazy seq of every nth item in coll.",
   :var-type "function",
   :line 2771,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([pred coll]),
   :name "take-while",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1824",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/take-while",
   :doc
   "Returns a lazy sequence of successive items from coll while\n(pred item) returns true. pred must be free of side-effects.",
   :var-type "function",
   :line 1824,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([v]),
   :name "test",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3157",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/test",
   :doc
   "test [v] finds fn at key :test in var metadata and calls it,\npresuming failure will throw exception",
   :var-type "function",
   :line 3157,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "the-ns",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2666",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/the-ns",
   :doc
   "If passed a namespace, returns it. Else, when passed a symbol,\nreturns the namespace named by it, throwing an exception if not\nfound.",
   :var-type "function",
   :line 2666,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([expr]),
   :name "time",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2454",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/time",
   :doc
   "Evaluates expr and prints the time it took.  Returns the value of\nexpr.",
   :var-type "macro",
   :line 2454,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([coll]),
   :name "to-array",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L255",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/to-array",
   :doc
   "Returns an array of Objects containing the contents of coll, which\ncan be any Collection.  Maps to java.util.Collection.toArray().",
   :var-type "function",
   :line 255,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([coll]),
   :name "to-array-2d",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2558",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/to-array-2d",
   :doc
   "Returns a (potentially-ragged) 2-dimensional array of Objects\ncontaining the contents of coll, which can be any Collection of any\nCollection.",
   :var-type "function",
   :line 2558,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([f] [f & args]),
   :name "trampoline",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4279",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/trampoline",
   :doc
   "trampoline can be used to convert algorithms requiring mutual\nrecursion without stack consumption. Calls f with supplied args, if\nany. If f returns a fn, calls that fn with no arguments, and\ncontinues to repeat, until the return value is not a fn, then\nreturns that non-fn value. Note that if you want to return a fn as a\nfinal value, you must wrap it in some data structure and unpack it\nafter trampoline returns.",
   :var-type "function",
   :line 4279,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([coll]),
   :name "transient",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4603",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/transient",
   :doc
   "Alpha - subject to change.\nReturns a new, transient version of the collection, in constant time.",
   :var-type "function",
   :line 4603,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([branch? children root]),
   :name "tree-seq",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3302",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/tree-seq",
   :doc
   "Returns a lazy sequence of the nodes in a tree, via a depth-first walk.\n branch? must be a fn of one arg that returns true if passed a node\n that can have children (but may not).  children must be a fn of one\n arg that returns a sequence of the children. Will only be called on\n nodes for which branch? returns true. Root is the root node of the\ntree.",
   :var-type "function",
   :line 3302,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "true?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L346",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/true?",
   :doc "Returns true if x is the value true, false otherwise.",
   :var-type "function",
   :line 346,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "type",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2199",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/type",
   :doc "Returns the :type metadata of x, or its Class if none",
   :var-type "function",
   :line 2199,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x y]),
   :name "unchecked-add",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L809",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/unchecked-add",
   :doc
   "Returns the sum of x and y, both int or long.\nNote - uses a primitive operator subject to overflow.",
   :var-type "function",
   :line 809,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "unchecked-dec",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L797",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/unchecked-dec",
   :doc
   "Returns a number one less than x, an int or long.\nNote - uses a primitive operator subject to overflow.",
   :var-type "function",
   :line 797,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x y]),
   :name "unchecked-divide",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L827",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/unchecked-divide",
   :doc
   "Returns the division of x by y, both int or long.\nNote - uses a primitive operator subject to truncation.",
   :var-type "function",
   :line 827,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "unchecked-inc",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L791",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/unchecked-inc",
   :doc
   "Returns a number one greater than x, an int or long.\nNote - uses a primitive operator subject to overflow.",
   :var-type "function",
   :line 791,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x y]),
   :name "unchecked-multiply",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L821",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/unchecked-multiply",
   :doc
   "Returns the product of x and y, both int or long.\nNote - uses a primitive operator subject to overflow.",
   :var-type "function",
   :line 821,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "unchecked-negate",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L803",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/unchecked-negate",
   :doc
   "Returns the negation of x, an int or long.\nNote - uses a primitive operator subject to overflow.",
   :var-type "function",
   :line 803,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x y]),
   :name "unchecked-remainder",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L833",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/unchecked-remainder",
   :doc
   "Returns the remainder of division of x by y, both int or long.\nNote - uses a primitive operator subject to truncation.",
   :var-type "function",
   :line 833,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x y]),
   :name "unchecked-subtract",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L815",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/unchecked-subtract",
   :doc
   "Returns the difference of x and y, both int or long.\nNote - uses a primitive operator subject to overflow.",
   :var-type "function",
   :line 815,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([tag parent] [h tag parent]),
   :name "underive",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3790",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/underive",
   :doc
   "Removes a parent/child relationship between parent and\ntag. h must be a hierarchy obtained from make-hierarchy, if not\nsupplied defaults to, and modifies, the global hierarchy.",
   :var-type "function",
   :line 3790,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([m [k & ks] f & args]),
   :name "update-in",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4200",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/update-in",
   :doc
   "'Updates' a value in a nested associative structure, where ks is a\nsequence of keys and f is a function that will take the old value\nand any supplied args and return the new value, and returns a new\nnested structure.  If any levels do not exist, hash-maps will be\ncreated.",
   :var-type "function",
   :line 4200,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([proxy mappings]),
   :name "update-proxy",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/040f083efc16dd830a4508a35a04465e3e5677d3/src/clj/clojure/core_proxy.clj#L278",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/040f083efc16dd830a4508a35a04465e3e5677d3/src/clj/clojure/core_proxy.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/update-proxy",
   :doc
   "Takes a proxy instance and a map of strings (which must\ncorrespond to methods of the proxy superclass/superinterfaces) to\nfns (which must take arguments matching the corresponding method,\nplus an additional (explicit) first arg corresponding to this, and\nupdates (via assoc) the proxy's fn map. nil can be passed instead of\na fn, in which case the corresponding method will revert to the\ndefault behavior. Note that this function can be used to update the\nbehavior of an existing instance without changing its identity.",
   :var-type "function",
   :line 278,
   :file "src/clj/clojure/core_proxy.clj"}
  {:arglists ([& args]),
   :name "use",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4140",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/use",
   :doc
   "Like 'require, but also refers to each lib's namespace using\nclojure.core/refer. Use :use in the ns macro in preference to calling\nthis directly.\n\n'use accepts additional options in libspecs: :exclude, :only, :rename.\nThe arguments and semantics for :exclude, :only, and :rename are the same\nas those documented for clojure.core/refer.",
   :var-type "function",
   :line 4140,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([e]),
   :name "val",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1038",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/val",
   :doc "Returns the value in the map entry.",
   :var-type "function",
   :line 1038,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([map]),
   :name "vals",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1029",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/vals",
   :doc "Returns a sequence of the map's values.",
   :var-type "function",
   :line 1029,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "var-get",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2792",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/var-get",
   :doc "Gets the value in the var object",
   :var-type "function",
   :line 2792,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x val]),
   :name "var-set",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2796",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/var-set",
   :doc
   "Sets the value in the var object to val. The var must be\nthread-locally bound.",
   :var-type "function",
   :line 2796,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([v]),
   :name "var?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3338",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/var?",
   :doc "Returns true if v is of type clojure.lang.Var",
   :var-type "function",
   :line 3338,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([obj f & args]),
   :name "vary-meta",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L446",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/vary-meta",
   :doc
   "Returns an object of the same type and value as obj, with\n(apply f (meta obj) args) as its metadata.",
   :var-type "function",
   :line 446,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([coll]),
   :name "vec",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L267",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/vec",
   :doc "Creates a new vector containing the contents of coll.",
   :var-type "function",
   :line 267,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([] [& args]),
   :name "vector",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L261",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/vector",
   :doc "Creates a new vector containing the args.",
   :var-type "function",
   :line 261,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "vector?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L133",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/vector?",
   :doc "Return true if x implements IPersistentVector ",
   :var-type "function",
   :line 133,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([test & body]),
   :name "when",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L326",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/when",
   :doc
   "Evaluates test. If logical true, evaluates body in an implicit do.",
   :var-type "macro",
   :line 326,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([bindings & body]),
   :name "when-first",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2991",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/when-first",
   :doc
   "bindings => x xs\n\nSame as (when (seq xs) (let [x (first xs)] body))",
   :var-type "macro",
   :line 2991,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([bindings & body]),
   :name "when-let",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1209",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/when-let",
   :doc
   "bindings => binding-form test\n\nWhen test is true, evaluates body with binding-form bound to the value of test",
   :var-type "macro",
   :line 1209,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([test & body]),
   :name "when-not",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L331",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/when-not",
   :doc
   "Evaluates test. If logical false, evaluates body in an implicit do.",
   :var-type "macro",
   :line 331,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([test & body]),
   :name "while",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L4309",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/while",
   :doc
   "Repeatedly executes body while test expression is true. Presumes\nsome side-effect will cause test to become false/nil. Returns nil",
   :var-type "macro",
   :line 4309,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([binding-map & body]),
   :name "with-bindings",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1287",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/with-bindings",
   :doc
   "Takes a map of Var/value pairs. Installs for the given Vars the associated\nvalues as thread-local bindings. The executes body. Pops the installed\nbindings after body was evaluated. Returns the value of body.",
   :var-type "macro",
   :line 1287,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([binding-map f & args]),
   :name "with-bindings*",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1276",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/with-bindings*",
   :doc
   "Takes a map of Var/value pairs. Installs for the given Vars the associated\nvalues as thread-local bindings. Then calls f with the supplied arguments.\nPops the installed bindings after f returned. Returns whatever f returns.",
   :var-type "function",
   :line 1276,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([s & body]),
   :name "with-in-str",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3113",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/with-in-str",
   :doc
   "Evaluates body in a context in which *in* is bound to a fresh\nStringReader initialized with the string s.",
   :var-type "macro",
   :line 3113,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([name-vals-vec & body]),
   :name "with-local-vars",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2801",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/with-local-vars",
   :doc
   "varbinding=> symbol init-expr\n\nExecutes the exprs in a context in which the symbols are bound to\nvars with per-thread bindings to the init-exprs.  The symbols refer\nto the var objects themselves, and must be accessed with var-get and\nvar-set",
   :var-type "macro",
   :line 2801,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([obj m]),
   :name "with-meta",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L161",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/with-meta",
   :doc
   "Returns an object of the same type and value as obj, with\nmap m as its metadata.",
   :var-type "function",
   :line 161,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([bindings & body]),
   :name "with-open",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L2409",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/with-open",
   :doc
   "bindings => [name init ...]\n\nEvaluates body in a try expression with names bound to the values\nof the inits, and a finally clause that calls (.close name) on each\nname in reverse order.",
   :var-type "macro",
   :line 2409,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([& body]),
   :name "with-out-str",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3103",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/with-out-str",
   :doc
   "Evaluates exprs in a context in which *out* is bound to a fresh\nStringWriter.  Returns the string created by any nested printing\ncalls.",
   :var-type "macro",
   :line 3103,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([precision & exprs]),
   :name "with-precision",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3415",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/with-precision",
   :doc
   "Sets the precision and rounding mode to be used for BigDecimal operations.\n\nUsage: (with-precision 10 (/ 1M 3))\nor:    (with-precision 10 :rounding HALF_DOWN (/ 1M 3))\n\nThe rounding mode is one of CEILING, FLOOR, HALF_UP, HALF_DOWN,\nHALF_EVEN, UP, DOWN and UNNECESSARY; it defaults to HALF_UP.",
   :var-type "macro",
   :line 3415,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([root]),
   :name "xml-seq",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L3325",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/xml-seq",
   :doc "A tree seq on the xml elements as per xml/parse",
   :var-type "function",
   :line 3325,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "zero?",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L600",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/zero?",
   :doc "Returns true if num is zero, else false",
   :var-type "function",
   :line 600,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([keys vals]),
   :name "zipmap",
   :namespace "clojure.core",
   :source-url
   "https://github.com/clojure/clojure/blob/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj#L1942",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/f4c58e3500b3668a0941ca21f9aa4f444de2c652/src/clj/clojure/core.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.core-api.html#clojure.core/zipmap",
   :doc
   "Returns a map with the keys mapped to the corresponding vals.",
   :var-type "function",
   :line 1942,
   :file "src/clj/clojure/core.clj"}
  {:arglists ([x]),
   :name "inspect",
   :namespace "clojure.inspector",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/inspector.clj#L148",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/inspector.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.inspector-api.html#clojure.inspector/inspect",
   :doc "creates a graphical (Swing) inspector on the supplied object",
   :var-type "function",
   :line 148,
   :file "src/clj/clojure/inspector.clj"}
  {:arglists ([data]),
   :name "inspect-table",
   :namespace "clojure.inspector",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/inspector.clj#L95",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/inspector.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.inspector-api.html#clojure.inspector/inspect-table",
   :doc
   "creates a graphical (Swing) inspector on the supplied regular\ndata, which must be a sequential data structure of data structures\nof equal length",
   :var-type "function",
   :line 95,
   :file "src/clj/clojure/inspector.clj"}
  {:arglists ([data]),
   :name "inspect-tree",
   :namespace "clojure.inspector",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/inspector.clj#L87",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/inspector.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.inspector-api.html#clojure.inspector/inspect-tree",
   :doc
   "creates a graphical (Swing) inspector on the supplied hierarchical data",
   :var-type "function",
   :line 87,
   :file "src/clj/clojure/inspector.clj"}
  {:arglists ([path]),
   :name "load-script",
   :namespace "clojure.main",
   :source-url
   "https://github.com/clojure/clojure/blob/2cc710e7aeaab08e0739debe21e2cc6b7020e1b1/src/clj/clojure/main.clj#L206",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/2cc710e7aeaab08e0739debe21e2cc6b7020e1b1/src/clj/clojure/main.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.main-api.html#clojure.main/load-script",
   :doc
   "Loads Clojure source from a file or resource given its path. Paths\nbeginning with @ or @/ are considered relative to classpath.",
   :var-type "function",
   :line 206,
   :file "src/clj/clojure/main.clj"}
  {:arglists ([& args]),
   :name "main",
   :namespace "clojure.main",
   :source-url
   "https://github.com/clojure/clojure/blob/2cc710e7aeaab08e0739debe21e2cc6b7020e1b1/src/clj/clojure/main.clj#L310",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/2cc710e7aeaab08e0739debe21e2cc6b7020e1b1/src/clj/clojure/main.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.main-api.html#clojure.main/main",
   :doc
   "Usage: java -cp clojure.jar clojure.main [init-opt*] [main-opt] [arg*]\n\nWith no options or args, runs an interactive Read-Eval-Print Loop\n\ninit options:\n  -i, --init path   Load a file or resource\n  -e, --eval string Evaluate expressions in string; print non-nil values\n\nmain options:\n  -r, --repl        Run a repl\n  path              Run a script from from a file or resource\n  -                 Run a script from standard input\n  -h, -?, --help    Print this help message and exit\n\noperation:\n\n  - Establishes thread-local bindings for commonly set!-able vars\n  - Enters the user namespace\n  - Binds *command-line-args* to a seq of strings containing command line\n    args that appear after any main option\n  - Runs all init options in order\n  - Runs a repl or script if requested\n\nThe init options may be repeated and mixed freely, but must appear before\nany main option. The appearance of any eval option before running a repl\nsuppresses the usual repl greeting message: \"Clojure ~(clojure-version)\".\n\nPaths may be absolute or relative in the filesystem or relative to\nclasspath. Classpath-relative paths have prefix of @ or @/",
   :var-type "function",
   :line 310,
   :file "src/clj/clojure/main.clj"}
  {:arglists ([& options]),
   :name "repl",
   :namespace "clojure.main",
   :source-url
   "https://github.com/clojure/clojure/blob/2cc710e7aeaab08e0739debe21e2cc6b7020e1b1/src/clj/clojure/main.clj#L118",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/2cc710e7aeaab08e0739debe21e2cc6b7020e1b1/src/clj/clojure/main.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.main-api.html#clojure.main/repl",
   :doc
   "Generic, reusable, read-eval-print loop. By default, reads from *in*,\nwrites to *out*, and prints exception summaries to *err*. If you use the\ndefault :read hook, *in* must either be an instance of\nLineNumberingPushbackReader or duplicate its behavior of both supporting\n.unread and collapsing CR, LF, and CRLF into a single \\newline. Options\nare sequential keyword-value pairs. Available options and their defaults:\n\n   - :init, function of no arguments, initialization hook called with\n     bindings for set!-able vars in place.\n     default: #()\n\n   - :need-prompt, function of no arguments, called before each\n     read-eval-print except the first, the user will be prompted if it\n     returns true.\n     default: (if (instance? LineNumberingPushbackReader *in*)\n                #(.atLineStart *in*)\n                #(identity true))\n\n   - :prompt, function of no arguments, prompts for more input.\n     default: repl-prompt\n\n   - :flush, function of no arguments, flushes output\n     default: flush\n\n   - :read, function of two arguments, reads from *in*:\n       - returns its first argument to request a fresh prompt\n         - depending on need-prompt, this may cause the repl to prompt\n           before reading again\n       - returns its second argument to request an exit from the repl\n       - else returns the next object read from the input stream\n     default: repl-read\n\n   - :eval, funtion of one argument, returns the evaluation of its\n     argument\n     default: eval\n\n   - :print, function of one argument, prints its argument to the output\n     default: prn\n\n   - :caught, function of one argument, a throwable, called when\n     read, eval, or print throws an exception or error\n     default: repl-caught",
   :var-type "function",
   :line 118,
   :file "src/clj/clojure/main.clj"}
  {:arglists ([e]),
   :name "repl-caught",
   :namespace "clojure.main",
   :source-url
   "https://github.com/clojure/clojure/blob/2cc710e7aeaab08e0739debe21e2cc6b7020e1b1/src/clj/clojure/main.clj#L113",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/2cc710e7aeaab08e0739debe21e2cc6b7020e1b1/src/clj/clojure/main.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.main-api.html#clojure.main/repl-caught",
   :doc "Default :caught hook for repl",
   :var-type "function",
   :line 113,
   :file "src/clj/clojure/main.clj"}
  {:arglists ([throwable]),
   :name "repl-exception",
   :namespace "clojure.main",
   :source-url
   "https://github.com/clojure/clojure/blob/2cc710e7aeaab08e0739debe21e2cc6b7020e1b1/src/clj/clojure/main.clj#L105",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/2cc710e7aeaab08e0739debe21e2cc6b7020e1b1/src/clj/clojure/main.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.main-api.html#clojure.main/repl-exception",
   :doc
   "Returns CompilerExceptions in tact, but only the root cause of other\nthrowables",
   :var-type "function",
   :line 105,
   :file "src/clj/clojure/main.clj"}
  {:arglists ([]),
   :name "repl-prompt",
   :namespace "clojure.main",
   :source-url
   "https://github.com/clojure/clojure/blob/2cc710e7aeaab08e0739debe21e2cc6b7020e1b1/src/clj/clojure/main.clj#L41",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/2cc710e7aeaab08e0739debe21e2cc6b7020e1b1/src/clj/clojure/main.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.main-api.html#clojure.main/repl-prompt",
   :doc "Default :prompt hook for repl",
   :var-type "function",
   :line 41,
   :file "src/clj/clojure/main.clj"}
  {:arglists ([request-prompt request-exit]),
   :name "repl-read",
   :namespace "clojure.main",
   :source-url
   "https://github.com/clojure/clojure/blob/2cc710e7aeaab08e0739debe21e2cc6b7020e1b1/src/clj/clojure/main.clj#L78",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/2cc710e7aeaab08e0739debe21e2cc6b7020e1b1/src/clj/clojure/main.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.main-api.html#clojure.main/repl-read",
   :doc
   "Default :read hook for repl. Reads from *in* which must either be an\ninstance of LineNumberingPushbackReader or duplicate its behavior of both\nsupporting .unread and collapsing all of CR, LF, and CRLF into a single\n\\newline. repl-read:\n  - skips whitespace, then\n    - returns request-prompt on start of line, or\n    - returns request-exit on end of stream, or\n    - reads an object from the input stream, then\n      - skips the next input character if it's end of line, then\n      - returns the object.",
   :var-type "function",
   :line 78,
   :file "src/clj/clojure/main.clj"}
  {:arglists ([s]),
   :name "skip-if-eol",
   :namespace "clojure.main",
   :source-url
   "https://github.com/clojure/clojure/blob/2cc710e7aeaab08e0739debe21e2cc6b7020e1b1/src/clj/clojure/main.clj#L46",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/2cc710e7aeaab08e0739debe21e2cc6b7020e1b1/src/clj/clojure/main.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.main-api.html#clojure.main/skip-if-eol",
   :doc
   "If the next character on stream s is a newline, skips it, otherwise\nleaves the stream untouched. Returns :line-start, :stream-end, or :body\nto indicate the relative location of the next character on s. The stream\nmust either be an instance of LineNumberingPushbackReader or duplicate\nits behavior of both supporting .unread and collapsing all of CR, LF, and\nCRLF to a single \\newline.",
   :var-type "function",
   :line 46,
   :file "src/clj/clojure/main.clj"}
  {:arglists ([s]),
   :name "skip-whitespace",
   :namespace "clojure.main",
   :source-url
   "https://github.com/clojure/clojure/blob/2cc710e7aeaab08e0739debe21e2cc6b7020e1b1/src/clj/clojure/main.clj#L60",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/2cc710e7aeaab08e0739debe21e2cc6b7020e1b1/src/clj/clojure/main.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.main-api.html#clojure.main/skip-whitespace",
   :doc
   "Skips whitespace characters on stream s. Returns :line-start, :stream-end,\nor :body to indicate the relative location of the next character on s.\nInterprets comma as whitespace and semicolon as comment to end of line.\nDoes not interpret #! as comment to end of line because only one\ncharacter of lookahead is available. The stream must either be an\ninstance of LineNumberingPushbackReader or duplicate its behavior of both\nsupporting .unread and collapsing all of CR, LF, and CRLF to a single\n\\newline.",
   :var-type "function",
   :line 60,
   :file "src/clj/clojure/main.clj"}
  {:arglists ([& body]),
   :name "with-bindings",
   :namespace "clojure.main",
   :source-url
   "https://github.com/clojure/clojure/blob/2cc710e7aeaab08e0739debe21e2cc6b7020e1b1/src/clj/clojure/main.clj#L20",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/2cc710e7aeaab08e0739debe21e2cc6b7020e1b1/src/clj/clojure/main.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.main-api.html#clojure.main/with-bindings",
   :doc
   "Executes body in the context of thread-local bindings for several vars\nthat often need to be set!: *ns* *warn-on-reflection* *math-context*\n*print-meta* *print-length* *print-level* *compile-path*\n*command-line-args* *1 *2 *3 *e",
   :var-type "macro",
   :line 20,
   :file "src/clj/clojure/main.clj"}
  {:arglists ([s1] [s1 s2] [s1 s2 & sets]),
   :name "difference",
   :namespace "clojure.set",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/set.clj#L46",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/set.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.set-api.html#clojure.set/difference",
   :doc
   "Return a set that is the first set without elements of the remaining sets",
   :var-type "function",
   :line 46,
   :file "src/clj/clojure/set.clj"}
  {:arglists ([xrel ks]),
   :name "index",
   :namespace "clojure.set",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/set.clj#L87",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/set.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.set-api.html#clojure.set/index",
   :doc
   "Returns a map of the distinct values of ks in the xrel mapped to a\nset of the maps in xrel with the corresponding values of ks.",
   :var-type "function",
   :line 87,
   :file "src/clj/clojure/set.clj"}
  {:arglists ([s1] [s1 s2] [s1 s2 & sets]),
   :name "intersection",
   :namespace "clojure.set",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/set.clj#L31",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/set.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.set-api.html#clojure.set/intersection",
   :doc "Return a set that is the intersection of the input sets",
   :var-type "function",
   :line 31,
   :file "src/clj/clojure/set.clj"}
  {:arglists ([xrel yrel] [xrel yrel km]),
   :name "join",
   :namespace "clojure.set",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/set.clj#L101",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/set.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.set-api.html#clojure.set/join",
   :doc
   "When passed 2 rels, returns the rel corresponding to the natural\njoin. When passed an additional keymap, joins on the corresponding\nkeys.",
   :var-type "function",
   :line 101,
   :file "src/clj/clojure/set.clj"}
  {:arglists ([m]),
   :name "map-invert",
   :namespace "clojure.set",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/set.clj#L97",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/set.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.set-api.html#clojure.set/map-invert",
   :doc "Returns the map with the vals mapped to the keys.",
   :var-type "function",
   :line 97,
   :file "src/clj/clojure/set.clj"}
  {:arglists ([xrel ks]),
   :name "project",
   :namespace "clojure.set",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/set.clj#L67",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/set.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.set-api.html#clojure.set/project",
   :doc
   "Returns a rel of the elements of xrel with only the keys in ks",
   :var-type "function",
   :line 67,
   :file "src/clj/clojure/set.clj"}
  {:arglists ([xrel kmap]),
   :name "rename",
   :namespace "clojure.set",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/set.clj#L82",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/set.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.set-api.html#clojure.set/rename",
   :doc
   "Returns a rel of the maps in xrel with the keys in kmap renamed to the vals in kmap",
   :var-type "function",
   :line 82,
   :file "src/clj/clojure/set.clj"}
  {:arglists ([map kmap]),
   :name "rename-keys",
   :namespace "clojure.set",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/set.clj#L72",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/set.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.set-api.html#clojure.set/rename-keys",
   :doc
   "Returns the map with the keys in kmap renamed to the vals in kmap",
   :var-type "function",
   :line 72,
   :file "src/clj/clojure/set.clj"}
  {:arglists ([pred xset]),
   :name "select",
   :namespace "clojure.set",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/set.clj#L61",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/set.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.set-api.html#clojure.set/select",
   :doc "Returns a set of the elements for which pred is true",
   :var-type "function",
   :line 61,
   :file "src/clj/clojure/set.clj"}
  {:arglists ([] [s1] [s1 s2] [s1 s2 & sets]),
   :name "union",
   :namespace "clojure.set",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/set.clj#L19",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/set.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.set-api.html#clojure.set/union",
   :doc "Return a set that is the union of the input sets",
   :var-type "function",
   :line 19,
   :file "src/clj/clojure/set.clj"}
  {:arglists ([]),
   :name "e",
   :namespace "clojure.stacktrace",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/stacktrace.clj#L69",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/stacktrace.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.stacktrace-api.html#clojure.stacktrace/e",
   :doc
   "REPL utility.  Prints a brief stack trace for the root cause of the\nmost recent exception.",
   :var-type "function",
   :line 69,
   :file "src/clj/clojure/stacktrace.clj"}
  {:arglists ([tr] [tr n]),
   :name "print-cause-trace",
   :namespace "clojure.stacktrace",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/stacktrace.clj#L60",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/stacktrace.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.stacktrace-api.html#clojure.stacktrace/print-cause-trace",
   :doc
   "Like print-stack-trace but prints chained exceptions (causes).",
   :var-type "function",
   :line 60,
   :file "src/clj/clojure/stacktrace.clj"}
  {:arglists ([tr] [tr n]),
   :name "print-stack-trace",
   :namespace "clojure.stacktrace",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/stacktrace.clj#L41",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/stacktrace.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.stacktrace-api.html#clojure.stacktrace/print-stack-trace",
   :doc
   "Prints a Clojure-oriented stack trace of tr, a Throwable.\nPrints a maximum of n stack frames (default: unlimited).\nDoes not print chained exceptions (causes).",
   :var-type "function",
   :line 41,
   :file "src/clj/clojure/stacktrace.clj"}
  {:arglists ([tr]),
   :name "print-throwable",
   :namespace "clojure.stacktrace",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/stacktrace.clj#L36",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/stacktrace.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.stacktrace-api.html#clojure.stacktrace/print-throwable",
   :doc "Prints the class and message of a Throwable.",
   :var-type "function",
   :line 36,
   :file "src/clj/clojure/stacktrace.clj"}
  {:arglists ([e]),
   :name "print-trace-element",
   :namespace "clojure.stacktrace",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/stacktrace.clj#L25",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/stacktrace.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.stacktrace-api.html#clojure.stacktrace/print-trace-element",
   :doc
   "Prints a Clojure-oriented view of one element in a stack trace.",
   :var-type "function",
   :line 25,
   :file "src/clj/clojure/stacktrace.clj"}
  {:arglists ([tr]),
   :name "root-cause",
   :namespace "clojure.stacktrace",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/stacktrace.clj#L18",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/stacktrace.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.stacktrace-api.html#clojure.stacktrace/root-cause",
   :doc "Returns the last 'cause' Throwable in a chain of Throwables.",
   :var-type "function",
   :line 18,
   :file "src/clj/clojure/stacktrace.clj"}
  {:arglists ([argv expr values]),
   :name "apply-template",
   :namespace "clojure.template",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/template.clj#L30",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/template.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.template-api.html#clojure.template/apply-template",
   :doc
   "For use in macros.  argv is an argument list, as in defn.  expr is\na quoted expression using the symbols in argv.  values is a sequence\nof values to be used for the arguments.\n\napply-template will recursively replace argument symbols in expr\nwith their corresponding values, returning a modified expr.\n\nExample: (apply-template '[x] '(+ x x) '[2])\n         ;=> (+ 2 2)",
   :var-type "function",
   :line 30,
   :file "src/clj/clojure/template.clj"}
  {:arglists ([argv expr & values]),
   :name "do-template",
   :namespace "clojure.template",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/template.clj#L45",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/template.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.template-api.html#clojure.template/do-template",
   :doc
   "Repeatedly copies expr (in a do block) for each group of arguments\nin values.  values are automatically partitioned by the number of\narguments in argv, an argument vector as in defn.\n\nExample: (macroexpand '(do-template [x y] (+ y x) 2 4 3 5))\n         ;=> (do (+ 4 2) (+ 5 3))",
   :var-type "macro",
   :line 45,
   :file "src/clj/clojure/template.clj"}
  {:file "src/clj/clojure/test.clj",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj#L239",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.test-api.html#clojure.test/*load-tests*",
   :namespace "clojure.test",
   :line 239,
   :var-type "var",
   :doc
   "True by default.  If set to false, no test functions will\nbe created by deftest, set-test, or with-test.  Use this to omit\ntests when compiling or loading production code.",
   :name "*load-tests*"}
  {:file "src/clj/clojure/test.clj",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj#L245",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.test-api.html#clojure.test/*stack-trace-depth*",
   :namespace "clojure.test",
   :line 245,
   :var-type "var",
   :doc
   "The maximum depth of stack traces to print when an Exception\nis thrown during a test.  Defaults to nil, which means print the \ncomplete stack trace.",
   :name "*stack-trace-depth*"}
  {:arglists ([argv expr & args]),
   :name "are",
   :namespace "clojure.test",
   :source-url
   "https://github.com/clojure/clojure/blob/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj#L518",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.test-api.html#clojure.test/are",
   :doc
   "Checks multiple assertions with a template expression.\nSee clojure.template/do-template for an explanation of\ntemplates.\n\nExample: (are [x y] (= x y)  \n              2 (+ 1 1)\n              4 (* 2 2))\nExpands to: \n         (do (is (= 2 (+ 1 1)))\n             (is (= 4 (* 2 2))))\n\nNote: This breaks some reporting features, such as line numbers.",
   :var-type "macro",
   :line 518,
   :file "src/clj/clojure/test.clj"}
  {:arglists ([msg form]),
   :name "assert-any",
   :namespace "clojure.test",
   :source-url
   "https://github.com/clojure/clojure/blob/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj#L404",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.test-api.html#clojure.test/assert-any",
   :doc
   "Returns generic assertion code for any test, including macros, Java\nmethod calls, or isolated symbols.",
   :var-type "function",
   :line 404,
   :file "src/clj/clojure/test.clj"}
  {:arglists ([msg form]),
   :name "assert-predicate",
   :namespace "clojure.test",
   :source-url
   "https://github.com/clojure/clojure/blob/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj#L386",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.test-api.html#clojure.test/assert-predicate",
   :doc
   "Returns generic assertion code for any functional predicate.  The\n'expected' argument to 'report' will contains the original form, the\n'actual' argument will contain the form with all its sub-forms\nevaluated.  If the predicate returns false, the 'actual' form will\nbe wrapped in (not...).",
   :var-type "function",
   :line 386,
   :file "src/clj/clojure/test.clj"}
  {:arglists ([f1 f2]),
   :name "compose-fixtures",
   :namespace "clojure.test",
   :source-url
   "https://github.com/clojure/clojure/blob/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj#L618",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.test-api.html#clojure.test/compose-fixtures",
   :doc
   "Composes two fixture functions, creating a new fixture function\nthat combines their behavior.",
   :var-type "function",
   :line 618,
   :file "src/clj/clojure/test.clj"}
  {:arglists ([name & body]),
   :name "deftest",
   :namespace "clojure.test",
   :source-url
   "https://github.com/clojure/clojure/blob/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj#L557",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.test-api.html#clojure.test/deftest",
   :doc
   "Defines a test function with no arguments.  Test functions may call\nother tests, so tests may be composed.  If you compose tests, you\nshould also define a function named test-ns-hook; run-tests will\ncall test-ns-hook instead of testing all vars.\n\nNote: Actually, the test body goes in the :test metadata on the var,\nand the real function (the value of the var) calls test-var on\nitself.\n\nWhen *load-tests* is false, deftest is ignored.",
   :var-type "macro",
   :line 557,
   :file "src/clj/clojure/test.clj"}
  {:arglists ([name & body]),
   :name "deftest-",
   :namespace "clojure.test",
   :source-url
   "https://github.com/clojure/clojure/blob/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj#L573",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.test-api.html#clojure.test/deftest-",
   :doc "Like deftest but creates a private var.",
   :var-type "macro",
   :line 573,
   :file "src/clj/clojure/test.clj"}
  {:arglists ([n]),
   :name "file-position",
   :namespace "clojure.test",
   :source-url
   "https://github.com/clojure/clojure/blob/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj#L275",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.test-api.html#clojure.test/file-position",
   :doc
   "Returns a vector [filename line-number] for the nth call up the\nstack.",
   :var-type "function",
   :line 275,
   :file "src/clj/clojure/test.clj"}
  {:arglists ([x]),
   :name "function?",
   :namespace "clojure.test",
   :source-url
   "https://github.com/clojure/clojure/blob/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj#L375",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.test-api.html#clojure.test/function?",
   :doc
   "Returns true if argument is a function or a symbol that resolves to\na function (not a macro).",
   :var-type "function",
   :line 375,
   :file "src/clj/clojure/test.clj"}
  {:arglists ([v]),
   :name "get-possibly-unbound-var",
   :namespace "clojure.test",
   :source-url
   "https://github.com/clojure/clojure/blob/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj#L368",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.test-api.html#clojure.test/get-possibly-unbound-var",
   :doc "Like var-get but returns nil if the var is unbound.",
   :var-type "function",
   :line 368,
   :file "src/clj/clojure/test.clj"}
  {:arglists ([name]),
   :name "inc-report-counter",
   :namespace "clojure.test",
   :source-url
   "https://github.com/clojure/clojure/blob/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj#L300",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.test-api.html#clojure.test/inc-report-counter",
   :doc
   "Increments the named counter in *report-counters*, a ref to a map.\nDoes nothing if *report-counters* is nil.",
   :var-type "function",
   :line 300,
   :file "src/clj/clojure/test.clj"}
  {:arglists ([form] [form msg]),
   :name "is",
   :namespace "clojure.test",
   :source-url
   "https://github.com/clojure/clojure/blob/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj#L501",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.test-api.html#clojure.test/is",
   :doc
   "Generic assertion macro.  'form' is any predicate test.\n'msg' is an optional message to attach to the assertion.\n\nExample: (is (= 4 (+ 2 2)) \"Two plus two should be 4\")\n\nSpecial forms:\n\n(is (thrown? c body)) checks that an instance of c is thrown from\nbody, fails if not; then returns the thing thrown.\n\n(is (thrown-with-msg? c re body)) checks that an instance of c is\nthrown AND that the message on the exception matches (with\nre-matches) the regular expression re.",
   :var-type "macro",
   :line 501,
   :file "src/clj/clojure/test.clj"}
  {:arglists ([fixtures]),
   :name "join-fixtures",
   :namespace "clojure.test",
   :source-url
   "https://github.com/clojure/clojure/blob/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj#L624",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.test-api.html#clojure.test/join-fixtures",
   :doc
   "Composes a collection of fixtures, in order.  Always returns a valid\nfixture function, even if the collection is empty.",
   :var-type "function",
   :line 624,
   :file "src/clj/clojure/test.clj"}
  {:file "src/clj/clojure/test.clj",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj#L312",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.test-api.html#clojure.test/report",
   :namespace "clojure.test",
   :line 312,
   :var-type "multimethod",
   :doc
   "Generic reporting function, may be overridden to plug in\ndifferent report formats (e.g., TAP, JUnit).  Assertions such as\n'is' call 'report' to indicate results.  The argument given to\n'report' will be a map with a :type key.  See the documentation at\nthe top of test_is.clj for more information on the types of\narguments for 'report'.",
   :name "report"}
  {:arglists ([] [re]),
   :name "run-all-tests",
   :namespace "clojure.test",
   :source-url
   "https://github.com/clojure/clojure/blob/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj#L695",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.test-api.html#clojure.test/run-all-tests",
   :doc
   "Runs all tests in all namespaces; prints results.\nOptional argument is a regular expression; only namespaces with\nnames matching the regular expression (with re-matches) will be\ntested.",
   :var-type "function",
   :line 695,
   :file "src/clj/clojure/test.clj"}
  {:arglists ([] [& namespaces]),
   :name "run-tests",
   :namespace "clojure.test",
   :source-url
   "https://github.com/clojure/clojure/blob/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj#L684",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.test-api.html#clojure.test/run-tests",
   :doc
   "Runs all tests in the given namespaces; prints results.\nDefaults to current namespace if none given.  Returns a map\nsummarizing test results.",
   :var-type "function",
   :line 684,
   :file "src/clj/clojure/test.clj"}
  {:arglists ([name & body]),
   :name "set-test",
   :namespace "clojure.test",
   :source-url
   "https://github.com/clojure/clojure/blob/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj#L581",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.test-api.html#clojure.test/set-test",
   :doc
   "Experimental.\nSets :test metadata of the named var to a fn with the given body.\nThe var must already exist.  Does not modify the value of the var.\n\nWhen *load-tests* is false, set-test is ignored.",
   :var-type "macro",
   :line 581,
   :file "src/clj/clojure/test.clj"}
  {:arglists ([summary]),
   :name "successful?",
   :namespace "clojure.test",
   :source-url
   "https://github.com/clojure/clojure/blob/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj#L703",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.test-api.html#clojure.test/successful?",
   :doc
   "Returns true if the given test summary indicates all tests\nwere successful, false otherwise.",
   :var-type "function",
   :line 703,
   :file "src/clj/clojure/test.clj"}
  {:arglists ([ns]),
   :name "test-all-vars",
   :namespace "clojure.test",
   :source-url
   "https://github.com/clojure/clojure/blob/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj#L649",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.test-api.html#clojure.test/test-all-vars",
   :doc
   "Calls test-var on every var interned in the namespace, with fixtures.",
   :var-type "function",
   :line 649,
   :file "src/clj/clojure/test.clj"}
  {:arglists ([ns]),
   :name "test-ns",
   :namespace "clojure.test",
   :source-url
   "https://github.com/clojure/clojure/blob/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj#L660",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.test-api.html#clojure.test/test-ns",
   :doc
   "If the namespace defines a function named test-ns-hook, calls that.\nOtherwise, calls test-all-vars on the namespace.  'ns' is a\nnamespace object or a symbol.\n\nInternally binds *report-counters* to a ref initialized to\n*inital-report-counters*.  Returns the final, dereferenced state of\n*report-counters*.",
   :var-type "function",
   :line 660,
   :file "src/clj/clojure/test.clj"}
  {:arglists ([v]),
   :name "test-var",
   :namespace "clojure.test",
   :source-url
   "https://github.com/clojure/clojure/blob/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj#L635",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.test-api.html#clojure.test/test-var",
   :doc
   "If v has a function in its :test metadata, calls that function,\nwith *testing-vars* bound to (conj *testing-vars* v).",
   :var-type "function",
   :line 635,
   :file "src/clj/clojure/test.clj"}
  {:arglists ([string & body]),
   :name "testing",
   :namespace "clojure.test",
   :source-url
   "https://github.com/clojure/clojure/blob/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj#L534",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.test-api.html#clojure.test/testing",
   :doc
   "Adds a new string to the list of testing contexts.  May be nested,\nbut must occur inside a test function (deftest).",
   :var-type "macro",
   :line 534,
   :file "src/clj/clojure/test.clj"}
  {:arglists ([]),
   :name "testing-contexts-str",
   :namespace "clojure.test",
   :source-url
   "https://github.com/clojure/clojure/blob/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj#L294",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.test-api.html#clojure.test/testing-contexts-str",
   :doc
   "Returns a string representation of the current test context. Joins\nstrings in *testing-contexts* with spaces.",
   :var-type "function",
   :line 294,
   :file "src/clj/clojure/test.clj"}
  {:arglists ([]),
   :name "testing-vars-str",
   :namespace "clojure.test",
   :source-url
   "https://github.com/clojure/clojure/blob/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj#L282",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.test-api.html#clojure.test/testing-vars-str",
   :doc
   "Returns a string representation of the current test.  Renders names\nin *testing-vars* as a list, then the source file and line of\ncurrent assertion.",
   :var-type "function",
   :line 282,
   :file "src/clj/clojure/test.clj"}
  {:arglists ([msg form]),
   :name "try-expr",
   :namespace "clojure.test",
   :source-url
   "https://github.com/clojure/clojure/blob/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj#L486",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.test-api.html#clojure.test/try-expr",
   :doc
   "Used by the 'is' macro to catch unexpected exceptions.\nYou don't call this.",
   :var-type "macro",
   :line 486,
   :file "src/clj/clojure/test.clj"}
  {:file "src/clj/clojure/test.clj",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj#L601",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.test-api.html#clojure.test/use-fixtures",
   :namespace "clojure.test",
   :line 601,
   :var-type "multimethod",
   :doc
   "Wrap test runs in a fixture function to perform setup and\nteardown. Using a fixture-type of :each wraps every test\nindividually, while:once wraps the whole run in a single function.",
   :name "use-fixtures"}
  {:arglists ([definition & body]),
   :name "with-test",
   :namespace "clojure.test",
   :source-url
   "https://github.com/clojure/clojure/blob/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj#L545",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.test-api.html#clojure.test/with-test",
   :doc
   "Takes any definition form (that returns a Var) as the first argument.\nRemaining body goes in the :test metadata function for that Var.\n\nWhen *load-tests* is false, only evaluates the definition, ignoring\nthe tests.",
   :var-type "macro",
   :line 545,
   :file "src/clj/clojure/test.clj"}
  {:arglists ([& body]),
   :name "with-test-out",
   :namespace "clojure.test",
   :source-url
   "https://github.com/clojure/clojure/blob/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj#L265",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/607389029cfec50f32b73c00a6f66d0a1dbcda23/src/clj/clojure/test.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.test-api.html#clojure.test/with-test-out",
   :doc "Runs body with *out* bound to the value of *test-out*.",
   :var-type "macro",
   :line 265,
   :file "src/clj/clojure/test.clj"}
  {:arglists ([m]),
   :name "keywordize-keys",
   :namespace "clojure.walk",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/walk.clj#L90",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/walk.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.walk-api.html#clojure.walk/keywordize-keys",
   :doc
   "Recursively transforms all map keys from strings to keywords.",
   :var-type "function",
   :line 90,
   :file "src/clj/clojure/walk.clj"}
  {:arglists ([form]),
   :name "macroexpand-all",
   :namespace "clojure.walk",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/walk.clj#L118",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/walk.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.walk-api.html#clojure.walk/macroexpand-all",
   :doc "Recursively performs all possible macroexpansions in form.",
   :var-type "function",
   :line 118,
   :file "src/clj/clojure/walk.clj"}
  {:arglists ([f form]),
   :name "postwalk",
   :namespace "clojure.walk",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/walk.clj#L52",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/walk.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.walk-api.html#clojure.walk/postwalk",
   :doc
   "Performs a depth-first, post-order traversal of form.  Calls f on\neach sub-form, uses f's return value in place of the original.\nRecognizes all Clojure data structures except sorted-map-by.\nConsumes seqs as with doall.",
   :var-type "function",
   :line 52,
   :file "src/clj/clojure/walk.clj"}
  {:arglists ([form]),
   :name "postwalk-demo",
   :namespace "clojure.walk",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/walk.clj#L78",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/walk.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.walk-api.html#clojure.walk/postwalk-demo",
   :doc
   "Demonstrates the behavior of postwalk by printing each form as it is\nwalked.  Returns form.",
   :var-type "function",
   :line 78,
   :file "src/clj/clojure/walk.clj"}
  {:arglists ([smap form]),
   :name "postwalk-replace",
   :namespace "clojure.walk",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/walk.clj#L111",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/walk.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.walk-api.html#clojure.walk/postwalk-replace",
   :doc
   "Recursively transforms form by replacing keys in smap with their\nvalues.  Like clojure/replace but works on any data structure.  Does\nreplacement at the leaves of the tree first.",
   :var-type "function",
   :line 111,
   :file "src/clj/clojure/walk.clj"}
  {:arglists ([f form]),
   :name "prewalk",
   :namespace "clojure.walk",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/walk.clj#L60",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/walk.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.walk-api.html#clojure.walk/prewalk",
   :doc "Like postwalk, but does pre-order traversal.",
   :var-type "function",
   :line 60,
   :file "src/clj/clojure/walk.clj"}
  {:arglists ([form]),
   :name "prewalk-demo",
   :namespace "clojure.walk",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/walk.clj#L84",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/walk.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.walk-api.html#clojure.walk/prewalk-demo",
   :doc
   "Demonstrates the behavior of prewalk by printing each form as it is\nwalked.  Returns form.",
   :var-type "function",
   :line 84,
   :file "src/clj/clojure/walk.clj"}
  {:arglists ([smap form]),
   :name "prewalk-replace",
   :namespace "clojure.walk",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/walk.clj#L104",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/walk.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.walk-api.html#clojure.walk/prewalk-replace",
   :doc
   "Recursively transforms form by replacing keys in smap with their\nvalues.  Like clojure/replace but works on any data structure.  Does\nreplacement at the root of the tree first.",
   :var-type "function",
   :line 104,
   :file "src/clj/clojure/walk.clj"}
  {:arglists ([m]),
   :name "stringify-keys",
   :namespace "clojure.walk",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/walk.clj#L97",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/walk.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.walk-api.html#clojure.walk/stringify-keys",
   :doc
   "Recursively transforms all map keys from keywords to strings.",
   :var-type "function",
   :line 97,
   :file "src/clj/clojure/walk.clj"}
  {:arglists ([inner outer form]),
   :name "walk",
   :namespace "clojure.walk",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/walk.clj#L35",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/walk.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.walk-api.html#clojure.walk/walk",
   :doc
   "Traverses form, an arbitrary data structure.  inner and outer are\nfunctions.  Applies inner to each element of form, building up a\ndata structure of the same type, then applies outer to the result.\nRecognizes all Clojure data structures except sorted-map-by.\nConsumes seqs as with doall.",
   :var-type "function",
   :line 35,
   :file "src/clj/clojure/walk.clj"}
  {:arglists ([s] [s startparse]),
   :name "parse",
   :namespace "clojure.xml",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/xml.clj#L78",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/xml.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.xml-api.html#clojure.xml/parse",
   :doc
   "Parses and loads the source s, which can be a File, InputStream or\nString naming a URI. Returns a tree of the xml/element struct-map,\nwhich has the keys :tag, :attrs, and :content. and accessor fns tag,\nattrs, and content. Other parsers can be supplied by passing\nstartparse, a fn taking a source and a ContentHandler and returning\na parser",
   :var-type "function",
   :line 78,
   :file "src/clj/clojure/xml.clj"}
  {:arglists ([loc item]),
   :name "append-child",
   :namespace "clojure.zip",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj#L200",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.zip-api.html#clojure.zip/append-child",
   :doc
   "Inserts the item as the rightmost child of the node at this loc,\nwithout moving",
   :var-type "function",
   :line 200,
   :file "src/clj/clojure/zip.clj"}
  {:arglists ([loc]),
   :name "branch?",
   :namespace "clojure.zip",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj#L64",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.zip-api.html#clojure.zip/branch?",
   :doc "Returns true if the node at loc is a branch",
   :var-type "function",
   :line 64,
   :file "src/clj/clojure/zip.clj"}
  {:arglists ([loc]),
   :name "children",
   :namespace "clojure.zip",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj#L69",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.zip-api.html#clojure.zip/children",
   :doc
   "Returns a seq of the children of node at loc, which must be a branch",
   :var-type "function",
   :line 69,
   :file "src/clj/clojure/zip.clj"}
  {:arglists ([loc]),
   :name "down",
   :namespace "clojure.zip",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj#L98",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.zip-api.html#clojure.zip/down",
   :doc
   "Returns the loc of the leftmost child of the node at this loc, or\nnil if no children",
   :var-type "function",
   :line 98,
   :file "src/clj/clojure/zip.clj"}
  {:arglists ([loc f & args]),
   :name "edit",
   :namespace "clojure.zip",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj#L189",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.zip-api.html#clojure.zip/edit",
   :doc
   "Replaces the node at this loc with the value of (f node args)",
   :var-type "function",
   :line 189,
   :file "src/clj/clojure/zip.clj"}
  {:arglists ([loc]),
   :name "end?",
   :namespace "clojure.zip",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj#L232",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.zip-api.html#clojure.zip/end?",
   :doc "Returns true if loc represents the end of a depth-first walk",
   :var-type "function",
   :line 232,
   :file "src/clj/clojure/zip.clj"}
  {:arglists ([loc item]),
   :name "insert-child",
   :namespace "clojure.zip",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj#L194",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.zip-api.html#clojure.zip/insert-child",
   :doc
   "Inserts the item as the leftmost child of the node at this loc,\nwithout moving",
   :var-type "function",
   :line 194,
   :file "src/clj/clojure/zip.clj"}
  {:arglists ([loc item]),
   :name "insert-left",
   :namespace "clojure.zip",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj#L165",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.zip-api.html#clojure.zip/insert-left",
   :doc
   "Inserts the item as the left sibling of the node at this loc,\nwithout moving",
   :var-type "function",
   :line 165,
   :file "src/clj/clojure/zip.clj"}
  {:arglists ([loc item]),
   :name "insert-right",
   :namespace "clojure.zip",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj#L174",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.zip-api.html#clojure.zip/insert-right",
   :doc
   "Inserts the item as the right sibling of the node at this loc,\nwithout moving",
   :var-type "function",
   :line 174,
   :file "src/clj/clojure/zip.clj"}
  {:arglists ([loc]),
   :name "left",
   :namespace "clojure.zip",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj#L150",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.zip-api.html#clojure.zip/left",
   :doc
   "Returns the loc of the left sibling of the node at this loc, or nil",
   :var-type "function",
   :line 150,
   :file "src/clj/clojure/zip.clj"}
  {:arglists ([loc]),
   :name "leftmost",
   :namespace "clojure.zip",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj#L157",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.zip-api.html#clojure.zip/leftmost",
   :doc
   "Returns the loc of the leftmost sibling of the node at this loc, or self",
   :var-type "function",
   :line 157,
   :file "src/clj/clojure/zip.clj"}
  {:arglists ([loc]),
   :name "lefts",
   :namespace "clojure.zip",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj#L87",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.zip-api.html#clojure.zip/lefts",
   :doc "Returns a seq of the left siblings of this loc",
   :var-type "function",
   :line 87,
   :file "src/clj/clojure/zip.clj"}
  {:arglists ([loc node children]),
   :name "make-node",
   :namespace "clojure.zip",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj#L76",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.zip-api.html#clojure.zip/make-node",
   :doc
   "Returns a new branch node, given an existing node and new\nchildren. The loc is only used to supply the constructor.",
   :var-type "function",
   :line 76,
   :file "src/clj/clojure/zip.clj"}
  {:arglists ([loc]),
   :name "next",
   :namespace "clojure.zip",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj#L206",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.zip-api.html#clojure.zip/next",
   :doc
   "Moves to the next loc in the hierarchy, depth-first. When reaching\nthe end, returns a distinguished loc detectable via end?. If already\nat the end, stays there.",
   :var-type "function",
   :line 206,
   :file "src/clj/clojure/zip.clj"}
  {:arglists ([loc]),
   :name "node",
   :namespace "clojure.zip",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj#L60",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.zip-api.html#clojure.zip/node",
   :doc "Returns the node at loc",
   :var-type "function",
   :line 60,
   :file "src/clj/clojure/zip.clj"}
  {:arglists ([loc]),
   :name "path",
   :namespace "clojure.zip",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj#L82",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.zip-api.html#clojure.zip/path",
   :doc "Returns a seq of nodes leading to this loc",
   :var-type "function",
   :line 82,
   :file "src/clj/clojure/zip.clj"}
  {:arglists ([loc]),
   :name "prev",
   :namespace "clojure.zip",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj#L221",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.zip-api.html#clojure.zip/prev",
   :doc
   "Moves to the previous loc in the hierarchy, depth-first. If already\nat the root, returns nil.",
   :var-type "function",
   :line 221,
   :file "src/clj/clojure/zip.clj"}
  {:arglists ([loc]),
   :name "remove",
   :namespace "clojure.zip",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj#L237",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.zip-api.html#clojure.zip/remove",
   :doc
   "Removes the node at loc, returning the loc that would have preceded\nit in a depth-first walk.",
   :var-type "function",
   :line 237,
   :file "src/clj/clojure/zip.clj"}
  {:arglists ([loc node]),
   :name "replace",
   :namespace "clojure.zip",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj#L183",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.zip-api.html#clojure.zip/replace",
   :doc "Replaces the node at this loc, without moving",
   :var-type "function",
   :line 183,
   :file "src/clj/clojure/zip.clj"}
  {:arglists ([loc]),
   :name "right",
   :namespace "clojure.zip",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj#L135",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.zip-api.html#clojure.zip/right",
   :doc
   "Returns the loc of the right sibling of the node at this loc, or nil",
   :var-type "function",
   :line 135,
   :file "src/clj/clojure/zip.clj"}
  {:arglists ([loc]),
   :name "rightmost",
   :namespace "clojure.zip",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj#L142",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.zip-api.html#clojure.zip/rightmost",
   :doc
   "Returns the loc of the rightmost sibling of the node at this loc, or self",
   :var-type "function",
   :line 142,
   :file "src/clj/clojure/zip.clj"}
  {:arglists ([loc]),
   :name "rights",
   :namespace "clojure.zip",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj#L92",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.zip-api.html#clojure.zip/rights",
   :doc "Returns a seq of the right siblings of this loc",
   :var-type "function",
   :line 92,
   :file "src/clj/clojure/zip.clj"}
  {:arglists ([loc]),
   :name "root",
   :namespace "clojure.zip",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj#L124",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.zip-api.html#clojure.zip/root",
   :doc
   "zips all the way up and returns the root node, reflecting any\nchanges.",
   :var-type "function",
   :line 124,
   :file "src/clj/clojure/zip.clj"}
  {:arglists ([root]),
   :name "seq-zip",
   :namespace "clojure.zip",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj#L34",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.zip-api.html#clojure.zip/seq-zip",
   :doc "Returns a zipper for nested sequences, given a root sequence",
   :var-type "function",
   :line 34,
   :file "src/clj/clojure/zip.clj"}
  {:arglists ([loc]),
   :name "up",
   :namespace "clojure.zip",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj#L111",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.zip-api.html#clojure.zip/up",
   :doc
   "Returns the loc of the parent of the node at this loc, or nil if at\nthe top",
   :var-type "function",
   :line 111,
   :file "src/clj/clojure/zip.clj"}
  {:arglists ([root]),
   :name "vector-zip",
   :namespace "clojure.zip",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj#L42",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.zip-api.html#clojure.zip/vector-zip",
   :doc "Returns a zipper for nested vectors, given a root vector",
   :var-type "function",
   :line 42,
   :file "src/clj/clojure/zip.clj"}
  {:arglists ([root]),
   :name "xml-zip",
   :namespace "clojure.zip",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj#L50",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.zip-api.html#clojure.zip/xml-zip",
   :doc
   "Returns a zipper for xml elements (as from xml/parse),\ngiven a root element",
   :var-type "function",
   :line 50,
   :file "src/clj/clojure/zip.clj"}
  {:arglists ([branch? children make-node root]),
   :name "zipper",
   :namespace "clojure.zip",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj#L18",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/zip.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.zip-api.html#clojure.zip/zipper",
   :doc
   "Creates a new zipper structure. \n\nbranch? is a fn that, given a node, returns true if can have\nchildren, even if it currently doesn't.\n\nchildren is a fn that, given a branch node, returns a seq of its\nchildren.\n\nmake-node is a fn that, given an existing node and a seq of\nchildren, returns a new branch node with the supplied children.\nroot is the root node.",
   :var-type "function",
   :line 18,
   :file "src/clj/clojure/zip.clj"}
  {:arglists ([& body]),
   :name "with-junit-output",
   :namespace "clojure.test.junit",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/test/junit.clj#L182",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/test/junit.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.test-api.html#clojure.test.junit/with-junit-output",
   :doc
   "Execute body with modified test-is reporting functions that write\nJUnit-compatible XML output.",
   :var-type "macro",
   :line 182,
   :file "src/clj/clojure/test/junit.clj"}
  {:arglists ([data]),
   :name "print-tap-diagnostic",
   :namespace "clojure.test.tap",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/test/tap.clj#L50",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/test/tap.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.test-api.html#clojure.test.tap/print-tap-diagnostic",
   :doc
   "Prints a TAP diagnostic line.  data is a (possibly multi-line)\nstring.",
   :var-type "function",
   :line 50,
   :file "src/clj/clojure/test/tap.clj"}
  {:arglists ([msg]),
   :name "print-tap-fail",
   :namespace "clojure.test.tap",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/test/tap.clj#L62",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/test/tap.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.test-api.html#clojure.test.tap/print-tap-fail",
   :doc
   "Prints a TAP 'not ok' line.  msg is a string, with no line breaks",
   :var-type "function",
   :line 62,
   :file "src/clj/clojure/test/tap.clj"}
  {:arglists ([msg]),
   :name "print-tap-pass",
   :namespace "clojure.test.tap",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/test/tap.clj#L57",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/test/tap.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.test-api.html#clojure.test.tap/print-tap-pass",
   :doc
   "Prints a TAP 'ok' line.  msg is a string, with no line breaks",
   :var-type "function",
   :line 57,
   :file "src/clj/clojure/test/tap.clj"}
  {:arglists ([n]),
   :name "print-tap-plan",
   :namespace "clojure.test.tap",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/test/tap.clj#L45",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/test/tap.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.test-api.html#clojure.test.tap/print-tap-plan",
   :doc
   "Prints a TAP plan line like '1..n'.  n is the number of tests",
   :var-type "function",
   :line 45,
   :file "src/clj/clojure/test/tap.clj"}
  {:arglists ([& body]),
   :name "with-tap-output",
   :namespace "clojure.test.tap",
   :source-url
   "https://github.com/clojure/clojure/blob/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/test/tap.clj#L106",
   :raw-source-url
   "https://github.com/clojure/clojure/raw/76e7c4317dc3eac80c4908ac5e5fb885e302b2a4/src/clj/clojure/test/tap.clj",
   :wiki-url
   "http://clojure.github.com/clojure//clojure.test-api.html#clojure.test.tap/with-tap-output",
   :doc
   "Execute body with modified test reporting functions that produce\nTAP output",
   :var-type "macro",
   :line 106,
   :file "src/clj/clojure/test/tap.clj"})}
