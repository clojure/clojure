{:namespaces
 ({:doc "Fundamental library of the Clojure language",
   :name "clojure.core",
   :wiki-url "https://clojure.github.io/clojure/clojure.core-api.html",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj"}
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
   "https://github.com/clojure/clojure/blob/385d0593efa658ada19f9a55af39cef146c75341/src/clj/clojure/instant.clj"}
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
   "https://github.com/clojure/clojure/blob/ee1b606ad066ac8df2efd4a6b8d0d365c206f5bf/src/clj/clojure/java/io.clj"}
  {:doc "A repl helper to quickly open javadocs.",
   :author "Christophe Grand, Stuart Sierra",
   :name "clojure.java.javadoc",
   :wiki-url
   "https://clojure.github.io/clojure/clojure.java.javadoc-api.html",
   :source-url
   "https://github.com/clojure/clojure/blob/3b6c31bc503afe8f25d01d6d7d05ebc960095abd/src/clj/clojure/java/javadoc.clj"}
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
   "https://github.com/clojure/clojure/blob/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj"}
  {:doc
   "Clojure wrapper functions for java.lang.Math static methods.\n\nFunction calls are inlined for performance, and type hinted for primitive\nlong or double parameters where appropriate. In general, Math methods are\noptimized for performance and have bounds for error tolerance. If\ngreater precision is needed, use java.lang.StrictMath directly instead.\n\nFor more complete information, see:\nhttps://docs.oracle.com/javase/8/docs/api/java/lang/Math.html",
   :author "Alex Miller",
   :name "clojure.math",
   :wiki-url "https://clojure.github.io/clojure/clojure.math-api.html",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj"}
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
   "A unit testing framework.\n\nASSERTIONS\n\nThe core of the library is the \"is\" macro, which lets you make\nassertions of any arbitrary expression:\n\n(is (= 4 (+ 2 2)))\n(is (instance? Integer 256))\n(is (.startsWith \"abcde\" \"ab\"))\n\nYou can type an \"is\" expression directly at the REPL, which will\nprint a message if it fails.\n\n    user> (is (= 5 (+ 2 2)))\n\n    FAIL in  (:1)\n    expected: (= 5 (+ 2 2))\n      actual: (not (= 5 4))\n    false\n\nThe \"expected:\" line shows you the original expression, and the\n\"actual:\" shows you what actually happened.  In this case, it\nshows that (+ 2 2) returned 4, which is not = to 5.  Finally, the\n\"false\" on the last line is the value returned from the\nexpression.  The \"is\" macro always returns the result of the\ninner expression.\n\nThere are two special assertions for testing exceptions.  The\n\"(is (thrown? c ...))\" form tests if an exception of class c is\nthrown:\n\n(is (thrown? ArithmeticException (/ 1 0))) \n\n\"(is (thrown-with-msg? c re ...))\" does the same thing and also\ntests that the message on the exception matches the regular\nexpression re:\n\n(is (thrown-with-msg? ArithmeticException #\"Divide by zero\"\n                      (/ 1 0)))\n\nDOCUMENTING TESTS\n\n\"is\" takes an optional second argument, a string describing the\nassertion.  This message will be included in the error report.\n\n(is (= 5 (+ 2 2)) \"Crazy arithmetic\")\n\nIn addition, you can document groups of assertions with the\n\"testing\" macro, which takes a string followed by any number of\nassertions.  The string will be included in failure reports.\nCalls to \"testing\" may be nested, and all of the strings will be\njoined together with spaces in the final report, in a style\nsimilar to RSpec <http://rspec.info/>\n\n(testing \"Arithmetic\"\n  (testing \"with positive integers\"\n    (is (= 4 (+ 2 2)))\n    (is (= 7 (+ 3 4))))\n  (testing \"with negative integers\"\n    (is (= -4 (+ -2 -2)))\n    (is (= -1 (+ 3 -4)))))\n\nNote that, unlike RSpec, the \"testing\" macro may only be used\nINSIDE a \"deftest\" or \"with-test\" form (see below).\n\n\nDEFINING TESTS\n\nThere are two ways to define tests.  The \"with-test\" macro takes\na defn or def form as its first argument, followed by any number\nof assertions.  The tests will be stored as metadata on the\ndefinition.\n\n(with-test\n    (defn my-function [x y]\n      (+ x y))\n  (is (= 4 (my-function 2 2)))\n  (is (= 7 (my-function 3 4))))\n\nAs of Clojure SVN rev. 1221, this does not work with defmacro.\nSee http://code.google.com/p/clojure/issues/detail?id=51\n\nThe other way lets you define tests separately from the rest of\nyour code, even in a different namespace:\n\n(deftest addition\n  (is (= 4 (+ 2 2)))\n  (is (= 7 (+ 3 4))))\n\n(deftest subtraction\n  (is (= 1 (- 4 3)))\n  (is (= 3 (- 7 4))))\n\nThis creates functions named \"addition\" and \"subtraction\", which\ncan be called like any other function.  Therefore, tests can be\ngrouped and composed, in a style similar to the test framework in\nPeter Seibel's \"Practical Common Lisp\"\n<http://www.gigamonkeys.com/book/practical-building-a-unit-test-framework.html>\n\n(deftest arithmetic\n  (addition)\n  (subtraction))\n\nThe names of the nested tests will be joined in a list, like\n\"(arithmetic addition)\", in failure reports.  You can use nested\ntests to set up a context shared by several tests.\n\n\nRUNNING TESTS\n\nRun tests with the function \"(run-tests namespaces...)\":\n\n(run-tests 'your.namespace 'some.other.namespace)\n\nIf you don't specify any namespaces, the current namespace is\nused.  To run all tests in all namespaces, use \"(run-all-tests)\".\n\nBy default, these functions will search for all tests defined in\na namespace and run them in an undefined order.  However, if you\nare composing tests, as in the \"arithmetic\" example above, you\nprobably do not want the \"addition\" and \"subtraction\" tests run\nseparately.  In that case, you must define a special function\nnamed \"test-ns-hook\" that runs your tests in the correct order:\n\n(defn test-ns-hook []\n  (arithmetic))\n\nNote: test-ns-hook prevents execution of fixtures (see below).\n\n\nOMITTING TESTS FROM PRODUCTION CODE\n\nYou can bind the variable \"*load-tests*\" to false when loading or\ncompiling code in production.  This will prevent any tests from\nbeing created by \"with-test\" or \"deftest\".\n\n\nFIXTURES\n\nFixtures allow you to run code before and after tests, to set up\nthe context in which tests should be run.\n\nA fixture is just a function that calls another function passed as\nan argument.  It looks like this:\n\n(defn my-fixture [f]\n   Perform setup, establish bindings, whatever.\n  (f)  Then call the function we were passed.\n   Tear-down / clean-up code here.\n )\n\nFixtures are attached to namespaces in one of two ways.  \"each\"\nfixtures are run repeatedly, once for each test function created\nwith \"deftest\" or \"with-test\".  \"each\" fixtures are useful for\nestablishing a consistent before/after state for each test, like\nclearing out database tables.\n\n\"each\" fixtures can be attached to the current namespace like this:\n(use-fixtures :each fixture1 fixture2 ...)\nThe fixture1, fixture2 are just functions like the example above.\nThey can also be anonymous functions, like this:\n(use-fixtures :each (fn [f] setup... (f) cleanup...))\n\nThe other kind of fixture, a \"once\" fixture, is only run once,\naround ALL the tests in the namespace.  \"once\" fixtures are useful\nfor tasks that only need to be performed once, like establishing\ndatabase connections, or for time-consuming tasks.\n\nAttach \"once\" fixtures to the current namespace like this:\n(use-fixtures :once fixture1 fixture2 ...)\n\nNote: Fixtures and test-ns-hook are mutually incompatible.  If you\nare using test-ns-hook, fixture functions will *never* be run.\n\n\nSAVING TEST OUTPUT TO A FILE\n\nAll the test reporting functions write to the var *test-out*.  By\ndefault, this is the same as *out*, but you can rebind it to any\nPrintWriter.  For example, it could be a file opened with\nclojure.java.io/writer.\n\n\nEXTENDING TEST-IS (ADVANCED)\n\nYou can extend the behavior of the \"is\" macro by defining new\nmethods for the \"assert-expr\" multimethod.  These methods are\ncalled during expansion of the \"is\" macro, so they should return\nquoted forms to be evaluated.\n\nYou can plug in your own test-reporting framework by rebinding\nthe \"report\" function: (report event)\n\nThe 'event' argument is a map.  It will always have a :type key,\nwhose value will be a keyword signaling the type of event being\nreported.  Standard events with :type value of :pass, :fail, and\n:error are called when an assertion passes, fails, and throws an\nexception, respectively.  In that case, the event will also have\nthe following keys:\n\n  :expected   The form that was expected to be true\n  :actual     A form representing what actually occurred\n  :message    The string message given as an argument to 'is'\n\nThe \"testing\" strings will be a list in \"*testing-contexts*\", and\nthe vars being tested will be a list in \"*testing-vars*\".\n\nYour \"report\" function should wrap any printing calls in the\n\"with-test-out\" macro, which rebinds *out* to the current value\nof *test-out*.\n\nFor additional event types, see the examples in the code.",
   :author
   "Stuart Sierra, with contributions and suggestions by \n  Chas Emerick, Allen Rohner, and Stuart Halloway",
   :name "clojure.test",
   :wiki-url "https://clojure.github.io/clojure/clojure.test-api.html",
   :source-url
   "https://github.com/clojure/clojure/blob/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj"}
  {:doc
   "This file defines a generic tree walker for Clojure data\nstructures.  It takes any data structure (list, vector, map, set,\nseq), calls a function on every element, and uses the return value\nof the function in place of the original.  This makes it fairly\neasy to write recursive search-and-replace functions, as shown in\nthe examples.\n\nNote: \"walk\" supports all Clojure data structures EXCEPT maps\ncreated with sorted-map-by.  There is no (obvious) way to retrieve\nthe sorting function.",
   :author "Stuart Sierra",
   :name "clojure.walk",
   :wiki-url "https://clojure.github.io/clojure/clojure.walk-api.html",
   :source-url
   "https://github.com/clojure/clojure/blob/b8c78ebf79b6a996f349dd112aaed658c132735d/src/clj/clojure/walk.clj"}
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
   "https://github.com/clojure/clojure/blob/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj"}
  {:doc nil,
   :name "clojure.core.protocols",
   :wiki-url
   "https://clojure.github.io/clojure/clojure.core-api.html#clojure.core.protocols",
   :source-url
   "https://github.com/clojure/clojure/blob/2cc37bb56a9125a1829c73c505e32995e663059a/src/clj/clojure/core/protocols.clj"}
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
   "https://github.com/clojure/clojure/blob/fc98f92c76254c5a6306debaf0f9df28c3bb3646/src/clj/clojure/core/server.clj"}
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
 ({:raw-source-url nil,
   :added "1.0",
   :name "&",
   :file nil,
   :source-url nil,
   :var-type "special syntax",
   :arglists nil,
   :doc
   "Syntax for use with fn.\n\nPlease see https://clojure.org/reference/special_forms#fn",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/&"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.2",
   :name "*",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1010",
   :line 1010,
   :var-type "function",
   :arglists ([] [x] [x y] [x y & more]),
   :doc
   "Returns the product of nums. (*) returns 1. Does not auto-promote\nlongs, will throw on overflow. See also: *'",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "*'",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L998",
   :line 998,
   :var-type "function",
   :arglists ([] [x] [x y] [x y & more]),
   :doc
   "Returns the product of nums. (*') returns 1. Supports arbitrary precision.\nSee also: *",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*'"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "*1",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6315",
   :dynamic true,
   :line 6315,
   :var-type "var",
   :arglists nil,
   :doc "bound in a repl thread to the most recent value printed",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*1"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "*2",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6320",
   :dynamic true,
   :line 6320,
   :var-type "var",
   :arglists nil,
   :doc
   "bound in a repl thread to the second most recent value printed",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*2"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "*3",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6325",
   :dynamic true,
   :line 6325,
   :var-type "var",
   :arglists nil,
   :doc
   "bound in a repl thread to the third most recent value printed",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*3"}
  {:raw-source-url nil,
   :added "1.0",
   :name "*agent*",
   :file nil,
   :source-url nil,
   :var-type "var",
   :arglists nil,
   :doc
   "The agent currently running an action on this thread, else nil",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*agent*"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "*clojure-version*",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7138",
   :dynamic true,
   :line 7138,
   :var-type "var",
   :arglists nil,
   :doc
   "The version info for Clojure core, as a map containing :major :minor \n:incremental and :qualifier keys. Feature releases may increment \n:minor and/or :major, bugfix releases will increment :incremental. \nPossible values of :qualifier include \"GA\", \"SNAPSHOT\", \"RC-x\" \"BETA-x\"",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*clojure-version*"}
  {:raw-source-url nil,
   :added "1.0",
   :name "*command-line-args*",
   :file nil,
   :source-url nil,
   :var-type "var",
   :arglists nil,
   :doc
   "A sequence of the supplied command line arguments, or nil if\nnone were supplied",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*command-line-args*"}
  {:raw-source-url nil,
   :added "1.0",
   :name "*compile-files*",
   :file nil,
   :source-url nil,
   :var-type "var",
   :arglists nil,
   :doc "Set to true when compiling files, false otherwise.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*compile-files*"}
  {:raw-source-url nil,
   :added "1.0",
   :name "*compile-path*",
   :file nil,
   :source-url nil,
   :var-type "var",
   :arglists nil,
   :doc
   "Specifies the directory where 'compile' will write out .class\nfiles. This directory must be in the classpath for 'compile' to\nwork.\n\nDefaults to \"classes\"",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*compile-path*"}
  {:raw-source-url nil,
   :added "1.4",
   :name "*compiler-options*",
   :file nil,
   :source-url nil,
   :var-type "var",
   :arglists nil,
   :doc
   "A map of keys to options.\nNote, when binding dynamically make sure to merge with previous value.\nSupported options:\n:elide-meta - a collection of metadata keys to elide during compilation.\n:disable-locals-clearing - set to true to disable clearing, useful for using a debugger\n:direct-linking - set to true to use direct static invocation of functions, rather than vars\n  Note that call sites compiled with direct linking will not be affected by var redefinition.\n  Use ^:redef (or ^:dynamic) on a var to prevent direct linking and allow redefinition.\nSee https://clojure.org/reference/compilation for more information.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*compiler-options*"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.4",
   :name "*data-readers*",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7873",
   :dynamic true,
   :line 7873,
   :var-type "var",
   :arglists nil,
   :doc
   "Map from reader tag symbols to data reader Vars.\n\nWhen Clojure starts, it searches for files named 'data_readers.clj'\nand 'data_readers.cljc' at the root of the classpath. Each such file\nmust contain a literal map of symbols, like this:\n\n    {foo/bar my.project.foo/bar\n     foo/baz my.project/baz}\n\nThe first symbol in each pair is a tag that will be recognized by\nthe Clojure reader. The second symbol in the pair is the\nfully-qualified name of a Var which will be invoked by the reader to\nparse the form following the tag. For example, given the\ndata_readers.clj file above, the Clojure reader would parse this\nform:\n\n    #foo/bar [1 2 3]\n\nby invoking the Var #'my.project.foo/bar on the vector [1 2 3]. The\ndata reader function is invoked on the form AFTER it has been read\nas a normal Clojure data structure by the reader.\n\nReader tags without namespace qualifiers are reserved for\nClojure. Default reader tags are defined in\nclojure.core/default-data-readers but may be overridden in\ndata_readers.clj, data_readers.cljc, or by rebinding this Var.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*data-readers*"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.5",
   :name "*default-data-reader-fn*",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7902",
   :dynamic true,
   :line 7902,
   :var-type "var",
   :arglists nil,
   :doc
   "When no data reader is found for a tag and *default-data-reader-fn*\nis non-nil, it will be called with two arguments,\nthe tag and the value.  If *default-data-reader-fn* is nil (the\ndefault), an exception will be thrown for the unknown tag.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*default-data-reader-fn*"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "*e",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6330",
   :dynamic true,
   :line 6330,
   :var-type "var",
   :arglists nil,
   :doc
   "bound in a repl thread to the most recent exception caught by the repl",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*e"}
  {:raw-source-url nil,
   :added "1.0",
   :name "*err*",
   :file nil,
   :source-url nil,
   :var-type "var",
   :arglists nil,
   :doc
   "A java.io.Writer object representing standard error for print operations.\n\nDefaults to System/err, wrapped in a PrintWriter",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*err*"}
  {:raw-source-url nil,
   :added "1.0",
   :name "*file*",
   :file nil,
   :source-url nil,
   :var-type "var",
   :arglists nil,
   :doc
   "The path of the file being evaluated, as a String.\n\nWhen there is no file, e.g. in the REPL, the value is not defined.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*file*"}
  {:raw-source-url nil,
   :added "1.0",
   :name "*flush-on-newline*",
   :file nil,
   :source-url nil,
   :var-type "var",
   :arglists nil,
   :doc
   "When set to true, output will be flushed whenever a newline is printed.\n\nDefaults to true.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*flush-on-newline*"}
  {:raw-source-url nil,
   :added "1.0",
   :name "*in*",
   :file nil,
   :source-url nil,
   :var-type "var",
   :arglists nil,
   :doc
   "A java.io.Reader object representing standard input for read operations.\n\nDefaults to System/in, wrapped in a LineNumberingPushbackReader",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*in*"}
  {:raw-source-url nil,
   :added "1.0",
   :name "*ns*",
   :file nil,
   :source-url nil,
   :var-type "var",
   :arglists nil,
   :doc
   "A clojure.lang.Namespace object representing the current namespace.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*ns*"}
  {:raw-source-url nil,
   :added "1.0",
   :name "*out*",
   :file nil,
   :source-url nil,
   :var-type "var",
   :arglists nil,
   :doc
   "A java.io.Writer object representing standard output for print operations.\n\nDefaults to System/out, wrapped in an OutputStreamWriter",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*out*"}
  {:raw-source-url nil,
   :added "1.0",
   :name "*print-dup*",
   :file nil,
   :source-url nil,
   :var-type "var",
   :arglists nil,
   :doc
   "When set to logical true, objects will be printed in a way that preserves\ntheir type when read in later.\n\nDefaults to false.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*print-dup*"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/51261641817005f872f9b6a752c5aef0bb7a5be4/src/clj/clojure/core_print.clj",
   :added "1.0",
   :name "*print-length*",
   :file "src/clj/clojure/core_print.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/51261641817005f872f9b6a752c5aef0bb7a5be4/src/clj/clojure/core_print.clj#L16",
   :dynamic true,
   :line 16,
   :var-type "var",
   :arglists nil,
   :doc
   "*print-length* controls how many items of each collection the\nprinter will print. If it is bound to logical false, there is no\nlimit. Otherwise, it must be bound to an integer indicating the maximum\nnumber of items of each collection to print. If a collection contains\nmore items, the printer will print items up to the limit followed by\n'...' to represent the remaining items. The root binding is nil\nindicating no limit.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*print-length*"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/51261641817005f872f9b6a752c5aef0bb7a5be4/src/clj/clojure/core_print.clj",
   :added "1.0",
   :name "*print-level*",
   :file "src/clj/clojure/core_print.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/51261641817005f872f9b6a752c5aef0bb7a5be4/src/clj/clojure/core_print.clj#L27",
   :dynamic true,
   :line 27,
   :var-type "var",
   :arglists nil,
   :doc
   "*print-level* controls how many levels deep the printer will\nprint nested objects. If it is bound to logical false, there is no\nlimit. Otherwise, it must be bound to an integer indicating the maximum\nlevel to print. Each argument to print is at level 0; if an argument is a\ncollection, its items are at level 1; and so on. If an object is a\ncollection and is at a level greater than or equal to the value bound to\n*print-level*, the printer prints '#' to represent it. The root binding\nis nil indicating no limit.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*print-level*"}
  {:raw-source-url nil,
   :added "1.0",
   :name "*print-meta*",
   :file nil,
   :source-url nil,
   :var-type "var",
   :arglists nil,
   :doc
   "If set to logical true, when printing an object, its metadata will also\nbe printed in a form that can be read back by the reader.\n\nDefaults to false.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*print-meta*"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/51261641817005f872f9b6a752c5aef0bb7a5be4/src/clj/clojure/core_print.clj",
   :added "1.9",
   :name "*print-namespace-maps*",
   :file "src/clj/clojure/core_print.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/51261641817005f872f9b6a752c5aef0bb7a5be4/src/clj/clojure/core_print.clj#L41",
   :dynamic true,
   :line 41,
   :var-type "var",
   :arglists nil,
   :doc
   "*print-namespace-maps* controls whether the printer will print\nnamespace map literal syntax. It defaults to false, but the REPL binds\nto true.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*print-namespace-maps*"}
  {:raw-source-url nil,
   :added "1.0",
   :name "*print-readably*",
   :file nil,
   :source-url nil,
   :var-type "var",
   :arglists nil,
   :doc
   "When set to logical false, strings and characters will be printed with\nnon-alphanumeric characters converted to the appropriate escape sequences.\n\nDefaults to true",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*print-readably*"}
  {:raw-source-url nil,
   :added "1.0",
   :name "*read-eval*",
   :file nil,
   :source-url nil,
   :var-type "var",
   :arglists nil,
   :doc
   "Defaults to true (or value specified by system property, see below)\n***This setting implies that the full power of the reader is in play,\nincluding syntax that can cause code to execute. It should never be\nused with untrusted sources. See also: clojure.edn/read.***\n\nWhen set to logical false in the thread-local binding,\nthe eval reader (#=) and record/type literal syntax are disabled in read/load.\nExample (will fail): (binding [*read-eval* false] (read-string \"#=(* 2 21)\"))\n\nThe default binding can be controlled by the system property\n'clojure.read.eval' System properties can be set on the command line\nlike this:\n\njava -Dclojure.read.eval=false ...\n\nThe system property can also be set to 'unknown' via\n-Dclojure.read.eval=unknown, in which case the default binding\nis :unknown and all reads will fail in contexts where *read-eval*\nhas not been explicitly bound to either true or false. This setting\ncan be a useful diagnostic tool to ensure that all of your reads\noccur in considered contexts. You can also accomplish this in a\nparticular scope by binding *read-eval* to :unknown\n",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*read-eval*"}
  {:raw-source-url nil,
   :added "1.3",
   :name "*unchecked-math*",
   :file nil,
   :source-url nil,
   :var-type "var",
   :arglists nil,
   :doc
   "While bound to true, compilations of +, -, *, inc, dec and the\ncoercions will be done without overflow checks. While bound\nto :warn-on-boxed, same behavior as true, and a warning is emitted\nwhen compilation uses boxed math. Default: false.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*unchecked-math*"}
  {:raw-source-url nil,
   :added "1.0",
   :name "*warn-on-reflection*",
   :file nil,
   :source-url nil,
   :var-type "var",
   :arglists nil,
   :doc
   "When set to true, the compiler will emit warnings when reflection is\nneeded to resolve Java method calls or field accesses.\n\nDefaults to false.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/*warn-on-reflection*"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.2",
   :name "+",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L986",
   :line 986,
   :var-type "function",
   :arglists ([] [x] [x y] [x y & more]),
   :doc
   "Returns the sum of nums. (+) returns 0. Does not auto-promote\nlongs, will throw on overflow. See also: +'",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/+"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "+'",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L974",
   :line 974,
   :var-type "function",
   :arglists ([] [x] [x y] [x y & more]),
   :doc
   "Returns the sum of nums. (+') returns 0. Supports arbitrary precision.\nSee also: +",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/+'"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.2",
   :name "-",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1045",
   :line 1045,
   :var-type "function",
   :arglists ([x] [x y] [x y & more]),
   :doc
   "If no ys are supplied, returns the negation of x, else subtracts\nthe ys from x and returns the result. Does not auto-promote\nlongs, will throw on overflow. See also: -'",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/-"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "-'",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1033",
   :line 1033,
   :var-type "function",
   :arglists ([x] [x y] [x y & more]),
   :doc
   "If no ys are supplied, returns the negation of x, else subtracts\nthe ys from x and returns the result. Supports arbitrary precision.\nSee also: -",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/-'"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "->",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1694",
   :line 1694,
   :var-type "macro",
   :arglists ([x & forms]),
   :doc
   "Threads the expr through the forms. Inserts x as the\nsecond item in the first form, making a list of it if it is not a\nlist already. If there are more forms, inserts the first form as the\nsecond item in second form, etc.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/->"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.1",
   :name "->>",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1710",
   :line 1710,
   :var-type "macro",
   :arglists ([x & forms]),
   :doc
   "Threads the expr through the forms. Inserts x as the\nlast item in the first form, making a list of it if it is not a\nlist already. If there are more forms, inserts the first form as the\nlast item in second form, etc.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/->>"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/79ac8b88f83c818314790449dda6ace7db275b56/src/clj/clojure/gvec.clj",
   :name "->ArrayChunk",
   :file "src/clj/clojure/gvec.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/79ac8b88f83c818314790449dda6ace7db275b56/src/clj/clojure/gvec.clj#L37",
   :line 37,
   :var-type "function",
   :arglists ([am arr off end]),
   :doc
   "Positional factory function for class clojure.core.ArrayChunk.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/->ArrayChunk"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :name "->Eduction",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7751",
   :line 7751,
   :var-type "function",
   :arglists ([xform coll]),
   :doc "Positional factory function for class clojure.core.Eduction.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/->Eduction"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/79ac8b88f83c818314790449dda6ace7db275b56/src/clj/clojure/gvec.clj",
   :name "->Vec",
   :file "src/clj/clojure/gvec.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/79ac8b88f83c818314790449dda6ace7db275b56/src/clj/clojure/gvec.clj#L170",
   :line 170,
   :var-type "function",
   :arglists ([am cnt shift root tail _meta]),
   :doc "Positional factory function for class clojure.core.Vec.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/->Vec"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/79ac8b88f83c818314790449dda6ace7db275b56/src/clj/clojure/gvec.clj",
   :name "->VecNode",
   :file "src/clj/clojure/gvec.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/79ac8b88f83c818314790449dda6ace7db275b56/src/clj/clojure/gvec.clj#L18",
   :line 18,
   :var-type "function",
   :arglists ([edit arr]),
   :doc "Positional factory function for class clojure.core.VecNode.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/->VecNode"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/79ac8b88f83c818314790449dda6ace7db275b56/src/clj/clojure/gvec.clj",
   :name "->VecSeq",
   :file "src/clj/clojure/gvec.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/79ac8b88f83c818314790449dda6ace7db275b56/src/clj/clojure/gvec.clj#L59",
   :line 59,
   :var-type "function",
   :arglists ([am vec anode i offset _meta]),
   :doc "Positional factory function for class clojure.core.VecSeq.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/->VecSeq"}
  {:raw-source-url nil,
   :added "1.0",
   :name ".",
   :file nil,
   :source-url nil,
   :var-type "special form",
   :arglists nil,
   :doc
   "The instance member form works for both fields and methods.\nThey all expand into calls to the dot operator at macroexpansion time.\n\nPlease see https://clojure.org/java_interop#dot",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/.",
   :forms
   [(.instanceMember instance args*)
    (.instanceMember Classname args*)
    (Classname/staticMethod args*)
    Classname/staticField]}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "..",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1676",
   :line 1676,
   :var-type "macro",
   :arglists ([x form] [x form & more]),
   :doc
   "form => fieldName-symbol or (instanceMethodName-symbol args*)\n\nExpands into a member access (.) of the first member on the first\nargument, followed by the next member on the result, etc. For\ninstance:\n\n(.. System (getProperties) (get \"os.name\"))\n\nexpands to:\n\n(. (. System (getProperties)) (get \"os.name\"))\n\nbut is easier to write, read, and understand.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/.."}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "/",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1022",
   :line 1022,
   :var-type "function",
   :arglists ([x] [x y] [x y & more]),
   :doc
   "If no denominators are supplied, returns 1/numerator,\nelse returns numerator divided by all of the denominators.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core//"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "<",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L902",
   :line 902,
   :var-type "function",
   :arglists ([x] [x y] [x y & more]),
   :doc
   "Returns non-nil if nums are in monotonically increasing order,\notherwise false.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/<"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "<=",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1057",
   :line 1057,
   :var-type "function",
   :arglists ([x] [x y] [x y & more]),
   :doc
   "Returns non-nil if nums are in monotonically non-decreasing order,\notherwise false.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/<="}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "=",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L785",
   :line 785,
   :var-type "function",
   :arglists ([x] [x y] [x y & more]),
   :doc
   "Equality. Returns true if x equals y, false if not. Same as\nJava x.equals(y) except it also works for nil, and compares\nnumbers and collections in a type-independent manner.  Clojure's immutable data\nstructures define equals() (and thus =) as a value, not an identity,\ncomparison.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/="}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "==",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1102",
   :line 1102,
   :var-type "function",
   :arglists ([x] [x y] [x y & more]),
   :doc
   "Returns non-nil if nums all have the equivalent\nvalue (type-independent), otherwise false",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/=="}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name ">",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1072",
   :line 1072,
   :var-type "function",
   :arglists ([x] [x y] [x y & more]),
   :doc
   "Returns non-nil if nums are in monotonically decreasing order,\notherwise false.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/>"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name ">=",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1087",
   :line 1087,
   :var-type "function",
   :arglists ([x] [x y] [x y & more]),
   :doc
   "Returns non-nil if nums are in monotonically non-increasing order,\notherwise false.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/>="}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.11",
   :name "NaN?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L8091",
   :line 8091,
   :var-type "function",
   :arglists ([num]),
   :doc "Returns true if num is NaN, else false",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/NaN?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/51261641817005f872f9b6a752c5aef0bb7a5be4/src/clj/clojure/core_print.clj",
   :added "1.10",
   :name "PrintWriter-on",
   :file "src/clj/clojure/core_print.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/51261641817005f872f9b6a752c5aef0bb7a5be4/src/clj/clojure/core_print.clj#L559",
   :line 559,
   :var-type "function",
   :arglists ([flush-fn close-fn]),
   :doc
   "implements java.io.PrintWriter given flush-fn, which will be called\nwhen .flush() is called, with a string built up since the last call to .flush().\nif not nil, close-fn will be called with no arguments when .close is called",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/PrintWriter-on"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/51261641817005f872f9b6a752c5aef0bb7a5be4/src/clj/clojure/core_print.clj",
   :added "1.9",
   :name "StackTraceElement->vec",
   :file "src/clj/clojure/core_print.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/51261641817005f872f9b6a752c5aef0bb7a5be4/src/clj/clojure/core_print.clj#L465",
   :line 465,
   :var-type "function",
   :arglists ([o]),
   :doc
   "Constructs a data representation for a StackTraceElement: [class method file line]",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/StackTraceElement->vec"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/51261641817005f872f9b6a752c5aef0bb7a5be4/src/clj/clojure/core_print.clj",
   :added "1.7",
   :name "Throwable->map",
   :file "src/clj/clojure/core_print.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/51261641817005f872f9b6a752c5aef0bb7a5be4/src/clj/clojure/core_print.clj#L471",
   :line 471,
   :var-type "function",
   :arglists ([o]),
   :doc
   "Constructs a data representation for a Throwable with keys:\n:cause - root cause message\n:phase - error phase\n:via - cause chain, with cause keys:\n         :type - exception class symbol\n         :message - exception message\n         :data - ex-data\n         :at - top stack element\n:trace - root cause stack elements",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/Throwable->map"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.11",
   :name "abs",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1137",
   :line 1137,
   :var-type "function",
   :arglists ([a]),
   :doc
   "Returns the absolute value of a.\nIf a is Long/MIN_VALUE => Long/MIN_VALUE\nIf a is a double and zero => +0.0\nIf a is a double and ##Inf or ##-Inf => ##Inf\nIf a is a double and ##NaN => ##NaN",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/abs"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "accessor",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4073",
   :line 4073,
   :var-type "function",
   :arglists ([s key]),
   :doc
   "Returns a fn that, given an instance of a structmap with the basis,\nreturns the value at the key.  The key must be in the basis. The\nreturned function should be (slightly) more efficient than using\nget, but such use of accessors should be limited to known\nperformance-critical areas.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/accessor"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "aclone",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3907",
   :line 3907,
   :var-type "function",
   :arglists ([array]),
   :doc
   "Returns a clone of the Java array. Works on arrays of known\ntypes.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/aclone"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "add-classpath",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5178",
   :line 5178,
   :deprecated "1.1",
   :var-type "function",
   :arglists ([url]),
   :doc
   "DEPRECATED \n\nAdds the url (String or URL object) to the classpath per\nURLClassLoader.addURL",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/add-classpath"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.10",
   :name "add-tap",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7983",
   :line 7983,
   :var-type "function",
   :arglists ([f]),
   :doc
   "adds f, a fn of one argument, to the tap set. This function will be called with anything sent via tap>.\nThis function may (briefly) block (e.g. for streams), and will never impede calls to tap>,\nbut blocking indefinitely may cause tap values to be dropped.\nRemember f in order to remove-tap",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/add-tap"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "add-watch",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2161",
   :line 2161,
   :var-type "function",
   :arglists ([reference key fn]),
   :doc
   "Adds a watch function to an agent/atom/var/ref reference. The watch\nfn must be a fn of 4 args: a key, the reference, its old-state, its\nnew-state. Whenever the reference's state might have been changed,\nany registered watches will have their functions called. The watch fn\nwill be called synchronously, on the agent's thread if an agent,\nbefore any pending sends if agent or ref. Note that an atom's or\nref's state may have changed again prior to the fn call, so use\nold/new-state rather than derefing the reference. Note also that watch\nfns may be called from multiple threads simultaneously. Var watchers\nare triggered only by root binding changes, not thread-local\nset!s. Keys must be unique per reference, and can be used to remove\nthe watch with remove-watch, but are otherwise considered opaque by\nthe watch mechanism.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/add-watch"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "agent",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2071",
   :line 2071,
   :var-type "function",
   :arglists ([state & options]),
   :doc
   "Creates and returns an agent with an initial value of state and\nzero or more options (in any order):\n\n:meta metadata-map\n\n:validator validate-fn\n\n:error-handler handler-fn\n\n:error-mode mode-keyword\n\nIf metadata-map is supplied, it will become the metadata on the\nagent. validate-fn must be nil or a side-effect-free fn of one\nargument, which will be passed the intended new state on any state\nchange. If the new state is unacceptable, the validate-fn should\nreturn false or throw an exception.  handler-fn is called if an\naction throws an exception or if validate-fn rejects a new state --\nsee set-error-handler! for details.  The mode-keyword may be either\n:continue (the default if an error-handler is given) or :fail (the\ndefault if no error-handler is given) -- see set-error-mode! for\ndetails.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/agent"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.2",
   :name "agent-error",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2186",
   :line 2186,
   :var-type "function",
   :arglists ([a]),
   :doc
   "Returns the exception thrown during an asynchronous action of the\nagent if the agent is failed.  Returns nil if the agent is not\nfailed.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/agent-error"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "agent-errors",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2253",
   :line 2253,
   :deprecated "1.2",
   :var-type "function",
   :arglists ([a]),
   :doc
   "DEPRECATED: Use 'agent-error' instead.\nReturns a sequence of the exceptions thrown during asynchronous\nactions of the agent.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/agent-errors"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "aget",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3914",
   :line 3914,
   :var-type "function",
   :arglists ([array idx] [array idx & idxs]),
   :doc
   "Returns the value at the index/indices. Works on Java arrays of all\ntypes.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/aget"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "alength",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3900",
   :line 3900,
   :var-type "function",
   :arglists ([array]),
   :doc
   "Returns the length of the Java array. Works on arrays of all\ntypes.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/alength"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "alias",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4266",
   :line 4266,
   :var-type "function",
   :arglists ([alias namespace-sym]),
   :doc
   "Add an alias in the current namespace to another\nnamespace. Arguments are two symbols: the alias to be used, and\nthe symbolic name of the target namespace. Use :as in the ns macro in preference\nto calling this directly.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/alias"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "all-ns",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4149",
   :line 4149,
   :var-type "function",
   :arglists ([]),
   :doc "Returns a sequence of all namespaces.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/all-ns"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "alter",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2460",
   :line 2460,
   :var-type "function",
   :arglists ([ref fun & args]),
   :doc
   "Must be called in a transaction. Sets the in-transaction-value of\nref to:\n\n(apply fun in-transaction-value-of-ref args)\n\nand returns the in-transaction-value of ref.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/alter"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "alter-meta!",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2423",
   :line 2423,
   :var-type "function",
   :arglists ([iref f & args]),
   :doc
   "Atomically sets the metadata for a namespace/var/ref/agent/atom to be:\n\n(apply f its-current-meta args)\n\nf must be free of side-effects",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/alter-meta!"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "alter-var-root",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5531",
   :line 5531,
   :var-type "function",
   :arglists ([v f & args]),
   :doc
   "Atomically alters the root binding of var v by applying f to its\ncurrent value plus any args",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/alter-var-root"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "amap",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5275",
   :line 5275,
   :var-type "macro",
   :arglists ([a idx ret expr]),
   :doc
   "Maps an expression across an array a, using an index named idx, and\nreturn value named ret, initialized to a clone of a, then setting \neach element of ret to the evaluation of expr, returning the new \narray ret.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/amap"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "ancestors",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5624",
   :line 5624,
   :var-type "function",
   :arglists ([tag] [h tag]),
   :doc
   "Returns the immediate and indirect parents of tag, either via a Java type\ninheritance relationship or a relationship established via derive. h\nmust be a hierarchy obtained from make-hierarchy, if not supplied\ndefaults to the global hierarchy",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ancestors"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "and",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L844",
   :line 844,
   :var-type "macro",
   :arglists ([] [x] [x & next]),
   :doc
   "Evaluates exprs one at a time, from left to right. If a form\nreturns logical false (nil or false), and returns that value and\ndoesn't evaluate any of the other expressions, otherwise it returns\nthe value of the last expr. (and) returns true.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/and"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.9",
   :name "any?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L540",
   :line 540,
   :var-type "function",
   :arglists ([x]),
   :doc "Returns true given any argument.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/any?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "apply",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L662",
   :line 662,
   :var-type "function",
   :arglists
   ([f args]
    [f x args]
    [f x y args]
    [f x y z args]
    [f a b c d & args]),
   :doc
   "Applies fn f to the argument list formed by prepending intervening arguments to args.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/apply"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "areduce",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5291",
   :line 5291,
   :var-type "macro",
   :arglists ([a idx ret init expr]),
   :doc
   "Reduces an expression across an array a, using an index named idx,\nand return value named ret, initialized to init, setting ret to the \nevaluation of expr at each step, returning ret.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/areduce"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "array-map",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4381",
   :line 4381,
   :var-type "function",
   :arglists ([] [& keyvals]),
   :doc
   "Constructs an array-map. If any keys are equal, they are handled as\nif by repeated uses of assoc.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/array-map"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.5",
   :name "as->",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7641",
   :line 7641,
   :var-type "macro",
   :arglists ([expr name & forms]),
   :doc
   "Binds name to expr, evaluates the first form in the lexical context\nof that binding, then binds name to that result, repeating for each\nsuccessive form, returning the result of the last form.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/as->"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "aset",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3925",
   :line 3925,
   :var-type "function",
   :arglists ([array idx val] [array idx idx2 & idxv]),
   :doc
   "Sets the value at the index/indices. Works on Java arrays of\nreference types. Returns val.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/aset"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "aset-boolean",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3958",
   :line 3958,
   :var-type "function",
   :arglists ([array idx val] [array idx idx2 & idxv]),
   :doc
   "Sets the value at the index/indices. Works on arrays of boolean. Returns val.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/aset-boolean"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "aset-byte",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3978",
   :line 3978,
   :var-type "function",
   :arglists ([array idx val] [array idx idx2 & idxv]),
   :doc
   "Sets the value at the index/indices. Works on arrays of byte. Returns val.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/aset-byte"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "aset-char",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3983",
   :line 3983,
   :var-type "function",
   :arglists ([array idx val] [array idx idx2 & idxv]),
   :doc
   "Sets the value at the index/indices. Works on arrays of char. Returns val.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/aset-char"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "aset-double",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3968",
   :line 3968,
   :var-type "function",
   :arglists ([array idx val] [array idx idx2 & idxv]),
   :doc
   "Sets the value at the index/indices. Works on arrays of double. Returns val.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/aset-double"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "aset-float",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3963",
   :line 3963,
   :var-type "function",
   :arglists ([array idx val] [array idx idx2 & idxv]),
   :doc
   "Sets the value at the index/indices. Works on arrays of float. Returns val.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/aset-float"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "aset-int",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3948",
   :line 3948,
   :var-type "function",
   :arglists ([array idx val] [array idx idx2 & idxv]),
   :doc
   "Sets the value at the index/indices. Works on arrays of int. Returns val.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/aset-int"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "aset-long",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3953",
   :line 3953,
   :var-type "function",
   :arglists ([array idx val] [array idx idx2 & idxv]),
   :doc
   "Sets the value at the index/indices. Works on arrays of long. Returns val.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/aset-long"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "aset-short",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3973",
   :line 3973,
   :var-type "function",
   :arglists ([array idx val] [array idx idx2 & idxv]),
   :doc
   "Sets the value at the index/indices. Works on arrays of short. Returns val.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/aset-short"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "assert",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4842",
   :line 4842,
   :var-type "macro",
   :arglists ([x] [x message]),
   :doc
   "Evaluates expr and throws an exception if it does not evaluate to\nlogical true.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/assert"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "assoc",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L183",
   :line 183,
   :var-type "function",
   :arglists ([map key val] [map key val & kvs]),
   :doc
   "assoc[iate]. When applied to a map, returns a new map of the\nsame (hashed/sorted) type, that contains the mapping of key(s) to\nval(s). When applied to a vector, returns a new vector that\ncontains val at index. Note - index must be <= (count vector).",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/assoc"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.1",
   :name "assoc!",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3370",
   :line 3370,
   :var-type "function",
   :arglists ([coll key val] [coll key val & kvs]),
   :doc
   "When applied to a transient map, adds mapping of key(s) to\nval(s). When applied to a transient vector, sets the val at index.\nNote - index must be <= (count vector). Returns coll.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/assoc!"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "assoc-in",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6197",
   :line 6197,
   :var-type "function",
   :arglists ([m [k & ks] v]),
   :doc
   "Associates a value in a nested associative structure, where ks is a\nsequence of keys and v is the new value and returns a new nested structure.\nIf any levels do not exist, hash-maps will be created.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/assoc-in"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "associative?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6280",
   :line 6280,
   :var-type "function",
   :arglists ([coll]),
   :doc "Returns true if coll implements Associative",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/associative?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "atom",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2344",
   :line 2344,
   :var-type "function",
   :arglists ([x] [x & options]),
   :doc
   "Creates and returns an Atom with an initial value of x and zero or\nmore options (in any order):\n\n:meta metadata-map\n\n:validator validate-fn\n\nIf metadata-map is supplied, it will become the metadata on the\natom. validate-fn must be nil or a side-effect-free fn of one\nargument, which will be passed the intended new state on any state\nchange. If the new state is unacceptable, the validate-fn should\nreturn false or throw an exception.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/atom"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "await",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3276",
   :line 3276,
   :var-type "function",
   :arglists ([& agents]),
   :doc
   "Blocks the current thread (indefinitely!) until all actions\ndispatched thus far, from this thread or agent, to the agent(s) have\noccurred.  Will block on failed agents.  Will never return if\na failed agent is restarted with :clear-actions true or shutdown-agents was called.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/await"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "await-for",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3298",
   :line 3298,
   :var-type "function",
   :arglists ([timeout-ms & agents]),
   :doc
   "Blocks the current thread until all actions dispatched thus\nfar (from this thread or agent) to the agents have occurred, or the\ntimeout (in milliseconds) has elapsed. Returns logical false if\nreturning due to timeout, logical true otherwise.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/await-for"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "bases",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5569",
   :line 5569,
   :var-type "function",
   :arglists ([c]),
   :doc
   "Returns the immediate superclass and direct interfaces of c, if any",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bases"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/658693f6cf97e6ab0ff789e096c9eb6654e4d3ab/src/clj/clojure/core_proxy.clj",
   :added "1.0",
   :name "bean",
   :file "src/clj/clojure/core_proxy.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/658693f6cf97e6ab0ff789e096c9eb6654e4d3ab/src/clj/clojure/core_proxy.clj#L403",
   :line 403,
   :var-type "function",
   :arglists ([x]),
   :doc
   "Takes a Java object and returns a read-only implementation of the\nmap abstraction based upon its JavaBean properties.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bean"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "bigdec",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3649",
   :line 3649,
   :var-type "function",
   :arglists ([x]),
   :doc "Coerce to BigDecimal",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bigdec"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.3",
   :name "bigint",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3621",
   :line 3621,
   :var-type "function",
   :arglists ([x]),
   :doc "Coerce to BigInt",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bigint"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "biginteger",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3635",
   :line 3635,
   :var-type "function",
   :arglists ([x]),
   :doc "Coerce to BigInteger",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/biginteger"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "binding",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1964",
   :line 1964,
   :var-type "macro",
   :arglists ([bindings & body]),
   :doc
   "binding => var-symbol init-expr\n\nCreates new bindings for the (already-existing) vars, with the\nsupplied initial values, executes the exprs in an implicit do, then\nre-establishes the bindings that existed before.  The new bindings\nare made in parallel (unlike let); all init-exprs are evaluated\nbefore the vars are bound to their new values.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/binding"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "bit-and",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1307",
   :line 1307,
   :var-type "function",
   :arglists ([x y] [x y & more]),
   :doc "Bitwise and",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bit-and"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "bit-and-not",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1334",
   :line 1334,
   :var-type "function",
   :arglists ([x y] [x y & more]),
   :doc "Bitwise and with complement",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bit-and-not"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "bit-clear",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1345",
   :line 1345,
   :var-type "function",
   :arglists ([x n]),
   :doc "Clear bit at index n",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bit-clear"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "bit-flip",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1357",
   :line 1357,
   :var-type "function",
   :arglists ([x n]),
   :doc "Flip bit at index n",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bit-flip"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "bit-not",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1300",
   :line 1300,
   :var-type "function",
   :arglists ([x]),
   :doc "Bitwise complement",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bit-not"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "bit-or",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1316",
   :line 1316,
   :var-type "function",
   :arglists ([x y] [x y & more]),
   :doc "Bitwise or",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bit-or"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "bit-set",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1351",
   :line 1351,
   :var-type "function",
   :arglists ([x n]),
   :doc "Set bit at index n",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bit-set"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "bit-shift-left",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1370",
   :line 1370,
   :var-type "function",
   :arglists ([x n]),
   :doc "Bitwise shift left",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bit-shift-left"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "bit-shift-right",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1376",
   :line 1376,
   :var-type "function",
   :arglists ([x n]),
   :doc "Bitwise shift right",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bit-shift-right"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "bit-test",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1363",
   :line 1363,
   :var-type "function",
   :arglists ([x n]),
   :doc "Test bit at index n",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bit-test"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "bit-xor",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1325",
   :line 1325,
   :var-type "function",
   :arglists ([x y] [x y & more]),
   :doc "Bitwise exclusive or",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bit-xor"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "boolean",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1620",
   :line 1620,
   :var-type "function",
   :arglists ([x]),
   :doc "Coerce to boolean",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/boolean"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.1",
   :name "boolean-array",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5311",
   :line 5311,
   :var-type "function",
   :arglists ([size-or-seq] [size init-val-or-seq]),
   :doc "Creates an array of booleans",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/boolean-array"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.9",
   :name "boolean?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L521",
   :line 521,
   :var-type "function",
   :arglists ([x]),
   :doc "Return true if x is a Boolean",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/boolean?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.1",
   :name "booleans",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5374",
   :line 5374,
   :var-type "function",
   :arglists ([xs]),
   :doc "Casts to boolean[]",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/booleans"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.1",
   :name "bound-fn",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2023",
   :line 2023,
   :var-type "macro",
   :arglists ([& fntail]),
   :doc
   "Returns a function defined by the given fntail, which will install the\nsame bindings in effect as in the thread at the time bound-fn was called.\nThis may be used to define a helper function which runs on a different\nthread, but needs the same bindings in place.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bound-fn"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.1",
   :name "bound-fn*",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2011",
   :line 2011,
   :var-type "function",
   :arglists ([f]),
   :doc
   "Returns a function, which will install the same bindings in effect as in\nthe thread at the time bound-fn* was called and then call f with any given\narguments. This may be used to define a helper function which runs on a\ndifferent thread, but needs the same bindings in place.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bound-fn*"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.2",
   :name "bound?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5538",
   :line 5538,
   :var-type "function",
   :arglists ([& vars]),
   :doc
   "Returns true if all of the vars provided as arguments have any bound value, root or thread-local.\nImplies that deref'ing the provided vars will succeed. Returns true if no vars are provided.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bound?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.9",
   :name "bounded-count",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7453",
   :line 7453,
   :var-type "function",
   :arglists ([n coll]),
   :doc
   "If coll is counted? returns its count, else will count at most the first n\nelements of coll using its seq",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bounded-count"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "butlast",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L274",
   :line 274,
   :var-type "function",
   :arglists ([coll]),
   :doc
   "Return a seq of all but the last item in coll, in linear time",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/butlast"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "byte",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3506",
   :line 3506,
   :var-type "function",
   :arglists ([x]),
   :doc "Coerce to byte",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/byte"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.1",
   :name "byte-array",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5319",
   :line 5319,
   :var-type "function",
   :arglists ([size-or-seq] [size init-val-or-seq]),
   :doc "Creates an array of bytes",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/byte-array"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.1",
   :name "bytes",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5379",
   :line 5379,
   :var-type "function",
   :arglists ([xs]),
   :doc "Casts to bytes[]",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bytes"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.9",
   :name "bytes?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5414",
   :line 5414,
   :var-type "function",
   :arglists ([x]),
   :doc "Return true if x is a byte array",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/bytes?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.2",
   :name "case",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6749",
   :line 6749,
   :var-type "macro",
   :arglists ([e & clauses]),
   :doc
   "Takes an expression, and a set of clauses.\n\nEach clause can take the form of either:\n\ntest-constant result-expr\n\n(test-constant1 ... test-constantN)  result-expr\n\nThe test-constants are not evaluated. They must be compile-time\nliterals, and need not be quoted.  If the expression is equal to a\ntest-constant, the corresponding result-expr is returned. A single\ndefault expression can follow the clauses, and its value will be\nreturned if no clause matches. If no default expression is provided\nand no clause matches, an IllegalArgumentException is thrown.\n\nUnlike cond and condp, case does a constant-time dispatch, the\nclauses are not considered sequentially.  All manner of constant\nexpressions are acceptable in case, including numbers, strings,\nsymbols, keywords, and (Clojure) composites thereof. Note that since\nlists are used to group multiple constants that map to the same\nexpression, a vector can be used to match a list if needed. The\ntest-constants need not be all of the same type.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/case"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "cast",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L348",
   :line 348,
   :var-type "function",
   :arglists ([c x]),
   :doc "Throws a ClassCastException if x is not a c, else returns x.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/cast"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.7",
   :name "cat",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7688",
   :line 7688,
   :var-type "function",
   :arglists ([rf]),
   :doc
   "A transducer which concatenates the contents of each input, which must be a\ncollection, into the reduction.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/cat"}
  {:raw-source-url nil,
   :added "1.0",
   :name "catch",
   :file nil,
   :source-url nil,
   :var-type "special syntax",
   :arglists nil,
   :doc
   "Syntax for use with try.\n\nPlease see https://clojure.org/reference/special_forms#try",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/catch"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.1",
   :name "char",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3512",
   :line 3512,
   :var-type "function",
   :arglists ([x]),
   :doc "Coerce to char",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/char"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.1",
   :name "char-array",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5327",
   :line 5327,
   :var-type "function",
   :arglists ([size-or-seq] [size init-val-or-seq]),
   :doc "Creates an array of chars",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/char-array"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/51261641817005f872f9b6a752c5aef0bb7a5be4/src/clj/clojure/core_print.clj",
   :added "1.0",
   :name "char-escape-string",
   :file "src/clj/clojure/core_print.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/51261641817005f872f9b6a752c5aef0bb7a5be4/src/clj/clojure/core_print.clj#L200",
   :line 200,
   :var-type "var",
   :arglists nil,
   :doc "Returns escape string for char or nil if none",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/char-escape-string"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/51261641817005f872f9b6a752c5aef0bb7a5be4/src/clj/clojure/core_print.clj",
   :added "1.0",
   :name "char-name-string",
   :file "src/clj/clojure/core_print.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/51261641817005f872f9b6a752c5aef0bb7a5be4/src/clj/clojure/core_print.clj#L342",
   :line 342,
   :var-type "var",
   :arglists nil,
   :doc "Returns name string for char or nil if none",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/char-name-string"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "char?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L155",
   :line 155,
   :var-type "function",
   :arglists ([x]),
   :doc "Return true if x is a Character",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/char?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.1",
   :name "chars",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5384",
   :line 5384,
   :var-type "function",
   :arglists ([xs]),
   :doc "Casts to chars[]",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/chars"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "class",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3462",
   :line 3462,
   :var-type "function",
   :arglists ([x]),
   :doc "Returns the Class of x",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/class"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "class?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5469",
   :line 5469,
   :var-type "function",
   :arglists ([x]),
   :doc "Returns true if x is an instance of Class",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/class?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "clear-agent-errors",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2263",
   :line 2263,
   :deprecated "1.2",
   :var-type "function",
   :arglists ([a]),
   :doc
   "DEPRECATED: Use 'restart-agent' instead.\nClears any exceptions thrown during asynchronous actions of the\nagent, allowing subsequent actions to occur.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/clear-agent-errors"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "clojure-version",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7150",
   :line 7150,
   :var-type "function",
   :arglists ([]),
   :doc "Returns clojure version as a printable string.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/clojure-version"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "coll?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6249",
   :line 6249,
   :var-type "function",
   :arglists ([x]),
   :doc "Returns true if x implements IPersistentCollection",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/coll?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "comment",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4736",
   :line 4736,
   :var-type "macro",
   :arglists ([& body]),
   :doc "Ignores body, yields nil",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/comment"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "commute",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2439",
   :line 2439,
   :var-type "function",
   :arglists ([ref fun & args]),
   :doc
   "Must be called in a transaction. Sets the in-transaction-value of\nref to:\n\n(apply fun in-transaction-value-of-ref args)\n\nand returns the in-transaction-value of ref.\n\nAt the commit point of the transaction, sets the value of ref to be:\n\n(apply fun most-recently-committed-value-of-ref args)\n\nThus fun should be commutative, or, failing that, you must accept\nlast-one-in-wins behavior.  commute allows for more concurrency than\nref-set.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/commute"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "comp",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2574",
   :line 2574,
   :var-type "function",
   :arglists ([] [f] [f g] [f g & fs]),
   :doc
   "Takes a set of functions and returns a fn that is the composition\nof those fns.  The returned fn takes a variable number of args,\napplies the rightmost of fns to the args, the next\nfn (right-to-left) to the result, etc.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/comp"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "comparator",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3096",
   :line 3096,
   :var-type "function",
   :arglists ([pred]),
   :doc
   "Returns an implementation of java.util.Comparator based upon pred.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/comparator"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "compare",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L833",
   :line 833,
   :var-type "function",
   :arglists ([x y]),
   :doc
   "Comparator. Returns a negative number, zero, or a positive number\nwhen x is logically 'less than', 'equal to', or 'greater than'\ny. Same as Java x.compareTo(y) except it also works for nil, and\ncompares numbers and collections in a type-independent manner. x\nmust implement Comparable",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/compare"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "compare-and-set!",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2385",
   :line 2385,
   :var-type "function",
   :arglists ([atom oldval newval]),
   :doc
   "Atomically sets the value of atom to newval if and only if the\ncurrent value of the atom is identical to oldval. Returns true if\nset happened, else false",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/compare-and-set!"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "compile",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6164",
   :line 6164,
   :var-type "function",
   :arglists ([lib]),
   :doc
   "Compiles the namespace named by the symbol lib into a set of\nclassfiles. The source for the lib must be in a proper\nclasspath-relative directory. The output files will go into the\ndirectory specified by *compile-path*, and that directory too must\nbe in the classpath.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/compile"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "complement",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1447",
   :line 1447,
   :var-type "function",
   :arglists ([f]),
   :doc
   "Takes a fn f and returns a fn that takes the same arguments as f,\nhas the same effects, if any, and returns the opposite truth value.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/complement"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.7",
   :name "completing",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6922",
   :line 6922,
   :var-type "function",
   :arglists ([f] [f cf]),
   :doc
   "Takes a reducing function f of 2 args and returns a fn suitable for\ntransduce by adding an arity-1 signature that calls cf (default -\nidentity) on the result argument.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/completing"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "concat",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L720",
   :line 720,
   :var-type "function",
   :arglists ([] [x] [x y] [x y & zs]),
   :doc
   "Returns a lazy seq representing the concatenation of the elements in the supplied colls.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/concat"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "cond",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L576",
   :line 576,
   :var-type "macro",
   :arglists ([& clauses]),
   :doc
   "Takes a set of test/expr pairs. It evaluates each test one at a\ntime.  If a test returns logical true, cond evaluates and returns\nthe value of the corresponding expr and doesn't evaluate any of the\nother tests or exprs. (cond) returns nil.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/cond"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.5",
   :name "cond->",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7607",
   :line 7607,
   :var-type "macro",
   :arglists ([expr & clauses]),
   :doc
   "Takes an expression and a set of test/form pairs. Threads expr (via ->)\nthrough each form for which the corresponding test\nexpression is true. Note that, unlike cond branching, cond-> threading does\nnot short circuit after the first true test expression.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/cond->"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.5",
   :name "cond->>",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7624",
   :line 7624,
   :var-type "macro",
   :arglists ([expr & clauses]),
   :doc
   "Takes an expression and a set of test/form pairs. Threads expr (via ->>)\nthrough each form for which the corresponding test expression\nis true.  Note that, unlike cond branching, cond->> threading does not short circuit\nafter the first true test expression.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/cond->>"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "condp",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6395",
   :line 6395,
   :var-type "macro",
   :arglists ([pred expr & clauses]),
   :doc
   "Takes a binary predicate, an expression, and a set of clauses.\nEach clause can take the form of either:\n\ntest-expr result-expr\n\ntest-expr :>> result-fn\n\nNote :>> is an ordinary keyword.\n\nFor each clause, (pred test-expr expr) is evaluated. If it returns\nlogical true, the clause is a match. If a binary clause matches, the\nresult-expr is returned, if a ternary clause matches, its result-fn,\nwhich must be a unary function, is called with the result of the\npredicate as its argument, the result of that call being the return\nvalue of condp. A single default expression can follow the clauses,\nand its value will be returned if no clause matches. If no default\nexpression is provided and no clause matches, an\nIllegalArgumentException is thrown.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/condp"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "conj",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L75",
   :line 75,
   :var-type "function",
   :arglists ([] [coll] [coll x] [coll x & xs]),
   :doc
   "conj[oin]. Returns a new collection with the xs\n'added'. (conj nil item) returns (item).\n(conj coll) returns coll. (conj) returns [].\nThe 'addition' may happen at different 'places' depending\non the concrete type.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/conj"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.1",
   :name "conj!",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3360",
   :line 3360,
   :var-type "function",
   :arglists ([] [coll] [coll x]),
   :doc
   "Adds x to the transient collection, and return coll. The 'addition'\nmay happen at different 'places' depending on the concrete type.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/conj!"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "cons",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L22",
   :line 22,
   :var-type "function",
   :arglists ([x seq]),
   :doc
   "Returns a new seq where x is the first element and seq is\nthe rest.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/cons"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "constantly",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1459",
   :line 1459,
   :var-type "function",
   :arglists ([x]),
   :doc
   "Returns a function that takes any number of arguments and returns x.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/constantly"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/658693f6cf97e6ab0ff789e096c9eb6654e4d3ab/src/clj/clojure/core_proxy.clj",
   :added "1.0",
   :name "construct-proxy",
   :file "src/clj/clojure/core_proxy.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/658693f6cf97e6ab0ff789e096c9eb6654e4d3ab/src/clj/clojure/core_proxy.clj#L295",
   :line 295,
   :var-type "function",
   :arglists ([c & ctor-args]),
   :doc
   "Takes a proxy class and any arguments for its superclass ctor and\ncreates and returns an instance of the proxy.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/construct-proxy"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "contains?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1498",
   :line 1498,
   :var-type "function",
   :arglists ([coll key]),
   :doc
   "Returns true if key is present in the given collection, otherwise\nreturns false.  Note that for numerically indexed collections like\nvectors and Java arrays, this tests if the numeric key is within the\nrange of indexes. 'contains?' operates constant or logarithmic time;\nit will not perform a linear search for a value.  See also 'some'.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/contains?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "count",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L876",
   :line 876,
   :var-type "function",
   :arglists ([coll]),
   :doc
   "Returns the number of items in the collection. (count nil) returns\n0.  Also works on strings, arrays, and Java Collections and Maps",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/count"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "counted?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6298",
   :line 6298,
   :var-type "function",
   :arglists ([coll]),
   :doc "Returns true if coll implements count in constant time",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/counted?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "create-ns",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4134",
   :line 4134,
   :var-type "function",
   :arglists ([sym]),
   :doc
   "Create a new namespace named by the symbol if one doesn't already\nexist, returns it or the already-existing namespace of the same\nname.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/create-ns"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "create-struct",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4040",
   :line 4040,
   :var-type "function",
   :arglists ([& keys]),
   :doc "Returns a structure basis object.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/create-struct"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "cycle",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2996",
   :line 2996,
   :var-type "function",
   :arglists ([coll]),
   :doc
   "Returns a lazy (infinite!) sequence of repetitions of the items in coll.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/cycle"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.2",
   :name "dec",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1156",
   :line 1156,
   :var-type "function",
   :arglists ([x]),
   :doc
   "Returns a number one less than num. Does not auto-promote\nlongs, will throw on overflow. See also: dec'",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/dec"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "dec'",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1149",
   :line 1149,
   :var-type "function",
   :arglists ([x]),
   :doc
   "Returns a number one less than num. Supports arbitrary precision.\nSee also: dec",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/dec'"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "decimal?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3600",
   :line 3600,
   :var-type "function",
   :arglists ([n]),
   :doc "Returns true if n is a BigDecimal",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/decimal?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "declare",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2793",
   :line 2793,
   :var-type "macro",
   :arglists ([& names]),
   :doc
   "defs the supplied var names with no bindings, useful for making forward declarations.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/declare"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.7",
   :name "dedupe",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7724",
   :line 7724,
   :var-type "function",
   :arglists ([] [coll]),
   :doc
   "Returns a lazy sequence removing consecutive duplicates in coll.\nReturns a transducer when no collection is provided.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/dedupe"}
  {:raw-source-url nil,
   :added "1.0",
   :name "def",
   :file nil,
   :source-url nil,
   :var-type "special form",
   :arglists nil,
   :doc
   "Creates and interns a global var with the name\nof symbol in the current namespace (*ns*) or locates such a var if\nit already exists.  If init is supplied, it is evaluated, and the\nroot binding of the var is set to the resulting value.  If init is\nnot supplied, the root binding of the var is unaffected.\n\nPlease see https://clojure.org/reference/special_forms#def",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/def",
   :forms [(def symbol doc-string? init?)]}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.4",
   :name "default-data-readers",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7865",
   :line 7865,
   :var-type "var",
   :arglists nil,
   :doc
   "Default map of data reader functions provided by Clojure. May be\noverridden by binding *data-readers*.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/default-data-readers"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "definline",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5255",
   :line 5255,
   :var-type "macro",
   :arglists ([name & decl]),
   :doc
   "Experimental - like defmacro, except defines a named function whose\nbody is the expansion, calls to which may be expanded inline as if\nit were a macro. Cannot be used with variadic (&) args.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/definline"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/8957a93099fc506c3b24ed5739bf9e2fc1811bef/src/clj/clojure/core_deftype.clj",
   :added "1.2",
   :name "definterface",
   :file "src/clj/clojure/core_deftype.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/8957a93099fc506c3b24ed5739bf9e2fc1811bef/src/clj/clojure/core_deftype.clj#L20",
   :line 20,
   :var-type "macro",
   :arglists ([name & sigs]),
   :doc
   "Creates a new Java interface with the given name and method sigs.\nThe method return types and parameter types may be specified with type hints,\ndefaulting to Object if omitted.\n\n(definterface MyInterface\n  (^int method1 [x])\n  (^Bar method2 [^Baz b ^Quux q]))",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/definterface"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "defmacro",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L446",
   :line 446,
   :var-type "macro",
   :arglists
   ([name doc-string? attr-map? [params*] body]
    [name doc-string? attr-map? ([params*] body) + attr-map?]),
   :doc
   "Like defn, but the resulting function name is declared as a\nmacro and will be used as a macro by the compiler when it is\ncalled.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/defmacro"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "defmethod",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1800",
   :line 1800,
   :var-type "macro",
   :arglists ([multifn dispatch-val & fn-tail]),
   :doc
   "Creates and installs a new method of multimethod associated with dispatch-value. ",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/defmethod"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "defmulti",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1742",
   :line 1742,
   :var-type "macro",
   :arglists ([name docstring? attr-map? dispatch-fn & options]),
   :doc
   "Creates a new multimethod with the associated dispatch function.\nThe docstring and attr-map are optional.\n\nOptions are key-value pairs and may be one of:\n\n:default\n\nThe default dispatch value, defaults to :default\n\n:hierarchy\n\nThe value used for hierarchical dispatch (e.g. ::square is-a ::shape)\n\nHierarchies are type-like relationships that do not depend upon type\ninheritance. By default Clojure's multimethods dispatch off of a\nglobal hierarchy map.  However, a hierarchy relationship can be\ncreated with the derive function used to augment the root ancestor\ncreated with make-hierarchy.\n\nMultimethods expect the value of the hierarchy option to be supplied as\na reference type e.g. a var (i.e. via the Var-quote dispatch macro #'\nor the var special form).",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/defmulti"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "defn",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L285",
   :line 285,
   :var-type "macro",
   :arglists
   ([name doc-string? attr-map? [params*] prepost-map? body]
    [name
     doc-string?
     attr-map?
     ([params*] prepost-map? body)
     +
     attr-map?]),
   :doc
   "Same as (def name (fn [params* ] exprs*)) or (def\nname (fn ([params* ] exprs*)+)) with any doc-string or attrs added\nto the var metadata. prepost-map defines a map with optional keys\n:pre and :post that contain collections of pre or post conditions.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/defn"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "defn-",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4951",
   :line 4951,
   :var-type "macro",
   :arglists ([name & decls]),
   :doc "same as defn, yielding non-public def",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/defn-"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "defonce",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5854",
   :line 5854,
   :var-type "macro",
   :arglists ([name expr]),
   :doc
   "defs name to have the root value of the expr iff the named var has no root value,\nelse expr is unevaluated",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/defonce"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/8957a93099fc506c3b24ed5739bf9e2fc1811bef/src/clj/clojure/core_deftype.clj",
   :added "1.2",
   :name "defprotocol",
   :file "src/clj/clojure/core_deftype.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/8957a93099fc506c3b24ed5739bf9e2fc1811bef/src/clj/clojure/core_deftype.clj#L713",
   :line 713,
   :var-type "macro",
   :arglists ([name & opts+sigs]),
   :doc
   "A protocol is a named set of named methods and their signatures:\n(defprotocol AProtocolName\n\n  ;optional doc string\n  \"A doc string for AProtocol abstraction\"\n\n ;options\n :extend-via-metadata true\n\n;method signatures\n  (bar [this a b] \"bar docs\")\n  (baz [this a] [this a b] [this a b c] \"baz docs\"))\n\nNo implementations are provided. Docs can be specified for the\nprotocol overall and for each method. The above yields a set of\npolymorphic functions and a protocol object. All are\nnamespace-qualified by the ns enclosing the definition The resulting\nfunctions dispatch on the type of their first argument, which is\nrequired and corresponds to the implicit target object ('this' in \nJava parlance). defprotocol is dynamic, has no special compile-time \neffect, and defines no new types or classes. Implementations of \nthe protocol methods can be provided using extend.\n\nWhen :extend-via-metadata is true, values can extend protocols by\nadding metadata where keys are fully-qualified protocol function\nsymbols and values are function implementations. Protocol\nimplementations are checked first for direct definitions (defrecord,\ndeftype, reify), then metadata definitions, then external\nextensions (extend, extend-type, extend-protocol)\n\ndefprotocol will automatically generate a corresponding interface,\nwith the same name as the protocol, i.e. given a protocol:\nmy.ns/Protocol, an interface: my.ns.Protocol. The interface will\nhave methods corresponding to the protocol functions, and the\nprotocol will automatically work with instances of the interface.\n\nNote that you should not use this interface with deftype or\nreify, as they support the protocol directly:\n\n(defprotocol P \n  (foo [this]) \n  (bar-me [this] [this y]))\n\n(deftype Foo [a b c] \n P\n  (foo [this] a)\n  (bar-me [this] b)\n  (bar-me [this y] (+ c y)))\n\n(bar-me (Foo. 1 2 3) 42)\n=> 45\n\n(foo \n  (let [x 42]\n    (reify P \n      (foo [this] 17)\n      (bar-me [this] x)\n      (bar-me [this y] x))))\n=> 17",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/defprotocol"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/8957a93099fc506c3b24ed5739bf9e2fc1811bef/src/clj/clojure/core_deftype.clj",
   :added "1.2",
   :name "defrecord",
   :file "src/clj/clojure/core_deftype.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/8957a93099fc506c3b24ed5739bf9e2fc1811bef/src/clj/clojure/core_deftype.clj#L313",
   :line 313,
   :var-type "macro",
   :arglists ([name [& fields] & opts+specs]),
   :doc
   "(defrecord name [fields*]  options* specs*)\n\nOptions are expressed as sequential keywords and arguments (in any order).\n\nSupported options:\n:load-ns - if true, importing the record class will cause the\n           namespace in which the record was defined to be loaded.\n           Defaults to false.\n\nEach spec consists of a protocol or interface name followed by zero\nor more method bodies:\n\nprotocol-or-interface-or-Object\n(methodName [args*] body)*\n\nDynamically generates compiled bytecode for class with the given\nname, in a package with the same name as the current namespace, the\ngiven fields, and, optionally, methods for protocols and/or\ninterfaces.\n\nThe class will have the (immutable) fields named by\nfields, which can have type hints. Protocols/interfaces and methods\nare optional. The only methods that can be supplied are those\ndeclared in the protocols/interfaces.  Note that method bodies are\nnot closures, the local environment includes only the named fields,\nand those fields can be accessed directly.\n\nMethod definitions take the form:\n\n(methodname [args*] body)\n\nThe argument and return types can be hinted on the arg and\nmethodname symbols. If not supplied, they will be inferred, so type\nhints should be reserved for disambiguation.\n\nMethods should be supplied for all methods of the desired\nprotocol(s) and interface(s). You can also define overrides for\nmethods of Object. Note that a parameter must be supplied to\ncorrespond to the target object ('this' in Java parlance). Thus\nmethods for interfaces will take one more argument than do the\ninterface declarations. Note also that recur calls to the method\nhead should *not* pass the target object, it will be supplied\nautomatically and can not be substituted.\n\nIn the method bodies, the (unqualified) name can be used to name the\nclass (for calls to new, instance? etc).\n\nThe class will have implementations of several (clojure.lang)\ninterfaces generated automatically: IObj (metadata support) and\nIPersistentMap, and all of their superinterfaces.\n\nIn addition, defrecord will define type-and-value-based =,\nand will defined Java .hashCode and .equals consistent with the\ncontract for java.util.Map.\n\nWhen AOT compiling, generates compiled bytecode for a class with the\ngiven name (a symbol), prepends the current ns as the package, and\nwrites the .class file to the *compile-path* directory.\n\nTwo constructors will be defined, one taking the designated fields\nfollowed by a metadata map (nil for none) and an extension field\nmap (nil for none), and one taking only the fields (using nil for\nmeta and extension fields). Note that the field names __meta,\n__extmap, __hash and __hasheq are currently reserved and should not\nbe used when defining your own records.\n\nGiven (defrecord TypeName ...), two factory functions will be\ndefined: ->TypeName, taking positional parameters for the fields,\nand map->TypeName, taking a map of keywords to field values.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/defrecord"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "defstruct",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4047",
   :line 4047,
   :var-type "macro",
   :arglists ([name & keys]),
   :doc "Same as (def name (create-struct keys...))",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/defstruct"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/8957a93099fc506c3b24ed5739bf9e2fc1811bef/src/clj/clojure/core_deftype.clj",
   :added "1.2",
   :name "deftype",
   :file "src/clj/clojure/core_deftype.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/8957a93099fc506c3b24ed5739bf9e2fc1811bef/src/clj/clojure/core_deftype.clj#L423",
   :line 423,
   :var-type "macro",
   :arglists ([name [& fields] & opts+specs]),
   :doc
   "(deftype name [fields*]  options* specs*)\n\nOptions are expressed as sequential keywords and arguments (in any order).\n\nSupported options:\n:load-ns - if true, importing the type class will cause the\n           namespace in which the type was defined to be loaded.\n           Defaults to false.\n\nEach spec consists of a protocol or interface name followed by zero\nor more method bodies:\n\nprotocol-or-interface-or-Object\n(methodName [args*] body)*\n\nDynamically generates compiled bytecode for class with the given\nname, in a package with the same name as the current namespace, the\ngiven fields, and, optionally, methods for protocols and/or\ninterfaces. \n\nThe class will have the (by default, immutable) fields named by\nfields, which can have type hints. Protocols/interfaces and methods\nare optional. The only methods that can be supplied are those\ndeclared in the protocols/interfaces.  Note that method bodies are\nnot closures, the local environment includes only the named fields,\nand those fields can be accessed directly. Fields can be qualified\nwith the metadata :volatile-mutable true or :unsynchronized-mutable\ntrue, at which point (set! afield aval) will be supported in method\nbodies. Note well that mutable fields are extremely difficult to use\ncorrectly, and are present only to facilitate the building of higher\nlevel constructs, such as Clojure's reference types, in Clojure\nitself. They are for experts only - if the semantics and\nimplications of :volatile-mutable or :unsynchronized-mutable are not\nimmediately apparent to you, you should not be using them.\n\nMethod definitions take the form:\n\n(methodname [args*] body)\n\nThe argument and return types can be hinted on the arg and\nmethodname symbols. If not supplied, they will be inferred, so type\nhints should be reserved for disambiguation.\n\nMethods should be supplied for all methods of the desired\nprotocol(s) and interface(s). You can also define overrides for\nmethods of Object. Note that a parameter must be supplied to\ncorrespond to the target object ('this' in Java parlance). Thus\nmethods for interfaces will take one more argument than do the\ninterface declarations. Note also that recur calls to the method\nhead should *not* pass the target object, it will be supplied\nautomatically and can not be substituted.\n\nIn the method bodies, the (unqualified) name can be used to name the\nclass (for calls to new, instance? etc).\n\nWhen AOT compiling, generates compiled bytecode for a class with the\ngiven name (a symbol), prepends the current ns as the package, and\nwrites the .class file to the *compile-path* directory.\n\nOne constructor will be defined, taking the designated fields.  Note\nthat the field names __meta, __extmap, __hash and __hasheq are currently\nreserved and should not be used when defining your own types.\n\nGiven (deftype TypeName ...), a factory function called ->TypeName\nwill be defined, taking positional parameters for the fields",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/deftype"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "delay",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L748",
   :line 748,
   :var-type "macro",
   :arglists ([& body]),
   :doc
   "Takes a body of expressions and yields a Delay object that will\ninvoke the body only the first time it is forced (with force or deref/@), and\nwill cache the result and return it on all subsequent force\ncalls. See also - realized?",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/delay"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "delay?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L757",
   :line 757,
   :var-type "function",
   :arglists ([x]),
   :doc "returns true if x is a Delay created with delay",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/delay?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.1",
   :name "deliver",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7196",
   :line 7196,
   :var-type "function",
   :arglists ([promise val]),
   :doc
   "Delivers the supplied value to the promise, releasing any pending\nderefs. A subsequent call to deliver on a promise will have no effect.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/deliver"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.2",
   :name "denominator",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3592",
   :line 3592,
   :var-type "function",
   :arglists ([r]),
   :doc "Returns the denominator part of a Ratio.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/denominator"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "deref",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2323",
   :line 2323,
   :var-type "function",
   :arglists ([ref] [ref timeout-ms timeout-val]),
   :doc
   "Also reader macro: @ref/@agent/@var/@atom/@delay/@future/@promise. Within a transaction,\nreturns the in-transaction-value of ref, else returns the\nmost-recently-committed value of ref. When applied to a var, agent\nor atom, returns its current state. When applied to a delay, forces\nit if not already forced. When applied to a future, will block if\ncomputation not complete. When applied to a promise, will block\nuntil a value is delivered.  The variant taking a timeout can be\nused for blocking references (futures and promises), and will return\ntimeout-val if the timeout (in milliseconds) is reached before a\nvalue is available. See also - realized?.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/deref"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "derive",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5652",
   :line 5652,
   :var-type "function",
   :arglists ([tag parent] [h tag parent]),
   :doc
   "Establishes a parent/child relationship between parent and\ntag. Parent must be a namespace-qualified symbol or keyword and\nchild can be either a namespace-qualified symbol or keyword or a\nclass. h must be a hierarchy obtained from make-hierarchy, if not\nsupplied defaults to, and modifies, the global hierarchy.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/derive"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "descendants",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5640",
   :line 5640,
   :var-type "function",
   :arglists ([tag] [h tag]),
   :doc
   "Returns the immediate and indirect children of tag, through a\nrelationship established via derive. h must be a hierarchy obtained\nfrom make-hierarchy, if not supplied defaults to the global\nhierarchy. Note: does not work on Java type inheritance\nrelationships.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/descendants"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "disj",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1533",
   :line 1533,
   :var-type "function",
   :arglists ([set] [set key] [set key & ks]),
   :doc
   "disj[oin]. Returns a new set of the same (hashed/sorted) type, that\ndoes not contain key(s).",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/disj"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.1",
   :name "disj!",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3402",
   :line 3402,
   :var-type "function",
   :arglists ([set] [set key] [set key & ks]),
   :doc
   "disj[oin]. Returns a transient set of the same (hashed/sorted) type, that\ndoes not contain key(s).",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/disj!"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "dissoc",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1519",
   :line 1519,
   :var-type "function",
   :arglists ([map] [map key] [map key & ks]),
   :doc
   "dissoc[iate]. Returns a new map of the same (hashed/sorted) type,\nthat does not contain a mapping for key(s).",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/dissoc"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.1",
   :name "dissoc!",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3383",
   :line 3383,
   :var-type "function",
   :arglists ([map key] [map key & ks]),
   :doc
   "Returns a transient map that doesn't contain a mapping for key(s).",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/dissoc!"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "distinct",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5055",
   :line 5055,
   :var-type "function",
   :arglists ([] [coll]),
   :doc
   "Returns a lazy sequence of the elements of coll with duplicates removed.\nReturns a stateful transducer when no collection is provided.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/distinct"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "distinct?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5711",
   :line 5711,
   :var-type "function",
   :arglists ([x] [x y] [x y & more]),
   :doc "Returns true if no two of the arguments are =",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/distinct?"}
  {:raw-source-url nil,
   :added "1.0",
   :name "do",
   :file nil,
   :source-url nil,
   :var-type "special form",
   :arglists nil,
   :doc
   "Evaluates the expressions in order and returns the value of\nthe last. If no expressions are supplied, returns nil.\n\nPlease see https://clojure.org/reference/special_forms#do",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/do",
   :forms [(do exprs*)]}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "doall",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3150",
   :line 3150,
   :var-type "function",
   :arglists ([coll] [n coll]),
   :doc
   "When lazy sequences are produced via functions that have side\neffects, any effects other than those needed to produce the first\nelement in the seq do not occur until the seq is consumed. doall can\nbe used to force any effects. Walks through the successive nexts of\nthe seq, retains the head and returns it, thus causing the entire\nseq to reside in memory at one time.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/doall"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "dorun",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3135",
   :line 3135,
   :var-type "function",
   :arglists ([coll] [n coll]),
   :doc
   "When lazy sequences are produced via functions that have side\neffects, any effects other than those needed to produce the first\nelement in the seq do not occur until the seq is consumed. dorun can\nbe used to force any effects. Walks through the successive nexts of\nthe seq, does not retain the head and returns nil.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/dorun"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "doseq",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3218",
   :line 3218,
   :var-type "macro",
   :arglists ([seq-exprs & body]),
   :doc
   "Repeatedly executes body (presumably for side-effects) with\nbindings and filtering as provided by \"for\".  Does not retain\nthe head of the sequence. Returns nil.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/doseq"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "dosync",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5102",
   :line 5102,
   :var-type "macro",
   :arglists ([& exprs]),
   :doc
   "Runs the exprs (in an implicit do) in a transaction that encompasses\nexprs and any nested calls.  Starts a transaction if none is already\nrunning on this thread. Any uncaught exception will abort the\ntransaction and flow out of dosync. The exprs may be run more than\nonce, but any effects on Refs will be atomic.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/dosync"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "dotimes",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3315",
   :line 3315,
   :var-type "macro",
   :arglists ([bindings & body]),
   :doc
   "bindings => name n\n\nRepeatedly executes body (presumably for side-effects) with name\nbound to integers from 0 through n-1.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/dotimes"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "doto",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3854",
   :line 3854,
   :var-type "macro",
   :arglists ([x & forms]),
   :doc
   "Evaluates x then calls all of the methods and functions with the\nvalue of x supplied at the front of the given arguments.  The forms\nare evaluated in order.  Returns x.\n\n(doto (new java.util.HashMap) (.put \"a\" 1) (.put \"b\" 2))",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/doto"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "double",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3494",
   :line 3494,
   :var-type "function",
   :arglists ([x]),
   :doc "Coerce to double",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/double"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "double-array",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5343",
   :line 5343,
   :var-type "function",
   :arglists ([size-or-seq] [size init-val-or-seq]),
   :doc "Creates an array of doubles",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/double-array"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.9",
   :name "double?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1440",
   :line 1440,
   :var-type "function",
   :arglists ([x]),
   :doc "Return true if x is a Double",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/double?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "doubles",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5404",
   :line 5404,
   :var-type "function",
   :arglists ([xs]),
   :doc "Casts to double[]",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/doubles"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "drop",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2926",
   :line 2926,
   :var-type "function",
   :arglists ([n] [n coll]),
   :doc
   "Returns a lazy sequence of all but the first n items in coll.\nReturns a stateful transducer when no collection is provided.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/drop"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "drop-last",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2951",
   :line 2951,
   :var-type "function",
   :arglists ([coll] [n coll]),
   :doc
   "Return a lazy sequence of all but the last n (default 1) items in coll",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/drop-last"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "drop-while",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2969",
   :line 2969,
   :var-type "function",
   :arglists ([pred] [pred coll]),
   :doc
   "Returns a lazy sequence of the items in coll starting from the\nfirst item for which (pred item) returns logical false.  Returns a\nstateful transducer when no collection is provided.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/drop-while"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.7",
   :name "eduction",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7763",
   :line 7763,
   :var-type "function",
   :arglists ([xform* coll]),
   :doc
   "Returns a reducible/iterable application of the transducers\nto the items in coll. Transducers are applied in order as if\ncombined with comp. Note that these applications will be\nperformed every time reduce/iterator is called.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/eduction"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "empty",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5267",
   :line 5267,
   :var-type "function",
   :arglists ([coll]),
   :doc
   "Returns an empty collection of the same category as coll, or nil",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/empty"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "empty?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6242",
   :line 6242,
   :var-type "function",
   :arglists ([coll]),
   :doc
   "Returns true if coll has no items - same as (not (seq coll)).\nPlease use the idiom (seq x) rather than (not (empty? x))",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/empty?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "ensure",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2505",
   :line 2505,
   :var-type "function",
   :arglists ([ref]),
   :doc
   "Must be called in a transaction. Protects the ref from modification\nby other transactions.  Returns the in-transaction-value of\nref. Allows for more concurrency than (ref-set ref @ref)",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ensure"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.7",
   :name "ensure-reduced",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2866",
   :line 2866,
   :var-type "function",
   :arglists ([x]),
   :doc
   "If x is already reduced?, returns it, else returns (reduced x)",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ensure-reduced"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "enumeration-seq",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5757",
   :line 5757,
   :var-type "function",
   :arglists ([e]),
   :doc "Returns a seq on a java.util.Enumeration",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/enumeration-seq"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.2",
   :name "error-handler",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2221",
   :line 2221,
   :var-type "function",
   :arglists ([a]),
   :doc
   "Returns the error-handler of agent a, or nil if there is none.\nSee set-error-handler!",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/error-handler"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.2",
   :name "error-mode",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2246",
   :line 2246,
   :var-type "function",
   :arglists ([a]),
   :doc "Returns the error-mode of agent a.  See set-error-mode!",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/error-mode"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "eval",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3212",
   :line 3212,
   :var-type "function",
   :arglists ([form]),
   :doc
   "Evaluates the form data structure (not text!) and returns the result.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/eval"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "even?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1400",
   :line 1400,
   :var-type "function",
   :arglists ([n]),
   :doc
   "Returns true if n is even, throws an exception if n is not an integer",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/even?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.3",
   :name "every-pred",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7465",
   :line 7465,
   :var-type "function",
   :arglists ([p] [p1 p2] [p1 p2 p3] [p1 p2 p3 & ps]),
   :doc
   "Takes a set of predicates and returns a function f that returns true if all of its\ncomposing predicates return a logical true value against all of its arguments, else it returns\nfalse. Note that f is short-circuiting in that it will stop execution on the first\nargument that triggers a logical false result against the original predicates.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/every-pred"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "every?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2689",
   :line 2689,
   :var-type "function",
   :arglists ([pred coll]),
   :doc
   "Returns true if (pred x) is logical true for every x in coll, else\nfalse.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/every?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.10",
   :name "ex-cause",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4833",
   :line 4833,
   :var-type "function",
   :arglists ([ex]),
   :doc
   "Returns the cause of ex if ex is a Throwable.\nOtherwise returns nil.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ex-cause"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.4",
   :name "ex-data",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4817",
   :line 4817,
   :var-type "function",
   :arglists ([ex]),
   :doc
   "Returns exception data (a map) if ex is an IExceptionInfo.\nOtherwise returns nil.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ex-data"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.4",
   :name "ex-info",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4808",
   :line 4808,
   :var-type "function",
   :arglists ([msg map] [msg map cause]),
   :doc
   "Create an instance of ExceptionInfo, a RuntimeException subclass\nthat carries a map of additional data.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ex-info"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.10",
   :name "ex-message",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4825",
   :line 4825,
   :var-type "function",
   :arglists ([ex]),
   :doc
   "Returns the message attached to ex if ex is a Throwable.\nOtherwise returns nil.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ex-message"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/8957a93099fc506c3b24ed5739bf9e2fc1811bef/src/clj/clojure/core_deftype.clj",
   :added "1.2",
   :name "extend",
   :file "src/clj/clojure/core_deftype.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/8957a93099fc506c3b24ed5739bf9e2fc1811bef/src/clj/clojure/core_deftype.clj#L777",
   :line 777,
   :var-type "function",
   :arglists ([atype & proto+mmaps]),
   :doc
   "Implementations of protocol methods can be provided using the extend construct:\n\n (extend AType\n   AProtocol\n    {:foo an-existing-fn\n     :bar (fn [a b] ...)\n     :baz (fn ([a]...) ([a b] ...)...)}\n   BProtocol \n     {...} \n   ...)\n\n extend takes a type/class (or interface, see below), and one or more\n protocol + method map pairs. It will extend the polymorphism of the\n protocol's methods to call the supplied methods when an AType is\n provided as the first argument. \n\n Method maps are maps of the keyword-ized method names to ordinary\n fns. This facilitates easy reuse of existing fns and fn maps, for\n code reuse/mixins without derivation or composition. You can extend\n an interface to a protocol. This is primarily to facilitate interop\n with the host (e.g. Java) but opens the door to incidental multiple\n inheritance of implementation since a class can inherit from more\n than one interface, both of which extend the protocol. It is TBD how\n to specify which impl to use. You can extend a protocol on nil.\n\n If you are supplying the definitions explicitly (i.e. not reusing\n exsting functions or mixin maps), you may find it more convenient to\n use the extend-type or extend-protocol macros.\n\n Note that multiple independent extend clauses can exist for the same\n type, not all protocols need be defined in a single extend call.\n\n See also:\n extends?, satisfies?, extenders",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/extend"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/8957a93099fc506c3b24ed5739bf9e2fc1811bef/src/clj/clojure/core_deftype.clj",
   :added "1.2",
   :name "extend-protocol",
   :file "src/clj/clojure/core_deftype.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/8957a93099fc506c3b24ed5739bf9e2fc1811bef/src/clj/clojure/core_deftype.clj#L877",
   :line 877,
   :var-type "macro",
   :arglists ([p & specs]),
   :doc
   "Useful when you want to provide several implementations of the same\nprotocol all at once. Takes a single protocol and the implementation\nof that protocol for one or more types. Expands into calls to\nextend-type:\n\n(extend-protocol Protocol\n  AType\n    (foo [x] ...)\n    (bar [x y] ...)\n  BType\n    (foo [x] ...)\n    (bar [x y] ...)\n  AClass\n    (foo [x] ...)\n    (bar [x y] ...)\n  nil\n    (foo [x] ...)\n    (bar [x y] ...))\n\nexpands into:\n\n(do\n (clojure.core/extend-type AType Protocol \n   (foo [x] ...) \n   (bar [x y] ...))\n (clojure.core/extend-type BType Protocol \n   (foo [x] ...) \n   (bar [x y] ...))\n (clojure.core/extend-type AClass Protocol \n   (foo [x] ...) \n   (bar [x y] ...))\n (clojure.core/extend-type nil Protocol \n   (foo [x] ...) \n   (bar [x y] ...)))",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/extend-protocol"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/8957a93099fc506c3b24ed5739bf9e2fc1811bef/src/clj/clojure/core_deftype.clj",
   :added "1.2",
   :name "extend-type",
   :file "src/clj/clojure/core_deftype.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/8957a93099fc506c3b24ed5739bf9e2fc1811bef/src/clj/clojure/core_deftype.clj#L845",
   :line 845,
   :var-type "macro",
   :arglists ([t & specs]),
   :doc
   "A macro that expands into an extend call. Useful when you are\nsupplying the definitions explicitly inline, extend-type\nautomatically creates the maps required by extend.  Propagates the\nclass as a type hint on the first argument of all fns.\n\n(extend-type MyType \n  Countable\n    (cnt [c] ...)\n  Foo\n    (bar [x y] ...)\n    (baz ([x] ...) ([x y & zs] ...)))\n\nexpands into:\n\n(extend MyType\n Countable\n   {:cnt (fn [c] ...)}\n Foo\n   {:baz (fn ([x] ...) ([x y & zs] ...))\n    :bar (fn [x y] ...)})",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/extend-type"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/8957a93099fc506c3b24ed5739bf9e2fc1811bef/src/clj/clojure/core_deftype.clj",
   :added "1.2",
   :name "extenders",
   :file "src/clj/clojure/core_deftype.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/8957a93099fc506c3b24ed5739bf9e2fc1811bef/src/clj/clojure/core_deftype.clj#L564",
   :line 564,
   :var-type "function",
   :arglists ([protocol]),
   :doc
   "Returns a collection of the types explicitly extending protocol",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/extenders"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/8957a93099fc506c3b24ed5739bf9e2fc1811bef/src/clj/clojure/core_deftype.clj",
   :added "1.2",
   :name "extends?",
   :file "src/clj/clojure/core_deftype.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/8957a93099fc506c3b24ed5739bf9e2fc1811bef/src/clj/clojure/core_deftype.clj#L557",
   :line 557,
   :var-type "function",
   :arglists ([protocol atype]),
   :doc "Returns true if atype extends protocol",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/extends?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "false?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L507",
   :line 507,
   :var-type "function",
   :arglists ([x]),
   :doc "Returns true if x is the value false, false otherwise.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/false?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "ffirst",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L100",
   :line 100,
   :var-type "function",
   :arglists ([x]),
   :doc "Same as (first (first x))",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ffirst"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "file-seq",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4974",
   :line 4974,
   :var-type "function",
   :arglists ([dir]),
   :doc "A tree seq on java.io.Files",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/file-seq"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "filter",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2810",
   :line 2810,
   :var-type "function",
   :arglists ([pred] [pred coll]),
   :doc
   "Returns a lazy sequence of the items in coll for which\n(pred item) returns logical true. pred must be free of side-effects.\nReturns a transducer when no collection is provided.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/filter"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.4",
   :name "filterv",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6989",
   :line 6989,
   :var-type "function",
   :arglists ([pred coll]),
   :doc
   "Returns a vector of the items in coll for which\n(pred item) returns logical true. pred must be free of side-effects.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/filterv"}
  {:raw-source-url nil,
   :added "1.0",
   :name "finally",
   :file nil,
   :source-url nil,
   :var-type "special syntax",
   :arglists nil,
   :doc
   "Syntax for use with try.\n\nPlease see https://clojure.org/reference/special_forms#try",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/finally"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "find",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1549",
   :line 1549,
   :var-type "function",
   :arglists ([map key]),
   :doc "Returns the map entry for key, or nil if key not present.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/find"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.3",
   :name "find-keyword",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L627",
   :line 627,
   :var-type "function",
   :arglists ([name] [ns name]),
   :doc
   "Returns a Keyword with the given namespace and name if one already\nexists.  This function will not intern a new keyword. If the keyword\nhas not already been interned, it will return nil.  Do not use :\nin the keyword strings, it will be added automatically.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/find-keyword"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "find-ns",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4128",
   :line 4128,
   :var-type "function",
   :arglists ([sym]),
   :doc
   "Returns the namespace named by the symbol or nil if it doesn't exist.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/find-ns"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "find-var",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2032",
   :line 2032,
   :var-type "function",
   :arglists ([sym]),
   :doc
   "Returns the global var named by the namespace-qualified symbol, or\nnil if no var with that name.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/find-var"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "first",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L49",
   :line 49,
   :var-type "function",
   :arglists ([coll]),
   :doc
   "Returns the first item in the collection. Calls seq on its\nargument. If coll is nil, returns nil.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/first"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.2",
   :name "flatten",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7205",
   :line 7205,
   :var-type "function",
   :arglists ([x]),
   :doc
   "Takes any nested combination of sequential things (lists, vectors,\netc.) and returns their contents as a single, flat lazy sequence.\n(flatten nil) returns an empty sequence.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/flatten"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "float",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3488",
   :line 3488,
   :var-type "function",
   :arglists ([x]),
   :doc "Coerce to float",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/float"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "float-array",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5303",
   :line 5303,
   :var-type "function",
   :arglists ([size-or-seq] [size init-val-or-seq]),
   :doc "Creates an array of floats",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/float-array"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "float?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3606",
   :line 3606,
   :var-type "function",
   :arglists ([n]),
   :doc "Returns true if n is a floating point number",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/float?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "floats",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5394",
   :line 5394,
   :var-type "function",
   :arglists ([xs]),
   :doc "Casts to float[]",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/floats"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "flush",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3707",
   :line 3707,
   :var-type "function",
   :arglists ([]),
   :doc
   "Flushes the output stream that is the current value of\n*out*",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/flush"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "fn",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4536",
   :line 4536,
   :var-type "special form",
   :arglists ([& sigs]),
   :doc
   "params => positional-params*, or positional-params* & rest-param\npositional-param => binding-form\nrest-param => binding-form\nbinding-form => name, or destructuring-form\n\nDefines a function.\n\nSee https://clojure.org/reference/special_forms#fn for more information",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/fn",
   :forms
   [(fn name? [params*] exprs*) (fn name? ([params*] exprs*) +)]}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "fn?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6273",
   :line 6273,
   :var-type "function",
   :arglists ([x]),
   :doc
   "Returns true if x implements Fn, i.e. is an object created via fn.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/fn?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "fnext",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L114",
   :line 114,
   :var-type "function",
   :arglists ([x]),
   :doc "Same as (first (next x))",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/fnext"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.2",
   :name "fnil",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6595",
   :line 6595,
   :var-type "function",
   :arglists ([f x] [f x y] [f x y z]),
   :doc
   "Takes a function f, and returns a function that calls f, replacing\na nil first argument to f with the supplied value x. Higher arity\nversions can replace arguments in the second and third\npositions (y, z). Note that the function f can take any number of\narguments, not just the one(s) being nil-patched.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/fnil"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "for",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4649",
   :line 4649,
   :var-type "macro",
   :arglists ([seq-exprs body-expr]),
   :doc
   "List comprehension. Takes a vector of one or more\n binding-form/collection-expr pairs, each followed by zero or more\n modifiers, and yields a lazy sequence of evaluations of expr.\n Collections are iterated in a nested fashion, rightmost fastest,\n and nested coll-exprs can refer to bindings created in prior\n binding-forms.  Supported modifiers are: :let [binding-form expr ...],\n :while test, :when test.\n\n(take 100 (for [x (range 100000000) y (range 1000000) :while (< y x)] [x y]))",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/for"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "force",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L763",
   :line 763,
   :var-type "function",
   :arglists ([x]),
   :doc
   "If x is a Delay, returns the (possibly cached) value of its expression, else returns x",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/force"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "format",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5764",
   :line 5764,
   :var-type "function",
   :arglists ([fmt & args]),
   :doc
   "Formats a string using java.lang.String.format, see java.util.Formatter for format\nstring syntax",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/format"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.2",
   :name "frequencies",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7272",
   :line 7272,
   :var-type "function",
   :arglists ([coll]),
   :doc
   "Returns a map from distinct items in coll to the number of times\nthey appear.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/frequencies"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.1",
   :name "future",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7058",
   :line 7058,
   :var-type "macro",
   :arglists ([& body]),
   :doc
   "Takes a body of expressions and yields a future object that will\ninvoke the body in another thread, and will cache the result and\nreturn it on all subsequent calls to deref/@. If the computation has\nnot yet finished, calls to deref/@ will block, unless the variant of\nderef with timeout is used. See also - realized?.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/future"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.1",
   :name "future-call",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7031",
   :line 7031,
   :var-type "function",
   :arglists ([f]),
   :doc
   "Takes a function of no args and yields a future object that will\ninvoke the function in another thread, and will cache the result and\nreturn it on all subsequent calls to deref/@. If the computation has\nnot yet finished, calls to deref/@ will block, unless the variant\nof deref with timeout is used. See also - realized?.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/future-call"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.1",
   :name "future-cancel",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7068",
   :line 7068,
   :var-type "function",
   :arglists ([f]),
   :doc "Cancels the future, if possible.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/future-cancel"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.1",
   :name "future-cancelled?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7074",
   :line 7074,
   :var-type "function",
   :arglists ([f]),
   :doc "Returns true if future f is cancelled",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/future-cancelled?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.1",
   :name "future-done?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6575",
   :line 6575,
   :var-type "function",
   :arglists ([f]),
   :doc "Returns true if future f is done",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/future-done?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.1",
   :name "future?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6569",
   :line 6569,
   :var-type "function",
   :arglists ([x]),
   :doc "Returns true if x is a future",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/future?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/38705b49fd3dbae11e94c576ef49ff3eb1c47395/src/clj/clojure/genclass.clj",
   :added "1.0",
   :name "gen-class",
   :file "src/clj/clojure/genclass.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/38705b49fd3dbae11e94c576ef49ff3eb1c47395/src/clj/clojure/genclass.clj#L507",
   :line 507,
   :var-type "macro",
   :arglists ([& options]),
   :doc
   "When compiling, generates compiled bytecode for a class with the\ngiven package-qualified :name (which, as all names in these\nparameters, can be a string or symbol), and writes the .class file\nto the *compile-path* directory.  When not compiling, does\nnothing. The gen-class construct contains no implementation, as the\nimplementation will be dynamically sought by the generated class in\nfunctions in an implementing Clojure namespace. Given a generated\nclass org.mydomain.MyClass with a method named mymethod, gen-class\nwill generate an implementation that looks for a function named by \n(str prefix mymethod) (default prefix: \"-\") in a\nClojure namespace specified by :impl-ns\n(defaults to the current namespace). All inherited methods,\ngenerated methods, and init and main functions (see :methods, :init,\nand :main below) will be found similarly prefixed. By default, the\nstatic initializer for the generated class will attempt to load the\nClojure support code for the class as a resource from the classpath,\ne.g. in the example case, ``org/mydomain/MyClass__init.class``. This\nbehavior can be controlled by :load-impl-ns\n\nNote that methods with a maximum of 18 parameters are supported.\n\nIn all subsequent sections taking types, the primitive types can be\nreferred to by their Java names (int, float etc), and classes in the\njava.lang package can be used without a package qualifier. All other\nclasses must be fully qualified.\n\nOptions should be a set of key/value pairs, all except for :name are optional:\n\n:name aname\n\nThe package-qualified name of the class to be generated\n\n:extends aclass\n\nSpecifies the superclass, the non-private methods of which will be\noverridden by the class. If not provided, defaults to Object.\n\n:implements [interface ...]\n\nOne or more interfaces, the methods of which will be implemented by the class.\n\n:init name\n\nIf supplied, names a function that will be called with the arguments\nto the constructor. Must return [ [superclass-constructor-args] state] \nIf not supplied, the constructor args are passed directly to\nthe superclass constructor and the state will be nil\n\n:constructors {[param-types] [super-param-types], ...}\n\nBy default, constructors are created for the generated class which\nmatch the signature(s) of the constructors for the superclass. This\nparameter may be used to explicitly specify constructors, each entry\nproviding a mapping from a constructor signature to a superclass\nconstructor signature. When you supply this, you must supply an :init\nspecifier. \n\n:post-init name\n\nIf supplied, names a function that will be called with the object as\nthe first argument, followed by the arguments to the constructor.\nIt will be called every time an object of this class is created,\nimmediately after all the inherited constructors have completed.\nIts return value is ignored.\n\n:methods [ [name [param-types] return-type], ...]\n\nThe generated class automatically defines all of the non-private\nmethods of its superclasses/interfaces. This parameter can be used\nto specify the signatures of additional methods of the generated\nclass. Static methods can be specified with ^{:static true} in the\nsignature's metadata. Do not repeat superclass/interface signatures\nhere.\n\n:main boolean\n\nIf supplied and true, a static public main function will be generated. It will\npass each string of the String[] argument as a separate argument to\na function called (str prefix main).\n\n:factory name\n\nIf supplied, a (set of) public static factory function(s) will be\ncreated with the given name, and the same signature(s) as the\nconstructor(s).\n\n:state name\n\nIf supplied, a public final instance field with the given name will be\ncreated. You must supply an :init function in order to provide a\nvalue for the state. Note that, though final, the state can be a ref\nor agent, supporting the creation of Java objects with transactional\nor asynchronous mutation semantics.\n\n:exposes {protected-field-name {:get name :set name}, ...}\n\nSince the implementations of the methods of the generated class\noccur in Clojure functions, they have no access to the inherited\nprotected fields of the superclass. This parameter can be used to\ngenerate public getter/setter methods exposing the protected field(s)\nfor use in the implementation.\n\n:exposes-methods {super-method-name exposed-name, ...}\n\nIt is sometimes necessary to call the superclass' implementation of an\noverridden method.  Those methods may be exposed and referred in \nthe new method implementation by a local name.\n\n:prefix string\n\nDefault: \"-\" Methods called e.g. Foo will be looked up in vars called\nprefixFoo in the implementing ns.\n\n:impl-ns name\n\nDefault: the name of the current ns. Implementations of methods will be \nlooked up in this namespace.\n\n:load-impl-ns boolean\n\nDefault: true. Causes the static initializer for the generated class\nto reference the load code for the implementing namespace. Should be\ntrue when implementing-ns is the default, false if you intend to\nload the code via some other method.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/gen-class"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/38705b49fd3dbae11e94c576ef49ff3eb1c47395/src/clj/clojure/genclass.clj",
   :added "1.0",
   :name "gen-interface",
   :file "src/clj/clojure/genclass.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/38705b49fd3dbae11e94c576ef49ff3eb1c47395/src/clj/clojure/genclass.clj#L688",
   :line 688,
   :var-type "macro",
   :arglists ([& options]),
   :doc
   "When compiling, generates compiled bytecode for an interface with\n the given package-qualified :name (which, as all names in these\n parameters, can be a string or symbol), and writes the .class file\n to the *compile-path* directory.  When not compiling, does nothing.\n\n In all subsequent sections taking types, the primitive types can be\n referred to by their Java names (int, float etc), and classes in the\n java.lang package can be used without a package qualifier. All other\n classes must be fully qualified.\n\n Options should be a set of key/value pairs, all except for :name are\n optional:\n\n :name aname\n\n The package-qualified name of the class to be generated\n\n :extends [interface ...]\n\n One or more interfaces, which will be extended by this interface.\n\n :methods [ [name [param-types] return-type], ...]\n\n This parameter is used to specify the signatures of the methods of\n the generated interface.  Do not repeat superinterface signatures\n here.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/gen-interface"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "gensym",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L606",
   :line 606,
   :var-type "function",
   :arglists ([] [prefix-string]),
   :doc
   "Returns a new symbol with a unique name. If a prefix string is\nsupplied, the name is prefix# where # is some unique number. If\nprefix is not supplied, the prefix is 'G__'.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/gensym"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "get",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1508",
   :line 1508,
   :var-type "function",
   :arglists ([map key] [map key not-found]),
   :doc
   "Returns the value mapped to key, not-found or nil if key not present\nin associative collection, set, string, array, or ILookup instance.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/get"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.2",
   :name "get-in",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6178",
   :line 6178,
   :var-type "function",
   :arglists ([m ks] [m ks not-found]),
   :doc
   "Returns the value in a nested associative structure,\nwhere ks is a sequence of keys. Returns nil if the key\nis not present, or the not-found value if supplied.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/get-in"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "get-method",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1834",
   :line 1834,
   :var-type "function",
   :arglists ([multifn dispatch-val]),
   :doc
   "Given a multimethod and a dispatch value, returns the dispatch fn\nthat would apply to that value, or nil if none apply and no default",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/get-method"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/658693f6cf97e6ab0ff789e096c9eb6654e4d3ab/src/clj/clojure/core_proxy.clj",
   :added "1.0",
   :name "get-proxy-class",
   :file "src/clj/clojure/core_proxy.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/658693f6cf97e6ab0ff789e096c9eb6654e4d3ab/src/clj/clojure/core_proxy.clj#L281",
   :line 281,
   :var-type "function",
   :arglists ([& bases]),
   :doc
   "Takes an optional single class followed by zero or more\ninterfaces. If not supplied class defaults to Object.  Creates an\nreturns an instance of a proxy class derived from the supplied\nclasses. The resulting value is cached and used for any subsequent\nrequests for the same class set. Returns a Class object.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/get-proxy-class"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.1",
   :name "get-thread-bindings",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1956",
   :line 1956,
   :var-type "function",
   :arglists ([]),
   :doc
   "Get a map with the Var/value pairs which is currently in effect for the\ncurrent thread.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/get-thread-bindings"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "get-validator",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2417",
   :line 2417,
   :var-type "function",
   :arglists ([iref]),
   :doc "Gets the validator-fn for a var/ref/agent/atom.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/get-validator"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.2",
   :name "group-by",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7215",
   :line 7215,
   :var-type "function",
   :arglists ([f coll]),
   :doc
   "Returns a map of the elements of coll keyed by the result of\nf on each element. The value at each key will be a vector of the\ncorresponding elements, in the order they appeared in coll.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/group-by"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.9",
   :name "halt-when",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7700",
   :line 7700,
   :var-type "function",
   :arglists ([pred] [pred retf]),
   :doc
   "Returns a transducer that ends transduction when pred returns true\nfor an input. When retf is supplied it must be a fn of 2 arguments -\nit will be passed the (completed) result so far and the input that\ntriggered the predicate, and its return value (if it does not throw\nan exception) will be the return value of the transducer. If retf\nis not supplied, the input that triggered the predicate will be\nreturned. If the predicate never returns true the transduction is\nunaffected.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/halt-when"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "hash",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5191",
   :line 5191,
   :var-type "function",
   :arglists ([x]),
   :doc
   "Returns the hash code of its argument. Note this is the hash code\nconsistent with =, and thus is different than .hashCode for Integer,\nShort, Byte and Clojure collections.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/hash"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "hash-map",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L381",
   :line 381,
   :var-type "function",
   :arglists ([] [& keyvals]),
   :doc
   "keyval => key val\nReturns a new hash map with supplied mappings.  If any keys are\nequal, they are handled as if by repeated uses of assoc.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/hash-map"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.6",
   :name "hash-ordered-coll",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5212",
   :line 5212,
   :var-type "function",
   :arglists ([coll]),
   :doc
   "Returns the hash code, consistent with =, for an external ordered\ncollection implementing Iterable.\nSee http://clojure.org/data_structures#hash for full algorithms.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/hash-ordered-coll"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "hash-set",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L391",
   :line 391,
   :var-type "function",
   :arglists ([] [& keys]),
   :doc
   "Returns a new hash set with supplied keys.  Any equal keys are\nhandled as if by repeated uses of conj.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/hash-set"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.6",
   :name "hash-unordered-coll",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5221",
   :line 5221,
   :var-type "function",
   :arglists ([coll]),
   :doc
   "Returns the hash code, consistent with =, for an external unordered\ncollection implementing Iterable. For maps, the iterator should\nreturn map entries whose hash is computed as\n  (hash-ordered-coll [k v]).\nSee http://clojure.org/data_structures#hash for full algorithms.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/hash-unordered-coll"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.9",
   :name "ident?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1627",
   :line 1627,
   :var-type "function",
   :arglists ([x]),
   :doc "Return true if x is a symbol or keyword",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ident?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "identical?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L777",
   :line 777,
   :var-type "function",
   :arglists ([x y]),
   :doc "Tests if 2 arguments are the same object",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/identical?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "identity",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1465",
   :line 1465,
   :var-type "function",
   :arglists ([x]),
   :doc "Returns its argument.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/identity"}
  {:raw-source-url nil,
   :added "1.0",
   :name "if",
   :file nil,
   :source-url nil,
   :var-type "special form",
   :arglists nil,
   :doc
   "Evaluates test. If not the singular values nil or false,\nevaluates and yields then, otherwise, evaluates and yields else. If\nelse is not supplied it defaults to nil.\n\nPlease see https://clojure.org/reference/special_forms#if",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/if",
   :forms [(if test then else?)]}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "if-let",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1858",
   :line 1858,
   :var-type "macro",
   :arglists ([bindings then] [bindings then else & oldform]),
   :doc
   "bindings => binding-form test\n\nIf test is true, evaluates then with binding-form bound to the value of \ntest, if not, yields else",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/if-let"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "if-not",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L769",
   :line 769,
   :var-type "macro",
   :arglists ([test then] [test then else]),
   :doc
   "Evaluates test. If logical false, evaluates and returns then expr, \notherwise else expr, if supplied, else nil.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/if-not"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.6",
   :name "if-some",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1893",
   :line 1893,
   :var-type "macro",
   :arglists ([bindings then] [bindings then else & oldform]),
   :doc
   "bindings => binding-form test\n\nIf test is not nil, evaluates then with binding-form bound to the\nvalue of test, if not, yields else",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/if-some"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "ifn?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6266",
   :line 6266,
   :var-type "function",
   :arglists ([x]),
   :doc
   "Returns true if x implements IFn. Note that many data structures\n(e.g. sets and maps) implement IFn",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ifn?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "import",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3427",
   :line 3427,
   :var-type "macro",
   :arglists ([& import-symbols-or-lists]),
   :doc
   "import-list => (package-symbol class-name-symbols*)\n\nFor each name in class-name-symbols, adds a mapping from name to the\nclass named by package.name to the current namespace. Use :import in the ns\nmacro in preference to calling this directly.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/import"}
  {:raw-source-url nil,
   :added "1.0",
   :name "in-ns",
   :file nil,
   :source-url nil,
   :var-type "function",
   :arglists ([name]),
   :doc
   "Sets *ns* to the namespace named by the symbol, creating it if needed.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/in-ns"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.2",
   :name "inc",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L924",
   :line 924,
   :var-type "function",
   :arglists ([x]),
   :doc
   "Returns a number one greater than num. Does not auto-promote\nlongs, will throw on overflow. See also: inc'",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/inc"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "inc'",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L917",
   :line 917,
   :var-type "function",
   :arglists ([x]),
   :doc
   "Returns a number one greater than num. Supports arbitrary precision.\nSee also: inc",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/inc'"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.9",
   :name "indexed?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6310",
   :line 6310,
   :var-type "function",
   :arglists ([coll]),
   :doc
   "Return true if coll implements Indexed, indicating efficient lookup by index",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/indexed?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.11",
   :name "infinite?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L8100",
   :line 8100,
   :var-type "function",
   :arglists ([num]),
   :doc
   "Returns true if num is negative or positive infinity, else false",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/infinite?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/658693f6cf97e6ab0ff789e096c9eb6654e4d3ab/src/clj/clojure/core_proxy.clj",
   :added "1.0",
   :name "init-proxy",
   :file "src/clj/clojure/core_proxy.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/658693f6cf97e6ab0ff789e096c9eb6654e4d3ab/src/clj/clojure/core_proxy.clj#L302",
   :line 302,
   :var-type "function",
   :arglists ([proxy mappings]),
   :doc
   "Takes a proxy instance and a map of strings (which must\ncorrespond to methods of the proxy superclass/superinterfaces) to\nfns (which must take arguments matching the corresponding method,\nplus an additional (explicit) first arg corresponding to this, and\nsets the proxy's fn map.  Returns the proxy.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/init-proxy"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.9",
   :name "inst-ms",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6839",
   :line 6839,
   :var-type "function",
   :arglists ([inst]),
   :doc
   "Return the number of milliseconds since January 1, 1970, 00:00:00 GMT",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/inst-ms"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.9",
   :name "inst?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6845",
   :line 6845,
   :var-type "function",
   :arglists ([x]),
   :doc "Return true if x satisfies Inst",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/inst?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "instance?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L141",
   :line 141,
   :var-type "function",
   :arglists ([c x]),
   :doc
   "Evaluates x and tests if it is an instance of the class\nc. Returns true or false",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/instance?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "int",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L884",
   :line 884,
   :var-type "function",
   :arglists ([x]),
   :doc "Coerce to int",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/int"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "int-array",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5358",
   :line 5358,
   :var-type "function",
   :arglists ([size-or-seq] [size init-val-or-seq]),
   :doc "Creates an array of ints",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/int-array"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.9",
   :name "int?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1414",
   :line 1414,
   :var-type "function",
   :arglists ([x]),
   :doc "Return true if x is a fixed precision integer",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/int?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "integer?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1388",
   :line 1388,
   :var-type "function",
   :arglists ([n]),
   :doc "Returns true if n is an integer",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/integer?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "interleave",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4311",
   :line 4311,
   :var-type "function",
   :arglists ([] [c1] [c1 c2] [c1 c2 & colls]),
   :doc
   "Returns a lazy seq of the first item in each coll, then the second etc.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/interleave"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "intern",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6353",
   :line 6353,
   :var-type "function",
   :arglists ([ns name] [ns name val]),
   :doc
   "Finds or creates a var named by the symbol name in the namespace\nns (which can be a symbol or a namespace), setting its root binding\nto val if supplied. The namespace must exist. The var will adopt any\nmetadata from the name symbol.  Returns the var.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/intern"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "interpose",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5232",
   :line 5232,
   :var-type "function",
   :arglists ([sep] [sep coll]),
   :doc
   "Returns a lazy seq of the elements of coll separated by sep.\nReturns a stateful transducer when no collection is provided.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/interpose"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "into",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6951",
   :line 6951,
   :var-type "function",
   :arglists ([] [to] [to from] [to xform from]),
   :doc
   "Returns a new coll consisting of to-coll with all of the items of\nfrom-coll conjoined. A transducer may be supplied.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/into"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "into-array",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3445",
   :line 3445,
   :var-type "function",
   :arglists ([aseq] [type aseq]),
   :doc
   "Returns an array with components set to the values in aseq. The array's\ncomponent type is type if provided, or the type of the first value in\naseq if present, or Object. All values in aseq must be compatible with\nthe component type. Class objects for the primitive types can be obtained\nusing, e.g., Integer/TYPE.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/into-array"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "ints",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5399",
   :line 5399,
   :var-type "function",
   :arglists ([xs]),
   :doc "Casts to int[]",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ints"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "io!",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2529",
   :line 2529,
   :var-type "macro",
   :arglists ([& body]),
   :doc
   "If an io! block occurs in a transaction, throws an\nIllegalStateException, else runs body in an implicit do. If the\nfirst expression in body is a literal string, will use that as the\nexception message.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/io!"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "isa?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5590",
   :line 5590,
   :var-type "function",
   :arglists ([child parent] [h child parent]),
   :doc
   "Returns true if (= child parent), or child is directly or indirectly derived from\nparent, either via a Java type inheritance relationship or a\nrelationship established via derive. h must be a hierarchy obtained\nfrom make-hierarchy, if not supplied defaults to the global\nhierarchy",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/isa?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "iterate",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3030",
   :line 3030,
   :var-type "function",
   :arglists ([f x]),
   :doc
   "Returns a lazy (infinite!) sequence of x, (f x), (f (f x)) etc.\nf must be free of side-effects",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/iterate"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.11",
   :name "iteration",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7787",
   :line 7787,
   :var-type "function",
   :arglists
   ([step
     &
     {:keys [somef vf kf initk],
      :or {vf identity, kf identity, somef some?, initk nil}}]),
   :doc
   "Creates a seqable/reducible via repeated calls to step,\na function of some (continuation token) 'k'. The first call to step\nwill be passed initk, returning 'ret'. Iff (somef ret) is true,\n(vf ret) will be included in the iteration, else iteration will\nterminate and vf/kf will not be called. If (kf ret) is non-nil it\nwill be passed to the next step call, else iteration will terminate.\n\nThis can be used e.g. to consume APIs that return paginated or batched data.\n\n step - (possibly impure) fn of 'k' -> 'ret'\n\n :somef - fn of 'ret' -> logical true/false, default 'some?'\n :vf - fn of 'ret' -> 'v', a value produced by the iteration, default 'identity'\n :kf - fn of 'ret' -> 'next-k' or nil (signaling 'do not continue'), default 'identity'\n :initk - the first value passed to step, default 'nil'\n\nIt is presumed that step with non-initk is unreproducible/non-idempotent.\nIf step with initk is unreproducible it is on the consumer to not consume twice.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/iteration"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "iterator-seq",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5747",
   :line 5747,
   :var-type "function",
   :arglists ([iter]),
   :doc
   "Returns a seq on a java.util.Iterator. Note that most collections\nproviding iterators implement Iterable and thus support seq directly.\nSeqs cache values, thus iterator-seq should not be used on any\niterator that repeatedly returns the same mutable object.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/iterator-seq"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.1",
   :name "juxt",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2593",
   :line 2593,
   :var-type "function",
   :arglists ([f] [f g] [f g h] [f g h & fs]),
   :doc
   "Takes a set of functions and returns a fn that is the juxtaposition\nof those fns.  The returned fn takes a variable number of args, and\nreturns a vector containing the result of applying each fn to the\nargs (left-to-right).\n((juxt a b c) x) => [(a x) (b x) (c x)]",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/juxt"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.2",
   :name "keep",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7382",
   :line 7382,
   :var-type "function",
   :arglists ([f] [f coll]),
   :doc
   "Returns a lazy sequence of the non-nil results of (f item). Note,\nthis means false return values will be included.  f must be free of\nside-effects.  Returns a transducer when no collection is provided.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/keep"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.2",
   :name "keep-indexed",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7415",
   :line 7415,
   :var-type "function",
   :arglists ([f] [f coll]),
   :doc
   "Returns a lazy sequence of the non-nil results of (f index item). Note,\nthis means false return values will be included.  f must be free of\nside-effects.  Returns a stateful transducer when no collection is\nprovided.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/keep-indexed"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "key",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1582",
   :line 1582,
   :var-type "function",
   :arglists ([e]),
   :doc "Returns the key of the map entry.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/key"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "keys",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1570",
   :line 1570,
   :var-type "function",
   :arglists ([map]),
   :doc
   "Returns a sequence of the map's keys, in the same order as (seq map).",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/keys"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "keyword",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L616",
   :line 616,
   :var-type "function",
   :arglists ([name] [ns name]),
   :doc
   "Returns a Keyword with the given namespace and name.  Do not use :\nin the keyword strings, it will be added automatically.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/keyword"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "keyword?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L570",
   :line 570,
   :var-type "function",
   :arglists ([x]),
   :doc "Return true if x is a Keyword",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/keyword?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "last",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L264",
   :line 264,
   :var-type "function",
   :arglists ([coll]),
   :doc "Return the last item in coll, in linear time",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/last"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "lazy-cat",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4639",
   :line 4639,
   :var-type "macro",
   :arglists ([& colls]),
   :doc
   "Expands to code which yields a lazy sequence of the concatenation\nof the supplied colls.  Each coll expr is not evaluated until it is\nneeded. \n\n(lazy-cat xs ys zs) === (concat (lazy-seq xs) (lazy-seq ys) (lazy-seq zs))",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/lazy-cat"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "lazy-seq",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L685",
   :line 685,
   :var-type "macro",
   :arglists ([& body]),
   :doc
   "Takes a body of expressions that returns an ISeq or nil, and yields\na Seqable object that will invoke the body only the first time seq\nis called, and will cache the result and return it on all subsequent\nseq calls. See also - realized?",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/lazy-seq"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "let",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4499",
   :line 4499,
   :var-type "special form",
   :arglists ([bindings & body]),
   :doc
   "binding => binding-form init-expr\nbinding-form => name, or destructuring-form\ndestructuring-form => map-destructure-form, or seq-destructure-form\n\nEvaluates the exprs in a lexical context in which the symbols in\nthe binding-forms are bound to their respective init-exprs or parts\ntherein.\n\nSee https://clojure.org/reference/special_forms#binding-forms for\nmore information about destructuring.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/let",
   :forms [(let [bindings*] exprs*)]}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "letfn",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6582",
   :line 6582,
   :var-type "special form",
   :arglists ([fnspecs & body]),
   :doc
   "fnspec ==> (fname [params*] exprs) or (fname ([params*] exprs)+)\n\nTakes a vector of function specs and a body, and generates a set of\nbindings of functions to their names. All of the names are available\nin all of the definitions of the functions, as well as the body.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/letfn",
   :forms [(letfn [fnspecs*] exprs*)]}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "line-seq",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3087",
   :line 3087,
   :var-type "function",
   :arglists ([rdr]),
   :doc
   "Returns the lines of text from rdr as a lazy sequence of strings.\nrdr must implement java.io.BufferedReader.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/line-seq"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "list",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L16",
   :line 16,
   :var-type "function",
   :arglists ([& items]),
   :doc "Creates a new list containing the items.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/list"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "list*",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L650",
   :line 650,
   :var-type "function",
   :arglists
   ([args] [a args] [a b args] [a b c args] [a b c d & more]),
   :doc
   "Creates a new seq containing the items prepended to the rest, the\nlast of which will be treated as a sequence.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/list*"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "list?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6255",
   :line 6255,
   :var-type "function",
   :arglists ([x]),
   :doc "Returns true if x implements IPersistentList",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/list?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "load",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6145",
   :line 6145,
   :var-type "function",
   :arglists ([& paths]),
   :doc
   "Loads Clojure code from resources in classpath. A path is interpreted as\nclasspath-relative if it begins with a slash or relative to the root\ndirectory for the current namespace otherwise.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/load"}
  {:raw-source-url nil,
   :added "1.0",
   :name "load-file",
   :file nil,
   :source-url nil,
   :var-type "function",
   :arglists ([name]),
   :doc
   "Sequentially read and evaluate the set of forms contained in the file.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/load-file"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "load-reader",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4084",
   :line 4084,
   :var-type "function",
   :arglists ([rdr]),
   :doc
   "Sequentially read and evaluate the set of forms contained in the\nstream/file",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/load-reader"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "load-string",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4091",
   :line 4091,
   :var-type "function",
   :arglists ([s]),
   :doc
   "Sequentially read and evaluate the set of forms contained in the\nstring",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/load-string"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "loaded-libs",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6140",
   :line 6140,
   :var-type "function",
   :arglists ([]),
   :doc
   "Returns a sorted set of symbols naming the currently loaded libs",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/loaded-libs"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "locking",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1662",
   :line 1662,
   :var-type "macro",
   :arglists ([x & body]),
   :doc
   "Executes exprs in an implicit do, while holding the monitor of x.\nWill release the monitor of x in all circumstances.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/locking"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "long",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3482",
   :line 3482,
   :var-type "function",
   :arglists ([x]),
   :doc "Coerce to long",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/long"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "long-array",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5366",
   :line 5366,
   :var-type "function",
   :arglists ([size-or-seq] [size init-val-or-seq]),
   :doc "Creates an array of longs",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/long-array"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "longs",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5409",
   :line 5409,
   :var-type "function",
   :arglists ([xs]),
   :doc "Casts to long[]",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/longs"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "loop",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4600",
   :line 4600,
   :var-type "special form",
   :arglists ([bindings & body]),
   :doc
   "Evaluates the exprs in a lexical context in which the symbols in\nthe binding-forms are bound to their respective init-exprs or parts\ntherein. Acts as a recur target.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/loop",
   :forms [(loop [bindings*] exprs*)]}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "macroexpand",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4028",
   :line 4028,
   :var-type "function",
   :arglists ([form]),
   :doc
   "Repeatedly calls macroexpand-1 on form until it no longer\nrepresents a macro form, then returns it.  Note neither\nmacroexpand-1 nor macroexpand expand macros in subforms.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/macroexpand"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "macroexpand-1",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4020",
   :line 4020,
   :var-type "function",
   :arglists ([form]),
   :doc
   "If form represents a macro form, returns its expansion,\nelse returns form.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/macroexpand-1"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "make-array",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3988",
   :line 3988,
   :var-type "function",
   :arglists ([type len] [type dim & more-dims]),
   :doc
   "Creates and returns an array of instances of the specified class of\nthe specified dimension(s).  Note that a class object is required.\nClass objects can be obtained by using their imported or\nfully-qualified name.  Class objects for the primitive types can be\nobtained using, e.g., Integer/TYPE.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/make-array"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "make-hierarchy",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5554",
   :line 5554,
   :var-type "function",
   :arglists ([]),
   :doc "Creates a hierarchy object for use with derive, isa? etc.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/make-hierarchy"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "map",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2744",
   :line 2744,
   :var-type "function",
   :arglists
   ([f] [f coll] [f c1 c2] [f c1 c2 c3] [f c1 c2 c3 & colls]),
   :doc
   "Returns a lazy sequence consisting of the result of applying f to\nthe set of first items of each coll, followed by applying f to the\nset of second items in each coll, until any one of the colls is\nexhausted.  Any remaining items in other colls are ignored. Function\nf should accept number-of-colls arguments. Returns a transducer when\nno collection is provided.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/map"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.8",
   :name "map-entry?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1492",
   :line 1492,
   :var-type "function",
   :arglists ([x]),
   :doc "Return true if x is a map entry",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/map-entry?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.2",
   :name "map-indexed",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7352",
   :line 7352,
   :var-type "function",
   :arglists ([f] [f coll]),
   :doc
   "Returns a lazy sequence consisting of the result of applying f to 0\nand the first item of coll, followed by applying f to 1 and the second\nitem in coll, etc, until coll is exhausted. Thus function f should\naccept 2 arguments, index and item. Returns a stateful transducer when\nno collection is provided.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/map-indexed"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "map?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L169",
   :line 169,
   :var-type "function",
   :arglists ([x]),
   :doc "Return true if x implements IPersistentMap",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/map?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "mapcat",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2800",
   :line 2800,
   :var-type "function",
   :arglists ([f] [f & colls]),
   :doc
   "Returns the result of applying concat to the result of applying map\nto f and colls.  Thus function f should return a collection. Returns\na transducer when no collections are provided",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/mapcat"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.4",
   :name "mapv",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6971",
   :line 6971,
   :var-type "function",
   :arglists ([f coll] [f c1 c2] [f c1 c2 c3] [f c1 c2 c3 & colls]),
   :doc
   "Returns a vector consisting of the result of applying f to the\nset of first items of each coll, followed by applying f to the set\nof second items in each coll, until any one of the colls is\nexhausted.  Any remaining items in other colls are ignored. Function\nf should accept number-of-colls arguments.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/mapv"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "max",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1117",
   :line 1117,
   :var-type "function",
   :arglists ([x] [x y] [x y & more]),
   :doc "Returns the greatest of the nums.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/max"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "max-key",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5015",
   :line 5015,
   :var-type "function",
   :arglists ([k x] [k x y] [k x y & more]),
   :doc
   "Returns the x for which (k x), a number, is greatest.\n\nIf there are multiple such xs, the last one is returned.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/max-key"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "memfn",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3873",
   :line 3873,
   :var-type "macro",
   :arglists ([name & args]),
   :doc
   "Expands into code that creates a fn that expects to be passed an\nobject and any args and calls the named instance method on the\nobject passing the args. Use when you want to treat a Java method as\na first-class fn. name may be type-hinted with the method receiver's\ntype in order to avoid reflective calls.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/memfn"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "memoize",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6379",
   :line 6379,
   :var-type "function",
   :arglists ([f]),
   :doc
   "Returns a memoized version of a referentially transparent function. The\nmemoized version of the function keeps a cache of the mapping from arguments\nto results and, when calls with the same arguments are repeated often, has\nhigher performance at the expense of higher memory use.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/memoize"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "merge",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3059",
   :line 3059,
   :var-type "function",
   :arglists ([& maps]),
   :doc
   "Returns a map that consists of the rest of the maps conj-ed onto\nthe first.  If a key occurs in more than one map, the mapping from\nthe latter (left-to-right) will be the mapping in the result.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/merge"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "merge-with",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3069",
   :line 3069,
   :var-type "function",
   :arglists ([f & maps]),
   :doc
   "Returns a map that consists of the rest of the maps conj-ed onto\nthe first.  If a key occurs in more than one map, the mapping(s)\nfrom the latter (left-to-right) will be combined with the mapping in\nthe result by calling (f val-in-result val-in-latter).",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/merge-with"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "meta",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L204",
   :line 204,
   :var-type "function",
   :arglists ([obj]),
   :doc
   "Returns the metadata of obj, returns nil if there is no metadata.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/meta"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "methods",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1828",
   :line 1828,
   :var-type "function",
   :arglists ([multifn]),
   :doc
   "Given a multimethod, returns a map of dispatch values -> dispatch fns",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/methods"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "min",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1127",
   :line 1127,
   :var-type "function",
   :arglists ([x] [x y] [x y & more]),
   :doc "Returns the least of the nums.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/min"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "min-key",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5035",
   :line 5035,
   :var-type "function",
   :arglists ([k x] [k x y] [k x y & more]),
   :doc
   "Returns the x for which (k x), a number, is least.\n\nIf there are multiple such xs, the last one is returned.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/min-key"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.6",
   :name "mix-collection-hash",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5201",
   :line 5201,
   :var-type "function",
   :arglists ([hash-basis count]),
   :doc
   "Mix final collection hash for ordered or unordered collections.\nhash-basis is the combined collection hash, count is the number\nof elements included in the basis. Note this is the hash code\nconsistent with =, different from .hashCode.\nSee http://clojure.org/data_structures#hash for full algorithms.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/mix-collection-hash"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "mod",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3568",
   :line 3568,
   :var-type "function",
   :arglists ([num div]),
   :doc "Modulus of num and div. Truncates toward negative infinity.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/mod"}
  {:raw-source-url nil,
   :added "1.0",
   :name "monitor-enter",
   :file nil,
   :source-url nil,
   :var-type "special form",
   :arglists nil,
   :doc
   "Synchronization primitive that should be avoided\nin user code. Use the 'locking' macro.\n\nPlease see https://clojure.org/reference/special_forms#monitor-enter",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/monitor-enter",
   :forms [(monitor-enter x)]}
  {:raw-source-url nil,
   :added "1.0",
   :name "monitor-exit",
   :file nil,
   :source-url nil,
   :var-type "special form",
   :arglists nil,
   :doc
   "Synchronization primitive that should be avoided\nin user code. Use the 'locking' macro.\n\nPlease see https://clojure.org/reference/special_forms#monitor-exit",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/monitor-exit",
   :forms [(monitor-exit x)]}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "name",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1604",
   :line 1604,
   :var-type "function",
   :arglists ([x]),
   :doc "Returns the name String of a string, symbol or keyword.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/name"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "namespace",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1612",
   :line 1612,
   :var-type "function",
   :arglists ([x]),
   :doc
   "Returns the namespace String of a symbol or keyword, or nil if not present.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/namespace"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/8957a93099fc506c3b24ed5739bf9e2fc1811bef/src/clj/clojure/core_deftype.clj",
   :added "1.2",
   :name "namespace-munge",
   :file "src/clj/clojure/core_deftype.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/8957a93099fc506c3b24ed5739bf9e2fc1811bef/src/clj/clojure/core_deftype.clj#L13",
   :line 13,
   :var-type "function",
   :arglists ([ns]),
   :doc
   "Convert a Clojure namespace name to a legal Java package name.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/namespace-munge"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.9",
   :name "nat-int?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1434",
   :line 1434,
   :var-type "function",
   :arglists ([x]),
   :doc "Return true if x is a non-negative fixed precision integer",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/nat-int?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.9",
   :name "neg-int?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1428",
   :line 1428,
   :var-type "function",
   :arglists ([x]),
   :doc "Return true if x is a negative fixed precision integer",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/neg-int?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "neg?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1268",
   :line 1268,
   :var-type "function",
   :arglists ([num]),
   :doc "Returns true if num is less than zero, else false",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/neg?"}
  {:raw-source-url nil,
   :added "1.0",
   :name "new",
   :file nil,
   :source-url nil,
   :var-type "special form",
   :arglists nil,
   :doc
   "The args, if any, are evaluated from left to right, and\npassed to the constructor of the class named by Classname. The\nconstructed object is returned.\n\nPlease see https://clojure.org/java_interop#new",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/new",
   :forms [(Classname. args*) (new Classname args*)]}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "newline",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3699",
   :line 3699,
   :var-type "function",
   :arglists ([]),
   :doc "Writes a platform-specific newline to *out*",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/newline"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "next",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L57",
   :line 57,
   :var-type "function",
   :arglists ([coll]),
   :doc
   "Returns a seq of the items after the first. Calls seq on its\nargument.  If there are no more items, returns nil.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/next"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "nfirst",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L107",
   :line 107,
   :var-type "function",
   :arglists ([x]),
   :doc "Same as (next (first x))",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/nfirst"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "nil?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L438",
   :line 438,
   :var-type "function",
   :arglists ([x]),
   :doc "Returns true if x is nil, false otherwise.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/nil?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "nnext",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L121",
   :line 121,
   :var-type "function",
   :arglists ([x]),
   :doc "Same as (next (next x))",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/nnext"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "not",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L526",
   :line 526,
   :var-type "function",
   :arglists ([x]),
   :doc "Returns true if x is logical false, false otherwise.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/not"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "not-any?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2720",
   :line 2720,
   :var-type "function",
   :arglists ([pred coll]),
   :doc
   "Returns false if (pred x) is logical true for any x in coll,\nelse true.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/not-any?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "not-empty",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5563",
   :line 5563,
   :var-type "function",
   :arglists ([coll]),
   :doc "If coll is empty, returns nil, else coll",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/not-empty"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "not-every?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2701",
   :line 2701,
   :var-type "function",
   :arglists ([pred coll]),
   :doc
   "Returns false if (pred x) is logical true for every x in\ncoll, else true.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/not-every?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "not=",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L821",
   :line 821,
   :var-type "function",
   :arglists ([x] [x y] [x y & more]),
   :doc "Same as (not (= obj1 obj2))",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/not="}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "ns",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5790",
   :line 5790,
   :var-type "macro",
   :arglists ([name docstring? attr-map? references*]),
   :doc
   "Sets *ns* to the namespace named by name (unevaluated), creating it\nif needed.  references can be zero or more of: (:refer-clojure ...)\n(:require ...) (:use ...) (:import ...) (:load ...) (:gen-class)\nwith the syntax of refer-clojure/require/use/import/load/gen-class\nrespectively, except the arguments are unevaluated and need not be\nquoted. (:gen-class ...), when supplied, defaults to :name\ncorresponding to the ns name, :main true, :impl-ns same as ns, and\n:init-impl-ns true. All options of gen-class are\nsupported. The :gen-class directive is ignored when not\ncompiling. If :gen-class is not supplied, when compiled only an\nnsname__init.class will be generated. If :refer-clojure is not used, a\ndefault (refer 'clojure.core) is used.  Use of ns is preferred to\nindividual calls to in-ns/require/use/import:\n\n(ns foo.bar\n  (:refer-clojure :exclude [ancestors printf])\n  (:require (clojure.contrib sql combinatorics))\n  (:use (my.lib this that))\n  (:import (java.util Date Timer Random)\n           (java.sql Connection Statement)))",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ns"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "ns-aliases",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4276",
   :line 4276,
   :var-type "function",
   :arglists ([ns]),
   :doc "Returns a map of the aliases for the namespace.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ns-aliases"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "ns-imports",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4202",
   :line 4202,
   :var-type "function",
   :arglists ([ns]),
   :doc "Returns a map of the import mappings for the namespace.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ns-imports"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "ns-interns",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4209",
   :line 4209,
   :var-type "function",
   :arglists ([ns]),
   :doc "Returns a map of the intern mappings for the namespace.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ns-interns"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "ns-map",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4173",
   :line 4173,
   :var-type "function",
   :arglists ([ns]),
   :doc "Returns a map of all the mappings for the namespace.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ns-map"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "ns-name",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4166",
   :line 4166,
   :var-type "function",
   :arglists ([ns]),
   :doc "Returns the name of the namespace, a symbol.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ns-name"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "ns-publics",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4191",
   :line 4191,
   :var-type "function",
   :arglists ([ns]),
   :doc
   "Returns a map of the public intern mappings for the namespace.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ns-publics"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "ns-refers",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4256",
   :line 4256,
   :var-type "function",
   :arglists ([ns]),
   :doc "Returns a map of the refer mappings for the namespace.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ns-refers"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "ns-resolve",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4361",
   :line 4361,
   :var-type "function",
   :arglists ([ns sym] [ns env sym]),
   :doc
   "Returns the var or Class to which a symbol will be resolved in the\nnamespace (unless found in the environment), else nil.  Note that\nif the symbol is fully qualified, the var/Class to which it resolves\nneed not be present in the namespace.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ns-resolve"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "ns-unalias",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4283",
   :line 4283,
   :var-type "function",
   :arglists ([ns sym]),
   :doc "Removes the alias for the symbol from the namespace.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ns-unalias"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "ns-unmap",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4180",
   :line 4180,
   :var-type "function",
   :arglists ([ns sym]),
   :doc "Removes the mappings for the symbol from the namespace.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ns-unmap"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "nth",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L891",
   :line 891,
   :var-type "function",
   :arglists ([coll index] [coll index not-found]),
   :doc
   "Returns the value at the index. get returns nil if index out of\nbounds, nth throws an exception unless not-found is supplied.  nth\nalso works for strings, Java arrays, regex Matchers and Lists, and,\nin O(n) time, for sequences.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/nth"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "nthnext",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3166",
   :line 3166,
   :var-type "function",
   :arglists ([coll n]),
   :doc "Returns the nth next of coll, (seq coll) when n is 0.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/nthnext"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.3",
   :name "nthrest",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3176",
   :line 3176,
   :var-type "function",
   :arglists ([coll n]),
   :doc "Returns the nth rest of coll, coll when n is 0.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/nthrest"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "num",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3475",
   :line 3475,
   :var-type "function",
   :arglists ([x]),
   :doc "Coerce to Number",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/num"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "number?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3561",
   :line 3561,
   :var-type "function",
   :arglists ([x]),
   :doc "Returns true if x is a Number",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/number?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.2",
   :name "numerator",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3584",
   :line 3584,
   :var-type "function",
   :arglists ([r]),
   :doc "Returns the numerator part of a Ratio.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/numerator"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.2",
   :name "object-array",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5351",
   :line 5351,
   :var-type "function",
   :arglists ([size-or-seq]),
   :doc "Creates an array of objects",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/object-array"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "odd?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1408",
   :line 1408,
   :var-type "function",
   :arglists ([n]),
   :doc
   "Returns true if n is odd, throws an exception if n is not an integer",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/odd?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "or",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L856",
   :line 856,
   :var-type "macro",
   :arglists ([] [x] [x & next]),
   :doc
   "Evaluates exprs one at a time, from left to right. If a form\nreturns a logical true value, or returns that value and doesn't\nevaluate any of the other expressions, otherwise it returns the\nvalue of the last expression. (or) returns nil.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/or"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "parents",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5611",
   :line 5611,
   :var-type "function",
   :arglists ([tag] [h tag]),
   :doc
   "Returns the immediate parents of tag, either via a Java type\ninheritance relationship or a relationship established via derive. h\nmust be a hierarchy obtained from make-hierarchy, if not supplied\ndefaults to the global hierarchy",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/parents"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.11",
   :name "parse-boolean",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L8080",
   :line 8080,
   :var-type "function",
   :arglists ([s]),
   :doc
   "Parse strings \"true\" or \"false\" and return a boolean, or nil if invalid",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/parse-boolean"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.11",
   :name "parse-double",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L8056",
   :line 8056,
   :var-type "function",
   :arglists ([s]),
   :doc
   "Parse string with floating point components and return a Double value,\nor nil if parse fails.\n\nGrammar: https://docs.oracle.com/javase/8/docs/api/java/lang/Double.html#valueOf-java.lang.String-",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/parse-double"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.11",
   :name "parse-long",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L8045",
   :line 8045,
   :var-type "function",
   :arglists ([s]),
   :doc
   "Parse string of decimal digits with optional leading -/+ and return a\nLong value, or nil if parse fails",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/parse-long"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.11",
   :name "parse-uuid",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L8069",
   :line 8069,
   :var-type "function",
   :arglists ([s]),
   :doc
   "Parse a string representing a UUID and return a java.util.UUID instance,\nor nil if parse fails.\n\nGrammar: https://docs.oracle.com/javase/8/docs/api/java/util/UUID.html#toString--",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/parse-uuid"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "partial",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2631",
   :line 2631,
   :var-type "function",
   :arglists
   ([f]
    [f arg1]
    [f arg1 arg2]
    [f arg1 arg2 arg3]
    [f arg1 arg2 arg3 & more]),
   :doc
   "Takes a function f and fewer than the normal arguments to f, and\nreturns a fn that takes a variable number of additional args. When\ncalled, the returned function calls f with args + additional args.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/partial"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "partition",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3186",
   :line 3186,
   :var-type "function",
   :arglists ([n coll] [n step coll] [n step pad coll]),
   :doc
   "Returns a lazy sequence of lists of n items each, at offsets step\napart. If step is not supplied, defaults to n, i.e. the partitions\ndo not overlap. If a pad collection is supplied, use its elements as\nnecessary to complete last partition upto n items. In case there are\nnot enough padding elements, return a partition with less than n items.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/partition"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.2",
   :name "partition-all",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7309",
   :line 7309,
   :var-type "function",
   :arglists ([n] [n coll] [n step coll]),
   :doc
   "Returns a lazy sequence of lists like partition, but may include\npartitions with fewer than n items at the end.  Returns a stateful\ntransducer when no collection is provided.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/partition-all"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.2",
   :name "partition-by",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7229",
   :line 7229,
   :var-type "function",
   :arglists ([f] [f coll]),
   :doc
   "Applies f to each value in coll, splitting it each time f returns a\nnew value.  Returns a lazy seq of partitions.  Returns a stateful\ntransducer when no collection is provided.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/partition-by"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "pcalls",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7105",
   :line 7105,
   :var-type "function",
   :arglists ([& fns]),
   :doc
   "Executes the no-arg fns in parallel, returning a lazy sequence of\ntheir values",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/pcalls"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "peek",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1474",
   :line 1474,
   :var-type "function",
   :arglists ([coll]),
   :doc
   "For a list or queue, same as first, for a vector, same as, but much\nmore efficient than, last. If the collection is empty, returns nil.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/peek"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.1",
   :name "persistent!",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3351",
   :line 3351,
   :var-type "function",
   :arglists ([coll]),
   :doc
   "Returns a new, persistent version of the transient collection, in\nconstant time. The transient collection cannot be used after this\ncall, any such use will throw an exception.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/persistent!"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "pmap",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7080",
   :line 7080,
   :var-type "function",
   :arglists ([f coll] [f coll & colls]),
   :doc
   "Like map, except f is applied in parallel. Semi-lazy in that the\nparallel computation stays ahead of the consumption, but doesn't\nrealize the entire result unless required. Only useful for\ncomputationally intensive functions where the time of f dominates\nthe coordination overhead.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/pmap"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "pop",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1481",
   :line 1481,
   :var-type "function",
   :arglists ([coll]),
   :doc
   "For a list or queue, returns a new list/queue without the first\nitem, for a vector, returns a new vector without the last item. If\nthe collection is empty, throws an exception.  Note - not the same\nas next/butlast.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/pop"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.1",
   :name "pop!",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3394",
   :line 3394,
   :var-type "function",
   :arglists ([coll]),
   :doc
   "Removes the last item from a transient vector. If\nthe collection is empty, throws an exception. Returns coll",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/pop!"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.1",
   :name "pop-thread-bindings",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1948",
   :line 1948,
   :var-type "function",
   :arglists ([]),
   :doc
   "Pop one set of bindings pushed with push-binding before. It is an error to\npop bindings without pushing before.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/pop-thread-bindings"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.9",
   :name "pos-int?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1422",
   :line 1422,
   :var-type "function",
   :arglists ([x]),
   :doc "Return true if x is a positive fixed precision integer",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/pos-int?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "pos?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1261",
   :line 1261,
   :var-type "function",
   :arglists ([num]),
   :doc "Returns true if num is greater than zero, else false",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/pos?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "pr",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3679",
   :dynamic true,
   :line 3679,
   :var-type "function",
   :arglists ([] [x] [x & more]),
   :doc
   "Prints the object(s) to the output stream that is the current value\nof *out*.  Prints the object(s), separated by spaces if there is\nmore than one.  By default, pr and prn print in a way that objects\ncan be read by the reader",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/pr"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "pr-str",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4761",
   :line 4761,
   :var-type "function",
   :arglists ([& xs]),
   :doc "pr to a string, returning it",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/pr-str"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "prefer-method",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1820",
   :line 1820,
   :var-type "function",
   :arglists ([multifn dispatch-val-x dispatch-val-y]),
   :doc
   "Causes the multimethod to prefer matches of dispatch-val-x over dispatch-val-y \nwhen there is a conflict",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/prefer-method"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "prefers",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1841",
   :line 1841,
   :var-type "function",
   :arglists ([multifn]),
   :doc
   "Given a multimethod, returns a map of preferred value -> set of other values",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/prefers"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "print",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3726",
   :line 3726,
   :var-type "function",
   :arglists ([& more]),
   :doc
   "Prints the object(s) to the output stream that is the current value\nof *out*.  print and println produce output for human consumption.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/print"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "print-str",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4779",
   :line 4779,
   :var-type "function",
   :arglists ([& xs]),
   :doc "print to a string, returning it",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/print-str"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "printf",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5772",
   :line 5772,
   :var-type "function",
   :arglists ([fmt & args]),
   :doc "Prints formatted output, as per format",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/printf"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "println",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3735",
   :line 3735,
   :var-type "function",
   :arglists ([& more]),
   :doc "Same as print followed by (newline)",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/println"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "println-str",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4788",
   :line 4788,
   :var-type "function",
   :arglists ([& xs]),
   :doc "println to a string, returning it",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/println-str"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "prn",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3716",
   :line 3716,
   :var-type "function",
   :arglists ([& more]),
   :doc
   "Same as pr followed by (newline). Observes *flush-on-newline*",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/prn"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "prn-str",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4770",
   :line 4770,
   :var-type "function",
   :arglists ([& xs]),
   :doc "prn to a string, returning it",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/prn-str"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.1",
   :name "promise",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7165",
   :line 7165,
   :var-type "function",
   :arglists ([]),
   :doc
   "Returns a promise object that can be read with deref/@, and set,\nonce only, with deliver. Calls to deref/@ prior to delivery will\nblock, unless the variant of deref with timeout is used. All\nsubsequent derefs will return the same delivered value without\nblocking. See also - realized?.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/promise"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/658693f6cf97e6ab0ff789e096c9eb6654e4d3ab/src/clj/clojure/core_proxy.clj",
   :added "1.0",
   :name "proxy",
   :file "src/clj/clojure/core_proxy.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/658693f6cf97e6ab0ff789e096c9eb6654e4d3ab/src/clj/clojure/core_proxy.clj#L334",
   :line 334,
   :var-type "macro",
   :arglists ([class-and-interfaces args & fs]),
   :doc
   "class-and-interfaces - a vector of class names\n\nargs - a (possibly empty) vector of arguments to the superclass\nconstructor.\n\nf => (name [params*] body) or\n(name ([params*] body) ([params+] body) ...)\n\nExpands to code which creates a instance of a proxy class that\nimplements the named class/interface(s) by calling the supplied\nfns. A single class, if provided, must be first. If not provided it\ndefaults to Object.\n\nThe interfaces names must be valid interface types. If a method fn\nis not provided for a class method, the superclass method will be\ncalled. If a method fn is not provided for an interface method, an\nUnsupportedOperationException will be thrown should it be\ncalled. Method fns are closures and can capture the environment in\nwhich proxy is called. Each method fn takes an additional implicit\nfirst arg, which is bound to 'this. Note that while method fns can\nbe provided to override protected methods, they have no other access\nto protected members, nor to super, as these capabilities cannot be\nproxied.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/proxy"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/658693f6cf97e6ab0ff789e096c9eb6654e4d3ab/src/clj/clojure/core_proxy.clj",
   :added "1.0",
   :name "proxy-mappings",
   :file "src/clj/clojure/core_proxy.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/658693f6cf97e6ab0ff789e096c9eb6654e4d3ab/src/clj/clojure/core_proxy.clj#L328",
   :line 328,
   :var-type "function",
   :arglists ([proxy]),
   :doc "Takes a proxy instance and returns the proxy's fn map.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/proxy-mappings"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/658693f6cf97e6ab0ff789e096c9eb6654e4d3ab/src/clj/clojure/core_proxy.clj",
   :added "1.0",
   :name "proxy-super",
   :file "src/clj/clojure/core_proxy.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/658693f6cf97e6ab0ff789e096c9eb6654e4d3ab/src/clj/clojure/core_proxy.clj#L396",
   :line 396,
   :var-type "macro",
   :arglists ([meth & args]),
   :doc
   "Use to call a superclass method in the body of a proxy method. \nNote, expansion captures 'this",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/proxy-super"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.1",
   :name "push-thread-bindings",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1930",
   :line 1930,
   :var-type "function",
   :arglists ([bindings]),
   :doc
   "WARNING: This is a low-level function. Prefer high-level macros like\nbinding where ever possible.\n\nTakes a map of Var/value pairs. Binds each Var to the associated value for\nthe current thread. Each call *MUST* be accompanied by a matching call to\npop-thread-bindings wrapped in a try-finally!\n\n    (push-thread-bindings bindings)\n    (try\n      ...\n      (finally\n        (pop-thread-bindings)))",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/push-thread-bindings"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "pvalues",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7112",
   :line 7112,
   :var-type "macro",
   :arglists ([& exprs]),
   :doc
   "Returns a lazy sequence of the values of the exprs, which are\nevaluated in parallel",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/pvalues"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.9",
   :name "qualified-ident?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1637",
   :line 1637,
   :var-type "function",
   :arglists ([x]),
   :doc "Return true if x is a symbol or keyword with a namespace",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/qualified-ident?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.9",
   :name "qualified-keyword?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1657",
   :line 1657,
   :var-type "function",
   :arglists ([x]),
   :doc "Return true if x is a keyword with a namespace",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/qualified-keyword?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.9",
   :name "qualified-symbol?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1647",
   :line 1647,
   :var-type "function",
   :arglists ([x]),
   :doc "Return true if x is a symbol with a namespace",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/qualified-symbol?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "quot",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1275",
   :line 1275,
   :var-type "function",
   :arglists ([num div]),
   :doc "quot[ient] of dividing numerator by denominator.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/quot"}
  {:raw-source-url nil,
   :added "1.0",
   :name "quote",
   :file nil,
   :source-url nil,
   :var-type "special form",
   :arglists nil,
   :doc
   "Yields the unevaluated form.\n\nPlease see https://clojure.org/reference/special_forms#quote",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/quote",
   :forms ['form]}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "rand",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4937",
   :line 4937,
   :var-type "function",
   :arglists ([] [n]),
   :doc
   "Returns a random floating point number between 0 (inclusive) and\nn (default 1) (exclusive).",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/rand"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "rand-int",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4945",
   :line 4945,
   :var-type "function",
   :arglists ([n]),
   :doc
   "Returns a random integer between 0 (inclusive) and n (exclusive).",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/rand-int"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.2",
   :name "rand-nth",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7300",
   :line 7300,
   :var-type "function",
   :arglists ([coll]),
   :doc
   "Return a random element of the (sequential) collection. Will have\nthe same performance characteristics as nth for the given\ncollection.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/rand-nth"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.7",
   :name "random-sample",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7742",
   :line 7742,
   :var-type "function",
   :arglists ([prob] [prob coll]),
   :doc
   "Returns items from coll with random probability of prob (0.0 -\n1.0).  Returns a transducer when no collection is provided.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/random-sample"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.11",
   :name "random-uuid",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6862",
   :line 6862,
   :var-type "function",
   :arglists ([]),
   :doc
   "Returns a pseudo-randomly generated java.util.UUID instance (i.e. type 4).\n\nSee: https://docs.oracle.com/javase/8/docs/api/java/util/UUID.html#randomUUID--",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/random-uuid"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "range",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3037",
   :line 3037,
   :var-type "function",
   :arglists ([] [end] [start end] [start end step]),
   :doc
   "Returns a lazy seq of nums from start (inclusive) to end\n(exclusive), by step, where start defaults to 0, step to 1, and end to\ninfinity. When step is equal to 0, returns an infinite sequence of\nstart. When start is equal to end, returns empty list.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/range"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "ratio?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3578",
   :line 3578,
   :var-type "function",
   :arglists ([n]),
   :doc "Returns true if n is a Ratio",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ratio?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "rational?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3614",
   :line 3614,
   :var-type "function",
   :arglists ([n]),
   :doc "Returns true if n is a rational number",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/rational?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "rationalize",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1291",
   :line 1291,
   :var-type "function",
   :arglists ([num]),
   :doc "returns the rational value of num",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/rationalize"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "re-find",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4924",
   :line 4924,
   :var-type "function",
   :arglists ([m] [re s]),
   :doc
   "Returns the next regex match, if any, of string to pattern, using\njava.util.regex.Matcher.find().  Uses re-groups to return the\ngroups.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/re-find"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "re-groups",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4884",
   :line 4884,
   :var-type "function",
   :arglists ([m]),
   :doc
   "Returns the groups from the most recent match/find. If there are no\nnested groups, returns a string of the entire match. If there are\nnested groups, returns a vector of the groups, the first element\nbeing the entire match.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/re-groups"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "re-matcher",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4875",
   :line 4875,
   :var-type "function",
   :arglists ([re s]),
   :doc
   "Returns an instance of java.util.regex.Matcher, for use, e.g. in\nre-find.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/re-matcher"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "re-matches",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4912",
   :line 4912,
   :var-type "function",
   :arglists ([re s]),
   :doc
   "Returns the match, if any, of string to pattern, using\njava.util.regex.Matcher.matches().  Uses re-groups to return the\ngroups.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/re-matches"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "re-pattern",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4865",
   :line 4865,
   :var-type "function",
   :arglists ([s]),
   :doc
   "Returns an instance of java.util.regex.Pattern, for use, e.g. in\nre-matcher.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/re-pattern"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "re-seq",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4900",
   :line 4900,
   :var-type "function",
   :arglists ([re s]),
   :doc
   "Returns a lazy sequence of successive matches of pattern in string,\nusing java.util.regex.Matcher.find(), each such match processed with\nre-groups.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/re-seq"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "read",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3743",
   :line 3743,
   :var-type "function",
   :arglists
   ([]
    [stream]
    [stream eof-error? eof-value]
    [stream eof-error? eof-value recursive?]
    [opts stream]),
   :doc
   "Reads the next object from stream, which must be an instance of\njava.io.PushbackReader or some derivee.  stream defaults to the\ncurrent value of *in*.\n\nOpts is a persistent map with valid keys:\n  :read-cond - :allow to process reader conditionals, or\n               :preserve to keep all branches\n  :features - persistent set of feature keywords for reader conditionals\n  :eof - on eof, return value unless :eofthrow, then throw.\n         if not specified, will throw\n\nNote that read can execute code (controlled by *read-eval*),\nand as such should be used only with trusted sources.\n\nFor data structure interop use clojure.edn/read",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/read"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.10",
   :name "read+string",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3772",
   :line 3772,
   :var-type "function",
   :arglists
   ([]
    [stream]
    [stream eof-error? eof-value]
    [stream eof-error? eof-value recursive?]
    [opts stream]),
   :doc
   "Like read, and taking the same args. stream must be a LineNumberingPushbackReader.\nReturns a vector containing the object read and the (whitespace-trimmed) string read.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/read+string"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "read-line",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3798",
   :line 3798,
   :var-type "function",
   :arglists ([]),
   :doc
   "Reads the next line from stream that is the current value of *in* .",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/read-line"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "read-string",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3807",
   :line 3807,
   :var-type "function",
   :arglists ([s] [opts s]),
   :doc
   "Reads one object from the string s. Optionally include reader\noptions, as specified in read.\n\nNote that read-string can execute code (controlled by *read-eval*),\nand as such should be used only with trusted sources.\n\nFor data structure interop use clojure.edn/read-string",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/read-string"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.7",
   :name "reader-conditional",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7853",
   :line 7853,
   :var-type "function",
   :arglists ([form splicing?]),
   :doc
   "Construct a data representation of a reader conditional.\nIf true, splicing? indicates read-cond-splicing.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/reader-conditional"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.7",
   :name "reader-conditional?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7847",
   :line 7847,
   :var-type "function",
   :arglists ([value]),
   :doc
   "Return true if the value is the data representation of a reader conditional",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/reader-conditional?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.3",
   :name "realized?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7602",
   :line 7602,
   :var-type "function",
   :arglists ([x]),
   :doc
   "Returns true if a value has been produced for a promise, delay, future or lazy sequence.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/realized?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/8957a93099fc506c3b24ed5739bf9e2fc1811bef/src/clj/clojure/core_deftype.clj",
   :added "1.6",
   :name "record?",
   :file "src/clj/clojure/core_deftype.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/8957a93099fc506c3b24ed5739bf9e2fc1811bef/src/clj/clojure/core_deftype.clj#L406",
   :line 406,
   :var-type "function",
   :arglists ([x]),
   :doc "Returns true if x is a record",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/record?"}
  {:raw-source-url nil,
   :added "1.0",
   :name "recur",
   :file nil,
   :source-url nil,
   :var-type "special form",
   :arglists nil,
   :doc
   "Evaluates the exprs in order, then, in parallel, rebinds\nthe bindings of the recursion point to the values of the exprs.\nExecution then jumps back to the recursion point, a loop or fn method.\n\nPlease see https://clojure.org/reference/special_forms#recur",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/recur",
   :forms [(recur exprs*)]}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "reduce",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6869",
   :line 6869,
   :var-type "function",
   :arglists ([f coll] [f val coll]),
   :doc
   "f should be a function of 2 arguments. If val is not supplied,\nreturns the result of applying f to the first 2 items in coll, then\napplying f to that result and the 3rd item, etc. If coll contains no\nitems, f must accept no arguments as well, and reduce returns the\nresult of calling f with no arguments.  If coll has only 1 item, it\nis returned and f is not called.  If val is supplied, returns the\nresult of applying f to val and the first item in coll, then\napplying f to that result and the 2nd item, etc. If coll contains no\nitems, returns val and f is not called.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/reduce"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.4",
   :name "reduce-kv",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6911",
   :line 6911,
   :var-type "function",
   :arglists ([f init coll]),
   :doc
   "Reduces an associative collection. f should be a function of 3\narguments. Returns the result of applying f to init, the first key\nand the first value in coll, then applying f to that result and the\n2nd key and value, etc. If coll contains no entries, returns init\nand f is not called. Note that reduce-kv is supported on vectors,\nwhere the keys will be the ordinals.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/reduce-kv"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.5",
   :name "reduced",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2853",
   :line 2853,
   :var-type "function",
   :arglists ([x]),
   :doc
   "Wraps x in a way such that a reduce will terminate with the value x",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/reduced"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.5",
   :name "reduced?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2859",
   :line 2859,
   :var-type "function",
   :arglists ([x]),
   :doc "Returns true if x is the result of a call to reduced",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/reduced?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.2",
   :name "reductions",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7283",
   :line 7283,
   :var-type "function",
   :arglists ([f coll] [f init coll]),
   :doc
   "Returns a lazy seq of the intermediate values of the reduction (as\nper reduce) of coll by f, starting with init.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/reductions"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "ref",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2279",
   :line 2279,
   :var-type "function",
   :arglists ([x] [x & options]),
   :doc
   "Creates and returns a Ref with an initial value of x and zero or\nmore options (in any order):\n\n:meta metadata-map\n\n:validator validate-fn\n\n:min-history (default 0)\n:max-history (default 10)\n\nIf metadata-map is supplied, it will become the metadata on the\nref. validate-fn must be nil or a side-effect-free fn of one\nargument, which will be passed the intended new state on any state\nchange. If the new state is unacceptable, the validate-fn should\nreturn false or throw an exception. validate-fn will be called on\ntransaction commit, when all refs have their final values.\n\nNormally refs accumulate history dynamically as needed to deal with\nread demands. If you know in advance you will need history you can\nset :min-history to ensure it will be available when first needed (instead\nof after a read fault). History is limited, and the limit can be set\nwith :max-history.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ref"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.1",
   :name "ref-history-count",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2480",
   :line 2480,
   :var-type "function",
   :arglists ([ref]),
   :doc "Returns the history count of a ref",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ref-history-count"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.1",
   :name "ref-max-history",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2496",
   :line 2496,
   :var-type "function",
   :arglists ([ref] [ref n]),
   :doc
   "Gets the max-history of a ref, or sets it and returns the ref",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ref-max-history"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.1",
   :name "ref-min-history",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2487",
   :line 2487,
   :var-type "function",
   :arglists ([ref] [ref n]),
   :doc
   "Gets the min-history of a ref, or sets it and returns the ref",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ref-min-history"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "ref-set",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2472",
   :line 2472,
   :var-type "function",
   :arglists ([ref val]),
   :doc
   "Must be called in a transaction. Sets the value of ref.\nReturns val.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/ref-set"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "refer",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4219",
   :line 4219,
   :var-type "function",
   :arglists ([ns-sym & filters]),
   :doc
   "refers to all public vars of ns, subject to filters.\nfilters can include at most one each of:\n\n:exclude list-of-symbols\n:only list-of-symbols\n:rename map-of-fromsymbol-tosymbol\n\nFor each public interned var in the namespace named by the symbol,\nadds a mapping from the name of the var to the var to the current\nnamespace.  Throws an exception if name is already mapped to\nsomething else in the current namespace. Filters can be used to\nselect a subset, via inclusion or exclusion, or to provide a mapping\nto a symbol different from the var's name, in order to prevent\nclashes. Use :use in the ns macro in preference to calling this directly.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/refer"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "refer-clojure",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5848",
   :line 5848,
   :var-type "macro",
   :arglists ([& filters]),
   :doc "Same as (refer 'clojure.core <filters>)",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/refer-clojure"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/8957a93099fc506c3b24ed5739bf9e2fc1811bef/src/clj/clojure/core_deftype.clj",
   :added "1.2",
   :name "reify",
   :file "src/clj/clojure/core_deftype.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/8957a93099fc506c3b24ed5739bf9e2fc1811bef/src/clj/clojure/core_deftype.clj#L70",
   :line 70,
   :var-type "macro",
   :arglists ([& opts+specs]),
   :doc
   "reify creates an object implementing a protocol or interface.\n reify is a macro with the following structure:\n\n(reify options* specs*)\n \n Currently there are no options.\n\n Each spec consists of the protocol or interface name followed by zero\n or more method bodies:\n\n protocol-or-interface-or-Object\n (methodName [args+] body)*\n\n Methods should be supplied for all methods of the desired\n protocol(s) and interface(s). You can also define overrides for\n methods of Object. Note that the first parameter must be supplied to\n correspond to the target object ('this' in Java parlance). Thus\n methods for interfaces will take one more argument than do the\n interface declarations.  Note also that recur calls to the method\n head should *not* pass the target object, it will be supplied\n automatically and can not be substituted.\n\n The return type can be indicated by a type hint on the method name,\n and arg types can be indicated by a type hint on arg names. If you\n leave out all hints, reify will try to match on same name/arity\n method in the protocol(s)/interface(s) - this is preferred. If you\n supply any hints at all, no inference is done, so all hints (or\n default of Object) must be correct, for both arguments and return\n type. If a method is overloaded in a protocol/interface, multiple\n independent method definitions must be supplied.  If overloaded with\n same arity in an interface you must specify complete hints to\n disambiguate - a missing hint implies Object.\n\n recur works to method heads The method bodies of reify are lexical\n closures, and can refer to the surrounding local scope:\n \n (str (let [f \"foo\"] \n      (reify Object \n        (toString [this] f))))\n == \"foo\"\n\n (seq (let [f \"foo\"] \n      (reify clojure.lang.Seqable \n        (seq [this] (seq f)))))\n == (\\f \\o \\o))\n \n reify always implements clojure.lang.IObj and transfers meta\n data of the form to the created object.\n \n (meta ^{:k :v} (reify Object (toString [this] \"foo\")))\n == {:k :v}",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/reify"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "release-pending-sends",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2150",
   :line 2150,
   :var-type "function",
   :arglists ([]),
   :doc
   "Normally, actions sent directly or indirectly during another action\nare held until the action completes (changes the agent's\nstate). This function can be used to dispatch any pending sent\nactions immediately. This has no impact on actions sent during a\ntransaction, which are still held until commit. If no action is\noccurring, does nothing. Returns the number of actions dispatched.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/release-pending-sends"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "rem",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1283",
   :line 1283,
   :var-type "function",
   :arglists ([num div]),
   :doc "remainder of dividing numerator by denominator.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/rem"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "remove",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2843",
   :line 2843,
   :var-type "function",
   :arglists ([pred] [pred coll]),
   :doc
   "Returns a lazy sequence of the items in coll for which\n(pred item) returns logical false. pred must be free of side-effects.\nReturns a transducer when no collection is provided.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/remove"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.2",
   :name "remove-all-methods",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1806",
   :line 1806,
   :var-type "function",
   :arglists ([multifn]),
   :doc "Removes all of the methods of multimethod.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/remove-all-methods"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "remove-method",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1813",
   :line 1813,
   :var-type "function",
   :arglists ([multifn dispatch-val]),
   :doc
   "Removes the method of multimethod associated with dispatch-value.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/remove-method"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "remove-ns",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4142",
   :line 4142,
   :var-type "function",
   :arglists ([sym]),
   :doc
   "Removes the namespace named by the symbol. Use with caution.\nCannot be used to remove the clojure namespace.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/remove-ns"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.10",
   :name "remove-tap",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7994",
   :line 7994,
   :var-type "function",
   :arglists ([f]),
   :doc "Remove f from the tap set.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/remove-tap"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "remove-watch",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2179",
   :line 2179,
   :var-type "function",
   :arglists ([reference key]),
   :doc "Removes a watch (set by add-watch) from a reference",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/remove-watch"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "repeat",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3016",
   :line 3016,
   :var-type "function",
   :arglists ([x] [n x]),
   :doc
   "Returns a lazy (infinite!, or length n if supplied) sequence of xs.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/repeat"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "repeatedly",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5169",
   :line 5169,
   :var-type "function",
   :arglists ([f] [n f]),
   :doc
   "Takes a function of no args, presumably with side effects, and\nreturns an infinite (or length n if supplied) lazy sequence of calls\nto it",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/repeatedly"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "replace",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5084",
   :line 5084,
   :var-type "function",
   :arglists ([smap] [smap coll]),
   :doc
   "Given a map of replacement pairs and a vector/collection, returns a\nvector/seq with any elements = a key in smap replaced with the\ncorresponding val in smap.  Returns a transducer when no collection\nis provided.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/replace"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "replicate",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3023",
   :line 3023,
   :deprecated "1.3",
   :var-type "function",
   :arglists ([n x]),
   :doc
   "DEPRECATED: Use 'repeat' instead.\nReturns a lazy seq of n xs.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/replicate"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "require",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6039",
   :line 6039,
   :var-type "function",
   :arglists ([& args]),
   :doc
   "Loads libs, skipping any that are already loaded. Each argument is\neither a libspec that identifies a lib, a prefix list that identifies\nmultiple libs whose names share a common prefix, or a flag that modifies\nhow all the identified libs are loaded. Use :require in the ns macro\nin preference to calling this directly.\n\nLibs\n\nA 'lib' is a named set of resources in classpath whose contents define a\nlibrary of Clojure code. Lib names are symbols and each lib is associated\nwith a Clojure namespace and a Java package that share its name. A lib's\nname also locates its root directory within classpath using Java's\npackage name to classpath-relative path mapping. All resources in a lib\nshould be contained in the directory structure under its root directory.\nAll definitions a lib makes should be in its associated namespace.\n\n'require loads a lib by loading its root resource. The root resource path\nis derived from the lib name in the following manner:\nConsider a lib named by the symbol 'x.y.z; it has the root directory\n<classpath>/x/y/, and its root resource is <classpath>/x/y/z.clj, or\n<classpath>/x/y/z.cljc if <classpath>/x/y/z.clj does not exist. The\nroot resource should contain code to create the lib's\nnamespace (usually by using the ns macro) and load any additional\nlib resources.\n\nLibspecs\n\nA libspec is a lib name or a vector containing a lib name followed by\noptions expressed as sequential keywords and arguments.\n\nRecognized options:\n:as takes a symbol as its argument and makes that symbol an alias to the\n  lib's namespace in the current namespace.\n:as-alias takes a symbol as its argument and aliases like :as, however\n  the lib will not be loaded. If the lib has not been loaded, a new\n  empty namespace will be created (as with create-ns).\n:refer takes a list of symbols to refer from the namespace or the :all\n  keyword to bring in all public vars.\n\nPrefix Lists\n\nIt's common for Clojure code to depend on several libs whose names have\nthe same prefix. When specifying libs, prefix lists can be used to reduce\nrepetition. A prefix list contains the shared prefix followed by libspecs\nwith the shared prefix removed from the lib names. After removing the\nprefix, the names that remain must not contain any periods.\n\nFlags\n\nA flag is a keyword.\nRecognized flags: :reload, :reload-all, :verbose\n:reload forces loading of all the identified libs even if they are\n  already loaded (has no effect on libspecs using :as-alias)\n:reload-all implies :reload and also forces loading of all libs that the\n  identified libs directly or indirectly load via require or use\n  (has no effect on libspecs using :as-alias)\n:verbose triggers printing information about each load, alias, and refer\n\nExample:\n\nThe following would load the libraries clojure.zip and clojure.set\nabbreviated as 's'.\n\n(require '(clojure zip [set :as s]))",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/require"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.10",
   :name "requiring-resolve",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6118",
   :line 6118,
   :var-type "function",
   :arglists ([sym]),
   :doc
   "Resolves namespace-qualified sym per 'resolve'. If initial resolve\nfails, attempts to require sym's namespace and retries.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/requiring-resolve"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "reset!",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2393",
   :line 2393,
   :var-type "function",
   :arglists ([atom newval]),
   :doc
   "Sets the value of atom to newval without regard for the\ncurrent value. Returns newval.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/reset!"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "reset-meta!",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2433",
   :line 2433,
   :var-type "function",
   :arglists ([iref metadata-map]),
   :doc
   "Atomically resets the metadata for a namespace/var/ref/agent/atom",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/reset-meta!"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.9",
   :name "reset-vals!",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2400",
   :line 2400,
   :var-type "function",
   :arglists ([atom newval]),
   :doc
   "Sets the value of atom to newval. Returns [old new], the value of the\natom before and after the reset.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/reset-vals!"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "resolve",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4374",
   :line 4374,
   :var-type "function",
   :arglists ([sym] [env sym]),
   :doc
   "same as (ns-resolve *ns* symbol) or (ns-resolve *ns* &env symbol)",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/resolve"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "rest",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L66",
   :line 66,
   :var-type "function",
   :arglists ([coll]),
   :doc
   "Returns a possibly empty seq of the items after the first. Calls seq on its\nargument.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/rest"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.2",
   :name "restart-agent",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2194",
   :line 2194,
   :var-type "function",
   :arglists ([a new-state & options]),
   :doc
   "When an agent is failed, changes the agent state to new-state and\nthen un-fails the agent so that sends are allowed again.  If\na :clear-actions true option is given, any actions queued on the\nagent that were being held while it was failed will be discarded,\notherwise those held actions will proceed.  The new-state must pass\nthe validator if any, or restart will throw an exception and the\nagent will remain failed with its old state and error.  Watchers, if\nany, will NOT be notified of the new state.  Throws an exception if\nthe agent is not failed.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/restart-agent"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "resultset-seq",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5728",
   :line 5728,
   :var-type "function",
   :arglists ([rs]),
   :doc
   "Creates and returns a lazy sequence of structmaps corresponding to\nthe rows in the java.sql.ResultSet rs",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/resultset-seq"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "reverse",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L949",
   :line 949,
   :var-type "function",
   :arglists ([coll]),
   :doc
   "Returns a seq of the items in coll in reverse order. Not lazy.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/reverse"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "reversible?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6304",
   :line 6304,
   :var-type "function",
   :arglists ([coll]),
   :doc "Returns true if coll implements Reversible",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/reversible?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "rseq",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1596",
   :line 1596,
   :var-type "function",
   :arglists ([rev]),
   :doc
   "Returns, in constant time, a seq of the items in rev (which\ncan be a vector or sorted-map), in reverse order. If rev is empty returns nil",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/rseq"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "rsubseq",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5152",
   :line 5152,
   :var-type "function",
   :arglists
   ([sc test key] [sc start-test start-key end-test end-key]),
   :doc
   "sc must be a sorted collection, test(s) one of <, <=, > or\n>=. Returns a reverse seq of those entries with keys ek for\nwhich (test (.. sc comparator (compare ek key)) 0) is true",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/rsubseq"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.7",
   :name "run!",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7779",
   :line 7779,
   :var-type "function",
   :arglists ([proc coll]),
   :doc
   "Runs the supplied procedure (via reduce), for purposes of side\neffects, on successive items in the collection. Returns nil",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/run!"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/8957a93099fc506c3b24ed5739bf9e2fc1811bef/src/clj/clojure/core_deftype.clj",
   :added "1.2",
   :name "satisfies?",
   :file "src/clj/clojure/core_deftype.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/8957a93099fc506c3b24ed5739bf9e2fc1811bef/src/clj/clojure/core_deftype.clj#L570",
   :line 570,
   :var-type "function",
   :arglists ([protocol x]),
   :doc "Returns true if x satisfies the protocol",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/satisfies?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "second",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L93",
   :line 93,
   :var-type "function",
   :arglists ([x]),
   :doc "Same as (first (next x))",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/second"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "select-keys",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1555",
   :line 1555,
   :var-type "function",
   :arglists ([map keyseq]),
   :doc
   "Returns a map containing only those entries in map whose key is in keys",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/select-keys"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "send",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2128",
   :line 2128,
   :var-type "function",
   :arglists ([a f & args]),
   :doc
   "Dispatch an action to an agent. Returns the agent immediately.\nSubsequently, in a thread from a thread pool, the state of the agent\nwill be set to the value of:\n\n(apply action-fn state-of-agent args)",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/send"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "send-off",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2139",
   :line 2139,
   :var-type "function",
   :arglists ([a f & args]),
   :doc
   "Dispatch a potentially blocking action to an agent. Returns the\nagent immediately. Subsequently, in a separate thread, the state of\nthe agent will be set to the value of:\n\n(apply action-fn state-of-agent args)",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/send-off"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.5",
   :name "send-via",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2118",
   :line 2118,
   :var-type "function",
   :arglists ([executor a f & args]),
   :doc
   "Dispatch an action to an agent. Returns the agent immediately.\nSubsequently, in a thread supplied by executor, the state of the agent\nwill be set to the value of:\n\n(apply action-fn state-of-agent args)",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/send-via"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "seq",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L128",
   :line 128,
   :var-type "function",
   :arglists ([coll]),
   :doc
   "Returns a seq on the collection. If the collection is\nempty, returns nil.  (seq nil) returns nil. seq also works on\nStrings, native Java arrays (of reference types) and any objects\nthat implement Iterable. Note that seqs cache values, thus seq\nshould not be used on any Iterable whose iterator repeatedly\nreturns the same mutable object.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/seq"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.11",
   :name "seq-to-map-for-destructuring",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4393",
   :line 4393,
   :var-type "function",
   :arglists ([s]),
   :doc
   "Builds a map from a seq as described in\nhttps://clojure.org/reference/special_forms#keyword-arguments",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/seq-to-map-for-destructuring"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "seq?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L148",
   :line 148,
   :var-type "function",
   :arglists ([x]),
   :doc "Return true if x implements ISeq",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/seq?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.9",
   :name "seqable?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6261",
   :line 6261,
   :var-type "function",
   :arglists ([x]),
   :doc "Return true if the seq function is supported for x",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/seqable?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "seque",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5423",
   :line 5423,
   :var-type "function",
   :arglists ([s] [n-or-q s]),
   :doc
   "Creates a queued seq on another (presumably lazy) seq s. The queued\nseq will produce a concrete seq in the background, and can get up to\nn items ahead of the consumer. n-or-q can be an integer n buffer\nsize, or an instance of java.util.concurrent BlockingQueue. Note\nthat reading from a seque can block if the reader gets ahead of the\nproducer.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/seque"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "sequence",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2664",
   :line 2664,
   :var-type "function",
   :arglists ([coll] [xform coll] [xform coll & colls]),
   :doc
   "Coerces coll to a (possibly empty) sequence, if it is not already\none. Will not force a lazy seq. (sequence nil) yields (), When a\ntransducer is supplied, returns a lazy sequence of applications of\nthe transform to the items in coll(s), i.e. to the set of first\nitems of each coll, followed by the set of second\nitems in each coll, until any one of the colls is exhausted.  Any\nremaining items in other colls are ignored. The transform should accept\nnumber-of-colls arguments",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/sequence"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "sequential?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6286",
   :line 6286,
   :var-type "function",
   :arglists ([coll]),
   :doc "Returns true if coll implements Sequential",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/sequential?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "set",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4107",
   :line 4107,
   :var-type "function",
   :arglists ([coll]),
   :doc "Returns a set of the distinct elements of coll.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/set"}
  {:raw-source-url nil,
   :added "1.0",
   :name "set!",
   :file nil,
   :source-url nil,
   :var-type "special form",
   :arglists nil,
   :doc
   "Used to set thread-local-bound vars, Java object instance\nfields, and Java class static fields.\n\nPlease see https://clojure.org/vars#set",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/set!",
   :forms
   [(set! var-symbol expr)
    (set! (. instance-expr instanceFieldName-symbol) expr)
    (set! (. Classname-symbol staticFieldName-symbol) expr)]}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.5",
   :name "set-agent-send-executor!",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2106",
   :line 2106,
   :var-type "function",
   :arglists ([executor]),
   :doc "Sets the ExecutorService to be used by send",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/set-agent-send-executor!"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.5",
   :name "set-agent-send-off-executor!",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2112",
   :line 2112,
   :var-type "function",
   :arglists ([executor]),
   :doc "Sets the ExecutorService to be used by send-off",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/set-agent-send-off-executor!"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.2",
   :name "set-error-handler!",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2211",
   :line 2211,
   :var-type "function",
   :arglists ([a handler-fn]),
   :doc
   "Sets the error-handler of agent a to handler-fn.  If an action\nbeing run by the agent throws an exception or doesn't pass the\nvalidator fn, handler-fn will be called with two arguments: the\nagent and the exception.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/set-error-handler!"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.2",
   :name "set-error-mode!",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2229",
   :line 2229,
   :var-type "function",
   :arglists ([a mode-keyword]),
   :doc
   "Sets the error-mode of agent a to mode-keyword, which must be\neither :fail or :continue.  If an action being run by the agent\nthrows an exception or doesn't pass the validator fn, an\nerror-handler may be called (see set-error-handler!), after which,\nif the mode is :continue, the agent will continue as if neither the\naction that caused the error nor the error itself ever happened.\n\nIf the mode is :fail, the agent will become failed and will stop\naccepting new 'send' and 'send-off' actions, and any previously\nqueued actions will be held until a 'restart-agent'.  Deref will\nstill work, returning the state of the agent before the error.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/set-error-mode!"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "set-validator!",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2406",
   :line 2406,
   :var-type "function",
   :arglists ([iref validator-fn]),
   :doc
   "Sets the validator-fn for a var/ref/agent/atom. validator-fn must be nil or a\nside-effect-free fn of one argument, which will be passed the intended\nnew state on any state change. If the new state is unacceptable, the\nvalidator-fn should return false or throw an exception. If the current state (root\nvalue if var) is not acceptable to the new validator, an exception\nwill be thrown and the validator will not be changed.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/set-validator!"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "set?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4101",
   :line 4101,
   :var-type "function",
   :arglists ([x]),
   :doc "Returns true if x implements IPersistentSet",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/set?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "short",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3500",
   :line 3500,
   :var-type "function",
   :arglists ([x]),
   :doc "Coerce to short",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/short"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.1",
   :name "short-array",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5335",
   :line 5335,
   :var-type "function",
   :arglists ([size-or-seq] [size init-val-or-seq]),
   :doc "Creates an array of shorts",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/short-array"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.1",
   :name "shorts",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5389",
   :line 5389,
   :var-type "function",
   :arglists ([xs]),
   :doc "Casts to shorts[]",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/shorts"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.2",
   :name "shuffle",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7343",
   :line 7343,
   :var-type "function",
   :arglists ([coll]),
   :doc "Return a random permutation of coll",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/shuffle"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "shutdown-agents",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2271",
   :line 2271,
   :var-type "function",
   :arglists ([]),
   :doc
   "Initiates a shutdown of the thread pools that back the agent\nsystem. Running actions will complete, but no new actions will be\naccepted",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/shutdown-agents"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.9",
   :name "simple-ident?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1632",
   :line 1632,
   :var-type "function",
   :arglists ([x]),
   :doc "Return true if x is a symbol or keyword without a namespace",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/simple-ident?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.9",
   :name "simple-keyword?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1652",
   :line 1652,
   :var-type "function",
   :arglists ([x]),
   :doc "Return true if x is a keyword without a namespace",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/simple-keyword?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.9",
   :name "simple-symbol?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1642",
   :line 1642,
   :var-type "function",
   :arglists ([x]),
   :doc "Return true if x is a symbol without a namespace",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/simple-symbol?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "slurp",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7010",
   :line 7010,
   :var-type "function",
   :arglists ([f & opts]),
   :doc
   "Opens a reader on f and reads all its contents, returning a string.\nSee clojure.java.io/reader for a complete list of supported arguments.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/slurp"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "some",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2709",
   :line 2709,
   :var-type "function",
   :arglists ([pred coll]),
   :doc
   "Returns the first logical true value of (pred x) for any x in coll,\nelse nil.  One common idiom is to use a set as pred, for example\nthis will return :fred if :fred is in the sequence, otherwise nil:\n(some #{:fred} coll)",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/some"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.5",
   :name "some->",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7653",
   :line 7653,
   :var-type "macro",
   :arglists ([expr & forms]),
   :doc
   "When expr is not nil, threads it into the first form (via ->),\nand when that result is not nil, through the next etc",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/some->"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.5",
   :name "some->>",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7667",
   :line 7667,
   :var-type "macro",
   :arglists ([expr & forms]),
   :doc
   "When expr is not nil, threads it into the first form (via ->>),\nand when that result is not nil, through the next etc",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/some->>"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.3",
   :name "some-fn",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7505",
   :line 7505,
   :var-type "function",
   :arglists ([p] [p1 p2] [p1 p2 p3] [p1 p2 p3 & ps]),
   :doc
   "Takes a set of predicates and returns a function f that returns the first logical true value\nreturned by one of its composing predicates against any of its arguments, else it returns\nlogical false. Note that f is short-circuiting in that it will stop execution on the first\nargument that triggers a logical true result against the original predicates.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/some-fn"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.6",
   :name "some?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L533",
   :line 533,
   :var-type "function",
   :arglists ([x]),
   :doc "Returns true if x is not nil, false otherwise.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/some?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "sort",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3104",
   :line 3104,
   :var-type "function",
   :arglists ([coll] [comp coll]),
   :doc
   "Returns a sorted sequence of the items in coll. If no comparator is\nsupplied, uses compare.  comparator must implement\njava.util.Comparator.  Guaranteed to be stable: equal elements will\nnot be reordered.  If coll is a Java array, it will be modified.  To\navoid this, sort a copy of the array.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/sort"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "sort-by",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3121",
   :line 3121,
   :var-type "function",
   :arglists ([keyfn coll] [keyfn comp coll]),
   :doc
   "Returns a sorted sequence of the items in coll, where the sort\norder is determined by comparing (keyfn item).  If no comparator is\nsupplied, uses compare.  comparator must implement\njava.util.Comparator.  Guaranteed to be stable: equal elements will\nnot be reordered.  If coll is a Java array, it will be modified.  To\navoid this, sort a copy of the array.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/sort-by"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "sorted-map",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L400",
   :line 400,
   :var-type "function",
   :arglists ([& keyvals]),
   :doc
   "keyval => key val\nReturns a new sorted map with supplied mappings.  If any keys are\nequal, they are handled as if by repeated uses of assoc.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/sorted-map"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "sorted-map-by",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L409",
   :line 409,
   :var-type "function",
   :arglists ([comparator & keyvals]),
   :doc
   "keyval => key val\nReturns a new sorted map with supplied mappings, using the supplied\ncomparator.  If any keys are equal, they are handled as if by\nrepeated uses of assoc.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/sorted-map-by"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "sorted-set",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L419",
   :line 419,
   :var-type "function",
   :arglists ([& keys]),
   :doc
   "Returns a new sorted set with supplied keys.  Any equal keys are\nhandled as if by repeated uses of conj.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/sorted-set"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.1",
   :name "sorted-set-by",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L427",
   :line 427,
   :var-type "function",
   :arglists ([comparator & keys]),
   :doc
   "Returns a new sorted set with supplied keys, using the supplied\ncomparator.  Any equal keys are handled as if by repeated uses of\nconj.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/sorted-set-by"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "sorted?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6292",
   :line 6292,
   :var-type "function",
   :arglists ([coll]),
   :doc "Returns true if coll implements Sorted",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/sorted?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "special-symbol?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4994",
   :line 4994,
   :var-type "function",
   :arglists ([s]),
   :doc "Returns true if s names a special form",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/special-symbol?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.2",
   :name "spit",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7022",
   :line 7022,
   :var-type "function",
   :arglists ([f content & options]),
   :doc
   "Opposite of slurp.  Opens f with writer, writes content, then\ncloses f. Options passed to clojure.java.io/writer.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/spit"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "split-at",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3002",
   :line 3002,
   :var-type "function",
   :arglists ([n coll]),
   :doc "Returns a vector of [(take n coll) (drop n coll)]",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/split-at"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "split-with",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3009",
   :line 3009,
   :var-type "function",
   :arglists ([pred coll]),
   :doc
   "Returns a vector of [(take-while pred coll) (drop-while pred coll)]",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/split-with"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "str",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L546",
   :line 546,
   :var-type "function",
   :arglists ([] [x] [x & ys]),
   :doc
   "With no args, returns the empty string. With one arg x, returns\nx.toString().  (str nil) returns the empty string. With more than\none arg, returns the concatenation of the str values of the args.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/str"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "string?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L162",
   :line 162,
   :var-type "function",
   :arglists ([x]),
   :doc "Return true if x is a String",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/string?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "struct",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4064",
   :line 4064,
   :var-type "function",
   :arglists ([s & vals]),
   :doc
   "Returns a new structmap instance with the keys of the\nstructure-basis. vals must be supplied for basis keys in order -\nwhere values are not supplied they will default to nil.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/struct"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "struct-map",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4054",
   :line 4054,
   :var-type "function",
   :arglists ([s & inits]),
   :doc
   "Returns a new structmap instance with the keys of the\nstructure-basis. keyvals may contain all, some or none of the basis\nkeys - where values are not supplied they will default to nil.\nkeyvals can also contain keys not in the basis.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/struct-map"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "subs",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5007",
   :line 5007,
   :var-type "function",
   :arglists ([s start] [s start end]),
   :doc
   "Returns the substring of s beginning at start inclusive, and ending\nat end (defaults to length of string), exclusive.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/subs"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "subseq",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5135",
   :line 5135,
   :var-type "function",
   :arglists
   ([sc test key] [sc start-test start-key end-test end-key]),
   :doc
   "sc must be a sorted collection, test(s) one of <, <=, > or\n>=. Returns a seq of those entries with keys ek for\nwhich (test (.. sc comparator (compare ek key)) 0) is true",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/subseq"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "subvec",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3820",
   :line 3820,
   :var-type "function",
   :arglists ([v start] [v start end]),
   :doc
   "Returns a persistent vector of the items in vector from\nstart (inclusive) to end (exclusive).  If end is not supplied,\ndefaults to (count vector). This operation is O(1) and very fast, as\nthe resulting vector shares structure with the original and no\ntrimming is done.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/subvec"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "supers",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5579",
   :line 5579,
   :var-type "function",
   :arglists ([class]),
   :doc
   "Returns the immediate and indirect superclasses and interfaces of c, if any",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/supers"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "swap!",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2362",
   :line 2362,
   :var-type "function",
   :arglists ([atom f] [atom f x] [atom f x y] [atom f x y & args]),
   :doc
   "Atomically swaps the value of atom to be:\n(apply f current-value-of-atom args). Note that f may be called\nmultiple times, and thus should be free of side effects.  Returns\nthe value that was swapped in.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/swap!"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.9",
   :name "swap-vals!",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2374",
   :line 2374,
   :var-type "function",
   :arglists ([atom f] [atom f x] [atom f x y] [atom f x y & args]),
   :doc
   "Atomically swaps the value of atom to be:\n(apply f current-value-of-atom args). Note that f may be called\nmultiple times, and thus should be free of side effects.\nReturns [old new], the value of the atom before and after the swap.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/swap-vals!"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "symbol",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L591",
   :line 591,
   :var-type "function",
   :arglists ([name] [ns name]),
   :doc
   "Returns a Symbol with the given namespace and name. Arity-1 works\non strings, keywords, and vars.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/symbol"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "symbol?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L564",
   :line 564,
   :var-type "function",
   :arglists ([x]),
   :doc "Return true if x is a Symbol",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/symbol?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "sync",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2515",
   :line 2515,
   :var-type "macro",
   :arglists ([flags-ignored-for-now & body]),
   :doc
   "transaction-flags => TBD, pass nil for now\n\nRuns the exprs (in an implicit do) in a transaction that encompasses\nexprs and any nested calls.  Starts a transaction if none is already\nrunning on this thread. Any uncaught exception will abort the\ntransaction and flow out of sync. The exprs may be run more than\nonce, but any effects on Refs will be atomic.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/sync"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.7",
   :name "tagged-literal",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7840",
   :line 7840,
   :var-type "function",
   :arglists ([tag form]),
   :doc
   "Construct a data representation of a tagged literal from a\ntag symbol and a form.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/tagged-literal"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.7",
   :name "tagged-literal?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7834",
   :line 7834,
   :var-type "function",
   :arglists ([value]),
   :doc
   "Return true if the value is the data representation of a tagged literal",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/tagged-literal?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "take",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2878",
   :line 2878,
   :var-type "function",
   :arglists ([n] [n coll]),
   :doc
   "Returns a lazy sequence of the first n items in coll, or all items if\nthere are fewer than n.  Returns a stateful transducer when\nno collection is provided.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/take"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.1",
   :name "take-last",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2958",
   :line 2958,
   :var-type "function",
   :arglists ([n coll]),
   :doc
   "Returns a seq of the last n items in coll.  Depending on the type\nof coll may be no better than linear time.  For vectors, see also subvec.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/take-last"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "take-nth",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4290",
   :line 4290,
   :var-type "function",
   :arglists ([n] [n coll]),
   :doc
   "Returns a lazy seq of every nth item in coll.  Returns a stateful\ntransducer when no collection is provided.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/take-nth"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "take-while",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2905",
   :line 2905,
   :var-type "function",
   :arglists ([pred] [pred coll]),
   :doc
   "Returns a lazy sequence of successive items from coll while\n(pred item) returns logical true. pred must be free of side-effects.\nReturns a transducer when no collection is provided.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/take-while"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.10",
   :name "tap>",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L8001",
   :line 8001,
   :var-type "function",
   :arglists ([x]),
   :doc
   "sends x to any taps. Will not block. Returns true if there was room in the queue,\nfalse if not (dropped).",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/tap>"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "test",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4855",
   :line 4855,
   :var-type "function",
   :arglists ([v]),
   :doc
   "test [v] finds fn at key :test in var metadata and calls it,\npresuming failure will throw exception",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/test"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "the-ns",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4155",
   :line 4155,
   :var-type "function",
   :arglists ([x]),
   :doc
   "If passed a namespace, returns it. Else, when passed a symbol,\nreturns the namespace named by it, throwing an exception if not\nfound.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/the-ns"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.2",
   :name "thread-bound?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5546",
   :line 5546,
   :var-type "function",
   :arglists ([& vars]),
   :doc
   "Returns true if all of the vars provided as arguments have thread-local bindings.\nImplies that set!'ing the provided vars will succeed.  Returns true if no vars are provided.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/thread-bound?"}
  {:raw-source-url nil,
   :added "1.0",
   :name "throw",
   :file nil,
   :source-url nil,
   :var-type "special form",
   :arglists nil,
   :doc
   "The expr is evaluated and thrown, therefore it should\nyield an instance of some derivee of Throwable.\n\nPlease see https://clojure.org/reference/special_forms#throw",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/throw",
   :forms [(throw expr)]}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "time",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3886",
   :line 3886,
   :var-type "macro",
   :arglists ([expr]),
   :doc
   "Evaluates expr and prints the time it took.  Returns the value of\nexpr.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/time"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "to-array",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L340",
   :line 340,
   :var-type "function",
   :arglists ([coll]),
   :doc
   "Returns an array of Objects containing the contents of coll, which\ncan be any Collection.  Maps to java.util.Collection.toArray().",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/to-array"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "to-array-2d",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4005",
   :line 4005,
   :var-type "function",
   :arglists ([coll]),
   :doc
   "Returns a (potentially-ragged) 2-dimensional array of Objects\ncontaining the contents of coll, which can be any Collection of any\nCollection.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/to-array-2d"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "trampoline",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6335",
   :line 6335,
   :var-type "function",
   :arglists ([f] [f & args]),
   :doc
   "trampoline can be used to convert algorithms requiring mutual\nrecursion without stack consumption. Calls f with supplied args, if\nany. If f returns a fn, calls that fn with no arguments, and\ncontinues to repeat, until the return value is not a fn, then\nreturns that non-fn value. Note that if you want to return a fn as a\nfinal value, you must wrap it in some data structure and unpack it\nafter trampoline returns.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/trampoline"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.7",
   :name "transduce",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6934",
   :line 6934,
   :var-type "function",
   :arglists ([xform f coll] [xform f init coll]),
   :doc
   "reduce with a transformation of f (xf). If init is not\nsupplied, (f) will be called to produce it. f should be a reducing\nstep function that accepts both 1 and 2 arguments, if it accepts\nonly 2 you can add the arity-1 with 'completing'. Returns the result\nof applying (the transformed) xf to init and the first item in coll,\nthen applying xf to that result and the 2nd item, etc. If coll\ncontains no items, returns init and f is not called. Note that\ncertain transforms may inject or skip items.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/transduce"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.1",
   :name "transient",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3344",
   :line 3344,
   :var-type "function",
   :arglists ([coll]),
   :doc
   "Returns a new, transient version of the collection, in constant time.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/transient"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "tree-seq",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4957",
   :line 4957,
   :var-type "function",
   :arglists ([branch? children root]),
   :doc
   "Returns a lazy sequence of the nodes in a tree, via a depth-first walk.\n branch? must be a fn of one arg that returns true if passed a node\n that can have children (but may not).  children must be a fn of one\n arg that returns a sequence of the children. Will only be called on\n nodes for which branch? returns true. Root is the root node of the\ntree.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/tree-seq"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "true?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L514",
   :line 514,
   :var-type "function",
   :arglists ([x]),
   :doc "Returns true if x is the value true, false otherwise.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/true?"}
  {:raw-source-url nil,
   :added "1.0",
   :name "try",
   :file nil,
   :source-url nil,
   :var-type "special form",
   :arglists nil,
   :doc
   "catch-clause => (catch classname name expr*)\nfinally-clause => (finally expr*)\n\nCatches and handles Java exceptions.\n\nPlease see https://clojure.org/reference/special_forms#try",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/try",
   :forms [(try expr* catch-clause* finally-clause?)]}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "type",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3468",
   :line 3468,
   :var-type "function",
   :arglists ([x]),
   :doc "Returns the :type metadata of x, or its Class if none",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/type"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "unchecked-add",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1212",
   :line 1212,
   :var-type "function",
   :arglists ([x y]),
   :doc
   "Returns the sum of x and y, both long.\nNote - uses a primitive operator subject to overflow.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-add"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "unchecked-add-int",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1205",
   :line 1205,
   :var-type "function",
   :arglists ([x y]),
   :doc
   "Returns the sum of x and y, both int.\nNote - uses a primitive operator subject to overflow.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-add-int"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.3",
   :name "unchecked-byte",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3518",
   :line 3518,
   :var-type "function",
   :arglists ([x]),
   :doc "Coerce to byte. Subject to rounding or truncation.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-byte"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.3",
   :name "unchecked-char",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3530",
   :line 3530,
   :var-type "function",
   :arglists ([x]),
   :doc "Coerce to char. Subject to rounding or truncation.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-char"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "unchecked-dec",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1184",
   :line 1184,
   :var-type "function",
   :arglists ([x]),
   :doc
   "Returns a number one less than x, a long.\nNote - uses a primitive operator subject to overflow.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-dec"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "unchecked-dec-int",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1177",
   :line 1177,
   :var-type "function",
   :arglists ([x]),
   :doc
   "Returns a number one less than x, an int.\nNote - uses a primitive operator subject to overflow.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-dec-int"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "unchecked-divide-int",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1247",
   :line 1247,
   :var-type "function",
   :arglists ([x y]),
   :doc
   "Returns the division of x by y, both int.\nNote - uses a primitive operator subject to truncation.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-divide-int"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.3",
   :name "unchecked-double",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3554",
   :line 3554,
   :var-type "function",
   :arglists ([x]),
   :doc "Coerce to double. Subject to rounding.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-double"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.3",
   :name "unchecked-float",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3548",
   :line 3548,
   :var-type "function",
   :arglists ([x]),
   :doc "Coerce to float. Subject to rounding.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-float"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "unchecked-inc",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1170",
   :line 1170,
   :var-type "function",
   :arglists ([x]),
   :doc
   "Returns a number one greater than x, a long.\nNote - uses a primitive operator subject to overflow.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-inc"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "unchecked-inc-int",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1163",
   :line 1163,
   :var-type "function",
   :arglists ([x]),
   :doc
   "Returns a number one greater than x, an int.\nNote - uses a primitive operator subject to overflow.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-inc-int"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.3",
   :name "unchecked-int",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3536",
   :line 3536,
   :var-type "function",
   :arglists ([x]),
   :doc "Coerce to int. Subject to rounding or truncation.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-int"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.3",
   :name "unchecked-long",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3542",
   :line 3542,
   :var-type "function",
   :arglists ([x]),
   :doc "Coerce to long. Subject to rounding or truncation.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-long"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "unchecked-multiply",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1240",
   :line 1240,
   :var-type "function",
   :arglists ([x y]),
   :doc
   "Returns the product of x and y, both long.\nNote - uses a primitive operator subject to overflow.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-multiply"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "unchecked-multiply-int",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1233",
   :line 1233,
   :var-type "function",
   :arglists ([x y]),
   :doc
   "Returns the product of x and y, both int.\nNote - uses a primitive operator subject to overflow.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-multiply-int"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "unchecked-negate",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1198",
   :line 1198,
   :var-type "function",
   :arglists ([x]),
   :doc
   "Returns the negation of x, a long.\nNote - uses a primitive operator subject to overflow.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-negate"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "unchecked-negate-int",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1191",
   :line 1191,
   :var-type "function",
   :arglists ([x]),
   :doc
   "Returns the negation of x, an int.\nNote - uses a primitive operator subject to overflow.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-negate-int"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "unchecked-remainder-int",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1254",
   :line 1254,
   :var-type "function",
   :arglists ([x y]),
   :doc
   "Returns the remainder of division of x by y, both int.\nNote - uses a primitive operator subject to truncation.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-remainder-int"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.3",
   :name "unchecked-short",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3524",
   :line 3524,
   :var-type "function",
   :arglists ([x]),
   :doc "Coerce to short. Subject to rounding or truncation.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-short"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "unchecked-subtract",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1226",
   :line 1226,
   :var-type "function",
   :arglists ([x y]),
   :doc
   "Returns the difference of x and y, both long.\nNote - uses a primitive operator subject to overflow.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-subtract"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "unchecked-subtract-int",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1219",
   :line 1219,
   :var-type "function",
   :arglists ([x y]),
   :doc
   "Returns the difference of x and y, both int.\nNote - uses a primitive operator subject to overflow.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unchecked-subtract-int"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "underive",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5690",
   :line 5690,
   :var-type "function",
   :arglists ([tag parent] [h tag parent]),
   :doc
   "Removes a parent/child relationship between parent and\ntag. h must be a hierarchy obtained from make-hierarchy, if not\nsupplied defaults to, and modifies, the global hierarchy.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/underive"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.7",
   :name "unreduced",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2872",
   :line 2872,
   :var-type "function",
   :arglists ([x]),
   :doc "If x is reduced?, returns (deref x), else returns x",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unreduced"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.6",
   :name "unsigned-bit-shift-right",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1382",
   :line 1382,
   :var-type "function",
   :arglists ([x n]),
   :doc "Bitwise shift right, without sign-extension.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/unsigned-bit-shift-right"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.7",
   :name "update",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6224",
   :line 6224,
   :var-type "function",
   :arglists
   ([m k f] [m k f x] [m k f x y] [m k f x y z] [m k f x y z & more]),
   :doc
   "'Updates' a value in an associative structure, where k is a\nkey and f is a function that will take the old value\nand any supplied args and return the new value, and returns a new\nstructure.  If the key does not exist, nil is passed as the old value.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/update"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "update-in",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6208",
   :line 6208,
   :var-type "function",
   :arglists ([m ks f & args]),
   :doc
   "'Updates' a value in a nested associative structure, where ks is a\nsequence of keys and f is a function that will take the old value\nand any supplied args and return the new value, and returns a new\nnested structure.  If any levels do not exist, hash-maps will be\ncreated.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/update-in"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.11",
   :name "update-keys",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L8025",
   :line 8025,
   :var-type "function",
   :arglists ([m f]),
   :doc
   "m f => {(f k) v ...}\n\nGiven a map m and a function f of 1-argument, returns a new map whose\nkeys are the result of applying f to the keys of m, mapped to the\ncorresponding values of m.\nf must return a unique key for each key of m, else the behavior is undefined.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/update-keys"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/658693f6cf97e6ab0ff789e096c9eb6654e4d3ab/src/clj/clojure/core_proxy.clj",
   :added "1.0",
   :name "update-proxy",
   :file "src/clj/clojure/core_proxy.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/658693f6cf97e6ab0ff789e096c9eb6654e4d3ab/src/clj/clojure/core_proxy.clj#L313",
   :line 313,
   :var-type "function",
   :arglists ([proxy mappings]),
   :doc
   "Takes a proxy instance and a map of strings (which must\ncorrespond to methods of the proxy superclass/superinterfaces) to\nfns (which must take arguments matching the corresponding method,\nplus an additional (explicit) first arg corresponding to this, and\nupdates (via assoc) the proxy's fn map. nil can be passed instead of\na fn, in which case the corresponding method will revert to the\ndefault behavior. Note that this function can be used to update the\nbehavior of an existing instance without changing its identity.\nReturns the proxy.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/update-proxy"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.11",
   :name "update-vals",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L8009",
   :line 8009,
   :var-type "function",
   :arglists ([m f]),
   :doc
   "m f => {k (f v) ...}\n\nGiven a map m and a function f of 1-argument, returns a new map where the keys of m\nare mapped to result of applying f to the corresponding values of m.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/update-vals"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.9",
   :name "uri?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7960",
   :line 7960,
   :var-type "function",
   :arglists ([x]),
   :doc "Return true if x is a java.net.URI",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/uri?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "use",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6129",
   :line 6129,
   :var-type "function",
   :arglists ([& args]),
   :doc
   "Like 'require, but also refers to each lib's namespace using\nclojure.core/refer. Use :use in the ns macro in preference to calling\nthis directly.\n\n'use accepts additional options in libspecs: :exclude, :only, :rename.\nThe arguments and semantics for :exclude, :only, and :rename are the same\nas those documented for clojure.core/refer.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/use"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.9",
   :name "uuid?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6857",
   :line 6857,
   :var-type "function",
   :arglists ([x]),
   :doc "Return true if x is a java.util.UUID",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/uuid?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "val",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1589",
   :line 1589,
   :var-type "function",
   :arglists ([e]),
   :doc "Returns the value in the map entry.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/val"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "vals",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1576",
   :line 1576,
   :var-type "function",
   :arglists ([map]),
   :doc
   "Returns a sequence of the map's values, in the same order as (seq map).",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/vals"}
  {:raw-source-url nil,
   :added "1.0",
   :name "var",
   :file nil,
   :source-url nil,
   :var-type "special form",
   :arglists nil,
   :doc
   "The symbol must resolve to a var, and the Var object\nitself (not its value) is returned. The reader macro #'x\nexpands to (var x).\n\nPlease see https://clojure.org/reference/special_forms#var",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/var",
   :forms [#'symbol]}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "var-get",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4329",
   :line 4329,
   :var-type "function",
   :arglists ([x]),
   :doc "Gets the value in the var object",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/var-get"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "var-set",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4335",
   :line 4335,
   :var-type "function",
   :arglists ([x val]),
   :doc
   "Sets the value in the var object to val. The var must be\nthread-locally bound.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/var-set"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "var?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5001",
   :line 5001,
   :var-type "function",
   :arglists ([v]),
   :doc "Returns true if v is of type clojure.lang.Var",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/var?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "vary-meta",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L677",
   :line 677,
   :var-type "function",
   :arglists ([obj f & args]),
   :doc
   "Returns an object of the same type and value as obj, with\n(apply f (meta obj) args) as its metadata.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/vary-meta"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "vec",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L369",
   :line 369,
   :var-type "function",
   :arglists ([coll]),
   :doc
   "Creates a new vector containing the contents of coll. Java arrays\nwill be aliased and should not be modified.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/vec"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "vector",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L355",
   :line 355,
   :var-type "function",
   :arglists
   ([]
    [a]
    [a b]
    [a b c]
    [a b c d]
    [a b c d e]
    [a b c d e f]
    [a b c d e f & args]),
   :doc "Creates a new vector containing the args.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/vector"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/79ac8b88f83c818314790449dda6ace7db275b56/src/clj/clojure/gvec.clj",
   :added "1.2",
   :name "vector-of",
   :file "src/clj/clojure/gvec.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/79ac8b88f83c818314790449dda6ace7db275b56/src/clj/clojure/gvec.clj#L523",
   :line 523,
   :var-type "function",
   :arglists ([t] [t & elements]),
   :doc
   "Creates a new vector of a single primitive type t, where t is one\nof :int :long :float :double :byte :short :char or :boolean. The\nresulting vector complies with the interface of vectors in general,\nbut stores the values unboxed internally.\n\nOptionally takes one or more elements to populate the vector.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/vector-of"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "vector?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L176",
   :line 176,
   :var-type "function",
   :arglists ([x]),
   :doc "Return true if x implements IPersistentVector",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/vector?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.7",
   :name "volatile!",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2542",
   :line 2542,
   :var-type "function",
   :arglists ([val]),
   :doc "Creates and returns a Volatile with an initial value of val.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/volatile!"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.7",
   :name "volatile?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2565",
   :line 2565,
   :var-type "function",
   :arglists ([x]),
   :doc "Returns true if x is a volatile.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/volatile?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.7",
   :name "vreset!",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2549",
   :line 2549,
   :var-type "function",
   :arglists ([vol newval]),
   :doc
   "Sets the value of volatile to newval without regard for the\ncurrent value. Returns newval.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/vreset!"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.7",
   :name "vswap!",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2556",
   :line 2556,
   :var-type "macro",
   :arglists ([vol f & args]),
   :doc
   "Non-atomically swaps the value of the volatile as if:\n(apply f current-value-of-vol args). Returns the value that\nwas swapped in.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/vswap!"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "when",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L495",
   :line 495,
   :var-type "macro",
   :arglists ([test & body]),
   :doc
   "Evaluates test. If logical true, evaluates body in an implicit do.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/when"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "when-first",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4625",
   :line 4625,
   :var-type "macro",
   :arglists ([bindings & body]),
   :doc
   "bindings => x xs\n\nRoughly the same as (when (seq xs) (let [x (first xs)] body)) but xs is evaluated only once",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/when-first"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "when-let",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1878",
   :line 1878,
   :var-type "macro",
   :arglists ([bindings & body]),
   :doc
   "bindings => binding-form test\n\nWhen test is true, evaluates body with binding-form bound to the value of test",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/when-let"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "when-not",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L501",
   :line 501,
   :var-type "macro",
   :arglists ([test & body]),
   :doc
   "Evaluates test. If logical false, evaluates body in an implicit do.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/when-not"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.6",
   :name "when-some",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1913",
   :line 1913,
   :var-type "macro",
   :arglists ([bindings & body]),
   :doc
   "bindings => binding-form test\n\nWhen test is not nil, evaluates body with binding-form bound to the\nvalue of test",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/when-some"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "while",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6369",
   :line 6369,
   :var-type "macro",
   :arglists ([test & body]),
   :doc
   "Repeatedly executes body while test expression is true. Presumes\nsome side-effect will cause test to become false/nil. Returns nil",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/while"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.1",
   :name "with-bindings",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L2003",
   :line 2003,
   :var-type "macro",
   :arglists ([binding-map & body]),
   :doc
   "Takes a map of Var/value pairs. Installs for the given Vars the associated\nvalues as thread-local bindings. Then executes body. Pops the installed\nbindings after body was evaluated. Returns the value of body.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/with-bindings"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.1",
   :name "with-bindings*",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L1990",
   :line 1990,
   :var-type "function",
   :arglists ([binding-map f & args]),
   :doc
   "Takes a map of Var/value pairs. Installs for the given Vars the associated\nvalues as thread-local bindings. Then calls f with the supplied arguments.\nPops the installed bindings after f returned. Returns whatever f returns.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/with-bindings*"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "with-in-str",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4752",
   :line 4752,
   :var-type "macro",
   :arglists ([s & body]),
   :doc
   "Evaluates body in a context in which *in* is bound to a fresh\nStringReader initialized with the string s.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/with-in-str"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "with-local-vars",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4342",
   :line 4342,
   :var-type "macro",
   :arglists ([name-vals-vec & body]),
   :doc
   "varbinding=> symbol init-expr\n\nExecutes the exprs in a context in which the symbols are bound to\nvars with per-thread bindings to the init-exprs.  The symbols refer\nto the var objects themselves, and must be accessed with var-get and\nvar-set",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/with-local-vars"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "with-meta",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L213",
   :line 213,
   :var-type "function",
   :arglists ([obj m]),
   :doc
   "Returns an object of the same type and value as obj, with\nmap m as its metadata.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/with-meta"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "with-open",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L3833",
   :line 3833,
   :var-type "macro",
   :arglists ([bindings & body]),
   :doc
   "bindings => [name init ...]\n\nEvaluates body in a try expression with names bound to the values\nof the inits, and a finally clause that calls (.close name) on each\nname in reverse order.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/with-open"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "with-out-str",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4741",
   :line 4741,
   :var-type "macro",
   :arglists ([& body]),
   :doc
   "Evaluates exprs in a context in which *out* is bound to a fresh\nStringWriter.  Returns the string created by any nested printing\ncalls.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/with-out-str"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "with-precision",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L5112",
   :line 5112,
   :var-type "macro",
   :arglists ([precision & exprs]),
   :doc
   "Sets the precision and rounding mode to be used for BigDecimal operations.\n\nUsage: (with-precision 10 (/ 1M 3))\nor:    (with-precision 10 :rounding HALF_DOWN (/ 1M 3))\n\nThe rounding mode is one of CEILING, FLOOR, HALF_UP, HALF_DOWN,\nHALF_EVEN, UP, DOWN and UNNECESSARY; it defaults to HALF_UP.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/with-precision"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.3",
   :name "with-redefs",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7587",
   :line 7587,
   :var-type "macro",
   :arglists ([bindings & body]),
   :doc
   "binding => var-symbol temp-value-expr\n\nTemporarily redefines Vars while executing the body.  The\ntemp-value-exprs will be evaluated and each resulting value will\nreplace in parallel the root value of its Var.  After the body is\nexecuted, the root values of all the Vars will be set back to their\nold values.  These temporary changes will be visible in all threads.\nUseful for mocking out functions during testing.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/with-redefs"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.3",
   :name "with-redefs-fn",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L7567",
   :line 7567,
   :var-type "function",
   :arglists ([binding-map func]),
   :doc
   "Temporarily redefines Vars during a call to func.  Each val of\nbinding-map will replace the root value of its key which must be\na Var.  After func is called with no args, the root values of all\nthe Vars will be set back to their old values.  These temporary\nchanges will be visible in all threads.  Useful for mocking out\nfunctions during testing.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/with-redefs-fn"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "xml-seq",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L4984",
   :line 4984,
   :var-type "function",
   :arglists ([root]),
   :doc "A tree seq on the xml elements as per xml/parse",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/xml-seq"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "zero?",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L869",
   :line 869,
   :var-type "function",
   :arglists ([num]),
   :doc "Returns true if num is zero, else false",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/zero?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj",
   :added "1.0",
   :name "zipmap",
   :file "src/clj/clojure/core.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/777456f5f485ed42cee386344fbce891c559ec4e/src/clj/clojure/core.clj#L6620",
   :line 6620,
   :var-type "function",
   :arglists ([keys vals]),
   :doc
   "Returns a map with the keys mapped to the corresponding vals.",
   :namespace "clojure.core",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core/zipmap"}
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
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/51c6d7a70912a8f65e81a8e11ae6f56c94920725/src/clj/clojure/data.clj",
   :added "1.3",
   :name "diff",
   :file "src/clj/clojure/data.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/51c6d7a70912a8f65e81a8e11ae6f56c94920725/src/clj/clojure/data.clj#L124",
   :line 124,
   :var-type "function",
   :arglists ([a b]),
   :doc
   "Recursively compares a and b, returning a tuple of\n[things-only-in-a things-only-in-b things-in-both].\nComparison rules:\n\n* For equal a and b, return [nil nil a].\n* Maps are subdiffed where keys match and values differ.\n* Sets are never subdiffed.\n* All sequential things are treated as associative collections\n  by their indexes, with results returned as vectors.\n* Everything else (including strings!) is treated as\n  an atom and compared for equality.",
   :namespace "clojure.data",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.data-api.html#clojure.data/diff"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/51c6d7a70912a8f65e81a8e11ae6f56c94920725/src/clj/clojure/data.clj",
   :added "1.3",
   :name "Diff",
   :file "src/clj/clojure/data.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/51c6d7a70912a8f65e81a8e11ae6f56c94920725/src/clj/clojure/data.clj#L73",
   :line 73,
   :var-type "protocol",
   :arglists nil,
   :doc "Implementation detail. Subject to change.",
   :namespace "clojure.data",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.data-api.html#clojure.data/Diff"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/51c6d7a70912a8f65e81a8e11ae6f56c94920725/src/clj/clojure/data.clj",
   :added "1.3",
   :name "EqualityPartition",
   :file "src/clj/clojure/data.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/51c6d7a70912a8f65e81a8e11ae6f56c94920725/src/clj/clojure/data.clj#L69",
   :line 69,
   :var-type "protocol",
   :arglists nil,
   :doc "Implementation detail. Subject to change.",
   :namespace "clojure.data",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.data-api.html#clojure.data/EqualityPartition"}
  {:raw-source-url nil,
   :added "1.3",
   :name "diff-similar",
   :file nil,
   :source-url nil,
   :var-type "function",
   :arglists ([a b]),
   :doc "Implementation detail. Subject to change.",
   :namespace "clojure.data",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.data-api.html#clojure.data/diff-similar"}
  {:raw-source-url nil,
   :added "1.3",
   :name "equality-partition",
   :file nil,
   :source-url nil,
   :var-type "function",
   :arglists ([x]),
   :doc "Implementation detail. Subject to change.",
   :namespace "clojure.data",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.data-api.html#clojure.data/equality-partition"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/b70db9639f9acddcabf7f760ea4bb050d6bfaa16/src/clj/clojure/datafy.clj",
   :name "datafy",
   :file "src/clj/clojure/datafy.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/b70db9639f9acddcabf7f760ea4bb050d6bfaa16/src/clj/clojure/datafy.clj#L15",
   :line 15,
   :var-type "function",
   :arglists ([x]),
   :doc
   "Attempts to return x as data.\ndatafy will return the value of clojure.core.protocols/datafy. If\nthe value has been transformed and the result supports\nmetadata, :clojure.datafy/obj will be set on the metadata to the\noriginal value of x, and :clojure.datafy/class to the name of the\nclass of x, as a symbol.",
   :namespace "clojure.datafy",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.datafy-api.html#clojure.datafy/datafy"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/b70db9639f9acddcabf7f760ea4bb050d6bfaa16/src/clj/clojure/datafy.clj",
   :name "nav",
   :file "src/clj/clojure/datafy.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/b70db9639f9acddcabf7f760ea4bb050d6bfaa16/src/clj/clojure/datafy.clj#L30",
   :line 30,
   :var-type "function",
   :arglists ([coll k v]),
   :doc
   "Returns (possibly transformed) v in the context of coll and k (a\nkey/index or nil). Callers should attempt to provide the key/index\ncontext k for Indexed/Associative/ILookup colls if possible, but not\nto fabricate one e.g. for sequences (pass nil). nav returns the\nvalue of clojure.core.protocols/nav.",
   :namespace "clojure.datafy",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.datafy-api.html#clojure.datafy/nav"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/c6756a8bab137128c8119add29a25b0a88509900/src/clj/clojure/edn.clj",
   :added "1.5",
   :name "read",
   :file "src/clj/clojure/edn.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/c6756a8bab137128c8119add29a25b0a88509900/src/clj/clojure/edn.clj#L14",
   :line 14,
   :var-type "function",
   :arglists ([] [stream] [opts stream]),
   :doc
   "Reads the next object from stream, which must be an instance of\njava.io.PushbackReader or some derivee.  stream defaults to the\ncurrent value of *in*.\n\nReads data in the edn format (subset of Clojure data):\nhttp://edn-format.org\n\nopts is a map that can include the following keys:\n:eof - value to return on end-of-file. When not supplied, eof throws an exception.\n:readers  - a map of tag symbols to data-reader functions to be considered before default-data-readers.\n            When not supplied, only the default-data-readers will be used.\n:default - A function of two args, that will, if present and no reader is found for a tag,\n           be called with the tag and the value.",
   :namespace "clojure.edn",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.edn-api.html#clojure.edn/read"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/c6756a8bab137128c8119add29a25b0a88509900/src/clj/clojure/edn.clj",
   :added "1.5",
   :name "read-string",
   :file "src/clj/clojure/edn.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/c6756a8bab137128c8119add29a25b0a88509900/src/clj/clojure/edn.clj#L37",
   :line 37,
   :var-type "function",
   :arglists ([s] [opts s]),
   :doc
   "Reads one object from the string s. Returns nil when s is nil or empty.\n\nReads data in the edn format (subset of Clojure data):\nhttp://edn-format.org\n\nopts is a map as per clojure.edn/read",
   :namespace "clojure.edn",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.edn-api.html#clojure.edn/read-string"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/5da21b38470d175b3cecbf93b9cd145ca36940c1/src/clj/clojure/inspector.clj",
   :added "1.0",
   :name "inspect",
   :file "src/clj/clojure/inspector.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/5da21b38470d175b3cecbf93b9cd145ca36940c1/src/clj/clojure/inspector.clj#L154",
   :line 154,
   :var-type "function",
   :arglists ([x]),
   :doc "creates a graphical (Swing) inspector on the supplied object",
   :namespace "clojure.inspector",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.inspector-api.html#clojure.inspector/inspect"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/5da21b38470d175b3cecbf93b9cd145ca36940c1/src/clj/clojure/inspector.clj",
   :added "1.0",
   :name "inspect-table",
   :file "src/clj/clojure/inspector.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/5da21b38470d175b3cecbf93b9cd145ca36940c1/src/clj/clojure/inspector.clj#L100",
   :line 100,
   :var-type "function",
   :arglists ([data]),
   :doc
   "creates a graphical (Swing) inspector on the supplied regular\ndata, which must be a sequential data structure of data structures\nof equal length",
   :namespace "clojure.inspector",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.inspector-api.html#clojure.inspector/inspect-table"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/5da21b38470d175b3cecbf93b9cd145ca36940c1/src/clj/clojure/inspector.clj",
   :added "1.0",
   :name "inspect-tree",
   :file "src/clj/clojure/inspector.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/5da21b38470d175b3cecbf93b9cd145ca36940c1/src/clj/clojure/inspector.clj#L91",
   :line 91,
   :var-type "function",
   :arglists ([data]),
   :doc
   "creates a graphical (Swing) inspector on the supplied hierarchical data",
   :namespace "clojure.inspector",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.inspector-api.html#clojure.inspector/inspect-tree"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/385d0593efa658ada19f9a55af39cef146c75341/src/clj/clojure/instant.clj",
   :name "parse-timestamp",
   :file "src/clj/clojure/instant.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/385d0593efa658ada19f9a55af39cef146c75341/src/clj/clojure/instant.clj#L53",
   :line 53,
   :var-type "function",
   :arglists ([new-instant cs]),
   :doc
   "Parse a string containing an RFC3339-like like timestamp.\n\nThe function new-instant is called with the following arguments.\n\n                min  max           default\n                ---  ------------  -------\n  years          0           9999      N/A (s must provide years)\n  months         1             12        1\n  days           1             31        1 (actual max days depends\n  hours          0             23        0  on month and year)\n  minutes        0             59        0\n  seconds        0             60        0 (though 60 is only valid\n  nanoseconds    0      999999999        0  when minutes is 59)\n  offset-sign   -1              1        0\n  offset-hours   0             23        0\n  offset-minutes 0             59        0\n\nThese are all integers and will be non-nil. (The listed defaults\nwill be passed if the corresponding field is not present in s.)\n\nGrammar (of s):\n\n  date-fullyear   = 4DIGIT\n  date-month      = 2DIGIT  ; 01-12\n  date-mday       = 2DIGIT  ; 01-28, 01-29, 01-30, 01-31 based on\n                            ; month/year\n  time-hour       = 2DIGIT  ; 00-23\n  time-minute     = 2DIGIT  ; 00-59\n  time-second     = 2DIGIT  ; 00-58, 00-59, 00-60 based on leap second\n                            ; rules\n  time-secfrac    = '.' 1*DIGIT\n  time-numoffset  = ('+' / '-') time-hour ':' time-minute\n  time-offset     = 'Z' / time-numoffset\n\n  time-part       = time-hour [ ':' time-minute [ ':' time-second\n                    [time-secfrac] [time-offset] ] ]\n\n  timestamp       = date-year [ '-' date-month [ '-' date-mday\n                    [ 'T' time-part ] ] ]\n\nUnlike RFC3339:\n\n  - we only parse the timestamp format\n  - timestamp can elide trailing components\n  - time-offset is optional (defaults to +00:00)\n\nThough time-offset is syntactically optional, a missing time-offset\nwill be treated as if the time-offset zero (+00:00) had been\nspecified.",
   :namespace "clojure.instant",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.instant-api.html#clojure.instant/parse-timestamp"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/385d0593efa658ada19f9a55af39cef146c75341/src/clj/clojure/instant.clj",
   :name "read-instant-calendar",
   :file "src/clj/clojure/instant.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/385d0593efa658ada19f9a55af39cef146c75341/src/clj/clojure/instant.clj#L281",
   :line 281,
   :var-type "function",
   :arglists ([cs]),
   :doc
   "To read an instant as a java.util.Calendar, bind *data-readers* to a map with\nthis var as the value for the 'inst key.  Calendar preserves the timezone\noffset.",
   :namespace "clojure.instant",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.instant-api.html#clojure.instant/read-instant-calendar"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/385d0593efa658ada19f9a55af39cef146c75341/src/clj/clojure/instant.clj",
   :name "read-instant-date",
   :file "src/clj/clojure/instant.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/385d0593efa658ada19f9a55af39cef146c75341/src/clj/clojure/instant.clj#L274",
   :line 274,
   :var-type "function",
   :arglists ([cs]),
   :doc
   "To read an instant as a java.util.Date, bind *data-readers* to a map with\nthis var as the value for the 'inst key. The timezone offset will be used\nto convert into UTC.",
   :namespace "clojure.instant",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.instant-api.html#clojure.instant/read-instant-date"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/385d0593efa658ada19f9a55af39cef146c75341/src/clj/clojure/instant.clj",
   :name "read-instant-timestamp",
   :file "src/clj/clojure/instant.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/385d0593efa658ada19f9a55af39cef146c75341/src/clj/clojure/instant.clj#L288",
   :line 288,
   :var-type "function",
   :arglists ([cs]),
   :doc
   "To read an instant as a java.sql.Timestamp, bind *data-readers* to a\nmap with this var as the value for the 'inst key. Timestamp preserves\nfractional seconds with nanosecond precision. The timezone offset will\nbe used to convert into UTC.",
   :namespace "clojure.instant",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.instant-api.html#clojure.instant/read-instant-timestamp"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/385d0593efa658ada19f9a55af39cef146c75341/src/clj/clojure/instant.clj",
   :name "validated",
   :file "src/clj/clojure/instant.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/385d0593efa658ada19f9a55af39cef146c75341/src/clj/clojure/instant.clj#L139",
   :line 139,
   :var-type "function",
   :arglists ([new-instance]),
   :doc
   "Return a function which constructs an instant by calling constructor\nafter first validating that those arguments are in range and otherwise\nplausible. The resulting function will throw an exception if called\nwith invalid arguments.",
   :namespace "clojure.instant",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.instant-api.html#clojure.instant/validated"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/19e3a2708def5ffb7f2be030d8e8e895464ce2d2/src/clj/clojure/java/browse.clj",
   :added "1.2",
   :name "browse-url",
   :file "src/clj/clojure/java/browse.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/19e3a2708def5ffb7f2be030d8e8e895464ce2d2/src/clj/clojure/java/browse.clj#L68",
   :line 68,
   :var-type "function",
   :arglists ([url]),
   :doc "Open url in a browser",
   :namespace "clojure.java.browse",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.browse-api.html#clojure.java.browse/browse-url"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/ee1b606ad066ac8df2efd4a6b8d0d365c206f5bf/src/clj/clojure/java/io.clj",
   :added "1.2",
   :name "as-relative-path",
   :file "src/clj/clojure/java/io.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/ee1b606ad066ac8df2efd4a6b8d0d365c206f5bf/src/clj/clojure/java/io.clj#L408",
   :line 408,
   :var-type "function",
   :arglists ([x]),
   :doc
   "Take an as-file-able thing and return a string if it is\na relative path, else IllegalArgumentException.",
   :namespace "clojure.java.io",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.io-api.html#clojure.java.io/as-relative-path"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/ee1b606ad066ac8df2efd4a6b8d0d365c206f5bf/src/clj/clojure/java/io.clj",
   :added "1.2",
   :name "copy",
   :file "src/clj/clojure/java/io.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/ee1b606ad066ac8df2efd4a6b8d0d365c206f5bf/src/clj/clojure/java/io.clj#L391",
   :line 391,
   :var-type "function",
   :arglists ([input output & opts]),
   :doc
   "Copies input to output.  Returns nil or throws IOException.\nInput may be an InputStream, Reader, File, byte[], char[], or String.\nOutput may be an OutputStream, Writer, or File.\n\nOptions are key/value pairs and may be one of\n\n  :buffer-size  buffer size to use, default is 1024.\n  :encoding     encoding to use if converting between\n                byte and char streams.   \n\nDoes not close any streams except those it opens itself \n(on a File).",
   :namespace "clojure.java.io",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.io-api.html#clojure.java.io/copy"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/ee1b606ad066ac8df2efd4a6b8d0d365c206f5bf/src/clj/clojure/java/io.clj",
   :added "1.2",
   :name "delete-file",
   :file "src/clj/clojure/java/io.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/ee1b606ad066ac8df2efd4a6b8d0d365c206f5bf/src/clj/clojure/java/io.clj#L430",
   :line 430,
   :var-type "function",
   :arglists ([f & [silently]]),
   :doc
   "Delete file f. If silently is nil or false, raise an exception on failure, else return the value of silently.",
   :namespace "clojure.java.io",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.io-api.html#clojure.java.io/delete-file"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/ee1b606ad066ac8df2efd4a6b8d0d365c206f5bf/src/clj/clojure/java/io.clj",
   :added "1.2",
   :name "file",
   :file "src/clj/clojure/java/io.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/ee1b606ad066ac8df2efd4a6b8d0d365c206f5bf/src/clj/clojure/java/io.clj#L418",
   :line 418,
   :var-type "function",
   :arglists ([arg] [parent child] [parent child & more]),
   :doc
   "Returns a java.io.File, passing each arg to as-file.  Multiple-arg\nversions treat the first argument as parent and subsequent args as\nchildren relative to the parent.",
   :namespace "clojure.java.io",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.io-api.html#clojure.java.io/file"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/ee1b606ad066ac8df2efd4a6b8d0d365c206f5bf/src/clj/clojure/java/io.clj",
   :added "1.2",
   :name "input-stream",
   :file "src/clj/clojure/java/io.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/ee1b606ad066ac8df2efd4a6b8d0d365c206f5bf/src/clj/clojure/java/io.clj#L121",
   :line 121,
   :var-type "function",
   :arglists ([x & opts]),
   :doc
   "Attempts to coerce its argument into an open java.io.InputStream.\nDefault implementations always return a java.io.BufferedInputStream.\n\nDefault implementations are defined for InputStream, File, URI, URL,\nSocket, byte array, and String arguments.\n\nIf the argument is a String, it tries to resolve it first as a URI, then\nas a local file name.  URIs with a 'file' protocol are converted to\nlocal file names.\n\nShould be used inside with-open to ensure the InputStream is properly\nclosed.",
   :namespace "clojure.java.io",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.io-api.html#clojure.java.io/input-stream"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/ee1b606ad066ac8df2efd4a6b8d0d365c206f5bf/src/clj/clojure/java/io.clj",
   :added "1.2",
   :name "make-parents",
   :file "src/clj/clojure/java/io.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/ee1b606ad066ac8df2efd4a6b8d0d365c206f5bf/src/clj/clojure/java/io.clj#L438",
   :line 438,
   :var-type "function",
   :arglists ([f & more]),
   :doc
   "Given the same arg(s) as for file, creates all parent directories of\nthe file they represent.",
   :namespace "clojure.java.io",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.io-api.html#clojure.java.io/make-parents"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/ee1b606ad066ac8df2efd4a6b8d0d365c206f5bf/src/clj/clojure/java/io.clj",
   :added "1.2",
   :name "output-stream",
   :file "src/clj/clojure/java/io.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/ee1b606ad066ac8df2efd4a6b8d0d365c206f5bf/src/clj/clojure/java/io.clj#L138",
   :line 138,
   :var-type "function",
   :arglists ([x & opts]),
   :doc
   "Attempts to coerce its argument into an open java.io.OutputStream.\nDefault implementations always return a java.io.BufferedOutputStream.\n\nDefault implementations are defined for OutputStream, File, URI, URL,\nSocket, and String arguments.\n\nIf the argument is a String, it tries to resolve it first as a URI, then\nas a local file name.  URIs with a 'file' protocol are converted to\nlocal file names.\n\nShould be used inside with-open to ensure the OutputStream is\nproperly closed.",
   :namespace "clojure.java.io",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.io-api.html#clojure.java.io/output-stream"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/ee1b606ad066ac8df2efd4a6b8d0d365c206f5bf/src/clj/clojure/java/io.clj",
   :added "1.2",
   :name "reader",
   :file "src/clj/clojure/java/io.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/ee1b606ad066ac8df2efd4a6b8d0d365c206f5bf/src/clj/clojure/java/io.clj#L86",
   :line 86,
   :var-type "function",
   :arglists ([x & opts]),
   :doc
   "Attempts to coerce its argument into an open java.io.Reader.\nDefault implementations always return a java.io.BufferedReader.\n\nDefault implementations are provided for Reader, BufferedReader,\nInputStream, File, URI, URL, Socket, byte arrays, character arrays,\nand String.\n\nIf argument is a String, it tries to resolve it first as a URI, then\nas a local file name.  URIs with a 'file' protocol are converted to\nlocal file names.\n\nShould be used inside with-open to ensure the Reader is properly\nclosed.",
   :namespace "clojure.java.io",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.io-api.html#clojure.java.io/reader"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/ee1b606ad066ac8df2efd4a6b8d0d365c206f5bf/src/clj/clojure/java/io.clj",
   :added "1.2",
   :name "resource",
   :file "src/clj/clojure/java/io.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/ee1b606ad066ac8df2efd4a6b8d0d365c206f5bf/src/clj/clojure/java/io.clj#L446",
   :line 446,
   :var-type "function",
   :arglists ([n] [n loader]),
   :doc
   "Returns the URL for a named resource. Use the context class loader\nif no loader is specified.",
   :namespace "clojure.java.io",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.io-api.html#clojure.java.io/resource"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/ee1b606ad066ac8df2efd4a6b8d0d365c206f5bf/src/clj/clojure/java/io.clj",
   :added "1.2",
   :name "writer",
   :file "src/clj/clojure/java/io.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/ee1b606ad066ac8df2efd4a6b8d0d365c206f5bf/src/clj/clojure/java/io.clj#L104",
   :line 104,
   :var-type "function",
   :arglists ([x & opts]),
   :doc
   "Attempts to coerce its argument into an open java.io.Writer.\nDefault implementations always return a java.io.BufferedWriter.\n\nDefault implementations are provided for Writer, BufferedWriter,\nOutputStream, File, URI, URL, Socket, and String.\n\nIf the argument is a String, it tries to resolve it first as a URI, then\nas a local file name.  URIs with a 'file' protocol are converted to\nlocal file names.\n\nShould be used inside with-open to ensure the Writer is properly\nclosed.",
   :namespace "clojure.java.io",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.io-api.html#clojure.java.io/writer"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/ee1b606ad066ac8df2efd4a6b8d0d365c206f5bf/src/clj/clojure/java/io.clj",
   :added "1.2",
   :name "Coercions",
   :file "src/clj/clojure/java/io.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/ee1b606ad066ac8df2efd4a6b8d0d365c206f5bf/src/clj/clojure/java/io.clj#L35",
   :line 35,
   :var-type "protocol",
   :arglists nil,
   :doc "Coerce between various 'resource-namish' things.",
   :namespace "clojure.java.io",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.io-api.html#clojure.java.io/Coercions"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/ee1b606ad066ac8df2efd4a6b8d0d365c206f5bf/src/clj/clojure/java/io.clj",
   :added "1.2",
   :name "IOFactory",
   :file "src/clj/clojure/java/io.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/ee1b606ad066ac8df2efd4a6b8d0d365c206f5bf/src/clj/clojure/java/io.clj#L69",
   :line 69,
   :var-type "protocol",
   :arglists nil,
   :doc
   "Factory functions that create ready-to-use, buffered versions of\nthe various Java I/O stream types, on top of anything that can\nbe unequivocally converted to the requested kind of stream.\n\nCommon options include\n\n  :append    true to open stream in append mode\n  :encoding  string name of encoding to use, e.g. \"UTF-8\".\n\nCallers should generally prefer the higher level API provided by\nreader, writer, input-stream, and output-stream.",
   :namespace "clojure.java.io",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.io-api.html#clojure.java.io/IOFactory"}
  {:raw-source-url nil,
   :added "1.2",
   :name "as-file",
   :file nil,
   :source-url nil,
   :var-type "function",
   :arglists ([x]),
   :doc "Coerce argument to a file.",
   :namespace "clojure.java.io",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.io-api.html#clojure.java.io/as-file"}
  {:raw-source-url nil,
   :added "1.2",
   :name "as-url",
   :file nil,
   :source-url nil,
   :var-type "function",
   :arglists ([x]),
   :doc "Coerce argument to a URL.",
   :namespace "clojure.java.io",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.io-api.html#clojure.java.io/as-url"}
  {:raw-source-url nil,
   :added "1.2",
   :name "make-input-stream",
   :file nil,
   :source-url nil,
   :var-type "function",
   :arglists ([x opts]),
   :doc "Creates a BufferedInputStream. See also IOFactory docs.",
   :namespace "clojure.java.io",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.io-api.html#clojure.java.io/make-input-stream"}
  {:raw-source-url nil,
   :added "1.2",
   :name "make-output-stream",
   :file nil,
   :source-url nil,
   :var-type "function",
   :arglists ([x opts]),
   :doc "Creates a BufferedOutputStream. See also IOFactory docs.",
   :namespace "clojure.java.io",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.io-api.html#clojure.java.io/make-output-stream"}
  {:raw-source-url nil,
   :added "1.2",
   :name "make-reader",
   :file nil,
   :source-url nil,
   :var-type "function",
   :arglists ([x opts]),
   :doc "Creates a BufferedReader. See also IOFactory docs.",
   :namespace "clojure.java.io",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.io-api.html#clojure.java.io/make-reader"}
  {:raw-source-url nil,
   :added "1.2",
   :name "make-writer",
   :file nil,
   :source-url nil,
   :var-type "function",
   :arglists ([x opts]),
   :doc "Creates a BufferedWriter. See also IOFactory docs.",
   :namespace "clojure.java.io",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.io-api.html#clojure.java.io/make-writer"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/3b6c31bc503afe8f25d01d6d7d05ebc960095abd/src/clj/clojure/java/javadoc.clj",
   :added "1.2",
   :name "add-local-javadoc",
   :file "src/clj/clojure/java/javadoc.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/3b6c31bc503afe8f25d01d6d7d05ebc960095abd/src/clj/clojure/java/javadoc.clj#L47",
   :line 47,
   :var-type "function",
   :arglists ([path]),
   :doc "Adds to the list of local Javadoc paths.",
   :namespace "clojure.java.javadoc",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.javadoc-api.html#clojure.java.javadoc/add-local-javadoc"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/3b6c31bc503afe8f25d01d6d7d05ebc960095abd/src/clj/clojure/java/javadoc.clj",
   :added "1.2",
   :name "add-remote-javadoc",
   :file "src/clj/clojure/java/javadoc.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/3b6c31bc503afe8f25d01d6d7d05ebc960095abd/src/clj/clojure/java/javadoc.clj#L53",
   :line 53,
   :var-type "function",
   :arglists ([package-prefix url]),
   :doc
   "Adds to the list of remote Javadoc URLs.  package-prefix is the\nbeginning of the package name that has docs at this URL.",
   :namespace "clojure.java.javadoc",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.javadoc-api.html#clojure.java.javadoc/add-remote-javadoc"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/3b6c31bc503afe8f25d01d6d7d05ebc960095abd/src/clj/clojure/java/javadoc.clj",
   :added "1.2",
   :name "javadoc",
   :file "src/clj/clojure/java/javadoc.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/3b6c31bc503afe8f25d01d6d7d05ebc960095abd/src/clj/clojure/java/javadoc.clj#L92",
   :line 92,
   :var-type "function",
   :arglists ([class-or-object]),
   :doc
   "Opens a browser window displaying the javadoc for the argument.\nTries *local-javadocs* first, then *remote-javadocs*.",
   :namespace "clojure.java.javadoc",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.javadoc-api.html#clojure.java.javadoc/javadoc"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/027d8ff2859442b222bf9cfa4c1be45567b788eb/src/clj/clojure/java/shell.clj",
   :added "1.2",
   :name "sh",
   :file "src/clj/clojure/java/shell.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/027d8ff2859442b222bf9cfa4c1be45567b788eb/src/clj/clojure/java/shell.clj#L79",
   :line 79,
   :var-type "function",
   :arglists ([& args]),
   :doc
   "Passes the given strings to Runtime.exec() to launch a sub-process.\n\nOptions are\n\n:in      may be given followed by any legal input source for\n         clojure.java.io/copy, e.g. InputStream, Reader, File, byte[],\n         or String, to be fed to the sub-process's stdin.\n:in-enc  option may be given followed by a String, used as a character\n         encoding name (for example \"UTF-8\" or \"ISO-8859-1\") to\n         convert the input string specified by the :in option to the\n         sub-process's stdin.  Defaults to UTF-8.\n         If the :in option provides a byte array, then the bytes are passed\n         unencoded, and this option is ignored.\n:out-enc option may be given followed by :bytes or a String. If a\n         String is given, it will be used as a character encoding\n         name (for example \"UTF-8\" or \"ISO-8859-1\") to convert\n         the sub-process's stdout to a String which is returned.\n         If :bytes is given, the sub-process's stdout will be stored\n         in a byte array and returned.  Defaults to UTF-8.\n:env     override the process env with a map (or the underlying Java\n         String[] if you are a masochist).\n:dir     override the process dir with a String or java.io.File.\n\nYou can bind :env or :dir for multiple operations using with-sh-env\nand with-sh-dir.\n\nsh returns a map of\n  :exit => sub-process's exit code\n  :out  => sub-process's stdout (as byte[] or String)\n  :err  => sub-process's stderr (String via platform default encoding)",
   :namespace "clojure.java.shell",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.shell-api.html#clojure.java.shell/sh"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/027d8ff2859442b222bf9cfa4c1be45567b788eb/src/clj/clojure/java/shell.clj",
   :added "1.2",
   :name "with-sh-dir",
   :file "src/clj/clojure/java/shell.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/027d8ff2859442b222bf9cfa4c1be45567b788eb/src/clj/clojure/java/shell.clj#L21",
   :line 21,
   :var-type "macro",
   :arglists ([dir & forms]),
   :doc "Sets the directory for use with sh, see sh for details.",
   :namespace "clojure.java.shell",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.shell-api.html#clojure.java.shell/with-sh-dir"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/027d8ff2859442b222bf9cfa4c1be45567b788eb/src/clj/clojure/java/shell.clj",
   :added "1.2",
   :name "with-sh-env",
   :file "src/clj/clojure/java/shell.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/027d8ff2859442b222bf9cfa4c1be45567b788eb/src/clj/clojure/java/shell.clj#L28",
   :line 28,
   :var-type "macro",
   :arglists ([env & forms]),
   :doc "Sets the environment for use with sh, see sh for details.",
   :namespace "clojure.java.shell",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.java.shell-api.html#clojure.java.shell/with-sh-env"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj",
   :added "1.3",
   :name "demunge",
   :file "src/clj/clojure/main.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj#L28",
   :line 28,
   :var-type "function",
   :arglists ([fn-name]),
   :doc
   "Given a string representation of a fn class,\nas in a stack trace element, returns a readable version.",
   :namespace "clojure.main",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/demunge"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj",
   :name "err->msg",
   :file "src/clj/clojure/main.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj#L343",
   :line 343,
   :var-type "function",
   :arglists ([e]),
   :doc "Helper to return an error message string from an exception.",
   :namespace "clojure.main",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/err->msg"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj",
   :added "1.10",
   :name "ex-str",
   :file "src/clj/clojure/main.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj#L269",
   :line 269,
   :var-type "function",
   :arglists
   ([{:clojure.error/keys
      [phase source path line column symbol class cause spec],
      :as triage-data}]),
   :doc
   "Returns a string from exception data, as produced by ex-triage.\nThe first line summarizes the exception phase and location.\nThe subsequent lines describe the cause.",
   :namespace "clojure.main",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/ex-str"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj",
   :added "1.10",
   :name "ex-triage",
   :file "src/clj/clojure/main.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj#L208",
   :line 208,
   :var-type "function",
   :arglists ([datafied-throwable]),
   :doc
   "Returns an analysis of the phase, error, cause, and location of an error that occurred\nbased on Throwable data, as returned by Throwable->map. All attributes other than phase\nare optional:\n  :clojure.error/phase - keyword phase indicator, one of:\n    :read-source :compile-syntax-check :compilation :macro-syntax-check :macroexpansion\n    :execution :read-eval-result :print-eval-result\n  :clojure.error/source - file name (no path)\n  :clojure.error/path - source path\n  :clojure.error/line - integer line number\n  :clojure.error/column - integer column number\n  :clojure.error/symbol - symbol being expanded/compiled/invoked\n  :clojure.error/class - cause exception class symbol\n  :clojure.error/cause - cause exception message\n  :clojure.error/spec - explain-data for spec error",
   :namespace "clojure.main",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/ex-triage"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj",
   :name "load-script",
   :file "src/clj/clojure/main.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj#L468",
   :line 468,
   :var-type "function",
   :arglists ([path]),
   :doc
   "Loads Clojure source from a file or resource given its path. Paths\nbeginning with @ or @/ are considered relative to classpath.",
   :namespace "clojure.main",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/load-script"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj",
   :name "main",
   :file "src/clj/clojure/main.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj#L616",
   :line 616,
   :var-type "function",
   :arglists ([& args]),
   :doc
   "Usage: java -cp clojure.jar clojure.main [init-opt*] [main-opt] [arg*]\n\nWith no options or args, runs an interactive Read-Eval-Print Loop\n\ninit options:\n  -i, --init path     Load a file or resource\n  -e, --eval string   Evaluate expressions in string; print non-nil values\n  --report target     Report uncaught exception to \"file\" (default), \"stderr\",\n                      or \"none\", overrides System property clojure.main.report\n\nmain options:\n  -m, --main ns-name  Call the -main function from a namespace with args\n  -r, --repl          Run a repl\n  path                Run a script from a file or resource\n  -                   Run a script from standard input\n  -h, -?, --help      Print this help message and exit\n\noperation:\n\n  - Establishes thread-local bindings for commonly set!-able vars\n  - Enters the user namespace\n  - Binds *command-line-args* to a seq of strings containing command line\n    args that appear after any main option\n  - Runs all init options in order\n  - Calls a -main function or runs a repl or script if requested\n\nThe init options may be repeated and mixed freely, but must appear before\nany main option. The appearance of any eval option before running a repl\nsuppresses the usual repl greeting message: \"Clojure ~(clojure-version)\".\n\nPaths may be absolute or relative in the filesystem or relative to\nclasspath. Classpath-relative paths have prefix of @ or @/",
   :namespace "clojure.main",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/main"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj",
   :added "1.10",
   :name "renumbering-read",
   :file "src/clj/clojure/main.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj#L140",
   :line 140,
   :var-type "function",
   :arglists ([opts reader line-number]),
   :doc
   "Reads from reader, which must be a LineNumberingPushbackReader, while capturing\nthe read string. If the read is successful, reset the line number and re-read.\nThe line number on re-read is the passed line-number unless :line or\n:clojure.core/eval-file meta are explicitly set on the read value.",
   :namespace "clojure.main",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/renumbering-read"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj",
   :name "repl",
   :file "src/clj/clojure/main.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj#L368",
   :line 368,
   :var-type "function",
   :arglists ([& options]),
   :doc
   "Generic, reusable, read-eval-print loop. By default, reads from *in*,\nwrites to *out*, and prints exception summaries to *err*. If you use the\ndefault :read hook, *in* must either be an instance of\nLineNumberingPushbackReader or duplicate its behavior of both supporting\n.unread and collapsing CR, LF, and CRLF into a single \\newline. Options\nare sequential keyword-value pairs. Available options and their defaults:\n\n   - :init, function of no arguments, initialization hook called with\n     bindings for set!-able vars in place.\n     default: #()\n\n   - :need-prompt, function of no arguments, called before each\n     read-eval-print except the first, the user will be prompted if it\n     returns true.\n     default: (if (instance? LineNumberingPushbackReader *in*)\n                #(.atLineStart *in*)\n                #(identity true))\n\n   - :prompt, function of no arguments, prompts for more input.\n     default: repl-prompt\n\n   - :flush, function of no arguments, flushes output\n     default: flush\n\n   - :read, function of two arguments, reads from *in*:\n       - returns its first argument to request a fresh prompt\n         - depending on need-prompt, this may cause the repl to prompt\n           before reading again\n       - returns its second argument to request an exit from the repl\n       - else returns the next object read from the input stream\n     default: repl-read\n\n   - :eval, function of one argument, returns the evaluation of its\n     argument\n     default: eval\n\n   - :print, function of one argument, prints its argument to the output\n     default: prn\n\n   - :caught, function of one argument, a throwable, called when\n     read, eval, or print throws an exception or error\n     default: repl-caught",
   :namespace "clojure.main",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/repl"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj",
   :name "repl-caught",
   :file "src/clj/clojure/main.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj#L348",
   :line 348,
   :var-type "function",
   :arglists ([e]),
   :doc "Default :caught hook for repl",
   :namespace "clojure.main",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/repl-caught"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj",
   :name "repl-exception",
   :file "src/clj/clojure/main.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj#L172",
   :line 172,
   :var-type "function",
   :arglists ([throwable]),
   :doc "Returns the root cause of throwables",
   :namespace "clojure.main",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/repl-exception"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj",
   :name "repl-prompt",
   :file "src/clj/clojure/main.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj#L103",
   :line 103,
   :var-type "function",
   :arglists ([]),
   :doc "Default :prompt hook for repl",
   :namespace "clojure.main",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/repl-prompt"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj",
   :name "repl-read",
   :file "src/clj/clojure/main.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj#L154",
   :line 154,
   :var-type "function",
   :arglists ([request-prompt request-exit]),
   :doc
   "Default :read hook for repl. Reads from *in* which must either be an\ninstance of LineNumberingPushbackReader or duplicate its behavior of both\nsupporting .unread and collapsing all of CR, LF, and CRLF into a single\n\\newline. repl-read:\n  - skips whitespace, then\n    - returns request-prompt on start of line, or\n    - returns request-exit on end of stream, or\n    - reads an object from the input stream, then\n      - skips the next input character if it's end of line, then\n      - returns the object.",
   :namespace "clojure.main",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/repl-read"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj",
   :name "repl-requires",
   :file "src/clj/clojure/main.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj#L355",
   :line 355,
   :var-type "var",
   :arglists nil,
   :doc
   "A sequence of lib specs that are applied to `require`\nby default when a new command-line REPL is started.",
   :namespace "clojure.main",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/repl-requires"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj",
   :name "report-error",
   :file "src/clj/clojure/main.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj#L584",
   :line 584,
   :var-type "function",
   :arglists ([t & {:keys [target], :or {target "file"}, :as opts}]),
   :doc
   "Create and output an exception report for a Throwable to target.\n\nOptions:\n  :target - \"file\" (default), \"stderr\", \"none\"\n\nIf file is specified but cannot be written, falls back to stderr.",
   :namespace "clojure.main",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/report-error"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj",
   :added "1.3",
   :name "root-cause",
   :file "src/clj/clojure/main.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj#L35",
   :line 35,
   :var-type "function",
   :arglists ([t]),
   :doc
   "Returns the initial cause of an exception or error by peeling off all of\nits wrappers",
   :namespace "clojure.main",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/root-cause"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj",
   :name "skip-if-eol",
   :file "src/clj/clojure/main.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj#L108",
   :line 108,
   :var-type "function",
   :arglists ([s]),
   :doc
   "If the next character on stream s is a newline, skips it, otherwise\nleaves the stream untouched. Returns :line-start, :stream-end, or :body\nto indicate the relative location of the next character on s. The stream\nmust either be an instance of LineNumberingPushbackReader or duplicate\nits behavior of both supporting .unread and collapsing all of CR, LF, and\nCRLF to a single \\newline.",
   :namespace "clojure.main",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/skip-if-eol"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj",
   :name "skip-whitespace",
   :file "src/clj/clojure/main.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj#L122",
   :line 122,
   :var-type "function",
   :arglists ([s]),
   :doc
   "Skips whitespace characters on stream s. Returns :line-start, :stream-end,\nor :body to indicate the relative location of the next character on s.\nInterprets comma as whitespace and semicolon as comment to end of line.\nDoes not interpret #! as comment to end of line because only one\ncharacter of lookahead is available. The stream must either be an\ninstance of LineNumberingPushbackReader or duplicate its behavior of both\nsupporting .unread and collapsing all of CR, LF, and CRLF to a single\n\\newline.",
   :namespace "clojure.main",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/skip-whitespace"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj",
   :added "1.3",
   :name "stack-element-str",
   :file "src/clj/clojure/main.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj#L62",
   :line 62,
   :var-type "function",
   :arglists ([el]),
   :doc
   "Returns a (possibly unmunged) string representation of a StackTraceElement",
   :namespace "clojure.main",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/stack-element-str"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj",
   :name "with-bindings",
   :file "src/clj/clojure/main.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj#L77",
   :line 77,
   :var-type "macro",
   :arglists ([& body]),
   :doc
   "Executes body in the context of thread-local bindings for several vars\nthat often need to be set!: *ns* *warn-on-reflection* *math-context*\n*print-meta* *print-length* *print-level* *compile-path*\n*command-line-args* *1 *2 *3 *e",
   :namespace "clojure.main",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/with-bindings"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj",
   :name "with-read-known",
   :file "src/clj/clojure/main.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/38524061dcb14c598c239be87184b3378ffc5bac/src/clj/clojure/main.clj#L361",
   :line 361,
   :var-type "macro",
   :arglists ([& body]),
   :doc
   "Evaluates body with *read-eval* set to a \"known\" value,\ni.e. substituting true for :unknown if necessary.",
   :namespace "clojure.main",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.main-api.html#clojure.main/with-read-known"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "E",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L24",
   :line 24,
   :var-type "var",
   :arglists nil,
   :doc
   "Constant for e, the base for natural logarithms.\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#E",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/E"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "IEEE-remainder",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L184",
   :line 184,
   :var-type "function",
   :arglists ([dividend divisor]),
   :doc
   "Returns the remainder per IEEE 754 such that\n  remainder = dividend - divisor * n\nwhere n is the integer closest to the exact value of dividend / divisor.\nIf two integers are equally close, then n is the even one.\nIf the remainder is zero, sign will match dividend.\nIf dividend or divisor is ##NaN, or dividend is ##Inf or ##-Inf, or divisor is zero => ##NaN\nIf dividend is finite and divisor is infinite => dividend\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#IEEEremainder-double-double-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/IEEE-remainder"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "PI",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L33",
   :line 33,
   :var-type "var",
   :arglists nil,
   :doc
   "Constant for pi, the ratio of the circumference of a circle to its diameter.\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#PI",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/PI"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "acos",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L85",
   :line 85,
   :var-type "function",
   :arglists ([a]),
   :doc
   "Returns the arc cosine of a, in the range 0.0 to pi.\nIf a is ##NaN or |a|>1 => ##NaN\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#acos-double-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/acos"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "add-exact",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L277",
   :line 277,
   :var-type "function",
   :arglists ([x y]),
   :doc
   "Returns the sum of x and y, throws ArithmeticException on overflow.\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#addExact-long-long-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/add-exact"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "asin",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L74",
   :line 74,
   :var-type "function",
   :arglists ([a]),
   :doc
   "Returns the arc sine of an angle, in the range -pi/2 to pi/2.\nIf a is ##NaN or |a|>1 => ##NaN\nIf a is zero => zero with the same sign as a\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#asin-double-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/asin"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "atan",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L95",
   :line 95,
   :var-type "function",
   :arglists ([a]),
   :doc
   "Returns the arc tangent of a, in the range of -pi/2 to pi/2.\nIf a is ##NaN => ##NaN\nIf a is zero => zero with the same sign as a\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#atan-double-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/atan"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "atan2",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L233",
   :line 233,
   :var-type "function",
   :arglists ([y x]),
   :doc
   "Returns the angle theta from the conversion of rectangular coordinates (x, y) to polar coordinates (r, theta).\nComputes the phase theta by computing an arc tangent of y/x in the range of -pi to pi.\nFor more details on special cases, see:\nhttps://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#atan2-double-double-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/atan2"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "cbrt",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L172",
   :line 172,
   :var-type "function",
   :arglists ([a]),
   :doc
   "Returns the cube root of a.\nIf a is ##NaN => ##NaN\nIf a is ##Inf or ##-Inf => a\nIf a is zero => zero with sign matching a\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#cbrt-double-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/cbrt"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "ceil",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L199",
   :line 199,
   :var-type "function",
   :arglists ([a]),
   :doc
   "Returns the smallest double greater than or equal to a, and equal to a\nmathematical integer.\nIf a is ##NaN or ##Inf or ##-Inf or already equal to an integer => a\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#ceil-double-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/ceil"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "copy-sign",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L448",
   :line 448,
   :var-type "function",
   :arglists ([magnitude sign]),
   :doc
   "Returns a double with the magnitude of the first argument and the sign of\nthe second.\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#copySign-double-double-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/copy-sign"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "cos",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L53",
   :line 53,
   :var-type "function",
   :arglists ([a]),
   :doc
   "Returns the cosine of an angle.\nIf a is ##NaN, ##-Inf, ##Inf => ##NaN\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#cos-double-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/cos"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "cosh",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L385",
   :line 385,
   :var-type "function",
   :arglists ([x]),
   :doc
   "Returns the hyperbolic cosine of x, (e^x + e^-x)/2.\nIf x is ##NaN => ##NaN\nIf x is ##Inf or ##-Inf => ##Inf\nIf x is zero => 1.0\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#cosh-double-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/cosh"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "decrement-exact",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L313",
   :line 313,
   :var-type "function",
   :arglists ([a]),
   :doc
   "Returns a decremented by 1, throws ArithmeticException on overflow.\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#decrementExact-long-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/decrement-exact"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "exp",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L124",
   :line 124,
   :var-type "function",
   :arglists ([a]),
   :doc
   "Returns Euler's number e raised to the power of a.\nIf a is ##NaN => ##NaN\nIf a is ##Inf => ##Inf\nIf a is ##-Inf => +0.0\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#exp-double-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/exp"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "expm1",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L421",
   :line 421,
   :var-type "function",
   :arglists ([x]),
   :doc
   "Returns e^x - 1. Near 0, expm1(x)+1 is more accurate to e^x than exp(x).\nIf x is ##NaN => ##NaN\nIf x is ##Inf => #Inf\nIf x is ##-Inf => -1.0\nIf x is zero => x\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#expm1-double-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/expm1"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "floor",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L210",
   :line 210,
   :var-type "function",
   :arglists ([a]),
   :doc
   "Returns the largest double less than or equal to a, and equal to a\nmathematical integer.\nIf a is ##NaN or ##Inf or ##-Inf or already equal to an integer => a\nIf a is less than zero but greater than -1.0 => -0.0\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#floor-double-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/floor"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "floor-div",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L331",
   :line 331,
   :var-type "function",
   :arglists ([x y]),
   :doc
   "Integer division that rounds to negative infinity (as opposed to zero).\nThe special case (floorDiv Long/MIN_VALUE -1) overflows and returns Long/MIN_VALUE.\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#floorDiv-long-long-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/floor-div"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "floor-mod",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L341",
   :line 341,
   :var-type "function",
   :arglists ([x y]),
   :doc
   "Integer modulus x - (floorDiv(x, y) * y). Sign matches y and is in the\nrange -|y| < r < |y|.\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#floorMod-long-long-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/floor-mod"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "get-exponent",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L458",
   :line 458,
   :var-type "function",
   :arglists ([d]),
   :doc
   "Returns the exponent of d.\nIf d is ##NaN, ##Inf, ##-Inf => Double/MAX_EXPONENT + 1\nIf d is zero or subnormal => Double/MIN_EXPONENT - 1\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#getExponent-double-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/get-exponent"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "hypot",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L410",
   :line 410,
   :var-type "function",
   :arglists ([x y]),
   :doc
   "Returns sqrt(x^2 + y^2) without intermediate underflow or overflow.\nIf x or y is ##Inf or ##-Inf => ##Inf\nIf x or y is ##NaN and neither is ##Inf or ##-Inf => ##NaN\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#hypot-double-double-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/hypot"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "increment-exact",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L304",
   :line 304,
   :var-type "function",
   :arglists ([a]),
   :doc
   "Returns a incremented by 1, throws ArithmeticException on overflow.\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#incrementExact-long-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/increment-exact"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "log",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L136",
   :line 136,
   :var-type "function",
   :arglists ([a]),
   :doc
   "Returns the natural logarithm (base e) of a.\nIf a is ##NaN or negative => ##NaN\nIf a is ##Inf => ##Inf\nIf a is zero => ##-Inf\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#log-double-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/log"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "log10",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L148",
   :line 148,
   :var-type "function",
   :arglists ([a]),
   :doc
   "Returns the logarithm (base 10) of a.\nIf a is ##NaN or negative => ##NaN\nIf a is ##Inf => ##Inf\nIf a is zero => ##-Inf\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#log10-double-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/log10"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "log1p",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L434",
   :line 434,
   :var-type "function",
   :arglists ([x]),
   :doc
   "Returns ln(1+x). For small values of x, log1p(x) is more accurate than\nlog(1.0+x).\nIf x is ##NaN or < -1 => ##NaN\nIf x is ##Inf => ##Inf\nIf x is -1 => ##-Inf\nIf x is 0 => 0 with sign matching x\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#log1p-double-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/log1p"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "multiply-exact",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L295",
   :line 295,
   :var-type "function",
   :arglists ([x y]),
   :doc
   "Returns the product of x and y, throws ArithmeticException on overflow.\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#multiplyExact-long-long-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/multiply-exact"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "negate-exact",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L322",
   :line 322,
   :var-type "function",
   :arglists ([a]),
   :doc
   "Returns the negation of a, throws ArithmeticException on overflow.\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#negateExact-long-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/negate-exact"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "next-after",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L469",
   :line 469,
   :var-type "function",
   :arglists ([start direction]),
   :doc
   "Returns the adjacent floating point number to start in the direction of\nthe second argument. If the arguments are equal, the second is returned.\nIf either arg is #NaN => #NaN\nIf both arguments are signed zeros => direction\nIf start is +-Double/MIN_VALUE and direction would cause a smaller magnitude\n  => zero with sign matching start\nIf start is ##Inf or ##-Inf and direction would cause a smaller magnitude\n  => Double/MAX_VALUE with same sign as start\nIf start is equal to +=Double/MAX_VALUE and direction would cause a larger magnitude\n  => ##Inf or ##-Inf with sign matching start\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#nextAfter-double-double-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/next-after"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "next-down",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L499",
   :line 499,
   :var-type "function",
   :arglists ([d]),
   :doc
   "Returns the adjacent double of d in the direction of ##-Inf.\nIf d is ##NaN => ##NaN\nIf d is ##-Inf => ##-Inf\nIf d is zero => -Double/MIN_VALUE\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#nextDown-double-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/next-down"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "next-up",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L487",
   :line 487,
   :var-type "function",
   :arglists ([d]),
   :doc
   "Returns the adjacent double of d in the direction of ##Inf.\nIf d is ##NaN => ##NaN\nIf d is ##Inf => ##Inf\nIf d is zero => Double/MIN_VALUE\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#nextUp-double-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/next-up"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "pow",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L244",
   :line 244,
   :var-type "function",
   :arglists ([a b]),
   :doc
   "Returns the value of a raised to the power of b.\nFor more details on special cases, see:\nhttps://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#pow-double-double-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/pow"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "random",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L267",
   :line 267,
   :var-type "function",
   :arglists ([]),
   :doc
   "Returns a positive double between 0.0 and 1.0, chosen pseudorandomly with\napproximately random distribution.\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#random--",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/random"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "rint",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L222",
   :line 222,
   :var-type "function",
   :arglists ([a]),
   :doc
   "Returns the double closest to a and equal to a mathematical integer.\nIf two values are equally close, return the even one.\nIf a is ##NaN or ##Inf or ##-Inf or zero => a\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#rint-double-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/rint"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "round",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L254",
   :line 254,
   :var-type "function",
   :arglists ([a]),
   :doc
   "Returns the closest long to a. If equally close to two values, return the one\ncloser to ##Inf.\nIf a is ##NaN => 0\nIf a is ##-Inf or < Long/MIN_VALUE => Long/MIN_VALUE\nIf a is ##Inf or > Long/MAX_VALUE => Long/MAX_VALUE\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#round-double-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/round"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "scalb",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L511",
   :line 511,
   :var-type "function",
   :arglists ([d scaleFactor]),
   :doc
   "Returns d * 2^scaleFactor, scaling by a factor of 2. If the exponent\nis between Double/MIN_EXPONENT and Double/MAX_EXPONENT, the answer is exact.\nIf d is ##NaN => ##NaN\nIf d is ##Inf or ##-Inf => ##Inf or ##-Inf respectively\nIf d is zero => zero of same sign as d\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#nextDown-double-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/scalb"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "signum",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L364",
   :line 364,
   :var-type "function",
   :arglists ([d]),
   :doc
   "Returns the signum function of d - zero for zero, 1.0 if >0, -1.0 if <0.\nIf d is ##NaN => ##NaN\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#signum-double-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/signum"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "sin",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L42",
   :line 42,
   :var-type "function",
   :arglists ([a]),
   :doc
   "Returns the sine of an angle.\nIf a is ##NaN, ##-Inf, ##Inf => ##NaN\nIf a is zero => zero with the same sign as a\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#sin-double-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/sin"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "sinh",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L374",
   :line 374,
   :var-type "function",
   :arglists ([x]),
   :doc
   "Returns the hyperbolic sine of x, (e^x - e^-x)/2.\nIf x is ##NaN => ##NaN\nIf x is ##Inf or ##-Inf or zero => x\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#sinh-double-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/sinh"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "sqrt",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L160",
   :line 160,
   :var-type "function",
   :arglists ([a]),
   :doc
   "Returns the positive square root of a.\nIf a is ##NaN or negative => ##NaN\nIf a is ##Inf => ##Inf\nIf a is zero => a\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#sqrt-double-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/sqrt"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "subtract-exact",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L286",
   :line 286,
   :var-type "function",
   :arglists ([x y]),
   :doc
   "Returns the difference of x and y, throws ArithmeticException on overflow.\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#subtractExact-long-long-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/subtract-exact"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "tan",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L63",
   :line 63,
   :var-type "function",
   :arglists ([a]),
   :doc
   "Returns the tangent of an angle.\nIf a is ##NaN, ##-Inf, ##Inf => ##NaN\nIf a is zero => zero with the same sign as a\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#tan-double-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/tan"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "tanh",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L397",
   :line 397,
   :var-type "function",
   :arglists ([x]),
   :doc
   "Returns the hyperbolic tangent of x, sinh(x)/cosh(x).\nIf x is ##NaN => ##NaN\nIf x is zero => zero, with same sign\nIf x is ##Inf => +1.0\nIf x is ##-Inf => -1.0\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#tanh-double-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/tanh"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "to-degrees",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L115",
   :line 115,
   :var-type "function",
   :arglists ([r]),
   :doc
   "Converts an angle in radians to an approximate equivalent angle in degrees.\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#toDegrees-double-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/to-degrees"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "to-radians",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L106",
   :line 106,
   :var-type "function",
   :arglists ([deg]),
   :doc
   "Converts an angle in degrees to an approximate equivalent angle in radians.\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#toRadians-double-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/to-radians"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj",
   :added "1.11",
   :name "ulp",
   :file "src/clj/clojure/math.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/304d7c6a81bd7d8511e9ef3d89dc199b1464afaa/src/clj/clojure/math.clj#L351",
   :line 351,
   :var-type "function",
   :arglists ([d]),
   :doc
   "Returns the size of an ulp (unit in last place) for d.\nIf d is ##NaN => ##NaN\nIf d is ##Inf or ##-Inf => ##Inf\nIf d is zero => Double/MIN_VALUE\nIf d is +/- Double/MAX_VALUE => 2^971\nSee: https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html#ulp-double-",
   :namespace "clojure.math",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.math-api.html#clojure.math/ulp"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj",
   :added "1.2",
   :name "*print-base*",
   :file "src/clj/clojure/pprint/pprint_base.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj#L87",
   :dynamic true,
   :line 87,
   :var-type "var",
   :arglists nil,
   :doc "The base to use for printing integers and rationals.",
   :namespace "clojure.pprint",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/*print-base*"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj",
   :added "1.2",
   :name "*print-miser-width*",
   :file "src/clj/clojure/pprint/pprint_base.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj#L47",
   :dynamic true,
   :line 47,
   :var-type "var",
   :arglists nil,
   :doc
   "The column at which to enter miser style. Depending on the dispatch table, \nmiser style add newlines in more places to try to keep lines short allowing for further \nlevels of nesting.",
   :namespace "clojure.pprint",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/*print-miser-width*"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj",
   :added "1.2",
   :name "*print-pprint-dispatch*",
   :file "src/clj/clojure/pprint/pprint_base.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj#L34",
   :dynamic true,
   :line 34,
   :var-type "multimethod",
   :arglists nil,
   :doc
   "The pretty print dispatch function. Use with-pprint-dispatch or set-pprint-dispatch\nto modify.",
   :namespace "clojure.pprint",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/*print-pprint-dispatch*"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj",
   :added "1.2",
   :name "*print-pretty*",
   :file "src/clj/clojure/pprint/pprint_base.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj#L30",
   :dynamic true,
   :line 30,
   :var-type "var",
   :arglists nil,
   :doc "Bind to true if you want write to use pretty printing",
   :namespace "clojure.pprint",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/*print-pretty*"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj",
   :added "1.2",
   :name "*print-radix*",
   :file "src/clj/clojure/pprint/pprint_base.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj#L80",
   :dynamic true,
   :line 80,
   :var-type "var",
   :arglists nil,
   :doc
   "Print a radix specifier in front of integers and rationals. If *print-base* is 2, 8, \nor 16, then the radix specifier used is #b, #o, or #x, respectively. Otherwise the \nradix specifier is in the form #XXr where XX is the decimal value of *print-base* ",
   :namespace "clojure.pprint",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/*print-radix*"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj",
   :added "1.2",
   :name "*print-right-margin*",
   :file "src/clj/clojure/pprint/pprint_base.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj#L40",
   :dynamic true,
   :line 40,
   :var-type "var",
   :arglists nil,
   :doc
   "Pretty printing will try to avoid anything going beyond this column.\nSet it to nil to have pprint let the line be arbitrarily long. This will ignore all \nnon-mandatory newlines.",
   :namespace "clojure.pprint",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/*print-right-margin*"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj",
   :added "1.2",
   :name "*print-suppress-namespaces*",
   :file "src/clj/clojure/pprint/pprint_base.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj#L72",
   :dynamic true,
   :line 72,
   :var-type "var",
   :arglists nil,
   :doc
   "Don't print namespaces with symbols. This is particularly useful when \npretty printing the results of macro expansions",
   :namespace "clojure.pprint",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/*print-suppress-namespaces*"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/fb916808669ef65dce5dfe58e23d4a902253ca55/src/clj/clojure/pprint/cl_format.clj",
   :added "1.2",
   :name "cl-format",
   :file "src/clj/clojure/pprint/cl_format.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/fb916808669ef65dce5dfe58e23d4a902253ca55/src/clj/clojure/pprint/cl_format.clj#L27",
   :line 27,
   :var-type "function",
   :arglists ([writer format-in & args]),
   :doc
   "An implementation of a Common Lisp compatible format function. cl-format formats its\narguments to an output stream or string based on the format control string given. It \nsupports sophisticated formatting of structured data.\n\nWriter is an instance of java.io.Writer, true to output to *out* or nil to output \nto a string, format-in is the format control string and the remaining arguments \nare the data to be formatted.\n\nThe format control string is a string to be output with embedded 'format directives' \ndescribing how to format the various arguments passed in.\n\nIf writer is nil, cl-format returns the formatted result string. Otherwise, cl-format \nreturns nil.\n\nFor example:\n (let [results [46 38 22]]\n        (cl-format true \"There ~[are~;is~:;are~]~:* ~d result~:p: ~{~d~^, ~}~%\" \n                   (count results) results))\n\nPrints to *out*:\n There are 3 results: 46, 38, 22\n\nDetailed documentation on format control strings is available in the \"Common Lisp the \nLanguage, 2nd edition\", Chapter 22 (available online at:\nhttp://www.cs.cmu.edu/afs/cs.cmu.edu/project/ai-repository/ai/html/cltl/clm/node200.html#SECTION002633000000000000000) \nand in the Common Lisp HyperSpec at \nhttp://www.lispworks.com/documentation/HyperSpec/Body/22_c.htm",
   :namespace "clojure.pprint",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/cl-format"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/af9f2ed34326a5f590f245155c73958e7a2dc17f/src/clj/clojure/pprint/dispatch.clj",
   :added "1.2",
   :name "code-dispatch",
   :file "src/clj/clojure/pprint/dispatch.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/af9f2ed34326a5f590f245155c73958e7a2dc17f/src/clj/clojure/pprint/dispatch.clj#L476",
   :line 476,
   :var-type "multimethod",
   :arglists [[object]],
   :doc
   "The pretty print dispatch function for pretty printing Clojure code.",
   :namespace "clojure.pprint",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/code-dispatch"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/fb916808669ef65dce5dfe58e23d4a902253ca55/src/clj/clojure/pprint/cl_format.clj",
   :added "1.2",
   :name "formatter",
   :file "src/clj/clojure/pprint/cl_format.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/fb916808669ef65dce5dfe58e23d4a902253ca55/src/clj/clojure/pprint/cl_format.clj#L1916",
   :line 1916,
   :var-type "macro",
   :arglists ([format-in]),
   :doc
   "Makes a function which can directly run format-in. The function is\nfn [stream & args] ... and returns nil unless the stream is nil (meaning \noutput to a string) in which case it returns the resulting string.\n\nformat-in can be either a control string or a previously compiled format.",
   :namespace "clojure.pprint",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/formatter"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/fb916808669ef65dce5dfe58e23d4a902253ca55/src/clj/clojure/pprint/cl_format.clj",
   :added "1.2",
   :name "formatter-out",
   :file "src/clj/clojure/pprint/cl_format.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/fb916808669ef65dce5dfe58e23d4a902253ca55/src/clj/clojure/pprint/cl_format.clj#L1936",
   :line 1936,
   :var-type "macro",
   :arglists ([format-in]),
   :doc
   "Makes a function which can directly run format-in. The function is\nfn [& args] ... and returns nil. This version of the formatter macro is\ndesigned to be used with *out* set to an appropriate Writer. In particular,\nthis is meant to be used as part of a pretty printer dispatch method.\n\nformat-in can be either a control string or a previously compiled format.",
   :namespace "clojure.pprint",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/formatter-out"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/fb916808669ef65dce5dfe58e23d4a902253ca55/src/clj/clojure/pprint/cl_format.clj",
   :added "1.2",
   :name "fresh-line",
   :file "src/clj/clojure/pprint/cl_format.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/fb916808669ef65dce5dfe58e23d4a902253ca55/src/clj/clojure/pprint/cl_format.clj#L1245",
   :line 1245,
   :var-type "function",
   :arglists ([]),
   :doc
   "Make a newline if *out* is not already at the beginning of the line. If *out* is\nnot a pretty writer (which keeps track of columns), this function always outputs a newline.",
   :namespace "clojure.pprint",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/fresh-line"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/fb916808669ef65dce5dfe58e23d4a902253ca55/src/clj/clojure/pprint/cl_format.clj",
   :added "1.2",
   :name "get-pretty-writer",
   :file "src/clj/clojure/pprint/cl_format.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/fb916808669ef65dce5dfe58e23d4a902253ca55/src/clj/clojure/pprint/cl_format.clj#L1203",
   :line 1203,
   :var-type "function",
   :arglists ([writer]),
   :doc
   "Returns the java.io.Writer passed in wrapped in a pretty writer proxy, unless it's \nalready a pretty writer. Generally, it is unnecessary to call this function, since pprint,\nwrite, and cl-format all call it if they need to. However if you want the state to be \npreserved across calls, you will want to wrap them with this. \n\nFor example, when you want to generate column-aware output with multiple calls to cl-format, \ndo it like in this example:\n\n    (defn print-table [aseq column-width]\n      (binding [*out* (get-pretty-writer *out*)]\n        (doseq [row aseq]\n          (doseq [col row]\n            (cl-format true \"~4D~7,vT\" col column-width))\n          (prn))))\n\nNow when you run:\n\n    user> (print-table (map #(vector % (* % %) (* % % %)) (range 1 11)) 8)\n\nIt prints a table of squares and cubes for the numbers from 1 to 10:\n\n       1      1       1    \n       2      4       8    \n       3      9      27    \n       4     16      64    \n       5     25     125    \n       6     36     216    \n       7     49     343    \n       8     64     512    \n       9     81     729    \n      10    100    1000",
   :namespace "clojure.pprint",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/get-pretty-writer"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj",
   :added "1.2",
   :name "pp",
   :file "src/clj/clojure/pprint/pprint_base.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj#L254",
   :line 254,
   :var-type "macro",
   :arglists ([]),
   :doc
   "A convenience macro that pretty prints the last thing output. This is\nexactly equivalent to (pprint *1).",
   :namespace "clojure.pprint",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/pp"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj",
   :added "1.2",
   :name "pprint",
   :file "src/clj/clojure/pprint/pprint_base.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj#L241",
   :line 241,
   :var-type "function",
   :arglists ([object] [object writer]),
   :doc
   "Pretty print object to the optional output writer. If the writer is not provided, \nprint the object to the currently bound value of *out*.",
   :namespace "clojure.pprint",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/pprint"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj",
   :added "1.2",
   :name "pprint-indent",
   :file "src/clj/clojure/pprint/pprint_base.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj#L341",
   :line 341,
   :var-type "function",
   :arglists ([relative-to n]),
   :doc
   "Create an indent at this point in the pretty printing stream. This defines how \nfollowing lines are indented. relative-to can be either :block or :current depending \nwhether the indent should be computed relative to the start of the logical block or\nthe current column position. n is an offset. \n\nThis function is intended for use when writing custom dispatch functions.\n\nOutput is sent to *out* which must be a pretty printing writer.",
   :namespace "clojure.pprint",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/pprint-indent"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj",
   :added "1.2",
   :name "pprint-logical-block",
   :file "src/clj/clojure/pprint/pprint_base.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj#L302",
   :line 302,
   :var-type "macro",
   :arglists [[options* body]],
   :doc
   "Execute the body as a pretty printing logical block with output to *out* which \nmust be a pretty printing writer. When used from pprint or cl-format, this can be \nassumed. \n\nThis function is intended for use when writing custom dispatch functions.\n\nBefore the body, the caller can optionally specify options: :prefix, :per-line-prefix, \nand :suffix.",
   :namespace "clojure.pprint",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/pprint-logical-block"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj",
   :added "1.2",
   :name "pprint-newline",
   :file "src/clj/clojure/pprint/pprint_base.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj#L329",
   :line 329,
   :var-type "function",
   :arglists ([kind]),
   :doc
   "Print a conditional newline to a pretty printing stream. kind specifies if the \nnewline is :linear, :miser, :fill, or :mandatory. \n\nThis function is intended for use when writing custom dispatch functions.\n\nOutput is sent to *out* which must be a pretty printing writer.",
   :namespace "clojure.pprint",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/pprint-newline"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj",
   :added "1.2",
   :name "pprint-tab",
   :file "src/clj/clojure/pprint/pprint_base.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj#L356",
   :line 356,
   :var-type "function",
   :arglists ([kind colnum colinc]),
   :doc
   "Tab at this point in the pretty printing stream. kind specifies whether the tab\nis :line, :section, :line-relative, or :section-relative. \n\nColnum and colinc specify the target column and the increment to move the target\nforward if the output is already past the original target.\n\nThis function is intended for use when writing custom dispatch functions.\n\nOutput is sent to *out* which must be a pretty printing writer.\n\nTHIS FUNCTION IS NOT YET IMPLEMENTED.",
   :namespace "clojure.pprint",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/pprint-tab"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj",
   :added "1.3",
   :name "print-length-loop",
   :file "src/clj/clojure/pprint/pprint_base.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj#L391",
   :line 391,
   :var-type "macro",
   :arglists ([bindings & body]),
   :doc
   "A version of loop that iterates at most *print-length* times. This is designed \nfor use in pretty-printer dispatch functions.",
   :namespace "clojure.pprint",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/print-length-loop"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/93d13d0c0671130b329863570080c72799563ac7/src/clj/clojure/pprint/print_table.clj",
   :added "1.3",
   :name "print-table",
   :file "src/clj/clojure/pprint/print_table.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/93d13d0c0671130b329863570080c72799563ac7/src/clj/clojure/pprint/print_table.clj#L11",
   :line 11,
   :var-type "function",
   :arglists ([ks rows] [rows]),
   :doc
   "Prints a collection of maps in a textual table. Prints table headings\nks, and then a line of output for each row, corresponding to the keys\nin ks. If ks are not specified, use the keys of the first item in rows.",
   :namespace "clojure.pprint",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/print-table"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj",
   :added "1.2",
   :name "set-pprint-dispatch",
   :file "src/clj/clojure/pprint/pprint_base.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj#L260",
   :line 260,
   :var-type "function",
   :arglists ([function]),
   :doc
   "Set the pretty print dispatch function to a function matching (fn [obj] ...)\nwhere obj is the object to pretty print. That function will be called with *out* set\nto a pretty printing writer to which it should do its printing.\n\nFor example functions, see simple-dispatch and code-dispatch in \nclojure.pprint.dispatch.clj.",
   :namespace "clojure.pprint",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/set-pprint-dispatch"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/af9f2ed34326a5f590f245155c73958e7a2dc17f/src/clj/clojure/pprint/dispatch.clj",
   :added "1.2",
   :name "simple-dispatch",
   :file "src/clj/clojure/pprint/dispatch.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/af9f2ed34326a5f590f245155c73958e7a2dc17f/src/clj/clojure/pprint/dispatch.clj#L174",
   :line 174,
   :var-type "multimethod",
   :arglists [[object]],
   :doc
   "The pretty print dispatch function for simple data structure format.",
   :namespace "clojure.pprint",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/simple-dispatch"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj",
   :added "1.2",
   :name "with-pprint-dispatch",
   :file "src/clj/clojure/pprint/pprint_base.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj#L274",
   :line 274,
   :var-type "macro",
   :arglists ([function & body]),
   :doc
   "Execute body with the pretty print dispatch function bound to function.",
   :namespace "clojure.pprint",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/with-pprint-dispatch"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj",
   :added "1.2",
   :name "write",
   :file "src/clj/clojure/pprint/pprint_base.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj#L197",
   :line 197,
   :var-type "function",
   :arglists ([object & kw-args]),
   :doc
   "Write an object subject to the current bindings of the printer control variables.\nUse the kw-args argument to override individual variables for this call (and any \nrecursive calls). Returns the string result if :stream is nil or nil otherwise.\n\nThe following keyword arguments can be passed with values:\n  Keyword              Meaning                              Default value\n  :stream              Writer for output or nil             true (indicates *out*)\n  :base                Base to use for writing rationals    Current value of *print-base*\n  :circle*             If true, mark circular structures    Current value of *print-circle*\n  :length              Maximum elements to show in sublists Current value of *print-length*\n  :level               Maximum depth                        Current value of *print-level*\n  :lines*              Maximum lines of output              Current value of *print-lines*\n  :miser-width         Width to enter miser mode            Current value of *print-miser-width*\n  :dispatch            The pretty print dispatch function   Current value of *print-pprint-dispatch*\n  :pretty              If true, do pretty printing          Current value of *print-pretty*\n  :radix               If true, prepend a radix specifier   Current value of *print-radix*\n  :readably*           If true, print readably              Current value of *print-readably*\n  :right-margin        The column for the right margin      Current value of *print-right-margin*\n  :suppress-namespaces If true, no namespaces in symbols    Current value of *print-suppress-namespaces*\n\n  * = not yet supported",
   :namespace "clojure.pprint",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/write"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj",
   :added "1.2",
   :name "write-out",
   :file "src/clj/clojure/pprint/pprint_base.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/0a6810ab3484b5be0afe4f505cd724eb5c974a09/src/clj/clojure/pprint/pprint_base.clj#L171",
   :line 171,
   :var-type "function",
   :arglists ([object]),
   :doc
   "Write an object to *out* subject to the current bindings of the printer control \nvariables. Use the kw-args argument to override individual variables for this call (and \nany recursive calls).\n\n*out* must be a PrettyWriter if pretty printing is enabled. This is the responsibility\nof the caller.\n\nThis method is primarily intended for use by pretty print dispatch functions that \nalready know that the pretty printer will have set up their environment appropriately.\nNormal library clients should use the standard \"write\" interface. ",
   :namespace "clojure.pprint",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.pprint-api.html#clojure.pprint/write-out"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj",
   :name "->AsmReflector",
   :file "src/clj/clojure/reflect/java.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj#L208",
   :line 208,
   :var-type "function",
   :arglists ([class-resolver]),
   :doc
   "Positional factory function for class clojure.reflect.AsmReflector.",
   :namespace "clojure.reflect",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/->AsmReflector"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj",
   :name "->Constructor",
   :file "src/clj/clojure/reflect/java.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj#L115",
   :line 115,
   :var-type "function",
   :arglists
   ([name declaring-class parameter-types exception-types flags]),
   :doc
   "Positional factory function for class clojure.reflect.Constructor.",
   :namespace "clojure.reflect",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/->Constructor"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj",
   :name "->Field",
   :file "src/clj/clojure/reflect/java.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj#L154",
   :line 154,
   :var-type "function",
   :arglists ([name type declaring-class flags]),
   :doc "Positional factory function for class clojure.reflect.Field.",
   :namespace "clojure.reflect",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/->Field"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj",
   :name "->JavaReflector",
   :file "src/clj/clojure/reflect/java.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj#L178",
   :line 178,
   :var-type "function",
   :arglists ([classloader]),
   :doc
   "Positional factory function for class clojure.reflect.JavaReflector.",
   :namespace "clojure.reflect",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/->JavaReflector"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj",
   :name "->Method",
   :file "src/clj/clojure/reflect/java.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj#L134",
   :line 134,
   :var-type "function",
   :arglists
   ([name
     return-type
     declaring-class
     parameter-types
     exception-types
     flags]),
   :doc
   "Positional factory function for class clojure.reflect.Method.",
   :namespace "clojure.reflect",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/->Method"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj",
   :name "flag-descriptors",
   :file "src/clj/clojure/reflect/java.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj#L77",
   :line 77,
   :var-type "var",
   :arglists nil,
   :doc
   "The Java access bitflags, along with their friendly names and\nthe kinds of objects to which they can apply.",
   :namespace "clojure.reflect",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/flag-descriptors"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj",
   :name "map->Constructor",
   :file "src/clj/clojure/reflect/java.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj#L115",
   :line 115,
   :var-type "function",
   :arglists ([m#]),
   :doc
   "Factory function for class clojure.reflect.Constructor, taking a map of keywords to field values.",
   :namespace "clojure.reflect",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/map->Constructor"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj",
   :name "map->Field",
   :file "src/clj/clojure/reflect/java.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj#L154",
   :line 154,
   :var-type "function",
   :arglists ([m#]),
   :doc
   "Factory function for class clojure.reflect.Field, taking a map of keywords to field values.",
   :namespace "clojure.reflect",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/map->Field"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj",
   :name "map->Method",
   :file "src/clj/clojure/reflect/java.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj#L134",
   :line 134,
   :var-type "function",
   :arglists ([m#]),
   :doc
   "Factory function for class clojure.reflect.Method, taking a map of keywords to field values.",
   :namespace "clojure.reflect",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/map->Method"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/ee00807bac64d55dbc7ec49442d6376352b77200/src/clj/clojure/reflect.clj",
   :added "1.3",
   :name "reflect",
   :file "src/clj/clojure/reflect.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/ee00807bac64d55dbc7ec49442d6376352b77200/src/clj/clojure/reflect.clj#L115",
   :line 115,
   :var-type "function",
   :arglists ([obj & options]),
   :doc
   "Alpha - subject to change.\nReflect on the type of obj (or obj itself if obj is a class).\nReturn value and options are the same as for type-reflect. ",
   :namespace "clojure.reflect",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/reflect"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/ee00807bac64d55dbc7ec49442d6376352b77200/src/clj/clojure/reflect.clj",
   :added "1.3",
   :name "type-reflect",
   :file "src/clj/clojure/reflect.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/ee00807bac64d55dbc7ec49442d6376352b77200/src/clj/clojure/reflect.clj#L58",
   :line 58,
   :var-type "function",
   :arglists ([typeref & options]),
   :doc
   "Alpha - subject to change.\n Reflect on a typeref, returning a map with :bases, :flags, and\n:members. In the discussion below, names are always Clojure symbols.\n\n :bases            a set of names of the type's bases\n :flags            a set of keywords naming the boolean attributes\n                   of the type.\n :members          a set of the type's members. Each member is a map\n                   and can be a constructor, method, or field.\n\n Keys common to all members:\n :name             name of the type \n :declaring-class  name of the declarer\n :flags            keyword naming boolean attributes of the member\n\n Keys specific to constructors:\n :parameter-types  vector of parameter type names\n :exception-types  vector of exception type names\n\n Key specific to methods:\n :parameter-types  vector of parameter type names\n :exception-types  vector of exception type names\n :return-type      return type name\n\n Keys specific to fields:\n :type             type name\n\n Options:\n\n   :ancestors     in addition to the keys described above, also\n                  include an :ancestors key with the entire set of\n                  ancestors, and add all ancestor members to\n                  :members.\n   :reflector     implementation to use. Defaults to JavaReflector,\n                  AsmReflector is also an option.",
   :namespace "clojure.reflect",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/type-reflect"}
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
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj",
   :name "ClassResolver",
   :file "src/clj/clojure/reflect/java.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/b0e5056217454073288e1643cd19e44999f081b8/src/clj/clojure/reflect/java.clj#L196",
   :line 196,
   :var-type "protocol",
   :arglists nil,
   :doc nil,
   :namespace "clojure.reflect",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/ClassResolver"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/ee00807bac64d55dbc7ec49442d6376352b77200/src/clj/clojure/reflect.clj",
   :name "Reflector",
   :file "src/clj/clojure/reflect.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/ee00807bac64d55dbc7ec49442d6376352b77200/src/clj/clojure/reflect.clj#L44",
   :line 44,
   :var-type "protocol",
   :arglists nil,
   :doc "Protocol for reflection implementers.",
   :namespace "clojure.reflect",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/Reflector"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/ee00807bac64d55dbc7ec49442d6376352b77200/src/clj/clojure/reflect.clj",
   :name "TypeReference",
   :file "src/clj/clojure/reflect.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/ee00807bac64d55dbc7ec49442d6376352b77200/src/clj/clojure/reflect.clj#L48",
   :line 48,
   :var-type "protocol",
   :arglists nil,
   :doc
   "A TypeReference can be unambiguously converted to a type name on\nthe host platform.\n\nAll typerefs are normalized into symbols. If you need to\nnormalize a typeref yourself, call typesym.",
   :namespace "clojure.reflect",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/TypeReference"}
  {:raw-source-url nil,
   :name "resolve-class",
   :file nil,
   :source-url nil,
   :var-type "function",
   :arglists ([this name]),
   :doc
   "Given a class name, return that typeref's class bytes as an InputStream.",
   :namespace "clojure.reflect",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/resolve-class"}
  {:raw-source-url nil,
   :name "do-reflect",
   :file nil,
   :source-url nil,
   :var-type "function",
   :arglists ([reflector typeref]),
   :doc nil,
   :namespace "clojure.reflect",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/do-reflect"}
  {:raw-source-url nil,
   :name "typename",
   :file nil,
   :source-url nil,
   :var-type "function",
   :arglists ([o]),
   :doc
   "Returns Java name as returned by ASM getClassName, e.g. byte[], java.lang.String[]",
   :namespace "clojure.reflect",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.reflect-api.html#clojure.reflect/typename"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj",
   :name "apropos",
   :file "src/clj/clojure/repl.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj#L181",
   :line 181,
   :var-type "function",
   :arglists ([str-or-pattern]),
   :doc
   "Given a regular expression or stringable thing, return a seq of all\npublic definitions in all currently-loaded namespaces that match the\nstr-or-pattern.",
   :namespace "clojure.repl",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.repl-api.html#clojure.repl/apropos"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj",
   :added "1.3",
   :name "demunge",
   :file "src/clj/clojure/repl.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj#L207",
   :line 207,
   :var-type "function",
   :arglists ([fn-name]),
   :doc
   "Given a string representation of a fn class,\nas in a stack trace element, returns a readable version.",
   :namespace "clojure.repl",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.repl-api.html#clojure.repl/demunge"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj",
   :name "dir",
   :file "src/clj/clojure/repl.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj#L201",
   :line 201,
   :var-type "macro",
   :arglists ([nsname]),
   :doc "Prints a sorted directory of public vars in a namespace",
   :namespace "clojure.repl",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.repl-api.html#clojure.repl/dir"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj",
   :name "dir-fn",
   :file "src/clj/clojure/repl.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj#L195",
   :line 195,
   :var-type "function",
   :arglists ([ns]),
   :doc
   "Returns a sorted seq of symbols naming public vars in\na namespace or namespace alias. Looks for aliases in *ns*",
   :namespace "clojure.repl",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.repl-api.html#clojure.repl/dir-fn"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj",
   :added "1.0",
   :name "doc",
   :file "src/clj/clojure/repl.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj#L131",
   :line 131,
   :var-type "macro",
   :arglists ([name]),
   :doc
   "Prints documentation for a var or special form given its name,\nor for a spec if given a keyword",
   :namespace "clojure.repl",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.repl-api.html#clojure.repl/doc"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj",
   :added "1.0",
   :name "find-doc",
   :file "src/clj/clojure/repl.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj#L115",
   :line 115,
   :var-type "function",
   :arglists ([re-string-or-pattern]),
   :doc
   "Prints documentation for any var whose documentation or name\ncontains a match for re-string-or-pattern",
   :namespace "clojure.repl",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.repl-api.html#clojure.repl/find-doc"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj",
   :added "1.3",
   :name "pst",
   :file "src/clj/clojure/repl.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj#L240",
   :line 240,
   :var-type "function",
   :arglists ([] [e-or-depth] [e depth]),
   :doc
   "Prints a stack trace of the exception, to the depth requested. If none supplied, uses the root cause of the\nmost recent repl exception (*e), and a depth of 12.",
   :namespace "clojure.repl",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.repl-api.html#clojure.repl/pst"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj",
   :added "1.3",
   :name "root-cause",
   :file "src/clj/clojure/repl.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj#L214",
   :line 214,
   :var-type "function",
   :arglists ([t]),
   :doc
   "Returns the initial cause of an exception or error by peeling off all of\nits wrappers",
   :namespace "clojure.repl",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.repl-api.html#clojure.repl/root-cause"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj",
   :name "set-break-handler!",
   :file "src/clj/clojure/repl.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj#L279",
   :line 279,
   :var-type "function",
   :arglists ([] [f]),
   :doc
   "Register INT signal handler.  After calling this, Ctrl-C will cause\nthe given function f to be called with a single argument, the signal.\nUses thread-stopper if no function given.",
   :namespace "clojure.repl",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.repl-api.html#clojure.repl/set-break-handler!"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj",
   :name "source",
   :file "src/clj/clojure/repl.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj#L172",
   :line 172,
   :var-type "macro",
   :arglists ([n]),
   :doc
   "Prints the source code for the given symbol, if it can find it.\nThis requires that the symbol resolve to a Var defined in a\nnamespace for which the .clj is in the classpath.\n\nExample: (source filter)",
   :namespace "clojure.repl",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.repl-api.html#clojure.repl/source"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj",
   :name "source-fn",
   :file "src/clj/clojure/repl.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj#L147",
   :line 147,
   :var-type "function",
   :arglists ([x]),
   :doc
   "Returns a string of the source code for the given symbol, if it can\nfind it.  This requires that the symbol resolve to a Var defined in\na namespace for which the .clj is in the classpath.  Returns nil if\nit can't find the source.  For most REPL usage, 'source' is more\nconvenient.\n\nExample: (source-fn 'filter)",
   :namespace "clojure.repl",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.repl-api.html#clojure.repl/source-fn"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj",
   :added "1.3",
   :name "stack-element-str",
   :file "src/clj/clojure/repl.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj#L227",
   :line 227,
   :var-type "function",
   :arglists ([el]),
   :doc
   "Returns a (possibly unmunged) string representation of a StackTraceElement",
   :namespace "clojure.repl",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.repl-api.html#clojure.repl/stack-element-str"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj",
   :name "thread-stopper",
   :file "src/clj/clojure/repl.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/3b6256e654bf250ddfd01cdaa4be9f39a74c2de6/src/clj/clojure/repl.clj#L273",
   :line 273,
   :var-type "function",
   :arglists ([] [thread]),
   :doc
   "Returns a function that takes one arg and uses that as an exception message\nto stop the given thread.  Defaults to the current thread",
   :namespace "clojure.repl",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.repl-api.html#clojure.repl/thread-stopper"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj",
   :added "1.0",
   :name "difference",
   :file "src/clj/clojure/set.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj#L49",
   :line 49,
   :var-type "function",
   :arglists ([s1] [s1 s2] [s1 s2 & sets]),
   :doc
   "Return a set that is the first set without elements of the remaining sets",
   :namespace "clojure.set",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.set-api.html#clojure.set/difference"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj",
   :added "1.0",
   :name "index",
   :file "src/clj/clojure/set.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj#L95",
   :line 95,
   :var-type "function",
   :arglists ([xrel ks]),
   :doc
   "Returns a map of the distinct values of ks in the xrel mapped to a\nset of the maps in xrel with the corresponding values of ks.",
   :namespace "clojure.set",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.set-api.html#clojure.set/index"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj",
   :added "1.0",
   :name "intersection",
   :file "src/clj/clojure/set.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj#L33",
   :line 33,
   :var-type "function",
   :arglists ([s1] [s1 s2] [s1 s2 & sets]),
   :doc "Return a set that is the intersection of the input sets",
   :namespace "clojure.set",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.set-api.html#clojure.set/intersection"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj",
   :added "1.0",
   :name "join",
   :file "src/clj/clojure/set.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj#L115",
   :line 115,
   :var-type "function",
   :arglists ([xrel yrel] [xrel yrel km]),
   :doc
   "When passed 2 rels, returns the rel corresponding to the natural\njoin. When passed an additional keymap, joins on the corresponding\nkeys.",
   :namespace "clojure.set",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.set-api.html#clojure.set/join"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj",
   :added "1.0",
   :name "map-invert",
   :file "src/clj/clojure/set.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj#L106",
   :line 106,
   :var-type "function",
   :arglists ([m]),
   :doc "Returns the map with the vals mapped to the keys.",
   :namespace "clojure.set",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.set-api.html#clojure.set/map-invert"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj",
   :added "1.0",
   :name "project",
   :file "src/clj/clojure/set.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj#L72",
   :line 72,
   :var-type "function",
   :arglists ([xrel ks]),
   :doc
   "Returns a rel of the elements of xrel with only the keys in ks",
   :namespace "clojure.set",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.set-api.html#clojure.set/project"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj",
   :added "1.0",
   :name "rename",
   :file "src/clj/clojure/set.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj#L89",
   :line 89,
   :var-type "function",
   :arglists ([xrel kmap]),
   :doc
   "Returns a rel of the maps in xrel with the keys in kmap renamed to the vals in kmap",
   :namespace "clojure.set",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.set-api.html#clojure.set/rename"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj",
   :added "1.0",
   :name "rename-keys",
   :file "src/clj/clojure/set.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj#L78",
   :line 78,
   :var-type "function",
   :arglists ([map kmap]),
   :doc
   "Returns the map with the keys in kmap renamed to the vals in kmap",
   :namespace "clojure.set",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.set-api.html#clojure.set/rename-keys"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj",
   :added "1.0",
   :name "select",
   :file "src/clj/clojure/set.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj#L65",
   :line 65,
   :var-type "function",
   :arglists ([pred xset]),
   :doc "Returns a set of the elements for which pred is true",
   :namespace "clojure.set",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.set-api.html#clojure.set/select"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj",
   :added "1.2",
   :name "subset?",
   :file "src/clj/clojure/set.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj#L146",
   :line 146,
   :var-type "function",
   :arglists ([set1 set2]),
   :doc "Is set1 a subset of set2?",
   :namespace "clojure.set",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.set-api.html#clojure.set/subset?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj",
   :added "1.2",
   :name "superset?",
   :file "src/clj/clojure/set.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj#L154",
   :line 154,
   :var-type "function",
   :arglists ([set1 set2]),
   :doc "Is set1 a superset of set2?",
   :namespace "clojure.set",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.set-api.html#clojure.set/superset?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj",
   :added "1.0",
   :name "union",
   :file "src/clj/clojure/set.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/631c46ed98ed3bfefdb8a15080e004ab470b0bf4/src/clj/clojure/set.clj#L20",
   :line 20,
   :var-type "function",
   :arglists ([] [s1] [s1 s2] [s1 s2 & sets]),
   :doc "Return a set that is the union of the input sets",
   :namespace "clojure.set",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.set-api.html#clojure.set/union"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/dbb448f7709b20c392558e7d7871d1e9b28c9440/src/clj/clojure/stacktrace.clj",
   :added "1.1",
   :name "e",
   :file "src/clj/clojure/stacktrace.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/dbb448f7709b20c392558e7d7871d1e9b28c9440/src/clj/clojure/stacktrace.clj#L82",
   :line 82,
   :var-type "function",
   :arglists ([]),
   :doc
   "REPL utility.  Prints a brief stack trace for the root cause of the\nmost recent exception.",
   :namespace "clojure.stacktrace",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.stacktrace-api.html#clojure.stacktrace/e"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/dbb448f7709b20c392558e7d7871d1e9b28c9440/src/clj/clojure/stacktrace.clj",
   :added "1.1",
   :name "print-cause-trace",
   :file "src/clj/clojure/stacktrace.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/dbb448f7709b20c392558e7d7871d1e9b28c9440/src/clj/clojure/stacktrace.clj#L72",
   :line 72,
   :var-type "function",
   :arglists ([tr] [tr n]),
   :doc
   "Like print-stack-trace but prints chained exceptions (causes).",
   :namespace "clojure.stacktrace",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.stacktrace-api.html#clojure.stacktrace/print-cause-trace"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/dbb448f7709b20c392558e7d7871d1e9b28c9440/src/clj/clojure/stacktrace.clj",
   :added "1.1",
   :name "print-stack-trace",
   :file "src/clj/clojure/stacktrace.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/dbb448f7709b20c392558e7d7871d1e9b28c9440/src/clj/clojure/stacktrace.clj#L50",
   :line 50,
   :var-type "function",
   :arglists ([tr] [tr n]),
   :doc
   "Prints a Clojure-oriented stack trace of tr, a Throwable.\nPrints a maximum of n stack frames (default: unlimited).\nDoes not print chained exceptions (causes).",
   :namespace "clojure.stacktrace",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.stacktrace-api.html#clojure.stacktrace/print-stack-trace"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/dbb448f7709b20c392558e7d7871d1e9b28c9440/src/clj/clojure/stacktrace.clj",
   :added "1.1",
   :name "print-throwable",
   :file "src/clj/clojure/stacktrace.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/dbb448f7709b20c392558e7d7871d1e9b28c9440/src/clj/clojure/stacktrace.clj#L40",
   :line 40,
   :var-type "function",
   :arglists ([tr]),
   :doc
   "Prints the class and message of a Throwable. Prints the ex-data map\nif present.",
   :namespace "clojure.stacktrace",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.stacktrace-api.html#clojure.stacktrace/print-throwable"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/dbb448f7709b20c392558e7d7871d1e9b28c9440/src/clj/clojure/stacktrace.clj",
   :added "1.1",
   :name "print-trace-element",
   :file "src/clj/clojure/stacktrace.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/dbb448f7709b20c392558e7d7871d1e9b28c9440/src/clj/clojure/stacktrace.clj#L28",
   :line 28,
   :var-type "function",
   :arglists ([e]),
   :doc
   "Prints a Clojure-oriented view of one element in a stack trace.",
   :namespace "clojure.stacktrace",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.stacktrace-api.html#clojure.stacktrace/print-trace-element"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/dbb448f7709b20c392558e7d7871d1e9b28c9440/src/clj/clojure/stacktrace.clj",
   :added "1.1",
   :name "root-cause",
   :file "src/clj/clojure/stacktrace.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/dbb448f7709b20c392558e7d7871d1e9b28c9440/src/clj/clojure/stacktrace.clj#L20",
   :line 20,
   :var-type "function",
   :arglists ([tr]),
   :doc "Returns the last 'cause' Throwable in a chain of Throwables.",
   :namespace "clojure.stacktrace",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.stacktrace-api.html#clojure.stacktrace/root-cause"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :added "1.2",
   :name "blank?",
   :file "src/clj/clojure/string.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L288",
   :line 288,
   :var-type "function",
   :arglists ([s]),
   :doc "True if s is nil, empty, or contains only whitespace.",
   :namespace "clojure.string",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/blank?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :added "1.2",
   :name "capitalize",
   :file "src/clj/clojure/string.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L196",
   :line 196,
   :var-type "function",
   :arglists ([s]),
   :doc
   "Converts first character of the string to upper-case, all other\ncharacters to lower-case.",
   :namespace "clojure.string",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/capitalize"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :added "1.8",
   :name "ends-with?",
   :file "src/clj/clojure/string.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L367",
   :line 367,
   :var-type "function",
   :arglists ([s substr]),
   :doc "True if s ends with substr.",
   :namespace "clojure.string",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/ends-with?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :added "1.2",
   :name "escape",
   :file "src/clj/clojure/string.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L301",
   :line 301,
   :var-type "function",
   :arglists ([s cmap]),
   :doc
   "Return a new string, using cmap to escape each character ch\nfrom s as follows:\n\nIf (cmap ch) is nil, append ch to the new string.\nIf (cmap ch) is non-nil, append (str (cmap ch)) instead.",
   :namespace "clojure.string",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/escape"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :added "1.8",
   :name "includes?",
   :file "src/clj/clojure/string.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L373",
   :line 373,
   :var-type "function",
   :arglists ([s substr]),
   :doc "True if s includes substr.",
   :namespace "clojure.string",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/includes?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :added "1.8",
   :name "index-of",
   :file "src/clj/clojure/string.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L319",
   :line 319,
   :var-type "function",
   :arglists ([s value] [s value from-index]),
   :doc
   "Return index of value (string or char) in s, optionally searching\nforward from from-index. Return nil if value not found.",
   :namespace "clojure.string",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/index-of"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :added "1.2",
   :name "join",
   :file "src/clj/clojure/string.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L180",
   :line 180,
   :var-type "function",
   :arglists ([coll] [separator coll]),
   :doc
   "Returns a string of all elements in coll, as returned by (seq coll),\nseparated by an optional separator.",
   :namespace "clojure.string",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/join"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :added "1.8",
   :name "last-index-of",
   :file "src/clj/clojure/string.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L340",
   :line 340,
   :var-type "function",
   :arglists ([s value] [s value from-index]),
   :doc
   "Return last index of value (string or char) in s, optionally\nsearching backward from from-index. Return nil if value not found.",
   :namespace "clojure.string",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/last-index-of"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :added "1.2",
   :name "lower-case",
   :file "src/clj/clojure/string.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L213",
   :line 213,
   :var-type "function",
   :arglists ([s]),
   :doc "Converts string to all lower-case.",
   :namespace "clojure.string",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/lower-case"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :added "1.5",
   :name "re-quote-replacement",
   :file "src/clj/clojure/string.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L54",
   :line 54,
   :var-type "function",
   :arglists ([replacement]),
   :doc
   "Given a replacement string that you wish to be a literal\nreplacement for a pattern match in replace or replace-first, do the\nnecessary escaping of special characters in the replacement.",
   :namespace "clojure.string",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/re-quote-replacement"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :added "1.2",
   :name "replace",
   :file "src/clj/clojure/string.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L75",
   :line 75,
   :var-type "function",
   :arglists ([s match replacement]),
   :doc
   "Replaces all instance of match with replacement in s.\n\nmatch/replacement can be:\n\nstring / string\nchar / char\npattern / (string or function of match).\n\nSee also replace-first.\n\nThe replacement is literal (i.e. none of its characters are treated\nspecially) for all cases above except pattern / string.\n\nFor pattern / string, $1, $2, etc. in the replacement string are\nsubstituted with the string that matched the corresponding\nparenthesized group in the pattern.  If you wish your replacement\nstring r to be used literally, use (re-quote-replacement r) as the\nreplacement argument.  See also documentation for\njava.util.regex.Matcher's appendReplacement method.\n\nExample:\n(clojure.string/replace \"Almost Pig Latin\" #\"\\b(\\w)(\\w+)\\b\" \"$2$1ay\")\n-> \"lmostAay igPay atinLay\"",
   :namespace "clojure.string",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/replace"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :added "1.2",
   :name "replace-first",
   :file "src/clj/clojure/string.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L138",
   :line 138,
   :var-type "function",
   :arglists ([s match replacement]),
   :doc
   "Replaces the first instance of match with replacement in s.\n\nmatch/replacement can be:\n\nchar / char\nstring / string\npattern / (string or function of match).\n\nSee also replace.\n\nThe replacement is literal (i.e. none of its characters are treated\nspecially) for all cases above except pattern / string.\n\nFor pattern / string, $1, $2, etc. in the replacement string are\nsubstituted with the string that matched the corresponding\nparenthesized group in the pattern.  If you wish your replacement\nstring r to be used literally, use (re-quote-replacement r) as the\nreplacement argument.  See also documentation for\njava.util.regex.Matcher's appendReplacement method.\n\nExample:\n(clojure.string/replace-first \"swap first two words\"\n                              #\"(\\w+)(\\s+)(\\w+)\" \"$3$2$1\")\n-> \"first swap two words\"",
   :namespace "clojure.string",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/replace-first"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :added "1.2",
   :name "reverse",
   :file "src/clj/clojure/string.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L48",
   :line 48,
   :var-type "function",
   :arglists ([s]),
   :doc "Returns s with its characters reversed.",
   :namespace "clojure.string",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/reverse"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :added "1.2",
   :name "split",
   :file "src/clj/clojure/string.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L219",
   :line 219,
   :var-type "function",
   :arglists ([s re] [s re limit]),
   :doc
   "Splits string on a regular expression.  Optional argument limit is\nthe maximum number of parts. Not lazy. Returns vector of the parts.\nTrailing empty strings are not returned - pass limit of -1 to return all.",
   :namespace "clojure.string",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/split"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :added "1.2",
   :name "split-lines",
   :file "src/clj/clojure/string.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L229",
   :line 229,
   :var-type "function",
   :arglists ([s]),
   :doc
   "Splits s on \\n or \\r\\n. Trailing empty lines are not returned.",
   :namespace "clojure.string",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/split-lines"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :added "1.8",
   :name "starts-with?",
   :file "src/clj/clojure/string.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L361",
   :line 361,
   :var-type "function",
   :arglists ([s substr]),
   :doc "True if s starts with substr.",
   :namespace "clojure.string",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/starts-with?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :added "1.2",
   :name "trim",
   :file "src/clj/clojure/string.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L235",
   :line 235,
   :var-type "function",
   :arglists ([s]),
   :doc "Removes whitespace from both ends of string.",
   :namespace "clojure.string",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/trim"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :added "1.2",
   :name "trim-newline",
   :file "src/clj/clojure/string.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L275",
   :line 275,
   :var-type "function",
   :arglists ([s]),
   :doc
   "Removes all trailing newline \\n or return \\r characters from\nstring.  Similar to Perl's chomp.",
   :namespace "clojure.string",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/trim-newline"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :added "1.2",
   :name "triml",
   :file "src/clj/clojure/string.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L252",
   :line 252,
   :var-type "function",
   :arglists ([s]),
   :doc "Removes whitespace from the left side of string.",
   :namespace "clojure.string",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/triml"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :added "1.2",
   :name "trimr",
   :file "src/clj/clojure/string.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L264",
   :line 264,
   :var-type "function",
   :arglists ([s]),
   :doc "Removes whitespace from the right side of string.",
   :namespace "clojure.string",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/trimr"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj",
   :added "1.2",
   :name "upper-case",
   :file "src/clj/clojure/string.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/ade22645ba5dbf4c0d8115b19938af96d6fb4cd5/src/clj/clojure/string.clj#L207",
   :line 207,
   :var-type "function",
   :arglists ([s]),
   :doc "Converts string to all upper-case.",
   :namespace "clojure.string",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.string-api.html#clojure.string/upper-case"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/c4c0740a0696bc95b2184c0fef55ed7c3bb097f6/src/clj/clojure/template.clj",
   :name "apply-template",
   :file "src/clj/clojure/template.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/c4c0740a0696bc95b2184c0fef55ed7c3bb097f6/src/clj/clojure/template.clj#L30",
   :line 30,
   :var-type "function",
   :arglists ([argv expr values]),
   :doc
   "For use in macros.  argv is an argument list, as in defn.  expr is\na quoted expression using the symbols in argv.  values is a sequence\nof values to be used for the arguments.\n\napply-template will recursively replace argument symbols in expr\nwith their corresponding values, returning a modified expr.\n\nExample: (apply-template '[x] '(+ x x) '[2])\n         ;=> (+ 2 2)",
   :namespace "clojure.template",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.template-api.html#clojure.template/apply-template"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/c4c0740a0696bc95b2184c0fef55ed7c3bb097f6/src/clj/clojure/template.clj",
   :name "do-template",
   :file "src/clj/clojure/template.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/c4c0740a0696bc95b2184c0fef55ed7c3bb097f6/src/clj/clojure/template.clj#L45",
   :line 45,
   :var-type "macro",
   :arglists ([argv expr & values]),
   :doc
   "Repeatedly copies expr (in a do block) for each group of arguments\nin values.  values are automatically partitioned by the number of\narguments in argv, an argument vector as in defn.\n\nExample: (macroexpand '(do-template [x y] (+ y x) 2 4 3 5))\n         ;=> (do (+ 4 2) (+ 5 3))",
   :namespace "clojure.template",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.template-api.html#clojure.template/do-template"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj",
   :added "1.1",
   :name "*load-tests*",
   :file "src/clj/clojure/test.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj#L245",
   :dynamic true,
   :line 245,
   :var-type "var",
   :arglists nil,
   :doc
   "True by default.  If set to false, no test functions will\nbe created by deftest, set-test, or with-test.  Use this to omit\ntests when compiling or loading production code.",
   :namespace "clojure.test",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/*load-tests*"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj",
   :added "1.1",
   :name "*stack-trace-depth*",
   :file "src/clj/clojure/test.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj#L252",
   :dynamic true,
   :line 252,
   :var-type "var",
   :arglists nil,
   :doc
   "The maximum depth of stack traces to print when an Exception\nis thrown during a test.  Defaults to nil, which means print the \ncomplete stack trace.",
   :namespace "clojure.test",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/*stack-trace-depth*"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj",
   :added "1.1",
   :name "are",
   :file "src/clj/clojure/test.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj#L572",
   :line 572,
   :var-type "macro",
   :arglists ([argv expr & args]),
   :doc
   "Checks multiple assertions with a template expression.\nSee clojure.template/do-template for an explanation of\ntemplates.\n\nExample: (are [x y] (= x y)  \n              2 (+ 1 1)\n              4 (* 2 2))\nExpands to: \n         (do (is (= 2 (+ 1 1)))\n             (is (= 4 (* 2 2))))\n\nNote: This breaks some reporting features, such as line numbers.",
   :namespace "clojure.test",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/are"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj",
   :added "1.1",
   :name "assert-any",
   :file "src/clj/clojure/test.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj#L455",
   :line 455,
   :var-type "function",
   :arglists ([msg form]),
   :doc
   "Returns generic assertion code for any test, including macros, Java\nmethod calls, or isolated symbols.",
   :namespace "clojure.test",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/assert-any"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj",
   :added "1.1",
   :name "assert-predicate",
   :file "src/clj/clojure/test.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj#L436",
   :line 436,
   :var-type "function",
   :arglists ([msg form]),
   :doc
   "Returns generic assertion code for any functional predicate.  The\n'expected' argument to 'report' will contains the original form, the\n'actual' argument will contain the form with all its sub-forms\nevaluated.  If the predicate returns false, the 'actual' form will\nbe wrapped in (not...).",
   :namespace "clojure.test",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/assert-predicate"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj",
   :added "1.1",
   :name "compose-fixtures",
   :file "src/clj/clojure/test.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj#L689",
   :line 689,
   :var-type "function",
   :arglists ([f1 f2]),
   :doc
   "Composes two fixture functions, creating a new fixture function\nthat combines their behavior.",
   :namespace "clojure.test",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/compose-fixtures"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj",
   :added "1.1",
   :name "deftest",
   :file "src/clj/clojure/test.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj#L622",
   :line 622,
   :var-type "macro",
   :arglists ([name & body]),
   :doc
   "Defines a test function with no arguments.  Test functions may call\nother tests, so tests may be composed.  If you compose tests, you\nshould also define a function named test-ns-hook; run-tests will\ncall test-ns-hook instead of testing all vars.\n\nNote: Actually, the test body goes in the :test metadata on the var,\nand the real function (the value of the var) calls test-var on\nitself.\n\nWhen *load-tests* is false, deftest is ignored.",
   :namespace "clojure.test",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/deftest"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj",
   :added "1.1",
   :name "deftest-",
   :file "src/clj/clojure/test.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj#L639",
   :line 639,
   :var-type "macro",
   :arglists ([name & body]),
   :doc "Like deftest but creates a private var.",
   :namespace "clojure.test",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/deftest-"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj",
   :added "1.2",
   :name "do-report",
   :file "src/clj/clojure/test.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj#L351",
   :line 351,
   :var-type "function",
   :arglists ([m]),
   :doc
   "Add file and line information to a test result and call report.\nIf you are writing a custom assert-expr method, call this function\nto pass test results to report.",
   :namespace "clojure.test",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/do-report"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj",
   :added "1.1",
   :name "file-position",
   :file "src/clj/clojure/test.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj#L282",
   :line 282,
   :deprecated "1.2",
   :var-type "function",
   :arglists ([n]),
   :doc
   "Returns a vector [filename line-number] for the nth call up the\nstack.\n\nDeprecated in 1.2: The information needed for test reporting is\nnow on :file and :line keys in the result map.",
   :namespace "clojure.test",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/file-position"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj",
   :added "1.1",
   :name "function?",
   :file "src/clj/clojure/test.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj#L424",
   :line 424,
   :var-type "function",
   :arglists ([x]),
   :doc
   "Returns true if argument is a function or a symbol that resolves to\na function (not a macro).",
   :namespace "clojure.test",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/function?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj",
   :added "1.1",
   :name "get-possibly-unbound-var",
   :file "src/clj/clojure/test.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj#L416",
   :line 416,
   :var-type "function",
   :arglists ([v]),
   :doc "Like var-get but returns nil if the var is unbound.",
   :namespace "clojure.test",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/get-possibly-unbound-var"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj",
   :added "1.1",
   :name "inc-report-counter",
   :file "src/clj/clojure/test.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj#L314",
   :line 314,
   :var-type "function",
   :arglists ([name]),
   :doc
   "Increments the named counter in *report-counters*, a ref to a map.\nDoes nothing if *report-counters* is nil.",
   :namespace "clojure.test",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/inc-report-counter"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj",
   :added "1.1",
   :name "is",
   :file "src/clj/clojure/test.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj#L554",
   :line 554,
   :var-type "macro",
   :arglists ([form] [form msg]),
   :doc
   "Generic assertion macro.  'form' is any predicate test.\n'msg' is an optional message to attach to the assertion.\n\nExample: (is (= 4 (+ 2 2)) \"Two plus two should be 4\")\n\nSpecial forms:\n\n(is (thrown? c body)) checks that an instance of c is thrown from\nbody, fails if not; then returns the thing thrown.\n\n(is (thrown-with-msg? c re body)) checks that an instance of c is\nthrown AND that the message on the exception matches (with\nre-find) the regular expression re.",
   :namespace "clojure.test",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/is"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj",
   :added "1.1",
   :name "join-fixtures",
   :file "src/clj/clojure/test.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj#L696",
   :line 696,
   :var-type "function",
   :arglists ([fixtures]),
   :doc
   "Composes a collection of fixtures, in order.  Always returns a valid\nfixture function, even if the collection is empty.",
   :namespace "clojure.test",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/join-fixtures"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj",
   :added "1.1",
   :name "report",
   :file "src/clj/clojure/test.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj#L324",
   :dynamic true,
   :line 324,
   :var-type "multimethod",
   :arglists nil,
   :doc
   "Generic reporting function, may be overridden to plug in\ndifferent report formats (e.g., TAP, JUnit).  Assertions such as\n'is' call 'report' to indicate results.  The argument given to\n'report' will be a map with a :type key.  See the documentation at\nthe top of test_is.clj for more information on the types of\narguments for 'report'.",
   :namespace "clojure.test",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/report"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj",
   :added "1.1",
   :name "run-all-tests",
   :file "src/clj/clojure/test.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj#L780",
   :line 780,
   :var-type "function",
   :arglists ([] [re]),
   :doc
   "Runs all tests in all namespaces; prints results.\nOptional argument is a regular expression; only namespaces with\nnames matching the regular expression (with re-matches) will be\ntested.",
   :namespace "clojure.test",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/run-all-tests"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj",
   :added "1.11",
   :name "run-test",
   :file "src/clj/clojure/test.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj#L813",
   :line 813,
   :var-type "macro",
   :arglists ([test-symbol]),
   :doc
   "Runs a single test.\n\nBecause the intent is to run a single test, there is no check for the namespace test-ns-hook.",
   :namespace "clojure.test",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/run-test"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj",
   :added "1.11",
   :name "run-test-var",
   :file "src/clj/clojure/test.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj#L797",
   :line 797,
   :var-type "function",
   :arglists ([v]),
   :doc
   "Runs the tests for a single Var, with fixtures executed around the test, and summary output after.",
   :namespace "clojure.test",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/run-test-var"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj",
   :added "1.1",
   :name "run-tests",
   :file "src/clj/clojure/test.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj#L768",
   :line 768,
   :var-type "function",
   :arglists ([] [& namespaces]),
   :doc
   "Runs all tests in the given namespaces; prints results.\nDefaults to current namespace if none given.  Returns a map\nsummarizing test results.",
   :namespace "clojure.test",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/run-tests"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj",
   :added "1.1",
   :name "set-test",
   :file "src/clj/clojure/test.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj#L648",
   :line 648,
   :var-type "macro",
   :arglists ([name & body]),
   :doc
   "Experimental.\nSets :test metadata of the named var to a fn with the given body.\nThe var must already exist.  Does not modify the value of the var.\n\nWhen *load-tests* is false, set-test is ignored.",
   :namespace "clojure.test",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/set-test"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj",
   :added "1.1",
   :name "successful?",
   :file "src/clj/clojure/test.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj#L789",
   :line 789,
   :var-type "function",
   :arglists ([summary]),
   :doc
   "Returns true if the given test summary indicates all tests\nwere successful, false otherwise.",
   :namespace "clojure.test",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/successful?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj",
   :added "1.1",
   :name "test-all-vars",
   :file "src/clj/clojure/test.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj#L737",
   :line 737,
   :var-type "function",
   :arglists ([ns]),
   :doc
   "Calls test-vars on every var interned in the namespace, with fixtures.",
   :namespace "clojure.test",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/test-all-vars"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj",
   :added "1.1",
   :name "test-ns",
   :file "src/clj/clojure/test.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj#L743",
   :line 743,
   :var-type "function",
   :arglists ([ns]),
   :doc
   "If the namespace defines a function named test-ns-hook, calls that.\nOtherwise, calls test-all-vars on the namespace.  'ns' is a\nnamespace object or a symbol.\n\nInternally binds *report-counters* to a ref initialized to\n*initial-report-counters*.  Returns the final, dereferenced state of\n*report-counters*.",
   :namespace "clojure.test",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/test-ns"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj",
   :added "1.1",
   :name "test-var",
   :file "src/clj/clojure/test.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj#L708",
   :dynamic true,
   :line 708,
   :var-type "function",
   :arglists ([v]),
   :doc
   "If v has a function in its :test metadata, calls that function,\nwith *testing-vars* bound to (conj *testing-vars* v).",
   :namespace "clojure.test",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/test-var"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj",
   :added "1.6",
   :name "test-vars",
   :file "src/clj/clojure/test.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj#L723",
   :line 723,
   :var-type "function",
   :arglists ([vars]),
   :doc
   "Groups vars by their namespace and runs test-var on them with\nappropriate fixtures applied.",
   :namespace "clojure.test",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/test-vars"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj",
   :added "1.1",
   :name "testing",
   :file "src/clj/clojure/test.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj#L597",
   :line 597,
   :var-type "macro",
   :arglists ([string & body]),
   :doc
   "Adds a new string to the list of testing contexts.  May be nested,\nbut must occur inside a test function (deftest).",
   :namespace "clojure.test",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/testing"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj",
   :added "1.1",
   :name "testing-contexts-str",
   :file "src/clj/clojure/test.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj#L307",
   :line 307,
   :var-type "function",
   :arglists ([]),
   :doc
   "Returns a string representation of the current test context. Joins\nstrings in *testing-contexts* with spaces.",
   :namespace "clojure.test",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/testing-contexts-str"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj",
   :added "1.1",
   :name "testing-vars-str",
   :file "src/clj/clojure/test.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj#L294",
   :line 294,
   :var-type "function",
   :arglists ([m]),
   :doc
   "Returns a string representation of the current test.  Renders names\nin *testing-vars* as a list, then the source file and line of\ncurrent assertion.",
   :namespace "clojure.test",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/testing-vars-str"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj",
   :added "1.1",
   :name "try-expr",
   :file "src/clj/clojure/test.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj#L538",
   :line 538,
   :var-type "macro",
   :arglists ([msg form]),
   :doc
   "Used by the 'is' macro to catch unexpected exceptions.\nYou don't call this.",
   :namespace "clojure.test",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/try-expr"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj",
   :added "1.1",
   :name "use-fixtures",
   :file "src/clj/clojure/test.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj#L670",
   :line 670,
   :var-type "multimethod",
   :arglists nil,
   :doc
   "Wrap test runs in a fixture function to perform setup and\nteardown. Using a fixture-type of :each wraps every test\nindividually, while :once wraps the whole run in a single function.",
   :namespace "clojure.test",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/use-fixtures"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj",
   :added "1.1",
   :name "with-test",
   :file "src/clj/clojure/test.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj#L609",
   :line 609,
   :var-type "macro",
   :arglists ([definition & body]),
   :doc
   "Takes any definition form (that returns a Var) as the first argument.\nRemaining body goes in the :test metadata function for that Var.\n\nWhen *load-tests* is false, only evaluates the definition, ignoring\nthe tests.",
   :namespace "clojure.test",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/with-test"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj",
   :added "1.1",
   :name "with-test-out",
   :file "src/clj/clojure/test.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/4b9eadccce2aaf97e64bcc8e35c05c529df8fdd2/src/clj/clojure/test.clj#L273",
   :line 273,
   :var-type "macro",
   :arglists ([& body]),
   :doc "Runs body with *out* bound to the value of *test-out*.",
   :namespace "clojure.test",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test/with-test-out"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/b8c78ebf79b6a996f349dd112aaed658c132735d/src/clj/clojure/walk.clj",
   :added "1.1",
   :name "keywordize-keys",
   :file "src/clj/clojure/walk.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/b8c78ebf79b6a996f349dd112aaed658c132735d/src/clj/clojure/walk.clj#L94",
   :line 94,
   :var-type "function",
   :arglists ([m]),
   :doc
   "Recursively transforms all map keys from strings to keywords.",
   :namespace "clojure.walk",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.walk-api.html#clojure.walk/keywordize-keys"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/b8c78ebf79b6a996f349dd112aaed658c132735d/src/clj/clojure/walk.clj",
   :added "1.1",
   :name "macroexpand-all",
   :file "src/clj/clojure/walk.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/b8c78ebf79b6a996f349dd112aaed658c132735d/src/clj/clojure/walk.clj#L126",
   :line 126,
   :var-type "function",
   :arglists ([form]),
   :doc "Recursively performs all possible macroexpansions in form.",
   :namespace "clojure.walk",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.walk-api.html#clojure.walk/macroexpand-all"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/b8c78ebf79b6a996f349dd112aaed658c132735d/src/clj/clojure/walk.clj",
   :added "1.1",
   :name "postwalk",
   :file "src/clj/clojure/walk.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/b8c78ebf79b6a996f349dd112aaed658c132735d/src/clj/clojure/walk.clj#L53",
   :line 53,
   :var-type "function",
   :arglists ([f form]),
   :doc
   "Performs a depth-first, post-order traversal of form.  Calls f on\neach sub-form, uses f's return value in place of the original.\nRecognizes all Clojure data structures. Consumes seqs as with doall.",
   :namespace "clojure.walk",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.walk-api.html#clojure.walk/postwalk"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/b8c78ebf79b6a996f349dd112aaed658c132735d/src/clj/clojure/walk.clj",
   :added "1.1",
   :name "postwalk-demo",
   :file "src/clj/clojure/walk.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/b8c78ebf79b6a996f349dd112aaed658c132735d/src/clj/clojure/walk.clj#L80",
   :line 80,
   :var-type "function",
   :arglists ([form]),
   :doc
   "Demonstrates the behavior of postwalk by printing each form as it is\nwalked.  Returns form.",
   :namespace "clojure.walk",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.walk-api.html#clojure.walk/postwalk-demo"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/b8c78ebf79b6a996f349dd112aaed658c132735d/src/clj/clojure/walk.clj",
   :added "1.1",
   :name "postwalk-replace",
   :file "src/clj/clojure/walk.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/b8c78ebf79b6a996f349dd112aaed658c132735d/src/clj/clojure/walk.clj#L118",
   :line 118,
   :var-type "function",
   :arglists ([smap form]),
   :doc
   "Recursively transforms form by replacing keys in smap with their\nvalues.  Like clojure/replace but works on any data structure.  Does\nreplacement at the leaves of the tree first.",
   :namespace "clojure.walk",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.walk-api.html#clojure.walk/postwalk-replace"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/b8c78ebf79b6a996f349dd112aaed658c132735d/src/clj/clojure/walk.clj",
   :added "1.1",
   :name "prewalk",
   :file "src/clj/clojure/walk.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/b8c78ebf79b6a996f349dd112aaed658c132735d/src/clj/clojure/walk.clj#L61",
   :line 61,
   :var-type "function",
   :arglists ([f form]),
   :doc "Like postwalk, but does pre-order traversal.",
   :namespace "clojure.walk",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.walk-api.html#clojure.walk/prewalk"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/b8c78ebf79b6a996f349dd112aaed658c132735d/src/clj/clojure/walk.clj",
   :added "1.1",
   :name "prewalk-demo",
   :file "src/clj/clojure/walk.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/b8c78ebf79b6a996f349dd112aaed658c132735d/src/clj/clojure/walk.clj#L87",
   :line 87,
   :var-type "function",
   :arglists ([form]),
   :doc
   "Demonstrates the behavior of prewalk by printing each form as it is\nwalked.  Returns form.",
   :namespace "clojure.walk",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.walk-api.html#clojure.walk/prewalk-demo"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/b8c78ebf79b6a996f349dd112aaed658c132735d/src/clj/clojure/walk.clj",
   :added "1.1",
   :name "prewalk-replace",
   :file "src/clj/clojure/walk.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/b8c78ebf79b6a996f349dd112aaed658c132735d/src/clj/clojure/walk.clj#L110",
   :line 110,
   :var-type "function",
   :arglists ([smap form]),
   :doc
   "Recursively transforms form by replacing keys in smap with their\nvalues.  Like clojure/replace but works on any data structure.  Does\nreplacement at the root of the tree first.",
   :namespace "clojure.walk",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.walk-api.html#clojure.walk/prewalk-replace"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/b8c78ebf79b6a996f349dd112aaed658c132735d/src/clj/clojure/walk.clj",
   :added "1.1",
   :name "stringify-keys",
   :file "src/clj/clojure/walk.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/b8c78ebf79b6a996f349dd112aaed658c132735d/src/clj/clojure/walk.clj#L102",
   :line 102,
   :var-type "function",
   :arglists ([m]),
   :doc
   "Recursively transforms all map keys from keywords to strings.",
   :namespace "clojure.walk",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.walk-api.html#clojure.walk/stringify-keys"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/b8c78ebf79b6a996f349dd112aaed658c132735d/src/clj/clojure/walk.clj",
   :added "1.1",
   :name "walk",
   :file "src/clj/clojure/walk.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/b8c78ebf79b6a996f349dd112aaed658c132735d/src/clj/clojure/walk.clj#L35",
   :line 35,
   :var-type "function",
   :arglists ([inner outer form]),
   :doc
   "Traverses form, an arbitrary data structure.  inner and outer are\nfunctions.  Applies inner to each element of form, building up a\ndata structure of the same type, then applies outer to the result.\nRecognizes all Clojure data structures. Consumes seqs as with doall.",
   :namespace "clojure.walk",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.walk-api.html#clojure.walk/walk"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/4a4a6e7717d411679820c4a3ce735a77aef45cc3/src/clj/clojure/xml.clj",
   :added "1.11",
   :name "disable-external-entities",
   :file "src/clj/clojure/xml.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/4a4a6e7717d411679820c4a3ce735a77aef45cc3/src/clj/clojure/xml.clj#L81",
   :line 81,
   :var-type "function",
   :arglists ([parser]),
   :doc
   "Modifies a SAXParser to disable external entity resolution to prevent XXE attacks",
   :namespace "clojure.xml",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.xml-api.html#clojure.xml/disable-external-entities"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/4a4a6e7717d411679820c4a3ce735a77aef45cc3/src/clj/clojure/xml.clj",
   :added "1.0",
   :name "parse",
   :file "src/clj/clojure/xml.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/4a4a6e7717d411679820c4a3ce735a77aef45cc3/src/clj/clojure/xml.clj#L106",
   :line 106,
   :var-type "function",
   :arglists ([s] [s startparse]),
   :doc
   "Parses and loads the source s, which can be a File, InputStream or\nString naming a URI. Returns a tree of the xml/element struct-map,\nwhich has the keys :tag, :attrs, and :content. and accessor fns tag,\nattrs, and content. Other parsers can be supplied by passing\nstartparse, a fn taking a source and a ContentHandler and returning\na parser.\n\nPrior to 1.11, used startparse-sax by default. As of 1.11, uses\nstartparse-sax-safe, which disables XXE (XML External Entity)\nprocessing. Pass startparse-sax to revert to prior behavior.",
   :namespace "clojure.xml",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.xml-api.html#clojure.xml/parse"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/4a4a6e7717d411679820c4a3ce735a77aef45cc3/src/clj/clojure/xml.clj",
   :added "1.11",
   :name "sax-parser",
   :file "src/clj/clojure/xml.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/4a4a6e7717d411679820c4a3ce735a77aef45cc3/src/clj/clojure/xml.clj#L75",
   :line 75,
   :var-type "function",
   :arglists ([]),
   :doc "Create a new SAXParser",
   :namespace "clojure.xml",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.xml-api.html#clojure.xml/sax-parser"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/4a4a6e7717d411679820c4a3ce735a77aef45cc3/src/clj/clojure/xml.clj",
   :added "1.0",
   :name "startparse-sax",
   :file "src/clj/clojure/xml.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/4a4a6e7717d411679820c4a3ce735a77aef45cc3/src/clj/clojure/xml.clj#L92",
   :line 92,
   :var-type "function",
   :arglists ([s ch]),
   :doc
   "A startparse function suitable for use with clojure.xml/parse.\nNote that this function is open to XXE entity attacks, see startparse-sax-safe.",
   :namespace "clojure.xml",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.xml-api.html#clojure.xml/startparse-sax"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/4a4a6e7717d411679820c4a3ce735a77aef45cc3/src/clj/clojure/xml.clj",
   :added "1.11",
   :name "startparse-sax-safe",
   :file "src/clj/clojure/xml.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/4a4a6e7717d411679820c4a3ce735a77aef45cc3/src/clj/clojure/xml.clj#L99",
   :line 99,
   :var-type "function",
   :arglists ([s ch]),
   :doc
   "A startparse function suitable for use with clojure.xml/parse.\nExternal entity resolution is disabled to prevent XXE entity attacks.",
   :namespace "clojure.xml",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.xml-api.html#clojure.xml/startparse-sax-safe"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj",
   :added "1.0",
   :name "append-child",
   :file "src/clj/clojure/zip.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj#L223",
   :line 223,
   :var-type "function",
   :arglists ([loc item]),
   :doc
   "Inserts the item as the rightmost child of the node at this loc,\nwithout moving",
   :namespace "clojure.zip",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/append-child"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj",
   :added "1.0",
   :name "branch?",
   :file "src/clj/clojure/zip.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj#L69",
   :line 69,
   :var-type "function",
   :arglists ([loc]),
   :doc "Returns true if the node at loc is a branch",
   :namespace "clojure.zip",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/branch?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj",
   :added "1.0",
   :name "children",
   :file "src/clj/clojure/zip.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj#L75",
   :line 75,
   :var-type "function",
   :arglists ([loc]),
   :doc
   "Returns a seq of the children of node at loc, which must be a branch",
   :namespace "clojure.zip",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/children"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj",
   :added "1.0",
   :name "down",
   :file "src/clj/clojure/zip.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj#L109",
   :line 109,
   :var-type "function",
   :arglists ([loc]),
   :doc
   "Returns the loc of the leftmost child of the node at this loc, or\nnil if no children",
   :namespace "clojure.zip",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/down"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj",
   :added "1.0",
   :name "edit",
   :file "src/clj/clojure/zip.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj#L210",
   :line 210,
   :var-type "function",
   :arglists ([loc f & args]),
   :doc
   "Replaces the node at this loc with the value of (f node args)",
   :namespace "clojure.zip",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/edit"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj",
   :added "1.0",
   :name "end?",
   :file "src/clj/clojure/zip.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj#L258",
   :line 258,
   :var-type "function",
   :arglists ([loc]),
   :doc "Returns true if loc represents the end of a depth-first walk",
   :namespace "clojure.zip",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/end?"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj",
   :added "1.0",
   :name "insert-child",
   :file "src/clj/clojure/zip.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj#L216",
   :line 216,
   :var-type "function",
   :arglists ([loc item]),
   :doc
   "Inserts the item as the leftmost child of the node at this loc,\nwithout moving",
   :namespace "clojure.zip",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/insert-child"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj",
   :added "1.0",
   :name "insert-left",
   :file "src/clj/clojure/zip.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj#L183",
   :line 183,
   :var-type "function",
   :arglists ([loc item]),
   :doc
   "Inserts the item as the left sibling of the node at this loc,\nwithout moving",
   :namespace "clojure.zip",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/insert-left"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj",
   :added "1.0",
   :name "insert-right",
   :file "src/clj/clojure/zip.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj#L193",
   :line 193,
   :var-type "function",
   :arglists ([loc item]),
   :doc
   "Inserts the item as the right sibling of the node at this loc,\nwithout moving",
   :namespace "clojure.zip",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/insert-right"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj",
   :added "1.0",
   :name "left",
   :file "src/clj/clojure/zip.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj#L166",
   :line 166,
   :var-type "function",
   :arglists ([loc]),
   :doc
   "Returns the loc of the left sibling of the node at this loc, or nil",
   :namespace "clojure.zip",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/left"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj",
   :added "1.0",
   :name "leftmost",
   :file "src/clj/clojure/zip.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj#L174",
   :line 174,
   :var-type "function",
   :arglists ([loc]),
   :doc
   "Returns the loc of the leftmost sibling of the node at this loc, or self",
   :namespace "clojure.zip",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/leftmost"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj",
   :added "1.0",
   :name "lefts",
   :file "src/clj/clojure/zip.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj#L96",
   :line 96,
   :var-type "function",
   :arglists ([loc]),
   :doc "Returns a seq of the left siblings of this loc",
   :namespace "clojure.zip",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/lefts"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj",
   :added "1.0",
   :name "make-node",
   :file "src/clj/clojure/zip.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj#L83",
   :line 83,
   :var-type "function",
   :arglists ([loc node children]),
   :doc
   "Returns a new branch node, given an existing node and new\nchildren. The loc is only used to supply the constructor.",
   :namespace "clojure.zip",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/make-node"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj",
   :added "1.0",
   :name "next",
   :file "src/clj/clojure/zip.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj#L230",
   :line 230,
   :var-type "function",
   :arglists ([loc]),
   :doc
   "Moves to the next loc in the hierarchy, depth-first. When reaching\nthe end, returns a distinguished loc detectable via end?. If already\nat the end, stays there.",
   :namespace "clojure.zip",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/next"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj",
   :added "1.0",
   :name "node",
   :file "src/clj/clojure/zip.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj#L64",
   :line 64,
   :var-type "function",
   :arglists ([loc]),
   :doc "Returns the node at loc",
   :namespace "clojure.zip",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/node"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj",
   :added "1.0",
   :name "path",
   :file "src/clj/clojure/zip.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj#L90",
   :line 90,
   :var-type "function",
   :arglists ([loc]),
   :doc "Returns a seq of nodes leading to this loc",
   :namespace "clojure.zip",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/path"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj",
   :added "1.0",
   :name "prev",
   :file "src/clj/clojure/zip.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj#L246",
   :line 246,
   :var-type "function",
   :arglists ([loc]),
   :doc
   "Moves to the previous loc in the hierarchy, depth-first. If already\nat the root, returns nil.",
   :namespace "clojure.zip",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/prev"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj",
   :added "1.0",
   :name "remove",
   :file "src/clj/clojure/zip.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj#L264",
   :line 264,
   :var-type "function",
   :arglists ([loc]),
   :doc
   "Removes the node at loc, returning the loc that would have preceded\nit in a depth-first walk.",
   :namespace "clojure.zip",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/remove"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj",
   :added "1.0",
   :name "replace",
   :file "src/clj/clojure/zip.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj#L203",
   :line 203,
   :var-type "function",
   :arglists ([loc node]),
   :doc "Replaces the node at this loc, without moving",
   :namespace "clojure.zip",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/replace"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj",
   :added "1.0",
   :name "right",
   :file "src/clj/clojure/zip.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj#L149",
   :line 149,
   :var-type "function",
   :arglists ([loc]),
   :doc
   "Returns the loc of the right sibling of the node at this loc, or nil",
   :namespace "clojure.zip",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/right"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj",
   :added "1.0",
   :name "rightmost",
   :file "src/clj/clojure/zip.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj#L157",
   :line 157,
   :var-type "function",
   :arglists ([loc]),
   :doc
   "Returns the loc of the rightmost sibling of the node at this loc, or self",
   :namespace "clojure.zip",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/rightmost"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj",
   :added "1.0",
   :name "rights",
   :file "src/clj/clojure/zip.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj#L102",
   :line 102,
   :var-type "function",
   :arglists ([loc]),
   :doc "Returns a seq of the right siblings of this loc",
   :namespace "clojure.zip",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/rights"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj",
   :added "1.0",
   :name "root",
   :file "src/clj/clojure/zip.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj#L137",
   :line 137,
   :var-type "function",
   :arglists ([loc]),
   :doc
   "zips all the way up and returns the root node, reflecting any\nchanges.",
   :namespace "clojure.zip",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/root"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj",
   :added "1.0",
   :name "seq-zip",
   :file "src/clj/clojure/zip.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj#L35",
   :line 35,
   :var-type "function",
   :arglists ([root]),
   :doc "Returns a zipper for nested sequences, given a root sequence",
   :namespace "clojure.zip",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/seq-zip"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj",
   :added "1.0",
   :name "up",
   :file "src/clj/clojure/zip.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj#L123",
   :line 123,
   :var-type "function",
   :arglists ([loc]),
   :doc
   "Returns the loc of the parent of the node at this loc, or nil if at\nthe top",
   :namespace "clojure.zip",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/up"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj",
   :added "1.0",
   :name "vector-zip",
   :file "src/clj/clojure/zip.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj#L44",
   :line 44,
   :var-type "function",
   :arglists ([root]),
   :doc "Returns a zipper for nested vectors, given a root vector",
   :namespace "clojure.zip",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/vector-zip"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj",
   :added "1.0",
   :name "xml-zip",
   :file "src/clj/clojure/zip.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj#L53",
   :line 53,
   :var-type "function",
   :arglists ([root]),
   :doc
   "Returns a zipper for xml elements (as from xml/parse),\ngiven a root element",
   :namespace "clojure.zip",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/xml-zip"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj",
   :added "1.0",
   :name "zipper",
   :file "src/clj/clojure/zip.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/59b65669860a1f33825775494809e5d500c19c63/src/clj/clojure/zip.clj#L18",
   :line 18,
   :var-type "function",
   :arglists ([branch? children make-node root]),
   :doc
   "Creates a new zipper structure. \n\nbranch? is a fn that, given a node, returns true if can have\nchildren, even if it currently doesn't.\n\nchildren is a fn that, given a branch node, returns a seq of its\nchildren.\n\nmake-node is a fn that, given an existing node and a seq of\nchildren, returns a new branch node with the supplied children.\nroot is the root node.",
   :namespace "clojure.zip",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.zip-api.html#clojure.zip/zipper"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/2cc37bb56a9125a1829c73c505e32995e663059a/src/clj/clojure/core/protocols.clj",
   :name "CollReduce",
   :file "src/clj/clojure/core/protocols.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/2cc37bb56a9125a1829c73c505e32995e663059a/src/clj/clojure/core/protocols.clj#L13",
   :line 13,
   :var-type "protocol",
   :arglists nil,
   :doc
   "Protocol for collection types that can implement reduce faster than\nfirst/next recursion. Called by clojure.core/reduce. Baseline\nimplementation defined in terms of Iterable.",
   :namespace "clojure.core.protocols",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.protocols/CollReduce"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/2cc37bb56a9125a1829c73c505e32995e663059a/src/clj/clojure/core/protocols.clj",
   :name "Datafiable",
   :file "src/clj/clojure/core/protocols.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/2cc37bb56a9125a1829c73c505e32995e663059a/src/clj/clojure/core/protocols.clj#L182",
   :line 182,
   :var-type "protocol",
   :arglists nil,
   :doc nil,
   :namespace "clojure.core.protocols",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.protocols/Datafiable"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/2cc37bb56a9125a1829c73c505e32995e663059a/src/clj/clojure/core/protocols.clj",
   :name "IKVReduce",
   :file "src/clj/clojure/core/protocols.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/2cc37bb56a9125a1829c73c505e32995e663059a/src/clj/clojure/core/protocols.clj#L175",
   :line 175,
   :var-type "protocol",
   :arglists nil,
   :doc
   "Protocol for concrete associative types that can reduce themselves\nvia a function of key and val faster than first/next recursion over map\nentries. Called by clojure.core/reduce-kv, and has same\nsemantics (just different arg order).",
   :namespace "clojure.core.protocols",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.protocols/IKVReduce"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/2cc37bb56a9125a1829c73c505e32995e663059a/src/clj/clojure/core/protocols.clj",
   :name "InternalReduce",
   :file "src/clj/clojure/core/protocols.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/2cc37bb56a9125a1829c73c505e32995e663059a/src/clj/clojure/core/protocols.clj#L19",
   :line 19,
   :var-type "protocol",
   :arglists nil,
   :doc
   "Protocol for concrete seq types that can reduce themselves\nfaster than first/next recursion. Called by clojure.core/reduce.",
   :namespace "clojure.core.protocols",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.protocols/InternalReduce"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/2cc37bb56a9125a1829c73c505e32995e663059a/src/clj/clojure/core/protocols.clj",
   :name "Navigable",
   :file "src/clj/clojure/core/protocols.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/2cc37bb56a9125a1829c73c505e32995e663059a/src/clj/clojure/core/protocols.clj#L194",
   :line 194,
   :var-type "protocol",
   :arglists nil,
   :doc nil,
   :namespace "clojure.core.protocols",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.protocols/Navigable"}
  {:raw-source-url nil,
   :name "coll-reduce",
   :file nil,
   :source-url nil,
   :var-type "function",
   :arglists ([coll f] [coll f val]),
   :doc nil,
   :namespace "clojure.core.protocols",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.protocols/coll-reduce"}
  {:raw-source-url nil,
   :name "datafy",
   :file nil,
   :source-url nil,
   :var-type "function",
   :arglists ([o]),
   :doc "return a representation of o as data (default identity)",
   :namespace "clojure.core.protocols",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.protocols/datafy"}
  {:raw-source-url nil,
   :name "kv-reduce",
   :file nil,
   :source-url nil,
   :var-type "function",
   :arglists ([amap f init]),
   :doc nil,
   :namespace "clojure.core.protocols",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.protocols/kv-reduce"}
  {:raw-source-url nil,
   :name "internal-reduce",
   :file nil,
   :source-url nil,
   :var-type "function",
   :arglists ([seq f start]),
   :doc nil,
   :namespace "clojure.core.protocols",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.protocols/internal-reduce"}
  {:raw-source-url nil,
   :name "nav",
   :file nil,
   :source-url nil,
   :var-type "function",
   :arglists ([coll k v]),
   :doc
   "return (possibly transformed) v in the context of coll and k (a key/index or nil),\ndefaults to returning v.",
   :namespace "clojure.core.protocols",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.protocols/nav"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj",
   :name "->Cat",
   :file "src/clj/clojure/core/reducers.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj#L230",
   :line 230,
   :var-type "function",
   :arglists ([cnt left right]),
   :doc
   "Positional factory function for class clojure.core.reducers.Cat.",
   :namespace "clojure.core.reducers",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.reducers/->Cat"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj",
   :added "1.5",
   :name "append!",
   :file "src/clj/clojure/core/reducers.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj#L275",
   :line 275,
   :var-type "function",
   :arglists ([acc x]),
   :doc ".adds x to acc and returns acc",
   :namespace "clojure.core.reducers",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.reducers/append!"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj",
   :added "1.5",
   :name "cat",
   :file "src/clj/clojure/core/reducers.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj#L255",
   :line 255,
   :var-type "function",
   :arglists ([] [ctor] [left right]),
   :doc
   "A high-performance combining fn that yields the catenation of the\nreduced values. The result is reducible, foldable, seqable and\ncounted, providing the identity collections are reducible, seqable\nand counted. The single argument version will build a combining fn\nwith the supplied identity constructor. Tests for identity\nwith (zero? (count x)). See also foldcat.",
   :namespace "clojure.core.reducers",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.reducers/cat"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj",
   :added "1.5",
   :name "drop",
   :file "src/clj/clojure/core/reducers.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj#L215",
   :line 215,
   :var-type "function",
   :arglists ([n] [n coll]),
   :doc "Elides the first n values from the reduction of coll.",
   :namespace "clojure.core.reducers",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.reducers/drop"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj",
   :added "1.5",
   :name "filter",
   :file "src/clj/clojure/core/reducers.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj#L154",
   :line 154,
   :var-type "function",
   :arglists ([pred] [pred coll]),
   :doc
   "Retains values in the reduction of coll for which (pred val)\nreturns logical true. Foldable.",
   :namespace "clojure.core.reducers",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.reducers/filter"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj",
   :added "1.5",
   :name "flatten",
   :file "src/clj/clojure/core/reducers.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj#L174",
   :line 174,
   :var-type "function",
   :arglists ([] [coll]),
   :doc
   "Takes any nested combination of sequential things (lists, vectors,\netc.) and returns their contents as a single, flat foldable\ncollection.",
   :namespace "clojure.core.reducers",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.reducers/flatten"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj",
   :added "1.5",
   :name "fold",
   :file "src/clj/clojure/core/reducers.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj#L51",
   :line 51,
   :var-type "function",
   :arglists
   ([reducef coll] [combinef reducef coll] [n combinef reducef coll]),
   :doc
   "Reduces a collection using a (potentially parallel) reduce-combine\nstrategy. The collection is partitioned into groups of approximately\nn (default 512), each of which is reduced with reducef (with a seed\nvalue obtained by calling (combinef) with no arguments). The results\nof these reductions are then reduced with combinef (default\nreducef). combinef must be associative, and, when called with no\narguments, (combinef) must produce its identity element. These\noperations may be performed in parallel, but the results will\npreserve order.",
   :namespace "clojure.core.reducers",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.reducers/fold"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj",
   :added "1.5",
   :name "foldcat",
   :file "src/clj/clojure/core/reducers.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj#L281",
   :line 281,
   :var-type "function",
   :arglists ([coll]),
   :doc "Equivalent to (fold cat append! coll)",
   :namespace "clojure.core.reducers",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.reducers/foldcat"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj",
   :added "1.5",
   :name "folder",
   :file "src/clj/clojure/core/reducers.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj#L81",
   :line 81,
   :var-type "function",
   :arglists ([coll xf]),
   :doc
   "Given a foldable collection, and a transformation function xf,\nreturns a foldable collection, where any supplied reducing\nfn will be transformed by xf. xf is a function of reducing fn to\nreducing fn.",
   :namespace "clojure.core.reducers",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.reducers/folder"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj",
   :added "1.5",
   :name "map",
   :file "src/clj/clojure/core/reducers.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj#L128",
   :line 128,
   :var-type "function",
   :arglists ([f] [f coll]),
   :doc "Applies f to every value in the reduction of coll. Foldable.",
   :namespace "clojure.core.reducers",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.reducers/map"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj",
   :added "1.5",
   :name "mapcat",
   :file "src/clj/clojure/core/reducers.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj#L138",
   :line 138,
   :var-type "function",
   :arglists ([f] [f coll]),
   :doc
   "Applies f to every value in the reduction of coll, concatenating the result\ncolls of (f val). Foldable.",
   :namespace "clojure.core.reducers",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.reducers/mapcat"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj",
   :added "1.5",
   :name "monoid",
   :file "src/clj/clojure/core/reducers.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj#L287",
   :line 287,
   :var-type "function",
   :arglists ([op ctor]),
   :doc
   "Builds a combining fn out of the supplied operator and identity\nconstructor. op must be associative and ctor called with no args\nmust return an identity value for it.",
   :namespace "clojure.core.reducers",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.reducers/monoid"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj",
   :name "reduce",
   :file "src/clj/clojure/core/reducers.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj#L38",
   :line 38,
   :var-type "function",
   :arglists ([f coll] [f init coll]),
   :doc
   "Like core/reduce except:\nWhen init is not provided, (f) is used.\nMaps are reduced with reduce-kv",
   :namespace "clojure.core.reducers",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.reducers/reduce"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj",
   :added "1.5",
   :name "reducer",
   :file "src/clj/clojure/core/reducers.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj#L67",
   :line 67,
   :var-type "function",
   :arglists ([coll xf]),
   :doc
   "Given a reducible collection, and a transformation function xf,\nreturns a reducible collection, where any supplied reducing\nfn will be transformed by xf. xf is a function of reducing fn to\nreducing fn.",
   :namespace "clojure.core.reducers",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.reducers/reducer"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj",
   :added "1.5",
   :name "remove",
   :file "src/clj/clojure/core/reducers.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj#L167",
   :line 167,
   :var-type "function",
   :arglists ([pred] [pred coll]),
   :doc
   "Removes values in the reduction of coll for which (pred val)\nreturns logical true. Foldable.",
   :namespace "clojure.core.reducers",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.reducers/remove"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj",
   :added "1.5",
   :name "take",
   :file "src/clj/clojure/core/reducers.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj#L201",
   :line 201,
   :var-type "function",
   :arglists ([n] [n coll]),
   :doc "Ends the reduction of coll after consuming n values.",
   :namespace "clojure.core.reducers",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.reducers/take"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj",
   :added "1.5",
   :name "take-while",
   :file "src/clj/clojure/core/reducers.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/c8752b4fbf317e9715b2d94cfb6b6939631a9bcb/src/clj/clojure/core/reducers.clj#L189",
   :line 189,
   :var-type "function",
   :arglists ([pred] [pred coll]),
   :doc
   "Ends the reduction of coll when (pred val) returns logical false.",
   :namespace "clojure.core.reducers",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.reducers/take-while"}
  {:name "Cat",
   :var-type "type",
   :namespace "clojure.core.reducers",
   :arglists nil,
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.reducers/Cat",
   :source-url nil,
   :raw-source-url nil,
   :file nil}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/fc98f92c76254c5a6306debaf0f9df28c3bb3646/src/clj/clojure/core/server.clj",
   :added "1.10",
   :name "io-prepl",
   :file "src/clj/clojure/core/server.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/fc98f92c76254c5a6306debaf0f9df28c3bb3646/src/clj/clojure/core/server.clj#L272",
   :line 272,
   :var-type "function",
   :arglists ([& {:keys [valf], :or {valf pr-str}}]),
   :doc
   "prepl bound to *in* and *out*, suitable for use with e.g. server/repl (socket-repl).\n:ret and :tap vals will be processed by valf, a fn of one argument\nor a symbol naming same (default pr-str)\n\nAlpha, subject to change.",
   :namespace "clojure.core.server",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.server/io-prepl"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/fc98f92c76254c5a6306debaf0f9df28c3bb3646/src/clj/clojure/core/server.clj",
   :added "1.10",
   :name "prepl",
   :file "src/clj/clojure/core/server.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/fc98f92c76254c5a6306debaf0f9df28c3bb3646/src/clj/clojure/core/server.clj#L191",
   :line 191,
   :var-type "function",
   :arglists ([in-reader out-fn & {:keys [stdin]}]),
   :doc
   "a REPL with structured output (for programs)\nreads forms to eval from in-reader (a LineNumberingPushbackReader)\nClosing the input or passing the form :repl/quit will cause it to return\n\nCalls out-fn with data, one of:\n{:tag :ret\n :val val ;;eval result, or Throwable->map data if exception thrown\n :ns ns-name-string\n :ms long ;;eval time in milliseconds\n :form string ;;iff successfully read\n :exception true ;;iff exception thrown\n}\n{:tag :out\n :val string} ;chars from during-eval *out*\n{:tag :err\n :val string} ;chars from during-eval *err*\n{:tag :tap\n :val val} ;values from tap>\n\nYou might get more than one :out or :err per eval, but exactly one :ret\ntap output can happen at any time (i.e. between evals)\nIf during eval an attempt is made to read *in* it will read from in-reader unless :stdin is supplied\n\nAlpha, subject to change.",
   :namespace "clojure.core.server",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.server/prepl"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/fc98f92c76254c5a6306debaf0f9df28c3bb3646/src/clj/clojure/core/server.clj",
   :added "1.10",
   :name "remote-prepl",
   :file "src/clj/clojure/core/server.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/fc98f92c76254c5a6306debaf0f9df28c3bb3646/src/clj/clojure/core/server.clj#L295",
   :line 295,
   :var-type "function",
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
        [p1__6786# p2__6787#]
        (read p1__6786# false p2__6787#))}}]),
   :doc
   "Implements a prepl on in-reader and out-fn by forwarding to a\nremote [io-]prepl over a socket.  Messages will be read by readf, a\nfn of a LineNumberingPushbackReader and EOF value or a symbol naming\nsame (default #(read %1 false %2)),\n:ret and :tap vals will be processed by valf, a fn of one argument\nor a symbol naming same (default read-string). If that function\nthrows, :val will be unprocessed.\n\nAlpha, subject to change.",
   :namespace "clojure.core.server",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.server/remote-prepl"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/fc98f92c76254c5a6306debaf0f9df28c3bb3646/src/clj/clojure/core/server.clj",
   :name "repl",
   :file "src/clj/clojure/core/server.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/fc98f92c76254c5a6306debaf0f9df28c3bb3646/src/clj/clojure/core/server.clj#L180",
   :line 180,
   :var-type "function",
   :arglists ([]),
   :doc "REPL with predefined hooks for attachable socket server.",
   :namespace "clojure.core.server",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.server/repl"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/fc98f92c76254c5a6306debaf0f9df28c3bb3646/src/clj/clojure/core/server.clj",
   :name "repl-init",
   :file "src/clj/clojure/core/server.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/fc98f92c76254c5a6306debaf0f9df28c3bb3646/src/clj/clojure/core/server.clj#L163",
   :line 163,
   :var-type "function",
   :arglists ([]),
   :doc
   "Initialize repl in user namespace and make standard repl requires.",
   :namespace "clojure.core.server",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.server/repl-init"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/fc98f92c76254c5a6306debaf0f9df28c3bb3646/src/clj/clojure/core/server.clj",
   :name "repl-read",
   :file "src/clj/clojure/core/server.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/fc98f92c76254c5a6306debaf0f9df28c3bb3646/src/clj/clojure/core/server.clj#L169",
   :line 169,
   :var-type "function",
   :arglists ([request-prompt request-exit]),
   :doc "Enhanced :read hook for repl supporting :repl/quit.",
   :namespace "clojure.core.server",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.server/repl-read"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/fc98f92c76254c5a6306debaf0f9df28c3bb3646/src/clj/clojure/core/server.clj",
   :name "start-server",
   :file "src/clj/clojure/core/server.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/fc98f92c76254c5a6306debaf0f9df28c3bb3646/src/clj/clojure/core/server.clj#L84",
   :line 84,
   :var-type "function",
   :arglists ([opts]),
   :doc
   "Start a socket server given the specified opts:\n :address Host or address, string, defaults to loopback address\n :port Port, integer, required\n :name Name, required\n :accept Namespaced symbol of the accept function to invoke, required\n :args Vector of args to pass to accept function\n :bind-err Bind *err* to socket out stream?, defaults to true\n :server-daemon Is server thread a daemon?, defaults to true\n :client-daemon Are client threads daemons?, defaults to true\nReturns server socket.",
   :namespace "clojure.core.server",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.server/start-server"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/fc98f92c76254c5a6306debaf0f9df28c3bb3646/src/clj/clojure/core/server.clj",
   :name "start-servers",
   :file "src/clj/clojure/core/server.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/fc98f92c76254c5a6306debaf0f9df28c3bb3646/src/clj/clojure/core/server.clj#L157",
   :line 157,
   :var-type "function",
   :arglists ([system-props]),
   :doc "Start all servers specified in the system properties.",
   :namespace "clojure.core.server",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.server/start-servers"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/fc98f92c76254c5a6306debaf0f9df28c3bb3646/src/clj/clojure/core/server.clj",
   :name "stop-server",
   :file "src/clj/clojure/core/server.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/fc98f92c76254c5a6306debaf0f9df28c3bb3646/src/clj/clojure/core/server.clj#L125",
   :line 125,
   :var-type "function",
   :arglists ([] [name]),
   :doc
   "Stop server with name or use the server-name from *session* if none supplied.\nReturns true if server stopped successfully, nil if not found, or throws if\nthere is an error closing the socket.",
   :namespace "clojure.core.server",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.server/stop-server"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/fc98f92c76254c5a6306debaf0f9df28c3bb3646/src/clj/clojure/core/server.clj",
   :name "stop-servers",
   :file "src/clj/clojure/core/server.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/fc98f92c76254c5a6306debaf0f9df28c3bb3646/src/clj/clojure/core/server.clj#L139",
   :line 139,
   :var-type "function",
   :arglists ([]),
   :doc "Stop all servers ignores all errors, and returns nil.",
   :namespace "clojure.core.server",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.core-api.html#clojure.core.server/stop-servers"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/d9f3f83182e146525a78cf638f0613487d7e18c6/src/clj/clojure/test/junit.clj",
   :added "1.1",
   :name "with-junit-output",
   :file "src/clj/clojure/test/junit.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/d9f3f83182e146525a78cf638f0613487d7e18c6/src/clj/clojure/test/junit.clj#L182",
   :line 182,
   :var-type "macro",
   :arglists ([& body]),
   :doc
   "Execute body with modified test-is reporting functions that write\nJUnit-compatible XML output.",
   :namespace "clojure.test.junit",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test.junit/with-junit-output"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/153a2d0f000ab2f254704e4970968fee6a0329a1/src/clj/clojure/test/tap.clj",
   :added "1.1",
   :name "print-tap-diagnostic",
   :file "src/clj/clojure/test/tap.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/153a2d0f000ab2f254704e4970968fee6a0329a1/src/clj/clojure/test/tap.clj#L51",
   :line 51,
   :var-type "function",
   :arglists ([data]),
   :doc
   "Prints a TAP diagnostic line.  data is a (possibly multi-line)\nstring.",
   :namespace "clojure.test.tap",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test.tap/print-tap-diagnostic"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/153a2d0f000ab2f254704e4970968fee6a0329a1/src/clj/clojure/test/tap.clj",
   :added "1.1",
   :name "print-tap-fail",
   :file "src/clj/clojure/test/tap.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/153a2d0f000ab2f254704e4970968fee6a0329a1/src/clj/clojure/test/tap.clj#L65",
   :line 65,
   :var-type "function",
   :arglists ([msg]),
   :doc
   "Prints a TAP 'not ok' line.  msg is a string, with no line breaks",
   :namespace "clojure.test.tap",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test.tap/print-tap-fail"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/153a2d0f000ab2f254704e4970968fee6a0329a1/src/clj/clojure/test/tap.clj",
   :added "1.1",
   :name "print-tap-pass",
   :file "src/clj/clojure/test/tap.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/153a2d0f000ab2f254704e4970968fee6a0329a1/src/clj/clojure/test/tap.clj#L59",
   :line 59,
   :var-type "function",
   :arglists ([msg]),
   :doc
   "Prints a TAP 'ok' line.  msg is a string, with no line breaks",
   :namespace "clojure.test.tap",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test.tap/print-tap-pass"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/153a2d0f000ab2f254704e4970968fee6a0329a1/src/clj/clojure/test/tap.clj",
   :added "1.1",
   :name "print-tap-plan",
   :file "src/clj/clojure/test/tap.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/153a2d0f000ab2f254704e4970968fee6a0329a1/src/clj/clojure/test/tap.clj#L45",
   :line 45,
   :var-type "function",
   :arglists ([n]),
   :doc
   "Prints a TAP plan line like '1..n'.  n is the number of tests",
   :namespace "clojure.test.tap",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test.tap/print-tap-plan"}
  {:raw-source-url
   "https://github.com/clojure/clojure/raw/153a2d0f000ab2f254704e4970968fee6a0329a1/src/clj/clojure/test/tap.clj",
   :added "1.1",
   :name "with-tap-output",
   :file "src/clj/clojure/test/tap.clj",
   :source-url
   "https://github.com/clojure/clojure/blob/153a2d0f000ab2f254704e4970968fee6a0329a1/src/clj/clojure/test/tap.clj#L117",
   :line 117,
   :var-type "macro",
   :arglists ([& body]),
   :doc
   "Execute body with modified test reporting functions that produce\nTAP output",
   :namespace "clojure.test.tap",
   :wiki-url
   "https://clojure.github.io/clojure//clojure.test-api.html#clojure.test.tap/with-tap-output"})}
