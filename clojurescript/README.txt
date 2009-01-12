This directory contains work in progress on what may eventually become
ClojureScript.  It currently allows code written in a very small
subset of Clojure to be automatically translated to JavaScript.

tojs.clj is Clojure code to translate Clojure forms to Javascript.  It
was used to generate core.js from clojure's own core.clj and
core_print.clj.

To run any of the tests from the command line, do something like:

java -cp ~/build/clojure/clojure.jar:/home/chouser/proj/clojure-contrib/src:src \
  clojure.main src/clojure/contrib/clojurescript/cli.clj -- \
  tests/t03.cljs > t03.js

Now that you've got the .js file, you can test using Rhino:

/usr/bin/java -jar /usr/share/java/js.jar \
  -f src/clojure/contrib/clojurescript/rt.js \
  -f src/clojure/contrib/clojurescript/core.js \
  -f t03.js

There's plenty more to do.  If you'd like to help, contact the Clojure
Google group: clojure@googlegroups.com

--Chouser
12 Jan 2009
