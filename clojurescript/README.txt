This directory contains work in progress on what may eventually become
ClojureScript.  If it worked, it would allow code written in a subset
of Clojure to be automatically translated to JavaScript.

The .js files are hand-written ports of Java included in Clojure, or
hand-written tests for the above.

tojs.clj is Clojure code to translate Clojure forms to Javascript.

clojurescript-compiler.patch is a patch file to add features to
Clojure that are required by tojs.clj.

There's plenty more to do.  If you'd like to help, contact the Clojure
Google group: clojure@googlegroups.com

--Chouser
12 Sept 2008
