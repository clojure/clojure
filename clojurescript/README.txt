This directory contains work in progress on what may eventually become
ClojureScript.  It currently allows code written in a very small
subset of Clojure to be automatically translated to JavaScript.

tojs.clj is Clojure code to translate Clojure forms to Javascript.  It
was used to generate core.js from clojure's own core.clj and
core-print.clj.

To run any of the tests, do something like:

java -cp ~/build/clojure/clojure.jar:/home/chouser/proj/clojure-contrib/src
clojure.lang.Script tojs.clj -- t03.cljs > t03.js

Now that you've got the .js file, you can test using Rhino:

/usr/bin/java -jar /usr/share/java/js.jar -f rt.js -f core.js -f t03.js

Or point a browser at test.html and choose the test you want to run.

There's plenty more to do.  If you'd like to help, contact the Clojure
Google group: clojure@googlegroups.com

--Chouser
23 Sept 2008
