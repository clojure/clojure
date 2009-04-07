This library, clojure-contrib, has a dependency on Clojure: the clojure-lang JAR file. This is needed to compile the Clojure classes.
Normally, it is specified using -Dclojure.jar=<path>.

The nightly-build and stable-build targets are intended for use on the Tapestry360 continuous integration server
(http://tapestry.formos.com/bamboo). They require the presense of the Maven Ant Tasks in the Ant lib folder.

