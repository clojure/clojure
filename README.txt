= Clojure-contrib =

The user contributions library, clojure.contrib, is a collection of
namespaces each of which implements features that we believe may be
useful to a large part of the Clojure community.

Clojure-contrib is open source under the Eclipse Public License and is
copyrighted by Rich Hickey and the various contributors.

Download releases from
http://code.google.com/p/clojure-contrib/downloads

The official source repository for clojure-contrib is
http://github.com/richhickey/clojure-contrib

Documentation and APIs are available at
http://richhickey.github.com/clojure-contrib/

Issues are maintained in the Assembla space at
http://www.assembla.com/spaces/clojure-contrib

General discussion occurs in the Clojure Google group at
http://groups.google.com/group/clojure
and developer discussions are in the Clojure Dev Google group at
http://groups.google.com/group/clojure-dev

Compiled JARs of development snapshots are available at
http://build.clojure.org/



= Building Clojure-contrib =

If you downloaded a release distribution or pre-compiled JAR, you
don't need to do anything.

If you downloaded the sources from Github, you will need Apache Maven
(2.0 or higher) to run the build.  See http://maven.apache.org/

Run the following command in this directory:

    mvn package

This will produce the file target/clojure-contrib-${VERSION}.jar that
you can add to your Java classpath.

Additional build commands are available:

    mvn clojure:repl
    To start a Clojure REPL (Read-Eval-Print Loop)

    mvn compile
    To compile sources without building a JAR

    mvn test
    To run unit tests

    mvn assembly:assembly
    To build ZIP/tar distributions containing source and JARs

To skip the testing phase when building, add "-Dmaven.test.skip=true"
to the mvn command line.



== Compiling with Local clojure.jar ==

If you want to compile/build with a customized clojure.jar file, use
the following command:

    mvn -Denv=local -Dclojure.jar=/path/to/clojure.jar package

The /path/to/clojure.jar MUST be an absolute path.  It defaults to
${current-dir}/../clojure/clojure.jar if you do not specify it.



= Clojure-contrib Versions =

Versions of clojure-contrib are matched to versions of Clojure.

If you are using Clojure 1.0, use clojure-contrib 1.0.*

If you are using Clojure 1.1, use clojure-contrib 1.1.*

If you are using Clojure from the "master" branch on Github, use
clojure-contrib from the "master" branch on Github.

If you are using Clojure from the "new" branch on Github, use
clojure-contrib from the "new" branch on Github.



= Clojure-contrib Committers =

The following people are committers to the official clojure-contrib
repositiory:

Tom Faulhaber
Stephen Gilardi
Christophe Grand
Rich Hickey
Konrad Hinsen
Stuart Holloway
Chris Houser
David Miller
Stuart Sierra
Frantisek Sodomka
