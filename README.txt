= Clojure-contrib =

The user contributions library, clojure.contrib, is a collection of
namespaces each of which implements features that we believe may be
useful to a large part of the Clojure community.

Clojure-contrib is open source under the Eclipse Public License and is
copyrighted by Rich Hickey and the various contributors.

The official source repository for clojure-contrib is
http://github.com/richhickey/clojure-contrib.

Pre-built binary packages are available at
http://code.google.com/p/clojure-contrib/downloads

Documentation and APIs are available at
http://richhickey.github.com/clojure-contrib/

Issues are maintained in the Assembla space at
http://www.assembla.com/spaces/clojure-contrib

General discussion occurs in the Clojure Google group at
http://groups.google.com/group/clojure
and developer discussions are in the Clojure Dev Google group at
http://groups.google.com/group/clojure-dev



= Building Clojure-contrib =

To build clojure-contrib, download the files or clone the git
repository.

You will need Apache Maven (2.0 or higher) to run the build. 
See http://maven.apache.org/

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

By default, the "package" and "assembly:assembly" targets will fail if
any unit tests fail.  To omit this testing phase from the build, add
"-Dmaven.test.skip=true" to the mvn command line.



= Clojure-contrib Versions =

There are currently two versions of clojure-contrib, stored in
separate branches on github.

The master branch represents the latest and greatest contrib code and
is tied to the master branch of clojure
(http://github.com/richhickey/clojure ).  If you're using this branch,
you'll want to be sure to keep your clojure and clojure-contrib code
in sync, though breaking changes between them are pretty rare in
practice.

The clojure-1.0-compatible branch is designed to stay compatible with
clojure release 1.0. New contrib features may be added to this branch,
at the discretion of the contributors, but not necessarily. If you
wish to stay on a "standard release" of clojure, this is the branch
for you.



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
