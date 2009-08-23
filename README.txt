Clojure-contrib
---------------

The user contributions library, clojure.contrib, is a collection of
namespaces each of which implements features that we believe may be
useful to a large part of the clojure community.

Clojure-contrib is open source under the Eclipse Public License and is 
copyrighted by Rich Hickey and the various contributors.

The official repository for clojure-contrib is 
http://github.com/richhickey/clojure-contrib.

For the latest documentation of the capabilities and APIs available 
in clojure-contrib, please see http://richhickey.github.com/clojure-contrib/


Building Clojure-contrib
------------------------

To build clojure-contrib, download the files or clone the git repository.

This library, clojure-contrib, has a dependency on Clojure: the clojure-lang
JAR file. This is needed to compile the Clojure classes. Normally, it is specified
using -Dclojure.jar=<path>.

The build works as follows:

ant -Dclojure.jar=<path>

This will produce a clojure-contrib.jar file that you can add to your classpath.

The nightly-build and stable-build targets are intended for use on the
Tapestry360 continuous integration server (http://tapestry.formos.com/bamboo).
They require the presense of the Maven Ant Tasks in the Ant lib folder.


Clojure-contrib Versions
------------------------

There are currently two versions of clojure-contrib, stored in separate
branches on github.

The master branch represents the latest and greatest contrib code and
is tied to the master branch of clojure (http://github.com/richhickey/clojure).
If you're using this branch, you'll want to be sure to keep your clojure
and clojure-contrib code in sync, though breaking changes between them
are pretty rare in practice.

The clojure-1.0-compatible branch is designed to stay compatible with
clojure release 1.0. New contrib features may be added to this branch, at
the discretion of the contributors, but not necessarily. If you wish to 
stay on a "standard release" of clojure, this is the branch for you. 


Clojure-contrib Committers
--------------------------

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
