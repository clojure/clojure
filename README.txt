Clojure-contrib
---------------

The user contributions library, clojure.contrib, is a collection of
namespaces each of which implements features that we believe may be
useful to a large part of the clojure community.

Clojure-contrib is open source under the Eclipse Public License and is 
copyrighted by Rich Hickey and the various contributors.

The official source repository for clojure-contrib is 
http://github.com/richhickey/clojure-contrib.

Issues are maintained in the Assembla space at http://www.assembla.com/spaces/dashboard/index/clojure-contrib

For the latest documentation of the capabilities and APIs available 
in clojure-contrib, please see http://richhickey.github.com/clojure-contrib/

General discussion occurs in the Clojure Google group (http://groups.google.com/group/clojure )
and developer discussions are in the Clojure Dev Google group 
(http://groups.google.com/group/clojure-dev ).


Building Clojure-contrib
------------------------

To build clojure-contrib, download the files or clone the git repository.

You will need Apache Maven (2.0 or higher) to run the build. Run the
following in this directory:

   mvn package

This will produce a target/clojure-contrib-{VERSION}.jar file that
you can add to your classpath.


Clojure-contrib Versions
------------------------

There are currently two versions of clojure-contrib, stored in separate
branches on github.

The master branch represents the latest and greatest contrib code and
is tied to the master branch of clojure (http://github.com/richhickey/clojure ).
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
