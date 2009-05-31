;;; load_all.clj - loads all contrib libraries for testing purposes

;; by Stuart Sierra, http://stuartsierra.com/
;; February 21, 2009

;; Copyright (c) Stuart Sierra, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.


;; This file simple attempts to "require" every library in
;; clojure-contrib.  The names of all contrib libs (minus the
;; "clojure.contrib" part) are in *all-contrib-libs*.  Libraries which
;; throw errors when loading under the latest trunk SVN revisions of
;; Clojure and clojure-contrib are commented out.
;;
;; This is only intended to check that the libraries will load without
;; errors, not that they work correctly.  If the libraries have tests
;; defined using test-is, you can run them with:
;;
;; (clojure.contrib.test-is/run-all-tests)
;;
;; If you write a new lib, please add it to the list in this file.


(ns clojure.contrib.load-all)

(def *all-contrib-libs* '[
accumulators
apply-macro
combinatorics
command-line
complex-numbers
cond
condt
def
duck-streams
error-kit
except
fcase
generic
generic.arithmetic
generic.collection
generic.comparison
generic.functor
generic.math-functions
import-static
jar
;; javadoc - moved to repl-utils
javadoc.browse
;; javalog
json.read
json.write
lazy-seqs
lazy-xml
macro-utils
macros
math
miglayout
mmap
monads
ns-utils
pprint
probabilities.finite-distributions
probabilities.monte-carlo
probabilities.random-numbers
prxml
repl-ln
repl-utils
seq-utils
server-socket
set
shell-out
sql
stacktrace
str-utils
stream-utils
swing-utils
template
test-is
test-is.tests
test-clojure
test-clojure.agents
test-clojure.atoms
test-clojure.clojure-main
test-clojure.clojure-set
test-clojure.clojure-xml
test-clojure.clojure-zip
test-clojure.compilation
test-clojure.control
test-clojure.data-structures
test-clojure.evaluation
test-clojure.for
test-clojure.java-interop
test-clojure.logic
test-clojure.macros
test-clojure.metadata
test-clojure.multimethods
test-clojure.ns-libs
test-clojure.numbers
test-clojure.other-functions
test-clojure.parallel
test-clojure.predicates
test-clojure.printer
test-clojure.reader
test-clojure.refs
test-clojure.sequences
test-clojure.special
test-clojure.vars
test-contrib
test-contrib.shell-out
test-contrib.str-utils
trace
types
walk
zip-filter
])

(doseq [name *all-contrib-libs*]
  (require (symbol (str "clojure.contrib." name))))
