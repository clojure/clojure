;;; gen-html-docs.clj: Generate HTML documentation for Clojure libs

;; by Craig Andera, http://pluralsight.com/craig, candera@wangdera.com
;; February 13th, 2009

;; Copyright (c) Craig Andera, 2009. All rights reserved.  The use
;; and distribution terms for this software are covered by the Eclipse
;; Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this
;; distribution.  By using this software in any fashion, you are
;; agreeing to be bound by the terms of this license.  You must not
;; remove this notice, or any other, from this software.

;; Generates a single HTML page that contains the documentation for
;; one or more Clojure libraries. See the comments section at the end
;; of this file for usage.

;; TODO
;; 
;; * Make symbols in the source hyperlinks to the appropriate section
;;   of the documentation.
;; * Investigate issue with miglayout mentioned here: 
;;   http://groups.google.com/group/clojure/browse_thread/thread/5a0c4395e44f5a79/3ae483100366bd3d?lnk=gst&q=documentation+browser#3ae483100366bd3d
;;
;; DONE
;;
;; * Move to clojure.contrib
;;   * Change namespace
;;   * Change license as appropriate
;;   * Double-check doc strings
;; * Remove doc strings from source code
;; * Add collapse/expand functionality for all namespaces
;; * Add collapse/expand functionality for each namespace
;; * See if converting to use clojure.contrib.prxml is possible
;; * Figure out why the source doesn't show up for most things
;; * Add collapsible source
;; * Add links at the top to jump to each namespace
;; * Add object type (var, function, whatever)
;; * Add argument lists for functions
;; * Add links at the top of each namespace to jump to members
;; * Add license statement
;; * Remove the whojure dependency

(ns 
  #^{:author "Craig Andera",
     :doc "Generates a single HTML page that contains the documentation for
one or more Clojure libraries."} 
  clojure.contrib.gen-html-docs
  (:require [clojure.contrib.duck-streams :as duck-streams])
  (:use [clojure.contrib seq-utils str-utils repl-utils def prxml])
  (:import [java.lang Exception]
	   [java.util.regex Pattern]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Doc generation constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def *script* " // <![CDATA[

function getElem(id)
{
  if( document.getElementById )
  {
    return document.getElementById( id )
  }
  else if ( document.all )
  {
    return eval( 'document.all.' + id )
  }
  else
    return false;
}

function setDisplayStyle(id,displayStyle)
{
  var elem = getElem (id)
  if (elem)
  {
    elem.style.display = displayStyle
  }

}

function setLinkToggleText (id, text)
{
 var elem = getElem (id)
 if (elem)
 {
   elem.innerHTML = text
 }
}

function collapse(id)
{
  setDisplayStyle (id, 'none')
}

function expand (id)
{
  setDisplayStyle (id, 'block')
}

function toggleSource( id )
{
  toggle(id, 'linkto-' + id, 'Hide Source', 'Show Source')
}

function toggle(targetid, linkid, textWhenOpen, textWhenClosed)
{
  var elem = getElem (targetid)
  var link = getElem (linkid)

  if (elem && link)
  {
    var isOpen = false
    if (elem.style.display == '')
    {
      isOpen = link.innerHTML == textWhenOpen
    }
    else if( elem.style.display == 'block' )
    {
      isOpen = true
    }
    
    if (isOpen)
    {
      elem.style.display = 'none'
      link.innerHTML = textWhenClosed
    }
    else
    {
      elem.style.display = 'block'
      link.innerHTML = textWhenOpen
    }
  }
}

      //]]>
")

(def *style* "
.library
{
  padding: 0.5em 0 0 0 
}
.all-libs-toggle,.library-contents-toggle
{
 font-size: small;
}
.all-libs-toggle a,.library-contents-toggle a
{
 color: white
}
.library-member-doc-whitespace
{
 white-space: pre
}
.library-member-source-toggle
{
  font-size: small;
  margin-top: 0.5em
}
.library-member-source
{
  display: none;
  border-left: solid lightblue 
}
.library-member-docs
{
  font-family:monospace
}
.library-member-arglists
{
  font-family: monospace
}
.library-member-type
{
  font-weight: bold; 
  font-size: small;
  font-style: italic;
  color: darkred
}
.lib-links
{
  margin: 0 0 1em 0
}

.lib-link-header
{
  color: white;
  background: darkgreen;
  width: 100%
}

.library-name 
{ 
  color: white;
  background: darkblue;
  width: 100%
}

.missing-library
{
  color: darkred; 
  margin: 0 0 1em 0 
}

.library-members
{
  list-style: none
}

.library-member-name
{
  font-weight: bold;
  font-size: 105%
}")

(defn- extract-documentation 
  "Pulls the documentation for a var v out and turns it into HTML"
  [v]
  (if-let [docs (:doc (meta v))]
    (map 
     (fn [l] 
       [:div {:class "library-member-doc-line"} 
	(if (= 0 (count l)) 
	  [:span {:class "library-member-doc-whitespace"} " "] ; We need something here to make the blank line show up
	  l)]) 
     (re-split #"\n" docs)) 
    ""))

(defn- member-type 
  "Figures out for a var x whether it's a macro, function, var or multifunction"
  [x]
  (try 
   (let [dx (deref x)] 
     (cond 
      (:macro (meta x)) :macro 
      (fn? dx) :fn 
      (= clojure.lang.MultiFn (:tag (meta x))) :multi 
      true :var))
   (catch Exception e
     :unknown)))

(defn- anchor-for-member 
  "Returns a suitable HTML anchor name given a library id and a member
  id" 
  [libid memberid]
  (str "member-" libid "-" memberid))

(defn- id-for-member-source 
  "Returns a suitable HTML id for a source listing given a library and
  a member"
  [libid memberid]
  (str "membersource-" libid "-" memberid))

(defn- id-for-member-source-link 
  "Returns a suitable HTML id for a link to a source listing given a
  library and a member"
  [libid memberid]
  (str "linkto-membersource-" libid "-" memberid))

(defn- symbol-for 
  "Given a namespace object ns and a namespaceless symbol memberid
  naming a member of that namespace, returns a namespaced symbol that
  identifies that member."
  [ns memberid]
  (symbol (name (ns-name ns)) (name memberid)))

(defn- elide-to-one-line 
  "Elides a string down to one line."
  [s]
  (re-sub #"(\n.*)+" "..." s))

(defn- elide-string 
  "Returns a string that is at most the first limit characters of s"
  [s limit]
  (if (< (- limit 3) (count s))
    (str (subs s 0 (- limit 3)) "...")
    s))

(defn- doc-elided-src 
  "Returns the src with the docs elided."
  [docs src]
  (re-sub (re-pattern (str "\"" (Pattern/quote docs) "\"")) 
	  (str "\""
		  (elide-to-one-line docs)
;; 	          (elide-string docs 10)
;;	          "..."
		  "\"")
	  src))

(defn- format-source [libid memberid v]
  (try
   (let [docs (:doc (meta v)) 
	 src (if-let [ns (find-ns libid)]
	       (get-source (symbol-for ns memberid)))]
     (if (and src docs)
       (doc-elided-src docs src)
       src))
   (catch Exception ex
     nil)))

(defn- generate-lib-member [libid [n v]]
  [:li {:class "library-member"}
   [:a {:name (anchor-for-member libid n)}]
   [:dl {:class "library-member-table"} 
    [:dt {:class "library-member-name"}
     (str n)]
    [:dd 
     [:div {:class "library-member-info"}
      [:span {:class "library-member-type"} (name (member-type v))]
      " "
      [:span {:class "library-member-arglists"} (str (:arglists (meta v)))]]
     (into [:div {:class "library-member-docs"}] (extract-documentation v))
     (let [member-source-id (id-for-member-source libid n)
	   member-source-link-id (id-for-member-source-link libid n)]
       (if-let [member-source (format-source libid n v)] 
	 [:div {:class "library-member-source-section"}
	  [:div {:class "library-member-source-toggle"}
	   "[ "
	   [:a {:href (format "javascript:toggleSource('%s')" member-source-id)
		:id member-source-link-id} "Show Source"]
	   " ]"]	  
	  [:div {:class "library-member-source" :id member-source-id}
	   [:pre member-source]]]))]]])

(defn- anchor-for-library 
  "Given a symbol id identifying a namespace, returns an identifier
suitable for use as the name attribute of an HTML anchor tag."
  [id]
  (str "library-" id))

(defn- generate-lib-member-link 
  "Emits a hyperlink to a member of a namespace given libid (a symbol
identifying the namespace) and the vector [n v], where n is the symbol
naming the member in question and v is the var pointing to the
member." 
  [libid [n v]]
  [:a {:class "lib-member-link" 
       :href (str "#" (anchor-for-member libid n))} (name n)])

(defn- anchor-for-library-contents 
  "Returns an HTML ID that identifies the element that holds the
documentation contents for the specified library."
  [lib]
  (str "library-contents-" lib))

(defn- anchor-for-library-contents-toggle 
  "Returns an HTML ID that identifies the element that toggles the
visibility of the library contents."
  [lib]
  (str "library-contents-toggle-" lib))

(defn- generate-lib-doc 
  "Emits the HTML that documents the namespace identified by the
symbol lib."
  [lib]
  [:div {:class "library"} 
   [:a {:name (anchor-for-library lib)}]
   [:div {:class "library-name"} 
    [:span {:class "library-contents-toggle"} 
     "[ "
     [:a {:id (anchor-for-library-contents-toggle lib) 
	  :href (format "javascript:toggle('%s', '%s', '-', '+')" 
			(anchor-for-library-contents lib)
			(anchor-for-library-contents-toggle lib))} 
      "-"]
     " ] "]
    (name lib)]
   (let [ns (find-ns lib)]
     (if ns 
       (let [lib-members (sort (ns-publics ns))]
	 [:a {:name (anchor-for-library lib)}]
	 [:div {:class "library-contents" :id (anchor-for-library-contents lib)}
	  (into [:div {:class "library-member-links"}]
		(interpose " " (map #(generate-lib-member-link lib %) lib-members)))
	  (into [:ol {:class "library-members"}]
		(map #(generate-lib-member lib %) lib-members))])
       [:div {:class "missing-library library-contents" :id (anchor-for-library-contents lib)} "Could not load library"]))])

(defn- load-lib 
  "Calls require on the library identified by lib, eating any
exceptions."
  [lib]
  (try 
   (require lib)
   (catch java.lang.Exception x
       nil)))

(defn- generate-lib-link 
  "Generates a hyperlink to the documentation for a namespace given
lib, a symbol identifying that namespace."
  [lib]
  (let [ns (find-ns lib)]
    (if ns
      [:a {:class "lib-link" :href (str "#" (anchor-for-library lib))} (str (ns-name ns))])))

(defn- generate-lib-links 
  "Generates the list of hyperlinks to each namespace, given libs, a
vector of symbols naming namespaces."
  [libs]
  (into [:div {:class "lib-links"} 
	 [:div {:class "lib-link-header"} "Namespaces"
	  [:span {:class "all-libs-toggle"} 
	   " [ "
	   [:a {:href "javascript:expandAllNamespaces()"}
	    "Expand All"]
	   " ] [ "
	   [:a {:href "javascript:collapseAllNamespaces()"}
	    "Collapse All"]
	   " ]"]]] 
	(interpose " " (map generate-lib-link libs))))

(defn generate-toggle-namespace-script 
  [action toggle-text lib]
  (str (format "%s('%s');\n" action (anchor-for-library-contents lib))
       (format "setLinkToggleText('%s', '%s');\n" (anchor-for-library-contents-toggle lib) toggle-text)))

(defn generate-all-namespaces-action-script 
  [action toggle-text libs]
  (str (format  "function %sAllNamespaces()" action)
       \newline
       "{"
       \newline
       (reduce str (map #(generate-toggle-namespace-script action toggle-text %) libs))
       \newline
       "}"))

(defn generate-documentation 
  "Returns a string which is the HTML documentation for the libraries
named by libs. Libs is a vector of symbols identifying Clojure
libraries."
  [libs]
  (dorun (map load-lib libs))
  (let [writer (new java.io.StringWriter)]
   (binding [*out* writer] 
     (prxml 
      [:html {:xmlns "http://www.w3.org/1999/xhtml"}
       [:head 
	[:title "Clojure documentation browser"]
	[:style *style*]
	[:script {:language "JavaScript" :type "text/javascript"} [:raw! *script*]]
	
	[:script {:language "JavaScript" :type "text/javascript"}
	 [:raw! "// <![CDATA[!" \newline]
	 (generate-all-namespaces-action-script "expand" "-" libs)
	 (generate-all-namespaces-action-script "collapse" "+" libs)
	 [:raw! \newline "// ]]>"]]]
       (let [lib-vec (sort libs)] 
	 (into [:body (generate-lib-links lib-vec)]
	       (map generate-lib-doc lib-vec)))]))
   (.toString writer)))


(defn generate-documentation-to-file 
  "Calls generate-documentation on the libraries named by libs and
emits the generated HTML to the path named by path."
  [path libs]
  (duck-streams/spit path (generate-documentation libs)))

(comment 
  (generate-documentation-to-file 
   "C:/TEMP/CLJ-DOCS.HTML"
   ['clojure.contrib.accumulators])

  (defn gen-all-docs [] 
    (generate-documentation-to-file 
     "C:/temp/clj-libs.html"
     [
     'clojure.set
     'clojure.main 
     'clojure.core  
     'clojure.zip   
     'clojure.xml
     'clojure.contrib.accumulators
     'clojure.contrib.apply-macro
     'clojure.contrib.auto-agent
     'clojure.contrib.combinatorics
     'clojure.contrib.command-line
     'clojure.contrib.complex-numbers
     'clojure.contrib.cond
     'clojure.contrib.condt
     'clojure.contrib.def
     'clojure.contrib.duck-streams
     'clojure.contrib.enum
     'clojure.contrib.error-kit
     'clojure.contrib.except
     'clojure.contrib.fcase
     'clojure.contrib.generic
     'clojure.contrib.generic.arithmetic
     'clojure.contrib.generic.collection
     'clojure.contrib.generic.comparison
     'clojure.contrib.generic.functor
     'clojure.contrib.generic.math-functions
     'clojure.contrib.import-static
     'clojure.contrib.javadoc
     'clojure.contrib.javalog
     'clojure.contrib.lazy-seqs
     'clojure.contrib.lazy-xml
     'clojure.contrib.macros
     'clojure.contrib.math
     'clojure.contrib.miglayout
     'clojure.contrib.mmap
     'clojure.contrib.monads
     'clojure.contrib.ns-utils
     'clojure.contrib.prxml
     'clojure.contrib.repl-ln
     'clojure.contrib.repl-utils
     'clojure.contrib.seq-utils
     'clojure.contrib.server-socket
     'clojure.contrib.shell-out
     'clojure.contrib.sql
     'clojure.contrib.stacktrace
     'clojure.contrib.stream-utils
     'clojure.contrib.str-utils
     'clojure.contrib.template
     'clojure.contrib.test-clojure
     'clojure.contrib.test-contrib
     'clojure.contrib.test-is
     'clojure.contrib.trace
     'clojure.contrib.types
     'clojure.contrib.walk
     'clojure.contrib.zip-filter
     'clojure.contrib.javadoc.browse
     'clojure.contrib.json.read
     'clojure.contrib.json.write
     'clojure.contrib.lazy-xml.with-pull
     'clojure.contrib.miglayout.internal
     'clojure.contrib.probabilities.finite-distributions
     'clojure.contrib.probabilities.monte-carlo
     'clojure.contrib.probabilities.random-numbers
     'clojure.contrib.sql.internal
     'clojure.contrib.test-clojure.evaluation
     'clojure.contrib.test-clojure.for
     'clojure.contrib.test-clojure.numbers
     'clojure.contrib.test-clojure.printer
     'clojure.contrib.test-clojure.reader
     'clojure.contrib.test-clojure.sequences
     'clojure.contrib.test-contrib.shell-out
     'clojure.contrib.test-contrib.str-utils
     'clojure.contrib.zip-filter.xml
     ]))
  )
