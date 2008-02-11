;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;   which can be found in the file CPL.TXT at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(in-ns 'xml)
(clojure/refer 'clojure)

(import '(org.xml.sax ContentHandler Attributes SAXException)
	'(javax.xml.parsers SAXParser SAXParserFactory))

(def *stack*)
(def *current*)
(def *state*) ; :element :chars :between
(def *sb*)

(defstruct element :tag :attrs :content)

(def tag (accessor element :tag))
(def attrs (accessor element :attrs))
(def content (accessor element :content))

(def content-handler
  (new clojure.lang.XMLHandler
       (implement [ContentHandler]
         (startElement [uri local-name q-name #^Attributes atts]
           (let [attrs (fn [ret i]
                           (if (neg? i)
                             ret
                             (recur (assoc ret 
                                      (. clojure.lang.Keyword (intern (symbol (. atts (getQName i)))))
                                      (. atts (getValue i)))
                                    (dec i))))
                 e (struct element 
                           (. clojure.lang.Keyword (intern (symbol q-name)))
                           (when (pos? (. atts (getLength)))
                             (attrs {} (dec (. atts (getLength))))))]
             (set! *stack* (conj *stack* *current*))
             (set! *current* e)
             (set! *state* :element))
           nil)
         (endElement [uri local-name q-name]
           (let [push-content (fn [e c]
                                  (assoc e :content (conj (or (:content e) []) c)))]
             (when (= *state* :chars)
               (set! *current* (push-content *current* (str *sb*))))
             (set! *current* (push-content (peek *stack*) *current*))
             (set! *stack* (pop *stack*))
             (set! *state* :between))
           nil)
         (characters [ch start length]
           (when-not (= *state* :between)
             (when (= *state* :element)
               (set! *sb* (new StringBuilder)))
             (let [#^StringBuilder sb *sb*]
               (. sb (append ch start length))
               (set! *state* :chars)))
           nil))))

(defn parse [s]
  (let [p (.. SAXParserFactory (newInstance) (newSAXParser))]
    (binding [*stack* nil
              *current* (struct element)
	      *state* :between
	      *sb* nil]
      (. p (parse s content-handler))
      ((:content *current*) 0))))

(defn emit-element [e]
  (if (instance? String e)
    (println e)
    (do
      (print (strcat "<" (name (:tag e))))
      (when (:attrs e)
	(doseq attr (:attrs e)
	  (print (strcat " " (name (key attr)) "='" (val attr)"'"))))
      (if (:content e)
	(do
	  (println ">")
	  (doseq c (:content e)
	    (emit-element c))
	  (println (strcat "</" (name (:tag e)) ">")))
	(println "/>")))))

(defn emit [x]
  (println "<?xml version='1.0' encoding='UTF-8'?>")
  (emit-element x))

;(export '(tag attrs content parse element emit emit-element))

;(load-file "/Users/rich/dev/clojure/src/xml.clj")
;(def x (xml/parse "http://arstechnica.com/journals.rssx"))
