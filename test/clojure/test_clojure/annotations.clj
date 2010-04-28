;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;; Authors: Stuart Halloway, Rich Hickey

(ns clojure.test-clojure.annotations
  (:use clojure.test))

(import [java.lang.annotation Annotation Retention RetentionPolicy Target ElementType]
        [javax.xml.ws WebServiceRef WebServiceRefs])
(definterface Foo (foo []))

;annotation on type
(deftype #^{Deprecated true
            Retention RetentionPolicy/RUNTIME
            javax.annotation.processing.SupportedOptions ["foo" "bar" "baz"]
            javax.xml.ws.soap.Addressing {:enabled false :required true}
            WebServiceRefs [(WebServiceRef {:name "fred" :type String})
                            (WebServiceRef {:name "ethel" :mappedName "lucy"})]}
  Bar [#^int a
       ;on field
       #^{:tag int
          Deprecated true
          Retention RetentionPolicy/RUNTIME
          javax.annotation.processing.SupportedOptions ["foo" "bar" "baz"]
          javax.xml.ws.soap.Addressing {:enabled false :required true}
          WebServiceRefs [(WebServiceRef {:name "fred" :type String})
                          (WebServiceRef {:name "ethel" :mappedName "lucy"})]}
       b]
  ;on method
  Foo (#^{Deprecated true
          Retention RetentionPolicy/RUNTIME
          javax.annotation.processing.SupportedOptions ["foo" "bar" "baz"]
          javax.xml.ws.soap.Addressing {:enabled false :required true}
          WebServiceRefs [(WebServiceRef {:name "fred" :type String})
                          (WebServiceRef {:name "ethel" :mappedName "lucy"})]}
       foo [this] 42))

(defn annotation->map
  "Converts a Java annotation (which conceals data)
   into a map (which makes is usable). Not lazy.
   Works recursively. Returns non-annotations unscathed."
  [#^java.lang.annotation.Annotation o]
  (cond
   (instance? Annotation o)
     (let [type (.annotationType o)
           itfs (-> (into #{type} (supers type)) (disj java.lang.annotation.Annotation))
           data-methods (into #{} (mapcat #(.getDeclaredMethods %) itfs))]
       (into
        {:annotation-type (.annotationType o)}
        (map
         (fn [m] [(keyword (.getName m)) (annotation->map (.invoke m o nil))])
        data-methods)))
   (or (sequential? o) (.isArray (class o)))
     (map annotation->map o)
   :else o))

(def expected-annotations
  #{{:annotation-type java.lang.annotation.Retention, :value RetentionPolicy/RUNTIME}
    {:annotation-type javax.xml.ws.WebServiceRefs,
     :value [{:annotation-type javax.xml.ws.WebServiceRef, :name "fred", :mappedName "", :type java.lang.String, :wsdlLocation "", :value java.lang.Object}
             {:annotation-type javax.xml.ws.WebServiceRef, :name "ethel", :mappedName "lucy", :type java.lang.Object, :wsdlLocation "", :value java.lang.Object}]}
    {:annotation-type javax.xml.ws.soap.Addressing, :enabled false, :required true}
    {:annotation-type javax.annotation.processing.SupportedOptions, :value ["foo" "bar" "baz"]}
    {:annotation-type java.lang.Deprecated}})

(deftest test-annotations-on-type
  (is (=
       expected-annotations
       (into #{} (map annotation->map (.getAnnotations Bar))))))

(deftest test-annotations-on-field
  (is (=
       expected-annotations
       (into #{} (map annotation->map (.getAnnotations (.getField Bar "b")))))))

(deftest test-annotations-on-method
  (is (=
       expected-annotations
       (into #{} (map annotation->map (.getAnnotations (.getMethod Bar "foo" nil)))))))


