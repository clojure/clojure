;; java 6 annotation tests
(in-ns 'clojure.test-clojure.annotations)

(import [java.lang.annotation Annotation Retention RetentionPolicy Target ElementType]
        [javax.xml.ws WebServiceRef WebServiceRefs])
(definterface Foo (foo []))

(deftype #^{Deprecated true
            Retention RetentionPolicy/RUNTIME
            javax.annotation.processing.SupportedOptions ["foo" "bar" "baz"]
            javax.xml.ws.soap.Addressing {:enabled false :required true}
            WebServiceRefs [(WebServiceRef {:name "fred" :type String})
                            (WebServiceRef {:name "ethel" :mappedName "lucy"})]}
  Bar [#^int a
       #^{:tag int
          Deprecated true
          Retention RetentionPolicy/RUNTIME
          javax.annotation.processing.SupportedOptions ["foo" "bar" "baz"]
            javax.xml.ws.soap.Addressing {:enabled false :required true}
          WebServiceRefs [(WebServiceRef {:name "fred" :type String})
                            (WebServiceRef {:name "ethel" :mappedName "lucy"})]}
       b]
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
      {:annotationType (.annotationType o)}
      (map
       (fn [m] [(keyword (.getName m)) (annotation->map (.invoke m o nil))])
       data-methods)))
   (or (sequential? o) (.isArray (class o)))
   (map annotation->map o)
     :else o))

(def expected-annotations
  #{{:annotationType java.lang.annotation.Retention, :value RetentionPolicy/RUNTIME}
    {:annotationType javax.xml.ws.WebServiceRefs,
     :value [{:annotationType javax.xml.ws.WebServiceRef, :name "fred", :mappedName "", :type java.lang.String, :wsdlLocation "", :value java.lang.Object}
             {:annotationType javax.xml.ws.WebServiceRef, :name "ethel", :mappedName "lucy", :type java.lang.Object, :wsdlLocation "", :value java.lang.Object}]}
    {:annotationType javax.xml.ws.soap.Addressing, :enabled false, :required true}
    {:annotationType javax.annotation.processing.SupportedOptions, :value ["foo" "bar" "baz"]}
    {:annotationType java.lang.Deprecated}})

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

(gen-class :name foo.Bar
           :extends clojure.lang.Box
           :constructors {^{Deprecated true} [Object] [Object]}
           :init init
           :prefix "foo")

(defn foo-init [obj]
  [[obj] nil])

(deftest test-annotations-on-constructor
  (is (some #(instance? Deprecated %)
            (for [ctor (.getConstructors (Class/forName "foo.Bar"))
                  annotation (.getAnnotations ctor)]
              annotation))))
