;; java 5 annotation tests
(in-ns 'lava.test-lava.annotations)

(import [java.lang.annotation Annotation Retention RetentionPolicy Target ElementType])
(definterface Foo (foo []))

(deftype #^{Deprecated true
            Retention RetentionPolicy/RUNTIME}
  Bar [#^int a
       #^{:tag int
          Deprecated true
          Retention RetentionPolicy/RUNTIME} b]
  Foo (#^{Deprecated true
          Retention RetentionPolicy/RUNTIME}
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

