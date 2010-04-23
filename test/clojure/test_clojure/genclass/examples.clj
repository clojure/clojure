(ns clojure.test-clojure.genclass.examples)

(definterface ExampleInterface
  (foo [a])
  (foo [a b])
  (foo [a #^int b]))

(gen-class :name clojure.test_clojure.genclass.examples.ExampleClass
           :implements [clojure.test_clojure.genclass.examples.ExampleInterface])

;; -foo-Object unimplemented to test missing fn case

(defn -foo-Object-Object
  [_ o1 o2]
  "foo with o, o")

(defn -foo-Object-int
  [_ o i]
  "foo with o, i")
