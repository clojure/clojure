(ns clojure.contrib.test-prxml
  (:use clojure.test clojure.contrib.prxml))

(deftest prxml-basic
  (is (= "<p>Hello, World!</p>"
         (with-out-str (prxml [:p "Hello, World!"])))))

(deftest prxml-escaping
  (is (= "<a href=\"foo&amp;bar\">foo&lt;bar</a>"
         (with-out-str (prxml [:a {:href "foo&bar"} "foo<bar"])))))
