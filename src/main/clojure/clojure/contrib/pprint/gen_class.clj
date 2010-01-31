;;; gen_class.clj: generate statically-named classes for pprint

(ns clojure.contrib.pprint.gen-class)

(gen-class :name clojure.contrib.pprint.ColumnWriter
           :impl-ns clojure.contrib.pprint.ColumnWriter
           :extends java.io.Writer
           :init init
           :constructors {[java.io.Writer Integer] [], 
                          [java.io.Writer] []}
           :methods [[getColumn [] Integer]
                     [getLine [] Integer]
                     [getMaxColumn [] Integer]
                     [setMaxColumn [Integer] Void]
                     [getWriter [] java.io.Writer]]
           :state state)

(gen-class :name clojure.contrib.pprint.PrettyWriter
           :impl-ns clojure.contrib.pprint.ColumnWriter
           :extends clojure.contrib.pprint.ColumnWriter
           :init init
           :constructors {[java.io.Writer Integer Object] [java.io.Writer Integer]}
           :methods [[startBlock [String String String] void]
                     [endBlock [] void]
                     [newline [clojure.lang.Keyword] void]
                     [indent [clojure.lang.Keyword Integer] void]
                     [getMiserWidth [] Object]
                     [setMiserWidth [Object] void]
                     [setLogicalBlockCallback [clojure.lang.IFn] void]]
           :exposes-methods {write col_write}
           :state pwstate)
