rm -Rf coclojure
mkdir coclojure
j2objc -d coclojure  -classpath target/classes src/jvm/clojure/*
j2objc -d coclojure  -classpath target/classes src/jvm/clojure/*/*
j2objc -d coclojure  -classpath target/classes src/jvm/clojure/*/*/*
j2objc -d coclojure  -classpath target/classes src/jvm/clojure/*/*/*/*

j2objc -d coclojure  -classpath target/classes target/src/clojure/*
j2objc -d coclojure  -classpath target/classes target/src/clojure/*/*
j2objc -d coclojure  -classpath target/classes target/src/clojure/*/*/*
j2objc -d coclojure  -classpath target/classes target/src/clojure/*/*/*/*
