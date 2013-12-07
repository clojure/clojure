j2objc -d coclojure  -classpath target/classes src/jvm/clojure/lang/*
j2objc -d coclojure -classpath target/classes src/jvm/clojure/api/*
j2objc -d coclojure -classpath target/classes:target/gen target/gen/clojure/*.java
