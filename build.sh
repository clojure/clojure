rm -Rf coclojure
mkdir coclojure

j2objc -d coclojure -classpath target/classes ./target/src/*
j2objc -d coclojure -classpath target/classes ./target/src/clojure/*
j2objc -d coclojure -classpath target/classes ./target/src/clojure/core/*
j2objc -d coclojure -classpath target/classes ./target/src/clojure/core/protocols/*
j2objc -d coclojure -classpath target/classes ./target/src/clojure/core/reducers/*
j2objc -d coclojure -classpath target/classes ./target/src/clojure/data/*
j2objc -d coclojure -classpath target/classes ./target/src/clojure/instant/*
j2objc -d coclojure -classpath target/classes ./target/src/clojure/instant/proxy$java/*
j2objc -d coclojure -classpath target/classes ./target/src/clojure/instant/proxy$java/lang/*
j2objc -d coclojure -classpath target/classes ./target/src/clojure/java/*
j2objc -d coclojure -classpath target/classes ./target/src/clojure/java/io/*
j2objc -d coclojure -classpath target/classes ./target/src/clojure/pprint/*
j2objc -d coclojure -classpath target/classes ./target/src/clojure/pprint/proxy$java/*
j2objc -d coclojure -classpath target/classes ./target/src/clojure/pprint/proxy$java/io/*
j2objc -d coclojure -classpath target/classes ./target/src/clojure/test/*
j2objc -d coclojure -classpath target/classes ./target/src/clojure/test/generative/*
j2objc -d coclojure -classpath target/classes ./target/src/clojure/test/generative/event/*
j2objc -d coclojure -classpath target/classes ./target/src/clojure/test/generative/runner/*
#j2objc -d coclojure -classpath target/classes ./target/src/clojure/test_clojure/*
j2objc -d coclojure -classpath target/classes ./target/src/clojure/tools/*
j2objc -d coclojure -classpath target/classes ./target/src/clojure/xml/*
j2objc -d coclojure -classpath target/classes ./target/src/clojure/xml/proxy$java/*
j2objc -d coclojure -classpath target/classes ./target/src/clojure/xml/proxy$java/lang/*

cp src/resources/clojure/version.properties coclojure/clojure/
