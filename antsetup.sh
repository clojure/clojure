#!/bin/bash

mvn -q dependency:build-classpath -Dmdep.outputFile=maven-classpath
cat <<EOF >maven-classpath.properties 
maven.compile.classpath=`cat maven-classpath`
EOF
echo "Wrote maven-classpath.properties for standalone ant use"
