#!/bin/bash

if [ -z $3 ]; then
  echo 'Usage: 
        (checkout onto correct branch)
        (edit  changes.txt)
        (optional: set CLOJURE_DEPLOY_URL for nonstandard location)
        release.sh <major> <minor> <incremental> (qualifier)'
  exit 0
fi

MAJOR_VERSION=$1
MINOR_VERSION=$2
INCREMENTAL_VERSION=$3
QUALIFIER=$4

echo "clojure.version.major=$MAJOR_VERSION
clojure.version.minor=$MINOR_VERSION
clojure.version.incremental=$INCREMENTAL_VERSION
clojure.version.qualifier=$QUALIFIER
clojure.version.interim=false" >src/clj/clojure/version.properties 

if [ -z $QUALIFIER ]; then
  VERSION="$MAJOR_VERSION.$MINOR_VERSION.$INCREMENTAL_VERSION"
else
  VERSION="$MAJOR_VERSION.$MINOR_VERSION.$INCREMENTAL_VERSION-$QUALIFIER"
fi	

git commit -a -m "[Automated release] Clojure $VERSION"
git tag -a -m "$VERSION" $VERSION

if [ -z $CLOJURE_DEPLOY_URL ]; then
  CLOJURE_DEPLOY_URL=scp://build.clojure.org/srv/www/releases
fi
ant release -Ddeployment.url=$CLOJURE_DEPLOY_URL

echo "Build is complete. git push if you are satisfied with the result."
