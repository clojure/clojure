mvn clean
mvn compile
mvn test-compile
rm -Rf target/objc
mkdir target/objc
cp -R src/objc/. target/objc
cp -R src/ffi/. target/objc
zip -r target/objc.jar target/gen src/jvm test/java
j2objc -d target/objc -classpath target/classes:target/test-classes target/objc.jar

if [ ! -d "target/include" ]; then
	mkdir target/include
fi

echo exporting headers...
cd target/objc
rsync -avm --delete --include='*.h' -f 'hide,! */' . ../include > /dev/null
cd ..

echo building static lib...

OBJC=$(pwd)/objc
IPHONEOS_SDK="/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS7.0.sdk"
IPHONESIMULATOR_SDK="/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator7.0.sdk"
FRAMEWORKS="-framework UIKit -framework Foundation"
INCLUDES="-I$J2OBJC_HOME/include -I$(pwd)/objc -I$(pwd)/../src/ffi"
OPTS="-miphoneos-version-min=5.0 -fmessage-length=0 -fmacro-backtrace-limit=0 -std=gnu99 -fpascal-strings -O0 -DDEBUG=1 -fstrict-aliasing -Wno-unsequenced -MT dependencies"
function build {
	NAME=$1
	ARCH=$2
	SDK=$3
	echo "compiling $NAME..."
	rm -Rf $NAME
	mkdir $NAME
	cd $NAME
	find $OBJC -name "*.m" | while read file; do echo $NAME $(basename $file);clang -x objective-c $ARCH $OPTS -isysroot $SDK $INCLUDES -c $file -o $(uuidgen).o; done 
	find $(pwd) -name "*.o" | tr "\\n" "\n" > $NAME.LinkFileList 
	echo "linking $NAME..."
	libtool -static -syslibroot $SDK -filelist $NAME.LinkFileList $FRAMEWORKS -o libclojure-objc.a
	cd .. 
}

#build "iphoneos" "-arch armv7 -arch armv7s -arch arm64" $IPHONEOS_SDK
#build "iphonesimulator" "-arch i386 -arch x86_64" $IPHONESIMULATOR_SDK

#if [ -f libclojure-objc.a ]; then
#	rm libclojure-objc.a
#fi
#lipo -create -output libclojure-objc.a $(find . -name "libclojure-objc.a" | tr "\\n" " ")
