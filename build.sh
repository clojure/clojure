#rm -Rf target/objc
#zip -r target/objc.jar target/gen
#zip -r target/clj.jar src/jvm
#j2objc -d target/objc -classpath target/classes:target/test-classes target/objc.jar test/java/java/net/*
#j2objc -d target/objc -classpath target/classes target/clj.jar

if [ ! -d "target/headers" ]; then
	mkdir target/headers
fi

echo exporting headers...
cd target/objc
rsync -avm --delete --include='*.h' -f 'hide,! */' . ../headers > /dev/null
cd ..

echo building static lib...

J2OBJC=/Users/admin/projects/j2objc
IPHONEOS_SDK="/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS7.0.sdk"
IPHONESIMULATOR_SDK="/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator7.0.sdk"
FRAMEWORKS="-framework UIKit -framework Foundation"
INCLUDES="-I$J2OBJC/dist/include -I$(pwd)/objc"
OPTS="-fmodules -g -miphoneos-version-min=5.0 -DDEBUG=1 -fmessage-length=0 -fdiagnostics-show-note-include-stack -fmacro-backtrace-limit=0 -std=gnu99 -fpascal-strings -fobjc-abi-version=2 -fexceptions -fasm-blocks -fstrict-aliasing -fobjc-legacy-dispatch -MMD -MT dependencies -Wno-unsequenced"
ALL_M_FILES=$(find $(pwd)/objc -name "*.m" | tr "\\n" " ")

function build {
	NAME=$1
	ARCH=$2
	SDK=$3
	echo "compiling $NAME..."
	rm -Rf $NAME
	mkdir $NAME
	cd $NAME
	clang -x objective-c $ARCH $OPTS $INCLUDES -c $ALL_M_FILES -isysroot $SDK -O0
	find $(pwd) -name "*.o" | tr "\\n" "\n" > all.FileList 
	echo "linking $NAME..."
	libtool -static -syslibroot $SDK -filelist all.FileList $FRAMEWORKS -o libclojure-objc.a
	cd .. 
}

build "iphoneos" "-arch armv7 -arch armv7s -arch arm64" $IPHONEOS_SDK
build "iphonesimulator" "-arch i386 -arch x86_64" $IPHONESIMULATOR_SDK

if [ -f libclojure-objc.a ]; then
	rm libclojure-objc.a
fi
lipo -create -output libclojure-objc.a $(find . -name "libclojure-objc.a" | tr "\\n" " ")