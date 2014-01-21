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

IPHONEOS_SDK="/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS7.0.sdk"
IPHONESIMULATOR_SDK="/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneSimulator.platform/Developer/SDKs/iPhoneSimulator7.0.sdk"
FRAMEWORKS="-framework QuartzCore -framework UIKit -framework Foundation"
INCLUDES="-I/Users/admin/projects/j2objc/dist/include -I$(pwd)/objc"
OPTS="-g -miphoneos-version-min=5.0 -DDEBUG=1 -fmessage-length=0 -fdiagnostics-show-note-include-stack -fmacro-backtrace-limit=0 -std=gnu99 -fpascal-strings -fobjc-abi-version=2 -fexceptions -fasm-blocks -fstrict-aliasing -fobjc-legacy-dispatch -MMD -MT dependencies -Wno-unsequenced"
ALL_M_FILES=$(find $(pwd)/objc -name "*.m" | tr "\\n" " ")

function build {
	ARCH=$1
	SDK=$2
	echo "compiling $ARCH..."
	rm -Rf $ARCH
	mkdir $ARCH
	cd $ARCH
	clang -x objective-c -arch $ARCH -arch x86_64 $OPTS $INCLUDES -c $ALL_M_FILES -isysroot $SDK -O0
	find $(pwd) -name "*.o" | tr "\\n" "\n" > all.FileList 
	echo "linking $ARCH..."
	libtool -static -arch_only $ARCH -syslibroot $SDK -filelist all.FileList $FRAMEWORKS -o libclojure-objc.a
	cd .. 
}

#build "armv7" $IPHONEOS_SDK
#build "armv7s" $IPHONEOS_SDK
#build "arm64" $IPHONEOS_SDK
build "i386" $IPHONESIMULATOR_SDK
#build "x86_64" $IPHONESIMULATOR_SDK

if [ -f libclojure-objc.a ]; then
	rm libclojure-objc.a
fi
lipo -create -output libclojure-objc.a $(find . -name "libclojure-objc.a" | tr "\\n" " ")

lipo -create -output libclojure-objc.a ./arm64/libclojure-objc.a ./armv7/libclojure-objc.a ./armv7s/libclojure-objc.a ./i386/libclojure-objc.a ./x86_64/libclojure-objc.a
