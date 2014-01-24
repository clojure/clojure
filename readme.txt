 *   Clojure
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.

--------------------------------------------------------------------------
==clojure-objc==

WARNING! THIS IS AN ALPHA RELEASE

clojure-objc is a hacked version of clojure-jvm that generates java code on top of the jvm bytecode.
The generated java sources are intended to be translated to objc using j2objc(https://code.google.com/p/j2objc/).
The runtime sourcebase is also modified to use only j2objc's jre emulated classes(https://code.google.com/p/j2objc/wiki/JreEmulation).

clojure.core and the runtime have a few tweaks to work faster on the objc runtime. But everything works with no modifications.

Goals
 * Write iOS and MacOS apps in clojure
 * Future proof: new features on clojure should be easy to add 
 * Distribute clojure-objc libs using maven
 * Pure clojure libs should 'just work' (if they only use the jre emulated classes)
 * ObjC dynamic interop
 * Run tests in the jvm (with no ObjC interop)
 
 What doesn't work (yet)
 * no repl in the objc runtime
 * no objc subclassing
 * no c interop
 * clojure.core/bean: missing the emulated jre support
 * clojure.core/case: hashCode in java and hash in objc are different, you can use case but condp will be used instead
 * all java.net (slurp, reader from url, etc.): https://code.google.com/p/j2objc/issues/detail?id=28 (you can always use objc interop for this)
 * many tests fail because NullPointerException is emulated in objc by j2objc, and not perfectly. Don't rely on NullPointerException
 * same applies for ClassCastException
 
 Dependency
 [galdolber/clojure-objc "1.5.1"]
 
 Where to start:
 https://github.com/galdolber/lein-objcbuild

--------------------------------------------------------------------------

Docs: http://clojure.org
Feedback: http://groups.google.com/group/clojure
Getting Started: http://dev.clojure.org/display/doc/Getting+Started

To run:  java -cp clojure-${VERSION}.jar clojure.main

To build locally with Ant:  

   One-time setup:    ./antsetup.sh
   To build:          ant

Maven 2 build instructions:

  To build:  mvn package 
  The built JARs will be in target/

  To build without testing:  mvn package -Dmaven.test.skip=true

  To build and install in local Maven repository:  mvn install

  To build a ZIP distribution:  mvn package -Pdistribution
  The built .zip will be in target/


--------------------------------------------------------------------------
This program uses the ASM bytecode engineering library which is distributed
with the following notice:

Copyright (c) 2000-2005 INRIA, France Telecom
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

3. Neither the name of the copyright holders nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
THE POSSIBILITY OF SUCH DAMAGE.
