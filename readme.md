# clojure-objc

WARNING! THIS IS AN ALPHA RELEASE

clojure-objc is a clojure compiler that generates java(instead of bytecode) and uses j2objc(https://code.google.com/p/j2objc/) to translate it to objc.
The runtime sourcebase is also modified to use only j2objc's jre emulated classes(https://code.google.com/p/j2objc/wiki/JreEmulation).

clojure.core and the runtime have a few tweaks to work faster on the objc runtime. But everything works with no modifications.

## Downloads

### Download clojure-objc 1.5.1-1 static lib and headers
  
  https://www.dropbox.com/s/19r4n24lu8t4utv/clojure-objc-1.5.1-1.zip
  
## Dependency
 
    [galdolber/clojure-objc "1.5.1-1"]
  
### Download j2objc 0.8.8 with arm64 support (not available in official site)

  https://docs.google.com/file/d/0B34oZK3UpQolb1UzcGt5cFcxbXM/edit

## Where to start
 
 https://github.com/galdolber/lein-objcbuild

## Goals

 * Write iOS and MacOS apps in clojure
 * Future proof: new features on clojure should be easy to add 
 * Distribute clojure-objc libs using maven
 * Pure clojure libs should 'just work' (if they only use the jre emulated classes)
 * ObjC dynamic interop
 * Run tests in the jvm (with no ObjC interop)
 
## Memory management
 
 All generated code manage memory automagically, but if you alloc with interop you need to release!
 
## ObjC interop

    (defn say-hi [name]
      ($ ($ ($ ($ UIAlertView) :alloc)
            :initWithTitle (str "Hello " name)
            :message "Hi! from clojure"
            :delegate nil
            :cancelButtonTitle "Cancelar"
            :otherButtonTitles nil) :show))
            
## nsproxy EXPERIMENTAL!
    
    nsproxy doesn't require a base class.
    
    ; If you need a UITextFieldDelegate
    (nsproxy
      ([:bool :textFieldShouldReturn :id field]
        ($ field :resignFirstResponder) 
        true))
        
    ; nsproxy with a base class
    (nsproxy UIView
      ([:void :dealloc]
        (println "dealloc!"))
        
### Gotchas
  
  Do not proxy a class containing a struct, like UIViewController. 
  More information:
  http://stackoverflow.com/questions/17927639/adding-a-root-view-controller-ocmockobjectuiviewcontroller-as-a-child-view-con
        
### nsproxy supported types

    :float
    :long-long
    :long
    :char
    :short
    :int
    :double
    :unsigned-long-long
    :unsigned-long
    :unsigned-char
    :unsigned-short
    :unsigned-int
    :bool
    :CGPoint
    :NSRange
    :UIEdgeInsets
    :CGSize
    :CGAffineTransform
    :CATransform3D
    :UIOffset
    :CGRect
    :id
    :void

## What doesn't work (yet)
 
 * no repl in the objc runtime
 * no objc subclassing
 * no c interop
 * clojure.core/bean: missing the emulated jre support
 * clojure.core/case: hashCode in java and hash in objc are different, you can use case but condp will be used instead
 * all java.net (slurp, reader from url, etc.): https://code.google.com/p/j2objc/issues/detail?id=28 (you can always use objc interop for this)
 * many tests fail because NullPointerException is emulated in objc by j2objc, and not perfectly. Don't rely on NullPointerException
 * same applies for ClassCastException
 
## Discuss
 
 https://groups.google.com/d/forum/clojure-objc-discuss
 
## How to build dist
 
 ./build.sh # (this takes a lot of time)

--------------------------------------------------------------------------

 *   Clojure
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.

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




-------------------------------------------------------------------------
This program uses the Guava Murmur3 hash implementation which is distributed
under the Apache License:


                                 Apache License
                           Version 2.0, January 2004
                        http://www.apache.org/licenses/

   TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION

   1. Definitions.

      "License" shall mean the terms and conditions for use, reproduction,
      and distribution as defined by Sections 1 through 9 of this document.

      "Licensor" shall mean the copyright owner or entity authorized by
      the copyright owner that is granting the License.

      "Legal Entity" shall mean the union of the acting entity and all
      other entities that control, are controlled by, or are under common
      control with that entity. For the purposes of this definition,
      "control" means (i) the power, direct or indirect, to cause the
      direction or management of such entity, whether by contract or
      otherwise, or (ii) ownership of fifty percent (50%) or more of the
      outstanding shares, or (iii) beneficial ownership of such entity.

      "You" (or "Your") shall mean an individual or Legal Entity
      exercising permissions granted by this License.

      "Source" form shall mean the preferred form for making modifications,
      including but not limited to software source code, documentation
      source, and configuration files.

      "Object" form shall mean any form resulting from mechanical
      transformation or translation of a Source form, including but
      not limited to compiled object code, generated documentation,
      and conversions to other media types.

      "Work" shall mean the work of authorship, whether in Source or
      Object form, made available under the License, as indicated by a
      copyright notice that is included in or attached to the work
      (an example is provided in the Appendix below).

      "Derivative Works" shall mean any work, whether in Source or Object
      form, that is based on (or derived from) the Work and for which the
      editorial revisions, annotations, elaborations, or other modifications
      represent, as a whole, an original work of authorship. For the purposes
      of this License, Derivative Works shall not include works that remain
      separable from, or merely link (or bind by name) to the interfaces of,
      the Work and Derivative Works thereof.

      "Contribution" shall mean any work of authorship, including
      the original version of the Work and any modifications or additions
      to that Work or Derivative Works thereof, that is intentionally
      submitted to Licensor for inclusion in the Work by the copyright owner
      or by an individual or Legal Entity authorized to submit on behalf of
      the copyright owner. For the purposes of this definition, "submitted"
      means any form of electronic, verbal, or written communication sent
      to the Licensor or its representatives, including but not limited to
      communication on electronic mailing lists, source code control systems,
      and issue tracking systems that are managed by, or on behalf of, the
      Licensor for the purpose of discussing and improving the Work, but
      excluding communication that is conspicuously marked or otherwise
      designated in writing by the copyright owner as "Not a Contribution."

      "Contributor" shall mean Licensor and any individual or Legal Entity
      on behalf of whom a Contribution has been received by Licensor and
      subsequently incorporated within the Work.

   2. Grant of Copyright License. Subject to the terms and conditions of
      this License, each Contributor hereby grants to You a perpetual,
      worldwide, non-exclusive, no-charge, royalty-free, irrevocable
      copyright license to reproduce, prepare Derivative Works of,
      publicly display, publicly perform, sublicense, and distribute the
      Work and such Derivative Works in Source or Object form.

   3. Grant of Patent License. Subject to the terms and conditions of
      this License, each Contributor hereby grants to You a perpetual,
      worldwide, non-exclusive, no-charge, royalty-free, irrevocable
      (except as stated in this section) patent license to make, have made,
      use, offer to sell, sell, import, and otherwise transfer the Work,
      where such license applies only to those patent claims licensable
      by such Contributor that are necessarily infringed by their
      Contribution(s) alone or by combination of their Contribution(s)
      with the Work to which such Contribution(s) was submitted. If You
      institute patent litigation against any entity (including a
      cross-claim or counterclaim in a lawsuit) alleging that the Work
      or a Contribution incorporated within the Work constitutes direct
      or contributory patent infringement, then any patent licenses
      granted to You under this License for that Work shall terminate
      as of the date such litigation is filed.

   4. Redistribution. You may reproduce and distribute copies of the
      Work or Derivative Works thereof in any medium, with or without
      modifications, and in Source or Object form, provided that You
      meet the following conditions:

      (a) You must give any other recipients of the Work or
          Derivative Works a copy of this License; and

      (b) You must cause any modified files to carry prominent notices
          stating that You changed the files; and

      (c) You must retain, in the Source form of any Derivative Works
          that You distribute, all copyright, patent, trademark, and
          attribution notices from the Source form of the Work,
          excluding those notices that do not pertain to any part of
          the Derivative Works; and

      (d) If the Work includes a "NOTICE" text file as part of its
          distribution, then any Derivative Works that You distribute must
          include a readable copy of the attribution notices contained
          within such NOTICE file, excluding those notices that do not
          pertain to any part of the Derivative Works, in at least one
          of the following places: within a NOTICE text file distributed
          as part of the Derivative Works; within the Source form or
          documentation, if provided along with the Derivative Works; or,
          within a display generated by the Derivative Works, if and
          wherever such third-party notices normally appear. The contents
          of the NOTICE file are for informational purposes only and
          do not modify the License. You may add Your own attribution
          notices within Derivative Works that You distribute, alongside
          or as an addendum to the NOTICE text from the Work, provided
          that such additional attribution notices cannot be construed
          as modifying the License.

      You may add Your own copyright statement to Your modifications and
      may provide additional or different license terms and conditions
      for use, reproduction, or distribution of Your modifications, or
      for any such Derivative Works as a whole, provided Your use,
      reproduction, and distribution of the Work otherwise complies with
      the conditions stated in this License.

   5. Submission of Contributions. Unless You explicitly state otherwise,
      any Contribution intentionally submitted for inclusion in the Work
      by You to the Licensor shall be under the terms and conditions of
      this License, without any additional terms or conditions.
      Notwithstanding the above, nothing herein shall supersede or modify
      the terms of any separate license agreement you may have executed
      with Licensor regarding such Contributions.

   6. Trademarks. This License does not grant permission to use the trade
      names, trademarks, service marks, or product names of the Licensor,
      except as required for reasonable and customary use in describing the
      origin of the Work and reproducing the content of the NOTICE file.

   7. Disclaimer of Warranty. Unless required by applicable law or
      agreed to in writing, Licensor provides the Work (and each
      Contributor provides its Contributions) on an "AS IS" BASIS,
      WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
      implied, including, without limitation, any warranties or conditions
      of TITLE, NON-INFRINGEMENT, MERCHANTABILITY, or FITNESS FOR A
      PARTICULAR PURPOSE. You are solely responsible for determining the
      appropriateness of using or redistributing the Work and assume any
      risks associated with Your exercise of permissions under this License.

   8. Limitation of Liability. In no event and under no legal theory,
      whether in tort (including negligence), contract, or otherwise,
      unless required by applicable law (such as deliberate and grossly
      negligent acts) or agreed to in writing, shall any Contributor be
      liable to You for damages, including any direct, indirect, special,
      incidental, or consequential damages of any character arising as a
      result of this License or out of the use or inability to use the
      Work (including but not limited to damages for loss of goodwill,
      work stoppage, computer failure or malfunction, or any and all
      other commercial damages or losses), even if such Contributor
      has been advised of the possibility of such damages.

   9. Accepting Warranty or Additional Liability. While redistributing
      the Work or Derivative Works thereof, You may choose to offer,
      and charge a fee for, acceptance of support, warranty, indemnity,
      or other liability obligations and/or rights consistent with this
      License. However, in accepting such obligations, You may act only
      on Your own behalf and on Your sole responsibility, not on behalf
      of any other Contributor, and only if You agree to indemnify,
      defend, and hold each Contributor harmless for any liability
      incurred by, or claims asserted against, such Contributor by reason
      of your accepting any such warranty or additional liability.

   END OF TERMS AND CONDITIONS
