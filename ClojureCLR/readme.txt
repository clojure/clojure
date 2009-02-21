ClojureCLR

This project is a native implementation of Clojure over the Microsoft .Net Framework 
programmed in C# and using the Dynamic Language Runtime.

Status: Alpha, developer release

Goals:

 -- Implement a feature-complete Clojure on top of CLR/DLR.
 -- Stay as close as possible to the JVM implementation.
      --- To the extent possible, use exactly the same boostrap *.clj files
          to define the environment.
      --- Match public interfaces and important data structure classes
 -- Try to use some of the more advanced features of the DLR, 
      where it makes sense to do so.
 -- Spawn a cottage industry of people making Visual Studio extensions
      for Clojure. :)
 -- Have fun. 

--------------------------------------------------------------------------------------

For more information on clojure:  http://clojure.org

For instructions on installing: .\install.txt

Version information:  .\versions.txt

Todo list:  .\todo.txt

--------------------------------------------------------------------------------------

 *   ClojureCLR
 *   Copyright (c) David Miller. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.

--------------------------------------------------------------------------------------

The work contained herein is derived from and in many places is a direct translation 
of the primary Clojure distribution.  That work contains the following notice:

 *   Clojure
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 * 	 the terms of this license.
 *   You must not remove this notice, or any other, from this software.



