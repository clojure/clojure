<head>
	<title>Clojure</title> 
	<meta http-equiv="Content-type" content="text/html; charset=iso-8859-1"/>
	<meta name="description" content="Clojure is a dynamic programming language for the JVM" />
	<meta name="keywords" content="Rich Hickey, Lisp, Java, software transactional memory, functional programming, persistent data structures"
   />
	<meta name="author" content="Rich Hickey" />
	<link rel="stylesheet" type="text/css" media="screen" href="clojure.css">
</head>

#Clo*j*ure
## A Dynamic Programming Language for the JVM
Copyright (c) Rich Hickey. All rights reserved.

The use and distribution terms for this software are covered by the [Common Public License 1.0][cpl], which can be found in the file CPL.TXT at the root of this distribution. By using this software in any fashion, you are agreeing to be bound by the terms of this license. You must not remove this notice, or any other, from this software.

[cpl]:http://www.opensource.org/licenses/cpl1.0.php

##Contents
###Introduction
Clojure is a dynamic programming language that targets the [Java Virtual Machine][jvm]. It is designed to be a general-purpose language, combining the approachability and interactive development of a scripting language with an efficient and robust infrastructure for multithreaded server programming. Clojure is a compiled language - it compiles directly to JVM bytecode, yet remains completely dynamic. *Every* feature supported by Clojure is supported at runtime. Clojure provides easy access to the Java frameworks, and optional type hints with type inference, to ensure that calls to Java can avoid reflection. 

Clojure is a dialect of Lisp, and shares with Lisp the code-as-data philosophy and a powerful macro system. Clojure is predominantly a [functional programming][fp] language, and features a rich set of immutable, [persistent data structures][pd]. When mutable state is needed, Clojure offers a [software transactional memory][stm] system that ensures clean, correct, multithreaded designs.

I hope you find Clojure's combination of facilities elegant, powerful and fun to use.

[jvm]:http://java.sun.com/docs/books/jvms/
[fp]: http://en.wikipedia.org/wiki/Functional_programming
[pd]: http://en.wikipedia.org/wiki/Persistent_data_structure
[stm]:http://en.wikipedia.org/wiki/Software_transactional_memory

Rich Hickey

###[Setup](#setup)
###[Quick Start](#quickstart)
###[Reader](#reader)
###[Evaluation](#evaluation)
###[Special Forms](#specialforms)
###[Data Structures](#datastructures)
###[Sequences](#sequences)
###[Vars and the Global Environment](#vars)
###[Refs and Transactions](#refs)
###[Access to Java](#java)			
###[Differences with other Lisps](#lisp)			

<h2 id="setup">Setup</h2>
Clojure is delivered in a zip file containing a single .jar, `clojure.jar`. It requires the [ASM 3.0 bytecode library][asm], and the current alpha distribution does not bundle it. `asm, asm-analysis, asm-commons and asm-util` jars are currently required. [Java][jdk] 1.5 or greater is required.

[asm]: http://asm.objectweb.org/
[jdk]: http://java.sun.com/javase/downloads/index.jsp

<h2 id="quickstart">Quick Start</h2>
Put the Clojure and ASM jars on the classpath and launch `java clojure.lang.Compiler`. This will bring up a simple read-eval-print loop (REPL). Much of Clojure is defined in Clojure itself, so the first thing you need to do is load the `boot.clj` file included in the distribution:

<pre><code>
(load-file "/your/path/to/boot.clj")	
</code></pre>

In the alpha, this will spew a bunch of bytecode as it compiles the file. This is to help me (and you) diagnose any code generation problems. After boot.clj is loaded you will have the language as described herein fully available.

Try:
<pre><code>
(+ 1 2 3)

(. javax.swing.JOptionPane (showMessageDialog nil "Hello World"))	
</code></pre>

<h2 id="reader">Reader</h2>

Clojure is a [homoiconic][hicon] language, which is a fancy term describing the fact that Clojure programs are represented by Clojure data structures. This is a very important difference between Clojure (and Common Lisp) and most other programming languages - Clojure is defined in terms of the evaluation of a data structure and **not** in terms of the syntax of character streams. It is quite common, and easy, for Clojure programs to manipulate, transform and produce other Clojure programs.

That said, most Clojure programs begin life as text files, and it is the task of the *reader* to parse the text and produce the data structure the compiler will see. It is not merely a phase of the compiler. The reader, and the Clojure data representations, have utility on their own in many of the same contexts one might use XML or JSON etc.

One might say the reader has syntax defined in terms of characters, and the Clojure language has syntax defined in terms of symbols, lists, vectors, maps etc.

### Data Structure Syntax
*	Lists/Seqs

	Lists are enclosed in parens: 
		
	`(a b c)`
	
* 	Vectors
	
	Vectors are enclosed in square brackets:
	
	`[1 2 3]`
	
* 	Maps

	Maps are key/value pairs enclosed in braces:
	
	`{:a 1 :b 2}`
	
* 	Literals
	* Strings - Enclosed in `"double quotes"`
	* Numbers - as per Java, plus indefinitely long integers are supported, as well as ratios, e.g. `22/7`
	* Characters - preceded by a backslash: `\c`
	* `nil` - represents null and false
	
* Symbols
* Keywords

[hicon]:http://en.wikipedia.org/wiki/Homoiconicity

<h2 id="evaluation">Evaluation</h2>
<h2 id="specialforms">Special Forms</h2>
<h2 id="datastructures">Data Structures</h2>
<h2 id="sequences">Sequences</h2>
<h2 id="vars">Vars and the Global Environment</h2>
<h2 id="refs">Refs and Transactions</h2>
<h2 id="java">Access to Java</h2>
<h2 id="lisp">Differences with other Lisps</h2>

* Clojure is case sensitive
* () is not the same as nil
* The reader is side-effect free

