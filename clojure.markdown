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

Clojure is [hosted on SourceForge][sf]. <a href="http://sourceforge.net"><img src="http://sflogo.sourceforge.net/sflogo.php?group_id=137961&amp;type=1" width="88" height="31" border="0" alt="SourceForge.net Logo" /></a>

Clojure is delivered in a zip file containing a single .jar, `clojure.jar`. It requires the [ASM 3.0 bytecode library][asm], and the current alpha distribution does not bundle it. `asm, asm-analysis, asm-commons and asm-util` jars are currently required. [Java][jdk] 1.5 or greater is required.

Feedback and discussion should occur on the [Clojure Google Group][cgg].

[asm]: http://asm.objectweb.org/
[jdk]: http://java.sun.com/javase/downloads/index.jsp
[sf]:  http://sourceforge.net/project/showfiles.php?group_id=137961
[cgg]: http://groups.google.com/group/clojure

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

That said, most Clojure programs begin life as text files, and it is the task of the *reader* to parse the text and produce the data structure the compiler will see. This is not merely a phase of the compiler. The reader, and the Clojure data representations, have utility on their own in many of the same contexts one might use XML or JSON etc.

One might say the reader has syntax defined in terms of characters, and the Clojure language has syntax defined in terms of symbols, lists, vectors, maps etc. The reader is represented by the function `read`, which reads the next form (not character) from a stream, and returns the object represented by that form.

	Since we have to start somewhere, we might as well start where evaluation starts, with the reader forms. This will inevitably entail talking about data structures whose descriptive details, and interpretation by the compiler, will follow.

[hicon]:http://en.wikipedia.org/wiki/Homoiconicity
	
### Reader forms
* 	Symbols

	Symbols begin with a non-numeric character and can contain alphanumeric characters and *, +, !, -, _, and ? (other characters will be allowed eventually, but not all macro characters have been determined). / has special meaning, it can be used once in the middle of a symbol to separate the namespace from the name, e.g. `my-namespace/foo`. '.' has special meaning, it can be used one or more times in the middle of a symbol to designate a fully-qualified class name, e.g. `java.util.BitSet`.

* 	Literals
	* Strings - Enclosed in `"double quotes"`
	* Numbers - as per Java, plus indefinitely long integers are supported, as well as ratios, e.g. `22/7`
	* Characters - preceded by a backslash: `\c`
	* `nil` - represents null and logical false
	* Keywords

		Keywords are like symbols, except: 
		* They can and must begin with a colon, e.g. `:fred`.
		* They cannot contain '.' or name classes.

*	Lists

	Lists are zero or more forms enclosed in parentheses: 
		
	`(a b c)`
	
* 	Vectors
	
	Vectors are zero or more forms enclosed in square brackets:
	
	`[1 2 3]`
	
* 	Maps

	Maps are zero or more key/value pairs enclosed in braces:
	
	`{:a 1 :b 2}`
	
	Commas are considered whitespace, and can be used to organize the pairs:
	
	`{:a 1, :b 2}`

### Macro characters
The behavior of the reader is driven by a combination of built-in constructs and an extension system called the read table. Entries in the read table provide mappings from certain characters, called macro characters, to specific reading behavior, called reader macros. Unless indicated otherwise, macro characters cannot be used in user symbols.

*	Quote (')

	`'form` => `(quote form)`
*	Syntax-quote (\`, note, the "backquote" character), Unquote (~) and Unquote-splicing (~@)

	For all forms other than Symbols, Lists, Vectors and Maps, \`x is the same as 'x. 
	
	For Symbols, syntax-quote resolves the symbol in the current context, yielding a fully-qualified symbol (i.e. namespace/name or fully.qualified.Classname). 
	
	For Lists/Vectors/Maps, syntax-quote establishes a template of the corresponding data structure. Within the template, unqualified forms behave as if recursively syntax-quoted, but forms can be exempted from such recursive quoting by qualifying them with unquote or unquote-splicing, in which case they will be treated as expressions and be replaced in the template by their value, or sequence of values, respectively.
	
	For example:
	
<pre><code>
(def x 5)
(def lst '(a b c))
`(fred x ~x lst ~@lst 7 8 :nine)

> (user/fred user/x 5 user/lst a b c 7 8 :nine)
</code></pre>	
	
*	Character (\\)

	As per above, yields a character literal.
	
*	Comment (;)

	Single-line comment, causes the reader to ignore everything from the semicolon to the end-of-line.
	
*	Meta (^)

	`^form` => `(meta form)`
*	Deref (@)

	`@form` => `(deref form)`
	
*	Dispatch (#)

	The dispatch macro causes the reader to use a reader macro from another table, indexed by the character following #:

	*	Metadata (#^)
	
		Symbols, Lists, Vector and Maps can have metadata, which is a map associated with the object. The metadata reader macro first reads the metadata and attaches it to the next form read:
		
		`#^{:a 1 :b 2} [1 2 3]` yields the vector [1 2 3] with a metadata map of {:a 1 :b 2}.
		
		A shorthand version allows the metadata to be a simple symbol or keyword, in which case it is treated as a single entry map with a key of :clojure/tag and a value of the symbol provided, e.g.:
		
		`#^String x` is the same as `#^{:clojure/tag String} x`
		
		Such tags can be used to convey type information to the compiler. 
		
	*	Var-quote (#')
		
		`#'x` => `(the-var x)`

The read table is currently not accessible to user programs.



<h2 id="evaluation">Evaluation</h2>
Evaluation can occur in many contexts:

*	Interactively, in the REPL
*	On a sequence of forms read from a stream, via `load` or `load-file`
*	Programmatically, via `eval`

In all cases, evaluation is the same - a single object is considered by the compiler, evaluated, and its result returned. If an expression needs to be compiled, it will be. There is no separate compilation step, nor any need to worry that a function you have defined is being interpreted. *Clojure has no interpreter*.

Strings, numbers, characters, `nil` and keywords evaluate to themselves.

A Symbol is *resolved*:

*	If it is namespace-qualified, the value is the value of the binding of the global var named by the symbol. It is an error if there is no global var named by the symbol.
*	Else, it is not namespace-qualified:
 	1. If it names a special form it is considered a special form, and must be utilized accordingly.
	2. Else, if in a local scope, a lookup is done to see if it names a local binding (e.g. a function argument or let-bound name). If so, the value is the value of the local binding.
	3. A lookup is done in the *refers* map to see if there is a mapping from the symbol to a global var. If so, the value is the value of the binding of the global var referred-to by the symbol.
	4. A lookup is done to see if there is a global var with a namespace equal to the *current-namespace* and the same name as the symbol. If so, the value is the value of the binding of the global var named by symbol *current-namespace*/symbol-name.
	5. It is an error.
	
If the Symbol has metadata, it may be used by the compiler, but will not be part of the resulting value.

Vectors and Maps yield vectors and maps whose contents are the *evaluated values* of the objects they contain. The same is true of metadata maps. If the vector or map has metadata, the *evaluated* metadata map will become the metadata of the resulting value.

<pre><code>
(def x 1)
(def y 2)
#^{:x x} [x y 3]

^{:x 1} [1 2 3]
</code></pre>	

Lists are considered *calls* to either special forms, macros, or functions. A call has the form `(operator operands*)`. 

Special forms are primitives built-in to Clojure that perform core operations. If the operator of a call is a symbol that resolves to the name of a special form, the call is to that special form. Each form discussed individually under [Special Forms](#specialforms).

Macros are functions that manipulate forms, allowing for syntactic abstraction. If the operator of a call is a symbol that names a global var that is a macro function, that function is called and is passed the *unevaluated* operand forms. The return value of the macro is then evaluated in its place.

If the operator is not a special form or macro, it and the operands (if any) are evaluated, from left to right. The result of the evaluation of the operator is then cast to IFn (the interface representing Clojure functions), and invoke() is called on it, passing the evaluated arguments. The return value of invoke() is the value of the call expression.

Note that special forms and macros might have other-than-normal evaluation of their arguments, as described in their entries under [Special Forms](#specialforms).

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
* Keywords are not Symbols
* `nil` is not a Symbol
* The read table is currently not accessible to user programs

