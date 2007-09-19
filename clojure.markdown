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

Clojure is a [homoiconic][hicon] language, which is a fancy term describing the fact that Clojure programs are represented by Clojure data structures. This is a very important difference between Clojure (and Common Lisp) and most other programming languages - Clojure is defined in terms of the evaluation of a data structure and **not** in terms of the syntax of character streams/files. It is quite common, and easy, for Clojure programs to manipulate, transform and produce other Clojure programs.

That said, most Clojure programs begin life as text files, and it is the task of the *reader* to parse the text and produce the data structure the compiler will see. This is not merely a phase of the compiler. The reader, and the Clojure data representations, have utility on their own in many of the same contexts one might use XML or JSON etc.

One might say the reader has syntax defined in terms of characters, and the Clojure language has syntax defined in terms of symbols, lists, vectors, maps etc. The reader is represented by the function `read`, which reads the next form (not character) from a stream, and returns the object represented by that form.

>Since we have to start somewhere, we might as well start where evaluation starts, with the reader forms. This will inevitably entail talking about data structures whose descriptive details, and interpretation by the compiler, will follow.

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
*	Else, it is not namespace-qualified and the first of the following applies:
 	1. If it names a special form it is considered a special form, and must be utilized accordingly.
	2. A lookup is done in the *imports* map to see if there is a mapping from the symbol to a fully qualified class name. If so, the symbol is considered to name a class. Note that class names are not first-class objects and are only valid in certain special forms.
	3. If in a local scope (i.e. in a function definition), a lookup is done to see if it names a local binding (e.g. a function argument or let-bound name). If so, the value is the value of the local binding.
	4. A lookup is done in the *refers* map to see if there is a mapping from the symbol to a global var. If so, the value is the value of the binding of the global var referred-to by the symbol.
	5. A lookup is done to see if there is a global var with a namespace equal to the *current-namespace* and the same name as the symbol. If so, the value is the value of the binding of the global var named by symbol *current-namespace*/symbol-name.
	6. It is an error.
	
If the Symbol has metadata, it may be used by the compiler, but will not be part of the resulting value.

Vectors and Maps yield vectors and maps whose contents are the *evaluated values* of the objects they contain. The same is true of metadata maps. If the vector or map has metadata, the *evaluated* metadata map will become the metadata of the resulting value.

<pre><code>
(def x 1)
(def y 2)
#^{:x x} [x y 3]

^{:x 1} [1 2 3]
</code></pre>	

An empty list `()` evaluates to an empty list.

Non-empty Lists are considered *calls* to either special forms, macros, or functions. A call has the form `(operator operands*)`. 

Special forms are primitives built-in to Clojure that perform core operations. If the operator of a call is a symbol that resolves to the name of a special form, the call is to that special form. Each form discussed individually under [Special Forms](#specialforms).

Macros are functions that manipulate forms, allowing for syntactic abstraction. If the operator of a call is a symbol that names a global var that is a macro function, that function is called and is passed the *unevaluated* operand forms. The return value of the macro is then evaluated in its place.

If the operator is not a special form or macro, the call is considered a function call, and it and the operands (if any) are evaluated, from left to right. The result of the evaluation of the operator is then cast to IFn (the interface representing Clojure functions), and invoke() is called on it, passing the evaluated arguments. The return value of invoke() is the value of the call expression. If the function call form has metadata, it may be used by the compiler, but will not be part of the resulting value.

Note that special forms and macros might have other-than-normal evaluation of their arguments, as described in their entries under [Special Forms](#specialforms).

The above describes the evaluation of a single form. `load` and `load-file` will sequentially evaluate the set of forms contained in the stream/file. Such sets of forms usually have side effects, often on the global environment, defining functions etc. The loading functions occur in a temporary context, in which *current-namespace*, *imports* and *refers* all have fresh bindings. That means that, should any form have an effect on those vars (e.g. `in-namespace, refers, import`), the effect will unwind at the completion of the load. 

<h2 id="specialforms">Special Forms</h2>

---
### (*def* symbol init?)
Creates or locates a global var with the name of `symbol` and a namespace of the value of `*current-namespace*`. If `init` is supplied, it is evaluated, and the root binding of the var is set to the resulting value. If `init` is not supplied, the root binding of var is unaffected. `def` always applies to the root binding, even if the var is thread-bound at the point where def is called.

---
### (*if* test then else?)
Evaluates `test`. If not nil, evaluates and yields `then`, otherwise, evaluates and yields `else`. If `else` is not supplied it defaults to `nil`.

---
### (*do* exprs*)
Evaluates the expressions in order and returns the value of the last. If no expressions are supplied, returns `nil`.

---
### (*let* [bindings* ] exprs*)
binding => symbol init-expr

Evaluates the exprs in a context in which the symbols are bound to their respective init-exprs. The bindings are sequential, so each binding can see the prior bindings. The exprs are contained in an implicit `do`.

<pre><code>
(let [x 1 y x] y)

1
</code></pre>	

---
### (*quote* form)
Yields the unevaluated form

<pre><code>
'(a b c)

(a b c)
</code></pre>

Note there is no attempt made to call the function `a`. The return value is a list of 3 symbols.

---
### (*the-var* symbol)
The symbol must resolve to a var, and the Var object itself (not its value) is returned.

---
### (*fn* [params* ] exprs*)
### (*fn* ([params* ] exprs*)+)
params => positional-params* , or positional-params* `&` rest-param

param => symbol

Defines a function (fn). Fns are first-class objects that implement the IFn interface. The IFn interface defines an invoke() function that is overloaded with arity ranging from 0-20. A single fn object can implement one or more invoke points, and thus be overloaded on arity. One and only one overload can itself be variadic, by specifying the ampersand followed by a single rest-param. Such a variadic entry point, when called with arguments that exceed the positional params, will find them in a seq contained in the rest arg. If the supplied args do not exceed the positional params, the rest arg will be nil.

The first form defines a fn with a single entry point. The second defines a fn with one or more overloads. The arities of the overloads must be distinct. In either case, the result of the expression is a single fn object.

The exprs are enclosed in an implicit `do`. The symbol `this-fn` is bound within the function definition to the function object itself, allowing for self-calling, even in anonymous functions.

<pre><code>
(def *
  (fn ([] 1)
      ([x] x)
      ([x y] (. Num (multiply x y)))
      ([x y & more]
          (apply thisfn (thisfn x y) more))))
</code></pre>

A fn (overload) defines a recursion point at the top of the function, with arity equal to the number of params *including the rest param, if present*. See `recur`.

---
### (*loop* [bindings* ] exprs*)

`Loop` is exactly like `let`, except that it establishes a recursion point at the top of the loop, with arity equal to the number of bindings. See `recur`.

---
### (*recur* exprs*)

Evaluates the exprs in order, then, in parallel, rebinds the bindings of the recursion point to the values of the exprs. If the recursion point was a fn, then it rebinds the params. If the recursion point was a `loop`, then it rebinds the loop bindings. Execution then jumps back to the recursion point. The `recur` expression must match the arity of the recursion point exactly. In particular, if the recursion point was the top of a variadic function, there is no gathering of rest args, a single seq (or null) should be passed. `recur` in other than a tail position is an error.

Note that `recur` is the only non-stack-consuming looping construct in Clojure. There is no tail-call optimization and the use of self-calls for looping is discouraged. `recur` is functional and its use in tail-position is verified by the compiler.

<pre><code>
(def factorial 
  (fn [n]
    (loop [cnt n acc 1]
       (if (zero? cnt)
            acc
          (recur (dec cnt) (* acc cnt))))))
</code></pre>

---
### (*.* instance-expr instanceFieldName-symbol)
### (*.* Classname-symbol staticFieldName-symbol)
### (*.* instance-expr (instanceMethodName-symbol args*))
### (*.* Classname-symbol (staticMethodName-symbol args*))

The '.' special form is the primary access to Java. It can be considered a member-access operator, and/or read as 'in the scope of'. 

If the first operand is a symbol that resolves to a class name, the access is considered to be to a static member of the named class. Otherwise it is presumed to be an instance member and the first argument is evaluated to produce the target object. 

If the second operand is a symbol it is taken to be a field access - the name of the field is the name of the symbol. The value of the expression is the value of the field.

If the second operand is a list it is taken to be a method call. The first element of the list must be a simple symbol, and the name of the method is the name of the symbol. The args, if any, are evaluated from left to right, and passed to the matching method, which is called, and its value returned. If the method has a void return type, the value of the expression will be `nil`. 

Note that boolean return values will be turned into nil/non-nil, chars will become Characters, and numeric primitives will become Clojure Nums.

---
### (*new* Classname-symbol args*)
The args, if any, are evaluated from left to right, and passed to the constructor of the class named by the symbol. The constructed object returned.

---
### (*class* Classname-symbol)
Yields the java.lang.Class corresponding to the symbol.

---
### (*instance?* expr Classname-symbol)
Evaluates expr and tests if it is an instance of the class named by the symbol.

---
### (*throw* expr)
The expr is evaluated and thrown, therefor it should yield an instance of some derivee of Throwable.

---
### (*try-finally* expr finally-expr)
The expr is evaluated and its value returned. Before returning, normally or abnormally, the finally-expr will be evaluated for its side effects.

---
### (*=* (. instance-expr instanceFieldName-symbol) expr)
### (*=* (. Classname-symbol staticFieldName-symbol) expr)
### (*=* var-symbol expr)
Assignment. 

When the first operand is a field member access form, the assignment is to the corresponding field. If it is an instance field, the instance expr will be evaluated, then the expr.

When the first operand is a symbol, it must resolve to a global var. The value of the vars current thread binding is set to the value of expr. Currently, it is an error to attempt to set the root binding of a var using `=`, i.e. var assignments are thread-local.

In all cases the value of expr is returned.

Note - *you cannot assign to function params or local bindings*.

---
### (*monitor-enter* x)
### (*monitor-exit* x)

These are synchronization primitives that should be avoided in user code. Use the `locking` macro.

<h2 id="datastructures">Data Structures</h2>

Clojure has a rich set of data structures. They share a set of properties:

*	They are immutable
*	They are read-able
*	They support proper equality semantics in their implementation of `equals`
*	In addition, the collections: 
	*	Are manipulated via interfaces.
	*	Support sequencing
	*	Support persistent manipulation.
	*	Support metadata
	*	Implement Iterable

### nil
While not properly a data structure, `nil` is a valid value of any data type in Clojure (since primitives are always boxed). `nil` has the same value as Java `null`. The Clojure conditional system is based around `nil`/non-nil, with `nil` representing the value of logical false in conditional tests. In addition, `nil` is used as the end-of-sequence sentinel value in the sequence protocol.

### Nums
All Clojure numbers are derived from Num, which in turn is derived from java.lang.Number. There are 4 types:

* 	FixNum
	
	Represent integer values that fit in 32 bits. When arithmetic involving FixNums exceeds their capacity, they automatically become BigNums.
	
*	BigNum

	Represent integers of arbitrary precision.
	
*	DoubleNum

	Correspond to Java's `double`.
	
*	RatioNum

	Represents a ratio between integers. Division of integers that can't be reduced to an integer yields a ratio, i.e. 22/7 = 22/7, rater than a floating point value.
	
Any numeric operation involving DoubleNums yields a DoubleNum. 

### Strings
Clojure strings are Java `Strings`.

### Characters
Clojure characters are Java `Characters`.

### Keywords
Keywords are symbolic identifiers that evaluate to themselves. They provide very fast equality tests. Like Symbols, they have names and optional namespaces, both of which are strings. The leading ':' is not part of the namespace or name.

### Symbols
Symbols are identifiers that are normally used to refer to something else. They can be used in program forms to refer to function parameters, let bindings, class names and global vars. They have names and optional namespaces, both of which are strings.

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
* `let` is like `let*`
* There is no tail-call optimization, use `recur`

