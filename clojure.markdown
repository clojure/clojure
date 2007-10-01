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
Clojure is a dynamic programming language that targets the [Java Virtual Machine][jvm]. It is designed to be a general-purpose language, combining the approachability and interactive development of a scripting language with an efficient and robust infrastructure for multithreaded programming. Clojure is a compiled language - it compiles directly to JVM bytecode, yet remains completely dynamic. *Every* feature supported by Clojure is supported at runtime. Clojure provides easy access to the Java frameworks, with *optional* type hints and type inference, to ensure that calls to Java can avoid reflection. 

Clojure is a dialect of Lisp, and shares with Lisp the code-as-data philosophy and a powerful macro system. Clojure is predominantly a [functional programming][fp] language, and features a rich set of immutable, [persistent data structures][pd]. When mutable state is needed, Clojure offers a [software transactional memory][stm] system that ensures clean, correct, multithreaded designs.

I hope you find Clojure's combination of facilities elegant, powerful, practical and fun to use.


[jvm]:http://java.sun.com/docs/books/jvms/
[fp]: http://en.wikipedia.org/wiki/Functional_programming
[pd]: http://en.wikipedia.org/wiki/Persistent_data_structure
[stm]:http://en.wikipedia.org/wiki/Software_transactional_memory

Rich Hickey

---
This documentation is continually being updated to reflect the current and new features of Clojure. Check back frequently for additions and corrections.

###[Setup](#setup)
###[Quick Start](#quickstart)
###[Reader](#reader)
###[Evaluation](#evaluation)
###[Special Forms](#specialforms)
###[Macros](#macros)
###[Other Useful Functions](#other)
###[Data Structures](#datastructures)
###[Metadata](#metadata)
###[Sequences](#sequences)
###[Namespaces](#namespaces)
###[Vars and the Global Environment](#vars)
###[Refs and Transactions](#refs)
###[Differences with other Lisps](#lisp)			

<h2 id="setup">Setup</h2>

Clojure is [hosted on SourceForge][sf]. 

Feedback and discussion should occur on the [Clojure Google Group][cgg].

Clojure is delivered in a zip file containing a single .jar, `clojure.jar`, a readme, the CPL license and the source code in a `src` subdirectory. It uses the [ASM 3.0 bytecode library][asm], and the current alpha distribution includes it. [Java][jdk] 1.5 or greater is required.

[asm]: http://asm.objectweb.org/
[jdk]: http://java.sun.com/javase/downloads/index.jsp
[sf]:  http://sourceforge.net/project/showfiles.php?group_id=137961
[cgg]: http://groups.google.com/group/clojure

<h2 id="quickstart">Quick Start</h2>
In the directory in which you expanded `clojure.zip`, run:

<pre><code>
java -cp clojure.jar clojure.lang.Compiler src/boot.clj
</code></pre>

This will bring up a simple read-eval-print loop (REPL). Much of Clojure is defined in Clojure itself (in the `boot.clj` file included in the `src` directory of distribution), so the command-line argument is needed to load it.

When boot.clj is loaded you will have the language as described herein fully available.

Try:

<pre><code>
user=> (+ 1 2 3)
6
user=> (. javax.swing.JOptionPane (showMessageDialog nil "Hello World"))	
</code></pre>

<h2 id="reader">Reader</h2>

Clojure is a [homoiconic][hicon] language, which is a fancy term describing the fact that Clojure programs are represented by Clojure data structures. This is a very important difference between Clojure (and Common Lisp) and most other programming languages - Clojure is defined in terms of the evaluation of a data structure and **not** in terms of the syntax of character streams/files. It is quite common, and easy, for Clojure programs to manipulate, transform and produce other Clojure programs.

That said, most Clojure programs begin life as text files, and it is the task of the *reader* to parse the text and produce the data structure the compiler will see. This is not merely a phase of the compiler. The reader, and the Clojure data representations, have utility on their own in many of the same contexts one might use XML or JSON etc.

One might say the reader has syntax defined in terms of characters, and the Clojure language has syntax defined in terms of symbols, lists, vectors, maps etc. The reader is represented by the function `read`, which reads the next form (not character) from a stream, and returns the object represented by that form.

Since we have to start somewhere, we might as well start where evaluation starts, with the reader forms. This will inevitably entail talking about data structures whose descriptive details, and interpretation by the compiler, will follow.

[hicon]:http://en.wikipedia.org/wiki/Homoiconicity
	
### Reader forms
* 	Symbols

	Symbols begin with a non-numeric character and can contain alphanumeric characters and *, +, !, -, _, and ? (other characters will be allowed eventually, but not all macro characters have been determined). '/' has special meaning, it can be used once in the middle of a symbol to separate the [namespace](#namespaces) from the name, e.g. `my-namespace/foo`. '/' by itself names the division function. '.' has special meaning - it can be used one or more times in the middle of a symbol to designate a fully-qualified class name, e.g. `java.util.BitSet`. Symbols beginning with '.' are reserved by Clojure.

* 	Literals
	* Strings - Enclosed in `"double quotes"`. Standard Java escape characters.
	* Numbers - as per Java, plus indefinitely long integers are supported, as well as ratios, e.g. `22/7`
	* Characters - preceded by a backslash: `\c`. 
	
		`\newline`, `\space` and `\tab` yield the corresponding characters.
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
	
	Keys and values can be any forms.

### Macro characters
The behavior of the reader is driven by a combination of built-in constructs and an extension system called the read table. Entries in the read table provide mappings from certain characters, called macro characters, to specific reading behavior, called reader macros. Unless indicated otherwise, macro characters cannot be used in user symbols.

*	Quote (')

	`'form` => `(quote form)`
*	Character (\\)

	As per above, yields a character literal.
	
*	Comment (;)

	Single-line comment, causes the reader to ignore everything from the semicolon to the end-of-line.
	
*	Meta (^)

	`^form` => `(meta form)`
*	Deref (@, @!)

	`@form` => `(deref form)`

	`@!form` => `(deref! form)`
	
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

*	Syntax-quote (\`, note, the "backquote" character), Unquote (~) and Unquote-splicing (~@)

	For all forms other than Symbols, Lists, Vectors and Maps, \`x is the same as 'x. 

	For Symbols, syntax-quote *resolves* the symbol in the current context, yielding a fully-qualified symbol (i.e. namespace/name or fully.qualified.Classname). If a symbol is non-namespace-qualified and ends with '#', it is resolved to a generated symbol with the same name to which '\_' and a unique id have been appended. e.g. `x#` will resolve to `x_123`. All references to that symbol within a syntax-quoted expression resolve to the same generated symbol.

	For Lists/Vectors/Maps, syntax-quote establishes a template of the corresponding data structure. Within the template, unqualified forms behave as if recursively syntax-quoted, but forms can be exempted from such recursive quoting by qualifying them with unquote or unquote-splicing, in which case they will be treated as expressions and be replaced in the template by their value, or sequence of values, respectively.

	For example:
<pre><code>
	
	user=> (def x 5)
	user=> (def lst '(a b c))
	user=> `(fred x ~x lst ~@lst 7 8 :nine)

	(user/fred user/x 5 user/lst a b c 7 8 :nine)
		
</code></pre>	


The read table is currently not accessible to user programs.



<h2 id="evaluation">Evaluation</h2>
Evaluation can occur in many contexts:

*	Interactively, in the REPL
*	On a sequence of forms read from a stream, via `load` or `load-file`
*	Programmatically, via `eval`

Clojure programs are composed of expressions. Every form not handled specially by a special form or macro is considered by the compiler to be an expression, which is evaluated to yield a value. There are no declarations or statements, although sometimes expressions may be evaluated for their side-effects and their values ignored.

In all cases, evaluation is the same - a single object is considered by the compiler, evaluated, and its result returned. If an expression needs to be compiled, it will be. There is no separate compilation step, nor any need to worry that a function you have defined is being interpreted. *Clojure has no interpreter*.

Strings, numbers, characters, `nil` and keywords evaluate to themselves.

A Symbol is *resolved*:

*	If it is namespace-qualified, the value is the value of the binding of the global var named by the symbol. It is an error if there is no global var named by the symbol.
*	Else, it is not namespace-qualified and the first of the following applies:
 	1. If it names a special form it is considered a special form, and must be utilized accordingly.
	2. A lookup is done in the \*imports\* map to see if there is a mapping from the symbol to a fully qualified class name. If so, the symbol is considered to name a Java class. Note that class names are not first-class objects and are only valid in certain special forms.
	3. If in a local scope (i.e. in a function definition), a lookup is done to see if it names a local binding (e.g. a function argument or let-bound name). If so, the value is the value of the local binding.
	4. A lookup is done in the \*refers\* map to see if there is a mapping from the symbol to a global var. If so, the value is the value of the binding of the global var referred-to by the symbol.
	5. A lookup is done to see if there is a global var with a namespace equal to the `*current-namespace*` and the same name as the symbol. If so, the value is the value of the binding of that global var.
	6. It is an error.
	
If a Symbol has metadata, it may be used by the compiler, but will not be part of the resulting value.

Vectors and Maps yield vectors and (hash) maps whose contents are the *evaluated values* of the objects they contain. The same is true of metadata maps. If the vector or map has metadata, the *evaluated* metadata map will become the metadata of the resulting value.

<pre><code>
user=> (def x 1)
user=> (def y 2)
user=> #^{:x x} [x y 3]

#^{:x 1} [1 2 3]
</code></pre>	

An empty list `()` evaluates to an empty list.

Non-empty Lists are considered *calls* to either special forms, macros, or functions. A call has the form `(operator operands*)`. 

Special forms are primitives built-in to Clojure that perform core operations. If the operator of a call is a symbol that resolves to the name of a special form, the call is to that special form. Each form discussed individually under [Special Forms](#specialforms).

[Macros](#macros) are functions that manipulate forms, allowing for syntactic abstraction. If the operator of a call is a symbol that names a global var that is a macro function, that macro function is called and is passed the *unevaluated* operand forms. The return value of the macro is then evaluated in its place.

If the operator is not a special form or macro, the call is considered a function call. Both the operator and the operands (if any) are evaluated, from left to right. The result of the evaluation of the operator is then cast to IFn (the interface representing Clojure functions), and invoke() is called on it, passing the evaluated arguments. The return value of invoke() is the value of the call expression. If the function call form has metadata, it may be used by the compiler, but will not be part of the resulting value.

Note that special forms and macros might have other-than-normal evaluation of their arguments, as described in their entries under [Special Forms](#specialforms).

The above describes the evaluation of a single form. `load` and `load-file` will sequentially evaluate the set of forms contained in the stream/file. Such sets of forms usually have side effects, often on the global environment, defining functions etc. The loading functions occur in a temporary context, in which `*current-namespace*` has a fresh binding. That means that, should any form have an effect on that var (e.g. `in-namespace), the effect will unwind at the completion of the load. 

<h2 id="specialforms">Special Forms</h2>

---
### (*def* symbol init?)
Creates or locates a global var with the name of `symbol` and a namespace of the value of `*current-namespace*`. If `init` is supplied, it is evaluated, and the root binding of the var is set to the resulting value. If `init` is not supplied, the root binding of the var is unaffected. `def` always applies to the root binding, even if the var is thread-bound at the point where def is called. `def` yields the var itself *(not its value)*. Throws an exception if symbol is in the `*refers*` map.

---
### (*if* test then else?)
Evaluates `test`. If not nil, evaluates and yields `then`, otherwise, evaluates and yields `else`. If `else` is not supplied it defaults to `nil`.

---
### (*do* exprs*)
Evaluates the expressions in order and returns the value of the last. If no expressions are supplied, returns `nil`.

---
### (*let* [bindings* ] exprs*)
binding => symbol init-expr

Evaluates the exprs in a context in which the symbols are bound to their respective init-exprs. The bindings are sequential, so each binding can see the prior bindings. The exprs are contained in an implicit `do`. If a binding symbol is annotated with a metadata tag, the compiler will try to resolve the tag to a class name and presume that type in subsequent references to the binding.

<pre><code>
user=> (let [x 1 y x] y)

1
</code></pre>	

---
### (*quote* form)
Yields the unevaluated form.

<pre><code>
user=> '(a b c)

(a b c)
</code></pre>

Note there is no attempt made to call the function `a`. The return value is a list of 3 symbols.

---
### (*the-var* symbol)
The symbol must resolve to a var, and the Var object itself *(not its value)* is returned.

---
### (*fn* [params* ] exprs*)
### (*fn* ([params* ] exprs*)+)
params => positional-params* , or positional-params* `&` rest-param

positional-param => symbol

rest-param => symbol

Defines a function (fn). Fns are first-class objects that implement the IFn interface. The IFn interface defines an invoke() function that is overloaded with arity ranging from 0-20. A single fn object can implement one or more invoke methods, and thus be overloaded on arity. One and only one overload can itself be variadic, by specifying the ampersand followed by a single rest-param. Such a variadic entry point, when called with arguments that exceed the positional params, will find them in a seq contained in the rest param. If the supplied args do not exceed the positional params, the rest param will be nil.

The first form defines a fn with a single invoke method. The second defines a fn with one or more overloaded invoke methods. The arities of the overloads must be distinct. In either case, the result of the expression is a single fn object.

The exprs are compiled in an environment in which the params are bound to the actual arguments. The exprs are enclosed in an implicit `do`. The reserved symbol `thisfn` is bound within the function definition to the function object itself, allowing for self-calling, even in anonymous functions. If a param symbol is annotated with a metadata tag, the compiler will try to resolve the tag to a class name and presume that type in subsequent references to the binding.

<pre><code>
(def *
  (fn ([] 1)
      ([x] x)
      ([x y] (. Num (multiply x y)))
      ([x y & more]
          (apply thisfn (this-fn x y) more))))
</code></pre>

A fn (overload) defines a recursion point at the top of the function, with arity equal to the number of params *including the rest param, if present*. See `recur`.

IFns are Callable.

---
### (*loop* [bindings* ] exprs*)

`Loop` is exactly like `let`, except that it establishes a recursion point at the top of the loop, with arity equal to the number of bindings. See `recur`.

---
### (*recur* exprs*)

Evaluates the exprs in order, then, in parallel, rebinds the bindings of the recursion point to the values of the exprs. If the recursion point was a fn method, then it rebinds the params. If the recursion point was a `loop`, then it rebinds the loop bindings. Execution then jumps back to the recursion point. The `recur` expression must match the arity of the recursion point exactly. In particular, if the recursion point was the top of a variadic fn method, there is no gathering of rest args - a single seq (or null) should be passed. `recur` in other than a tail position is an error.

Note that `recur` is the only non-stack-consuming looping construct in Clojure. There is no tail-call optimization and the use of self-calls for looping of unknown bounds is discouraged. `recur` is functional and its use in tail-position is verified by the compiler.

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

Note that boolean return values will be turned into `nil` or non-nil, chars will become Characters, and numeric primitives will become Clojure Nums.

---
### (*new* Classname-symbol args*)
The args, if any, are evaluated from left to right, and passed to the constructor of the class named by the symbol. The constructed object is returned.

---
### (*class* Classname-symbol)
Yields the java.lang.Class object corresponding to the symbol.

---
### (*instance?* expr Classname-symbol)
Evaluates expr and tests if it is an instance of the class named by the symbol. Returns `nil` or non-nil

---
### (*throw* expr)
The expr is evaluated and thrown, therefor it should yield an instance of some derivee of Throwable.

---
### (*try-finally* expr finally-expr)
The expr is evaluated and its value returned. Before returning, normally or abnormally, the finally-expr will be evaluated for its side effects.

---
### (*set!* (. instance-expr instanceFieldName-symbol) expr)
### (*set!* (. Classname-symbol staticFieldName-symbol) expr)
### (*set!* var-symbol expr)
Assignment. 

When the first operand is a field member access form, the assignment is to the corresponding field. If it is an instance field, the instance expr will be evaluated, then the expr.

When the first operand is a symbol, it must resolve to a global var. The value of the var's current thread binding is set to the value of expr. Currently, it is an error to attempt to set the root binding of a var using `set!`, i.e. var assignments are thread-local.

In all cases the value of expr is returned.

Note - *you cannot assign to function params or local bindings. Only Java fields, Vars and Refs are mutable in Clojure*.

---
### (*monitor-enter* x)
### (*monitor-exit* x)

These are synchronization primitives that should be avoided in user code. Use the `locking` macro.

<h2 id="macros">Macros</h2>
Clojure has a programmatic macro system which allows the compiler to be extended by user code. Macros can be used to define syntactic constructs  which would require primitives or built-in support in other languages. Many core constructs of Clojure are not, in fact, primitives, but are normal macros. First we'll cover some of the included macros and then look at the facilities for creating your own.

---
### (*and* exprs*)
Evaluates exprs one at a time, from left to right. If a form returns `nil`, `and` returns `nil` and doesn't evaluate any of the other expressions, otherwise it returns the value of the last expr. `(and)` returns `t`.

---
### (*or* exprs*)
Evaluates exprs one at a time, from left to right. If a form returns a non-nil value, `or` returns that value and doesn't evaluate any of the other expressions, otherwise it returns `nil`. `(or)` returns `nil`.

---
### (*when* test exprs*)
Evaluates `test`. If non-nil, evaluates exprs in an implicit `do`.

---
### (*when-not* test exprs*)
Evaluates `test`. If `nil`, evaluates exprs in an implicit `do`.

---
### (*cond* test-expr-pairs*)
test-expr-pair => test expr

`cond` takes a set of test/expr pairs. It evaluates each test one at a time. If a test returns non-nil, `cond` evaluates and returns the value of the corresponding expr and doesn't evaluate any of the other tests or exprs. `(cond)` returns `nil`. `cond ` is a succinct and readable alternative to nested `if`s.

---
### (*locking* x exprs*)
Executes exprs in an implicit `do`, while holding the monitor of x. Will release the monitor of x in all circumstances.

---
### (*..* instance-expr member+)
### (*..* Classname-symbol member+)

member => fieldName-symbol or (instanceMethodName-symbol args*)

Expands into a member access (.) of the first member on the first argument, followed by the next member on the result etc. For instance:
 
`(.. System (getProperties) (get "os.name"))`

expands to:

`(. (. System (getProperties)) (get "os.name"))`

but is easier to write, read, and understand.

---
### (*defn* name [params* ] exprs*)
### (*defn* name ([params* ] exprs*)+)
Same as `(def name (fn [params* ] exprs*))` or  `(def name (fn ([params* ] exprs*)+))`

---
### (*defmacro* name [params* ] exprs*)
### (*defmacro* name ([params* ] exprs*)+)
Like defn, but the resulting function name is declared as a macro and will be used as a macro by the compiler when it is called.

<h2 id="other">Other Useful Functions</h2>

---
### (*apply* f args* argseq)
Applies fn f to the argument list formed by prepending args to argseq.

---
### (*appl* f args+)
Takes a function f and fewer than the normal arguments to f, and returns a fn that takes a variable number of additional args. When called, the returned function calls f with args + additional args.

<pre><code>

user=> (map (appl + 2) [1 2 3])
(3 4 5)

</code></pre>

---
### (*comp* fns+)

Takes a set of functions and returns a fn that is the composition of those fns. The returned fn takes a variable number of args, applies the rightmost of fns to the args, the next fn (right-to-left) to the result, etc.

((comp a b c) x y z)  ==> (a (b (c x y z))) 

---
### (*nil?* x)
### (*not* x)

Returns non-nil if x is nil.

---
### (*complement* f)
Takes a fn `f` and returns a fn that takes the same arguments as f, has the same effects, if any, and returns the opposite truth value.

---
### (*constantly* x)
Returns a function that takes any number of arguments and returns x.

---
### (*identity* x)
Returns x.

---
### (*str* x)

Returns x.toString()

---
### (*strcat* x y)

Returns x.concat(y)


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

---
### (*eql?* obj1 obj2)

Supported by all data structures.  Returns non-nil if obj1 equals obj2, `nil` if not. Same as Java `obj1.equals(obj2)` except it also works for `nil`.


### _nil_
`nil` is a possible value of any data type in Clojure (since primitives are always boxed). `nil` has the same value as Java `null`. The Clojure conditional system is based around `nil`/non-nil, with `nil` representing the value of logical false in conditional tests - anything else is logical true. In addition, `nil` is used as the end-of-sequence sentinel value in the sequence protocol.

### _Nums_
All Clojure numbers are derived from clojure.lang.Num, which in turn is derived from java.lang.Number. There are 4 types:

* 	FixNum
	
	Represent integer values that fit in 32 bits. When arithmetic involving FixNums exceeds their capacity, they automatically become BigNums.
	
*	BigNum

	Represent integers of arbitrary precision.
	
*	DoubleNum

	Correspond to Java's `double`.
	
*	RatioNum

	Represents a ratio between integers. Division of integers that can't be reduced to an integer yields a ratio, i.e. 22/7 = 22/7, rather than a floating point value.
	
Any numeric operation involving DoubleNums yields a DoubleNum. 

---
### (*+* nums*)
Returns the sum of nums. `(+)` returns 0.

---
### (*\** nums*)
Returns the product of nums. `(*)` returns 1.

---
### (*/* numerator denominators*)
If no denominators are supplied, returns 1/numerator, else returns numerator divided by all of the denominators.

---
### (*-* num subs*)
If no subs are supplied, returns the negation of num, else subtracts of the subs from num and returns the result.

---
### (*==* nums+)
Returns non-nil if nums all have the same value, otherwise `nil`.

<pre><code>

user=> (== 1 1.0)
t
user=> (eql? 1 1.0)
nil

</code></pre>
---
### (*<* nums+)
Returns non-nil if nums are in monotonically increasing order, otherwise `nil`.

---
### (*<=* nums+)
Returns non-nil if nums are in monotonically non-decreasing order, otherwise `nil`.

---
### (*>* nums+)
Returns non-nil if nums are in monotonically decreasing order, otherwise `nil`.

---
### (*>=* nums+)
Returns non-nil if nums are in monotonically non-increasing order, otherwise `nil`.

---
### (*inc* num)
Returns a number one greater than num.

---
### (*dec* num)
Returns a number one less than num.

---
### (*zero?* num)
Returns non-nil if num is zero, else `nil`

---
### (*pos?* num)
Returns non-nil if num is greater than zero, else `nil`

---
### (*neg?* num)
Returns non-nil if num is less than zero, else `nil`

### _Strings_
Clojure strings are Java `Strings`.

### _Characters_
Clojure characters are Java `Characters`.

### _Keywords_
Keywords are symbolic identifiers that evaluate to themselves. They provide very fast equality tests. Like Symbols, they have names and optional [namespaces](#namespaces), both of which are strings. The leading ':' is not part of the namespace or name. Keywords implement IFn, for invoke() of one argument, which they expect to be a map, in which they look themselves up, i.e. keywords are functions of maps.



### _Symbols_
Symbols are identifiers that are normally used to refer to something else. They can be used in program forms to refer to function parameters, let bindings, class names and global vars. They have names and optional [namespaces](#namespaces), both of which are strings. Symbols can have metadata.

---
### (*gensym* prefix?)

Returns a new symbol with a unique name. If a prefix string is supplied, the name is prefix__# where # is some unique number. If prefix is not supplied, the prefix is "G".

### _Collections_

All of the Clojure collections are immutable and [persistent][pd]. In particular, the Clojure collections support efficient creation of 'modified' versions, by utilizing structural sharing, and make all of their performance bound guarantees for persistent use. The collections are efficient and inherently thread-safe. Collections are represented by abstractions, and there may be one or more concrete realizations. In particular, since 'modification' operations yield new collections, the new collection might not have the same concrete type as the source collection, but will have the same logical (interface) type. All the collections support these functions:

---
### (*count* coll)
Returns the number of items in the collection. `(count nil)` returns `0`.

---
### (*conj* coll item)
Conj[oin]. Returns a *new* collection with the item 'added'. `(conj nil item)` returns `(item)`. The 'addition' may happen at different 'places' depending on the concrete type.

---
### (*seq* coll)
Sequence. Returns a new ISeq on the collection. If the collection is empty, returns `nil`. `(seq nil)` returns `nil`. `seq` also works on native Java arrays and any objects that implement Iterable.


### _Lists (IPersistentList)_
Lists are collections. They implement the ISeq interface directly. `count` is O(1). `conj` puts the item at the front of the list. In addition, lists support the functions:

---
### (*list*  items*)
Creates a new list containing the items.

---
### (*list** items* seq)
Creates a new list containing the items prepended to seq.

---
### (*peek* list)
Same as `first`. Returns the first item in the list. If the list is empty, returns `nil`.

---
### (*pop* list)
Returns a new list without the first item. If the list is empty, throws an exception. Note - *not* the same as `rest`.

### _Vectors (IPersistentVector)_
Vectors are collections. They are sequences of values indexed by contiguous integers. Vectors support O(log<sub>32</sub>N) access to items by index. `count` is O(1). `conj` puts the item at the end of the vector. Vectors also support `rseq`, which returns the items in reverse order. Vectors implement IFn, for invoke() of one argument, which they presume is an index and look up in themselves as if by `nth`, i.e. vectors are functions of their indices.  In addition, vectors support the functions:

---
### (*vector* items*)
Creates a new vector containing the items.

---
### (*assoc* vector index val)
Assoc[iate]. Returns a new vector that contains val at index. Note - index must be <= (count vector).

---
### (*get* vector index)
### (*nth* vector index)
Returns the value at the index. `get` returns `nil` if index out of bounds, `nth` throws an exception. `nth` also works for Java arrays, and, in O(n) time, for sequences.

---
### (*peek* vector)
Returns the last item in the vector. If the vector is empty, returns `nil`.

---
### (*pop* vector)
Returns a new vector without the last item. If the vector is empty, throws an exception.
		
### _Maps_ (IPersistentMap)

Map keys to values. Two different map types are provided - hashed and sorted. Hash maps require keys that correctly support hashCode and equals. Sorted maps require keys that implement Comparable, or an instance of Comparator. Hash maps provide faster access O(log<sub>32</sub>N) vs O(logN), but sorted maps are, well, sorted. `count` is O(1). `conj` expects another (possibly single entry) map as the item, and returns a new map which is the old map plus the entries from the new, which may overwrite entries of the old. `seq` returns a sequence of map entries, which are key/value pairs. Sorted map also supports `rseq`, which returns the entries in reverse order. Maps implement IFn, for invoke() of one argument, which they presume is a key and look up in themselves, i.e. maps are functions of their keys. `nil` keys and values are ok. 

Map functions:

---
### (*hash-map* keyvals*)
### (*sorted-map* keyvals*)
### (*sorted-map-by* comparator keyvals*)

keyval => key val

Returns a new hash/sorted map with supplied mappings.

---
### (*assoc* map key val)
Assoc[iate]. Returns a new map of the same (hashed/sorted) type, that contains the mapping of key to val.

---
### (*dissoc* map key)
Dissoc[iate]. Returns a new map of the same (hashed/sorted) type, that does not contain a mapping for key.

---
### (*get* map key)
Returns the value mapped to key, or `nil` if key not present.

---
### (*contains* map key)
Returns `nil` if key not present, else non-nil.

---
### (*find* map key)
Returns the map entry for key, or `nil` if key not present.

---
### (*keys* map)
Returns a sequence of the map's keys.

---
### (*vals* map)
Returns a sequence of the map's values.

---
### (*merge* maps+)
Returns a map that consists of the rest of the maps conj-ed onto the first. If a key occurs in more than one map, the mapping from the latter (left-to-right) will be the mapping in the result.

<h2 id="sequences">Sequences</h2>

Clojure defines many algorithms in terms of sequences (seqs). A seq is a logical list, and unlike most Lisps where the list is represented by a concrete, 2-slot structure, Clojure uses the ISeq interface to allow many data structures to provide access to their elements as sequences. The `seq` function yields an implementation of ISeq appropriate to the collection. Seqs differ from iterators in that they are persistent and immutable, not stateful cursors into a collection. As such, they are useful for much more than foreach - functions can consume and produce seqs, they are thread safe, they can share structure etc. 

Most of the sequence library functions are *lazy*, i.e. functions that return seqs do so incrementally, as they are consumed, and thus consume any seq arguments incrementally as well.

When `seq` is used on native Java arrays and objects that implement Iterable, the resulting sequence is still immutable and persistent, and will represent a single pass across the data. Because that pass might happen lazily, the pass might see changes that happen after `seq` has been called. Also, if the backing iterator is subject to ConcurrentModificationException, then so to is the resulting seq. That said, there is still a lot of utility to using seq on Iterables and arrays - since seqs support multi-pass and lazy algorithms. Robust programs should not mutate arrays or Iterables that have seqs on them.

####The Seq interface:

---
### (*first* seq)
Returns the first item in the sequence. Is seq is `nil`, returns `nil`.

---
### (*rest* seq)
Returns a seq of the items after the first. If there are no more items, returns `nil`.

---
### (*cons* item coll)
Returns a new seq where item is the first element and the sequence of items in coll is the rest of the seq.

####The Seq library:

---
### (every pred seq)
### (not-every pred seq)
### (any pred seq)
### (not-any pred seq)
### (concat seqs*)
### (map f seqs*)
### (mapcat f seqs*)
### (reduce f seq)
### (reduce f seq val)
### (filter pred seq)
### (take n seq)
### (take-while pred seq)
### (drop n seq)
### (drop-while pred seq)
### (reverse seq)
### (cycle seq)
### (split-at n seq)
### (split-with pred seq)
### (repeat x)
### (replicate n x)
### (iterate f x)	

<h2 id="metadata">Metadata</h2>

Symbols and collections support metadata, a map of data *about* the symbol or collection. The metadata system allows for arbitrary annotation of data. It is used to convey information to the compiler about types, but can also be used by application developers for many purposes, annotating data sources, policy etc.

An important thing to understand about metadata is that it is not considered to be part of the value of an object. As such, *metadata does not impact equality (or hash codes)*. Two objects that differ only in metadata are equal.

That said, metadata and its relationship to an object is immutable - an object with different metadata is a different object.

---
### (*meta* obj)
### ^obj
Returns the metadata of obj, returns `nil` if there is no metadata.

---
### (*with-meta* obj map)
Returns an object of the same type and value as obj, with map as its metadata.


<h2 id="namespaces">Namespaces</h2>

Symbols and keywords are two-part identifiers - with an optional namespace and a name, both strings. Namespaces are used to distinguish two symbols that have the same name. Vars are named by symbols that must have a namespace part, and thus can be considered to be in namespaces. Note that namespaces are not first-class - they are not collections of symbols/vars sharing the same prefix, they cannot be enumerated etc. Essentially, namespaces just allow simpler identifiers to be used in contexts where a namespace is either not required (fn params and let-locals) or can be inferred. Namespaces do have 2 concrete properties, `*imports*` and `*refers*`, described below, which create a lookup context associated with a namespace.

As we've seen, all `def`s create vars in the current namespace. The `*current-namespace*` can and should be set only with a call to `in-namespace`.  `in-namespace` will set `*current-namespace*` to the supplied symbol, and will set up the bindings to the `*refers*` and `*imports*` vars. There are `*refers*` and `*imports*` in each namespace, and `in-namespace` will create them if they don't already exist. `*refers*` is a map from simple symbols to Vars, and its initial value consists of the symbols exported from the `clojure` namespace. `*imports*` is a map from simple symbols to fully-qualified classnames, and its initial value is the classes from the `java.lang` package.
Mappings can be added to `*refers*` with `refer`, and to `*imports*` with `import`, and only via those functions.

---
### (*in-namespace* ns-symbol)
Sets `*current-namespace*` to the supplied symbol, and will set up the bindings to the `*refers*` and `*imports*` vars, creating them if they don't already exist.

---
### (*import* import-lists+)

import-list => (package-symbol class-name-symbols*)

For each name in class-name-symbols, adds a mapping from `name` to "package.name" to the `*imports*` map of the current namespace.

<pre><code>
(import '(java.util Date Timer Random)
        '(java.sql Connection Statement))
</code></pre>
  
---
### (*unimport* symbols+)

Removes the mappings from the `*imports*` map of the current namespace.

<pre><code>
(unimport 'Date 'Statement)
</code></pre>

---
### (*refer* refer-lists+)

refer-list => (ns-symbol name-symbols*)

For each name in name-symbols, adds a mapping from `name` to the var named ns/name to the `*refers*` map of the current namespace. The vars must exist. Throws an exception if name is already in `*refers*` mapped to a different var.

<pre><code>
(refer '(fred-ns ricky lucy ethel)
       '(barney-ns wilma fred betty))
</code></pre>
  
---
### (*unrefer* symbols+)

Removes the mappings from the `*refers*` map of the current namespace.

---
### (*name* symbol-or-keyword)
Returns the name String.

---
### (*namespace* symbol-or-keyword)
Returns the namespace String or `nil` if not present.

<h2 id="vars">Vars and the Global Environment</h2>
Clojure is a practical language that recognizes the occasional need to maintain a persistent reference to a changing value and provides 2 distinct mechanisms for doing so in a controlled manner - Vars and [Refs](*refs). Vars provide a mechanism to refer to a mutable storage location that can be dynamically rebound (to a new storage location) on a per-thread basis. Every Var can (but needn't) have a root binding, which is a binding that is shared by all threads that do not have a per-thread binding. Thus, the value of a Var is the value of its per-thread binding, or, if it is not bound in the thread requesting the value, the value of the root binding, if any.

The special form `def` creates (and [interns](#interning)) a Var. If the Var did not already exist and no initial value is supplied, the var is unbound:

<pre><code>
	
user=> (def x)
Var: user/x
user=> x
java.lang.IllegalStateException: Var user/x is unbound.

</code></pre>

Supplying an initial value binds the root (even if it was already bound).

<pre><code>
	
user=> (def x 1)
Var: user/x
user=> x
1

</code></pre>

Per-thread bindings for one or more Vars can be established via the macro `binding`:

---
### (*binding* [bindings* ] exprs*) - Macro
binding => var-symbol init-expr

Creates new bindings for the (*already-existing*) vars, with the supplied initial values, executes the exprs in an implicit `do`, then re-establishes the bindings that existed before. 

Thus within-thread bindings obey a stack discipline:

<pre><code>
	
user=> (def x 1)
user=> (def y 1)
user=> (+ x y)
2
user=> (binding [x 2 y 3] 
         (+ x y))
5
user=> (+ x y)
2

</code></pre>

Bindings created with `binding` cannot be seen by any other thread. Bindings created with `binding` can be assigned to, which provides a means for nested contexts to communicate with code before it the call stack.

Functions defined with `defn` are stored in Vars, allowing for the re-definition of functions in a running program. This also enables many of the possibilities of aspect- or context-oriented programming. For instance, you could wrap a function with logging behavior only in certain call contexts or threads. 

<h3 id="interning">Interning</h3>
	
The Var system maintains a global map of (namespace-qualified) symbols to Var objects. If a `def` expression does not find an entry in this map for the symbol being `def`-ed, it creates one, otherwise, it uses the existing Var. This find-or-create process is called interning. This means is that, unless they have been `unintern`-ed, Var objects are stable references, and need not be looked up every time. It also means that the Var map constitutes a global environment, in which, as described in [Evaluation](#evaluation), the compiler attempts to resolve all free symbols as Vars.

---
### (*find-var* varsym)

Returns the global var named by the namespace-qualified symbol, or `nil` if not var with that name.

---
### (*unintern* varsym)

Removes the global var named by the namespace-qualified symbol from the global namespace.

 
<h2 id="refs">Refs and Transactions</h2>
While Vars ensure safe use of mutable mutable storage locations via thread isolation, Refs ensure safe *shared* use of mutable storage locations via a [software transactional memory][stm] (STM) system. Refs are bound to a single storage location for their lifetime, and only allow reference to<sup>*</sup>, and mutation of, that location to occur within a transaction.

<sup>*</sup>(except the `deref!` function, which yields the current value of a Ref outside of a transaction)

Clojure transactions should be easy to understand if you've ever used database transactions - they ensure that all actions on Refs are atomic and isolated. Atomic means that every change to Refs made within a transaction occurs or none do. Isolated means that no transaction sees the effects of any other transaction while it is running. Another feature common to STMs is that, should a transaction have a conflict while running, it is automatically retried.

There are many ways to do STMs (locking/pessimistic, lock-free/optimistic and hybrids) and it is still a research problem. The Clojure STM is, I think, novel in its use of [multiversion concurrency control][mvcc] with adaptive history queues for [snapshot isolation][snapshot], and providing a distinct `commute` operation.

In practice, this means:

1.	All reads of Refs will see a consistent snapshot of the 'Ref world' as of the starting point of the transaction (its 'read point'). The transaction *will* see any changes it has made. This is called the *in-transaction-value*.

2.	All changes made to Refs during a transaction (via `set` or `commute`) will appear to occur at a single point in the 'Ref world' timeline (its 'write point').

3.	No changes will have been made by any other transactions to any Refs that have been `set` by this transaction.

4.	Changes *may have* been made by other transactions to any Refs that have been `commute`d by this transaction. That should be okay since the function applied by `commute` should be commutative.

5.	Readers and commuters will never block writers, commuters, or other readers.

6.	Writers will never block commuters, or readers.

7.	I/O and other activities with side-effects should be avoided in transactions, since transactions *will* be retried.

8.	If a constraint on the validity of a value of a Ref that is being changed depends upon the simultaneous value of a Ref that is *not being changed*, that second Ref can be protected from modification by calling `set` without a second argument. Refs 'set' this way will be protected (item #3), but don't change the world (item #2).

9.	The Clojure MVCC STM is designed to work with the persistent collections, and it is strongly recommended that you use the Clojure collections as the values of your Refs. Since all work done in an STM transaction is speculative, it is imperative that there be a low cost to making copies and modifications. Persistent collections have free copies (just use the original, it can't be changed), and 'modifications' share structure efficiently. In any case:

10.	The values placed in Refs *must be, or be considered, immutable*!! Otherwise, Clojure can't help you.

[mvcc]:     http://en.wikipedia.org/wiki/Multiversion_concurrency_control
[snapshot]: http://en.wikipedia.org/wiki/Snapshot_isolation

---
### (*ref* init-val)
Creates and returns a Ref with an initial value of `init-val`.

---
### (*sync* transaction-flags exprs*) - Macro
transaction-flags => TBD, pass `nil` for now

Runs the exprs (in an implicit `do`) in a transaction that encompasses exprs and any nested calls. Starts a transaction if none is already running on this thread. Any uncaught exception will abort the transaction and flow out of `sync`. The exprs may be run more than once, but any effects on Refs will be atomic.

---
### (*deref* ref)
### @ref

Must be called in a transaction. Returns the in-transaction-value of ref.

### (*deref!* ref)
### @!ref

May be called outside a transaction. Returns the most-recently-committed value of ref.

---
### (*set* ref)
Must be called in a transaction. Protects the ref from modification by other transactions. Returns the in-transaction-value of ref. Allows for more concurrency than `(set ref @ref)`

### (*set* ref val)
Must be called in a transaction. Sets the value of ref. Returns val.

---
### (*commute* ref fun)
Must be called in a transaction. Sets the in-transaction-value of ref to:

 `(fun in-transaction-value-of-ref)`

At the commit point of the transaction, sets the value of ref to be: 

`(fun most-recently-committed-value-of-ref)`

Thus `fun` should be commutative, or, failing that, you must accept last-one-in-wins behavior. `commute` allows for more concurrency than `set`.

<h2 id="lisp">Differences with other Lisps</h2>
This information is provided for programmers familiar with Common Lisp or Scheme.
 
* Clojure is case sensitive
* Clojure is a Lisp-1
* () is not the same as nil
* The reader is side-effect free
* Keywords are not Symbols
* Symbols are not storage locations (see Var)
* `nil` is not a Symbol
* The read table is currently not accessible to user programs
* `let` binds sequentially
* `do` is not a looping construct
* There is no tail-call optimization, use `recur`.
* syntax-quote does symbol resolution, so \`x is not the same as 'x. 
* \` has auto-gensyms.
* ~ is unquote ',' is whitespace
* There is reader syntax for maps and vectors
* `cons`, `first` and `rest` manipulate sequence abstractions, not concrete cons cells
* Most data structures are immutable
* lambda is fn, and supports overloading by arity
* eql? is the equality predicate
* All Vars can be dynamically rebound, no `special` declaration. Since Clojure is a Lisp-1, functions can be dynamically rebound.

<a href="http://sourceforge.net"><img src="http://sflogo.sourceforge.net/sflogo.php?group_id=137961&amp;type=1" width="88" height="31" border="0" alt="SourceForge.net Logo" /></a>