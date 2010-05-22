# A Common Lisp-compatible Format Function
cl-format is an implementation of the incredibly baroque Common Lisp format function as specified 
in [Common Lisp, the Language, 2nd edition, Chapter 22](http://www.cs.cmu.edu/afs/cs.cmu.edu/project/ai-repository/ai/html/cltl/clm/node200.html#SECTION002633000000000000000).

Format gives you an easy and powerful way to format text and data for output. It supports rich 
formatting of strings and numbers, loops, conditionals, embedded formats, etc. It is really a 
domain-specific language for formatting.

This implementation for clojure has the following goals:

 * Support the full feature set of the Common Lisp format function (including the X3J13 extensions) with the only exception being concepts that make no sense or are differently interpreted in Clojure.
 * Make porting code from Common Lisp easier.
 * Provide a more native feeling solution for Clojure programmers than the Java format method and its relatives.
 * Be fast. This includes the ability to precompile formats that are going to be used reptitively.
 * Include useful error handling and comprehensive documentation.

## Why would I use cl-format?

For some people the answer to this question is that they are used to
Common Lisp and, therefore, they already know the syntax of format
strings and all the directives.

A more interesting answer is that cl-format provides a way of
rendering strings that is much more suited to Lisp and its data
structures. 

Because iteration and conditionals are built into the directive
structure of cl-format, it is possible to render sequences and other
complex data structures directly without having to loop over the data
structure. 

For example, to print the elements of a sequence separated by commas,
you simply say:

    (cl-format true "~{~a~^, ~}" aseq)

(This example is taken from 
[Practical Common Lisp](http://www.gigamonkeys.com/book/)
by Peter Seibel.)

The corresponding output using Clojure's Java-based _format_ function
would involve a nasty loop/recur with some code to figure out about
the commas. Yuck!

## Current Status of cl-format

cl-format is 100% compatible with the Common Lisp standard as
specified in CLtLv2.
This includes all of the functionality of Common
Lisp's format function including iteration, conditionals, 
text justification and rich
options for displaying real and integer values. It also includes the
directives to support pretty printing structured output.

If you find a bug in a directive, drop me a line
with a chunk of code that exhibits the bug and the version of
cl-format you found it in and I'll try to get it fixed.

I also intend to have good built-in documentation for the directives,
but I haven't built that yet.

The following directives are
not yet supported: ~:T and ~@:T (but all other forms of ~T work) 
and extensions with ~/. 

The pretty printer interface is similar, but not identical to the 
interface in Common Lisp.

Next up: 

 * Support for ~/
 * True compiled formats
 * Restructure unit tests into modular chunks.
 * Import tests from CLISP and SBCL.
 * Unit tests for exception conditions.
 * Interactive documentation
 
## How to use cl-format

### Loading cl-format in your program

Once cl-format is in your path, adding it to your code is easy:

    (ns your-namespace-here
      (:use [clojure.pprint :only (cl-format)]))

If you want to refer to the cl-format function as "format" (rather
than using the clojure function of that name), you can use this idiom:

    (ns your-namespace-here
      (:refer-clojure :exclude [format])
      (:use clojure.pprint))

    (def format cl-format)

You might want to do this in code that you've ported from Common Lisp,
for instance, or maybe just because old habits die hard.

From the REPL, you can grab it using (use):

    (use 'clojure.pprint)

### Calling cl-format

cl-format is a standard clojure function that takes a variable number
of arguments. You call it like this:

    (cl-format stream format args...)

_stream_ can be any Java Writer (that is java.io.Writer) or the values
_true_, _false_, or _nil_. The argument _true_ is identical to using
`*`out`*` while _false_ or _nil_ indicate that cl-format should return
its result as a string rather than writing it to a stream.

_format_ is either a format string or a compiled format (see
below). The format string controls the output that's written in a way
that's similar to (but much more powerful than) the standard Clojure
API format function (which is based on Java's
java.lang.String.Format).

Format strings consist of characters that are to be written to the
output stream plus directives (which are marked by ~) as in "The
answer is ~,2f". Format strings are documented in detail in 
[*Common Lisp the Language*, 2nd edition, Chapter 22](http://www.cs.cmu.edu/afs/cs.cmu.edu/project/ai-repository/ai/html/cltl/clm/node200.html#SECTION002633000000000000000).

_args_ is a set of arguments whose use is defined by the format.

## Using column aware streams across format invocations

Writers in Java have no real idea of current column or device page width, so the format
directives that want to work relative to the current position on the
page have nothing to work with. To deal with this, cl-format contains
an extension to writer called pretty-writer. A pretty-writer watches the
output and keeps track of what column the current output is going to.

When you call format and your format includes a directive that cares
about what column it's in (~T, ~&, ~<...~>), cl-format will
automatically wrap the Writer you passed in with a pretty-writer. This
means that by default all cl-format statements act like they begin on
a fresh line and have a page width of 72.

For many applications, these assumptions are fine and you need to do
nothing more. But sometimes you want to use multiple cl-format calls
that output partial lines. You may also want to mix cl-format calls
with the native clojure calls like print. If you want stay
column-aware while doing this you need to create a pretty-writer of
your own (and possibly bind it to `*`out`*`).

As an example of this, this function takes a nested list and prints it
as a table (returning the result as a string):

    (defn list-to-table [aseq column-width]
      (let [string-writer (java.io.StringWriter.)
            stream (get-pretty-writer string-writer)]
        (binding [*out* stream]
          (doseq [row aseq]
            (doseq [col row]
              (cl-format true "~4D~7,vT" col column-width))
            (prn)))
        (.flush stream)
        (.toString string-writer)))

(In reality, you'd probably do this as a single call to cl-format.)

The get-pretty-writer function takes the Writer to wrap and
(optionally) the page width (in columns) for use with ~<...~>. 

## Examples

The following function uses cl-format to dump a columnized table of the Java system properties:

    (defn show-props [stream]
      (let [p (mapcat 
    	       #(vector (key %) (val %)) 
    	       (sort-by key (System/getProperties)))]
        (cl-format stream "~30A~A~%~{~20,,,'-A~10A~}~%~{~30A~S~%~}" 
    	           "Property" "Value" ["" "" "" ""] p)))
    
There are some more examples in the pretty print examples gallery at 
http://github.com/tomfaulhaber/pprint-examples:

 * hexdump - a program that uses cl-format to create a standard formatted hexdump of the requested stream.
 * multiply - a function to show a formatted multipication table in a very "first-order" way.
 * props - the show-props example shown above.
 * show_doc - some utilities for showing documentation from various name spaces.

## Differences from the Common Lisp format function

The floating point directives that show exponents (~E, ~G) show E for
the exponent character in all cases (unless overridden with an
_exponentchar_).  Clojure does not distinguish between floats and
doubles in its printed representation and neither does cl-format.

The ~A and ~S directives accept the colon prefix, but ignore it since
() and nil are not equivalent in Clojure.

Clojure has 3 different reader syntaxes for characters. The ~@c
directive to cl-format has an argument extension to let you choose:

 * ~@c (with no argument) prints "\c" (backslash followed by the printed representation of the character or \newline, \space, \tab, \backspace, \return)
 * ~'o@c prints "\oDDD" where DDD are the octal digits representing the character. 
 * ~'u@c prints "\uXXXX" prints the hex Unicode representation of the character.  
