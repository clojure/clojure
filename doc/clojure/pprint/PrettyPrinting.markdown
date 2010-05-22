# A Pretty Printer for Clojure

## Overview

This namespace adds a new feature to Clojure: a generalized pretty
printer.

The pretty printer is easy to use:

    user=> (println (for [x (range 10)] (range x)))
    (() (0) (0 1) (0 1 2) (0 1 2 3) (0 1 2 3 4) (0 1 2 3 4 5) (0 1 2 3 4 5 6) (0 1 2 3 4 5 6 7) (0 1 2 3 4 5 6 7 8))
    nil
    user=> (use 'clojure.pprint)             
    nil
    user=> (pprint (for [x (range 10)] (range x)))         
    (()
     (0)
     (0 1)
     (0 1 2)
     (0 1 2 3)
     (0 1 2 3 4)
     (0 1 2 3 4 5)
     (0 1 2 3 4 5 6)
     (0 1 2 3 4 5 6 7)
     (0 1 2 3 4 5 6 7 8))
    nil
    user=>

The pretty printer supports two modes: _code_ which has special
formatting for special forms and core macros and _simple_ (the
default) which formats the various Clojure data structures as
appropriate for raw data. In fact, the pretty printer is
highly customizable, but basic use is pretty simple.

All the functions and variables described here are in the
clojure.pprint namespace. Using them is as simple as adding a 
`(:use clojure.pprint)` to
your namespace declarations. Or, better practice would be 
`(:use [clojure.pprint :only (<functions you wish to use>)])`.

pprint is being developed by Tom Faulhaber (to mail me you can use
my first name at my domain which is infolace.com).

As with the rest of Clojure, the pretty printer is licensed under the 
[http://opensource.org/licenses/eclipse-1.0.php Eclipse Public License 1.0].

Future development is guided by those using it, so send feedback about
what's working and not working for you and what you'd like to see in the 
pretty printer.

## Pretty Printing Basics

Pretty printing is primarily implemented with the function
pprint. pprint takes a single argument and formats it according to the
settings of several special variables.

Generally, the defaults are fine for pretty printing and you can
simply use:

    (pprint obj)

to print your object. If you wish to write to
another stream besides `*`out`*`, you can use:

    (write obj :pretty true :stream foo)

where foo is the stream to which you wish to write. (The write
function has a lot more options which are not yet documented. Stay
tuned.)

When at the REPL, the pp macro pretty prints the last output
value. This is useful when you get something too complex to read
comfortably. Just type:

    user=> (pp)

and you'll get a pretty printed version of the last thing output (the
magic variable `*`1).

## Dispatch tables and code formatting

The behavior of the pretty printer can be finely controlled through
the use of _dispatch tables_ that contain descriptions for how
different structures should be formatted. 

Using custom dispatch tables, the pretty printer can create formatted
output for data structures that is customized for the
application. This allows pretty printing to be baked into any
structured output. For information and examples, see below in
[#Custom_Dispatch_Functions Custom Dispatch Functions].

The pretty printer comes with two pre-defined dispatch tables to cover
the most common situations:

`*`simple-dispatch`*` - supports basic representation of data in various
Clojure structures: seqs, maps, vectors, etc. in a fairly statndard
way. When structures need to be broken across lines, following lines
are indented to line up with the first element. `*`simple-dispatch`*` is
the default and is good for showing the output of most operations.

`*`code-dispatch`*` - has special representation for various structures
found in code: defn, condp, binding vectors, anonymous functions,
etc. This dispatch indents following lines of a list one more space as
appropriate for a function/argument type of list.

An example formatted with code dispatch:

    user=> (def code '(defn cl-format 
    "An implementation of a Common Lisp compatible format function"
    [stream format-in & args] (let [compiled-format (if (string? format-in) 
    (compile-format format-in) format-in) navigator (init-navigator args)] 
    (execute-format stream compiled-format navigator))))
    #'user/code
    user=> (with-pprint-dispatch *code-dispatch* (pprint code))
    (defn cl-format
      "An implementation of a Common Lisp compatible format function"
      [stream format-in & args]
      (let [compiled-format (if (string? format-in)
                              (compile-format format-in)
                              format-in)
            navigator (init-navigator args)]
        (execute-format stream compiled-format navigator)))
    nil
    user=> 

There are three ways to set the current dispatch: set it to a specific
table permanantly with set-pprint-dispatch, bind it with
with-pprint-dispatch (as shown in the example above), or use the
:dispatch keyword argument to write.

## Control variables

The operation of pretty printing is also controlled by a set of variables
that control general parameters of how the pretty printer makes
decisions. The current list is as follows:

*`*`print-pretty`*`*: Default: *true*  

Bind to true if you want write to use pretty printing. (pprint and pp automatically 
bind this to true.)

*`*`print-right-margin`*`*: Default: *72*

Pretty printing will try to avoid anything going beyond this column.

*`*`print-miser-width`*`*: Default: *40*

The column at which to enter miser style. Depending on the dispatch table, 
miser style add newlines in more places to try to keep lines short allowing for further 
levels of nesting. For example, in the code dispatch table, the pretty printer will 
insert a newline between the "if" and its condition when in miser style.

*`*`print-suppress-namespaces`*`*: Default: *false*

Don't print namespaces with symbols. This is particularly useful when 
pretty printing the results of macro expansions

*`*`print-level`*`*: Default: *nil*

As with the regular Clojure print function, this variable controls the 
depth of structure that is printed. The argument itself is level 0,
the first level of a collection is level 1, etc. When the structure
gets deeper than the specified `*`print-level`*`, a hash sign (#) is
printed.

For example:

    user=> (binding [*print-level* 2] (pprint '(a b (c d) ((e) ((f d) g)))))
    (a b (c d) (# #))
    nil
    user=> 

*`*`print-length`*`*: Default: *nil*

As with the regular Clojure print function, this variable controls the 
number of items that are printed at each layer of structure. When a
layer has too many items, elipses (...) are displayed. 

For example:

    user=> (defn foo [x] (for [i (range x) ] (range 1 (- x (dec i)))))
    #'user/foo
    user=> (binding [*print-length* 6] (pprint (foo 10)))
    ((1 2 3 4 5 6 ...)
     (1 2 3 4 5 6 ...)
     (1 2 3 4 5 6 ...)
     (1 2 3 4 5 6 ...)
     (1 2 3 4 5 6)
     (1 2 3 4 5)
     ...)
    nil
    user=>

## Custom Dispatch Functions

Using custom dispatch, you can easily create your own formatted output
for structured data. Examples included with the pretty printer show
how to use custom dispatch to translate simple Clojure structures into
nicely formatted JSON and XML.

### Basic Concepts of Pretty Printing

In order to create custom dispatch functions, you need to understand
the fundamentals of pretty printing. The clojure pretty printer is
based on the XP pretty printer algorithm (used in many Lisps including
Common Lisp) which supports sophisticated decision-making about line
breaking and indentation with reasonable performance even for very
large structures. The XP algorithm is documented in the paper,
[http://dspace.mit.edu/handle/1721.1/6504 XP. A Common Lisp Pretty
Printing System].

The Clojure implementation of XP is similar in spirit to the Common
Lisp implementation, but the details of the interface are somewhat
different. The result is that writing custom dispatch in Clojure is
more "Clojure-y."

There are three key concepts to understand when creating custom pretty
printing functions: _logical blocks_,  _conditional newlines_, and
_indentation_.

A _logical block_ marks a set of output that should be thought about
as a single unit by the pretty printer. Logical blocks can contain
other logical blocks (that is, they nest). As a simple example, when
printing list structure, every sublist will typically be a logical
block.

_Conditional newlines_ tell the pretty printer where it can insert
line breaks and how to make the decisions about when to do it. There
are four types of conditional newline:

 * Linear newlines tell the pretty printer to insert a newline in a
   place whenever the enclosing logical block won't fit on a single
   line. Linear newlines are an all-or-nothing proposition; if the
   logical block doesn't fit on a single line, *all* the linear
   newlines are emitted as actual newlines.
 * Fill newlines tell the pretty printer that it should fit as many
   chunks of the logical block as possible on this line and then emit
   a newline.
 * Mandatory newlines tell the pretty printer to emit a newline
   regardless of where it is in the output line.  
 * Miser newlines tell the pretty printer to emit a newline if the
   output column is in the miser region (as defined by the pretty
   printer variable `*`pprint-miser-width`*`). This allows you to
   define special behavior as the output gets heavily nested near the
   right margin.

_Indentation_ commands allow you to specify how wrapped lines should
be indented. Indentation can be relative to either the start column of
the current logical block or the current column position of the output. 

(This section is still incomplete...)

## Current limitations and future plans

This is an early version release of the pretty printer and there is
plenty that is yet to come.

Here are some examples:

 * Support all the types and forms in Clojure (most of the way there now).
 * Support for limiting pretty printing based on line counts.
 * Support for circular and shared substructure detection.
 * Finishing the integration with the format function (support for ~/ and tabular pretty printing).
 * Performance! (Not much thought has been made to making this go fast, but there are a bunch of pretty obvious speedups to be had.)
 * Handle Java objects intelligently

Please let me know about anything that's not working right, anything that
should work differently, or the feature you think should be at the top
of my list. 

