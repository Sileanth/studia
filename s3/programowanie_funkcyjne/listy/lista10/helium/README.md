# README #

## Helium: programming with abstract algebraic effects

Helium is a *very* experimental programming language that boasts
advanced algebraic effects, sophisticated polymorphism and abstraction
for types and effects through a simple module system in the style of
ML. This package contains the language itself, as well as a
rudimentary standard library and some larger examples. The source code
can be found in the `src` subdirectory, and the library and examples
respectively in `lib` and `examples` subdirectories. There is also
some basic support for editing Helium code in vim and emacs, which
should at the least provide syntax highlighting.

### Requirements

Helium is written in pure OCaml with no external libraries. In order
to build Helium you need running installation of OCaml 4.05.0 or higher
and `ocamlbuild`.

### Installation

Simply type `make` in the main directory. This will build an
interpreter and put the binary in the `bin` subdirectory. Then type
`bin/helium -help` to get some information about what are the
interpreters capabilities. By default Helium searches its own standard
library in `lib` subdirectory of current directory, but you can change
this behavior by setting `HELIUM_LIB` environmental variable.

### Usage

In the batch mode, use `bin/helium filename.he` to typecheck the
program and execute it via the abstract machine implementation. A
number of switches is provided, which can instead print various
intermediate representation. Particularly useful is the `-core` switch
that instead of running the program prints its representation in the
Core language that (save for algebraic data types) matches the
calculus presented in "Abstracting Algebraic Effects". The interpreter
can also be used in as a REPL, allowing the user to enter simple
programs interactively and observe the results.

### Examples

We provide several simple tests with the implementation, located
within the `test` subdirectory. However, these mostly test a single
feature of the language each, and are not terribly
informative. Instead, we provide three larger examples in the
subdirectory of the same name. These are, in the order of increasing
complexity, a regular expression matcher, a union-find based
unification algorithm, and a parser generator library. We suggest
carefully looking at the first two at least, since they can give an
insight both into the syntax of the language, and into the idioms it
supports.

### More resources

* [Helium wiki](https://bitbucket.org/pl-uwr/helium/wiki/Home)
* [Abstracting Algebraic Effects paper](https://bitbucket.org/pl-uwr/helium/downloads/popl19.pdf)
