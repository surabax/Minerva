# Minerva
An implementation of R5RS Scheme in Common Lisp and C, inspired by the paper ["An Incremental Approach to Compiler Construction", Abdulaziz Ghuloum, Proceedings of the 2006 Scheme and Functional Programming Workshop](http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf).

Minerva compiles Scheme expressions into x86 (32-bit) binaries along with a currently very simple [runtime](https://github.com/surabax/Minerva/blob/master/runtime.c).

The implementation is far from being complete right now, you can see the supported subset of the language in [the tests suite](https://github.com/surabax/Minerva/blob/master/tests.lisp).

Minerva wasn't tested on operating systems other than Windows and Linux, and on Common Lisp implementations other than SBCL yet.

## Install
The runtime is written in C, and the compiler depends on GNU as and ld to produce executables from emitted assembly code, so you will need to **install either MinGW on Windows or gcc-multilib on Linux.**

If you are using Quicklisp, clone the repository to your local-projects directory. Otherwise you can clone it wherever you want and load the system manually:
```
(asdf:load-asd #p"/path/to/Minerva")
(asdf:load-system :minerva)
```

If you want to use the test suite, set your preferred directory for intermediate files in [tests.lisp](https://github.com/surabax/Minerva/blob/master/tests.lisp) by changing the `*intermediate-directory*` definition.

## Usage
To run the test suite:
```
(minerva:run-all-tests)
```

To compile an expression:
```
(minerva:compile-scheme-expr <your-expression-in-quote> <path-to-the-new-executable>)
```
For example:
```
(minerva:compile-scheme-expr '(+ 1 1) "/home/foo/bar")
```
produces an executable called `bar` in the directory `/home/foo/` that prints `2`.

## License
Copyright (c) 2018-2020 Yaroslav Khnygin

Licensed under [the MIT License](https://github.com/surabax/Minerva/blob/master/LICENSE).
