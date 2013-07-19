Lampas
======
Lampas is my first Lisp; currently its unique features include lambda shorthand, vector notation, continuations, and Lisp-style macros. Macros allow for codes to be specified that will manipulate any S-Expressions which they begin. That is, if `a` were a macro, any S-Expression led with `a` would be passed to the macro definition prior to evaluation; an example is below. Continuations were defined purely with macros. The general workflow of a continuation is to initiate a continuation statement with `begincc`, and then in the context of the statement, where the value of interest is present, to call `call/cc` with a lambda taking a continuation as a parameter. That continuation can then be set to a variable for later calling, and an initial value should be returned. Upon calling the continuation, values should be quoted (for now).

```scheme
[1 2 3]
" => (1 2 3)
"

({|x| (+ 1 x)} 5)
" => 6
"

(defmacro 
  (let name val body) 
  `((lambda (,name) ,body) ,val))
(let a 5 (cons a 2))
" => (5 2)
"

(define print 5)    
(begincc (write (call/cc {|cc| (set! print cc) 'initial})))
(print ''second)
"  => initial
"" => second
"
```

Syntax
------
See examples of syntax in `src/test.lampas`. 

For macros, after beginning with a `define-rewriter` approach, wherein macros received each S-Expression in which they were embedded as arguments, I instead opted for `defmacro`. `defmacro` parses components as arguments, but the full power of `define-rewriter` could be easily rebuilt. My implementation of macros was pretty straight-forward, each `defmacro` defined a function in the usual environment with a distinct name. From there, each S-Expression is checked for a corresponding macro name. If one is found, the tail of the S-Expression is passed as argument to the defined function. This method keeps macros hygienic. This implementation may be improper, or perhaps it has too much overhead, but it was very easily implemented.

Compilation
-----------
Compile the source using GHC and the Existential flag.

```sh
$ ghc Main.hs -XExistentialQuantification
```

Or, if on a Unix machine, run the build script which will compile, test, and clean-up. Support for Windows will be added soon.

```sh
$ ./build.sh
```

Usage
-----
Then run the interpreter either with a program as a parameter or individually to fire up a REPL.

```sh
$ ./lampas
Lampas >>
```

```sh
$ ./lampas test.lampas
```

Include the library functions with the following.

```sh
(load "Prelude.lampas")
```

Todo
-----
- `@,` unquote-splicing
- `case` statements
- `currying`
- `continuations` - an early implementation is done (`env`-free)
- `numerical tower`

References
----------
- This is very much thanks to the tutorial [Write Yourself a Scheme in 48 Hours](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours) by *Jonathan Tang*. It makes quite clear how to implement a language in Haskell, developing a REPL early on and building it up to a full-fledged Scheme.
- Fogus' [Caerbannog](https://github.com/fogus/caerbannog).