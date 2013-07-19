Lampas
======
Lampas is my first Lisp; currently its unique features include lambda shorthand, vector notation, and Lisp-style macros. Macros allow for arbitrary features to be made as functions of S-Expressions which return a new S-Expression to be evaluated.

```scheme
[1 2 3]
" => (1 2 3)
"

({|x| (+ 1 x)} 5)
" => 6
"

(define-syntax 
  let 
  {|expr| 
    `((lambda 
       (,(cadr expr)) 
       ,(cadddr expr)) 
       ,(caddr expr))})
(let a 5 (cons a 2))
" => (5 2)
"
```

Syntax
------
See examples of syntax in `src/test.lampas`.

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
4
```

Include the library functions with the following.

```sh
(load "Prelude.lampas")
```

Credits
-------
This is very much thanks to the tutorial [Write Yourself a Scheme in 48 Hours](http://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours) by *Jonathan Tang*. It makes quite clear how to implement a language in Haskell, developing a REPL early on and building it up to a full-fledged Scheme.