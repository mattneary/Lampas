Lampas
======
Lampas is my first attempt at a Scheme implementation of my own. Right now it is a very minimal and traditional implementation, but I plan on giving it a unique syntax and eventually some power features.

The emphasis of Lampas has not yet been decided; however, I intend to give it a specific focus.

Usage
-----
Compile the source using GHC and the Existential flag.

```sh
ghc lampas.hs -XExistentialQuantification
```

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