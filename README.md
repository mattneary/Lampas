Lampas
======
Lampas is my first attempt at a Scheme implementation of my own. Currently its unique features include lambda shorthand and vector notation.

```ruby
[1 2 3]
" => '(1 2 3)
"
({|x| (+ 1 x)} 5)
" => 6
"
```

Syntax
------
See examples of syntax in `examples.lampas`.

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