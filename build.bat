@ECHO off

cd src
ghc Main.hs -XExistentialQuantification
DEL *.hi
DEL *.o
