cd src
ghc Main.hs -XExistentialQuantification
rm *.hi
rm *.o
./Main test.lampas