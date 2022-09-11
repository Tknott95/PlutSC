# Plutz

``` NOTES FOR WHEN OPEN SOURCED
  Instead of an executable for each
  script I am going to just write them in main.
  To add an executable, edit Plutz.cabal and add in the
  proper module wanted. 
  
  Using an executable for each was my original intention yet I find
  this to be the better proc for right now. 
  Adding a module as an executable takes 
  only a few seconds inside Plutz.cabal.
```

### The yntax for typed validators is a tad diff after V2 as Typed137.hs shows 
__________________________________

- How do I even run this code?
  * cabal run
- Where is the uncompiled code?
  * You can find this inside app/smart-contracts/
- How do I write a new script without an executable and cabal run <that-module>?
  * You use the write...ScriptV2/V1 function inside main. If it is requested, or I find a need for binaries for each, I can do it within seconds by doing what above states.

- Where do scripts compile to?
  * you will find them in compiled-scripts/

__________________________________


##### @TODO
* Slowly add more scripts
* ...

__________________________________


##### Smart Contracts For Ref
- AlwaysSucceeds
  * V1 and V2 of no datum or redeemer to always succeed

- Hello Datum
  * Takes in a bytestring/hex saying hello

- Untyped137 
  * Requires a datum of 137

- Typed 137 - NEEDS TO BE FIXED (v2 changed the wrapValidator and it isn't importing from any loc)
  * Requires a datum of 137
  * The syntax is diff from pre V2 plutus typed validators
