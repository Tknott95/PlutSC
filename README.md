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

### The syntax for typed validators is a tad diff after V2 as Typed137.hs shows 
__________________________________

- How do I even run this code?
  * cabal run
- Where is the uncompiled code?
  * You can find this inside app/smart-contracts/
- How do I write a new script without an executable and cabal run <that-module>?
  * You use the write...ScriptV2/V1 function inside main. If it is requested, or I find a need for binaries for each, I can do it within seconds by doing what above states.

- Where do scripts compile to?
  * you will find them in compiled-scripts/

- What is the script folder?
  * Little bash scripts for dev ops

- What is the crypt folder?
  * This holds addresses and keys

__________________________________


##### @TODO
* Slowly add more scripts
* ...

__________________________________


``` IMPORTANT NOTE ABOUT V2

ẁrapMintingPolicy and wrapValidator have changed to mkUntypedMintingPolicyt and mkUntypedValidator

via : https://github.com/input-output-hk/plutus-apps/issues/190

How I know will wrap validators (post Plutus 1-3)
 -> https://plutus-apps.readthedocs.io/en/stable/plutus/tutorials/basic-validators.html

```
__________________________________


##### Smart Contracts For Ref
- AlwaysSucceeds
  * V1 and V2 of no datum or redeemer to always succeed

- Hello Datum
  * Takes in a bytestring/hex saying hello

- Untyped137 
  * Requires a datum of 137

- Typed 137 -  (v2 changed from wrapValidator)
  * Requires a datum of 137
  * The syntax is diff from pre V2 plutus typed validators

- CustomRdmr137
  * wrapValidator is now mkUntypedValidator

__________________________________

##### Scripts For Ref
- scr_addr.sh $1
  * creates a script address for a compiled .plutus script
  * $1 is your compiled .plutus script

- query_all_utxo.sh $1
  * return all utxos at that addr 
  * $1 is your address you are using to query your utxos

__________________________________

##### @TODO
- finish these docs with what has been done
  * bash script documentation for one
- start bringing in v2 mkMintingPolicy and paramterized scripts
- throw in some fun scripts with little innovative twists
