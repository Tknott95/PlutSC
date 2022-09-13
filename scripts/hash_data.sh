#!/bin/bash

: '
 $1 is "loc of file" 

 takes in a json file and returns a hash

 strings do not work, you have to encode into Word8 then store ints into an array
 OR as a hex encoded value

 REF TO MAKE JSON AS OBJECT PROPER: https://forum.cardano.org/t/cardano-cli-hash-script-data/80770/4 */


 EXAMPLE:
  ./hash_data.sh test_cmplx.json
'

cardano-cli transaction hash-script-data --script-data-file $1
