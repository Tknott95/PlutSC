#!/bin/bash

: '
 $1 is "loc of file" 

 takes in a json file and returns a hash

 REF TO MAKE JSON AS OBJECT PROPER: https://forum.cardano.org/t/cardano-cli-hash-script-data/80770/4 */

'

cardano-cli transaction hash-script-data --script-data-file $1
