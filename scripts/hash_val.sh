#!/bin/bash

: '
 $1 is an integer 

 takes in an integer and returns a hash
'

cardano-cli transaction hash-script-data --script-data-value $1
