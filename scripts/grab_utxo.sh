#!/bin/sh

addr_using=$1

# GRABBING 3rd UTXO RIGHT NOW - NEED TO QUERY AND TAKE LARGEST AMOUNT AVAIL UTXO
# | sed -n 3p is where I take 3rd line 1p is first unspent utxo at txID 0

# @TODO
# query all and take only the top amount
# might do this in haskell and just run on the binary
# or both

cardano-cli query utxo --address $addr_using $MAGIC | grep " 0 " | sed -n 1p | egrep  -o '[a-z0-9]+' | head -1
