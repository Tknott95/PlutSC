#!/bin/sh

: '
 $1 is the addr

 EXAMPLE:
   ./query_all_utxo.sh ../crypt/addrs/customDtm137.addr 
'

cardano-cli query utxo --address $(cat $1) $MAGIC
