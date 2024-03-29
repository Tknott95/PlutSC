#!/bin/bash

: '
 $1 is "loc of file" 
 EX: datum-redeemer.plutus

 $2 is .addr file out
 EX: datum-redeemer.addr

 EXAMPLE OF SCRIPT RUNNING:
  ./scr_addr.sh ../compiled-scripts/customDtm137-v2.plutus customDtm137.addr


 replace $MAGIC w/ --mainnet for mainnet
'

cardano-cli address build --payment-script-file $1  $MAGIC --out-file ../crypt/addrs/$2
