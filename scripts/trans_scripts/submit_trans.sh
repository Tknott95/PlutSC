#!/bin/bash

# ./submit_trans.sh ../transactions/tx00/tx.signed

signed_trans=$1

echo -e "\e[0;0;36m  SUBMITTING TRANS AT: $signed_trans \e[0m"


cardano-cli transaction submit --tx-file $1 $MAGIC

