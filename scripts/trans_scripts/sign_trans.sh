#!/bin/sh

# ./sign_trans.sh ../transactions/tx00/tx.build  ../keys/alwayssucceeds/alwayssucceeds_payment.skey  ../transactions/tx00/tx.signed

# ./sign_trans.sh \
# ../transactions/tx00/tx.build \
#  ../keys/alwayssucceeds/alwayssucceeds_payment.skey \
#  ../transactions/tx00/tx.signed

built_trans=$1

# ../keys/alwayssucceeds/alwayssucceeds_payment.skey
signing_key_file=$2

# ../transactions/tx00/tx.signed
signed_out_file=$3

echo -e "\e[0;1;30m  BUILT TRANS: $built_trans \e[0m"
echo -e "\e[0;0;33m  SIGNING KEY: $signing_key_file \e[0m"
echo -e "\e[0;0;36m  SINGED TRANS AT: $signed_out_file \e[0m"

cardano-cli transaction sign \
--tx-body-file $built_trans \
$MAGIC \
--signing-key-file $signing_key_file \
--out-file $signed_out_file
