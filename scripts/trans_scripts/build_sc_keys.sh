#!/bin/sh

# will use $1


mkdir -p keys/$1
cd keys/$1

cardano-cli address key-gen \
--verification-key-file $1_payment.vkey \
--signing-key-file $1_payment.skey

cardano-cli stake-address key-gen \
--verification-key-file $1_stake.vkey \
--signing-key-file stake.skey

cardano-cli address build \
--payment-verification-key-file $1_payment.vkey \
--stake-verification-key-file $1_stake.vkey \
--out-file $1_payment.addr \
$MAGIC

cd ../../

echo "SMART CONTRACT KEYS CREATED FOR $1"
