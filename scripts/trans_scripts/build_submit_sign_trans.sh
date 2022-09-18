#!/bin/sh

sc_addr=$(cat ./keys/alwayssucceeds/alwayssucceeds_payment.addr)
utxo_using=$(./scripts/grab_utxo.sh $sc_addr)"#0"

sc_skey=./keys/alwayssucceeds/alwayssucceeds_payment.skey

# @TODO - make this dynamic - this is X4Wallet137
payout_addr=addr_test1qpdvvdalsqscc3899gk67zdx7lkrlqlnwm3xzzk88jc65c50a06ns46p0wjxe6xqkvnrs4f79wjp6tz07wrl2k2nctyqqkhtak
tx_out_loc=./transactions/tx00

echo "SC_ADDR USING: " $sc_addr
echo "UTXO USING:  " $utxo_using

./scripts/build_trans.sh \
  $sc_addr\
  $utxo_using \
  $payout_addr \
  $tx_out_loc"/tx.build"


echo "TX BUILT:  $tx_out_loc/tx.build"


./scripts/sign_trans.sh \
  $tx_out_loc"/tx.build" \
  $sc_skey \
  $tx_out_loc"/tx.signed"


echo "TX SIGNED:  $tx_out_loc/tx.signed"


./scripts/submit_trans.sh $tx_out_loc"/tx.signed"
