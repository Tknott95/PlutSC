#!/bin/sh

tx_dir_name=$1

tx_out_loc=./transactions/$tx_dir_name

./scripts/submit_trans.sh $tx_out_loc"/tx.signed"
