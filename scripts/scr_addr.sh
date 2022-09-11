#!/bin/bash

```
 $1 is "loc of file" 
 EX: datum-redeemer.plutus

 $2 is .addr file out
 EX: datum-redeemer.addr
```

cardano-cli address build --payment-script-file $1  --mainnet --out-file $2
