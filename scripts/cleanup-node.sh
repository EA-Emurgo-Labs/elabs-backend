#!/bin/sh

echo "Cleaning private testnet setup"
rm -rf ./cardano-private-testnet-setup/ ./kupo ./logs-privnet

echo "send kill signal for each running node PID"
for PID in $(ps -ef | pgrep 'cardano-node' | grep -v grep |  awk '{print $2}');do kill -TERM "$PID" 2> /dev/null; done