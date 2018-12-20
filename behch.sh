#!/bin/sh
for i in x.rambo y.rambo z.rambo str.rambo; do cargo build --release --quiet > /dev/null && /usr/bin/time -v ./target/release/rambo test-data/$i -w 2>&1 | grep Elapsed | perl -pe 's/.*: (.*)/\1/g'; done
