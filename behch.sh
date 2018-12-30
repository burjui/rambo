#!/bin/sh
set -e

case "$1" in
	release)
		MODE="--release"
		TARGET="release"
		;;
	debug)
		MODE=""
		TARGET="debug"
		;;
	*)
		echo "Usage: $0 <release|debug>"
		exit 1
		;;
esac
cargo build $MODE
for i in x.rambo y.rambo z.rambo str.rambo; do /usr/bin/time -v ./target/$TARGET/rambo test-data/$i -w 2>&1 | grep Elapsed | perl -pe 's/.*: (.*)/\1/g'; done
