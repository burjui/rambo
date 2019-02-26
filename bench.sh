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
shift
cargo build $MODE
for i in x.rambo y.rambo z.rambo str.rambo; do
	echo -n "$i: "
	/usr/bin/time -f "%E" ./target/$TARGET/rambo test-data/$i $@ > /dev/null | perl -pe 's/.\w+: (.*)/\1/g'
done
