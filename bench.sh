#!/bin/sh
set -e

SRC_DIR="$1"

case "$2" in
	release)
		MODE="--release"
		TARGET="release"
		;;
	debug)
		MODE=""
		TARGET="debug"
		;;
	*)
		echo "Usage: $0 <SOURCE DIR> <release|debug> [OPTIONS]..."
		exit 1
		;;
esac

shift
shift

cargo build $MODE
for i in x.rambo y.rambo z.rambo; do #str.rambo; do
	./target/$TARGET/rambo "$SRC_DIR/$i" $@
done
