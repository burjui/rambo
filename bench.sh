#!/bin/sh
set -e

SRC_DIR="$1"

usage() {
	echo "Usage: $0 <SOURCE DIR> <release|debug> [OPTIONS]..."
	exit 1
}

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
		usage
		;;
esac

TOOLCHAIN="$3"
case "$3" in
	stable | beta | nightly)
		;;
	*)
		usage
		;;
esac

shift
shift
shift

cargo "+$TOOLCHAIN" build $MODE
for i in x.rambo y.rambo z.rambo; do
	./target/$TARGET/rambo "$SRC_DIR/$i" $@
done
