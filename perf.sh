#!/bin/sh
set -eu

cargo build --release
perf record --call-graph=dwarf --freq 10000  ./target/release/rambo -w test-data/z.rambo
perf script | inferno-collapse-perf | inferno-flamegraph > flamegraph.svg
firefox flamegraph.svg
perf report
