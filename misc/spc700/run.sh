#!/bin/sh
cat spc700.txt | ./spc700.awk > tmp.txt
cargo run $(pwd)/tmp.txt > ../../src/apu/decode.rs
rm -f tmp.txt
