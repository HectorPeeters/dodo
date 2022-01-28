#!/bin/bash

cargo b
for i in tests/*.dodo; do echo "Running" $i; target/debug/dodo $i && nasm -f elf64 output.asm -o /tmp/output.o -g; done
