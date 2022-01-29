#!/bin/bash

cargo b --release
for i in tests/*.dodo; do 
    echo "Running" $i
    target/release/dodo $i && 
    nasm -f elf64 output.asm -o /tmp/output.o -g &&
    gcc -o /tmp/output /tmp/output.o -nostartfiles -no-pie -g &&
    /tmp/output > /dev/null
    if [ $? -ne 0 ] 
    then
        exit 
    fi
done
