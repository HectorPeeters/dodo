#!/bin/bash

cargo b
for i in tests/*.dodo; do echo "Running" $i; target/debug/dodo $i; done
