#!/bin/bash
set -e
clear
rm -f tests/PrackTests
echo -en "Building tests...\n\n"
lazbuild -q -q tests/PrackTests.lpi
echo -en "\nRunning tests...\n\n"
./tests/PrackTests -a --sparse --format=plain
