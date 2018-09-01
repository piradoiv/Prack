#!/bin/bash
set -e
lazbuild tests/PrackTests.lpi
echo -en "\nRunning tests\n\n"
./tests/PrackTests -a -p --format=plain
