#!/bin/bash

EXEC="./glados"
TEST_FILE="./compiler/test/files_tests/language_test"

$EXEC -f $TEST_FILE > /dev/null 2>&1
if [[ $? -ne 0 ]]; then
    echo "Error: glados failed to launch with file $TEST_FILE"
    exit 84
else
    exit 0
fi