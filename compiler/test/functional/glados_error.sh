#!/bin/bash

EXEC="./glados"
TEST_FILE="./compiler/test/files_tests/language_error"

$EXEC -f $TEST_FILE > /dev/null 2>&1
if [[ $? -ne 0 ]]; then
    exit 0
else
    echo "Error: glados success to launch with file $TEST_FILE"
    exit 84
fi