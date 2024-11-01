#!/bin/bash

EXEC="./glados"

$EXEC --help > /dev/null 2>&1
if [[ $? -ne 0 ]]; then
    echo " Error: glados cannot launch"
    exit 84
else
    exit 0
fi