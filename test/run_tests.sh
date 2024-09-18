#!/bin/bash

TEST_DIR="./test/functional"
EXEC="./glados"

passed=0
failed=0
crashed=0

run_test() {
    local test_file=$1
    echo "Test execution : $test_file"

    bash "$test_file"
    local exit_code=$?

    if [[ $exit_code -eq 0 ]]; then
        echo "Test $test_file : Passed"
        passed=$((passed + 1))
    elif [[ $exit_code -eq 84 ]]; then
        echo "Test $test_file : Failed"
        failed=$((failed + 1))
    else
        echo "Test $test_file : Crashed (exit code: $exit_code)"
        crashed=$((crashed + 1))
    fi
}

for test_file in "$TEST_DIR"/*.sh; do
    if [[ -f "$test_file" ]]; then
        run_test "$test_file"
    else
        echo "There is no test file in $TEST_DIR"
    fi
done

echo
echo "=== Test Summary ==="
echo "Passed : $passed"
echo "Failed : $failed"
echo "Crashed : $crashed"