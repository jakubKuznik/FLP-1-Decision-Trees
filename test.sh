#!/bin/bash

# ./flp-fun -1 tests/trees/tree_1 tests/new_data/new_data_1 | diff - tests/classifications/classification_1

# Loop over the test cases
for ((i=1; i<=10; i++)); do
    echo "Running test case $i..."
    
    # Run the program and compare the output with the expected classification
    ./flp-fun -1 "tests/trees/tree_$i" "tests/new_data/new_data_$i" | diff - "tests/classifications/classification_$i"
    
    # Check the exit status of the diff command
    if [ $? -eq 0 ]; then
        echo "Test case $i passed!"
    else
        echo "Test case $i failed!"
    fi
    
    echo
done

