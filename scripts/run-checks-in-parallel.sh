#!/usr/bin/env bash

totalTasks=0
function startTask {
    npm run "$1" > "$2" 2> "$3" &
    totalTasks=$((totalTasks + 1))
}

COMPILE_OUT=$(mktemp)
COMPILE_ERR=$(mktemp)
TEST_OUT=$(mktemp)
TEST_ERR=$(mktemp)
LINT_OUT=$(mktemp)
LINT_ERR=$(mktemp)

startTask "compile" $COMPILE_OUT $COMPILE_ERR
startTask "test" $TEST_OUT $TEST_ERR
startTask "lint" $LINT_OUT $LINT_ERR

ret=0
tasksLeft=$totalTasks
echo "running ${tasksLeft} tasks"
while [[ $tasksLeft -gt 0 ]]; do
    wait -n
    code="$?"
    ret=$((ret + code))
    tasksLeft=$((tasksLeft - 1))
done

cat $COMPILE_OUT $TEST_OUT $LINT_OUT

exit $ret
