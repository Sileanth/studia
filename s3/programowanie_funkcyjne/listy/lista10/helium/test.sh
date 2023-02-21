#!/bin/bash
if ! [ -n "$1" ] || ! [ -n "$2" ]; then
	echo "USAGE: ./test.sh HELIUM_BIN TEST_SUITE"
	exit 1
fi

helium=$1
flags=""

RED='\033[0;31m'
NC='\033[0m'

total_tests=0
passed_tests=0

function start_test {
	total_tests=$(($total_tests + 1))
}

function pass_test {
	passed_tests=$(($passed_tests + 1))
}

function run_test {
	start_test
	local tmp_stdout=$(mktemp)
	local tmp_stderr=$(mktemp)
	echo "$1"
	./$1 >$tmp_stdout 2>$tmp_stderr
	local status=$?
	if $2 $status $tmp_stdout $tmp_stderr; then
		pass_test
	else
		echo -e "${RED}$3${NC}"
		echo "stdout: >>>>>"
		cat $tmp_stdout
		echo "<<<<<"
		echo "stderr: >>>>>"
		cat $tmp_stderr
		echo "<<<<<"
		echo "exit_code: $status"
	fi
	rm $tmp_stdout
	rm $tmp_stderr
}

function check_simple_test {
	[ $1 -eq 0 ]
}

function simple_test {
	local cmd="$helium $flags $1"
	local err="Command ${cmd} failed."
	run_test "$cmd" check_simple_test "$err"
}

function check_exit_code_test {
	[ $2 -eq $1 ]
}

function exit_code_test {
	local cmd="$helium $flags $2"
	local err="Command ${cmd} does not exited with code $1."
	run_test "$cmd" "check_exit_code_test $1" "$err"
}

function run_with_flags {
	flags=$2
	$1
}

source "$2"

echo "Passed: ${passed_tests}/${total_tests}"
