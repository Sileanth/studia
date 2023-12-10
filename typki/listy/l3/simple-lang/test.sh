RED='\033[0;31m'
NC='\033[0m'

total_tests=0
passed_tests=0

for file in tests/ok*.sl
do
	echo -n "$file: "
	total_tests=$(($total_tests + 1))
	if ./simple.native $file
	then
		passed_tests=$(($passed_tests + 1))
	else
		echo -e "${RED}failed!${NC}"
	fi
done

echo "passed: ${passed_tests}/${total_tests}"
