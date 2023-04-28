echo " # 1st argument is the name of testing program."
echo " # If 2nd arg is YES then sanitizers wil be used."

if [ ! $1 ]; then
	echo "Expected .ml file as first argument, aborting!"
	exit
fi


echo "Compiling without sanitizers."
ocamlopt -o prog $1

if [ ! -f prog ]; then
	echo "Probably compilation fail, aborting!"
	exit
fi

groups_cnt=0
declare -a groups
declare -a countt
declare -a passed

echo "* Test groups: *"
for i in $(ls -1 | sort -r)
do
	if [ -d $i ]; then
		((groups_cnt++))
		groups[groups_cnt]=$i
		echo " - $i"
	fi
done

for i in $(seq 1 $groups_cnt)
do
	gr=${groups[i]}
	countt[i]=$(ls $gr/in -1 | wc -l)
	passed[i]=0
	echo "Found ${countt[i]} in $gr"
	for j in $(seq 1 ${countt[i]})
	do
		echo "Test $gr/in/$j.in"
		/usr/bin/time -f "  Time: %E" ./prog < $gr/in/$j.in > TT
		if cmp TT $gr/out/$j.out; then
			echo "  OK"
			((passed[i]++))
		else
			echo "  WA"
		fi
	done
done

for i in $(seq 1 $groups_cnt)
do
	gr=${groups[i]}
	ct=${countt[i]}
	pa=${passed[i]}
	echo "In group: $gr $pa tests passed out of $ct ($pa/$ct)."
done
rm prog
rm TT
