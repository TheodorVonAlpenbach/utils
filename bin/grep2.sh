args=("$@")
files=("${args[@]:2}")

function printUsage {
    echo "Usage: $grep2 PATTERN1 PATTERN2 [FILE]..."
    echo "Search for FILES that matches both PATTERN1 and PATTERN2."
    echo "TODO: highlight patterns in output."
}

if [ "$#" -lt 3 ]; then
   printUsage
   exit 0
fi

res1=`grep -nH -m 1 $1 ${files[@]}`
res2=`grep -nH -m 1 $2 ${files[@]}`

if [ -n "$res1" ] && [ -n "$res2" ]; then
   echo $res1
   # echo $res2
   ## replace filename with blanks for the second line
   echo "$(echo $res2 | cut -d : -f 1 | sed -e 's/./ /g'):$(echo $res2 | cut -d : -f 2-)"
fi
