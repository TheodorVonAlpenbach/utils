after=${1:-0}
summaryLength=$[$after + 1]
target=${2:-"Error occured at"}
ofile=${3:-errors.txt}
env=${4:-prod-blue}

echo "Extracting $summaryLength line error summaries matching '$target'"

for pod in `kubectl -n prod-blue get pods | grep portal-api | cut -d \  -f 1`
do
    echo "Extracting from pod $pod..."
    kubectl -n "$env" logs "$pod" | grep -A "$after" "$target" >> "$ofile"
    echo "--" >> "$ofile" # same separator line as the one grep -A uses between hits 
done

wc -l "$ofile"

numLines=`wc -l $ofile | cut -d\  -f 1`
numSummaries=$[$numLines / $summaryLength]
echo "Wrote $numSummaries error summaries to $ofile"
