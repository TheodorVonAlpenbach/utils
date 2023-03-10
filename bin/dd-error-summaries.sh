target=${1:-"Error occured at"}
ofile=${2:-errors.txt}

echo "Extracting error summaries matching '$target'"

for pod in `kubectl -n prod-blue get pods | grep portal-api | cut -d \  -f 1`
do
    echo "Extracting from pod $pod..."
    kubectl -n prod-blue logs "$pod" | grep "$target" >> "$ofile"
done

wc -l "$ofile"

echo "Wrote `wc -l $ofile | cut -d\  -f 1` error summaries to $ofile"

