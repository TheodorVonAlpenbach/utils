#!/bin/bash

environment=${1:-prod}
echo "Current environment is $environment"

# Identify a running admin-api pod
podname=$(kubectl -n $environment get pods | grep 'admin-api.*Running' | head -1 | cut -d \  -f 1)
if [ -z "$podname" ]; then
    echo "Could not find any running admin-api pods in $environment" 
    exit
fi

echo "Use running pod $podname" 

echo "Copy Sowiso export files ..."
(cd ~/data/sowiso && tar cz exercises_*.xml) | \
kubectl -n "$environment" exec -i "$podname" -- \
    sh -c 'mkdir -p /tmp/sowiso-data && tar xz -C /tmp/sowiso-data'
echo "Finished copying."

# In one terminal, create and enter admin-api pod sh container, and run script
echo "Enter $podname and run import script" 
kubectl -n $environment exec $podname -it -- node dist/admin-api-import-sowiso.js /tmp/sowiso-data mats.bergstrom@cappelendamm.no
