!/bin/bash

adadir=/home/mats/ada

# ; sudo openconnect --no-external-auth --csd-wrapper=/usr/libexec/openconnect/csd-wrapper.sh --user=mats.bergstrom@cappelendamm.no --useragent AnyConnect vpn.intility.no & echo 'Job finished'
# does not work
gnome-terminal --tab -- bash -c "cd $adadir; exec bash"
gnome-terminal --tab -- bash -c "cd $adadir/java/pu-message-consumer; ../gradlew bootRun & echo 'Job finished.'; exec bash"
gnome-terminal --tab -- bash -c "cd $adadir/java/portal-api; ../gradlew bootRun & echo 'Job finished.'; exec bash"
gnome-terminal --tab -- bash -c "cd $adadir; top; exec bash"

