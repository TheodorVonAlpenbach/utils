#!/bin/bash

bindir=/home/mats/git/utils/bin

gnome-terminal --window --maximize -- bash -c "${bindir}/launcher1.sh"

# The class argument is for recognizing this terminal window in other scripts
gnome-terminal --window --geometry 120x42+700+250 --class=LaptopTerminal -- bash -c "${bindir}/launcher2.sh"
