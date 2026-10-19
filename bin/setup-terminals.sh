#!/bin/bash

bindir=/home/mats/git/utils/bin

gnome-terminal --window --maximize -- bash -c "${bindir}/launcher1.sh"

# launcher2.sh registers and moves this window to the laptop before opening tabs.
gnome-terminal --window --geometry 120x42+700+250 --title=LaptopTerminal -- bash -c "${bindir}/launcher2.sh"
