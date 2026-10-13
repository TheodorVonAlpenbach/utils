#!/bin/bash

bindir=/home/mats/git/utils/bin

gnome-terminal --window --maximize -- bash -c "${bindir}/launcher1.sh"

# Keep a unique initial title so the GNOME Shell extension can recognize it on Wayland.
gnome-terminal --window --geometry 120x42+700+250 --title=LaptopTerminal -- bash -c "${bindir}/launcher2.sh"
