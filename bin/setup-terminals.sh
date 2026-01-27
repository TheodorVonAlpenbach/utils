#!/bin/bash

bindir=/home/mats/git/utils/bin

gnome-terminal --window --maximize -- bash -c "${bindir}/launcher1.sh"
gnome-terminal --window --geometry 100x45+700+10 -- bash -c "${bindir}/launcher2.sh"
