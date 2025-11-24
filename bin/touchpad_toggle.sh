#!/bin/bash

# Use the command
# xinput list
# to identify the touchpad device name
# touchpadDeviceName="ELAN067A:00 04F3:3197 Touchpad"
touchpadDeviceName="SynPS/2 Synaptics TouchPad"
if xinput list | grep -q "∼ $touchpadDeviceName"; then
    echo "Enabling $touchpadDeviceName"
    xinput enable "$touchpadDeviceName"
    echo "Done"
else
    echo "Disabling $touchpadDeviceName"
    xinput disable "$touchpadDeviceName"
    echo "Done"
fi
