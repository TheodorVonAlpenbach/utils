#!/bin/bash

# Use the command
# xinput list
# to identify the optical mouse device name
# opticalMouseDeviceName="PixArt Dell MS116 USB Optical Mouse"
opticalMouseDeviceName="Optical Mouse"
touchpadDeviceName="ELAN067A:00 04F3:3197 Touchpad"
LOGFILE="/var/log/touchpad_autotoggle.log"

if xinput list | grep -q "$opticalMouseDeviceName"; then
    echo "$(date): Disabling touchpad '$touchpadDeviceName'" >> $LOGFILE
    xinput disable "$touchpadDeviceName"
    echo "$(date): Done" >> $LOGFILE
else
    echo "$(date): Enabling touchpad '$touchpadDeviceName'" >> $LOGFILE
    xinput enable "$touchpadDeviceName"
    echo "$(date): Done" >> $LOGFILE
fi
