#!/usr/bin/env python3

import json, sys, re, time, datetime

def timestruct2isoDate(timestruct):
    return time.strftime("%Y-%m-%dT%H:%M:%S%Z", timestruct)

def unix2isoDate(msDate):
    return timestruct2isoDate(time.localtime(msDate))

if not sys.stdin.isatty():
    # first, try read from stdin 
    myJson = sys.stdin.read()
else:
    # read command arguments
    nargs = len(sys.argv)
    # print(nargs)
    if (nargs > 1):
        myJson = sys.argv[1]

# print(sys.argv)
print(myJson)
msunixtimeString = sys.argv[1]
print(msunixtimeString)
msunixtimeLines = msunixtimeString.split('\n')
# iso = unix2isoDate(msunixtime/1000)
print(msunixtimeString)
print(len(msunixtimeLines))
# print(iso)
