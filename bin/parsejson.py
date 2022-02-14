#!/usr/bin/env python3

import json, sys, re, time, datetime

myJson = '{ "error":"Could\'t read json" }'

if not sys.stdin.isatty():
    myJson = sys.stdin.read()
else:
    nargs = len(sys.argv)
    # print(nargs)
    if (nargs > 1):
        myJson = sys.argv[1]

# print(myJson)
y = json.loads(myJson)

data = y["data"]
# print(len(data))

for datum in data:
    attributes = datum["attributes"]
    timestampString = attributes["timestamp"]
    timestamp = time.strptime(timestampString[0:-5]+"UTC", "%Y-%m-%dT%H:%M:%S%Z")
    timestampUnixtime = time.mktime(timestamp) 
    # print(timestampUnixtime)
    isoTimestamp = time.strftime("%Y-%m-%dT%H:%M:%S", timestamp)
    # print(isoTimestamp)
    # print(message) 
    message = attributes["message"]
    x = re.search("created\":[^,]*", message) 
    createdUnixtime = int(x.group()[9:-3])
    created = time.localtime(createdUnixtime)
    isoCreated = time.strftime("%Y-%m-%dT%H:%M:%S", created)
    # print(timestamp)
    # print(created)
    print("{}\t{}\t{}".format(isoTimestamp, isoCreated, (timestampUnixtime + 3600 - createdUnixtime) / 3600))
