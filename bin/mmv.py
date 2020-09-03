#!/usr/bin/env python3

import sys, shutil, os.path

def printUsage():
    print("Usage: mmv OLD NEW [FILE] ...") 

def printHelp():
    printUsage()
    print("Rename all FILEs such that each occurrence of substring OLD is replaced by substring NEW.")

def moveFile(old, new, path):
    if os.path.isfile(path):
        newpath = path.replace(old, new)
        shutil.move(path, newpath)
    else:
        print("mmv: path '{0}' does not exist. Could not rename.".format(path)) 
    
nargs = len(sys.argv)

if (nargs == 2) and ((sys.argv[1] == "--help") or (sys.argv[1] == "-h")):
    printHelp()
elif nargs < 3:
    print("mmv: Too few arguments") 
    printUsage()
elif nargs == 3:
    print("mmv: No files given. Nothing wil be done.") 
    printUsage()
else:
    old = sys.argv[1]
    new = sys.argv[2]
    files = sys.argv[3:]
    [moveFile(old, new, file) for file in files]
