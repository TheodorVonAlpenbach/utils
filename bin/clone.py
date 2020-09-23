#!/usr/bin/env python3

import sys, shutil, os.path, glob

def toggleVerboseMode(isOn):
    IS_VERBOSE = isOn
# https://docs.python.org/3/library/argparse.html
import argparse
parser = argparse.ArgumentParser(description='Clone files and content.')
parser.add_argument('-v', '--verbose',
                    action='store_true',
                    dest='VERBOSE',
                    required=False,
                    help='run in verbose mode')
parser.add_argument('-a', '--dry-run',
                    action='store_true',
                    dest='DRY_RUN',
                    required=False,
                    help='run as in verbose mode without any actions')

subparsers = parser.add_subparsers(help='Available sub commands', dest='command')

parserCp = subparsers.add_parser('cp', help='clone description here...')
parserMv = subparsers.add_parser('mv', help='rename description here...')
parserReport = subparsers.add_parser('report', help='report description here...')

for subparser in [parserCp, parserMv, parserReport]:
    subparser.add_argument('old', metavar='OLD', type=str,
                           help='The old name in kebab case, e.g. \'my-old-name\'')

for subparser in [parserCp, parserMv]:
    subparser.add_argument('new', metavar='NEW', type=str,
                           help='The new name in kebab case, e.g. \'my-new-name\'')

for subparser in [parserCp, parserMv, parserReport]:
    subparser.add_argument('dir', metavar='DIR', type=str, nargs='?', default='.',
                           help='The directory from where to recursively clone')

args = parser.parse_args()

### Utils (consider moving these)

## Case utils
def camelToSnake(str): 
    return ''.join(['_'+i.lower() if i.isupper()  
               else i for i in str]).lstrip('_') 

def snakeToKebab(str): 
    return str.replace('_', '-')

def kebabToSnake(str): 
    return str.replace('-', '_')

def uncapCamel(str):
    return str[0].lower() + str[1:]

def snakeToCamel(str):
    return str.title().replace("_", "")

def kebabToCamel(str):
    return str.title().replace("-", "")

## String utils
# TODO: rename this! Something with position, match, and string
def findR(str, pattern):
    if (type(pattern) is list):
        # TODO: use some sequence magic instead of this...
        for i in range(len(pattern)):
            if (findR(str, pattern[i]) >= 0):
                return i
        return -1
    else:
        try:
            return str.find(pattern)
        except:
            printVerbose(str)
            return -1

def substituteAll(str, olds, news):
    res = str
    for i in range(len(olds)):
        res = res.replace(olds[i], news[i])
    return res

## File utils
def fileContains(path, pattern):
    return findR(fileToString(path), pattern) >= 0

def fileToString(path):
    try:
        file = open(path)
        res = file.read()
        file.close
        return res
    except UnicodeDecodeError: 
        printVerbose("Could not read %s because of UnicodeDecodeError" % path)
    else:
        return ""

def stringToFile(str, path):
    file = open(path, "w")
    file.write(str)
    file.close

## Local functions
def printVerbose(string):
    if args.VERBOSE:
        print(string)

def isDryRun():
    return args.DRY_RUN

def cloneDirectory(name, root, olds, news, i):
    path = os.path.join(root, name)
    newPath = os.path.join(root, name.replace(olds[i], news[i]))
    printVerbose('newPath')
    printVerbose(newPath)
    if (not os.path.isdir(newPath)):
        printVerbose("Creating new directory " + newPath)
        if (not isDryRun()):
            os.mkdir(newPath)
        
def cloneFile(name, root, olds, news, i):
    path = os.path.join(root, name)
    newPath = os.path.join(root, name.replace(olds[i], news[i]))
    printVerbose("Cloning file " + path + " to " + newPath)
    if (not isDryRun()):
        stringToFile(substituteAll(fileToString(path), olds, news), newPath)

def cloneStrings(kebab):
    snake = kebabToSnake(kebab)
    camel = kebabToCamel(kebab)

    return [kebab, kebab.upper(),
            snake, snake.upper(),
            camel, uncapCamel(camel),
            camel.lower(), camel.upper()] 

## Commands
def cloneCopy(olds, news, rootdir):
    printVerbose("Cloning old files in directory " + rootdir + "...")
    printVerbose(olds)
    for i in range(len(olds)):
        for root, dirs, files in os.walk(rootdir):
            printVerbose(dirs)
            for dir in dirs:
                printVerbose(dir + ', ' + olds[i])
                if (findR(dir, olds) >= 0):
                    cloneDirectory(dir, root, olds, news, i);

            for file in files:
                if (findR(file, olds) >= 0):
                    cloneFile(file, root, olds, news, i)

def cloneMove(olds, news, rootdir):
    printVerbose("Renaming old files in directory " + rootdir + "...")
    for root, dirs, files in os.walk(rootdir):
        for dir in dirs:
            if (findR(dir, olds) >= 0):
                moveDirectory(dir, root, olds, news, i);

        for file in files:
            if (findR(file, olds) >= 0):
                moveFile(file, root, olds, news, i)

def cloneReport(olds, rootdir):
    printVerbose("report occurrences of OLD-ish strings in files with a name not matching OLD")
    for root, dirs, files in os.walk(rootdir):
        for file in files:
            path = os.path.join(root, file)
            if (findR(path, olds) < 0):
                if fileContains(path, olds):
                    print(path)

## Main
printVerbose(args)
# printVerbose(findR('old-file-dir', ['iold-file']))
# sys.exit(0)

olds = cloneStrings(args.old)

if (args.command == 'cp'):
    cloneCopy(olds, cloneStrings(args.new), args.dir)
elif (args.command == 'mv'):
    cloneMove(olds, cloneStrings(args.new), args.dir)
elif (args.command == 'report'):
    cloneReport(olds, args.dir) 
else:
    sys.exit(0)
