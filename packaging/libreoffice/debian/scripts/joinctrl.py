#! /usr/bin/python

import re, sys, fileinput

def splitlines():
    fields = ('Build-Depends', 'Build-Conflicts', 'Build-Depends-Indep', 'Depends', 'Replaces',
              'Provides', 'Conflicts', 'Recommends', 'Suggests')
    for line in fileinput.input():
        line = line[:-1]
        field = None
        for f in fields:
            if line.startswith(f+':'):
                field = f
                break
        if not field:
            print line
            continue
        values = [f.strip() for f in line.split(':',1)[1].strip().split(',')]
        if len(values) > 2:
            print '%s: %s' % (field, ',\n '.join(values))
        else:
            print '%s: %s' % (field, ', '.join(values))

    
def joinlines():
    fields = ('Build-Depends', 'Build-Conflicts', 'Build-Depends-Indep', 'Depends', 'Replaces',
              'Provides', 'Conflicts', 'Recommends', 'Suggests')
    buffer = None
    for line in fileinput.input():
        line = line[:-1]
        if buffer:
            if line.startswith(' '):
                buffer = buffer + ' ' + line.strip()
                continue
            else:
                print re.sub(r' *,', r',', buffer)
                buffer = None
        field = None
        for f in fields:
            if line.startswith(f+':'):
                field = f
                break
        if field:
            buffer = line.strip()
            continue
        print line

def main():
    #splitlines()
    joinlines()

main()
