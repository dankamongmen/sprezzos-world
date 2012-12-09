#!/usr/bin/python
# helper script for license 
# check manualy
import fileinput
import re


def initialstate(fd,line):
     if fd.isfirstline():
         print "Files: %s" % fileinput.filename()
     if(not re.match("^#",line)):
         return "endofcomment"
     if(re.search("^#[\s]*LICENSE",line)):
        return "licensesection"
     return "initialstate"

def licensesection(fd,line):
    if(re.search("^#[\s]*Copyright",line)):
        m = re.match("^#[\s]*Copyright.* ([0-9]+)[\s]*(.*)",line)
        print "Copyright: %s, %s" % (m.group(1) ,m.group(2))
        return "onecopyright"
    if(not re.match("^#",line)):
        return "endofcomment"
    return "licensesection"

def onecopyright(fd,line):
    if(re.search("^#(\s)*Copyright",line)):
        m = re.match("^#[\s]*Copyright.* ([0-9]+)[\s]*(.*)",line)
        print "           %s, %s" % (m.group(1) ,m.group(2))
        return "onecopyright"
    if(not re.match("^#",line)):
        return "endofcomment"
    return "licensetext"

def licensetext(fd,line):
    if(not re.match("^#",line)):
        return "endofcomment"
    if(re.match("^#[\s]*$",line)):
        return "licensetext"
    m = re.match("^#[\s]*(.*)",line)
    print "License: GNU All-Permissive License"
    print "License: GPL2+ with Autoconf Macro's exception"
    print "License: GPL3+ with Autoconf Macro's exception"
    print "License: GPL3+ with Autoconf Configure Script 3 exception"
    print "License: CHECK"
    print "  %s" % m.group(1)
    return "licensetextfollow"

def licensetextfollow(fd,line):
    if(not re.match("^#",line)):
        return "endofcomment"
    if(re.match("^#[\s]*$",line)):
        return "licensetextfollow"
    m = re.match("^#[\s]*(.*)",line)
    print "  %s" % m.group(1)
    return "licensetextfollow"

takeaction = {
    "initialstate" : initialstate,
    "licensesection" : licensesection,
    "onecopyright" : onecopyright,
    "licensetext" : licensetext,
    "licensetextfollow" : licensetextfollow
}

def process(fd,line,state):
    return takeaction.get(state)(fd,line)    
 
state = "initialstate" 
for line in fileinput.input():
    state = process(fileinput,line,state)
    if(state == "endofcomment"):
        print "\n"
        exit(0)
