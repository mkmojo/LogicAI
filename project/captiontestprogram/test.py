#!/usr/bin/env python
import sys
#import httplib
import urllib
import urllib2
import os
import time
from datetime import datetime
from os import path

def write_file (filename, body, mode):
    #print body
    pfile = open(filename,mode)
    pfile.write(body)
    pfile.close()

def print_file(filename):
    pfile = open(filename,"r")
    print pfile.read()
    pfile.close()

def remove_humans_tag (filename):
    u = ""
    with open(filename) as f:
        for line in f:
            if (not ("<?xml?>" in line)):
                u += line
           # else:
           #     print "in file",filename
           #     print line
    return u


for filename in os.listdir(os.getcwd()):
    if filename.endswith(".xml"):
        #print "processing",filename
        filein = open(filename, 'r')
        xml = filein.read()
        filein.close()
        u = urllib2.urlopen(url='http://cs.rochester.edu/research/epilog/api/caption.php',
                data=urllib.urlencode({'xml': xml}))
        if not os.path.isdir(os.path.join("result")):
            os.makedirs("result", 0700)

        outfile = os.path.join("result","out_"+filename)
        outfile_exists = os.path.isfile(outfile)
        if not outfile_exists :
            write_file(outfile,u.read(),'w')
        elif outfile_exists:
            outfile_ctime = datetime.fromtimestamp(path.getctime(outfile))
            if outfile_ctime < datetime.now():
                write_file(outfile,u.read(),'w')
        else:
            print "error"

#print "combining results"
os.chdir("result")
#print os.getcwd()
final_result = os.path.join("final_result.xml")
write_file(final_result,"<?xml?>\n","w")
write_file(final_result,"<multiple-results>\n","a")
for filename in os.listdir(os.getcwd()):
    if filename != "final_result.xml":
        body = remove_humans_tag(filename)
        write_file(final_result,body,"a")
write_file(final_result,"</multiple-results>\n","a")
print_file(final_result)
