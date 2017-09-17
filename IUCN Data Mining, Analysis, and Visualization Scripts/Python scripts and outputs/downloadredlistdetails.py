#!/usr/bin/python

#Program designed by Niko Hartline code developed courtesy of Julian Hartline.
#For more info on Julian and his work, visit http://www.julianhartline.com/
#For more info on Niko and his work, visit https://www.nikohartline.com/

import csv;
import sys;
import requests;
import os;
import os.path;

downloadPath = "details"
try:
    os.mkdir("./"+downloadPath)
except OSError:
    pass

for line in sys.stdin:
    id = line.strip()
    url = "http://www.iucnredlist.org/details/%s/0" % (id)
    detailsPath = "./"+downloadPath+"/"+id+".html"
    if not os.path.isfile(detailsPath):
        r = requests.get(url)
        f = open(detailsPath,"w")
        f.write(r.content)
        f.close();

    print id;
