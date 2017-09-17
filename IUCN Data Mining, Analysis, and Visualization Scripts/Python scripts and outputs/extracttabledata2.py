#!/usr/bin/python

#Program designed by Niko Hartline code developed courtesy of Julian Hartline.
#For more info on Julian and his work, visit http://www.julianhartline.com/
#For more info on Niko and his work, visit https://www.nikohartline.com/

import csv;
import sys;
import re;
import os;
import unicodedata
from sets import Set;

def strip_accents(s):
   return ''.join(c for c in unicodedata.normalize('NFD', s)
                  if unicodedata.category(c) != 'Mn')

def parseTable(t):
    tableData = []
    rows = re.findall("<tr.*?>(.*?)</tr>",t,re.DOTALL);
    for r in rows:
        rowData = []
        td = re.findall("<t[dh].*?>(.*?)</t[dh]>",r,re.DOTALL);
        for d in td:
            d = re.sub('<[^<]+?>', '', d)
            rowData.append(d.strip());
        tableData.append(rowData);
    return tableData;

def extractTableData(content):
    tables = []
    matches = re.findall("<table .*?class=\"tab_data\".*?>(.*?)</table>",content,re.DOTALL);
    for m in matches:
        tables.append(parseTable(m));
    return tables

def extractKeyValuesFromTable(data):
    info = {};
    for table in data:
        for row in table:
            if (row.__len__() != 2):
#print "MISSING DATA: ",row;
                continue;
            key = row[0].strip(":");
            value = row[1];
            info[key] = value;
#print key.ljust(20),value
    return info;

def writeCsv(data,filename):
    with open(filename, 'wb') as csvfile:
        writer = csv.writer(csvfile, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)
        for r in data:
            writer.writerow(r);


downloadPath = "details"


#no status = 0 each other one corresponds to the index
statuses = ["","introduced", "native", "possibly extinct", "present - origin uncertain", "regionally extinct", "reintroduced", "vagrant"];

i = 0
entries = [];
headers = Set();
for line in sys.stdin:
    id = line.strip()
    detailsPath = "./"+downloadPath+"/"+id+".html"
    f = open(detailsPath,"r");
    text = f.read()
    f.close();
    m = re.search("<span class=\"sciname\">(.*?)</span>",text,re.DOTALL)
    sciname = m.group(1)
    data = extractTableData(text)
    taxonomyRaw = data[0]
    taxonomyInformation = {};
    for key,value in zip(taxonomyRaw[0],taxonomyRaw[1]):
        taxonomyInformation[key] = value;
    data.pop(0)
    info = extractKeyValuesFromTable(data);

#    if "Systems" in info and info["Systems"].strip() in ["Freshwater", "Terrestrial; Marine","Freshwater; Marine", "Marine"]:
#        print("Removing: "+sciname);
#        continue; #### REMOVED THIS CODE TO LEAVE IN ALL SYSTEMS

    if ("Countries" not in info):
        print("NO COUNTRY FOUND: "+sciname)
        entries.append({"Name":sciname})
        continue

    converted = info["Countries"].decode("utf8").encode("ascii","ignore");
    sections = converted.strip().split("\n\n")

    colmap = {"Name":sciname};
    for s in sections:
        status,_,locations = s.partition(":")
        for l in locations.split(";"):
            l = l.strip()
            before = l;
            l = re.sub("\(.*\)","",l) #remove parens
            l = re.sub("[\' ,\-]","",l) #remove spaces
            l = re.sub(status,"",l) #remove status designation
#print("  "+before+" : "+l);
            headers.add(l);
            colmap[l] = statuses.index(status.lower())
#print(statuses.index(status.lower()))
#print("  "+l);
    entries.append(colmap);

    i = i + 1


csvdata = [];
csvdata.append(["Name"] + sorted(headers));
for countrymap in entries:
    rowdata = [];
    rowdata.append(countrymap["Name"]);
    for col in sorted(headers):
        if col in countrymap:
            rowdata.append(countrymap[col]);
        else:
            rowdata.append(0);

    csvdata.append(rowdata);

#print "##########"
#print("name",sciname);
#print(info.keys());
#print "##########"
#print id;

writeCsv(csvdata,"output.csv")
