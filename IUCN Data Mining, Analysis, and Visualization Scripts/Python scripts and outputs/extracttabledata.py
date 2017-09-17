#!/usr/bin/python

#Program designed by Niko Hartline code developed courtesy of Julian Hartline.
#For more info on Julian and his work, visit http://www.julianhartline.com/
#For more info on Niko and his work, visit https://www.nikohartline.com/

import csv;
import sys;
import requests;
import re;
import os;

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
includeKeys = ['Population Trend', 'Habitat and Ecology', 'Range Description', 'Countries','Systems', 'Conservation Actions', 'Population']

csvdata = [];
header = []
header.append("Scientific Name");
header.extend(['Kingdom', 'Phylum', 'Class', 'Order', 'Family']);
for col in includeKeys:
    header.append(col);
csvdata.append(header);

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

    rowdata = [];
    rowdata.append(sciname);

    keys = ['Kingdom', 'Phylum', 'Class', 'Order', 'Family'];
    for k in keys:
        if (k in taxonomyInformation):
            rowdata.append(taxonomyInformation[k]);
        else:
            rowdata.append("");

    for col in includeKeys:
        if col in info:
            rowdata.append(info[col]);
        else:
            rowdata.append("");
    csvdata.append(rowdata);

#print "##########"
#print("name",sciname);
#print(info.keys());
#print "##########"
#print id;

writeCsv(csvdata,"outputinfo.csv")
