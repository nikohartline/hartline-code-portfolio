#!/usr/bin/python

#Program designed by Niko Hartline code developed courtesy of Julian Hartline.
#For more info on Julian and his work, visit http://www.julianhartline.com/
#For more info on Niko and his work, visit https://www.nikohartline.com/

import csv;
import sys;

def loadCsv(csvname):
    with open(csvname,'rbU') as csvfile:
        reader = csv.reader(csvfile,delimiter=",",quotechar="\"");
        i = 0;
        header = [];
        data = [];
        for row in reader:
            if (i == 0):
                header = row;
            else:
                rowdata = {}
                for col,head in zip(row,header):
                    rowdata[head] = col;
                data.append(rowdata);

            i = i + 1;
    return data;


def filterByCategory(data,categories):
    filtered = [];
    for info in data:
        if info["Category"] in categories:
            filtered.append(info);
    return filtered;


data = loadCsv("All_Species_ID_List.csv")
filtered = data

#print out only the id

alreadyPrinted = {}
for info in filtered:
    if info["Red List Species ID"] not in alreadyPrinted:
        alreadyPrinted[info["Red List Species ID"]] = True;
#print info["Scientific Name"],info["Category"],info["Red List Species ID"]
        print info["Red List Species ID"]


