import math
import csv
import os
import json

##Try to load the CSV files. If it doesn't exist, exit.
switch = True
while switch == True:
    try:
        f = open('inputs/2011-2012 shipment network analysis by company.csv', 'rt')
        switch = False
    except:
        print "Sorry, the database could not be opened"
        switch = False

reader = csv.reader(f)


##do nodes
nodesList = [{"name": "default"}]
for row in reader:
        name = row[0]
        for i in range(0,len(nodesList)):
            if (nodesList[i]["name"] == name):
                break
            elif i == len(nodesList)-1:
                nodesList.append({"name": name, "index": i})

f.seek(0)
nodesList.remove({"name": "default"})

##count shipments made
shipsMade = []
for k in range(0,len(nodesList)):
    counter = 0
    f.seek(0)
    s = nodesList[k]["name"]
    for row in reader:
        if nodesList[k]["name"] == row[0]:
            counter += 1
    shipsMade.append({s: counter})

##count shipments received
shipsRecd = []
for k in range(0,len(nodesList)):
    counter = 0
    f.seek(0)
    s = nodesList[k]["name"]
    for row in reader:
        if nodesList[k]["name"] == row[1]:
            counter += 1
    shipsRecd.append({s: counter})

##do links
linksList = []
f.seek(0)
source = "ACTION ANODIZING AND PLATING"
target = "US ECOLOGY"
counter = 0
for row in reader:
    if row[0] == source and row[1]== target:
        counter += 1
    else:
        for k in range(0,len(nodesList)):
            if source == nodesList[k]["name"]:
                s = nodesList[k]["index"]
        for l in range(0,len(nodesList)):
            if target == nodesList[l]["name"]:
                t = nodesList[l]["index"]
        linksList.append({"source": s, "target": t, "value": counter})
        source = row[0]
        target = row[1]
        counter = 1     
f.close()

dic = {"nodes": nodesList, "links": linksList, "exports": shipsMade, "imports": shipsRecd}
json = json.dumps(dic)
j= open('outputs/trades11-12.json', 'w+')
j.write(json)
j.close()



     

