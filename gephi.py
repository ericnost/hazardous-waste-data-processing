import math
import csv
import os

#Try to load the CSV files. If it doesn't exist, exit.
switch = True
while switch == True:
    try:
        f = open('gephi.csv', 'rt')
        switch = False
    except:
        print "Sorry, the database could not be opened"
        switch = False

csvfile = open('gephiResults.csv', 'wb')
writer = csv.writer(csvfile, delimiter=',')
writer.writerow(['exporter'] + ['importer'])

reader = csv.reader(f)       

for row in reader:
    exporter = row[4]
    importer = row[26]
    writer.writerow([exporter] + [importer])

f.close()
csvfile.close()

     

