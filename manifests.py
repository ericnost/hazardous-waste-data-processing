import math
import csv
import os

#a script to take filenames of all PDFs and match them with EPA ID#s

#Try to load the CSV files. If it doesn't exist, exit.
switch = True
while switch == True:
    try:
        f = open('allImportsforHMM.csv', 'rt')
        switch = False
    except:
        print "Sorry, the database could not be opened"
        switch = False

csvfile = open('manifestsHMM1-21-16.csv', 'wb')
writer = csv.writer(csvfile, delimiter=',')
writer.writerow(['epaNumber']+['year'] + ['filename'] + ['fileOpener'])

g = open('allforMans.csv', 'rt')

reader = csv.reader(f)
secondReader = csv.reader(g)

##scan spreadsheet
for row in reader:
    g.seek(0)
    for row2 in secondReader:
        ##print row2
        row2[1] = row2[1]+".pdf"
       ## print row[0], row2[2]
        if row[0] == row2[1]:
            fname = row[0]
            year = int(fname[3:7])
            epa = row2[0]
            ##fileOpener = row[2]
            writer.writerow([epa] + [year] + [fname])
            break
        
f.close()
csvfile.close()
g.close()


     

