import math
import csv
import os

#Try to load the CSV files. If it doesn't exist, exit.
switch = True
while switch == True:
    try:
        f = open('inputs/gephi.csv', 'rt')
        switch = False
    except:
        print "Sorry, the database could not be opened"
        switch = False

csvfile = open('outputs/manifestCheck.csv', 'wb')
writer = csv.writer(csvfile, delimiter=',')
writer.writerow(['manifest #1'] + ['filename1'] + ['manifest #2'] + ['filename2'])

reader = csv.reader(f)
file2=list(csv.reader(f))
f.seek(0)
for row in reader:
    manifest = row[3]
    filename = row[41]
    for row2 in file2:
        manifest2 = row2[3]
        filename2 = row2[41]
        ##print manifest, manifest2
        if manifest == manifest2 and filename != filename2:
            ##print "match"
            writer.writerow([manifest]+[filename]+[manifest2]+[filename2])
        else:
            pass
            
f.close()
csvfile.close()

     

