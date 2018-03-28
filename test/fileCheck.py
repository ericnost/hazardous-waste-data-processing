import math
import csv
import os

#Try to load the CSV files. If it doesn't exist, exit.
switch = True
while switch == True:
    try:
        f = open('test.csv', 'rt')
        switch = False
    except:
        print "Sorry, the database could not be opened"
        switch = False

csvfile = open('FilesTBD.csv', 'wb')
writer = csv.writer(csvfile, delimiter=',')
writer.writerow(['year'] + ['filename'])
    
reader = csv.reader(f)       
row_count = sum(1 for row in reader)
print row_count

##pull filename from dir
rootDir = 'folder'
for dirName, subdirList, fileList in os.walk(rootDir):
    print('Found directory: %s' % dirName)
    for fname in fileList:
        ##print('\t%s' % fname)
        ##scan spreadsheet
        f.seek(0)
        x = 0
        for row in reader:
            x += 1
            row[1] = row[1]+".pdf"
            if row[1] == fname: #if find, move on
                break
            if x == row_count: ## if at end and haven't found it, write it
                ## do string work on fname to get year - pos 4-7
                year = int(fname[3:7])
                writer.writerow([year] + [fname])

f.close()
csvfile.close()


     

