import math
import csv
import os

#Try to load the CSV files. If it doesn't exist, exit.
switch = True
while switch == True:
    try:
        f = open('inputs/data.csv', 'rt')
        switch = False
    except:
        print "Sorry, the database could not be opened"
        switch = False

csvfile = open('inputs/FilesTBD.csv', 'wb')
writer = csv.writer(csvfile, delimiter=',')
##writer.writerow(['year'] + ['filename'] + ['directory'])

csvfileTotal = open('outputs/allImportsforHMM.csv', 'wb')
totalWriter = csv.writer(csvfileTotal, delimiter=',')
totalWriter.writerow(['year'] + ['filename']+['directory'])
    
reader = csv.reader(f)       
row_count = sum(1 for row in reader)

##pull filename from dir
rootDir = 'data/EPA follow up data OCR' #not currently available
for dirName, subdirList, fileList in os.walk(rootDir):
    print('Found directory: %s' % dirName)
    for fname in fileList:
        year = int(fname[3:7])
        totalWriter.writerow([year] + [fname] + [dirName])
##        ##scan spreadsheet
##        f.seek(0)
##        x = 0
##        for row in reader:
##            x += 1
##            row[1] = row[1]+".pdf"
##            if row[1] == fname: #if find, move on
##                break
##            if x == row_count: ## if at end and haven't found it, write it
##                ## do string work on fname to get year - pos 4-7
##                year = int(fname[3:7])
##                writer.writerow([year] + [fname] + [dirName])

f.close()
csvfile.close()
csvfileTotal.close()


     

