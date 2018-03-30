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

waste = raw_input("What waste do you want to know about?(Hint: USE CAPS!) ")

csvfile = open('outputs/'+waste+'.csv', 'wb')
writer = csv.writer(csvfile, delimiter=',')
writer.writerow(['exporter'] + ['importer'])

reader = csv.reader(f)       

for row in reader:
    if waste in row[12]:
        writer.writerow(row)

f.close()
csvfile.close()

     

