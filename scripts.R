#### R Functions for processing hazardous waste data ####
# This script processes North American hazardous waste import/export data. You can access and find more about the data here: https://geography.wisc.edu/hazardouswaste/data.html
# It may be run straight-through, or modularly. 
# It outputs:
### a CSV of import/export info aggregated at the US ~state~ level
### a CSV of import/export info aggregated at the US ~site~ level (site = waste import and/or export facility)
### basic pie and bar chart representations of the waste trade, broken down by kg, l, # of shipments, and types/danger level/responsible firms of waste traded 
### raw output from a LISA (local indicators of spatial association) analysis, specifically Local Moran's. It operates on both US import and export data, with kg, l, and # of shipments as units, aggregated by state. It does not map the clusters, but outputs a CSV that can be joined with spatial data (states).
### plots a point pattern analysis (Ripley's K) of US import sites
### scores (f stat) from a hierarchical linear regression approach to correlating a US importer's waste profile with the demographics of its surroundings
### plots and scores from various approaches to: 1) reducing waste type and demographic variables to 2-D (multidimensional scaling); 2) clustering import sites based on their waste and demographic profiles (k-means, DIANA, AHC)


#### DEFINITIONS AND TERMINOLOGY ####
# imports - US imports of hazardous waste from Mexico and Canada
# exports - US exports of waste to Mexico and Canada
# sites - facilities that export waste or import it for processing (incineration, landfilling, recycling, etc.)
# kg - kilograms
# l - liters
# # of shipments - a shipment is a discrete unit of waste traded and is the basic unit by which waste managers describe the danger level, chemical composition, etc. of the waste. 
# DEV - any comment following DEV is a note on potential future development of this script



#### TABLE OF CONTENTS ####
# Requirements - the packages or libraries that you will need to install and call in order to access special functions for processing the hazardous waste data

# Prep data - always run this module, as it loads all the data and creates an object to store and use the data in later functions
## Load main data
## Summarize exports and imports by years 

# Geography - aggregates data by units (kg, l, shipments) and then by both states and sites
## US imports by units: shipments, liters, kg
## US imports by states
## US exports by units: shipments, liters, kg
## US exports by states
## US imports by units
## US imports by sites
## US exports by units
## US exports by sites

# Attributes - aggregates data by attributes (UN waste type, expected processing method, packing group (a proxy for danger level)) and produces pie and bar chart representations
## Compare number of shipments between imports and exports
## Compare total kg between imports and exports
## Compare total l between imports and exports
## US imports by originating country: shipments
## US imports by originating country: kg
## US imports by originating country: l
## US exports by destination: shipments
## US exports by destination: kg
## US exports by destination: l
## US imports by packing group (danger level): shipments
## US imports by packing group (danger level): kg
## US imports by packing group (danger level): l
## Histograms of import types (UN code) 2011-2012
## Histograms of export types (UN code) 2011-2012
## Histograms of top ten US importing firms: shipments, kg, l.
## Histograms of top ten North American trading firms: shipments 2011-2012

# Spatial Clusters - First, a LISA analysis of waste trade clusters at the US state level. For both imports and exports, and each unit type (kg, l, and shipment). Then, a point pattern analysis of import sites.
## Prep data by doing spatial join
## Local Moran's measures - US exports by states
## Local Moran's measures - US imports by states
## Point Pattern Analysis

# Correlates - First, a hierarchical linear modeling approach to associating various attributes of the imports dataset with facility-level demographic variables. Then, a multi-dimensional scaling approach to grouping import sites by their waste and demographic profiles
## Create a waste type matrix
## Hierarchical Linear Regression
## Multidimensional Scaling (MDS)
## Hierarchical clustering
## K-means clusters



#### REQUIREMENTS ####
#You may need to install each of these 'packages' if you haven't already. To do so, you will need to run ~ install.packages("ggplot2") ~ and so on for each
library(ggplot2)
library(maptools)
library(rgeos)
library(rgdal)
library(spdep)
library(tidyr)
library(dplyr)
library(utf8)
library(cluster)
library(stringr)
library(spatstat)



#### PREP DATA ####
## Load main data. For more information on the dataset, see here: https://geography.wisc.edu/hazardouswaste/data.html
imports = read.csv('inputs/imports.csv') # US imports of waste from Canada and Mexico 2007, 2009-2012
exports = read.csv('inputs/exports.csv') # US exports of waste from Canada and Mexico (data only complete for 2011-2012)
trades = read.csv('inputs/network.csv') # a pre-compiled list of waste trade relationships between firms. Each row represents a shipment. The first column is the exporter, the second the importer.
demos= read.csv('inputs/demographics.csv') # a pre-compiled set of demographic data for each US import facility. Obtained from the US Census and EPA ECHO.

## Summarize exports and imports by years - create 2011-2012 versions of the data for consistent comparison (imports.years and exports.years)
imports.years <- imports[ which (imports$Year == '2011' | imports$Year == '2012'),]
exports <- exports[ which (exports$Report.Year == '2011' | exports$Report.Year == '2012'),] # By default we are limiting exports to 2011-2012. This overrides the original load of the export data as 'exports,' ensuring that users do not employ incomplete exports data. DEV: For consistency with imports, may change name to exports.years



#### GEOGRAPHY ####
## US imports by units: shipments, liters, kg
kg<-subset(imports.years, units.final=="kg") # For all years in the data (2007,2009-2012), use imports rather than imports.years
l<-subset(imports.years, units.final=="l")
blanks<-subset(imports.years, units.final=="") # The set of shipments that did not list any units

## US imports by states
state.kg<-data.frame(tapply(kg$quantity.final, kg$Receiving.Facility.State, sum, na.rm=TRUE)) #na.rm=TRUE handles summarizing null values
state.l<-data.frame(tapply(l$quantity.final, l$Receiving.Facility.State, sum, na.rm=TRUE))
state.blanks<-data.frame(tapply(blanks$quantity.final, blanks$Receiving.Facility.State, sum, na.rm=TRUE))
state.shipments <- data.frame(tapply(imports.years$Year, imports.years$Receiving.Facility.State, length))
state<-cbind(state.shipments, state.blanks, state.kg, state.l)
colnames(state)<-c("shipments", "blanks", "kg", "l")

## US exports by units: shipments, liters, kg
kg<-subset(exports, unit.final=="kg")
l<-subset(exports, unit.final=="l")
blanks<-subset(exports, unit.final=="")

## US exports by states
state.ex.kg<-data.frame(tapply(kg$quant.final, kg$US.Exporter.Site.State, sum, na.rm=TRUE))
state.ex.l<-data.frame(tapply(l$quant.final, l$US.Exporter.Site.State, sum, na.rm=TRUE))
state.ex.blanks<-data.frame(tapply(blanks$quant.final, blanks$US.Exporter.Site.State, sum, na.rm=TRUE))
state.ex.shipments <- data.frame(tapply(exports$Report.Year, exports$US.Exporter.Site.State, length))
state.ex<-cbind(state.ex.shipments, state.ex.blanks, state.ex.kg, state.ex.l)
colnames(state.ex)<-c("export shipments", "export blanks", "export kg", "export l")

state<-merge(state, state.ex, by=0, all=TRUE) # Joins exports and imports table 

write.csv(state, file = "outputs/state.csv") # Output state-level import/export data


## US imports by units
kg<-subset(imports, units.final=="kg") # For 2011-2012 specifically, imports.years
l<-subset(imports, units.final=="l")
blanks<-subset(imports, units.final=="") #the set of shipments that did not list units

## US imports by sites
site.kg<-data.frame(tapply(kg$quantity.final, kg$Receiving.Facility.EPA.ID.Number.2, sum, na.rm=TRUE)) #na.rm=TRUE handles summation of null values
site.l<-data.frame(tapply(l$quantity.final, l$Receiving.Facility.EPA.ID.Number.2, sum, na.rm=TRUE))
site.blanks<-data.frame(tapply(blanks$quantity.final, blanks$Receiving.Facility.EPA.ID.Number.2, sum, na.rm=TRUE))
site.shipments <- data.frame(tapply(imports$Year, imports$Receiving.Facility.EPA.ID.Number.2, length))
site<-cbind(site.shipments, site.blanks, site.kg, site.l)
colnames(site)<-c("shipments", "blanks", "kg", "l")

## US exports by units
kg<-subset(exports, unit.final=="kg")
l<-subset(exports, unit.final=="l")
blanks<-subset(exports, unit.final=="")

## US exports by sites
site.ex.kg<-data.frame(tapply(kg$quant.final, kg$US.Exporter.EPA.ID.Number, sum, na.rm=TRUE))
site.ex.l<-data.frame(tapply(l$quant.final, l$US.Exporter.EPA.ID.Number, sum, na.rm=TRUE))
site.ex.blanks<-data.frame(tapply(blanks$quant.final, blanks$US.Exporter.EPA.ID.Number, sum, na.rm=TRUE))
site.ex.shipments <- data.frame(tapply(exports$Report.Year, exports$US.Exporter.EPA.ID.Number, length))
site.ex<-cbind(site.ex.shipments, site.ex.blanks, site.ex.kg, site.ex.l)
colnames(site.ex)<-c("export shipments", "export blanks", "export kg", "export l")

site.total<-merge(site, site.ex, by=0, all=TRUE) # 'all=TRUE' lists ALL import and export sites. Remove to list only sites that both import and export.

write.csv(site.total, file = "outputs/sites.csv") # output import/export site-level data



#### ATTRIBUTES ####
## Compare number of shipments between imports and exports
i<-nrow(imports.years) # imports.years = 2011-2012
e<-nrow(exports) # exports = 2011-2012
slices<-c(i,e)
labels<-c("imports", "exports")
pct <- round(slices/sum(slices)*100)
labels <- paste(labels, pct) # add percents to labels 
labels <- paste(labels,"%",sep="") # add % to labels 
pie(slices, labels, main="Imports vs Exports (shipments) 2011-2012")

## Compare total kg between imports and exports
kg<-subset(imports.years, units.final=="kg")
i<-sum(kg$quantity.final, na.rm=TRUE) 
kg<-subset(exports, unit.final=="kg")
e<-sum(kg$quant.final, na.rm=TRUE) 
slices<-c(i,e)
labels<-c("imports", "exports")
pct <- round(slices/sum(slices)*100)
labels <- paste(labels, pct) # add percents to labels 
labels <- paste(labels,"%",sep="") # add % to labels 
pie(slices, labels, main="Imports vs Exports (kg) 2011-2012")

## Compare total l between imports and exports
l<-subset(imports.years, units.final=="l") #2011-2012
i<-sum(l$quantity.final, na.rm=TRUE) 
l<-subset(exports, unit.final=="l")
e<-sum(l$quant.final, na.rm=TRUE) 
slices<-c(i,e)
labels<-c("imports", "exports")
pct <- round(slices/sum(slices)*100)
labels <- paste(labels, pct) # add percents to labels 
labels <- paste(labels,"%",sep="") # ad % to labels 
pie(slices, labels, main="Imports vs Exports (l) 2011-2012")

## US imports by originating country: shipments. DEV: This can be filtered to weed out e.g. the 1 Germany import
shipments <- data.frame(tapply(imports.years$Year, imports.years$Country.of.Export, length))
slices<-shipments$tapply.imports.years.Year..imports.years.Country.of.Export..length.
slices[is.na(slices)]<-0
labels <- paste(names(slices), "\n", round(slices), sep="")
pie(slices, labels, main="Imports by originating country (shipments) 2011-2012")

## US imports by originating country: kg
kg<-subset(imports.years, units.final=="kg") #for 2011-2012, imports.years
kg<-data.frame(tapply(kg$quantity.final, kg$Country.of.Export, sum, na.rm=TRUE)) #na.rm=TRUE handles summation of null values
slices<-kg$tapply.kg.quantity.final..kg.Country.of.Export..sum..na.rm...TRUE.
slices[is.na(slices)]<-0
labels <- paste(names(slices), "\n", round(slices), sep="")
pie(slices, labels, main="Imports by originating country (kg) 2011-2012")

## US imports by originating country: l
l<-subset(imports.years, units.final=="l")
l<-data.frame(tapply(l$quantity.final, l$Country.of.Export, sum, na.rm=TRUE))
slices<-l$tapply.l.quantity.final..l.Country.of.Export..sum..na.rm...TRUE.
slices[is.na(slices)]<-0
labels <- paste(names(slices), "\n", round(slices), sep="")
pie(slices, labels, main="Imports by originating country (l) 2011-2012")

## US exports by destination: shipments
# A note on data structure: we do not have destination country explicitly represented, just province/state. Need to create lookups, perhaps with spatial data below.
shipments <- data.frame(tapply(exports$Report.Year, exports$Receiving.Facility.State.Province, length)) 
slices<-shipments$tapply.exports.Report.Year..exports.Receiving.Facility.State.Province..
slices[is.na(slices)]<-0
labels <- paste(names(slices), "\n", round(slices), sep="")
pie(slices, labels, main="Exports by destination (shipments) 2011-2012")

## US exports by destination: kg
kg<-subset(exports, unit.final=="kg")
kg<-data.frame(tapply(kg$quant.final, kg$Receiving.Facility.State.Province, sum, na.rm=TRUE))
slices<-kg$tapply.kg.quant.final..kg.Receiving.Facility.State.Province..
slices[is.na(slices)]<-0
labels <- paste(names(slices), "\n", round(slices), sep="")
pie(slices, labels, main="Exports by destination (kg) 2011-2012")

## US exports by destination: l
l<-subset(exports, unit.final=="l")
l<-data.frame(tapply(l$quant.final, l$Receiving.Facility.State.Province, sum, na.rm=TRUE))
slices<-l$tapply.l.quant.final..l.Receiving.Facility.State.Province..sum..
slices[is.na(slices)]<-0
labels <- paste(names(slices), "\n", round(slices), sep="")
pie(slices, labels, main="Exports by destination (l) 2011-2012")

## US imports by packing group (danger level): shipments
# We cannot make any comparison on danger level between imports and exports, so here we use the full import dataset from 2007, 2009-2012
shipments <- data.frame(tapply(imports$Year, imports$DOT.Packing.Group, length))
slices<-shipments$tapply.imports.Year..imports.DOT.Packing.Group..length.
slices[is.na(slices)]<-0
labels <- paste(names(slices), "\n", round(slices), sep="")
pie(slices, labels, main="Imports by packing group/danger level (shipments) 2011-2012")

## US imports by packing group (danger level): kg
kg<-subset(imports, units.final=="kg") #for 2011-2012, imports.years
kg<-data.frame(tapply(kg$quantity.final, kg$DOT.Packing.Group, sum, na.rm=TRUE)) #na.rm=TRUE handles summation of null values
slices<-kg$tapply.kg.quantity.final..kg.DOT.Packing.Group..sum..na.rm...TRUE.
slices[is.na(slices)]<-0
labels <- paste(names(slices), "\n", round(slices), sep="")
pie(slices, labels, main="Imports by packing group/danger level (kg) 2011-2012")

## US imports by packing group (danger level): l
l<-subset(imports, units.final=="l")
l<-data.frame(tapply(l$quantity.final, l$DOT.Packing.Group, sum, na.rm=TRUE))
slices<-l$tapply.l.quantity.final..l.DOT.Packing.Group..sum..na.rm...TRUE.
slices[is.na(slices)]<-0
labels <- paste(names(slices), "\n", round(slices), sep="")
pie(slices, labels, main="Imports by packing group/danger level (l) 2011-2012")

## Histograms of import types (UN code) 2011-2012
# Prep the data for histogram (bar chart) representation
kg<-subset(imports.years, units.final=="kg")
l<-subset(imports.years, units.final=="l")
blanks<-subset(imports.years, units.final=="")
un.kg<-data.frame(tapply(kg$quantity.final, kg$DOT.UN.ID.Code, sum, na.rm=TRUE))
un.l<-data.frame(tapply(l$quantity.final, l$DOT.UN.ID.Code, sum, na.rm=TRUE))
un.blanks<-data.frame(tapply(blanks$quantity.final, blanks$DOT.UN.ID.Code, sum, na.rm=TRUE))
un.shipments <- data.frame(tapply(imports.years$Year, imports.years$DOT.UN.ID.Code, length))
imports.by.un<-cbind(un.shipments, un.blanks, un.kg, un.l)
colnames(imports.by.un)<-c("shipments", "blanks", "kg", "l")

imports.by.un<-imports.by.un[order(-imports.by.un$shipments),]
topten<-imports.by.un[1:10,] # By default top ~10~ UN codes
barplot(topten$shipments, cex.names =.1, xlab="UN Code", ylab="shipments", main="Top Ten Imports by UN Waste Code (shipments) 2011-2012")

imports.by.un<-imports.by.un[order(-imports.by.un$kg),]
topten<-imports.by.un[1:10,] # By default top ~10~ UN codes
barplot(topten$kg, cex.names =.1, xlab="UN Code", ylab="kg", main="Top Ten Imports by UN Waste Code (kg) 2011-2012")

imports.by.un<-imports.by.un[order(-imports.by.un$l),]
topten<-imports.by.un[1:10,] # By default top ~10~ UN codes
barplot(topten$l, cex.names =.1, xlab="UN Code", ylab="liters", main="Top Ten Imports by UN Waste Code (l) 2011-2012")

## Histograms of export types (UN code) 2011-2012. UN Code info for exports is not very complete, so top result for shipments, kg, and l is null.
# Prep data for histogram (bar chart) representation
kg<-subset(exports, unit.final=="kg")
l<-subset(exports, unit.final=="l")
blanks<-subset(exports, unit.final=="")
un.kg<-data.frame(tapply(kg$quant.final, kg$DOT.UNIDCode, sum, na.rm=TRUE))
un.l<-data.frame(tapply(l$quant.final, l$DOT.UNIDCode, sum, na.rm=TRUE))
un.blanks<-data.frame(tapply(blanks$quant.final, blanks$DOT.UNIDCode, sum, na.rm=TRUE))
un.shipments <- data.frame(tapply(exports$Report.Year, exports$DOT.UNIDCode, length))
exports.by.un<-cbind(un.shipments, un.blanks, un.kg, un.l)
colnames(exports.by.un)<-c("shipments", "blanks", "kg", "l")

exports.by.un<-exports.by.un[order(-exports.by.un$shipments),]
topten<-exports.by.un[1:10,] # By default top ~10~ UN codes
barplot(topten$shipments, cex.names =.1, xlab="UN Code", ylab="shipments", main="Top Ten Exports by UN Waste Code (shipments) 2011-2012")

exports.by.un<-exports.by.un[order(-exports.by.un$kg),]
topten<-exports.by.un[1:10,] # By default top ~10~ UN codes
barplot(topten$kg,  cex.names =.1, xlab="UN Code", ylab="kg", main="Top Ten Exports by UN Waste Code (kg) 2011-2012")

exports.by.un<-exports.by.un[order(-exports.by.un$l),]
topten<-exports.by.un[1:10,] # By default top ~10~ UN codes
barplot(topten$l,  cex.names =.1, xlab="UN Code", ylab="liters", main="Top Ten Exports by UN Waste Code (l) 2011-2012")

## Histograms of top ten US importing firms: shipments, kg, l. We are not comparing this to exports (we do not explicitly have that data), so we can use the full imports dataset from 2007, 2009-2012
# Prep the data
kg<-subset(imports, units.final=="kg")
l<-subset(imports, units.final=="l")
blanks<-subset(imports, units.final=="")
firm.kg<-data.frame(tapply(kg$quantity.final, kg$Company, sum, na.rm=TRUE))
firm.l<-data.frame(tapply(l$quantity.final, l$Company, sum, na.rm=TRUE))
firm.blanks<-data.frame(tapply(blanks$quantity.final, blanks$Company, sum, na.rm=TRUE))
firm.shipments <- data.frame(tapply(imports$Year, imports$Company, length))
imports.by.firm<-cbind(firm.shipments, firm.blanks, firm.kg, firm.l)
colnames(imports.by.firm)<-c("shipments", "blanks", "kg", "l")
imports.by.firm<-imports.by.firm[-1,] #remove #N/As that we don't have company data for

imports.by.firm<-imports.by.firm[order(-imports.by.firm$shipments),]
topten<-imports.by.firm[1:10,] # By default top ~10~ UN codes
barplot(topten$shipments,  cex.names =.1,xlab="Firm", ylab="shipments", main="Top Ten Import Firms (shipments) 2007,2009-2012")

imports.by.firm<-imports.by.firm[order(-imports.by.firm$kg),]
topten<-imports.by.firm[1:10,] # By default top ~10~ UN codes
barplot(topten$kg, cex.names =.1, xlab="Firm", ylab="kg", main="Top Ten Import Firms (kg) 2007,2009-2012")

imports.by.firm<-imports.by.firm[order(-imports.by.firm$l),]
topten<-imports.by.firm[1:10,] # By default top ~10~ UN codes
barplot(topten$l, cex.names =.1, xlab="Firm", ylab="liters", main="Top Ten Import Firms (l) 2007,2009-2012") # DEV: cex.names =.1 will print everything out at least....

## Histograms of top ten North American trading firms: shipments 2011-2012
trades<-trades[1:2] # Clear other columns in the trading networks data
e <- data.frame(tapply(trades$importer, trades$exporter, length))
i <- data.frame(tapply(trades$exporter, trades$importer, length))
trades.sum<-merge(i, e, by=0, all=TRUE) # To include only firms that both import ~and~ export, remove all=TRUE
colnames(trades.sum)<-c("firm", "imports", "exports")

i<-trades.sum[order(-trades.sum$imports),]
topten<-i[1:10,] # By default top ~10~ UN codes
barplot(topten$imports, cex.names =.1, xlab="Firm", ylab="shipments", main="Top Ten North America Firms (import shipments) 2011-2012")

e<-trades.sum[order(-trades.sum$exports),]
topten<-e[1:10,] # By default top ~10~ UN codes
barplot(topten$exports,  cex.names =.1, xlab="Firm", ylab="shipments", main="Top Ten North American Firms (export shipments) 2011-2012")

#### SPATIAL CLUSTERS ####
na.map.shp<- readOGR(dsn = "inputs/na", layer = "na") # load our shapefile of North American states and provinces
# plot(na.map.shp,border="wheat3", col="wheat1") # To diagnostic the shapefile, plot it with this line of code
# na.map.shp.copy<-na.map.shp # create a temporary copy of the shapefile in the R environment, in order to do certain data manipulations

## Prep data by doing spatial join. We are joining the shapefile (US states) with our processed imports and exports data
na.map.shp <- merge(na.map.shp, state, by.x='postal', by.y='Row.names')
# Convert waste data to integer format
na.map.shp$`export shipments`<-as.integer(na.map.shp$`export shipments`)
na.map.shp$`export kg`<-as.integer(na.map.shp$`export kg`)
na.map.shp$`export l`<-as.integer(na.map.shp$`export l`)
na.map.shp$shipments <-as.integer(na.map.shp$shipments)
na.map.shp$kg <-as.integer(na.map.shp$kg)
na.map.shp$l <-as.integer(na.map.shp$l)

## Local Moran's - US imports and Exports by shipments, kg, l 2011-2012
s<-subset(na.map.shp, iso_a2=="US" & postal!="AK") # Subset data to US for LM and remove Alaska. DEV: And Hawaii?
s@data[is.na(s@data)]<-0 # Replace Null values (states with no imports or exports) with 0s

# Calculate contiguities
spatmatrix <- poly2nb(s) # Spatial matrix
listw <- nb2listw(spatmatrix) # Weights. Add 'zero.policy=TRUE' to handle zeros

##Local Moran's measures - US exports by states
lmoran <- localmoran(s$`export shipments`, listw, na.action=na.pass) # Assign zeros to NAs
output<-data.frame(s$`export shipments`,s$postal, lmoran[,1:5])
colnames(output)<-c("ex shipments", "postal", "ex ship LMi", "ex ship eLMi", "ex ship var LMi", "ex ship z LMi", "ex ship prob LMi")

lmoran <- localmoran(s$`export kg`, listw, na.action=na.pass)
dump<-data.frame(s$`export kg`,s$postal, lmoran[,1:5])
colnames(dump)<-c("ex kg", "postal", "ex kg LMi", "ex kg eLMi", "ex kg var LMi", "ex kg z LMi", "ex kg prob LMi")
output<-merge(dump, output, by="postal")

lmoran <- localmoran(s$`export l`, listw, na.action=na.pass)
dump<-data.frame(s$`export l`,s$postal, lmoran[,1:5])
colnames(dump)<-c("ex l", "postal", "ex l LMi", "ex l eLMi", "ex l var LMi", "ex l z LMi", "ex l prob LMi")
output<-merge(dump, output, by="postal")

## Local Moran's measures - US imports by states
lmoran <- localmoran(s$shipments, listw, na.action=na.pass)
dump<-data.frame(s$shipments,s$postal, lmoran[,1:5])
colnames(dump)<-c("im shipments", "postal", "im ship LMi", "im ship eLMi", "im ship var LMi", "im ship z LMi", "im ship prob LMi")
output<-merge(dump, output, by="postal")

lmoran <- localmoran(s$kg, listw, na.action=na.pass)
dump<-data.frame(s$kg,s$postal, lmoran[,1:5])
colnames(dump)<-c("im kg", "postal", "im kg LMi", "im kg eLMi", "im kg var LMi", "im kg z LMi", "im kg prob LMi")
output<-merge(dump, output, by="postal")

lmoran <- localmoran(s$l, listw, na.action=na.pass)
dump<-data.frame(s$l,s$postal, lmoran[,1:5])
colnames(dump)<-c("im l", "postal", "im l LMi", "im l eLMi", "im l var LMi", "im l z LMi", "im l prob LMi")
output<-merge(dump, output, by="postal")

write.csv(output, file = "outputs/USlocal-morans.csv") # Output CSV of Local Moran's results. For each direction of trade (import and export) and for each unit of trade (kg, l, shipment) we have: the LM score, the estimated LM i, variance in LM, the z scores, and the probability (p values)


## Point Pattern Analysis
# To analyze any spatial clustering amongst the import sites, we conduct PPP using Ripley's K
# Ripley's K https://cran.r-project.org/web/packages/spatstat/vignettes/getstart.pdf
pattern<-ppp(demos$long, demos$lat, c(min(demos$long), max(demos$long)), c(min(demos$lat), max(demos$lat))) # Add lat/long to sites information # DEV: should do this before any outputting above
plot(pattern)
plot(envelope(pattern,Kest))



#### CORRELATES ####
# Pull out the demographic data we need (% minority and % in poverty at various geo scales for each US import facility) and re-enumerate it from 0-1 
d<-demos[6:14]
d<-d/100 # Re-enumerate the % to 0-1 (e.g. 75->.75)
d[is.na(d)]<-0 # Convert null values to 0
demos[6:14]<-d # Add converted data back to demographics set
#demos.forMDS<-demos # DEV: create a copy of the demographic data for multidimensional scaling

## Create a waste type matrix
# Here we create a matrix describing each US importer's waste profile in terms of shipments by type. Each row is an importer and each column is a unique waste type (either by "waste description" - less standardized - or UN Code - more standardized)
# Process the data
dump<-data.frame(table(imports$Receiving.Facility.EPA.ID.Number.2, imports$Hazardous.Waste.Description)) # The Hazardous Waste Description column is how waste managers described the waste. It is not particularly standardized. The imports$DOT.UN.ID.Code variable is the UN Code managers imputed to the waste and is more standardized (fewer unique types)
colnames(dump)<-c("site", "type", "count")
dump<-spread(dump, type, count)
site.list<-dump[1] # Separate and drop site ids to do normalization
dump <- dump[,-1]
dump<- data.frame(lapply(dump, function(x) scale(x, center = FALSE, scale = max(x, na.rm = TRUE)/1))) # Normalize the shipments by waste type data to a 0-1 scale #log scale each column?
dump<-cbind(site.list, dump) # Add sites' IDs back

# Add site demographic data to the waste type matrix
site<-data.frame(lapply(site, function(x) scale(x, center = FALSE, scale = max(x, na.rm = TRUE)/1))) # Normalize each site's overall shipments, kg, and l values to 0-1
site[is.na(site)]<-0 # Clear NAs and NaNs from site data
demos<-merge(demos, site, by.x="epa.id", by.y=0) # DEV: for MDS use ~demos.forMDS<-merge(demos.forMDS, site, by.x="epa.id", by.y=0)~
output<-merge(demos, dump, by.x="epa.id", by.y="site") # Merge scaled import data with demographic data #.forMDS

# Optionally add aggregated waste type data to the matrix. For example, summarize all wastes that include mercury into one type
toxics<-grep("LEAD|MERCURY", names(output)) # This variable is for managing specific kinds of waste types for analysis. In this case, we collect just the waste descriptions that include lead or mercury in them
data.hlm<-output[,c(6:18, toxics)] # ~data.hlm~ is a dummy variable
summed <- rowSums(data.hlm[, c(19:188)]) # The columns we keep (19-188) is very context specific to this selection of toxics and years (2011-2012)
summed<-data.frame(summed)
summed<- data.frame(lapply(summed, function(x) scale(x, center = FALSE, scale = max(x, na.rm = TRUE)/1))) # Normalize to 0-1 # DEV: log scale each column?
output<-cbind(summed, output) # ~output~ represents our final matrix: each row is a US importer. Columns are site IDs, scaled demographic data, and scaled import data (by unit and type)

## Hierarchical Linear Regression
# We are testing different models for explaining US importer shipments, kg, l, and shipments of specific wastes. Theoretically, these have to do with demographics, e.g. there is a correlation between how much a site imports and the rate of minority and/or impoverished neighbors of that site
# Shipments
ship.fit0 <- lm(shipments ~ 1, data = output)
ship.fit1 <- lm(shipments ~ threeMilePov, data=output) # A site's shipments as a function of the rate of poverty within a three mile radius of the importer
ship.fit2 <- lm(shipments ~ threeMilePov + threeMileRace, data=output) # Shipments as a function of the poverty rate and rate of non-white residents
ship.fit3 <- lm(shipments ~ threeMilePov + threeMileRace + nonComplianceRate.Last12QuartersAsOfMarch2018, data=output) # Shipments as a function of poverty, race, and a site's rate of noncompliance with relevant regulations 
anova.ship <- anova(ship.fit1, ship.fit2, ship.fit3)

# Kg
kg.fit0 <- lm(kg ~ 1, data = output)
kg.fit1 <- lm(kg ~ threeMilePov, data=output)
kg.fit2 <- lm(kg ~ threeMilePov + threeMileRace, data=output)
kg.fit3 <- lm(kg ~ threeMilePov + threeMileRace + nonComplianceRate.Last12QuartersAsOfMarch2018, data=output)
anova.kg <- anova(kg.fit1, kg.fit2, kg.fit3)

# L
l.fit0 <- lm(l ~ 1, data = output)
l.fit1 <- lm(l ~ threeMilePov, data=output)
l.fit2 <- lm(l ~ threeMilePov + threeMileRace, data=output)
l.fit3 <- lm(l ~ threeMilePov + threeMileRace + nonComplianceRate.Last12QuartersAsOfMarch2018, data=output)
anova.l <- anova(l.fit1, l.fit2, l.fit3)

# Toxics (selected toxics from above)
toxics.fit0 <- lm(summed ~ 1, data = output)
toxics.fit1 <- lm(summed ~ threeMilePov, data=output)
toxics.fit2 <- lm(summed ~ threeMilePov + threeMileRace, data=output)
toxics.fit3 <- lm(summed ~ threeMilePov + threeMileRace + nonComplianceRate.Last12QuartersAsOfMarch2018, data=output)
anova.toxics <- anova(toxics.fit1, toxics.fit2, toxics.fit3)

# Build output
# We build a structure for outputting our HLR results. We report the coefficients for each model
# DEV: To report p-values for each model, do summary(model)$coefficients[2,4] e.g. summary(ship.fit1)$coefficients[2,4]
# Below we output the ANOVA for the model combinations)
row1<-data.frame(ship.fit1$coefficients[2], kg.fit1$coefficients[2], l.fit1$coefficients[2], toxics.fit1$coefficients[2], ship.fit2$coefficients[2], kg.fit2$coefficients[2], l.fit2$coefficients[2], toxics.fit2$coefficients[2], ship.fit3$coefficients[2], kg.fit3$coefficients[2], l.fit3$coefficients[2], toxics.fit3$coefficients[2])
row2<-data.frame(NA,NA,NA,NA,ship.fit2$coefficients[3], kg.fit2$coefficients[3], l.fit2$coefficients[3], toxics.fit2$coefficients[3], ship.fit3$coefficients[2], kg.fit3$coefficients[3], l.fit3$coefficients[3], toxics.fit3$coefficients[3])
row3<-data.frame(NA,NA,NA,NA,NA,NA,NA,NA,ship.fit3$coefficients[4], kg.fit3$coefficients[4], l.fit3$coefficients[4], toxics.fit3$coefficients[4])
colnames(row1)<-c("MODEL1 - shipments", "MODEL1 - kg", "MODEL1 - l", "MODEL1 - toxic shipments","MODEL2 - shipments", "MODEL2 - kg", "MODEL2 - l", "MODEL2 - toxic shipments","MODEL3 - shipments", "MODEL3 - kg", "MODEL3 - l", "MODEL3 - toxic shipments")
colnames(row2)<-c("MODEL1 - shipments", "MODEL1 - kg", "MODEL1 - l", "MODEL1 - toxic shipments","MODEL2 - shipments", "MODEL2 - kg", "MODEL2 - l", "MODEL2 - toxic shipments","MODEL3 - shipments", "MODEL3 - kg", "MODEL3 - l", "MODEL3 - toxic shipments")
colnames(row3)<-c("MODEL1 - shipments", "MODEL1 - kg", "MODEL1 - l", "MODEL1 - toxic shipments","MODEL2 - shipments", "MODEL2 - kg", "MODEL2 - l", "MODEL2 - toxic shipments","MODEL3 - shipments", "MODEL3 - kg", "MODEL3 - l", "MODEL3 - toxic shipments")

hlr<-rbind(row1, row2, row3)

write.csv(hlr, file = "outputs/hlr.csv")

# Print out ANOVA for the various HLR models
sink("outputs/anova.txt")
print(anova.ship)
print(anova.kg)
print(anova.l)
print(anova.toxics)
sink()

#DEV: loop through other potential combinations of demographic variables.
# Multiscalar HLR example
fit0 <- lm(shipments ~ 1, data = demos)
fit1 <- lm(shipments ~ threeMilePov, data=demos)
fit2 <- lm(shipments ~ threeMilePov + tractPov, data=demos)
fit3 <- lm(shipments ~ threeMilePov + tractPov + zipPov, data=demos)
fit4 <- lm(shipments ~ threeMilePov + tractPov + zipPov + statePov, data=demos)
# anova(fit1, fit2, fit3, fit4) # Print out the ANOVA for this multiscalar HLR example


## Multidimensional Scaling (MDS)
# In MDS, we aim to reduce the number of variables/columns (demographic data, waste types, etc.) needed to explain variation between instances (import sites)
# Prep data
output<-output[-c(1:6)] # Remove columns 1-5 (site IDs and summary stats) for MDS. Can also test removing most demographic data -  7:9, 11:13 - to focus on waste types
# DEV: test<-output[,c(6,10,14,16,17)] # Test this calculation using only demographic data and total shipments, kg, l

# MDS calculation
mds.result<-dist(output)
loc<-cmdscale(mds.result)
x<-loc[,1]
y<-loc[,2]
xy<-data.frame(loc)
xy<-xy+(abs(min(xy))+.001) # Add an insignificant constant to each data point to deal with negatives that can't be log transformed (for representation on plot)
p <- ggplot(xy, aes(x = X1, y = X2, label=demos$epa.id)) + geom_point() # + scale_x_continuous(trans='log2') + ylim(4,5) # ylim to zoom in on plot
p + geom_text(check_overlap = TRUE, size=3,hjust = 0, nudge_x = 0.05) # Scale the xaxis

## Hierarchical clustering
# Here we perform two kinds of hierarchical clustering in order to visualize how the data group together (i.e. how import sites are grouped together by similar waste and demographic profiles)
# DIANA approach
# DEV: outputHC<-output[-c(4,50),] # Remove outliers
hc<-diana(output) # ~outputHC~ for outliers
pltree(hc, cex = 0.6, hang=-1)

# AHC approach
hc1 <- hclust(mds.result, method = "complete" )
plot(hc1, cex = 0.6, hang = -1)

## K-menas clusters
# We can perform k-means clustering on the MDS results to group the data (import sites) by similar waste and demographic profiles
cl<-kmeans(loc,4) # We tell it how many clusters to find. Currently, 4
cols <- c("1" = "red", "2" = "blue", "3" = "darkgreen", "4" = "orange")
p + scale_x_continuous(trans='log2') + geom_point(aes(color = factor(cl$cluster)),size=3) + scale_colour_manual(values = cols)
# plot(loc, col = cl$cluster, xlab = "", ylab = "", asp = 1, axes = FALSE)
# text(loc, pos=4,offset=.5,cex=.5)

output<-merge(data.frame(cl$cluster), output, by.x=0, by.y=0) # Merge cluster results with our matrix
# DEV: add basic site info (EPA id) back in
write.csv(output, file = "outputs/MDS_raw.csv") # Output entire matrix