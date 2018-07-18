# hazardous-waste-data-processing

Python scripts for integrating CSVs related to hazardous waste data. Learn more about the project at [geography.wisc.edu/hazardouswaste](http://www.geography.wisc.edu/hazardouswaste)

Input data is variously in gephi.csv or data.csv (both of which are deprecated as of 3/29/18)

- `scripts.R` - processes hazardous waste dataset through various statistical analyses
- `gephi.py` - takes the hazardous waste dataset and produces exporter/importer company pairs for social network analysis
- `lookup.py` - searches the data for shipments of hazardous waste descriptions matching those of a user-entered search term
- `manifestCheck.py` - checks whether any of the original PDFs contained the same manifest
- `fileCheck.py` - checks the files that have been processed
- `manifests.py` - takes filenames of all PDFs and matches them with EPA ID#s
- `JSONbuilder.py` - builds a JSON file out of importer/exporter pairs, for visualization purposes
