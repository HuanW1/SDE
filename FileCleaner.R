
# FileCleaner.R
#
# script to perform data cleaning operations antecedent to the
# file split (FileSplitter.R)
#
# draft in progress
# 1. 12/11/20: wininger - processes 1-3 
# 2. 12/11/20: kleppinger - process 0 and collaborative edits
# 3. 12/14/20: wininger - processes 4+ 
# 4. 12/14/20: wininger - city county cleanup, etcetera
# 5. 12/15/20: kleppinger- testing code and updated splitting file
# 6. 12/19/20: wininger - completion of beta city-cleaning
# 7. 12/23/20: wininger - file-sourcing, edits to sources
# 8. 02/08/21: kleppinger - link to new source file and edit county and city and create new matching file
################################################################

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### WORKSPACE PREPARATION AND VARIABLE DECLARATIONS
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# clear all variables from workspace
#rm(list=ls(all=TRUE))

# load libraries
library(readxl)			# read_excel
library(dplyr)			# filter
library(stringr)		# str_to_title

# set working directory
setwd("/Users/KleppingerA/Documents/Nancy Barrett")
#setwd("/Users/wininger/desktop/docs/CTDPH/Alison")

######################################################################
#read only one file (no loops)
read_file="CELR_Feb04_Feb08.csv"

# read-in raw data from file
#data = read.delim(read_file, header = TRUE, stringsAsFactors = FALSE, quote = "")
#data=read.table(read_file, header = TRUE, sep=".")
data=read.csv(read_file)

############################################################################

# before cleaning counts!
# look for counts of counties
library(summarytools)
summarytools::freq(data$Patient_county, order = "freq")

library(summarytools)
summarytools::freq(data$Patient_city, order = "freq")

  # clean city (Nancy Barrett Process 7)
	source("_CityCleaner3.R")
	# yields four new columns at far-right of data-frame

	# process all other elements (Nancy Barrett all-other Processes)
	#source("_DataProcessor.R")

	# split dataset and write to file
  source("_FileSplitter.R")

#} # end-for on f [iteration through read-files]

###############################################################
