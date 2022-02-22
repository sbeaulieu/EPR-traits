# Mariana 2010 pumps matched to World Register of Marine Species for submitting data to BCO-DMO
# Stace Beaulieu
# 2022-02-22
# 
# Input files:
#  Mariana_2010_pumps_WORKING-COPY_20220221.xlsx downloaded from Google sheet
#  Mariana_providedname_20220212_matched.txt output from WoRMS Taxon Match GUI
# Output file:
#  Mariana_larval_counts_BCODMO.csv

###Import libraries
library(readxl)
library(dplyr) # for select and join
library(magrittr) # for pipe
library(readr) # for write_csv

###Import data
# input files in working directory

larval_counts_input <- read_excel("Mariana_2010_pumps_WORKING-COPY_20220221.xlsx", sheet = "larval_counts", skip=8)
# Skip specifies the number of rows to be skipped before reading

WoRMS_input <- read.delim("Mariana_providedname_20220212_matched.txt")
# using base R
WoRMS_input <- WoRMS_input %>% mutate_all(na_if,"")
# change blanks to NAs

###Exclude some columns before joining
larval_counts <- select(larval_counts_input,-"Size Fraction",-"...9",-"...10",-"Total...29", -"Total...30", -"Total...31", -"Total...32", -"Total...33", -"Total...34")

WoRMS <- WoRMS_input %>% select("ScientificName","LSID","Kingdom","Phylum","Class","Order","Family","Genus","Species")
# providedname is what I provided to the WoRMS Taxon Match tool, and
# ScientificName is what was output from that tool matched to LSID
# all providedname were exact matches to ScientificName

###Inspect tables
summary(larval_counts) # class character due to taxa with presence absence
summary(WoRMS)

###Join tables
larval_counts_WoRMS <- cbind(larval_counts, WoRMS)
# can't use join per se due to repeated rows in the WoRMS table
# retain WoRMS ScientifcName to be able to test exact match
larval_counts_WoRMS$boolean <- with(larval_counts_WoRMS, scientificName == ScientificName)

###Inspect the boolean for equivalence

###Move a column, exclude a column
# the data product for BCODMO will use the camel case Darwin Core term scientificName
Mariana_larval_counts_BCODMO <- larval_counts_WoRMS %>% select(-ScientificName,-boolean) %>%
  relocate(LSID, .before = taxonRank)

###Output file
write_csv(Mariana_larval_counts_BCODMO, "c:/Users/sbeaulieu/Desktop/Mariana_larval_counts_BCODMO.csv") # give this a date when generated

