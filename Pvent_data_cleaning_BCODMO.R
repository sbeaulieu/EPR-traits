# R script to clean EPR colonists data for submitting to BCO-DMO
# these new counts data will become the 2nd version of https://www.bco-dmo.org/dataset/733173
# includes updating colonization sample log to 2nd version of https://www.bco-dmo.org/dataset/733210
# Stace Beaulieu on 2020-08-18 we realized we needed to change monthsSinceEruption 108 to 106
# 
# Input files:
#    - Pvent_P&S_135_supptable1_submit.xlsx Lauren Mullineaux's original data EXCEL
#      (needs 1 spelling correction, also need to change 108 to 106)
#    - Pvent_P&S_135_supptable1_submit_AphiaIDs.csv matching of dataProviderName to WoRMS Aphia ID
#      (needs 1 correction)
#
# Output files:
#    - Pvent_P&S_135_supptable1_submit_merged_wide.csv (for new EPR colonists data): write mergedwide to csv to represent the original wide format with single row column headers
#      with 1 dataProviderName correction and 1 AphiaID correction
#      Note this file needs the six columns with 108 to be replaced with 106
#    - Pvent_P&S_135_supptable1_submit_merged_long_106.csv
#    - I also update existing BCO-DMO dataset for sample locations [Colonization sampler log https://www.bco-dmo.org/dataset/733210]
#
# Recommendations for long format data table:
#    - use the merged wide to create a long format file aligned to Darwin Core for harvest by OBIS
#      see details in comments below
#
library(readxl)
library(readr)
library(dplyr)
setwd("C:/Users/sbeaulieu/Desktop/EPR_traits_AT_SEA")

# grab top part of EXCEL file to concatenate column headers for wide format (which will become eventIDs)
toppart <- read_excel("Pvent_P&S_135_supptable1_submit.xlsx", range = "A1:BA4", col_names = FALSE)

# make the column names first
# https://stackoverflow.com/questions/35157307/concatenate-values-across-two-rows-in-r
row1 <- as.character(toppart[1,])
row2 <- as.character(toppart[2,])
row3 <- as.character(toppart[3,])
# if you read in row4 as character it reads in way past decimal
row4 <- as.numeric(toppart[4,])
newtoppart <- rbind(row1,row2,row3,row4)
cnames = apply(newtoppart, 2, paste0, collapse = "-")
# make leftmost column dataProviderName
cnames[1] <- "dataProviderName"
# confirm unique
unique(cnames)

# grab wide data from EXCEL file, read in except first 5 rows
# https://readxl.tidyverse.org/articles/articles/multiple-header-rows.html
widedata <- read_excel("Pvent_P&S_135_supptable1_submit.xlsx", skip = 5, col_names = cnames)

# fix error in 1 dataProviderName
# correct "polycheates, juv" to "polychaetes, juv" in Pvent_P&S_135_supptable1_submit.xlsx
# it is risky to use the row numbers but I can't figure out right now with mutate and replace
if (widedata$dataProviderName[48] == "polycheates, juv") {widedata$dataProviderName[48] <- "polychaetes, juv"}

# add machine-readable taxonomic identifiers
# merge on dataProviderName and make sure the merged data table still has 68 obs.
# Full join to keep all rows from both data frames
identifiers <- read_csv("Pvent_P&S_135_supptable1_submit_AphiaIDs.csv")

identifiers <- rename(identifiers, dataProviderName = dataProvider_Name)

# *Laminatubus alvini group may include small Protis hydrothermica
# I HAVE TO CORRECT THE TAXON TABLE, KEEP THE ASTERISK
# correct "Laminatubus alvini" to "*Laminatubus alvini"
# GO UP TO LEVEL THAT INCLUDES BOTH Serpulidae (Family) urn:lsid:marinespecies.org:taxname:988
if (identifiers$dataProviderName[37] == "Laminatubus alvini") {
  identifiers$dataProviderName[37] <- "*Laminatubus alvini"
  identifiers$scientificName[37] <- "Serpulidae"
  identifiers$scientificNameID[37] <- "urn:lsid:marinespecies.org:taxname:988"
  identifiers$AphiaID[37] <- "988"
}

# merge the identifiers with the counts per taxon per sample
mergedwide <- full_join(identifiers, widedata, by = "dataProviderName")
nrow(mergedwide) # ensure still 68 rows

# write widedata to csv to represent the original wide format with 2 corrections and single row column headers
write.csv(mergedwide, "Pvent_P&S_135_supptable1_submit_merged_wide.csv")

# Note I put the output csv into https://www.marinespecies.org/aphia.php?p=match to confirm AphiaIDs

# Recommendations for long format data table:
# 2019-12-19 Amber provided abundances.csv in long format that keeps zeros
# With the corrected wide file, now can output the corrected long format,
# change her column headers:
#   Identification to dataProviderName,
#   Date to monthsSinceEruption,
#   Zone OK,
#   ID to samplerID,
#   Recovery_T to temperatureRecovered
#   abundance to individualCount
# next can add the following columns for harvest by OBIS:
#   eventID from wide cnames,
#   occurrenceID concatenate eventID with dataProviderName,
#   occurrenceStatus present if > 0 or absent if = 0
#   basisOfRecord = PreservedSpecimen

wide <- read_csv("c:/Users/sbeaulieu/Desktop/EPR_traits_AT_SEA/Pvent_P&S_135_supptable1_submit_merged_wide.csv")
# use tidyr pivot_longer
# replace original column name with individualCount
# - X1 -dataProviderName -scientificName -scientificNameID -AphiaID
# -c("X1", "dataProviderName", "scientificName", "scientificNameID", "AphiaID")
library(tidyr)
long <- wide %>% pivot_longer(cols = "9-H-2-26.7":"Pre-C-199-2", names_to = "eventID", values_to = "individualCount")
# use tidyr separate with hyphen to new columns monthsSinceEruption, zone, samplerID, temperatureRecovered
into = c("monthsSinceEruption", "zone", "samplerID", "temperatureRecovered")
longplus <- long %>% separate(eventID, into, sep = "-", remove = FALSE)
# use dplyr if monthsSinceEruption is 108 then change to 106
library(dplyr)
correctlongplus <- longplus %>% mutate(monthsSinceEruption = replace(monthsSinceEruption, monthsSinceEruption == "108", "106"))
# delete extraneous row numbering column
correctlongplus <- select(correctlongplus,-X1)
write_csv(correctlongplus, "c:/Users/sbeaulieu/Desktop/EPR_traits_AT_SEA/Pvent_P&S_135_supptable1_submit_merged_long_106.csv")


#########################################################
# update an existing BCO-DMO dataset for sample locations

library(readr)
library(dplyr)

# load dataset Colonization sampler log from BCO-DMO ERDDAP
# Mullineaux, L. (2018) Dates and locations of colonization sampler deployments and recoveries from East Pacific Rise (EPR) deep-sea vents, 2006-2017. Biological and Chemical Oceanography Data Management Office (BCO-DMO). Dataset version 2018-04-11. doi:10.1575/1912/bco-dmo.733210.1 [access date]
locations <- read_csv("https://erddap.bco-dmo.org/erddap/tabledap/bcodmo_dataset_733210.csvp")

# make sure those dates are characters or can't rbind
locations[] <- lapply(locations, as.character)

# add row for pre-eruption East Wall sample location
cruise1998list <- list("AT0319","Atlantis","AT0319","1998-05-10","1998-06-01","Lauren Mullineaux","Alvin","1995-04-05","1998-05-15","East Pacific Rise 9 50 N hydrothermal vent field",9.8421,-104.2919,"East Wall",NA,NA)
cruise1998<-data.frame(cruise1998list, stringsAsFactors = FALSE)
cruise1998 <- setNames(cruise1998, names(locations))
locations <- rbind(cruise1998, locations)

# create key to match Date (top row) from new data "Pvent_P&S_135_supptable1_submit.xlsx"
# could build this from unique(row1) in code above if insert NA for Atalante cruise
# plus add or rename columns to be able to provide occurrences to OBIS

monthsSinceEruption <- list("Pre",9,11,22,33,NA,96,108,135) # note type list leads to problems with output to csv
# on 2020-08-18 we realized we needed to change monthsSinceEruption 108 to 106
# see below

locations <- mutate(locations,
                    monthsSinceEruption = monthsSinceEruption,
                    coordinateUncertaintyInMeters = 50,  # OBIS optional DwC term
                    minimumDepthInMeters = 2500,         # OBIS optional DwC term
                    maximumDepthInMeters = 2510          # OBIS optional DwC term
)

locations$monthsSinceEruption <- as.character(locations$monthsSinceEruption)

locations <- rename(locations,
                    eventDate = `End (unitless)`,                  # OBIS required DwC term
                    decimalLatitude = `latitude (degrees_north)`,  # OBIS required DwC term
                    decimalLongitude = `longitude (degrees_east)`  # OBIS required DwC term
)
# update location to East Pacific Rise 9 50'N hydrothermal vent field
locations$`Location (unitless)`<- "East Pacific Rise 9 50 N hydrothermal vent field"

write_csv(locations, "c:/Users/sbeaulieu/Desktop/EPR_traits_AT_SEA/bcodmo_dataset_733210_update.csv")
# next we read in that file to replace the 108 with 106 and edit the output filename to provide to BCO-DMO
# needs readr and dplyr
incorrect <- read_csv("c:/Users/sbeaulieu/Desktop/EPR_traits_AT_SEA/bcodmo_dataset_733210_update.csv")
correct <- incorrect %>% mutate(monthsSinceEruption = replace(monthsSinceEruption, monthsSinceEruption == "108", "106"))
write_csv(correct, "c:/Users/sbeaulieu/Desktop/EPR_traits_AT_SEA/bcodmo_dataset_733210_update_106.csv")
