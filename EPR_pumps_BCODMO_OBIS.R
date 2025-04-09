#EPR_pumps_BCODMO_OBIS
#Stace Beaulieu
#2025-04-08

# R script to standardize EPR pumps Composite data to Darwin Core
# and output tables for BCO-DMO (occurrence table with occurrences left and events right)
# and OBIS (event table and occurrence table)

#Setting wd

setwd("/Users/sbeaulieu/Downloads")

#Loading in the packages

library(readxl)
library(dplyr)
library(data.table)
library(tidyr)

#Load datasheets downloaded from Google Drive

counts_input <- readxl::read_xlsx("Pump_near_bottom_compilation_WORKING_COPY_20250407.xlsx", sheet = "Composite_Actual_Numbers", skip = 1)
counts_input <- select(counts_input,-"...1")

# initiate event table with top rows Composite sheet
# ultimately for BCO-DMO join separately to counts_long using eventID
event_metadata <- dplyr::slice_head(counts_input,n = 8)

# transpose for event table
event_t <- data.table::transpose(event_metadata, keep.names = "eventID", make.names="All near bottom, including off-axis")

# rename columns to Darwin Core
event_dwc <- event_t %>%
  rename(verbatimEventDate = "Date Deployed") %>%
  rename(locality = "Location") %>%
  rename(sampleSizeValue = "Liters Pumped")
event_dwc$sampleSizeUnit = "litre"
event_dwc$"Distance off axis in meters" <- as.integer(event_dwc$"Distance off axis in meters")
event_dwc$"Distance off site in meters" <- as.integer(event_dwc$"Distance off site in meters")
event_dwc <- event_dwc %>%
  unite(locationRemarks, c("Distance off axis in meters", "Distance off site in meters", "Direction off axis or off site"), remove = FALSE)

# not DwC
# "Height above bottom in meters"
# "Cruise number"

# initiate occurrence table
counts <- slice(counts_input, 10:n())
colnames(counts)[1] <- "verbatimIdentification"

# add column with row counter to be used as suffix for occurrenceID
counts <- counts %>% mutate(vIrow = row_number())

#Pivoting Longer
counts_long <- counts %>%
  pivot_longer(
    cols = 2:66, # new column vIrow was added to far right
    names_to = "eventID",
    values_to = "individualCount"
  )


