#EPR_pumps_BCODMO_OBIS
#Stace Beaulieu
#2025-04-19

# R script to standardize EPR pumps Composite data to Darwin Core (DwC)
# and output tables for BCO-DMO (single table with occurrence extension left and event Core right)
# and OBIS (event Core table and occurrence extension table)

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

taxa_input <- readxl::read_xlsx("Pump_near_bottom_compilation_WORKING_COPY_20250407.xlsx", sheet = "taxa")

vent_input <- readxl::read_xlsx("Pump_near_bottom_compilation_WORKING_COPY_20250407.xlsx", sheet = "vent_site_locations", skip = 1, col_types = "text")
vent_input <- vent_input[,1:4] # keep only leftmost columns
vent_input <- dplyr::slice_head(vent_input, n = 14) # keep only topmost rows
# needed to read in as text to be able to provide lat lon to 5 decimal places
vent_input$decimalLatitude <- as.numeric(vent_input$decimalLatitude)
vent_input$decimalLongitude <- as.numeric(vent_input$decimalLongitude)
vent_input$decimalLatitude <- round(vent_input$decimalLatitude, 5)
vent_input$decimalLongitude <- round(vent_input$decimalLongitude, 5)
vent_input <- vent_input %>%
  rename(locality = "Location N to S") %>%
  rename(Bottom_Depth = "Bottom_Depth_Meters from 20210618 published merged bathy product except for K and K/PBR")
vent_input$Bottom_Depth <- as.integer(vent_input$Bottom_Depth)
# write.csv(vent_input, 'vent_input.csv') # confirm 5 decimal places


# initiate event table with top rows Composite sheet
# ultimately for BCO-DMO join separately to counts_long using eventID
event_metadata <- dplyr::slice_head(counts_input,n = 8)

# transpose for event table
event_t <- data.table::transpose(event_metadata, keep.names = "eventID", make.names="All near bottom, including off-axis")
# strip off the trailing underscore for some eventID
event_t$eventID <- sub("_$", "", event_t$eventID)


# rename columns to Darwin Core
event_dwc <- event_t %>%
  rename(verbatimEventDate = "Date Deployed") %>%
  rename(locality = "Location") %>%
  rename(sampleSizeValue = "Liters Pumped")
event_dwc$sampleSizeUnit = "litre"
event_dwc$"Distance off axis in meters" <- as.integer(event_dwc$"Distance off axis in meters")
event_dwc$"Distance off site in meters" <- as.integer(event_dwc$"Distance off site in meters")
event_dwc <- event_dwc %>%
  unite(locationRemarks, c("Height above bottom in meters", "Distance off axis in meters", "Distance off site in meters", "Direction off axis or off site"), remove = FALSE)
event_dwc$eventDate <- as.Date(event_dwc$verbatimEventDate, tryFormats = "%d/%b/%Y")

# not DwC
# "Cruise number"

# the join is by named vent to position and depth so will need to replace values
# if not NA in "Direction off axis or off site"
event_dwc_vent <- left_join(event_dwc, vent_input, by = "locality")

# initiate occurrence table
counts <- slice(counts_input, 10:n())
colnames(counts)[1] <- "verbatimIdentification"
# need to exclude rows with totals or NAs
counts <- counts %>%
  filter(!is.na(verbatimIdentification)) %>%
  filter(!verbatimIdentification %in% c('Totals','Totals excluding unknown 9660'))

# add column with row counter to be used as suffix for occurrenceID
counts <- counts %>% mutate(vIrow = row_number())

# join with WoRMS taxa
taxa <- select(taxa_input, verbatimIdentification, scientificName, scientificNameID, kingdom) # add taxonRank
counts_taxa <- full_join(counts, taxa)

#Pivoting Longer
counts_long <- counts_taxa %>%
  pivot_longer(
    cols = 2:66, # new column vIrow was added to far right then taxa joined far right
    names_to = "eventID",
    values_to = "individualCount"
  )
# strip off the trailing underscore for some eventID
counts_long$eventID <- sub("_$", "", counts_long$eventID)

# add required terms for DwC Occurrence extension table
# since this will be Event core with Occurrence extension the eventDate and position can just be in event table
occurrence_dwc <- counts_long %>%
  unite(occurrenceID, c("eventID", "vIrow"), remove = FALSE)
occurrence_dwc$basisOfRecord <- "PreservedSpecimen"
# earlier samples: remove NAs because only declaring present for occurrenceStatus
# more recent samples: can declare absent if 0
occurrence_dwc <- filter(occurrence_dwc, !is.na(individualCount))
occurrence_dwc$individualCount <- as.integer(occurrence_dwc$individualCount)

occurrence_dwc <- occurrence_dwc %>% 
  mutate(occurrenceStatus = case_when(
    individualCount == 0 ~ "absent",
    individualCount != 0 ~ "present"
    ))

# drop vIrow and order the columns
occurrence_dwc <- select(occurrence_dwc, -vIrow)
col_order <- c("verbatimIdentification", "scientificName", "scientificNameID",
               "kingdom", "individualCount", "occurrenceStatus",
               "basisOfRecord", "occurrenceID", "eventID")
occurrence <- occurrence_dwc[, col_order]
# manually confirm total counts for each verbatimIdentification against Composite_Actual_Numbers sheet
aggregate(occurrence$individualCount, by=list(verbatimIdentification=occurrence$verbatimIdentification), FUN=sum)
