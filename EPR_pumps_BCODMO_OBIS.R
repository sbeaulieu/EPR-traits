#EPR_pumps_BCODMO_OBIS
#Stace Beaulieu
#2025-07-21

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
library(geosphere)
library(anytime) # for eventDate

#Load datasheets downloaded from Google Drive

# first manually confirm counts in WORKING_COPY match Susan's most recent version
# note need to fill blanks with zeroes for recent samples
counts_input <- readxl::read_xlsx("Pump_near_bottom_compilation_WORKING_COPY_20250721.xlsx", sheet = "Composite_Actual_Numbers", skip = 1)
counts_input <- select(counts_input,-"...1")
# consider stripping off underscore eventID here

taxa_input <- readxl::read_xlsx("Pump_near_bottom_compilation_WORKING_COPY_20250721.xlsx", sheet = "taxa")

vent_input <- readxl::read_xlsx("Pump_near_bottom_compilation_WORKING_COPY_20250721.xlsx", sheet = "vent_site_locations", skip = 1, col_types = "text")
vent_input <- vent_input[,1:4] # keep only leftmost columns
vent_input <- dplyr::slice_head(vent_input, n = 15) # keep only topmost rows
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

# will also need to read in tab depth_when_Direction_off
# for those locations that were calculated below

# initiate event table with top rows Composite sheet
# ultimately for BCO-DMO join separately to counts_long using eventID
event_metadata <- dplyr::slice_head(counts_input,n = 8)

# transpose for event table
event_t <- data.table::transpose(event_metadata, keep.names = "eventID", make.names="All near bottom, including off-axis")
# strip off the trailing underscore for some eventID
event_t$eventID <- sub("_$", "", event_t$eventID)


# rename most columns to Darwin Core
event_dwc <- event_t %>%
  rename(verbatimEventDate = "Date Recovered") %>%
  rename(locality = "Location") %>%
  rename(sampleSizeValue = "Liters Pumped")
event_dwc$sampleSizeUnit = "litre"
event_dwc$"Distance off axis in meters" <- as.integer(event_dwc$"Distance off axis in meters")
event_dwc$"Distance off site in meters" <- as.integer(event_dwc$"Distance off site in meters")
event_dwc <- event_dwc %>%
  unite(locationRemarks, c("Height above bottom in meters", "Distance off axis in meters", "Distance off site in meters", "Direction off axis or off site"), remove = FALSE)
#event_dwc$eventDate <- as.Date(event_dwc$verbatimEventDate, tryFormats = "%d/%b/%Y") # but multiple date formats
event_dwc$eventDate <- anydate(event_dwc$verbatimEventDate)
event_dwc$`Height above bottom in meters` <- as.integer(event_dwc$`Height above bottom in meters`)

# the join is by named vent to position and depth so will need to replace values
# if not NA in "Direction off axis or off site"
event_dwc_vent <- left_join(event_dwc, vent_input, by = "locality")

# calculate DwC min max depth
event_dwc_vent$minimumDepthInMeters <- event_dwc_vent$Bottom_Depth - event_dwc_vent$`Height above bottom in meters` - 5
event_dwc_vent$maximumDepthInMeters <- event_dwc_vent$Bottom_Depth - event_dwc_vent$`Height above bottom in meters` + 5
# for those off site will need tab depth_when_Direction_off for depth
# temporary fill with NA
event_dwc_vent$minimumDepthInMeters[!is.na(event_dwc_vent$`Direction off axis or off site`)] <- NA
event_dwc_vent$maximumDepthInMeters[!is.na(event_dwc_vent$`Direction off axis or off site`)] <- NA

# in order to use destPoint function from geosphere need bearing in degrees
df <- dplyr::tribble(
  ~cardinal_direction, ~degrees,
  "N",                 0,                    
  "NNE",               22.5,                   
  "NE",                45,                     
  "ENE",               67.5,                   
  "E",                 90,                     
  "ESE",               112.5,                  
  "SE",                135,                    
  "SSE",               157.5,                  
  "S",                 180,                    
  "SSW",               202.5,                  
  "SW",                225,                    
  "WSW",               247.5,                  
  "W",                 270,                    
  "WNW",               292.5,                  
  "NW",                315,                    
  "NNW",               337.5
)

event_dwc_vent <- rename(event_dwc_vent, "cardinal_direction" = "Direction off axis or off site")
event_dwc_vent <- left_join(event_dwc_vent, df, by = "cardinal_direction")
event_dwc_vent$distance_off <- ifelse(event_dwc_vent$`Distance off axis in meters` > 9, event_dwc_vent$`Distance off axis in meters`, event_dwc_vent$`Distance off site in meters`)

p = cbind(event_dwc_vent$decimalLongitude, event_dwc_vent$decimalLatitude)
b = event_dwc_vent$degrees
d = event_dwc_vent$distance_off
result = destPoint(p, b, d, a=6378137, f=1/298.257223563)
# returns NAs and NaNs
lon_lat_new <- as.data.frame(result)
# column bind then export csv to check in QIS
event_lon_lat_new <- bind_cols(event_dwc_vent, lon_lat_new)
# write.csv(event_lon_lat_new, 'event_lon_lat_new.csv') # confirm use of geosphere destPoint

# next need to replace position and depth columns
# for the when_Direction_off

# initiate occurrence table
counts <- slice(counts_input, 10:n())
colnames(counts)[1] <- "verbatimIdentification"
# need to exclude rows with totals or NAs
counts <- counts %>%
  filter(!is.na(verbatimIdentification)) %>%
  filter(!verbatimIdentification %in% c('Totals','Totals excluding unknown 9660'))

# counts should be integer
# add column with row counter to be used as suffix for occurrenceID
counts <- counts %>% mutate(vIrow = row_number())

# join with WoRMS taxa
taxa <- select(taxa_input, verbatimIdentification, scientificName, scientificNameID, kingdom) # add taxonRank
counts_taxa <- full_join(counts, taxa)

#Pivoting Longer
counts_long <- counts_taxa %>%
  pivot_longer(
    cols = 2:82, # new column vIrow was added to far right then taxa joined far right
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
