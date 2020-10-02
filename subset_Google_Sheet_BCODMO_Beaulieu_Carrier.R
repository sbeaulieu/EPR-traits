# R script to extract subset from Google Sheet for submission to BCO-DMO
# Deep-sea larval specimens prepared for Tyler Carrier's microbiome study
# Stace Beaulieu 2020-10-02

# Input file is read directly from Google Sheet
# Output file is comma separated text csv


# import libraries
library(tidyverse) # includes dplyr
library(googledrive)
library(googlesheets4)

# Google Sheet is the input
myurl <- "https://docs.google.com/spreadsheets/d/1JFbSLtNj13bYjw5NaaPqSwDMTRj8sm9F5iLo-mXzoOo/edit?usp=sharing" # url for googlesheet
gs4_auth() # manual authorization thru web browser
samples <- drive_get(myurl) # manual authorization thru web browser

gs4_get(samples) 
specimens <- range_read(samples, col_types = "ccccccnniiiccccccccccccccccc", skip = 3)
# remove last 2 rows
specimens <- specimens[1:60,]

# exclude the first and last 3 columns
subsetcolumns <- specimens[,2:25]
# exclude 4 columns Decimal.Minute, Similar Mariana polychaete specimen COI sequence Florence Pradillon, Similar Pescadero specimen BOLD COI, Tyler Carrier 28S results notes
subsetcolumns <- select(subsetcolumns, -Decimal.Minute, -starts_with("Similar"), -"Tyler Carrier 28S results notes")


# indicate with asterisk on TubeID the 6 specimens lost during preparation for sequencing
# corresponds to NA in column Tyler Carrier 16S microbiome results Dryad doi:10.5061/dryad.sqv9s4n18
# this is inefficient but works
subsetcolumns <- subsetcolumns %>% 
  mutate(TubeID = replace(TubeID, TubeID == "EG03", "EG03*"))
subsetcolumns <- subsetcolumns %>% 
  mutate(TubeID = replace(TubeID, TubeID == "EG05", "EG05*"))
subsetcolumns <- subsetcolumns %>% 
  mutate(TubeID = replace(TubeID, TubeID == "EG09", "EG09*"))
subsetcolumns <- subsetcolumns %>% 
  mutate(TubeID = replace(TubeID, TubeID == "EG16", "EG16*"))
subsetcolumns <- subsetcolumns %>% 
  mutate(TubeID = replace(TubeID, TubeID == "EG18", "EG18*"))
subsetcolumns <- subsetcolumns %>% 
  mutate(TubeID = replace(TubeID, TubeID == "EG20", "EG20*"))

# ensure the decimal lat and lon to 4 decimal places
# R rounds the 9.84015 down to 9.8401  
subsetcolumns$decimalLatitude = round(subsetcolumns$decimalLatitude, digits = 4)
subsetcolumns$decimalLongitude = round(subsetcolumns$decimalLongitude, digits = 4)

# output comma separated text csv
write.csv(subsetcolumns, "C:/Users/sbeaulieu/Desktop/BCODMO_Beaulieu_Carrier_submit.csv")
