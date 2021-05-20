# script to query OBIS given a geometry polygon around a known position
# Stace Beaulieu, Bethany Fleming Modified 2021-05-20

# install.packages("robis")
# https://iobis.github.io/robis/articles/getting-started.html
# appears to require package called 'fansi'
# install.packages("fansi")

setwd("C:/Users/sbeaulieu/Desktop")
library(robis) # used to query OBIS API
library(dplyr) # used to filter occurrences

# Vents Database Ver. 3.4 Name.ID "Snail" 12.9533,143.62

checklist_Snail_all_dates <- checklist("Animalia", geometry = "POLYGON ((143.6700 13.0000, 143.6700 12.9000, 143.5700 12.9000, 143.5700 13.0000, 143.6700 13.0000))")
# by specifying "Animalia" this will not hit on foram or ciliate
#geometry: a wkt geometry string; numbers are coordinates of bounds of polygon, last point matches first
# can't figure out how to use startdate or enddate
# 2021-05-20 has 42 taxa

# write.csv(checklist_Snail_all_dates, file="SnailVent_robis_checklist_YYYYMMDD.csv")
#Adjust "Date" to relevant date

occurrences_Snail_all_dates <- occurrence("Animalia", geometry = "POLYGON ((143.6700 13.0000, 143.6700 12.9000, 143.5700 12.9000, 143.5700 13.0000, 143.6700 13.0000))")

# by specifying "Animalia" this will not hit on foram or ciliate
# unique taxa should equal the output from checklist for all dates
unique(occurrences_Snail_all_dates$scientificNameID)
# 2021-05-20 confirm 42 taxa
# write.csv(occurrences_Snail_all_dates, file="SnailVent_robis_occurrences_YYYYMMDD.csv")
#Adjust "Date" to relevant date


# date_year appears to be populated from eventDate and is easy to filter
occurrences_Snail_2010 <- filter(occurrences_Snail_all_dates, date_year == "2010")
occurrences_Snail_2014 <- filter(occurrences_Snail_all_dates, date_year == "2014")
# 2021-05-20 occurrences in 2010 from JAMSTEC and from 2014 from Mullineaux lab

# "TOTO Caldera","Nakayama Field, Toto Caldera",12.7167,143.5333
checklist_TOTO_all_dates <- checklist("Animalia", geometry = "POLYGON ((143.5433 12.7267, 143.5433 12.7067, 143.5233 12.7067, 143.5233 12.7267, 143.5433 12.7267))")
# 2021-05-20 hits only Calyptogena
occurrences_TOTO_all_dates <- occurrence("Animalia", geometry = "POLYGON ((143.5433 12.7267, 143.5433 12.7067, 143.5233 12.7067, 143.5233 12.7267, 143.5433 12.7267))")
# 2021-05-20 1 record from JAMSTEC Shinkai dive 773 in 2003

# "EPR, 9 30'N",9.5167,-104.2414
# 2021-03-21 did not hit any records with only ~1 km so increased box
# checklist_EPR930_all_dates <- checklist("Animalia", geometry = "POLYGON ((-104.2514 9.5267, -104.2514 9.5067, -104.2314 9.5067, -104.2314 9.5267, -104.2514 9.5267))")
checklist_EPR930_all_dates <- checklist("Animalia", geometry = "POLYGON ((-104.2614 9.5367, -104.2614 9.4967, -104.2214 9.4967, -104.2214 9.5367, -104.2614 9.5367))")
# 2021-05-20 only Aphotopontius 
occurrences_EPR930_all_dates <- occurrence("Animalia", geometry = "POLYGON ((-104.2614 9.5367, -104.2614 9.4967, -104.2214 9.4967, -104.2214 9.5367, -104.2614 9.5367))")
# 2021-05-20 2 records, one may be from the publication describing the taxon with locality incorrect, and the other may be a re-publication in a database

# determine a position from vents database, then build a bounding box around it
# 1km is roughly 0.009 degrees latitude
km1 <- 0.009
# note there may also be some useful tools in https://github.com/iobis/obistools
# I think possible to do a radius

# Vents Database Ver. 3.4 Name.ID "Snail" 12.9533,143.62
dlatSn <- 12.9533
dlonSn <- 143.62

# assigning variables in the POLYGON 
E <- dlonSn+km1
W <- dlonSn-km1
N <- dlatSn+km1
S <- dlatSn-km1

###Using paste() after geometry = allows you to use the above variables
###Need to put quotation marks around the parts of the statement that are not the variables e.g. "," to put commas between the coordinates.
###Each bit within the paste() statement needs to be separated by a comma
# note the order of the corners of the polygon matters!!
occurrences_Snail_all_dates_ordered <- occurrence("Animalia", geometry = paste("POLYGON ((", W, N, ",", W, S , ",", E, S, ",", E, N, ",", W, N,"))"))
# note the order of the corners of the polygon mattered for retrieval of records
confirm_subset <- full_join(occurrences_Snail_all_dates, occurrences_Snail_all_dates_ordered, by = "occurrenceID")
# should be subset in smaller area queried

# Vents Database Ver. 3.4 "EPR, 9 50'N",9.8300,-104.2900
dlatEPR950 <- 9.8300
dlonEPR950 <- -104.2900

# assigning variables in the POLYGON, use 2km
E <- dlonEPR950+km1*2
W <- dlonEPR950-km1*2
N <- dlatEPR950+km1*2
S <- dlatEPR950-km1*2

occurrences_EPR950_all_dates_ordered <- occurrence("Animalia", geometry = paste("POLYGON ((", W, N, ",", W, S , ",", E, S, ",", E, N, ",", W, N,"))"))
# 2021-05-20 retrieves our records from BCODMO OBIS dataset


