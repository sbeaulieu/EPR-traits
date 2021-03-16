# script to query OBIS given a geometry polygon around a known position
# Stace Beaulieu 2021-03-16

# install.packages("robis")
# https://iobis.github.io/robis/articles/getting-started.html
# appears to require package called 'fansi'
# install.packages("fansi")

setwd("C:/Users/sbeaulieu/Desktop")
library(robis) # used to query OBIS API
library(dplyr) # used to filter occurrences

# determine a position from vents database, then build a bounding box around it
# note there may also be some useful tools in https://github.com/iobis/obistools
# I think possible to do a radius
# Vents Database Ver. 3.4 Name.ID "Snail" 12.9533,143.62
checklist_Snail_all_dates <- checklist("Animalia", geometry = "POLYGON ((143.6700 13.0000, 143.6700 12.9000, 143.5700 12.9000, 143.5700 13.0000, 143.6700 13.0000))")
# by specifying "Animalia" this will not hit on foram or ciliate
#geometry: a wkt geometry string; numbers are coordinates of bounds of polygon, last point matches first
# can't figure out how to use startdate or enddate

write.csv(checklist_Snail_all_dates, file="SnailVent_robis_checklist_YYYYMMDD.csv")
#Adjust "Date" to relevant date

occurrences_Snail_all_dates <- occurrence("Animalia", geometry = "POLYGON ((143.6700 13.0000, 143.6700 12.9000, 143.5700 12.9000, 143.5700 13.0000, 143.6700 13.0000))")
# by specifying "Animalia" this will not hit on foram or ciliate
# unique taxa should equal the output from checklist for all dates
unique(occurrences_Snail_all_dates$scientificNameID)
# good, 42
write.csv(occurrences_Snail_all_dates, file="SnailVent_robis_occurrences_YYYYMMDD.csv")
#Adjust "Date" to relevant date

# date_year appears to be populated from eventDate and is easy to filter
occurrences_Snail_2010 <- filter(occurrences_Snail_all_dates, date_year == "2010")
occurrences_Snail_2014 <- filter(occurrences_Snail_all_dates, date_year == "2014")
# so far occurrences in 2010 from JAMSTEC and from 2014 from Mullineaux lab

