# script to query OBIS given a bounding box around a known position

# # CRAN
# install.packages("robis")
# also needed to install package "mapedit"

library(robis) # used to query OBIS API
library(dplyr) # used to subset data

# determine a position from vents database, then build a bounding box around it
# Vents Database Ver. 3.4 doi:10.1594/PANGAEA.917894 "EPR, 9 50'N",9.8300,-104.2900
dlatEPR950 <- 9.8300
dlonEPR950 <- -104.2900

# 1km is roughly 0.009 degrees latitude
km1 <- 0.009

# assigning variables in the POLYGON, use 2km
# note this is approximate for longitude near equator
E <- dlonEPR950+km1*2
W <- dlonEPR950-km1*2
N <- dlatEPR950+km1*2
S <- dlatEPR950-km1*2

occurrences_EPR950_2km <- robis::occurrence("Animalia", geometry = paste("POLYGON ((", W, N, ",", W, S , ",", E, S, ",", E, N, ",", W, N,"))"))

# subset by datasetID
unique(occurrences_EPR950_2km$datasetID)
subset_EPR950_2km <- dplyr::filter(occurrences_EPR950_2km, datasetID == "WHOI_EPR_DeepSeaVents_SpeciesCounts")
# note this is a subset of:
# https://mapper.obis.org/?datasetid=c37c6228-542c-45a2-8bef-009ee03672a8
# Mullineaux, L. (2020) Counts of colonists collected from colonization plates at the East Pacific Rise (EPR) deep-sea vents (1998-2017). Biological and Chemical Oceanography Data Management Office (BCO-DMO). (Version 2) Version Date 2020-08-31. doi:10.26008/1912/bco-dmo.733173.2
