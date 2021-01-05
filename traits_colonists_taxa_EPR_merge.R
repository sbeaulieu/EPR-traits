# Script to confirm that LD's traits data table has subset of taxa in LM's all taxa colonists table
# Stace Beaulieu 2021-01-05
#
# Inputs:
# DataS2_BCO-DMO.csv received in email from LD 2021-01-04
# Pvent_P-S_135_supptable1_submit_merged_wide_106.csv Alternate wide data format in BCO-DMO

library(dplyr)

setwd("C:/Users/sbeaulieu/Desktop")
traits <- read.csv("DataS2_BCO-DMO.csv") # 58 rows
colonists <- read.csv("Pvent_P-S_135_supptable1_submit_merged_wide_106.csv") # 68 rows
# colonists table has 10 extra rows

IDall <- full_join(traits, colonists, by="scientificNameID")
# full join on column with same header in the 2 tables
# yields 70 rows, so expect 2 mis-matches if subset
# Mis-matches
#   traits has AphiaID for species pompejana instead of genus: Alvinella sp.	urn:lsid:marinespecies.org:taxname:336171 (colonists is correct Alvinella sp.	Alvinella	324590)
#   traits has AphiaID for genus instead of species  Phymorhynchus major	urn:lsid:marinespecies.org:taxname:137827 (colonists is correct Phymorhynchus major	urn:lsid:marinespecies.org:taxname:434621)
#   note traits table not providing the paired scientificName/scientificNameID

# colonists has 9 categories not in traits:
#   gastropod unk A; polychaetes, unk; polychaetes, juv; worm, unsegmented; barnacle cyprids; Abyssotherma pacifica; Metafolliculina sp.; other forams; possible flatworm

# why not 10?
#   because the colonists tables has polynoids, adult and polynoids, unk so 2 rows match to the 1 in traits
#   this means that the 58 traits taxa are an exact subset of 59 of the 68 in the colonists table

# how closely does the column traits SCIENTIFIC_NAME match to colonists dataProviderName?
# rename traits to dataProviderName for full join
traits_rename <- rename(traits, dataProviderName = SCIENTIFIC_NAME)
nameall <- full_join(traits_rename, colonists, by="dataProviderName")
# yields 73 rows so what are the 5 fuzzy matches?
# Hesionid sp. to hesionid
# serpulid spp. to *Laminatubus alvini
# polynoid spp. to polynoids, adult and polynoids, unk
# siboglinid spp. to siboglinids, small
# shrimp to shrimp, juvenile

