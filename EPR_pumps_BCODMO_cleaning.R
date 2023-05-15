# R script to clean EPR pumps data for submitting to BCO-DMO
# Stace Beaulieu 2023-05-15
# 
# Input files:
#    - Pump_near_bottom_compilation.xlsx Susan Mills's compiled data EXCEL
#    - [ultimately this will also include our lookup table for WorMS taxon matching]
#
# The script will:
#    - confirm using sheet "Composite_Actual_Numbers"
#    - create wide occurrence table with unique column headers as eventID's for "Composite_Actual_Numbers"
#    - create a separate event table for sampling metadata
#
library(readxl) # for input xls file
# library(readr) # for input WoRMS lookup csv file if don't use sheet "Categories"
library(stringr)
library(dplyr)
setwd("C:/Users/sbeaulieu/Downloads") # xls file will be local (not copied to git repo)

# grab top part of EXCEL file to concatenate column headers for wide format (which will become eventIDs)
# toppart <- read_excel("Pump_near_bottom_compilation.xlsx", sheet = "Composite_Actual_Numbers", range = "A3:BO9", col_names = FALSE)
# just start with Date Deployed row for comparing sheets
toppart <- read_excel("Pump_near_bottom_compilation.xlsx", sheet = "Composite_Actual_Numbers", range = "A4:BO9", col_names = FALSE)

# make the column names
# appears all columns already as character except 1st
cnames = apply(toppart, 2, paste0, collapse = "-")
cnames[1] <- "delete_this"
cnames[2] <- "category"
# confirm unique
unique(cnames)

# grab wide compiled data from EXCEL file
# read in except first 10 rows
widedata <- read_xlsx("Pump_near_bottom_compilation.xlsx", sheet = "Composite_Actual_Numbers", col_names = cnames, skip = 10)

# counts are in columns 3 to 67
# columns 3 to 29 should be sheet "1998-2000_Raw" (27 columns)
# columns 30 to 34 should be sheet "2004_Raw" (5 columns)
# columns 35 to 55 should be sheet "LADDER1-3_Raw" (21 columns)
# columns 56 to 61 should be sheet "2019_Raw" (6 columns)
# columns 62 to 67 should be sheet "2021_Raw" (6 columns)

# compare to sheet "1998-2000_Raw"

# grab top part of EXCEL file to concatenate column headers for wide format (which will become eventIDs)
toppart1998 <- read_excel("Pump_near_bottom_compilation.xlsx", sheet = "1998-2000_Raw", range = "A4:AC8", col_names = FALSE)
# make the column names
# appears all columns already as character except 1st
cnames1998 = apply(toppart1998, 2, paste0, collapse = "-")
cnames1998[1] <- "delete_this"
cnames1998[2] <- "category"
# confirm unique
unique(cnames1998)

sheet1998 <- read_xlsx("Pump_near_bottom_compilation.xlsx", sheet = "1998-2000_Raw", col_names = cnames1998, skip = 12)

# could use dplyr to not select delete_this column
sheet1998 <- sheet1998[,2:29]
# but easier to specify column indices for the widedata
widedatasubset1998 <- widedata[,2:29]

# exclude the 3 rows with totals in widedata
widedatasubset1998 <- filter(widedatasubset1998,!str_detect(category,'Total'))
# using str_detect also deletes rows with category NA
# no totals just NAs in sheet1998
# sheet1998 <- sheet1998 %>% dplyr::filter(!is.na(category))
sheet1998 <- filter(sheet1998,!is.na(category))

check1998 <- dplyr::full_join(sheet1998, widedatasubset1998, by='category')
# alphabetize the columns and rows for easier manual inspection
# this will look better with tidy pipe
check1998 <- select(check1998,order(colnames(check1998)))
check1998 <- arrange(check1998,category)
# write to csv for inspection in spreadsheet
write.csv(check1998, "confirm_matching_sheet1998.csv")
# sum the columns in the spreadsheet to aid looking for discrepancies
# can hide rows when paired columns using same category (or next alphabetically)    

# compare to sheet "2004_Raw"

# grab top part of EXCEL file to concatenate column headers for wide format (which will become eventIDs)
toppart2004 <- read_excel("Pump_near_bottom_compilation.xlsx", sheet = "2004_Raw", range = "A4:G7", col_names = FALSE)
# make the column names
# appears all columns already as character except 1st
cnames2004 = apply(toppart2004, 2, paste0, collapse = "-")
cnames2004[1] <- "delete_this"
cnames2004[2] <- "category"
# confirm unique
unique(cnames2004)

sheet2004 <- read_xlsx("Pump_near_bottom_compilation.xlsx", sheet = "2004_Raw", col_names = cnames2004, skip = 10)

# could use dplyr to not select delete_this column
sheet2004 <- sheet2004[,2:7]
# but easier to specify column indices for the widedata
# columns 30 to 34 should be sheet "2004_Raw" (5 columns)
widedatasubset2004 <- select(widedata,2,30:34)

# exclude the 3 rows with totals in widedata
widedatasubset2004 <- filter(widedatasubset2004,!str_detect(category,'Total'))
# using str_detect also deletes rows with category NA
# no totals just NAs in sheet2004
sheet2004 <- filter(sheet2004,!is.na(category))

check2004 <- dplyr::full_join(sheet2004, widedatasubset2004, by='category')
# alphabetize the columns and rows for easier manual inspection
check2004sort <- check2004 %>%
  select(order(colnames(check2004))) %>%
  arrange(category)
# write to csv for inspection in spreadsheet
write.csv(check2004sort, "confirm_matching_sheet2004.csv")

# compare to sheet "LADDER1-3_Raw"

# grab top part of EXCEL file to concatenate column headers for wide format (which will become eventIDs)
toppartLADDER <- read_excel("Pump_near_bottom_compilation.xlsx", sheet = "LADDER1-3_Raw", range = "A4:z8", col_names = FALSE)
# make the column names
# appears all columns already as character except 1st and last 2
cnamesLADDER = apply(toppartLADDER, 2, paste0, collapse = "-")
cnamesLADDER[1] <- "delete_this"
cnamesLADDER[2] <- "category"
cnamesLADDER[25] <- "delete_this_too"
cnamesLADDER[26] <- "delete_this_also"
# confirm unique
unique(cnamesLADDER)

sheetLADDER <- read_xlsx("Pump_near_bottom_compilation.xlsx", sheet = "LADDER1-3_Raw", col_names = cnamesLADDER, skip = 13)

# could use dplyr to not select delete columns
sheetLADDER <- dplyr::select(sheetLADDER,-matches("del*"))
# 39032-L7 K Vent 3mab on was not sorted thus is missing in compilation sheet
sheetLADDER <- dplyr::select(sheetLADDER,-matches("39032-L7-*"))
# easier to specify column indices for the widedata
# columns 35 to 55 should be sheet "LADDER1-3_Raw" (21 columns)
widedatasubsetLADDER <- select(widedata,2,35:55)
# exclude the 3 rows with totals in widedata
widedatasubsetLADDER <- filter(widedatasubsetLADDER,!str_detect(category,'Total'))
# using str_detect also deletes rows with category NA
# no totals just NAs in sheetLADDER
sheetLADDER <- filter(sheetLADDER,!is.na(category))

checkLADDER <- dplyr::full_join(sheetLADDER, widedatasubsetLADDER, by='category')
# alphabetize the columns and rows for easier manual inspection
checkLADDERsort <- checkLADDER %>%
  select(order(colnames(checkLADDER))) %>%
  arrange(category)
# write to csv for inspection in spreadsheet
write.csv(checkLADDERsort, "confirm_matching_sheetLADDER.csv")

# compare to sheet "2019_Raw"

# grab top part of EXCEL file to concatenate column headers for wide format (which will become eventIDs)
toppart2019 <- read_excel("Pump_near_bottom_compilation.xlsx", sheet = "2019_Raw", range = "A4:h8", col_names = FALSE)
# make the column names
# appears all columns already as character except 1st
cnames2019 = apply(toppart2019, 2, paste0, collapse = "-")
cnames2019[1] <- "delete_this"
cnames2019[2] <- "category"
# confirm unique
unique(cnames2019)

sheet2019 <- read_xlsx("Pump_near_bottom_compilation.xlsx", sheet = "2019_Raw", col_names = cnames2019, skip = 13)

# could use dplyr to not select delete_this column
sheet2019 <- sheet2019[,2:8]
# but easier to specify column indices for the widedata
# columns 56 to 61 should be sheet "2019_Raw" (6 columns)
widedatasubset2019 <- select(widedata,2,56:61)

# exclude the 3 rows with totals in widedata
widedatasubset2019 <- filter(widedatasubset2019,!str_detect(category,'Total'))
# using str_detect also deletes rows with category NA
# no totals just NAs in sheet2019
sheet2019 <- filter(sheet2019,!is.na(category))

check2019 <- dplyr::full_join(sheet2019, widedatasubset2019, by='category')
# alphabetize the columns and rows for easier manual inspection
check2019sort <- check2019 %>%
  select(order(colnames(check2019))) %>%
  arrange(category)
# write to csv for inspection in spreadsheet
write.csv(check2019sort, "confirm_matching_sheet2019.csv")

# compare to sheet "2021_Raw"

# grab top part of EXCEL file to concatenate column headers for wide format (which will become eventIDs)
toppart2021 <- read_excel("Pump_near_bottom_compilation.xlsx", sheet = "2021_Raw", range = "A4:h8", col_names = FALSE)
# make the column names
# appears all columns already as character except 1st
cnames2021 = apply(toppart2021, 2, paste0, collapse = "-")
cnames2021[1] <- "delete_this"
cnames2021[2] <- "category"
# confirm unique
unique(cnames2021)

sheet2021 <- read_xlsx("Pump_near_bottom_compilation.xlsx", sheet = "2021_Raw", col_names = cnames2021, skip = 13)

# could use dplyr to not select delete_this column
sheet2021 <- sheet2021[,2:8]
# but easier to specify column indices for the widedata
# columns 62 to 67 should be sheet "2021_Raw" (6 columns)
widedatasubset2021 <- select(widedata,2,62:67)

# exclude the 3 rows with totals in widedata
widedatasubset2021 <- filter(widedatasubset2021,!str_detect(category,'Total'))
# using str_detect also deletes rows with category NA
# no totals just NAs in sheet2021
sheet2021 <- filter(sheet2021,!is.na(category))

check2021 <- dplyr::full_join(sheet2021, widedatasubset2021, by='category')
# alphabetize the columns and rows for easier manual inspection
check2021sort <- check2021 %>%
  select(order(colnames(check2021))) %>%
  arrange(category)
# write to csv for inspection in spreadsheet
write.csv(check2021sort, "confirm_matching_sheet2021.csv")

