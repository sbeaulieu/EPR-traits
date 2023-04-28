# R script to clean EPR pumps data for submitting to BCO-DMO
# Stace Beaulieu 2023-04-28
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

# compare to next sheet "xx"
