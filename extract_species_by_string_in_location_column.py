# little Python script to extract the rows in a data table that have partial strings in one of the columns
# I am using this to find species reported in the sFDvent database for the East Pacific Rise and neighboring regions
# Stace Beaulieu (stace@whoi.edu) 2019-10-07

import pandas
# note I edited the column headers in Excel file that Abbie provided to remove the special characters and spaces
abbie = pandas.read_excel('Table_S4.4_CleanedRaw_SBeaulieu.xlsx')
# original file has 261 species

# adapt this:
# https://stackoverflow.com/questions/44933071/select-rows-by-partial-string-with-query-with-pandas/53344073#53344073
# print(df.query('name.str.contains("lu")', engine='python').head())

EPRGalGuay = abbie.query('Location.str.contains("EPR|Galapagos|East Pacific Rise|Guaymas|Costa Rica|Gulf of California")', engine='python')
# after initial exploration I added "Costa Rica" and "Gulf of California" as search terms, but not "Eastern Pacific" or "E Pacific"
# note I did not use "NE Pacific" because it appeared to refer mainly to Juan de Fuca
EPRGalGuay.to_csv(r'EPRGalGuay.csv')
# subset with these strings has 165 species

# next figure out the subset that DOES NOT have those strings to see if I missed any important
# and this should have 261 - 165 = 96 species
# I couldn't get this to work:
# https://stackoverflow.com/questions/21055068/reversal-of-string-contains-in-python-pandas
# so instead used a left-join and visually inspected in EXCEL:
# https://stackoverflow.com/questions/28901683/pandas-get-rows-which-are-not-in-other-dataframe

df_all = abbie.merge(EPRGalGuay.drop_duplicates(), on=['Taxon','Location'], 
                   how='left', indicator=True)
df_all.to_csv(r'left-join-abbie-EPRGalGuay.csv')