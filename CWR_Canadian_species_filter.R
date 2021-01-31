library(tidyverse)

# Full US CWR Inventory from PNAS 2020 Table S1
US_inventory <- read.csv("pnas_tableS1_Usinventory_1A_1B_1C.csv")
# US CWR list from GRIN filter
US_GRIN <- read.csv("GRIN_US_cleaned_data.csv")
# Canada CWR list from GRIN filter
Canada_GRIN <- read.csv("GRIN_Canada_cleaned_data.csv")

nrow(US_GRIN) # 502 rows
nrow(US_inventory) # 656 rows
# The GRIN US filter should give the same species list as the US inventory
# However, the GRIN file has fewer entries. 
# are there any species in the GRIN US filter that are not in the PNAS supp?
# any in the PNAS supp that are not in the GRIN filter?

# First, figure out which column to compare (want to find unmatched species names)
str(US_GRIN) # species name column is "CROP.WILD.RELATIVE"
str(US_inventory) # # species name column is "Taxon"
US_inventory <- rename(US_inventory, CROP.WILD.RELATIVE = Taxon) # change "Taxon" to ""
US_GRIN$CROP.WILD.RELATIVE <- trimws(US_GRIN$CROP.WILD.RELATIVE, which = c("right")) # remove trailing white space on species names

unmatched_records_from_US_GRIN <- anti_join(US_GRIN, US_inventory, by="CROP.WILD.RELATIVE")
unmatched_records_from_US_Inventory <- anti_join(US_inventory, US_GRIN, by="CROP.WILD.RELATIVE")

# generate a list of endemic Canadian CWR
# are there any species in the GRIN Canadian filter that are not in the GRIN US filter?
Canada_GRIN$CROP.WILD.RELATIVE <- trimws(Canada_GRIN$CROP.WILD.RELATIVE, which = c("right")) # remove trailing white space on species names
Canadian_endemic_CWR <- anti_join(Canada_GRIN, US_GRIN, by="CROP.WILD.RELATIVE")

