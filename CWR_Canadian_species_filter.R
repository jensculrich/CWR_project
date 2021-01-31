# Full US CWR Inventory from PNAS 2020 Table S1
US_inventory <- read.csv("pnas_tableS1_Usinventory_1A_1B_1C.csv")
# US CWR list from GRIN filter
US_GRIN <- read.csv("All_US_CWR_Pri_Sec_Tert.csv")
# Canada CWR list from GRIN filter
Canada_GRIN <- read.csv("All_Canadian_CWR_Pri_Sec_Tert.csv")

nrow(US_GRIN) # 1025 rows
nrow(US_inventory) # 656 rows
# The GRIN US filter should give the same species list as the US inventory
# However, the GRIN file has many more entries. In the GRIN file,
# a species is listed multiple times if it is related to more than 1 crop
# Is this the only reason the length is longer, or are there any species that 
# were not covered in the PNAS inventory?

# are there any species in the GRIN US filter that are not in the PNAS supp?
# any in the PNAS supp that are not in the GRIN filter?