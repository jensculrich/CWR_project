library(tidyverse)
library(stringr)

df <- read.csv("Davidson_Canada_raw_data.csv")

Canada_Davidson <- df %>% 
  # need genus and species all in one variable
  unite("X", 
        Wild_Relative_Genus:Wild_Relative_species, remove = FALSE) %>%
  mutate(CROP.WILD.RELATIVE = str_replace(X, "_", " ")) %>%
  select(Crop, CROP.WILD.RELATIVE)
Canada_Davidson <- Canada_Davidson[1:450,] # there are a lot of empty rows at the end of the df

Canada_GRIN <- read.csv("GRIN_Canada_cleaned_data.csv")
Canada_GRIN$CROP.WILD.RELATIVE <- trimws(Canada_GRIN$CROP.WILD.RELATIVE, which = c("right")) # remove trailing white space on species names


# Identify species listed in the GRIN that are not in the Davidson Canadian CWR taxon list
# Canadian_GRIN_Unique_CWR <- anti_join(Canada_GRIN, Canada_Davidson, by="CROP.WILD.RELATIVE")
# About half of these simply don't match because GRIN has subspecies/variety info in the names
# Add this to the Davidson list?

# Don't want to collapse Cop/CROP yet because sometimes the label is the same plant 
# just different way of using the common name eg "raspberry" v "raspberry, red"
CWR_Master_list <- full_join(Canada_Davidson, Canada_GRIN, by = "CROP.WILD.RELATIVE")

# Re-read csv. I manually collapsed "Crop" and "CROP"
df2 <- read.csv("CWR_Master_list.csv")

# Things to do:
# 1) figure out what to do with subspecies, varieties, and hybrids. 
# collapse all to one species?
# keep each subspecies and remove the full species row?
# keep each subspecies and KEEP the full species row?

# 2) Join with PNAS full data set to see if we can get category 1A, 1B statuses for taxa from 
# Davidson list that were not in the GRIN list. Assign rest to category 1C? Need to 
# check that they are indeed in same genus as crop?

# 3) Join with PNAS full data set to see if we can add multiple crops when applicable for 
# each CWR. E.g. if a species is a CWR of peach and plum, we want the column "Crop" to include both

# 4) Begin adding additional info for each CWR for filtering and back-end data summary,
# e.g. category for food crop specific, tree, ornamental, wild-utilized.
# e.g. conservation status when applicable.
# e.g. nut, fruit, vegetable, flower
# Look to PNAS inventory and Tara's UBC summary for ideas.

# 5) Decide formatting for crop name. Does it matter if we have "Raspberry, Red" or do
# we want to change to "Red Raspberry" To be consistent with Davidson should we circumscribe 
# crop broadly, as in change "Raspberry, Red" to "Raspberry". We can go broader for the GRIN 
# data but I'm not sure how we could go more specific for the Davidson data. That is,
# I'm not sure how we would separate a Davidson "Raspberry" into "Raspberry, Red" or "Raspberry, Black".

# 6) eventually intergate with geographic range data, and accession data from gardens/repository.