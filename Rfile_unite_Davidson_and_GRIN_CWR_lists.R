library(tidyverse)
library(stringr)

# CWR listr from Davidson 1995
df <- read.csv("Davidson_Canada_raw_data.csv")

Canada_Davidson <- df %>% 
  # need genus and species all in one variable
  unite("X", 
        Wild_Relative_Genus:Wild_Relative_species, remove = FALSE) %>%
  mutate(CROP.WILD.RELATIVE = str_replace(X, "_", " ")) %>%
  select(Crop, CROP.WILD.RELATIVE)
Canada_Davidson <- Canada_Davidson[1:450,] # there are a lot of empty rows at the end of the df

# CWR list from https://npgsweb.ars-grin.gov/gringlobal/search
Canada_GRIN <- read.csv("GRIN_Canada_cleaned_data.csv")
Canada_GRIN$CROP.WILD.RELATIVE <- trimws(Canada_GRIN$CROP.WILD.RELATIVE, which = c("right")) # remove trailing white space on species names

# Identify species listed in the GRIN that are not in the Davidson Canadian CWR taxon list
# Canadian_GRIN_Unique_CWR <- anti_join(Canada_GRIN, Canada_Davidson, by="CROP.WILD.RELATIVE")
# About half of these simply don't match because GRIN has subspecies/variety info in the names
# Add this to the Davidson list?

# Don't want to collapse Crop/CROP yet because sometimes the label is the same plant 
# just different way of using the common name eg "raspberry" v "raspberry, red"
CWR_Master_list <- full_join(Canada_Davidson, Canada_GRIN, by = "CROP.WILD.RELATIVE")

# "Crop" and "CROP" were manually collapsed into one column following the full_join
