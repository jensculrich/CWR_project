library(tidyverse)
library(stringr)

df <- read.csv("C:/Users/jensj/Documents/UBC2/Classes/ConservationScience/Project/Davidson_Canada_raw_data.csv")

Canada_Davidson <- df %>% 
  unite("X", 
        Wild_Relative_Genus:Wild_Relative_species, remove = FALSE) %>%
  mutate(CROP.WILD.RELATIVE = str_replace(X, "_", " ")) %>%
  select(Crop, CROP.WILD.RELATIVE)

Canada_GRIN <- read.csv("GRIN_Canada_cleaned_data.csv")
Canada_GRIN$CROP.WILD.RELATIVE <- trimws(Canada_GRIN$CROP.WILD.RELATIVE, which = c("right")) # remove trailing white space on species names


# Identify species listed in the GRIN that are not in the Davidson Canadian CWR taxon list
Canadian_GRIN_Unique_CWR <- anti_join(Canada_GRIN, Canada_Davidson, by="CROP.WILD.RELATIVE")
