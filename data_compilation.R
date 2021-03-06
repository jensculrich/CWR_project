library(tidyverse)
library(stringr)
library(ggplot2)

# compile garden data
df <- read.csv("GBIF_by_Province.csv")

df2 <- df %>%
  select(Crop, sci_nam, ECO_CODE, ECO_NAME, PRENAME, geometry, X.1)
  
# remove "()" and "c" from geometry and X.1, rename as longitude and latitude
# change from chr to numeric
df2$longitude <- as.numeric(str_sub(df2$geometry, 3))  
df2$latitude <- as.numeric(str_remove(df2$X.1, "[)]"))
df3 <- df2 %>% # drop unformatted columns
  select(-geometry, -X.1)

str(df3)

# some summary information
# ecoregion with the most CWRs
df4 <- df3 %>%
  group_by(ECO_NAME) %>%
  distinct(sci_nam, .keep_all = TRUE) %>%
  add_tally() %>%
  rename(num_CWRs_in_Ecoregion = "n") 

# province with the most CWRs
df5 <- df3 %>%
  group_by(PRENAME) %>%
  distinct(sci_nam, .keep_all = TRUE) %>%
  add_tally() %>%
  rename(num_CWRs_in_Province = "n") 

# ecoregion with the most Amelanchier CWRs
df6 <- df3 %>%
  group_by(ECO_NAME) %>%
  filter(grepl('Amelanchier', sci_nam)) %>%
  distinct(sci_nam, .keep_all = TRUE) %>%
  add_tally() %>%
  rename(num_Amelanchier_relatives_in_Ecoregion = "n") 

# province with the most Amelanchier CWRs
df7 <- df3 %>%
  group_by(PRENAME) %>%
  filter(grepl('Amelanchier', sci_nam)) %>%
  distinct(sci_nam, .keep_all = TRUE) %>%
  add_tally() %>%
  rename(num_Amelanchier_relatives_in_Province = "n") 


plot(df3$long, df3$lat)

which.max(df3$ECO_NAME)
str(df3)
