library(tidyverse)
library(stringr)
library(Rfast)

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
  add_tally()


names(nth(table(df3$ECO_NAME, 1, descending = T)))
names(which.max(table(df3$ECO_NAME)))

plot(df3$long, df3$lat)

which.max(df3$ECO_NAME)
str(df3)
