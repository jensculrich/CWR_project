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
df3 <- df2 %>% # drop unformatted columns, change chr to factor data class
  select(-geometry, -X.1) %>%
  mutate(sci_nam = as.factor(sci_nam), Crop = as.factor(Crop), 
         PRENAME = as.factor(PRENAME), ECO_NAME = as.factor(ECO_NAME), 
         ECO_CODE = as.factor(ECO_CODE))


str(df3)

# some summary information
# ecoregion with the most CWRs
df4 <- df3 %>%
  group_by(ECO_NAME) %>%
  distinct(sci_nam, .keep_all = TRUE) %>%
  add_tally() %>%
  rename(num_CWRs_in_Ecoregion = "n")
  # group_by %>% top_n(5) to filter out top five ecoregions 
# maybe also look for number endemic CWRs?
  

# province with the most CWRs
df5 <- df3 %>%
  group_by(PRENAME) %>% # group by province
  distinct(sci_nam, .keep_all = TRUE) %>% # only one CWR per province (if it's in multiple ECOs in the same province can show up >1 times)
  add_tally() %>% # tally number CWRs in tthe province
  rename(num_CWRs_in_Province = "n") %>%
  mutate(num_CWRs_in_Province = as.numeric(num_CWRs_in_Province))

CWRS_group_by_province <- df5 %>% # all I want for a graph is number of CWRS in each province
  group_by(PRENAME) %>%
  summarise(mean = mean(num_CWRs_in_Province))

CWRS_group_by_province$PRENAME <- # order provinces by number of  CWRs 
  factor(CWRS_group_by_province$PRENAME,
         levels = CWRS_group_by_province$PRENAME[
           order(CWRS_group_by_province$mean)])
# Plot number CWRs in each province
q <- ggplot(CWRS_group_by_province, aes(x = PRENAME, y = mean)) + theme_bw() + 
  geom_bar(stat = "identity") + theme(axis.text.x=element_text(angle=45, hjust=1))
q

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

# Basic box plot
s <- ggplot(df7, aes(x = PRENAME, y = num_Amelanchier_relatives_in_Province)) + theme_bw() + geom_bar(stat = "identity")
s


plot(df3$long, df3$lat)

