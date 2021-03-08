library(tidyverse)
library(stringr)
library(ggplot2)

# full CWR table summary stats
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
  rename(num_CWRs_in_Ecoregion = "n") %>%
  mutate(num_CWRs_in_Ecoregion = as.numeric(num_CWRs_in_Ecoregion))

CWRs_group_by_ecoregion <- df4 %>% # all I want for a graph is number of CWRS in each province
  group_by(ECO_NAME) %>%
  summarise(CWRs_per_Ecoregion = mean(num_CWRs_in_Ecoregion))

CWRs_group_by_ecoregion$ECO_NAME <- # order provinces by number of  CWRs 
  factor(CWRs_group_by_ecoregion$ECO_NAME,
         levels = CWRs_group_by_ecoregion$ECO_NAME[
           order(CWRs_group_by_ecoregion$CWRs_per_Ecoregion)])

# Plot number CWRs in each province (as a histogram)
p <- ggplot(CWRs_group_by_ecoregion, aes(x = CWRs_per_Ecoregion)) + theme_bw() + 
  geom_histogram()
p

# show five ecoregions with most CWRs
top_n(CWRs_group_by_ecoregion, 5, wt = CWRs_per_Ecoregion)
# show five ecoregions with least CWRs
top_n(CWRs_group_by_ecoregion, -5, wt = CWRs_per_Ecoregion) 

# maybe also look for number eco-endemic CWRs using df4?
  

# province with the most CWRs
df5 <- df3 %>%
  group_by(PRENAME) %>% # group by province
  distinct(sci_nam, .keep_all = TRUE) %>% # only one CWR per province (if it's in multiple ECOs in the same province can show up >1 times)
  add_tally() %>% # tally number CWRs in tthe province
  rename(num_CWRs_in_Province = "n") %>%
  mutate(num_CWRs_in_Province = as.numeric(num_CWRs_in_Province))

CWRS_group_by_province <- df5 %>% # all I want for a graph is number of CWRS in each province
  group_by(PRENAME) %>%
  summarise(CWRs = mean(num_CWRs_in_Province))

CWRS_group_by_province$PRENAME <- # order provinces by number of  CWRs 
  factor(CWRS_group_by_province$PRENAME,
         levels = CWRS_group_by_province$PRENAME[
           order(CWRS_group_by_province$CWRs)])

# Plot number CWRs in each province
q <- ggplot(CWRS_group_by_province, aes(x = PRENAME, y = CWRs)) + theme_bw() + 
  geom_bar(stat = "identity") + theme(axis.text.x=element_text(angle=45, hjust=1))
q

# ecoregion with the most Amelanchier CWRs
df6 <- df3 %>%
  group_by(ECO_NAME) %>%
  filter(grepl('Amelanchier', sci_nam)) %>%
  distinct(sci_nam, .keep_all = TRUE) %>%
  add_tally() %>%
  rename(num_Amelanchier_relatives_in_Ecoregion = "n") %>%
  mutate(num_Amelanchier_relatives_in_Ecoregion = as.numeric(num_Amelanchier_relatives_in_Ecoregion))

Amelanchier_group_by_ecoregion <- df6 %>% # all I want for a graph is number of CWRS in each province
  group_by(ECO_CODE) %>%
  summarise(Amelanchier_relatives = mean(num_Amelanchier_relatives_in_Ecoregion))

Amelanchier_group_by_ecoregion$ECO_CODE <- # order provinces by number of  CWRs 
  factor(Amelanchier_group_by_ecoregion$ECO_CODE,
         levels = Amelanchier_group_by_ecoregion$ECO_CODE[
           order(Amelanchier_group_by_ecoregion$Amelanchier_relatives)])

# Plot number CWRs in each province
r <- ggplot(Amelanchier_group_by_ecoregion, aes(x = ECO_CODE, y = Amelanchier_relatives)) + theme_bw() + 
  geom_bar(stat = "identity") + theme(axis.text.x=element_text(angle=45, hjust=1))
r

# province with the most Amelanchier CWRs
df7 <- df3 %>%
  group_by(PRENAME) %>%
  filter(grepl('Amelanchier', sci_nam)) %>%
  distinct(sci_nam, .keep_all = TRUE) %>%
  add_tally() %>%
  rename(num_Amelanchier_relatives_in_Province = "n") %>%
  mutate(num_Amelanchier_relatives_in_Province = as.numeric(num_Amelanchier_relatives_in_Province))

Amelanchier_group_by_Province <- df7 %>% # all I want for a graph is number of CWRS in each province
  group_by(PRENAME) %>%
  summarise(Amelanchier_relatives = mean(num_Amelanchier_relatives_in_Province))

Amelanchier_group_by_Province$PRENAME <- # order provinces by number of  CWRs 
  factor(Amelanchier_group_by_Province$PRENAME,
         levels = Amelanchier_group_by_Province$PRENAME[
           order(Amelanchier_group_by_Province$Amelanchier_relatives)])

# Plot number CWRs in each province
s <- ggplot(Amelanchier_group_by_Province, aes(x = PRENAME, y = Amelanchier_relatives)) + theme_bw() + 
  geom_bar(stat = "identity") + theme(axis.text.x=element_text(angle=45, hjust=1))
s

# lat/long of one GBIF occurrence per species per ecoregion
plot(df3$long, df3$lat)

############################################################
# compile garden data

library(sf) # the base package manipulating shapes
library(rgeos)
library(rgdal) # geo data abstraction library
library(geojsonio) # geo json input and output
library(spdplyr) # the `dplyr` counterpart for shapes
library(rmapshaper) # the package that allows geo shape transformation
library(magrittr) # data wrangling
library(dplyr)
library(tidyverse)
library(ggplot2)
library(viridis)


# data from gardens (already filtered to only CWRs)
ubc_cwr <- read.csv("CWR_of_UBC.csv")
rbg_cwr <- read.csv("CWR_of_RBG.csv")
mbg_cwr <- read.csv("CWR_of_montreal.csv")

ubc_cwr2 <- ubc_cwr %>%
  select(Crop, CROP.WILD.RELATIVE, CoordLatDD, CoordLongDD, IUCNRedListCode, LifeForm, CountryName)

str(ubc_cwr2)

rbg_cwr2 <- rbg_cwr %>%
  select()

str(rbg_cwr2)

mbg_cwr2 <- mbg_cwr %>%
  select()

str(mbg_cwr2)


# translate lat and long into province (later ecoregion)

# add geojson map with province boundaries 
canada_cd <- st_read("canada_provinces.geojson", quiet = TRUE) # 1
crs_string = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs" # 2

# Define the maps' theme -- remove axes, ticks, borders, legends, etc.
theme_map <- function(base_size=9, base_family="") { # 3
  require(grid)
  theme_bw(base_size=base_size, base_family=base_family) %+replace%
    theme(axis.line=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid=element_blank(),
          panel.spacing=unit(0, "lines"),
          plot.background=element_blank(),
          legend.justification = c(0,0),
          legend.position = c(0,0)
    )
}
# Define the filling colors for each province; max allowed is 9 but good enough for the 13 provinces + territories
map_colors <- RColorBrewer::brewer.pal(9, "Greens") %>% rep(37) # 4

# Plot the maps
ggplot() +
  geom_sf(aes(fill = name), color = "gray60", size = 0.1, data = canada_cd) + # 5
  coord_sf(crs = crs_string) + # 6
  scale_fill_manual(values = map_colors) +
  guides(fill = FALSE) +
  theme_map() +
  theme(panel.grid.major = element_line(color = "white"),
        legend.key = element_rect(color = "gray40", size = 0.1))

# Conforming the data frame (lat and long of wild origin accession) to shape objects
sf_cwr_origins = ubc_cwr2 %>%
  select(CoordLongDD, CoordLatDD) %>% # 1
  drop_na()  %>%
  as.matrix() %>% # 2
  st_multipoint(dim = 'XY') %>% # 3
  st_sfc() %>% # 4
  st_set_crs(4269) # 5

ggplot() +
  geom_sf(aes(fill = name), color = "gray60", size = 0.1, data = canada_cd) +
  geom_sf(data = sf_cwr_origins, color = '#001e73', alpha = 0.5, size = 3) + # 17
  coord_sf(crs = crs_string) +
  scale_fill_manual(values = map_colors) +
  guides(fill = FALSE) +
  theme_map() +
  theme(panel.grid.major = element_line(color = "white"),
        legend.key = element_rect(color = "gray40", size = 0.1))

# join with df3



# want a row for each accession where Crop.wild.relative matches
