############################################################
# join province/ecoregion with gardens based on lat/long data
# some gardens only have lat/long, others will have province.


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
library(raster)

# data from gardens (already filtered to only CWRs)
cwr_ubc <- read.csv("CWR_of_UBC.csv")
cwr_rbg <- read.csv("CWR_of_RBG.csv")
cwr_montreal <- read.csv("CWR_of_MontrealBG.csv")
cwr_guelph <- read.csv("CWR_of_UofGuelph.csv")
cwr_mountp <- read.csv("CWR_of_MountPleasantGroup.csv")
cwr_vandusen <- read.csv("CWR_of_VanDusenBG.csv")
cwr_pgrc <- read.csv("Amelanchier_PGRC.csv")
cwr_usask <- read.csv("Amelanchier_UofSask.csv")

# join all garden data into one long table
garden_accessions <- rbind(cwr_ubc, cwr_rbg, cwr_montreal, cwr_guelph, cwr_mountp, cwr_vandusen,
      cwr_pgrc, cwr_usask)

str(garden_accessions)
garden_accessions <- garden_accessions %>% # format columns
  mutate(latitude = as.numeric(latitude), longitude = as.numeric(longitude))

# translate lat and long into province (later ecoregion)


# Transform garden data into a projected shape file
sf_garden_accessions <- garden_accessions %>%
  # na.fail = FALSE to keep all of the accessions (about 80% don't have lat long,
  # but many of these have province at least)
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, na.fail = FALSE)

# add geojson map with province boundaries 
canada_cd <- st_read("canada_provinces.geojson", quiet = TRUE) # 1
crs_string = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs" # 2

# add geojson map with ecoregion boundaries
# to do

# Plot the maps
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

ggplot() +
  geom_sf(aes(fill = name), color = "gray60", size = 0.1, data = canada_cd) +
  geom_sf(data = sf_garden_accessions, color = '#001e73', alpha = 0.5, size = 3) + # 17
  coord_sf(crs = crs_string) +
  scale_fill_manual(values = map_colors) +
  guides(fill = FALSE) +
  theme_map() +
  theme(panel.grid.major = element_line(color = "white"),
        legend.key = element_rect(color = "gray40", size = 0.1))
# where is the weird very far north point coming from????

# Append Province to accession using lat and longitudecanada_cd <- st_transform(st_as_sf(canada_cd), 4326)
points_sf = st_transform( st_as_sf(sf_garden_accessions), 
                          coords = c("longitude", "latitude"), 
                          crs = 4326, agr = "constant")
points_polygon <- st_join(sf_garden_accessions, canada_cd, left = TRUE)

# merge these data back with the garden accessions, since I had to drop 
# all with lat/long = na
points_polygon <- points_polygon %>%
  # break coordinates into lat/long
  mutate(longitude=gsub("\\,.*","", geometry)) %>%
  mutate(latitude=gsub(".*,","",geometry)) %>%
  # format to remove "c(" and  ")"
  mutate(longitude = as.numeric(str_sub(longitude, 3)))  %>% 
  mutate(latitude = as.numeric(str_remove(latitude, "[)]"))) %>% 
  # select columns that match garden accessions
  select(X, garden, crop, species, variant, latitude, longitude, country,
         IUCNRedList, province, name) %>%
  rename(new = province) %>% # add a dummy name for province 
  mutate(province = ifelse(is.na(new), name, new)) %>%
  select(-new, - name)

# write.csv(points_polygon, "all_garden_accessions_with_geo_data.csv")



#################
# what to do next?

# https://www.r-graph-gallery.com/choropleth-map.html

# join with df3

# want a row for each accession where Crop.wild.relative matches

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