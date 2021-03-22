############################################################
# in order to conduct a GAP ANALYSIS, we need to determine the ecoregion and province
# represented by each of the CWR ex situ accessions in our gardens data sets.
# then this ex situ conservation coverage must be compared against the native range.

# PART ONE: 
# join province/ecoregion with each accession from garden collection data based on 
# lat/long they provided us. some gardens only have lat/long, 
# others will have province. Most accessions (about 80%, see below) have neither lat/long or province.

# load required packages
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
library(cartography)
library(viridis)

# load natural occurrence dataset
df <- read.csv("GBIF_by_Province.csv")

df2 <- df %>%
  dplyr::select(Crop, sci_nam, ECO_CODE, ECO_NAME, PRENAME, geometry, X.1)

# format natural occurrence dataset...
# remove "()" and "c" from geometry and X.1, rename as longitude and latitude
# change from chr to numeric
df2$longitude <- as.numeric(str_sub(df2$geometry, 3))  
df2$latitude <- as.numeric(str_remove(df2$X.1, "[)]"))
native_occurrence_df <- df2 %>% # drop unformatted columns, change chr to factor data class
  dplyr::select(-geometry, -X.1) %>%
  mutate(sci_nam = as.factor(sci_nam), Crop = as.factor(Crop), 
         PRENAME = as.factor(PRENAME), ECO_NAME = as.factor(ECO_NAME), 
         ECO_CODE = as.factor(ECO_CODE))

##########
# Part 1A compile garden data and append ecoregion or province
# when lat/long was given

# load data from garden collections (already filtered to only CWRs)
# update and add new gardens as we receive additional datasets
cwr_ubc <- read.csv("CWR_of_UBC.csv")
cwr_rbg <- read.csv("CWR_of_RBG.csv")
cwr_montreal <- read.csv("CWR_of_MontrealBG.csv")
cwr_guelph <- read.csv("CWR_of_UofGuelph.csv")
cwr_mountp <- read.csv("CWR_of_MountPleasantGroup.csv")
cwr_vandusen <- read.csv("CWR_of_VanDusenBG.csv")
cwr_pgrc <- read.csv("Amelanchier_PGRC.csv")
cwr_usask <- read.csv("Amelanchier_UofSask.csv")

# join all garden data into one long table
# update and add new gardens as we receive additional datasets
garden_accessions <- rbind(cwr_ubc, cwr_rbg, cwr_montreal, cwr_guelph, cwr_mountp, cwr_vandusen,
      cwr_pgrc, cwr_usask)
garden_accessions <- garden_accessions %>% # format columns
  mutate(latitude = as.numeric(latitude), 
         longitude = as.numeric(longitude)) %>%
  # for now, we want to filter our data for coverage of ONLY CANADIAN ecoregions/admin districts
  # delete the follwoing line of code if the focus expands to North America or world
  filter(country == "Canada")

# Transform garden data into a projected shape file
sf_garden_accessions <- garden_accessions %>%
  # na.fail = FALSE to keep all of the accessions (about 80% don't have lat long,
  # but many of these have province at least)
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, na.fail = FALSE)

# add geojson map with province boundaries 
canada_cd <- st_read("canada_provinces.geojson", quiet = TRUE) # 1
crs_string = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs" # 2

# add geojson map with all of canada (no inner boundaries)
# we will use this as a boundary for trimming all the ecoregion maps
canada <- st_read("canada.geojson", quiet = TRUE) # 1

# add geojson map with ecoregion boundaries
world_eco <- st_read("world_ecoregions.geojson", quiet = TRUE)
# Trim geojson world map to canada ecoregions from native_occurrence_df
canada_eco <- semi_join(world_eco, native_occurrence_df, by=("ECO_CODE")) 

# clip ecoregions to canada national border
canada_eco_subset <- st_intersection(canada_eco, canada)

canada_cd <- canada_cd %>%
  rename("province" = "name")

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

# Plot Ecoregions
P <- ggplot() +
  geom_sf(
    # aes(fill = name), 
    color = "gray60", size = 0.1, data = canada_eco_subset) +
  geom_sf(data = sf_garden_accessions, color = '#001e73', alpha = 0.5, size = 3) + # 17
  coord_sf(crs = crs_string) +
  # scale_fill_manual(values = map_colors) +
  guides(fill = FALSE) +
  theme_map() +
  ggtitle("Known Geographic Origins of Native CWR's in Surveyed Canadian Botanic Gardens") +
  theme(panel.grid.major = element_line(color = "white"),
        legend.key = element_rect(color = "gray40", size = 0.1),
        plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5)
  )
P


# Plot By province
Q <- ggplot() +
  geom_sf(
    # aes(fill = name), 
    color = "gray60", size = 0.1, data = canada_cd) +
  geom_sf(data = sf_garden_accessions, color = '#001e73', alpha = 0.5, size = 3) + # 17
  coord_sf(crs = crs_string) +
  # scale_fill_manual(values = map_colors) +
  guides(fill = FALSE) +
  theme_map() +
  ggtitle("Known Geographic Origins of Native CWR's in Surveyed Canadian Botanic Gardens") +
  theme(panel.grid.major = element_line(color = "white"),
        legend.key = element_rect(color = "gray40", size = 0.1),
        plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5)
        )
Q

# Append Province to accession using lat and longitude
points_sf = st_transform( st_as_sf(sf_garden_accessions), 
                          coords = c("longitude", "latitude"), 
                          crs = 4326, agr = "constant")
# spatial join to add accession province
points_polygon <- st_join(sf_garden_accessions, canada_cd, left = TRUE)
# spatial join to add accession ecoregion
points_polygon_2 <- st_join(points_polygon, canada_eco_subset, left = TRUE)

# break out new latitude and longitude columns and reformat
points_polygon_3 <- points_polygon_2 %>%
  # break coordinates into lat/long
  mutate(longitude=gsub("\\,.*","", geometry)) %>%
  mutate(latitude=gsub(".*,","",geometry)) %>%
  # format to remove "c(" and  ")"
  mutate(longitude = as.numeric(str_sub(longitude, 3)))  %>% 
  mutate(latitude = as.numeric(str_remove(latitude, "[)]"))) %>% 
  # select columns that match garden accessions
  dplyr::select(X, garden, crop, species, variant, latitude, longitude, country,
         IUCNRedList, province, name.x, ECO_CODE, ECO_NAME) %>%
  rename(new = province) %>% # add a dummy name for province 
  # take province from cd_canada unless was already provided by garden (just want one column)
  mutate(province = ifelse(is.na(new), name.x, new)) %>%
  dplyr::select(-new, - name.x)

# remove points that come from lat/long outside of Canada
# accessions that have latitude and longitude or province
points_polygon_4 <- points_polygon_3 %>%
  filter(province != "") %>%
  filter(country == "Canada")
# accessions that have province but not lat/long
points_polygon_5 <- points_polygon_4 %>%
  filter(is.na(latitude))

# write.csv(points_polygon_3, "all_garden_accessions_with_geo_data.csv")
# write as geojson?

##########
# Part 1B determine density of all accessions with geo data,
# by province and by ecoregion

#################
# heat map, provinces with density/ number of accessions 
# calculate accession density by province
accessions_summarized_by_province <- points_polygon_3 %>%
  # remove all accessions with no associated province data
  # could add a filter here for crop category, crop or CWR taxon
  filter(!is.na(province)) %>% 
  group_by(province) %>%
  add_tally # sum the number of accessions from each province
# join the acession data with the province shapefile
# do this but keep when lat/lomg is NA but still has province name 
# (nrows of join should be same as accessions_summarized_by_province)
# join <- st_join(canada_cd, accessions_summarized_by_province, left = TRUE)
join <- left_join(canada_cd, accessions_summarized_by_province, by = "province")

# format for choropleth map
join2 <- join %>%
  group_by(name) %>% # only want one row representing each province (since every row in same
  # province has the same value for the tally, n)
  filter(row_number() == 1) %>%
  # transform to a log scale since Ontario is orders of magnitude greater than other provinces
  mutate("logn" = log(n))
# Replace provinces with no accessions so that the value is "0" rather than NA
# join2$logn[is.na(join2$logn)] <- 0

# Plot By province
Province_HeatMap <- ggplot() +
  geom_sf(
    aes(fill = logn), size = 0.1, data = join2) +
  scale_fill_distiller(palette = "Spectral") +
  #geom_sf(data = sf_garden_accessions, color = '#001e73', alpha = 0.5, size = 3) + # 17
  coord_sf(crs = crs_string) +
  # scale_fill_manual(values = map_colors) +
  guides(fill = FALSE) +
  theme_map() +
  theme()
  #ggtitle("Known Geographic Origins of Native CWR's in Surveyed Canadian Botanic Gardens") +
  #theme(panel.grid.major = element_line(color = "white"),
        #legend.key = element_rect(color = "gray40", size = 0.1),
        #plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5)
  #)
# add a legend?
Province_HeatMap

##############
# heat map, ecoregioins with density/ number of accessions  
# calculate accession density by ecoregion
accessions_summarized_by_eco <- points_polygon_3 %>%
  # remove all accessions with no ecoregion (lat/long) data
  # could add a filter here for crop category, crop or CWR taxon
  # currently this is for number of accessions rather than unique accessions,
  # e.g. 2 accessions of the same CWR from the same eco = 2 accessions, not 1
  filter(!is.na(ECO_CODE)) %>% 
  group_by(ECO_CODE) %>%
  add_tally # sum the number of accessions from each ecoregion
# join the acession data with the ecoregion shapefile
join_eco <- st_join(canada_eco_subset, accessions_summarized_by_eco, left = TRUE)
# format for heat map
join2_eco <- join_eco %>%
  # only want one row representing each ecoregion (since every row in same
  # ecoregion has the same value for the tally, n)
  group_by(ECO_CODE.x) %>%
  filter(row_number() == 1)  %>% 
  # transform to a log scale since Ontario is orders of magnitude greater than other provinces
  mutate("logn" = log(n))
# Replace ecoregions with no accessions so that the value is "0" rather than NA
# join2_eco$logn[is.na(join2_eco$logn)] <- 0

# Plot By ecoregion
Ecoregion_HeatMap <- ggplot() +
  geom_sf(
    aes(fill = logn), size = 0.1, data = join2_eco) +
  scale_fill_distiller(palette = "Spectral") +
  #geom_sf(data = sf_garden_accessions, color = '#001e73', alpha = 0.5, size = 3) + # 17
  coord_sf(crs = crs_string) +
  # scale_fill_manual(values = map_colors) +
  guides(fill = FALSE) +
  theme_map() +
  theme()
#ggtitle("Known Geographic Origins of Native CWR's in Surveyed Canadian Botanic Gardens") +
#theme(panel.grid.major = element_line(color = "white"),
#legend.key = element_rect(color = "gray40", size = 0.1),
#plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5)
#)
Ecoregion_HeatMap


# choroLayer(x = join2_eco, 
#           var = "logn")
# title("CWR Accessions Per Ecoregion")
# https://www.r-graph-gallery.com/choropleth-map.html


# repeat for natural occurrences in ecoregions 


##### 
# some summary stats about garden accessions
# number of unique CWR taxa in gardens
n_accessions_by_taxa <- garden_accessions %>%
  group_by(species) %>%
  summarise(count=n())
nrow(n_accessions_by_taxa)
# mean number of accessions per taxa (with at least one accession)
(mean(n_accessions_by_taxa$count))
(sd(n_accessions_by_taxa$count))
# number of unique crops with relatives in gardens
n_accessions_by_crop <- garden_accessions %>%
  group_by(crop) %>%
  summarise(count=n())
nrow(n_accessions_by_crop)
# mean number of accessions per crop (with at least one accession)
(mean(n_accessions_by_crop$count))
(sd(n_accessions_by_crop$count))

# bar plot with total CWR taxa versus CWR taxa with >= 1 accession 
Category <- c("Native CWR's", "Native CWR's conserved in surveyed BG's")
CWRs <- as.numeric(c("509", "159")) 
bar_df <- as.data.frame(CWRs, Category) 

R <- ggplot(bar_df, aes(x = Category, y = CWRs)) + 
  geom_bar(stat = "identity") +
  theme_bw() +
  theme(axis.title.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x = element_text(size = 12)
  )
R

##################################
# Part Two
# conduct a GAP ANALYSIS, 
# compare garden accession range ()
# with natural occurrence range (native_occurrence_df)

