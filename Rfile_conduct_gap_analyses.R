# INTRO (EDIT THIS)

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
library(viridis)
library(tigris)

######################################################################################

######################################################################################

# Load required data and shapefiles for building reactive maps and data tables

canada_ecoregions_geojson <- st_read("./Geo_Data/canada_ecoregions_clipped.geojson", quiet = TRUE)
canada_provinces_geojson <- st_read("./Geo_Data/canada_provinces.geojson", quiet = TRUE)

province_gap_table <- as_tibble(read.csv("./Output_Data_and_Files/province_gap_table.csv"))
ecoregion_gap_table <- as_tibble(read.csv("./Output_Data_and_Files/ecoregion_gap_table.csv"))

# CRS 
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

############################################################################
# Reformat Gap Tables So That Garden Points Can Be Projected alternates
############################################################################

province_gap_table <- province_gap_table %>%
  dplyr::select(-geometry, -X, -X.1, -ECO_CODE, -ECO_NAME)
  
province_gap_table_sf <- st_as_sf(province_gap_table, 
                                  coords = c("longitude", "latitude"), 
                                  crs = 4326, 
                                  na.fail = FALSE)

ecoregion_gap_table <- ecoregion_gap_table %>%
  dplyr::select(-geometry, -X, -X.1, -province)

ecoregion_gap_table_sf <- st_as_sf(ecoregion_gap_table, 
                                  coords = c("longitude", "latitude"), 
                                  crs = 4326, 
                                  na.fail = FALSE)

# For P and Q Maybe Also add points that represent our surveyed gardens?

# Plot By province
Q <- ggplot() +
  geom_sf(
    # aes(fill = name), 
    color = "gray60", size = 0.1, data = canada_provinces_geojson) +
  geom_sf(data = province_gap_table_sf, color = '#001e73', alpha = 0.5, size = 3) + # 17
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


# Plot Ecoregions
P <- ggplot() +
  geom_sf(
    # aes(fill = name), 
    color = "gray60", size = 0.1, data = canada_ecoregions_geojson) +
  geom_sf(data = ecoregion_gap_table_sf, color = '#001e73', alpha = 0.5, size = 3) + # 17
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


######################################################################################
#
######################################################################################






















# Append Province to accession using lat and longitude
points_sf = st_transform( st_as_sf(sf_garden_accessions), 
                          coords = c("longitude", "latitude"), 
                          crs = 4326, agr = "constant")
# spatial join to add accession province
points_polygon <- st_join(sf_garden_accessions, canada_cd, left = TRUE)
# spatial join to add accession ecoregion
points_polygon_2 <- st_join(points_polygon, canada_eco_subset, left = TRUE)

# break out new latitude and longitude columns and reformat
all_garden_accessions_shapefile <- points_polygon_2 %>%
  # break coordinates into lat/long
  mutate(longitude=gsub("\\,.*","", geometry)) %>%
  mutate(latitude=gsub(".*,","",geometry)) %>%
  # format to remove "c(" and  ")"
  mutate(longitude = as.numeric(str_sub(longitude, 3)))  %>% 
  mutate(latitude = as.numeric(str_remove(latitude, "[)]"))) %>% 
  # select columns that match garden accessions
  dplyr::select(X, garden, crop, species, variant, latitude, longitude, country,
         IUCNRedList, province.x, province.y, ECO_CODE, ECO_NAME) %>%
  #rename(new = province) %>% # add a dummy name for province 
  # take province from cd_canada unless was already provided by garden (just want one column)
  mutate(province = ifelse(is.na(province.x), province.y, province.x)) %>%
  dplyr::select(-province.y, - province.x)

# accessions that have province but not lat/long
accessions_w_province_but_no_geo_data <- all_garden_accessions_shapefile %>%
  filter(!is.na(province)) %>%
  filter(is.na(latitude))

# write.csv(all_garden_accessions_shapefile, "all_garden_accessions_with_geo_data.csv")
# write as geojson?

##########
# Part 1B determine density of all accessions with geo data,
# by province and by ecoregion

#################
# heat map, provinces with density/ number of accessions 
# calculate accession density by province
accessions_summarized_by_province <- all_garden_accessions_shapefile %>%
  # remove all accessions with no associated province data
  # could add a filter here for crop category, crop or CWR taxon
  filter(!is.na(province)) %>% 
  group_by(province) %>%
  add_tally # sum the number of accessions from each province
# join the acession data with the province shapefile
# do this but keep when lat/lomg is NA but still has province name 
# (nrows of join should be same as accessions_summarized_by_province)
join <- st_join(canada_cd, accessions_summarized_by_province, left = TRUE)

# format for choropleth map
join2 <- join %>%
  group_by(province.x) %>% # only want one row representing each province (since every row in same
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
accessions_summarized_by_eco <- all_garden_accessions_shapefile %>%
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

# change shapefile to data frame format for join (not sure if I want df or shapefile)
df_all_accessions_with_provinces_and_eco <- as.data.frame(all_garden_accessions_shapefile)
all_garden_accessions_shapefile
# join all_accessions with native provinces

native_occurrence_df_province_formatted <- native_occurrence_df %>%
  rename("province" = "PRENAME", "crop" = "Crop", "species" = "sci_nam") %>%
  # drop eco_name and eco_code
  dplyr::select(-latitude, -longitude, -ECO_NAME, -ECO_CODE) %>%
  # now delete any repeated rows within CWR (caused by multiple ecoregions in each province)
  group_by(crop, species) %>%
  distinct(province)
  
native_occurrence_df_ecoregion_formatted <- native_occurrence_df %>%
  rename("province" = "PRENAME", "crop" = "Crop", "species" = "sci_nam") %>%
  # drop province
  dplyr::select(-latitude, -longitude, -province) %>%
  # now delete any repeated rows within CWR (caused by ecoregions spanning multiple province)
  group_by(crop, species) %>%
  distinct(ECO_NAME)


province_gap_table <- full_join(native_occurrence_df_province_formatted, all_garden_accessions_shapefile)
ecoregion_gap_table <- full_join(native_occurrence_df_ecoregion_formatted, all_garden_accessions_shapefile)
write.csv(province_gap_table, "province_gap_table.csv")
write.csv(ecoregion_gap_table, "ecoregion_gap_table.csv")

# full_gap_table_geojson <- tigris::geo_join( , full_gap_table, by = "province")
# geojsonio::geojson_write(full_gap_table_geojson, file = "full_gap_table.geojson")

full_gap_table_analysis <- full_gap_table %>%
  group_by(ECO_CODE) %>%
  # tally the number of rows in each ecoregion with an existing accession (garden is not NA)
  add_tally(!is.na(garden)) %>%
  rename("accessions_in_ecoregion" = "n") %>%
  group_by(province) %>%
  # tally the number of rows in each ecoregion with an existing accession (garden is not NA)
  add_tally(!is.na(garden)) %>%
  rename("accessions_in_province" = "n") 


##############################
# example code for a single species gap analysis
# in the shiny app, allow user to select this function for province or the ecoregion function for ecoregion
species_gap_analyis_by_province <- full_gap_table %>%
  # in the shiny app, this filter will be the user input
  filter(species == "Amelanchier alnifolia") %>%
  group_by(province) %>%
  # tally the number of rows in each ecoregion with an existing accession (garden is not NA)
  add_tally(!is.na(garden)) %>%
  rename("accessions_in_province" = "n")  %>%
  ungroup() %>%
  # maybe add an if else statement here, so if there's either zero accessions or at least 1?
  mutate(total_accessions_for_species = sum(!is.na(garden))) %>%
  mutate(accessions_no_geo_data = sum(is.na(province))) %>%
  mutate(accessions_with_geo_data = sum(!is.na(province))) %>%
  group_by(province) %>%
  filter(row_number() == 1) %>%
  filter(!is.na(province)) %>%
  # find number of accessions where province = NA and add this as a universal col
  # drop rows where province = NA
  mutate(binary = ifelse(
    accessions_in_province > 0, 1, 0)) %>%
  ungroup() %>%
  mutate(num_native_provinces = sum(!duplicated(province))) %>%
  mutate(num_covered_provinces = sum(binary)) %>%
  mutate(perc_provincial_range_covered = 
           num_covered_provinces / num_native_provinces) %>%
  dplyr::select(-country, -geometry, -latitude, -longitude, 
                -garden, -X, -ECO_CODE, -ECO_NAME)

species_gap_analyis_by_province_sf <- tigris::geo_join(canada_cd, species_gap_analyis_by_province, by = "province")

Z <- ggplot() +
  geom_sf(
    aes(fill = binary), 
    color = "gray60", size = 0.1, data = species_gap_analyis_by_province_sf) +
  coord_sf(crs = crs_string) +
  scale_fill_distiller(palette = "Spectral") +
  guides(fill = FALSE) +
  theme_map() +
  ggtitle("Conservation of Species X Across Native Range in Canadian Botanic Gardens") +
  theme(panel.grid.major = element_line(color = "white"),
        legend.key = element_rect(color = "gray40", size = 0.1),
        plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5)
  )
Z


# example df
# write.csv(species_gap_analyis_by_province, "species_gap_analyis_by_province.csv")

species_gap_analyis_by_ecoregion <- full_gap_table %>%
  # in the shiny app, this filter will be the user input
  filter(species == "Amelanchier alnifolia") %>%
  group_by(ECO_NAME) %>%
  # tally the number of rows in each ecoregion with an existing accession (garden is not NA)
  add_tally(!is.na(garden)) %>%
  rename("accessions_in_ecoregion" = "n")  %>%
  ungroup() %>%
  # maybe add an if else statement here, so if there's either zero accessions or at least 1?
  mutate(total_accessions_for_species = sum(!is.na(garden))) %>%
  mutate(accessions_no_geo_data = sum(is.na(ECO_NAME))) %>%
  mutate(accessions_with_geo_data = sum(!is.na(ECO_NAME))) %>%
  group_by(ECO_NAME) %>%
  filter(row_number() == 1) %>%
  filter(!is.na(ECO_NAME)) %>%
  # find number of accessions where ECO_NAME = NA and add this as a universal col
  # drop rows where ECO_NAME = NA
  mutate(binary = ifelse(
    accessions_in_ecoregion > 0, 1, 0)) %>%
  ungroup() %>%
  mutate(num_native_ecoregions = sum(!duplicated(ECO_NAME))) %>%
  mutate(num_covered_ecoregions = sum(binary)) %>%
  mutate(perc_ecoregion_range_covered = 
           num_covered_ecoregions / num_native_ecoregions) %>%
  dplyr::select(-country, -geometry, -latitude, -longitude, 
                -garden, -X, -province)

species_gap_analyis_by_ecoregion_sf <- tigris::geo_join(canada_eco_subset, species_gap_analyis_by_ecoregion, by = "ECO_NAME")

ZZ <- ggplot() +
  geom_sf(
    aes(fill = binary), 
    color = "gray60", size = 0.1, data = species_gap_analyis_by_ecoregion_sf) +
  coord_sf(crs = crs_string) +
  scale_fill_distiller(palette = "Spectral") +
  guides(fill = FALSE) +
  theme_map() +
  ggtitle("Conservation of Species X Across Native Range in Canadian Botanic Gardens") +
  theme(panel.grid.major = element_line(color = "white"),
        legend.key = element_rect(color = "gray40", size = 0.1),
        plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5)
  )
ZZ

######################
# Native Occurrence  #
######################

province_gap_table <- as_tibble(read.csv("province_gap_table.csv"))
ecoregion_gap_table <- as_tibble(read.csv("ecoregion_gap_table.csv"))
# order gap tables so that user choices are alphabetically organized
province_gap_table <- province_gap_table[order(province_gap_table$crop),]
ecoregion_gap_table <- ecoregion_gap_table[order(ecoregion_gap_table$crop),]

# native occurrence heatmap
# by province
native_occurrence_heatmap_provinces <- province_gap_table %>%
  # filter for garden = NA
  filter(is.na(garden)) %>%
  # group by province
  group_by(province) %>%
  # tally the number of species
  add_tally() %>%
  rename("native_crop_wild_relatives" = "n") %>%
  ungroup() %>%
  # identify endemic species per province
  # species that occur in only one province
  group_by(species) %>%
  # if group is only one row, endemic = 1, else endemic = 0
  add_tally() %>%
  rename("native_provinces_for_species" = "n") %>%
  mutate(is_endemic = ifelse(
    native_provinces_for_species == 1, 1, 0)) %>%
  ungroup() %>%
  group_by(province) %>%
  mutate(total_endemics_in_province = sum(is_endemic))
  # for the shiny/report we could generate filtered tables of narrow endemics?


native_occurrence_sf_provinces <- tigris::geo_join(canada_cd, native_occurrence_heatmap_provinces, by = "province")

Natural_Occurrence_Province_Heatmap <- ggplot() +
  geom_sf(
    aes(fill = native_crop_wild_relatives), size = 0.1, data = native_occurrence_sf_provinces) +
  scale_fill_distiller(palette = "Spectral") +
  coord_sf(crs = crs_string) +
  guides() +
  theme_map() +
  theme(legend.position = c(-.47,0))
Natural_Occurrence_Province_Heatmap

# by ecoregion
native_occurrence_heatmap_ecoregion <- ecoregion_gap_table %>%
  # filter for garden = NA
  filter(is.na(garden)) %>%
  # group by province
  group_by(ECO_NAME) %>%
  # tally the number of species
  add_tally() %>%
  rename("native_crop_wild_relatives" = "n") %>%
  ungroup() %>%
  # identify endemic species per province
  # species that occur in only one province
  group_by(species) %>%
  # if group is only one row, endemic = 1, else endemic = 0
  add_tally() %>%
  rename("native_ecoregions_for_species" = "n") %>%
  mutate(is_endemic = ifelse(
    native_ecoregions_for_species == 1, 1, 0)) %>%
  ungroup() %>%
  group_by(ECO_NAME) %>%
  mutate(total_endemics_in_ecoregion = sum(is_endemic))
  # for the shiny/report we could generate filtered tables of narrow endemics?


native_occurrence_sf_ecoregions <- tigris::geo_join(canada_eco_subset, native_occurrence_heatmap_ecoregion, by = "ECO_NAME")

Natural_Occurrence_Ecoregion_Heatmap <- ggplot() +
  geom_sf(
    aes(fill = native_crop_wild_relatives), size = 0.1, data = native_occurrence_sf_ecoregions) +
  scale_fill_distiller(palette = "Spectral") +
  coord_sf(crs = crs_string) +
  guides() +
  theme_map() +
  theme(legend.position = c(-.47,0))
Natural_Occurrence_Ecoregion_Heatmap

