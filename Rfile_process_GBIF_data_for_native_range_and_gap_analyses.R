library(tidyverse)
library(sf)

############################################################
# in order to conduct a GAP ANALYSIS, we need to determine the ecoregion and province
# represented by each of the CWR ex situ accessions in our gardens data sets.
# then this ex situ conservation coverage must be compared against the native range.


###################################################################
# Section 1 Load and format GBIF native range data
###################################################################

# Load CWR master list. Length tells us how many taxa in our inventory
cwr_list <- read.csv("./Input_Data_and_Files/master_list_apr_3.csv")
cwr_list <- cwr_list %>% rename("sci_nam" = "sci_name")
number_of_CWRs_in_our_checklist <- nrow(cwr_list)

# the "GBIF_by_province.csv" dataset includes a row for each unique 
# combination of ecoregion and province that a CWR naturally occurs in (given GBIF data), 
# and one coordinate point for each of those unique ecoregion and 
# province combinations to facilitate mapping.

# Load data and format so that it can be changed into a projected shapefile
df <- read.csv("./Input_Data_and_Files/GBIF_long.csv")

# group by species
# and then take one row per native province
GBIF_province_new <- df %>%
  group_by(Column1.sci_nam) %>%
  distinct(Column1.PRENAME, .keep_all = TRUE) %>%
  dplyr::select(Column1.sci_nam, Column1.PRENAME) %>%
  rename("sci_nam" = "Column1.sci_nam", "PRENAME" = "Column1.PRENAME")

GBIF_ecoregion_new <- df %>%
  group_by(Column1.sci_nam) %>%
  distinct(Column1.ECO_NAME, .keep_all = TRUE)%>%
  dplyr::select(Column1.sci_nam, Column1.ECO_NAME) %>%
  rename("sci_nam" = "Column1.sci_nam", "ECO_NAME" = "Column1.ECO_NAME")

native_occurrence_df_province <- left_join(GBIF_province_new, cwr_list, by = "sci_nam")
native_occurrence_df_province_formatted <- native_occurrence_df_province %>%
  rename("province" = "PRENAME", "crop" = "Crop", "species" = "sci_nam") 

native_occurrence_df_ecoregion <- left_join(GBIF_ecoregion_new, cwr_list, by = "sci_nam")
native_occurrence_df_ecoregion_formatted <- native_occurrence_df_ecoregion %>%
  rename("crop" = "Crop", "species" = "sci_nam")

#########################################################################################
# Section 2 Load and format garden collection data                                
#########################################################################################



##########
# compile garden data and append ecoregion or province
# when lat/long was given

# load data from garden collections (already filtered to only CWRs)
# update and add new gardens as we receive additional datasets
cwr_ubc <- read.csv("./Garden_Data/CWR_of_UBC.csv")
cwr_rbg <- read.csv("./Garden_Data/CWR_of_RBG.csv")
cwr_montreal <- read.csv("./Garden_Data/CWR_of_MontrealBG.csv")
cwr_guelph <- read.csv("./Garden_Data/CWR_of_UofGuelph.csv")
cwr_mountp <- read.csv("./Garden_Data/CWR_of_MountPleasantGroup.csv")
cwr_vandusen <- read.csv("./Garden_Data/CWR_of_VanDusenBG.csv")
# cwr_pgrc <- read.csv("./Garden_Data/Amelanchier_PGRC.csv") # removing these subsetted data sets for now
# cwr_usask <- read.csv("Amelanchier_UofSask.csv") # removing these subsetted data sets for now
cwr_readerrock <- read.csv("./Garden_Data/CWR_of_ReaderRock.csv")

# join all garden data into one long table
# update and add new gardens as we receive additional datasets
garden_accessions <- rbind(cwr_ubc, cwr_rbg, cwr_montreal, cwr_guelph, cwr_mountp, cwr_vandusen,
                           cwr_readerrock)
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

####################################################################################
# Section 3 - Load and format shapefile data
####################################################################################

# CRS
crs_string = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs" # 2

# add geojson map with province boundaries 
canada_cd <- st_read("./Geo_Data/canada_provinces.geojson", quiet = TRUE) # 1
canada_cd <- canada_cd %>%
  rename("province" = "name")

# add geojson map with all of canada (no inner boundaries)
# we will use this as a boundary for trimming all the ecoregion maps
canada <- st_read("./Geo_Data/canada.geojson", quiet = TRUE) # 1

# add geojson map with ecoregion boundaries
world_eco <- st_read("./Geo_Data/world_ecoregions.geojson", quiet = TRUE)
# Trim geojson world map to canada ecoregions from native_occurrence_df
canada_eco <- semi_join(world_eco, native_occurrence_df_ecoregion, by=("ECO_NAME")) 

# clip ecoregions to canada national border
canada_eco_subset <- st_intersection(canada_eco, canada)
#geojsonio::geojson_write(canada_eco_subset, file = "canada_ecoregions_clipped.geojson")


######################################################################################
# Section 4 - Project Garden Accessions, Append Geo Data, Format for outputs         #
######################################################################################

# Append Province to accession using lat and longitude
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

# gardens often give province but no lat/long
accessions_w_province_but_no_geo_data <- all_garden_accessions_shapefile %>%
  filter(!is.na(province)) %>%
  filter(is.na(latitude))
# the province layers don't always catch coastal/island collections bounded by ecoregion
# manually edit these accessions afterwards?
accessions_w_ecoregion_but_no_province <- all_garden_accessions_shapefile %>%
  filter(!is.na(ECO_NAME)) %>%
  filter(is.na(province))


province_gap_table <- native_occurrence_df_province_formatted %>%
  # need to rejoin with CWR list to get species crop categories and crop for species that weren't in the range maps
  full_join(cwr_list, by = c("species" = "sci_nam")) %>%
  dplyr::select(-Group.x, -crop) %>%
  rename("Group" = "Group.y", "crop" = "Crop") %>%
  full_join(all_garden_accessions_shapefile) 

ecoregion_gap_table <- native_occurrence_df_ecoregion_formatted %>%
  # need to rejoin with CWR list to get species crop categories and crop for species that weren't in the range maps
  full_join(cwr_list, by = c("species" = "sci_nam")) %>%
  dplyr::select(-Group.x, -crop) %>%
  rename("Group" = "Group.y", "crop" = "Crop") %>%
  full_join(all_garden_accessions_shapefile) 


#################################################################################
# Section 5 Write Output Files                                                  #
#################################################################################

# unselect when these files need to be overwritten
# geojsonio::geojson_write(canada_eco_subset, file = "./Geo_Data/canada_ecoregions_clipped.geojson")
# write.csv(province_gap_table, "./Output_Data_and_Files/province_gap_table.csv")
# write.csv(ecoregion_gap_table, "./Output_Data_and_Files/ecoregion_gap_table.csv")

# note: delete "Oxycoccus sp." and "Julans sp." rows which were errors in the original list that was used to generate range maps and attaches to the gap tables
