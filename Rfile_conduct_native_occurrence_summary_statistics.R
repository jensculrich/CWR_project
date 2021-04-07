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
cwr_list <- read.csv("./Input_Data_and_Files/master_list_apr_3.csv")

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


##############################
# Explore Summary Statistics #
##############################

##############################
# CWRs in each category
cwr_list_summary <- cwr_list %>%
  group_by(Group) %>% 
  add_tally() %>%
  distinct(Group, .keep_all = TRUE ) %>%
  arrange(desc(n)) %>%
  dplyr::select(Group, n) 


par(mar=c(4,11,4,4))
barplot(cwr_list_summary$n, main = "Native CWR Taxa in Broad Crop Categories",
        names.arg = cwr_list_summary$Group, xlab = "", ylab = "",
        cex.names=0.8, horiz=T, las=1, xlim = c(0,140))


##############################
# regional richness and endemics

# find ecoregions with the most total native CWRs
# and most endemic native CWRs
total_and_endemic_CWRs_ecoregion <- ecoregion_gap_table_sf %>%
  # count total CWRs (unique sci_name in each ecoregion)
  # want the rows where garden is NA (just the range data)
  filter(is.na(garden)) %>%
  # group by ecoregion
  group_by(ECO_NAME) %>%
  # tally the number of unique CWR species
  distinct(species, .keep_all = TRUE) %>%
  add_tally() %>%
  rename(total_CWRs_in_ecoregion = "n") %>%
  mutate(total_CWRs_in_ecoregion = as.numeric(total_CWRs_in_ecoregion)) %>%
  ungroup() %>%
  
  # count endemic CWRs (species that occurs in only 1 ecoregion)
  group_by(species) %>%
  # if group is only one row, endemic = 1, else endemic = 0
  add_tally() %>%
  rename("native_ecoregions_for_species" = "n") %>%
  mutate(is_endemic = ifelse(
    native_ecoregions_for_species == 1, 1, 0)) %>%
  ungroup() %>%
  group_by(ECO_NAME) %>%
  mutate(endemic_CWRs_in_ecoregion = sum(is_endemic))

# just want number of CWRS in each region
# for a histogram and to easily see ranked list of top ecoregions
# by total CWRs:
total_CWRs_group_by_ecoregion <- total_and_endemic_CWRs_ecoregion %>% 
  distinct(ECO_NAME, .keep_all = TRUE ) %>%
  arrange(desc(total_CWRs_in_ecoregion))
# and by endemic CWRs:
total_CWRs_group_by_ecoregion <- total_CWRs_group_by_ecoregion %>% 
  arrange(desc(endemic_CWRs_in_ecoregion))

# Plot number CWRs in each province (as a histogram)
P <- ggplot(total_CWRs_group_by_ecoregion, aes(x = total_CWRs_in_ecoregion)) + theme_bw() + 
  geom_histogram()
P

# Go ahead and add leaflet and/or heatmap here for ecoregions
# using the total_and_endemic_CWRs_ecoregion table?


# find provinces with the most total native CWRs
# and most endemic native CWRs
total_and_endemic_CWRs_province <- province_gap_table_sf %>%
  # count total CWRs (unique sci_name in each province)
  # want the rows where garden is NA (just the range data)
  filter(is.na(garden)) %>%
  # group by province
  group_by(province) %>%
  # tally the number of unique CWR species
  distinct(species, .keep_all = TRUE) %>%
  add_tally() %>%
  rename(total_CWRs_in_province = "n") %>%
  mutate(total_CWRs_in_province = as.numeric(total_CWRs_in_province)) %>%
  ungroup() %>%
  
  # count endemic CWRs (species that occurs in only 1 province)
  group_by(species) %>%
  # if group is only one row, endemic = 1, else endemic = 0
  add_tally() %>%
  rename("native_provinces_for_species" = "n") %>%
  mutate(is_endemic = ifelse(
    native_provinces_for_species == 1, 1, 0)) %>%
  ungroup() %>%
  group_by(province) %>%
  mutate(endemic_CWRs_in_province = sum(is_endemic))

# just want number of CWRS in each region
# for a histogram and to easily see ranked list of top ecoregions
# by total CWRs:
total_CWRs_group_by_province <- total_and_endemic_CWRs_province %>% 
  distinct(province, .keep_all = TRUE ) %>%
  arrange(desc(total_CWRs_in_province))
# and by endemic CWRs:
total_CWRs_group_by_province <- total_CWRs_group_by_province %>% 
  arrange(desc(endemic_CWRs_in_province))

# Plot number CWRs in each province (as a histogram)
Q <- ggplot(total_CWRs_group_by_province, aes(x = total_CWRs_in_province)) + theme_bw() + 
  geom_histogram()
Q

##############################
# regional richness and endemics by crop category

# for each category:
# filter to category
# then...
# find ecoregions with the most total native CWRs
# and most endemic native CWRs
find_native_cwrs_by_group_ecoregion <- function(x) {
  temp <- ecoregion_gap_table %>%
    filter(Group == x)  %>%
    # count total CWRs (unique sci_name in each ecoregion)
    # want the rows where garden is NA (just the range data)
    filter(is.na(garden)) %>%
    # group by ecoregion
    group_by(ECO_NAME) %>%
    # tally the number of unique CWR species
    distinct(species, .keep_all = TRUE) %>%
    add_tally() %>%
    rename(total_CWRs_in_ecoregion = "n") %>%
    mutate(total_CWRs_in_ecoregion = as.numeric(total_CWRs_in_ecoregion)) %>%
    ungroup() %>%
    
    # count endemic CWRs (species that occurs in only 1 ecoregion)
    group_by(species) %>%
    # if group is only one row, endemic = 1, else endemic = 0
    add_tally() %>%
    rename("native_ecoregions_for_species" = "n") %>%
    mutate(is_endemic = ifelse(
      native_ecoregions_for_species == 1, 1, 0)) %>%
    ungroup() %>%
    group_by(ECO_NAME) %>%
    mutate(endemic_CWRs_in_ecoregion = sum(is_endemic)) %>%
    
    distinct(ECO_NAME, .keep_all = TRUE ) %>%
    arrange(desc(total_CWRs_in_ecoregion))
    
  return(as_tibble(temp))
  
} 


native_cwrs_by_group_ecoregion <- data.frame("species" = character(),
                 "ECO_CODE"= character(), 
                 "ECO_NAME" = character(), 
                 "Group" = character(), 
                 "crop" = character(), 
                 "garden" = character(),
                 "variant" = character(),
                 "country" = character(), 
                 "IUCNRedList" = character(), 
                 "geometry" = character(), 
                 "total_CWRs_in_ecoregion" = character(),
                 "native_ecoregions_for_species" = character(),
                 "is_endemic" = character(), 
                 "endemic_CWRs_in_ecoregion" = character(),
                 stringsAsFactors=FALSE)


for(i in 1:9) {
  
  group_name <- cwr_list_summary[[i,1]]
  as.data.frame(temp <- find_native_cwrs_by_group_ecoregion(
    x = group_name)) 
  native_cwrs_by_group_ecoregion <- rbind(
    native_cwrs_by_group_ecoregion, temp)

} 

hotspots_by_crop_category_ecoregion <- native_cwrs_by_group_ecoregion %>%
  dplyr::select(-species, -crop, -variant, -latitude, -longitude, 
                -country, -garden, -IUCNRedList, -ECO_CODE) %>%
  group_by(Group) %>%
  # keep row with max total_CWRs_in_ecoregion
  slice(which.max(total_CWRs_in_ecoregion))
  
#################
# by province
# for each category:
# filter to category
# then...
# find province with the most total native CWRs
# and most endemic native CWRs
find_native_cwrs_by_group_province <- function(x) {
  temp <- province_gap_table %>%
    filter(Group == x)  %>%
    # count total CWRs (unique sci_name in each province)
    # want the rows where garden is NA (just the range data)
    filter(is.na(garden)) %>%
    # group by province
    group_by(province) %>%
    # tally the number of unique CWR species
    distinct(species, .keep_all = TRUE) %>%
    add_tally() %>%
    rename(total_CWRs_in_province = "n") %>%
    mutate(total_CWRs_in_province = as.numeric(total_CWRs_in_province)) %>%
    ungroup() %>%
    
    # count endemic CWRs (species that occurs in only 1 province)
    group_by(species) %>%
    # if group is only one row, endemic = 1, else endemic = 0
    add_tally() %>%
    rename("native_province_for_species" = "n") %>%
    mutate(is_endemic = ifelse(
      native_province_for_species == 1, 1, 0)) %>%
    ungroup() %>%
    group_by(province) %>%
    mutate(endemic_CWRs_in_province = sum(is_endemic)) %>%
    
    distinct(province, .keep_all = TRUE ) %>%
    arrange(desc(total_CWRs_in_province))
  
  return(as_tibble(temp))
  
} 


native_cwrs_by_group_province <- data.frame("species" = character(),
                                             "province"= character(), 
                                             "Group" = character(), 
                                             "crop" = character(), 
                                             "garden" = character(),
                                             "variant" = character(),
                                             "country" = character(), 
                                             "IUCNRedList" = character(), 
                                             "geometry" = character(), 
                                             "total_CWRs_in_ecoregion" = character(),
                                             "native_ecoregions_for_species" = character(),
                                             "is_endemic" = character(), 
                                             "endemic_CWRs_in_ecoregion" = character(),
                                             stringsAsFactors=FALSE)


for(i in 1:9) {
  
  group_name <- cwr_list_summary[[i,1]]
  as.data.frame(temp <- find_native_cwrs_by_group_province(
    x = group_name)) 
  native_cwrs_by_group_province <- rbind(
    native_cwrs_by_group_province, temp)
  
} 

hotspots_by_crop_category_province <- native_cwrs_by_group_province %>%
  dplyr::select(-species, -crop, -variant, -latitude, -longitude, 
                -country, -garden, -IUCNRedList) %>%
  group_by(Group) %>%
  # keep row with max total_CWRs_in_province
  slice(which.max(total_CWRs_in_province))




# Everything through here has been checked










# province with the most CWRs
df5 <- Native_Range_DF %>%
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
df6 <- Native_Range_DF %>%
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

# show five ecoregions with most CWRs
top_n(Amelanchier_group_by_ecoregion, 5, wt = Amelanchier_relatives)
# show five ecoregions with least CWRs
top_n(Amelanchier_group_by_ecoregion, -5, wt = Amelanchier_relatives) 


# province with the most Amelanchier CWRs
df7 <- Native_Range_DF %>%
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
plot(Native_Range_DF$long, Native_Range_DF$lat)

