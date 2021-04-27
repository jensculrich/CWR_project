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
library(gridExtra)

######################################################################################
# 1 INPUT DATA
######################################################################################

# Load required data and shapefiles for building reactive maps and data tables
cwr_list <- read.csv("./Input_Data_and_Files/master_list_apr_3.csv")
# CWRs in each category
cwr_list_summary <- cwr_list %>%
  group_by(Group) %>% 
  add_tally() %>%
  distinct(Group, .keep_all = TRUE ) %>%
  arrange(desc(n)) %>%
  dplyr::select(Group, n) 

canada_ecoregions_geojson <- st_read("./Geo_Data/canada_ecoregions_clipped.geojson", quiet = TRUE)
canada_provinces_geojson <- st_read("./Geo_Data/canada_provinces.geojson", quiet = TRUE)

province_gap_table <- as_tibble(read.csv("./Output_Data_and_Files/province_gap_table_post_manual_range_edits.csv"))
ecoregion_gap_table <- as_tibble(read.csv("./Output_Data_and_Files/ecoregion_gap_table_post_manual_range_edits.csv"))

garden_list <- as_tibble(read.csv("./Input_Data_and_Files/garden_list.csv")) %>%
  mutate(type = "Surveyed Garden")
  
# CRS 
crs_string = "+proj=lcc +lat_1=49 +lat_2=77 +lon_0=-91.52 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs" # 2

# Define the maps' theme -- remove axes, ticks, borders, legends, etc.
theme_map <- function(base_size=10, base_family="") { # 3
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
  dplyr::select(-geometry, -X, -ECO_CODE, -ECO_NAME) %>%
  mutate(type = "Origin of CWR Garden Accession")
  
province_gap_table_sf <- st_as_sf(province_gap_table, 
                                  coords = c("longitude", "latitude"), 
                                  crs = 4326, 
                                  na.fail = FALSE)
  # could filter to country = canada here

ecoregion_gap_table <- ecoregion_gap_table %>%
  dplyr::select(-geometry, -X, -province) %>%
  mutate(type = "Origin of CWR Garden Accession")

ecoregion_gap_table_sf <- st_as_sf(ecoregion_gap_table, 
                                  coords = c("longitude", "latitude"), 
                                  crs = 4326, 
                                  na.fail = FALSE)
  # could filter to country = canada here

garden_list_sf <- st_as_sf(garden_list, 
                                   coords = c("longitude", "latitude"), 
                                   crs = 4326, 
                                   na.fail = FALSE)

#################################
#   Map all garden accessions   #
#################################
# filter to species for which we had a range map
province_gap_table_sf_in_Canada <- province_gap_table_sf %>%
  filter(country == "Canada" | is.na(country))

cols <- c("CWR geographical origin" = "blue", "Surveyed Garden" = "red")
# Plot By province
P <- ggplot() +
  geom_sf(
    color = "gray60", size = 0.1, data = canada_provinces_geojson) +
  geom_sf(data = province_gap_table_sf_in_Canada, aes(color = type), alpha = 0.5, size = 2, 
          show.legend = TRUE) + # 17
  geom_sf(data = garden_list_sf, aes(color = type), alpha = 0.7, size = 3) +
  coord_sf(crs = crs_string) +
  theme_map() +
  ggtitle("") +
  scale_fill_manual(values = cols) +
  labs(color='') + 
  theme(panel.grid.major = element_line(color = "white"),
        legend.key = element_rect(color = "gray40", size = 0.1),
        plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5),
        plot.margin=unit(c(0, -2, 0, -2), "cm"),
        legend.text = element_text(size=11),
        legend.background = element_rect(fill = "transparent")
  )
P

# Plot Ecoregions
Q <- ggplot() +
  geom_sf(
    # aes(fill = name), 
    color = "gray60", size = 0.1, data = canada_ecoregions_geojson) +
  geom_sf(data = ecoregion_gap_table_sf, aes(color = type), alpha = 0.5, size = 2) + # 17
  geom_sf(data = garden_list_sf, aes(color = type), alpha = 0.7, size = 3) +
  coord_sf(crs = crs_string) +
  theme_map() +
  ggtitle("") +
  scale_fill_manual(values = cols) +
  labs(color='') + 
  theme(panel.grid.major = element_line(color = "white"),
        legend.key = element_rect(color = "gray40", size = 0.1),
        plot.title = element_text(color="black", size=10, face="bold.italic", hjust = 0.5),
        plot.margin=unit(c(0, -2, 0, -2), "cm"),
        legend.text = element_text(size=11),
        legend.background = element_rect(fill = "transparent")
  )
Q


######################################################################################
#
######################################################################################




######################################################################################
#
######################################################################################
# Gap Analysis By Province
# Need to run this across all taxa

# filter to species for which we had a range map
province_gap_table_in_Canada <- province_gap_table %>%
  filter(country == "Canada" | is.na(country))

# define the gap analysis function
province_gap_analysis <- function(taxon) {
  
  provinceTableData <- province_gap_table_in_Canada %>%
    # filter the table to the selected CWR
    filter(province_gap_table_in_Canada$species == taxon) %>%
    
    # tally the number of rows in each province with an existing accession (garden is not NA)
    group_by(province) %>%
    add_tally(!is.na(garden)) %>%
    rename("accessions_in_province" = "n")  %>%
    ungroup() %>%
    
    # count the number of accessions w/ and w/out geographic data
    mutate(accessions_no_geo_data = sum(is.na(province))) %>%
    mutate(accessions_with_geo_data = sum(!is.na(province) & !is.na(garden)))  %>%
    mutate(total_accessions_for_species = accessions_with_geo_data + accessions_no_geo_data) %>%
    
    # convert number of accessions (per province) to a binary "is there or is there not an accession from x region"
    group_by(province) %>%
    filter(row_number() == 1) %>%
    filter(!is.na(province)) %>%
    mutate(binary = ifelse(
    accessions_in_province > 0, 1, 0)) %>%
    ungroup() %>%
    
    # use the binary variable to determine the proportion of native regions with an accession
    mutate(num_native_province = sum(!duplicated(province))) %>%
    mutate(num_covered_province = sum(binary)) %>%
    mutate(perc_province_range_covered = 
           (num_covered_province / num_native_province)) %>%
    
    # convert number of accessions (overall) to a binary "is there or is there not an accession from x region"
    mutate(at_least_one_accession = ifelse(
      sum(total_accessions_for_species) > 0, 1, 0)) %>%
    
    # format the data for the summary table 
    filter(row_number() == 1) %>% # for now only want one row (could adjust this with row per province)
    dplyr::select(Group, crop, species, num_native_province, num_covered_province, perc_province_range_covered, 
                accessions_with_geo_data, accessions_no_geo_data, 
                total_accessions_for_species, at_least_one_accession) %>%
    mutate(num_covered_province = as.integer(num_covered_province)) %>%
    rename("native provinces" = num_native_province,
         "covered provinces" = num_covered_province,
         "accessions with geographic data" = accessions_with_geo_data,
         "accessions lacking geographic data" = accessions_no_geo_data,
         "total accessions" = total_accessions_for_species)

}

# make an empty tibble that will be filled out with species rows
gap_analysis_df_by_province <- data.frame("Group" = character(),
                                             "crop"= character(), 
                                             "species" = character(), 
                                             "native provinces" = character(), 
                                             "covered provinces" = character(), 
                                             "accessions with geographic data" = character(),
                                             "accessions lacking geographic data" = character(),
                                             stringsAsFactors=FALSE)

# for each species with a range,
# determine which regions in the range are represented by 1 or more accession
# and then determine the proportion of native regions with 1 or more accession
for(i in 1:nrow(cwr_list)) {
  
  selected_taxon <- cwr_list[[i,3]] # r is species ("sci_name")
  as.data.frame(temp <- province_gap_analysis(
    taxon = selected_taxon)) 
  gap_analysis_df_by_province <- rbind(
    gap_analysis_df_by_province, temp)
  
} 

# group by crop category and determine 
R <- ggplot(gap_analysis_df_by_province, aes(perc_province_range_covered, Group)) + 
  geom_boxplot(outlier.shape = NA) + geom_jitter() + 
  xlab("Proportion of native provinces represented in garden collections") + ylab("") +
  theme_bw() +
  scale_x_continuous(labels = function(x) paste0(x*100, "%")) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14), 
        axis.title.x = element_text(size = 14))
R

# what's the mean proportion?
(mean_range <- mean(gap_analysis_df_by_province$perc_province_range_covered))
(sd_range <- sd(gap_analysis_df_by_province$perc_province_range_covered))

# mean and sd by group 
group_means_sd <- gap_analysis_df_by_province %>%
  group_by(Group) %>%
  mutate(mean_range = mean(perc_province_range_covered)) %>%
  mutate(sd_range = sd(perc_province_range_covered)) %>%
  distinct(Group, .keep_all = TRUE)

# gap analysis w/out respect to geo data
# ie how many species have at least one accession in one of our gardens?
# by crop category

gap_analysis_df_by_province_2 <- province_gap_table_in_Canada %>%
# tally the number of rows in each province with an existing accession (garden is not NA)
  group_by(species) %>%
  add_tally(!is.na(garden)) %>%
  rename("accessions_for_species" = "n")  %>%
  ungroup() %>%

  # convert number of accessions (per species) to a binary "is there or is there not an accession from x region"
  group_by(species) %>%
  filter(row_number() == 1) %>%
  # filter(!is.na(province)) %>%
  mutate(one_or_more_accession = ifelse(
    accessions_for_species > 0, 1, 0)) %>%
  ungroup() %>%

  group_by(Group) %>%
  mutate(sum_species_w_accessions = sum(one_or_more_accession)) %>%
  add_tally %>%
  mutate(proportion_species_conserved = sum_species_w_accessions / n) %>%
  ungroup()

gap_analysis_df_by_province_3 <- gap_analysis_df_by_province_2 %>%
  distinct(Group, .keep_all = TRUE)

S <- ggplot(gap_analysis_df_by_province_3, aes(x = Group,
                                               y = proportion_species_conserved)) +
  geom_bar(stat = "identity") + 
  theme_bw() +
  coord_flip() +
  ylab("Proportion of CWRs Represented in Garden Collections") + xlab("") +
  scale_y_continuous(labels = function(x) paste0(x*100, "%"), limits = c(0, 1)) +
  theme(axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14), 
        axis.title.x = element_text(size = 14))
S

# across all taxa
gap_analysis_df_by_province_4 <- gap_analysis_df_by_province_2 %>%
  add_tally(name = "total_num_CWRs") %>%
  mutate(total_proportion_with_an_accession = sum(one_or_more_accession / 
                                                    total_num_CWRs)) %>%
  mutate(CWR = as_factor("All CWRs")) %>%
  slice(1)

SS <- ggplot(gap_analysis_df_by_province_4, aes(x = CWR, 
                                                y = total_proportion_with_an_accession)) +
  geom_bar(stat = "identity") + 
  theme_bw() +
  coord_flip() +
  ylab("Proportion of CWRs Represented in Garden Collections") + xlab("") +
  scale_y_continuous(labels = function(x) paste0(x*100, "%"), limits = c(0, 1))
SS

############################
# Gap Analysis By Ecoregion
# Need to run this across all taxa

# filter accessions to those coming from Canada 
ecoregion_gap_table_in_Canada <- ecoregion_gap_table %>%
  filter(country == "Canada" | is.na(country))

ecoregion_gap_analysis <- function(taxon) {
  
  ecoregionTableData <- ecoregion_gap_table_in_Canada %>%
    # filter the table to the selected CWR
    filter(ecoregion_gap_table_in_Canada$species == taxon) %>%
    
    # tally the number of rows in each ecoregion with an existing accession (garden is not NA)
    group_by(ECO_NAME) %>%
    add_tally(!is.na(garden)) %>%
    rename("accessions_in_ecoregion" = "n")  %>%
    ungroup() %>%
    
    # count the number of accessions w/ and w/out geographic data
    mutate(accessions_no_geo_data = sum(is.na(ECO_NAME))) %>%
    mutate(accessions_with_geo_data = sum(!is.na(ECO_NAME) & !is.na(garden)))  %>%
    mutate(total_accessions_for_species = accessions_with_geo_data + accessions_no_geo_data) %>%
    
    # convert number of accessions (per ecoregion) to a binary "is there or is there not an accession from x region"
    group_by(ECO_NAME) %>%
    filter(row_number() == 1) %>%
    filter(!is.na(ECO_NAME)) %>%
    mutate(binary = ifelse(
      accessions_in_ecoregion > 0, 1, 0)) %>%
    ungroup() %>%
    
    # use the binary variable to determine the proportion of native regions with an accession
    mutate(num_native_ecoregion = sum(!duplicated(ECO_NAME))) %>%
    mutate(num_covered_ecoregion = sum(binary)) %>%
    mutate(perc_ecoregion_range_covered = 
             (num_covered_ecoregion / num_native_ecoregion)) %>%
    
    # convert number of accessions (overall) to a binary "is there or is there not an accession from x region"
    mutate(at_least_one_accession = ifelse(
      sum(total_accessions_for_species) > 0, 1, 0)) %>%
    
    # format the data for the summary table 
    filter(row_number() == 1) %>% # for now only want one row (could adjust this with row per ecoregion)
    dplyr::select(Group, crop, species, num_native_ecoregion, num_covered_ecoregion, perc_ecoregion_range_covered, 
                  accessions_with_geo_data, accessions_no_geo_data, 
                  total_accessions_for_species, at_least_one_accession) %>%
    mutate(num_covered_ecoregion = as.integer(num_covered_ecoregion)) %>%
    rename("native ecoregion" = num_native_ecoregion,
           "covered ecoregion" = num_covered_ecoregion,
           "accessions with geographic data" = accessions_with_geo_data,
           "accessions lacking geographic data" = accessions_no_geo_data,
           "total accessions" = total_accessions_for_species)
  
}

# test <- province_gap_analysis("Amelanchier arborea")
gap_analysis_df_by_ecoregion <- data.frame("Group" = character(),
                                          "crop"= character(), 
                                          "species" = character(), 
                                          "native ecoregion" = character(), 
                                          "covered ecoregion" = character(), 
                                          "accessions with geographic data" = character(),
                                          "accessions lacking geographic data" = character(),
                                          stringsAsFactors=FALSE)


for(i in 1:nrow(cwr_list)) {
  
  selected_taxon <- cwr_list[[i,3]] # r is species ("sci_name")
  as.data.frame(temp <- ecoregion_gap_analysis(
    taxon = selected_taxon)) 
  gap_analysis_df_by_ecoregion <- rbind(
    gap_analysis_df_by_ecoregion, temp)
  
} 

# proportion of native range represented for each taxa hrouped by category w/
gap_analysis_df_by_ecoregion_2 <- gap_analysis_df_by_ecoregion %>%
  group_by(Group) %>%
  mutate(sum_species_w_accessions = sum(at_least_one_accession)) %>%
  add_tally %>%
  mutate(proportion_species_conserved = sum_species_w_accessions / n) %>%
  ungroup()

T <- ggplot(gap_analysis_df_by_ecoregion_2, aes(perc_ecoregion_range_covered, Group)) + 
  geom_boxplot(outlier.shape = NA) + geom_jitter() + 
  xlab("Proportion of native ecoregions represented in garden collections") + ylab("") +
  theme_bw() +
  scale_x_continuous(labels = function(x) paste0(x*100, "%")) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14), 
        axis.title.x = element_text(size = 14))
T

# what's the mean proportion?
(mean_range <- mean(gap_analysis_df_by_ecoregion_2$perc_ecoregion_range_covered))
(sd_range <- sd(gap_analysis_df_by_ecoregion_2$perc_ecoregion_range_covered))



################################
# which taxa did we not do a gap analysis on because we lacked any GBIF data?
anti_join_missing_cwrs <- anti_join(cwr_list, gap_analysis_df_by_province, by = c("sci_name" = "species"))
# write.csv(anti_join_missing_cwrs, "./Output_Data_and_Files/anti_join_missing_cwrs.csv")

################################
#   Amelanchier Case Study     #
################################

# Redo everything above but with a dataset filtered to crop == "Saskatoon"

saskatoon_cwr_list <- cwr_list %>%
  filter(Crop == "Saskatoon")

# Gap Analysis By Province
# Need to run this across all taxa

saskatoon_province_gap_table <- province_gap_table %>%
  filter(crop == "Saskatoon")

saskatoon_province_gap_analysis <- function(taxon) {
  
  provinceTableData <- saskatoon_province_gap_table %>%
    # filter the table to the selected CWR
    filter(saskatoon_province_gap_table$species == taxon) %>%
    
    # tally the number of rows in each province with an existing accession (garden is not NA)
    group_by(province) %>%
    add_tally(!is.na(garden)) %>%
    rename("accessions_in_province" = "n")  %>%
    ungroup() %>%
    
    # count the number of accessions w/ and w/out geographic data
    mutate(accessions_no_geo_data = sum(is.na(province))) %>%
    mutate(accessions_with_geo_data = sum(!is.na(province) & !is.na(garden)))  %>%
    mutate(total_accessions_for_species = accessions_with_geo_data + accessions_no_geo_data) %>%
    
    # convert number of accessions (per province) to a binary "is there or is there not an accession from x region"
    group_by(province) %>%
    filter(row_number() == 1) %>%
    filter(!is.na(province)) %>%
    mutate(binary = ifelse(
      accessions_in_province > 0, 1, 0)) %>%
    ungroup() %>%
    
    # use the binary variable to determine the proportion of native regions with an accession
    mutate(num_native_province = sum(!duplicated(province))) %>%
    mutate(num_covered_province = sum(binary)) %>%
    mutate(perc_province_range_covered = 
             (num_covered_province / num_native_province)) %>%
    
    # convert number of accessions (overall) to a binary "is there or is there not an accession from x region"
    mutate(at_least_one_accession = ifelse(
      sum(total_accessions_for_species) > 0, 1, 0)) %>%
    
    # format the data for the summary table 
    filter(row_number() == 1) %>% # for now only want one row (could adjust this with row per province)
    dplyr::select(Group, crop, species, num_native_province, num_covered_province, perc_province_range_covered, 
                  accessions_with_geo_data, accessions_no_geo_data, 
                  total_accessions_for_species, at_least_one_accession) %>%
    mutate(num_covered_province = as.integer(num_covered_province)) %>%
    rename("native provinces" = num_native_province,
           "covered provinces" = num_covered_province,
           "accessions with geographic data" = accessions_with_geo_data,
           "accessions lacking geographic data" = accessions_no_geo_data,
           "total accessions" = total_accessions_for_species)
  
}

# test <- province_gap_analysis("Amelanchier arborea")
saskatoon_gap_analysis_df_by_province <- data.frame("Group" = character(),
                                          "crop"= character(), 
                                          "species" = character(), 
                                          "native provinces" = character(), 
                                          "covered provinces" = character(), 
                                          "accessions with geographic data" = character(),
                                          "accessions lacking geographic data" = character(),
                                          stringsAsFactors=FALSE)


for(i in 1:nrow(saskatoon_cwr_list)) {
  
  selected_taxon <- saskatoon_cwr_list[[i,3]] # r is species ("sci_name")
  as.data.frame(temp <- saskatoon_province_gap_analysis(
    taxon = selected_taxon)) 
  saskatoon_gap_analysis_df_by_province <- rbind(
    saskatoon_gap_analysis_df_by_province, temp)
  
} 

# write.csv(saskatoon_gap_analysis_df_by_province, "./Output_Data_and_Files/saskatoon_gap_table_province.csv")

############################
# Gap Analysis By Ecoregion
# Need to run this across all taxa

saskatoon_ecoregion_gap_table <- ecoregion_gap_table %>%
  filter(crop == "Saskatoon")

saskatoon_ecoregion_gap_analysis <- function(taxon) {
  
  ecoregionTableData <- saskatoon_ecoregion_gap_table %>%
    # filter the table to the selected CWR
    filter(saskatoon_ecoregion_gap_table$species == taxon) %>%
    
    # tally the number of rows in each ecoregion with an existing accession (garden is not NA)
    group_by(ECO_NAME) %>%
    add_tally(!is.na(garden)) %>%
    rename("accessions_in_ecoregion" = "n")  %>%
    ungroup() %>%
    
    # count the number of accessions w/ and w/out geographic data
    mutate(accessions_no_geo_data = sum(is.na(ECO_NAME))) %>%
    mutate(accessions_with_geo_data = sum(!is.na(ECO_NAME) & !is.na(garden)))  %>%
    mutate(total_accessions_for_species = accessions_with_geo_data + accessions_no_geo_data) %>%
    
    # convert number of accessions (per ecoregion) to a binary "is there or is there not an accession from x region"
    group_by(ECO_NAME) %>%
    filter(row_number() == 1) %>%
    filter(!is.na(ECO_NAME)) %>%
    mutate(binary = ifelse(
      accessions_in_ecoregion > 0, 1, 0)) %>%
    ungroup() %>%
    
    # use the binary variable to determine the proportion of native regions with an accession
    mutate(num_native_ecoregions = sum(!duplicated(ECO_NAME))) %>%
    mutate(num_covered_ecoregions = sum(binary)) %>%
    mutate(perc_ecoregion_range_covered = 
             (num_covered_ecoregions / num_native_ecoregions)) %>%
    
    # convert number of accessions (overall) to a binary "is there or is there not an accession from x region"
    mutate(at_least_one_accession = ifelse(
      sum(total_accessions_for_species) > 0, 1, 0)) %>%
    
    # format the data for the summary table 
    filter(row_number() == 1) %>% # for now only want one row (could adjust this with row per ecoregion)
    dplyr::select(Group, crop, species, num_native_ecoregions, num_covered_ecoregions, perc_ecoregion_range_covered, 
                  accessions_with_geo_data, accessions_no_geo_data, 
                  total_accessions_for_species, at_least_one_accession) %>%
    mutate(num_covered_ecoregions = as.integer(num_covered_ecoregions)) %>%
    rename("native ecoregions" = num_native_ecoregions,
           "covered ecoregions" = num_covered_ecoregions,
           "accessions with geographic data" = accessions_with_geo_data,
           "accessions lacking geographic data" = accessions_no_geo_data,
           "total accessions" = total_accessions_for_species)
  
}

# test <- saskatoon_ecoregion_gap_analysis("Amelanchier arborea")
saskatoon_gap_analysis_df_by_ecoregion <- data.frame("Group" = character(),
                                           "crop"= character(), 
                                           "species" = character(), 
                                           "native ecoregion" = character(), 
                                           "covered ecoregion" = character(), 
                                           "accessions with geographic data" = character(),
                                           "accessions lacking geographic data" = character(),
                                           stringsAsFactors=FALSE)


for(i in 1:nrow(saskatoon_cwr_list)) {
  
  selected_taxon <- saskatoon_cwr_list[[i,3]] # r is species ("sci_name")
  as.data.frame(temp <- saskatoon_ecoregion_gap_analysis(
    taxon = selected_taxon)) 
  saskatoon_gap_analysis_df_by_ecoregion <- rbind(
    saskatoon_gap_analysis_df_by_ecoregion, temp)
  
} 

# write.csv(saskatoon_gap_analysis_df_by_ecoregion, "./Output_Data_and_Files/saskatoon_gap_table_ecoregion.csv")

##################################################
# Plot Amelanchier Range Conservation Gap Analysis

##############
# By Province

# Function to get plot data by taxon
saskatoon_plotData <- function(taxon){
  # filter province_gap_table frame and calculate species specific stats
  provinceTableData <- saskatoon_province_gap_table %>%
    # filter the table to the selected CWR
    filter(saskatoon_province_gap_table$species == taxon) %>%
    
    # tally the number of rows in each province with an existing accession (garden is not NA)
    group_by(province) %>%
    add_tally(!is.na(garden)) %>%
    rename("accessions_in_province" = "n")  %>%
    ungroup() %>%
    
    # count the number of accessions w/ and w/out geographic data
    mutate(total_accessions_for_species = sum(!is.na(garden))) %>%
    mutate(accessions_no_geo_data = sum(is.na(province))) %>%
    mutate(accessions_with_geo_data = sum(!is.na(province))) %>%
    
    # convert number of accessions to a binary "is there or is there not an accession from x region"
    group_by(province) %>%
    filter(row_number() == 1) %>%
    filter(!is.na(province)) %>%
    mutate(binary = ifelse(
      accessions_in_province > 0, 1, 0)) %>%
    ungroup() %>%
    
    # use the binary variable to determine the proportion of native regions with an accession
    mutate(num_native_province = sum(!duplicated(province))) %>%
    mutate(num_covered_province = sum(binary)) %>%
    mutate(perc_province_range_covered = 
             num_covered_province / num_native_province) 
  
  # join plot data with the spatial data frame necessary for projecting the plot  
  tigris::geo_join(canada_provinces_geojson, provinceTableData,  
                   by_sp = "name", by_df = "province")
  
} 

make_a_plot <- function(taxon) {
  subset_province_gap_table_sf <- province_gap_table_sf %>%
    filter(species == taxon)
  
  plot <- ggplot(saskatoon_plotData(taxon = taxon)) +
    geom_sf(aes(fill = as.factor(binary)),
            color = "gray60", size = 0.1) +
    geom_sf(data = subset_province_gap_table_sf, color = 'skyblue', alpha = 0.5, size = 2) + 
    coord_sf(crs = crs_string) +
    scale_fill_manual(values = c("gray80", "gray18"), 
                      labels = c("No accessions with geographic data held in collection", 
                                 ">1 accession with geographic data held in collection", 
                                 "Outside of native range")) +
    # guides(fill = guide_legend(title = "Conservation Status in Botanic Gardens", 
    #                            title.position = "top",
    #                           title.theme = element_text(size = 10, face = "bold")
    # )) +
    theme_map() +
    ggtitle(taxon) +
    theme(panel.grid.major = element_line(color = "white"),
          plot.title = element_text(color="black",
                                    size=14, face="bold.italic", hjust = 0.5),
          plot.margin=unit(c(0.1,-0.2,0.1,-0.2), "cm"),
          legend.position = "none") # , legend.text = element_text(size=10)),
  return(plot)
}
  
test <- make_a_plot("Amelanchier arborea")
test

plot = list()
p = 1
for(i in 1:nrow(saskatoon_cwr_list)) {
  
  selected_taxon <- saskatoon_cwr_list[[i,3]] # r is species ("sci_name")
  plot[[p]] <- make_a_plot(selected_taxon)
  
  p = p+1
  
} 

# plot
plot[c(5, 6, 8, 11, 12, 15)] <- NULL # remove species w no range data
print(do.call(grid.arrange,plot))


# need to get a legend
single_species_plot_data <- saskatoon_plotData(taxon = "Amelanchier alnifolia")

plot_single <- ggplot(single_species_plot_data) +
  geom_sf(aes(fill = as.factor(binary)),
          color = "gray60", size = 0.1) +
  coord_sf(crs = crs_string) +
  scale_fill_manual(values = c("gray80", "gray18"), 
                    labels = c("No accessions with geographic data held in collection", 
                               "1 or more accessions with geographic data held in collection", 
                               "Outside of native range")) +
  guides(fill = guide_legend(title = "Conservation Status in Botanic Gardens", 
                              title.position = "top",
                             title.theme = element_text(size = 10, face = "bold"))) +
  theme_map() +
  ggtitle("") +
  theme(panel.grid.major = element_line(color = "white"),
        plot.title = element_text(color="black",
                                  size=10, face="bold.italic", hjust = 0.5),
        plot.margin=unit(c(0.1,-0.2,0.1,-0.2), "cm"),
        legend.text = element_text(size=10))
plot_single

##############
# By Ecoregion
# Function to get plot data by taxon
saskatoon_plotData_ecoregion <- function(taxon){
  # filter province_gap_table frame and calculate species specific stats
  ecoregionTableData <- saskatoon_ecoregion_gap_table %>%
    # filter the table to the selected CWR
    filter(saskatoon_ecoregion_gap_table$species == taxon) %>%
    
    # tally the number of rows in each ecoregion with an existing accession (garden is not NA)
    group_by(ECO_NAME) %>%
    add_tally(!is.na(garden)) %>%
    rename("accessions_in_ecoregion" = "n")  %>%
    ungroup() %>%
    
    # count the number of accessions w/ and w/out geographic data
    mutate(total_accessions_for_species = sum(!is.na(garden))) %>%
    mutate(accessions_no_geo_data = sum(is.na(ECO_NAME))) %>%
    mutate(accessions_with_geo_data = sum(!is.na(ECO_NAME))) %>%
    
    # convert number of accessions to a binary "is there or is there not an accession from x region"
    group_by(ECO_NAME) %>%
    filter(row_number() == 1) %>%
    filter(!is.na(ECO_NAME)) %>%
    mutate(binary = ifelse(
      accessions_in_ecoregion > 0, 1, 0)) %>%
    ungroup() %>%
    
    # use the binary variable to determine the proportion of native regions with an accession
    mutate(num_native_ecoregion = sum(!duplicated(ECO_NAME))) %>%
    mutate(num_covered_ecoregion = sum(binary)) %>%
    mutate(perc_ecoregion_range_covered = 
             num_covered_ecoregion / num_native_ecoregion) 
  
  # join plot data with the spatial data frame necessary for projecting the plot  
  tigris::geo_join(canada_ecoregions_geojson, ecoregionTableData,  
                   by_sp = "ECO_NAME", by_df = "ECO_NAME")
  
} 

make_a_plot_ecoregion <- function(taxon) {
  subset_ecoregion_gap_table_sf <- ecoregion_gap_table_sf %>%
    filter(species == taxon)
  
  plot <- ggplot(saskatoon_plotData_ecoregion(taxon = taxon)) +
    geom_sf(aes(fill = as.factor(binary)),
            color = "gray60", size = 0.1) +
    geom_sf(data = subset_ecoregion_gap_table_sf, color = 'skyblue', alpha = 0.5, size = 2) +
    coord_sf(crs = crs_string) +
    scale_fill_manual(values = c("gray80", "gray18"), 
                      labels = c("No accessions with geographic data held in collection", 
                                 ">1 accession with geographic data held in collection", 
                                 "Outside of native range")) +
    # guides(fill = guide_legend(title = "Conservation Status in Botanic Gardens", 
    #                            title.position = "top",
    #                           title.theme = element_text(size = 10, face = "bold")
    # )) +
    theme_map() +
    ggtitle(taxon) +
    theme(panel.grid.major = element_line(color = "white"),
          plot.title = element_text(color="black",
                                    size=14, face="bold.italic", hjust = 0.5),
          plot.margin=unit(c(0.1,-0.2,0.1,-0.2), "cm"),
          legend.position = "none") # , legend.text = element_text(size=10)),
  return(plot)
}

test <- make_a_plot_ecoregion("Amelanchier arborea")
test

plot_ecoregions = list()
q = 1
for(i in 1:nrow(saskatoon_cwr_list)) {
  
  selected_taxon <- saskatoon_cwr_list[[i,3]] # r is species ("sci_name")
  plot_ecoregions[[q]] <- make_a_plot_ecoregion(selected_taxon)
  
  q = q+1
  
} 

# plot
plot_ecoregions[c(5, 6, 8, 11, 12, 15)] <- NULL # remove species w no range data
print(do.call(grid.arrange,plot_ecoregions))

################################################
# Differences by Garden
################################################
garden_accessions <- province_gap_table %>%
  filter(!is.na(garden)) %>%
  group_by(garden) %>%
  add_tally() %>%
  distinct(garden, .keep_all=TRUE) %>%
  ungroup() %>%
  dplyr::select(garden, n)

U <- ggplot(garden_accessions) +
  geom_histogram(aes(n), breaks=c(0, 1000, 2000, 3000, 4000, 5000, 
                                  6000, 7000, 8000, 9000, 10000)) + 
  theme_bw() +
  labs(x = "Number of CWR Accessions", y = "Number of Gardens")
U


##################

# number of crops

num_crops <- province_gap_table %>%
 distinct(crop)

