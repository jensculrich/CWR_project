library(tidyverse)

setwd("C:/Users/jensj/Documents/UBC2/Classes/ConservationScience/Project/CWR of the USA occurrences by genus 2020_7_30")

# full.names = T reads in full file path. Important when files are not in working
# directory 
filelist <- list.files("C:/Users/jensj/Documents/UBC2/Classes/ConservationScience/Project/CWR of the USA occurrences by genus 2020_7_30",
                       full.names = T)

#create one csv out of all the csvs in CWR folder
final_sheet <- data.frame()
for (i in filelist){
  print(i)
  sheet <- read.csv(i)
  final_sheet <- rbind(final_sheet, sheet)
}

# New copy so if things go bad don't have to redo loop
final_sheet1 <- final_sheet

#create list of species names
species_list <- c("Rubus ursinus", "Rubus idaeus subsp. strigosus", "Fragaria chiloensis",
                  "Fragaria virginiana", "Fragaria chiloensis subsp. pacifica", "Fragaria virginiana subsp. glauca",
                  "Fragaria virginiana subsp. platypetala", "Fragaria virginiana subsp. virginiana", 
                  "Fragaria chiloensis subsp. lucida", "Fragaria × ananassa subsp. cuneifolia", "Helianthus annuus")

#select species under taxon column that are found in species_list
finalsheet_2 <- final_sheet1 %>%
  filter(taxon %in% species_list) %>%
  filter(latitude != "NULL", longitude != "NULL", latitude != "") %>%
  drop_na(latitude)

write.csv(finalsheet_2, "CWR_BC4.csv")
