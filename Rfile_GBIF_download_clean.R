library(tidyverse)
library(readr)
library("rgbif")

# Enter your GBIF user info here before proceeding
# user=user 
# pwd=pwd 
# email=email

# load separate lists of species names. GBIF has a limit 100 000 observations that it's able to
#download at once, which is well under the number for the species combined
csvlist = c("part1.csv","part2.csv","part3.csv","part4.csv","part5.csv", "part6.csv")

# Download data from GBIF looping through csvlist
user = #"GBIF username"
pwd= #"GBIF password"
email= #"email used for GBIF"
for (i in csvlist){
  print(i)
  df = read.csv(i) 
  keys = unique(df$speciesKey, na.last = TRUE)
  keys = keys[!is.na(keys)]
  down_code = occ_download(
    pred_in("taxonKey", keys),
    pred_in("basisOfRecord", c('PRESERVED_SPECIMEN','HUMAN_OBSERVATION','OBSERVATION','MACHINE_OBSERVATION')),
    pred("country", "CA"),
    pred("hasCoordinate", TRUE),
    pred("hasGeospatialIssue", FALSE),
    format = "SIMPLE_CSV",
    user=user, pwd=pwd, email=email)
  Sys.sleep(180) # need to pause the loop to give GBIF time to gather data
  occ_download_get(down_code[1], overwrite = TRUE)
}  

