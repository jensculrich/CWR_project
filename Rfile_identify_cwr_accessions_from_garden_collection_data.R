##GOAL: Compare Botanical Garden accession list with our Master Canadian CWR list
##END-RESULT: A list of all CWR accessions from Botanical Garden

# read list of CWR taxa
master_list <- read.csv(".Input_Data_and_Files/master_list_apr_3.csv")

#read data for whichever garden you want to compare
rbg<- read.csv("RBG_all.csv")

#Most gardens have variant info in their species name so we need to seperate that out in order to compare with our Master List

rbg_replace <- gsub("\\[", "(", rbg$NAME)  #messy work-around: 
                                         #replace '[' with '(' because str_split_fixed function can't deal with '['
rebinded <- cbind(rbg_replace, rbg) #rebind them together so it's recursive

is.recursive(rebinded) #check to see if the data is recursive, str_split_fixed won't work with atomic only recursive


#this line will likely have to be slightly modified for each garden after looking at it a bit..
split_var <- str_split_fixed(rebinded$rbg_replace, " var. | cv.| subsp.|[']|[(]", 2)  #seperate names into 2 columns
                                                                                   # based off of different ways 'variant' 
                                                                                  #categories were written in the in the database

total<-cbind(rbg, split_var) #put split columns back into original RBG database with proper headings
colnames(total)[25] <- "species" 
colnames(total)[26] <- "variant"

total$species <- trimws(total$species, which = c("right"))  #remove trailing white space on species names


CWR_of_RBG<-merge(master_list,total, by.x = "CROP.WILD.RELATIVE", by.y = "species") #Finally, cross-reference RBG list with Master list
                                                                                    #getting a list of all CWR plants in RBG
