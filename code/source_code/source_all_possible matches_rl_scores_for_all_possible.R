#source everything

#CLS Cleaning 
source("code/data_processing_for_record_linkage/cleaning_cls_reports_all.R")

#Clean MRIP - 2016
source("code/data_processing_for_record_linkage/cleaning mrip data 2016.R")

#Same for 2017
source("code/data_processing_for_record_linkage/cleaning mrip data 2017.R")

#Clean up and do a full join between CLS
#And Mrip based on cls ID and wave/year

#2016
source("code/data_processing_for_record_linkage/matching mrip and cls all possible matches 2016.R")

#2017
source("code/data_processing_for_record_linkage/matching mrip and cls all possible matches 2017.R")

#Clean up all possible matches
#Get a dataset of what can be considered the "non-matches"
#since there are so few matches relative to all possible matches

#2016
source("code/data_processing_for_record_linkage/cleaning up all possible matches 2016.R")

#2017
source("code/data_processing_for_record_linkage/cleaning up all possible matches 2017.R")

#Bind the rows of 2016 and 2017 together
#Output all_possible_matches_16_17.csv

source("code/data_processing_for_record_linkage/merging 2016 and 2017 all possible matches.R")

#This takes the possible matches and runs record linkage
#outputs record linkage scores for each match
source("code/record_linkage/auto_rl_algorithm.R")