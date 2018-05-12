#source everything

#Get 2 tidy datasets, 1 for CLS and 1 for MRIP - 2016
source("code/cleaning cls and mrip data 2016.R")

#Same for 2017
source("code/cleaning cls and mrip data 2017.R")

#Clean up and do a full join between CLS
#And Mrip based on cls ID and wave/year

#2016
source("code/matching mrip and cls all possible matches 2016.R")

#2017
source("code/matching mrip and cls all possible matches 2017.R")

#Clean up all possible matches
#Get a dataset of what can be considered the "non-matches"
#since there are so few matches relative to all possible matches

#2016
source("code/cleaning up all possible matches 2016.R")

#2017
source("code/cleaning up all possible matches 2017.R")

#Bind the rows of 2016 and 2017 together
#Output all_possible_matches_16_17.csv

source("code/merging 2016 and 2017 all possible matches.R")

#This takes the possible matches and runs record linkage
#outputs record linkage scores for each match
source("code/auto_rl_algorithm.R")