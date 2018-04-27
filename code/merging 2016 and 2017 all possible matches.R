library(tidyverse)

all_possible_2016 <- read_csv("data/tidy_all_matches_docks_2016.csv")
all_possible_2017 <- read_csv("data/tidy_all_matches_docks_2017.csv")

#fix some formats

cols <-c(22:109,130:215)
all_possible_2016[,cols] = apply(all_possible_2016[,cols],2,function(x)as.numeric(x))  
all_possible_2017[,cols] = apply(all_possible_2017[,cols],2,function(x)as.numeric(x))  

all_possible_matches <- bind_rows(all_possible_2016,
          all_possible_2017)

write_csv(all_possible_matches,"data/all_possible_matches_16_17.csv")

