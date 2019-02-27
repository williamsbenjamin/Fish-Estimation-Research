library(tidyverse)

all_possible_2016 <- read_csv("data/tidy_all_matches_docks_2016.csv",
                              col_types = cols(estimated_returndate = col_datetime(),
                                               psu_id = col_character()))
all_possible_2016 <- all_possible_2016 %>% 
  mutate(GPS_INTSITE_RETURN = as.integer(GPS_INTSITE_RETURN))
all_possible_2017 <- read_csv("data/tidy_all_matches_docks_2017.csv",
                              col_types = cols(estimated_returndate = col_datetime(),
                                               psu_id = col_character()))

#fix some formats

cols <-c(22:106,134:213)
all_possible_2016[,cols] = apply(all_possible_2016[,cols],2,function(x)as.numeric(x))  
all_possible_2017[,cols] = apply(all_possible_2017[,cols],2,function(x)as.numeric(x))  

all_possible_matches <- bind_rows(all_possible_2016,
          all_possible_2017)


write_csv(all_possible_matches,"data/all_possible_matches_16_17.csv")

