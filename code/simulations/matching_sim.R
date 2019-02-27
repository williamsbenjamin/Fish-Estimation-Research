library(tidyverse)
library(ggthemes)

#How many unique MRIP entries are there?
tidy_all_matches_docks_rl %>%
  distinct(id) %>%
  nrow()

#top score for each MRIP DATE_TIME, sampling weight, and ST
tidy_all_matches_docks_rl %>%
  group_by(id) %>%
  top_n(n=1,rl_score) %>%
  ungroup() %>%
  group_by(id) %>%
  top_n(n=-1,diff_date) %>%
  ungroup() %>% 
  group_by(id) %>%
  top_n(n=-1,km_difference) %>%
  ungroup() %>%
  nrow()

#need to filter it so that only one cls report 
#is matched with an mrip trip
one_per_first <- tidy_all_matches_docks_rl %>%
  filter(!is.na(trip_id)) %>% 
  group_by(id) %>%
  top_n(n=1,rl_score) %>%
  ungroup() %>%
  group_by(id) %>%
  top_n(n=-1,diff_date) %>%
  ungroup() %>% 
  group_by(id) %>%
  top_n(n=-1,km_difference) %>%
  ungroup() %>%
  group_by(trip_id) %>% 
  top_n(n=1,rl_score) %>% 
  ungroup() %>% 
  group_by(trip_id) %>% 
  top_n(n=-1,abs(diff_date)) %>% 
  ungroup()

nrow(one_per_first )
#203 rows, so we lost some mrip trips

one_per_first %>% 
  pull(id) %>% 
  .[duplicated(.)] #duplicated mrip trips

one_per_first %>% pull(trip_id) %>% 
      .[duplicated(.)] #duplicated CLS trips

#no trips were duplicated

mrip_dates_one_per_first <- one_per_first %>% 
  pull(id)
mrip_tripids_one_per_first <- one_per_first %>% 
  pull(trip_id)

#So one per first has good mrip entries matched with cls entries
#there are 518 matches here, but there are still mrip samples
#which need matching, so remove these good matches from 
#tidy_all_matches_rl and match again

second_to_match <- tidy_all_matches_docks_rl %>% 
  filter(!id %in% mrip_dates_one_per_first) %>% 
  filter(!is.na(trip_id)) 

matched_second <- second_to_match %>%
  filter(!trip_id %in% mrip_tripids_one_per_first) %>% 
  group_by(id) %>%
  top_n(n=1,rl_score) %>%
  ungroup() %>%
  group_by(id) %>%
  top_n(n=-1,diff_date) %>%
  ungroup() %>% 
  group_by(trip_id) %>% 
  top_n(n=1,rl_score) %>% 
  ungroup() %>% 
  group_by(trip_id) %>% 
  top_n(n=-1,abs(diff_date)) %>% 
  ungroup()

#do i need a third round?
sec_d <- matched_second %>%
  pull(id)
first_trips <- one_per_first %>% pull(trip_id)
sec_trips <- matched_second %>% pull(trip_id)

tidy_all_matches_docks_rl %>% 
  distinct(id,.keep_all = T) %>% 
  filter(!id %in% c(mrip_dates_one_per_first,sec_d) &
         !trip_id %in% c(first_trips, sec_trips)) %>%  
  select(id,trip_id)

#3 trips were missed
#Need a third round

third_to_match <- tidy_all_matches_docks_rl %>% 
  filter(!id %in% c(mrip_dates_one_per_first,sec_d) &
           !trip_id%in% c(first_trips, sec_trips))  
  
matched_third <- third_to_match %>% 
  filter(!is.na(trip_id)) %>% 
  group_by(id) %>%
  top_n(n=1,rl_score) %>%
  ungroup() %>%
  group_by(id) %>%
  top_n(n=-1,diff_date) %>%
  ungroup() %>% 
  group_by(trip_id) %>% 
  top_n(n=1,rl_score) %>% 
  ungroup() %>% 
  group_by(trip_id) %>% 
  top_n(n=-1,abs(diff_date)) %>% 
  ungroup()

#Check third round
third_trips <- matched_third %>% pull(trip_id)
third_prt <- matched_third %>% pull(id)
tidy_all_matches_docks_rl %>% 
  distinct(id,.keep_all = T) %>% 
  filter(!id %in% c(mrip_dates_one_per_first,
                          sec_d,third_prt) &
           !trip_id %in% c(first_trips, sec_trips,
                          third_trips)) %>%  
  select(id,trip_id)

so_far <- matched_second %>% 
  bind_rows(one_per_first) %>% 
  bind_rows(matched_third)
#three was good!

good_match <- so_far

nrow(good_match)
length(unique(good_match$id))
length(unique(good_match$trip_id)) #OK
good_match %>% 
  pull(id) %>% 
  .[duplicated(.)]
good_match %>% 
  pull(trip_id) %>% 
  .[duplicated(.)]

tidy_all_matches_one_per <- good_match

#write_csv(tidy_all_matches_one_per,"data/tidy_all_matches_rl_one_per.csv")

#tidy_all_matches_one_per <- read_csv("data/tidy_all_matches_rl_one_per.csv")
nrow(tidy_all_matches_one_per)

#so now we have 590 rows of potential matches,
#look at the distribution of the scores
tidy_all_matches_one_per %>%
  ggplot(aes(rl_score)) +
  geom_histogram(bins=30) +
  xlab("Score") +
  ylab(NULL) +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))
