library(tidyverse)
library(ggthemes)
tidy_all_matches_docks_rl <- read_csv("data/tidy_all_matches_docks_rl.csv")

#need to decide which record pairs 
#are true matches

#How many MRIP entries are there?
tidy_all_matches_docks_rl %>%
  distinct(date_time_mrip) %>%
  nrow()

#Of these, how many distinct CLS trips are there
mrip_dts <- tidy_all_matches_docks_rl %>%
  distinct(date_time_mrip) %>%
  pull(date_time_mrip)

tidy_all_matches_docks_rl %>% 
  filter(date_time_mrip %in% mrip_dts) %>% 
  distinct(tripID,CLS_ID) %>% 
  nrow()

#top score for each MRIP DATE_TIME
tidy_all_matches_docks_rl %>%
  group_by(date_time_mrip) %>%
  top_n(n=1,rl_score) %>%
  ungroup() %>%
  group_by(date_time_mrip) %>%
  top_n(n=-1,km_difference) %>%
  nrow()
#Too many here

#need to filter it so that only one cls report 
#is matched with an mrip trip

one_per_first <- tidy_all_matches_docks_rl %>%
  filter(!is.na(tripID)) %>% 
  group_by(date_time_mrip) %>%
  top_n(n=1,rl_score) %>%
  ungroup() %>%
  group_by(date_time_mrip) %>%
  top_n(n=-1,km_difference) %>%
  ungroup() %>% 
  group_by(tripID) %>% 
  top_n(n=1,rl_score) %>% 
  ungroup() 

one_per_first %>% 
  pull(date_time_mrip) %>% 
  .[duplicated(.)] #duplicated date_times

first_date_times_duplicated <- one_per_first %>% 
  pull(date_time_mrip) %>% 
  .[duplicated(.)]

one_per_first %>% 
  filter(date_time_mrip %in% first_date_times_duplicated) %>% 
  select(tripID,date_time_mrip,reportdate,returndate) %>% 
  View()

one_per_first <- tidy_all_matches_docks_rl %>%
  group_by(date_time_mrip) %>%
  top_n(n=1,rl_score) %>%
  ungroup() %>%
  group_by(date_time_mrip) %>%
  top_n(n=-1,km_difference) %>%
  ungroup() %>% 
  group_by(tripID) %>% 
  top_n(n=1,rl_score) %>% 
  ungroup() %>% 
  group_by(date_time_mrip) %>% #removes duplicates made 
  #when a captain reports exact same trip twice
  top_n(n=-1,reportdate) %>% 
  ungroup() %>% 
  filter(tripID != 13170)#A complete duplicate

mrip_dates_one_per_first <- one_per_first %>% 
  pull(date_time_mrip)
mrip_tripids_one_per_first <- one_per_first %>% 
  pull(tripID)

#So one per first has 418 good mrip entries matched with 418 cls entries
#Remove these trips from tidy_all_matches_rl and match again

second_to_match <- tidy_all_matches_docks_rl %>% 
  filter(!date_time_mrip %in% mrip_dates_one_per_first) %>% 
  filter(!is.na(tripID)) 

matched_second <- second_to_match %>%
  filter(!tripID %in% mrip_tripids_one_per_first) %>% 
  group_by(date_time_mrip) %>%
  top_n(n=1,rl_score) %>%
  ungroup() %>%
  group_by(date_time_mrip) %>%
  top_n(n=-1,km_difference) %>%
  ungroup() %>% 
  group_by(tripID) %>% 
  top_n(n=1,rl_score) %>% 
  ungroup() %>% 
  group_by(date_time_mrip) %>% 
  top_n(n=-1,reportdate) %>% 
  ungroup()


good_match <- matched_second %>% 
  bind_rows(one_per_first) 

nrow(good_match)
length(unique(good_match$date_time_mrip))
length(unique(good_match$tripID))

good_match %>% 
  pull(date_time_mrip) %>% 
  .[duplicated(.)]

tidy_all_matches_one_per <- good_match

write_csv(tidy_all_matches_one_per,"data/tidy_all_matches_rl_one_per.csv")
nrow(tidy_all_matches_one_per)
#so now we have 444 rows of potential matches,
#look at the distribution of the scores
tidy_all_matches_one_per %>%
  ggplot(aes(rl_score)) +
  geom_histogram(bins=38) +
  xlab("Score") +
  ylab(NULL) +
  #theme_economist() +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))
ggsave("C:/Users/32443181/Box Sync/Research/Dissertation/Thesis/record_linkage_scores_all.jpg",
       height=5, width=8.5, units='in', dpi=600)

#only scores greater than 2
tidy_all_matches_one_per %>%
  filter(rl_score > 2) %>% 
  ggplot(aes(rl_score)) +
  geom_histogram(bins=28) +
  xlab("Score") +
  ylab(NULL) +
  #theme_economist() +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16)) +
  geom_vline(xintercept = 15)
ggsave("C:/Users/32443181/Box Sync/Research/Dissertation/Thesis/record_linkage_scores_gt2.jpg",
       height=5, width=8.5, units='in', dpi=600)

#only scores greater than 0
tidy_all_matches_one_per %>%
  filter(rl_score > 0) %>% 
  ggplot(aes(rl_score)) +
  geom_histogram(bins=30) +
  xlab("Score") +
  ylab(NULL) +
  #theme_economist() +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))


#pick a score to make the cutoff value
tidy_all_matches_one_per %>%
  filter(rl_score > 5.5) %>%
  nrow()


tidy_all_matches_docks_rl %>%
  arrange(-rl_score) %>%
  group_by(CLS_ID, date_time_mrip) %>%
  top_n(n = 1) %>%
  ungroup() %>%
  select(-ends_with("claim")) %>%
  select(-ends_with("release")) %>%
  select(-ends_with("kept")) %>%
  select(-ends_with("released")) %>%
  filter(rl_score>5) %>%
  View()




