library(tidyverse)
library(ggthemes)
tidy_all_matches_docks_rl <- read_csv("data/tidy_all_matches_docks_rl.csv",
                                      col_types = cols(psu_id = col_character()))

#need to decide which record pairs 
#are true matches

#How many unique MRIP entries are there?
tidy_all_matches_docks_rl %>%
  distinct(PRT_CODE) %>%
  nrow()

# st_d <- tidy_all_matches_docks_rl %>%
#   distinct(date_time_mrip,wp_int,INTSITE,ST) %>%
#   pull(date_time_mrip) %>% 
#   .[duplicated(.)]
# 
# tidy_all_matches_docks_rl %>%
#   distinct(date_time_mrip,wp_int,INTSITE,ST) %>%
#   filter(date_time_mrip %in% st_d) %>% 
#   View()


#Of these, how many distinct CLS trips are there
mrip_dts <- tidy_all_matches_docks_rl %>%
  distinct(PRT_CODE,.keep_all = T) %>%
  pull(date_time_mrip)

tidy_all_matches_docks_rl %>% 
  filter(date_time_mrip %in% mrip_dts) %>% 
  distinct(tripID,CLS_ID) %>% 
  nrow()

#top score for each MRIP DATE_TIME, sampling weight, and ST
tidy_all_matches_docks_rl %>%
  group_by(PRT_CODE) %>%
  top_n(n=1,rl_score) %>%
  ungroup() %>%
  group_by(PRT_CODE) %>%
  top_n(n=-1,date_diff) %>%
  ungroup() %>% 
  group_by(PRT_CODE) %>%
  top_n(n=-1,km_difference) %>%
  ungroup() %>%
  nrow()

#need to filter it so that only one cls report 
#is matched with an mrip trip
one_per_first <- tidy_all_matches_docks_rl %>%
  filter(!is.na(tripID)) %>% 
  group_by(PRT_CODE) %>%
  top_n(n=1,rl_score) %>%
  ungroup() %>%
  group_by(PRT_CODE) %>%
  top_n(n=-1,date_diff) %>%
  ungroup() %>% 
  group_by(PRT_CODE) %>%
  top_n(n=-1,km_difference) %>%
  ungroup() %>%
  group_by(tripID) %>% 
  top_n(n=1,rl_score) %>% 
  ungroup() %>% 
  group_by(tripID) %>% 
  top_n(n=-1,abs(date_diff)) %>% 
  ungroup()

nrow(one_per_first )
#518 rows, so we lost some mrip trips

one_per_first %>% 
  pull(PRT_CODE) %>% 
  .[duplicated(.)] #duplicated mrip trips

one_per_first %>% pull(tripID) %>% 
      .[duplicated(.)] #duplicated CLS trips

#no trips were duplicated

mrip_dates_one_per_first <- one_per_first %>% 
  pull(PRT_CODE)
mrip_tripids_one_per_first <- one_per_first %>% 
  pull(tripID)

#So one per first has good mrip entries matched with cls entries
#there are 518 matches here, but there are still mrip samples
#which need matching, so remove these good matches from 
#tidy_all_matches_rl and match again

second_to_match <- tidy_all_matches_docks_rl %>% 
  filter(!PRT_CODE %in% mrip_dates_one_per_first) %>% 
  filter(!is.na(tripID)) 

matched_second <- second_to_match %>%
  filter(!tripID %in% mrip_tripids_one_per_first) %>% 
  group_by(PRT_CODE) %>%
  top_n(n=1,rl_score) %>%
  ungroup() %>%
  group_by(PRT_CODE) %>%
  top_n(n=-1,date_diff) %>%
  ungroup() %>% 
  group_by(tripID) %>% 
  top_n(n=1,rl_score) %>% 
  ungroup() %>% 
  group_by(tripID) %>% 
  top_n(n=-1,abs(date_diff)) %>% 
  ungroup()

#do i need a third round?
sec_d <- matched_second %>%
  pull(PRT_CODE)
first_trips <- one_per_first %>% pull(tripID)
sec_trips <- matched_second %>% pull(tripID)

tidy_all_matches_docks_rl %>% 
  distinct(PRT_CODE,.keep_all = T) %>% 
  filter(!PRT_CODE %in% c(mrip_dates_one_per_first,sec_d) &
         !tripID %in% c(first_trips, sec_trips)) %>%  
  select(PRT_CODE,tripID)

#9 trips were missed
#Need a third round

third_to_match <- tidy_all_matches_docks_rl %>% 
  filter(!PRT_CODE %in% c(mrip_dates_one_per_first,sec_d) &
           !tripID %in% c(first_trips, sec_trips))  
  
matched_third <- third_to_match %>% 
  filter(!is.na(tripID)) %>% 
  group_by(PRT_CODE) %>%
  top_n(n=1,rl_score) %>%
  ungroup() %>%
  group_by(PRT_CODE) %>%
  top_n(n=-1,date_diff) %>%
  ungroup() %>% 
  group_by(tripID) %>% 
  top_n(n=1,rl_score) %>% 
  ungroup() %>% 
  group_by(tripID) %>% 
  top_n(n=-1,abs(date_diff)) %>% 
  ungroup()

#Check third round
third_trips <- matched_third %>% pull(tripID)
third_prt <- matched_third %>% pull(PRT_CODE)
tidy_all_matches_docks_rl %>% 
  distinct(PRT_CODE,.keep_all = T) %>% 
  filter(!PRT_CODE %in% c(mrip_dates_one_per_first,
                          sec_d,third_prt) &
           !tripID %in% c(first_trips, sec_trips,
                          third_trips)) %>%  
  select(PRT_CODE,tripID)

so_far <- matched_second %>% 
  bind_rows(one_per_first) %>% 
  bind_rows(matched_third)
#three was good!
# matched_four <- tidy_all_matches_docks_rl %>% 
#   filter(!PRT_CODE %in% so_far$PRT_CODE &
#            !tripID %in% so_far$tripID) %>% 
#   filter(!is.na(tripID)) %>% 
#   group_by(PRT_CODE) %>%
#   top_n(n=1,rl_score) %>%
#   ungroup() %>%
#   group_by(PRT_CODE) %>%
#   top_n(n=-1,date_diff) %>%
#   ungroup() %>% 
#   group_by(tripID) %>% 
#   top_n(n=1,rl_score) %>% 
#   ungroup() %>% 
#   group_by(tripID) %>% 
#   top_n(n=-1,abs(date_diff)) %>% 
#   ungroup()
# so_far_2 <- matched_second %>% 
#   bind_rows(one_per_first) %>% 
#   bind_rows(matched_third) %>% 
#   bind_rows(matched_four)
# 
# matched_five <- tidy_all_matches_docks_rl %>% 
#   filter(!PRT_CODE %in% so_far_2$PRT_CODE &
#            !tripID %in% so_far_2$tripID) %>% 
#   filter(!is.na(tripID)) %>% 
#   group_by(PRT_CODE) %>%
#   top_n(n=1,rl_score) %>%
#   ungroup() %>%
#   group_by(PRT_CODE) %>%
#   top_n(n=-1,date_diff) %>%
#   ungroup() %>% 
#   group_by(tripID) %>% 
#   top_n(n=1,rl_score) %>% 
#   ungroup() %>% 
#   group_by(tripID) %>% 
#   top_n(n=-1,abs(date_diff)) %>% 
#   ungroup()
# 
# 
# good_match <- matched_second %>% 
#               bind_rows(one_per_first) %>% 
#               bind_rows(matched_third) %>% 
#               bind_rows(matched_four) %>% 
#               bind_rows(matched_five)
good_match <- so_far

nrow(good_match)
length(unique(good_match$PRT_CODE))
length(unique(good_match$tripID)) #OK
good_match %>% 
  pull(PRT_CODE) %>% 
  .[duplicated(.)]
good_match %>% 
  pull(tripID) %>% 
  .[duplicated(.)]

tidy_all_matches_one_per <- good_match

write_csv(tidy_all_matches_one_per,"data/tidy_all_matches_rl_one_per.csv")

#tidy_all_matches_one_per <- read_csv("data/tidy_all_matches_rl_one_per.csv")
nrow(tidy_all_matches_one_per)

#so now we have 591 rows of potential matches,
#look at the distribution of the scores
tidy_all_matches_one_per %>%
  ggplot(aes(rl_score)) +
  geom_histogram(bins=40) +
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
  geom_histogram(bins=30) +
  xlab("Score") +
  ylab(NULL) +
  #theme_economist() +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16)) +
  geom_vline(xintercept = 11)
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
