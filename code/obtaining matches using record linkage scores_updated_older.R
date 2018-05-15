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
  group_by(date_time_mrip) %>%
  top_n(n=1,rl_score) %>%
  ungroup() %>%
  group_by(date_time_mrip) %>%
  top_n(n=-1,km_difference) %>%
  ungroup() %>% 
  group_by(tripID) %>% 
  top_n(n=1,rl_score) 
  
#There are probably some repeated tripIDs...
cls_repeats_ids <- tidy_all_matches_docks_rl %>%
  group_by(CLS_ID,date_time_mrip) %>%
  top_n(n=1,rl_score) %>%
  ungroup() %>%
  group_by(CLS_ID,date_time_mrip) %>%
  top_n(n=-1,km_difference) %>%
  ungroup() %>%
  group_by(date_time_mrip) %>%
  top_n(n=1,rl_score) %>%
  ungroup() %>%
  pull(tripID) %>% 
  .[duplicated(.)] 

#for these duplicated tripIDs, I need to remove them from the 
#list of tripIDs and for the matches associated with these trips
#match the mrip trips us with their second highest match

#the matches associated with these duplicated tripIDs
dup_matches <- one_per_first %>% 
  filter(tripID %in% cls_repeats_ids) 
nrow(dup_matches)

#Take these, for each trip ID, get the highest score
dup_matches %>% 
  group_by(tripID) %>% 
  top_n(n=1,rl_score) %>% 
  select(rl_score,tripID,CLS_ID,date_time_mrip) %>% 
  View() #works

# good ones 
good_dups <- dup_matches %>% 
  group_by(tripID) %>% 
  top_n(n=1,rl_score) %>% 
  ungroup()

# bad ones
bad_dups <- dup_matches %>% 
  anti_join(good_dups,by=c("tripID","date_time_mrip")) 

#Remove the bad ones from the initial matches
good_matches_first <- one_per_first %>%
  anti_join(bad_dups) #these are good matches

#need to find matches for bad_dups
#remove the tripIDs from the good matches and then match the bad matches

#remove these trips from tidy_one_per
#do matching on these, but remove the good tripIDs
good_matches_second <- tidy_all_matches_docks_rl %>% 
  filter(!tripID %in% good_matches_first$tripID) %>%
  filter(date_time_mrip %in% bad_dups$date_time_mrip) %>%
  group_by(date_time_mrip) %>%
  top_n(n=1,rl_score) %>%
  ungroup() %>%
  group_by(date_time_mrip) %>%
  top_n(n=-1,km_difference) %>%
  ungroup() 

#Still a few duplicated tripIDs
#Just do the process again

reps_cls_tripID_2 <- good_matches_second %>% 
  pull(tripID) %>% 
  .[duplicated(.)]

dup_matches_2 <- good_matches_second %>% 
  filter(tripID %in% reps_cls_tripID_2)

good_dups_2 <- dup_matches_2 %>% 
  group_by(tripID) %>% 
  top_n(n=1,rl_score) %>% 
  ungroup()

bad_dups_2 <- dup_matches_2 %>% 
  anti_join(good_dups_2,by=c("tripID","date_time_mrip")) 

good_matches <- good_matches_first %>%
  bind_rows(good_matches_second) %>%
  anti_join(bad_dups_2) #these are good matches

good_matches_three <- tidy_all_matches_docks_rl %>% 
  filter(!tripID %in% good_matches$tripID) %>%
  filter(date_time_mrip %in% bad_dups_2$date_time_mrip) %>%
  group_by(date_time_mrip) %>%
  top_n(n=1,rl_score) %>%
  ungroup() %>%
  group_by(date_time_mrip) %>%
  top_n(n=-1,km_difference) %>%
  ungroup() 

good_matches <- good_matches %>%
  bind_rows(good_matches_three)

#see if there are any tripID duplicates
good_matches %>%
  pull(tripID) %>% 
  .[duplicated(.)] #Nope

tst_2 <- good_matches %>%
  pull(date_time_mrip) %>% 
  .[duplicated(.)] #there are some duplicated mrip date/tiems
tst_2

#check it out
good_matches %>%
  filter(date_time_mrip %in% tst_2) %>%
  select(tripID,date_time_mrip,rl_score,
         return_date_ymd,reportdate,returndate) %>% 
  View("trouble")
#The issue with these trips is that they
#are duplicates reports by captains
#referring to same trip
#so just keep the smallest reportdate for each date_time_mrip

good_matches %>% 
  filter(date_time_mrip %in% tst_2) %>%
  group_by(date_time_mrip) %>% 
  top_n(-1,reportdate) %>% 
  ungroup() %>% 
  pull(date_time_mrip) %>% 
  .[duplicated(.)] #only one extra, it is a complete duplicate

tripID_dups_to_remove <- good_matches %>% 
  filter(date_time_mrip %in% tst_2) %>%
  group_by(date_time_mrip) %>% 
  top_n(-1,reportdate) %>% 
  ungroup() %>% 
  filter(tripID != 13170) %>% 
#need to keep on one of the 2 complete duplicate records
  pull(tripID)

good_matches %>%
  filter(!tripID %in% tripID_dups_to_remove) %>% 
  nrow()

#make this a new dataset
tidy_all_matches_one_per <- good_matches %>%
  filter(!tripID %in% tripID_dups_to_remove)

setdiff(as.character(tidy_all_matches_docks_rl$date_time_mrip),
        as.character(tidy_all_matches_one_per$date_time_mrip))


write_csv(tidy_all_matches_one_per,"data/tidy_all_matches_rl_one_per.csv")
nrow(tidy_all_matches_one_per)
#so now we have 419 rows of potential matches,
#look at the distribution of the scores
tidy_all_matches_one_per %>%
  ggplot(aes(rl_score)) +
  geom_histogram(bins=40) +
  xlab("Score") +
  ylab(NULL) +
  #theme_economist() +
  theme(axis.title.x = element_text(size=16),
        axis.title.y = element_text(size=16))
ggsave("C:/Users/32443181/Box Sync/Research/Dissertation/Prospectus/record_linkage_scores_all.jpg",
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
        axis.title.y = element_text(size=16))
ggsave("C:/Users/32443181/Box Sync/Research/Dissertation/Prospectus/record_linkage_scores_gt2.jpg",
       height=5, width=8.5, units='in', dpi=600)

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




