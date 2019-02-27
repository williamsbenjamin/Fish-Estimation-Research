library(tidyverse)
library(ggthemes)
tidy_all_matches_docks_rl <- read_csv("tidy_all_matches_docks_rl.csv")

#decide on the records that are true matches

tidy_all_matches_docks_rl %>%
  ggplot(aes(rl_score)) +
  geom_histogram(bins=60) +
  ggtitle("Histogram of Record Linkage Scores for all possible matches") +
  theme_economist()



tidy_all_matches_docks_rl %>%
  distinct(CLS_ID) %>%
  nrow()

#top score for each CLS_ID and MRIP DATE_TIME
####This is best right now
tidy_all_matches_docks_rl %>%
  group_by(CLS_ID,date_time_mrip) %>%
  top_n(n=1,rl_score) %>%
  ungroup() %>%
  group_by(CLS_ID,date_time_mrip) %>%
  top_n(n=-1,km_difference) %>%
  #View()
  nrow()

#need to filter it so that only one cls report is matched up
#just group by tripID and get top rl_score for each one
#if there is a tie do the thing I did just did

tidy_all_matches_docks_rl %>%
  group_by(CLS_ID,date_time_mrip) %>%
  top_n(n=1,rl_score) %>%
  ungroup() %>%
  group_by(CLS_ID,date_time_mrip) %>%
  top_n(n=-1,km_difference) %>%
  ungroup() %>%
  filter(tripID != 12112) %>%#a duplicate from a captain
  group_by(date_time_mrip) %>%
  top_n(n=1,rl_score) %>%
  ungroup() %>%
  group_by(tripID) %>%
  top_n(n=1,rl_score) %>% 
  ungroup() %>%
  #View()
  nrow()

#see if there are any tripID duplicates
tst <- tidy_all_matches_docks_rl %>%
  group_by(CLS_ID,date_time_mrip) %>%
  top_n(n=1,rl_score) %>%
  ungroup() %>%
  group_by(CLS_ID,date_time_mrip) %>%
  top_n(n=-1,km_difference) %>%
  ungroup() %>%
  filter(tripID != 12112) %>%#a duplicate from a captain
  group_by(date_time_mrip) %>%
  top_n(n=1,rl_score) %>%
  ungroup() %>%
  group_by(tripID) %>%
  top_n(n=1,rl_score) %>% 
  ungroup() %>%
  pull(tripID)

tst[duplicated(tst)] #yes, 3

#3 tripID duplicares still so investigate them
tidy_all_matches_docks_rl %>%
  group_by(CLS_ID,date_time_mrip) %>%
  top_n(n=1,rl_score) %>%
  ungroup() %>%
  group_by(CLS_ID,date_time_mrip) %>%
  top_n(n=-1,km_difference) %>%
  ungroup() %>%
  filter(tripID != 12112) %>%#a duplicate from a captain
  group_by(date_time_mrip) %>%
  top_n(n=1,rl_score) %>%
  ungroup() %>%
  group_by(tripID) %>%
  top_n(n=1,rl_score) %>% 
  ungroup() %>%
  filter(tripID %in% c(8133, 8245, 13245)) %>%
  View()

#solved by restricting reports to be at most 2 days before the sample
tidy_all_matches_docks_rl %>%
  group_by(CLS_ID,date_time_mrip) %>%
  top_n(n=1,rl_score) %>%
  ungroup() %>%
  group_by(CLS_ID,date_time_mrip) %>%
  top_n(n=-1,km_difference) %>%
  ungroup() %>%
  filter(tripID != 12112) %>%#a duplicate from a captain
  group_by(date_time_mrip) %>%
  top_n(n=1,rl_score) %>%
  ungroup() %>%
  group_by(tripID) %>%
  top_n(n=1,rl_score) %>% 
  ungroup() %>%
  mutate(date_diff = as.numeric(ymd(return_date_ymd) - ymd(date))) %>%
  filter(date_diff >= -2 )

#make this a new dataset
tidy_all_matches_one_per <- tidy_all_matches_docks_rl %>%
  group_by(CLS_ID,date_time_mrip) %>%
  top_n(n=1,rl_score) %>%
  ungroup() %>%
  group_by(CLS_ID,date_time_mrip) %>%
  top_n(n=-1,km_difference) %>%
  ungroup() %>%
  filter(tripID != 12112) %>%#a duplicate from a captain
  group_by(date_time_mrip) %>%
  top_n(n=1,rl_score) %>%
  ungroup() %>%
  group_by(tripID) %>%
  top_n(n=1,rl_score) %>% 
  ungroup() %>%
  mutate(date_diff = as.numeric(ymd(return_date_ymd) - ymd(date))) %>%
  filter(date_diff >= -2 )

#have a few duplicates of date_time_mrip
tst2<-tidy_all_matches_one_per %>%
  pull(date_time_mrip)
tst2[duplicated(tst2)]
tidy_all_matches_one_per %>% 
  filter(date_time_mrip %in% c(tst2[duplicated(tst2)]))# %>%
  #View() #seems like some people reported twice the same trip

tidy_all_matches_one_per <- tidy_all_matches_one_per %>% 
  group_by(date_time_mrip) %>%
  top_n(n=-1,report_date) %>%
  ungroup() #takes care of all but one

tidy_all_matches_one_per <- tidy_all_matches_one_per %>% 
  filter(tripID != 10800) #a duplicate somehow...

write_csv(tidy_all_matches_one_per,"tidy_all_matches_rl_one_per.csv")

#so now we have 318 rows of potential matches (restricted
#to reports at most 2 days before the sample), look at
#the distribution of the scores
tidy_all_matches_one_per %>%
  ggplot(aes(rl_score)) +
  geom_histogram(bins=65) +
  xlab("Matching Score") +
  ylab("Count") +
  theme_economist() +
  theme(axis.title.x = element_text(size=25),
        axis.title.y = element_text(size=25))

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




