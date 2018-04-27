library(tidyverse)
library(ggthemes)
tidy_all_matches_docks <- read_csv("tidy_all_matches_docks_2017.csv")

#use total kept/claim,
#Party/nbanglers
#state
#date/returndate
#as the linking variables for now
#I still see one row that is a cls trip that took 2 trips,
#was sampled twice and only reported once, so I manually remove it
#I keep the one with a smaller distance between reporting site and dock (km_diff)

#these are the "perfect matches"
perfect_match_2017 <- tidy_all_matches_docks %>%
  filter(total_kept_cls == total_claim_mrip,
         nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T)  %>% #remove duplicates
  filter(km_difference != 180.11977) #remove a record that was 2 mrip samples for only 1 cls report

#now hold all but one variable constant to get distribution of linking variables
tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  mutate(red_diff = `RED SNAPPER_kept` - `RED SNAPPER_claim`) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  ggplot(aes(diff_total_catch))+
  geom_histogram(bins=10)+
  theme_economist()

##hold all but total_catch/kept equal


tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  mutate(red_diff = `RED SNAPPER_kept` - `RED SNAPPER_claim`) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  ggplot(aes(diff_total_release))+
  geom_histogram(bins=10,fill="royalblue4")+
  theme_economist()

tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  mutate(red_diff = `RED SNAPPER_kept` - `RED SNAPPER_claim`) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  ggplot(aes(km_difference))+
  geom_histogram(bins=20,fill="royalblue4")+
  theme_economist()

tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  mutate(red_diff = `RED SNAPPER_kept` - `RED SNAPPER_claim`) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  ggplot(aes(red_diff))+
  geom_histogram(bins=10)+
  theme_economist()

tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  mutate(red_diff = `RED SNAPPER_kept` - `RED SNAPPER_claim`) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  mutate(dolph_diff = DOLPHIN_claim-DOLPHIN_claim) %>%
  ggplot(aes(dolph_diff))+
  geom_histogram(bins=10)+
  theme_economist()

#hold all but party equal #not many observations like this
tidy_all_matches_docks %>%
  filter(total_kept_cls == total_claim_mrip,
         state==STATE,
         date == return_date_ymd) %>%
  mutate(red_diff = `RED SNAPPER_kept` - `RED SNAPPER_claim`) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T)  %>% 
  ggplot(aes(diff_cls_mrip_anglers)) +
  geom_histogram(bins=10) +
  theme_economist()

tidy_all_matches_docks %>%
  filter(total_kept_cls == total_claim_mrip,
         state==STATE,
         date == return_date_ymd) %>%
  mutate(red_diff = `RED SNAPPER_kept` - `RED SNAPPER_claim`) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T)  %>% 
  ggplot(aes(red_diff)) +
  geom_histogram(bins=10) +
  theme_economist()

#hold all but date equal 
tidy_all_matches_docks %>%
  filter(total_kept_cls == total_claim_mrip,
         PARTY == nbAnglers,
         state==STATE) %>%
  mutate(red_diff = `RED SNAPPER_kept` - `RED SNAPPER_claim`) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T)  %>% 
  mutate(date_diff=as.numeric(return_date_ymd - date)) %>%
  ggplot(aes(date_diff)) +
  geom_histogram(bins=10) +
  theme_economist()

tidy_all_matches_docks %>%
  filter(total_kept_cls == total_claim_mrip,
         PARTY == nbAnglers,
         state==STATE) %>%
  mutate(red_diff = `RED SNAPPER_kept` - `RED SNAPPER_claim`) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T)  %>% 
  ggplot(aes(red_diff)) +
  geom_histogram(bins=10) +
  theme_economist()

#hold all but STATE equal 
tidy_all_matches_docks %>%
  filter(total_kept_cls == total_claim_mrip,
         PARTY == nbAnglers,
         date == return_date_ymd)  %>%
  mutate(red_diff = `RED SNAPPER_kept` - `RED SNAPPER_claim`) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T)  %>% 
  ggplot(aes(state,fill=STATE)) +
  geom_bar() +
  theme_economist()

tidy_all_matches_docks %>%
  filter(total_kept_cls == total_claim_mrip,
         PARTY == nbAnglers,
         date == return_date_ymd)  %>%
  mutate(red_diff = `RED SNAPPER_kept` - `RED SNAPPER_claim`) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T)  %>% 
  ggplot(aes(red_diff)) +
  geom_histogram() +
  theme_economist()

#all but red snapper equal #very few obs
tidy_all_matches_docks %>%
  filter(total_kept_cls == total_claim_mrip,
         nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  mutate(red_diff = `RED SNAPPER_kept` - `RED SNAPPER_claim`) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T)  %>% #remove duplicates
  ggplot(aes(red_diff)) +
  geom_histogram(bins=10) +
  theme_economist()

#relax it a bit
tidy_all_matches_docks %>%
  filter(total_kept_cls == total_claim_mrip,
         nbAnglers == PARTY,
         state==STATE) %>%
  mutate(red_diff = `RED SNAPPER_kept` - `RED SNAPPER_claim`) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T)  %>% #remove duplicates
  ggplot(aes(red_diff)) +
  geom_histogram(bins=10) +
  theme_economist()
