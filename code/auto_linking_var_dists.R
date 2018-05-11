library(tidyverse)

# Data : All Possible Matches
# The data already has variables for the difference between
# the variables in cls and in mrip
tidy_all_matches_docks <- read_csv("data/all_possible_matches_16_17.csv")

# hold all but one variable constant to get distribution of 
# linking variables

#######
## Total Catch
#######

tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state == STATE,
         date == return_date_ymd) %>%
  distinct(departdate, returndate, date_time_mrip, .keep_all = T) %>% 
  filter(diff_total_catch != "NA") %>%
  nrow()

tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% 
  #remove duplicates
  filter(diff_total_catch != "NA" ) %>%
  filter(diff_total_catch == 0 ) %>%
  nrow()

tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  filter(diff_total_catch %in% c(-4:-1,1:4)) %>%
  nrow()

tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  filter(diff_total_catch %in% c(-15:-5,5:15)) %>%
  nrow()

tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  filter(diff_total_catch %in% c(-30:-16,16:30)) %>%
  nrow()

tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  filter(diff_total_catch %in% c(-30000:-31,31:9006)) %>% #everything else
  nrow()


#diff_total_release 215 cases
tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  ggplot(aes(diff_total_release))+
  geom_histogram(bins=10,fill="royalblue4")+
  theme_economist()
tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>%
  filter(diff_total_release != "NA" ) %>%
  drop_na(CLS_ID) %>%
  nrow()

tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>%
  filter(diff_total_release == 0 ) %>%
  nrow()

tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>%
  filter(diff_total_release %in% c(-4:-1,1:4)) %>%
  nrow()
tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>%
  filter(diff_total_release %in% c(-15:-5,5:15)) %>%
  nrow()
tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>%
  filter(diff_total_release %in% c(-30:-16,16:30)) %>%
  nrow()
tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>%
  filter(diff_total_release %in% c(-3000:-31,31:300)) %>%
  nrow()

##START HERR BENN
#km_diff, 215 cases
tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  ggplot(aes(km_difference))+
  geom_histogram(bins=20,fill="royalblue4")+
  theme_economist()

tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>%
  filter(km_difference != "NA") %>%
  nrow()

tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>%
  filter(between(km_difference,0,25)) %>%
  nrow()

tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>%
  filter(between(km_difference,25.001,70)) %>%
  nrow()

tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>%
  filter(between(km_difference,70.001,200)) %>%
  nrow()

tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>%
  filter(between(km_difference,200.001,4000)) %>%
  nrow()

#Red Snapper Catch Difference (81 cases)
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
  drop_na(red_diff) %>%
  nrow()

tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  mutate(red_diff = `RED SNAPPER_kept` - `RED SNAPPER_claim`) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  drop_na(red_diff) %>%
  filter(red_diff %in% c(0)) %>%
  nrow()

tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  mutate(red_diff = `RED SNAPPER_kept` - `RED SNAPPER_claim`) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  drop_na(red_diff) %>%
  filter(red_diff %in% c(-5:-1,1:5)) %>%
  nrow()

tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  mutate(red_diff = `RED SNAPPER_kept` - `RED SNAPPER_claim`) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  filter(red_diff %in% c(-15:-6,6:15)) %>%
  nrow()

tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  mutate(red_diff = `RED SNAPPER_kept` - `RED SNAPPER_claim`) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  drop_na(red_diff) %>%
  filter(red_diff %in% c(-100:-16,16:100)) %>%
  nrow()


#Number Species Caught Difference
tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  mutate(spec_caught_diff = reported_species_kept_cls - reported_species_claim_mrip) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  ggplot(aes(spec_caught_diff))+
  geom_histogram(bins=15)+
  theme_economist()
#frequencies - 215 cases
tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  mutate(spec_caught_diff = reported_species_kept_cls - reported_species_claim_mrip) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  drop_na(spec_caught_diff) %>%
  nrow()

tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  mutate(spec_caught_diff = reported_species_kept_cls - reported_species_claim_mrip) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  filter(spec_caught_diff %in% c(0)) %>%
  nrow()

tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  mutate(spec_caught_diff = reported_species_kept_cls - reported_species_claim_mrip) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  filter(spec_caught_diff %in% c(-2,-1,1,2)) %>%
  nrow()

tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  mutate(spec_caught_diff = reported_species_kept_cls - reported_species_claim_mrip) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  filter(spec_caught_diff %in% c(-10:-3,3:10)) %>%
  nrow()

#Number Species Released Difference
tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  mutate(spec_rel_diff = reported_species_released_cls - reported_species_release_mrip) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  ggplot(aes(spec_rel_diff))+
  geom_histogram(bins=15)+
  theme_economist()

#frequencies - 215 cases
tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  mutate(spec_rel_diff = reported_species_released_cls - reported_species_release_mrip) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  drop_na(spec_rel_diff) %>%
  nrow()

tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  mutate(spec_rel_diff = reported_species_released_cls - reported_species_release_mrip) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  filter(spec_rel_diff %in% c(0)) %>%
  nrow()

tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  mutate(spec_rel_diff = reported_species_released_cls - reported_species_release_mrip) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  filter(spec_rel_diff %in% c(-2,-1,1,2)) %>%
  nrow()

tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  mutate(spec_rel_diff = reported_species_released_cls - reported_species_release_mrip) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  filter(spec_rel_diff %in% c(-10:-3,3:10)) %>%
  nrow()

#hold all but party equal 
#not many observations like this
#difference in number of anglers (35 cases)
tidy_all_matches_docks %>%
  filter(total_kept_cls == total_claim_mrip,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,INTSITE,.keep_all = T)  %>% 
  ggplot(aes(diff_cls_mrip_anglers)) +
  geom_histogram(bins=10) +
  theme_economist()

tidy_all_matches_docks %>%
  filter(total_kept_cls == total_claim_mrip,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,strat_id,INTSITE,.keep_all = T)  %>% 
  drop_na(diff_cls_mrip_anglers) %>% 
  nrow()

tidy_all_matches_docks %>%
  filter(total_kept_cls == total_claim_mrip,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,strat_id,INTSITE,.keep_all = T)  %>% 
  drop_na(diff_cls_mrip_anglers) %>%
  filter(diff_cls_mrip_anglers %in% c(0)) %>% 
  nrow()

tidy_all_matches_docks %>%
  filter(total_kept_cls == total_claim_mrip,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,strat_id,INTSITE,.keep_all = T)  %>% 
  drop_na(diff_cls_mrip_anglers) %>%
  filter(diff_cls_mrip_anglers %in% c(-1,1)) %>% 
  nrow()


tidy_all_matches_docks %>%
  filter(total_kept_cls == total_claim_mrip,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,strat_id,INTSITE,.keep_all = T)  %>% 
  drop_na(diff_cls_mrip_anglers) %>%
  filter(diff_cls_mrip_anglers %in% c(-15:-2,2:15)) %>% 
  nrow()

#hold all but date equal 
#look at difference in date (62 cases)
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
  distinct(departdate,returndate,date_time_mrip,.keep_all = T)  %>% 
  mutate(date_diff=as.numeric(return_date_ymd - date)) %>%
  drop_na(date_diff) %>% 
  filter(date_diff > -2) %>% #filter such that the report is
  nrow() #62 cases           #at most 2 days before the sample

tidy_all_matches_docks %>%
  filter(total_kept_cls == total_claim_mrip,
         PARTY == nbAnglers,
         state==STATE) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T)  %>% 
  mutate(date_diff=as.numeric(return_date_ymd - date)) %>%
  drop_na(date_diff) %>% 
  filter(date_diff >= -2) %>% 
  filter(date_diff == 0) %>%
  nrow()


tidy_all_matches_docks %>%
  filter(total_kept_cls == total_claim_mrip,
         PARTY == nbAnglers,
         state==STATE) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T)  %>% 
  mutate(date_diff=as.numeric(return_date_ymd - date)) %>%
  drop_na(date_diff) %>%
  filter(date_diff > -2) %>% 
  filter(date_diff %in% c(-2,-1,1,2)) %>%
  nrow()

tidy_all_matches_docks %>%
  filter(total_kept_cls == total_claim_mrip,
         PARTY == nbAnglers,
         state==STATE) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T)  %>% 
  mutate(date_diff=as.numeric(return_date_ymd - date)) %>%
  drop_na(date_diff) %>% 
  filter(date_diff %in% c(3:100)) %>%
  nrow()


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


