# hold all but one variable constant to get distribution of 
# linking variables

#######
## Total Catch
#######

n_total.catch <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state == STATE,
         date == return_date_ymd) %>%
  distinct(departdate, returndate, date_time_mrip, .keep_all = T) %>% 
  filter(diff_total_catch != "NA") %>%
  nrow()

p.agree_total_catch <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% 
  filter(diff_total_catch != "NA" ) %>%
  filter(diff_total_catch == 0 ) %>%
  nrow() / 
  n_total.catch

p.close_total_catch <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  filter(diff_total_catch %in% c(-4:-1,1:4)) %>%
  nrow() /
  n_total.catch

p.disagree_total_catch <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  filter(diff_total_catch %in% c(-3000:-5,5:9006)) %>%
  nrow() /
  n_total.catch

####
# Total Release
####

n_total.release <- tidy_all_matches_docks %>%
     filter(nbAnglers == PARTY,
                       state==STATE,
                       date == return_date_ymd) %>%
     distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>%
     filter(diff_total_release != "NA" ) %>%
     drop_na(CLS_ID) %>%
     nrow()

p.agree_total_release <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>%
  filter(diff_total_release == 0 ) %>%
  nrow() /
  n_total.release

p.close_total_release <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>%
  filter(diff_total_release %in% c(-4:-1,1:4)) %>%
  nrow() /
  n_total.release

p.disagree_total_release <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>%
  filter(diff_total_release %in% c(-3000:-5,5:300)) %>%
  nrow() /
  n_total.release

#####
# KM Distance
#####

n_total_distance <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>%
  filter(km_difference != "NA") %>%
  nrow()

p.agree_total_distance <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>%
  filter(between(km_difference,0,25)) %>%
  nrow() /
  n_total_distance

p.close_total_distance <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>%
  filter(between(km_difference,25.001,70)) %>%
  nrow() /
  n_total_distance

p.disagree_total_distance <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>%
  filter(between(km_difference,70.001,4000)) %>%
  nrow() /
  n_total_distance

#Red Snapper Catch Difference (81 cases)

n_total_rs <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  mutate(red_diff = `RED SNAPPER_kept` - `RED SNAPPER_claim`) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  drop_na(red_diff) %>%
  nrow()

p.agree_total_rs <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  mutate(red_diff = `RED SNAPPER_kept` - `RED SNAPPER_claim`) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  drop_na(red_diff) %>%
  filter(red_diff %in% c(0)) %>%
  nrow() /
  n_total_rs

p.close_total_rs <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  mutate(red_diff = `RED SNAPPER_kept` - `RED SNAPPER_claim`) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  drop_na(red_diff) %>%
  filter(red_diff %in% c(-5:-1,1:5)) %>%
  nrow() /
  n_total_rs

p.disagree_total_rs <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  mutate(red_diff = `RED SNAPPER_kept` - `RED SNAPPER_claim`) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  filter(red_diff %in% c(-100:-6,6:100)) %>%
  nrow() /
  n_total_rs

######
# Number Species Caught Difference
######

n_total_num_species <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  mutate(spec_caught_diff = reported_species_kept_cls - 
           reported_species_claim_mrip) %>%
  distinct(departdate,returndate,date_time_mrip,
           .keep_all = T) %>% 
  drop_na(spec_caught_diff) %>%
  nrow()

p.agree_total_num_species <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  mutate(spec_caught_diff = reported_species_kept_cls - 
           reported_species_claim_mrip) %>%
  distinct(departdate,returndate,date_time_mrip,
           .keep_all = T) %>% #remove duplicates
  filter(spec_caught_diff %in% c(0)) %>%
  nrow() /
  n_total_num_species

p.close_total_num_species <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  mutate(spec_caught_diff = reported_species_kept_cls - 
           reported_species_claim_mrip) %>%
  distinct(departdate,returndate,date_time_mrip,
           .keep_all = T) %>% 
  filter(spec_caught_diff %in% c(-2,-1,1,2)) %>%
  nrow() /
  n_total_num_species

p.disagree_total_num_species <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  mutate(spec_caught_diff = reported_species_kept_cls - reported_species_claim_mrip) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  filter(spec_caught_diff %in% c(-10:-3,3:10)) %>%
  nrow() /
  n_total_num_species

#####
# Number Species Released Difference
#####

n_total_num_species_rel <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  mutate(spec_rel_diff = reported_species_released_cls - reported_species_release_mrip) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  drop_na(spec_rel_diff) %>%
  nrow()

p.agree_total_num_species_rel <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  mutate(spec_rel_diff = reported_species_released_cls - reported_species_release_mrip) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  filter(spec_rel_diff %in% c(0)) %>%
  nrow() /
  n_total_num_species_rel

p.close_total_num_species_rel <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  mutate(spec_rel_diff = reported_species_released_cls - reported_species_release_mrip) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  filter(spec_rel_diff %in% c(-2,-1,1,2)) %>%
  nrow()/
  n_total_num_species_rel

p.disagree_total_num_spec_rel <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  mutate(spec_rel_diff = reported_species_released_cls - reported_species_release_mrip) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  filter(spec_rel_diff %in% c(-10:-3,3:10)) %>%
  nrow()/
  n_total_num_species_rel

#####
# Number of anglers
#####
n_num_anglers <- tidy_all_matches_docks %>%
  filter(total_kept_cls == total_claim_mrip,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,strat_id,INTSITE,.keep_all = T)  %>% 
  drop_na(diff_cls_mrip_anglers) %>% 
  nrow()

p.agree_num_anglers <- tidy_all_matches_docks %>%
  filter(total_kept_cls == total_claim_mrip,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,strat_id,INTSITE,.keep_all = T)  %>% 
  drop_na(diff_cls_mrip_anglers) %>%
  filter(diff_cls_mrip_anglers %in% c(0)) %>% 
  nrow() / 
  n_num_anglers

p.close_num_anglers <- tidy_all_matches_docks %>%
  filter(total_kept_cls == total_claim_mrip,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,strat_id,INTSITE,.keep_all = T)  %>% 
  drop_na(diff_cls_mrip_anglers) %>%
  filter(diff_cls_mrip_anglers %in% c(-1,1)) %>% 
  nrow()/ 
  n_num_anglers


p.disagree_num_anglers <- tidy_all_matches_docks %>%
  filter(total_kept_cls == total_claim_mrip,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,strat_id,INTSITE,.keep_all = T)  %>% 
  drop_na(diff_cls_mrip_anglers) %>%
  filter(diff_cls_mrip_anglers %in% c(-15:-2,2:15)) %>% 
  nrow()/ 
  n_num_anglers

#####
# Date
#####

n_total_date <- tidy_all_matches_docks %>%
  filter(total_kept_cls == total_claim_mrip,
         PARTY == nbAnglers,
         state==STATE) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T)  %>% 
  mutate(date_diff=as.numeric(return_date_ymd - date)) %>%
  drop_na(date_diff) %>% 
  filter(date_diff > -2) %>% #filter such that the report is
  nrow() #62 cases           #at most 2 days before the sample

p.agree_date <- tidy_all_matches_docks %>%
  filter(total_kept_cls == total_claim_mrip,
         PARTY == nbAnglers,
         state==STATE) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T)  %>% 
  mutate(date_diff=as.numeric(return_date_ymd - date)) %>%
  drop_na(date_diff) %>% 
  filter(date_diff >= -2) %>% 
  filter(date_diff == 0) %>%
  nrow() /
  n_total_date

p.close_date <- tidy_all_matches_docks %>%
  filter(total_kept_cls == total_claim_mrip,
         PARTY == nbAnglers,
         state==STATE) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T)  %>% 
  mutate(date_diff=as.numeric(return_date_ymd - date)) %>%
  drop_na(date_diff) %>%
  filter(date_diff > -2) %>% 
  filter(date_diff %in% c(-2,-1,1,2)) %>%
  nrow() /
  n_total_date

p.disagree_date <- tidy_all_matches_docks %>%
  filter(total_kept_cls == total_claim_mrip,
         PARTY == nbAnglers,
         state==STATE) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T)  %>% 
  mutate(date_diff=as.numeric(return_date_ymd - date)) %>%
  drop_na(date_diff) %>% 
  filter(date_diff %in% c(3:100)) %>%
  nrow() /
  n_total_date
