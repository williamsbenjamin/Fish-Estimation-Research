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

#agree
p.agree_total_catch <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% 
  filter(between(diff_total_catch,-0.9999,0.9999)) %>%
  nrow() / 
  n_total.catch

#close
p.close_total_catch <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  filter(between(diff_total_catch,-5.9999,-1) |
           between(diff_total_catch,1,5.9999)) %>%
  nrow() /
  n_total.catch

#far
p.far_total_catch <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  filter(between(diff_total_catch,-15.9999,-6) |
           between(diff_total_catch,6,15.9999)) %>%
  nrow() /
  n_total.catch

#farther
p.farther_total_catch <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  filter(between(diff_total_catch,-25.9999,-16) |
           between(diff_total_catch,16,25.9999)) %>%
  nrow() /
  n_total.catch

#disagree
p.disagree_total_catch <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  filter(between(diff_total_catch,-3000,-26) |
           between(diff_total_catch,26,9006)) %>%
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

#agree
p.agree_total_release <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>%
  filter(between(diff_total_release,-0.9999,0.9999) ) %>%
  nrow() /
  n_total.release

#close
p.close_total_release <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>%
  filter(between(diff_total_release,-4.9999,-1) | 
                between(diff_total_release,1,4.9999)) %>%
  nrow() /
  n_total.release

#far
p.far_total_release <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>%
  filter(between(diff_total_release,-10.9999,-5) |
           between(diff_total_release,5,10.9999)) %>%
  nrow() /
  n_total.release

#disagree
p.disagree_total_release <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>%
  filter(between(diff_total_release,-3000,-11) |
           between(diff_total_release,11,300)) %>%
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

#agree
p.agree_total_distance <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>%
  filter(between(km_difference,0,15)) %>%
  nrow() /
  n_total_distance

#close
p.close_total_distance <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>%
  filter(between(km_difference,15.0001,40)) %>%
  nrow() /
  n_total_distance

#far
p.far_total_distance <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>%
  filter(between(km_difference,40.0001,75)) %>%
  nrow() /
  n_total_distance

#disagree
p.disagree_total_distance <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>%
  filter(between(km_difference,75.0001,4000)) %>%
  nrow() /
  n_total_distance

#Red Snapper Catch Difference (55 cases)

# n_total_rs <- tidy_all_matches_docks %>%
#   filter(nbAnglers == PARTY,
#          state==STATE,
#          date == return_date_ymd) %>%
#   mutate(red_diff = `RED SNAPPER_kept` - `RED SNAPPER_claim`) %>%
#   distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
#   drop_na(red_diff) %>%
#   nrow()
# #agree
# p.agree_total_rs <- tidy_all_matches_docks %>%
#   filter(nbAnglers == PARTY,
#          state==STATE,
#          date == return_date_ymd) %>%
#   mutate(red_diff = `RED SNAPPER_kept` - `RED SNAPPER_claim`) %>%
#   distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
#   drop_na(red_diff) %>%
#   filter(between(red_diff,-0.1,0.1)) %>%
#   nrow() /
#   n_total_rs
# 
# #close
# p.close_total_rs <- tidy_all_matches_docks %>%
#   filter(nbAnglers == PARTY,
#          state==STATE,
#          date == return_date_ymd) %>%
#   mutate(red_diff = `RED SNAPPER_kept` - `RED SNAPPER_claim`) %>%
#   distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
#   drop_na(red_diff) %>%
#   filter(between(red_diff,-4,-0.1001) |
#            between(red_diff,0.1001,4)) %>%
#   nrow() /
#   n_total_rs
# 
# #far
# p.far_total_rs <- tidy_all_matches_docks %>%
#   filter(nbAnglers == PARTY,
#          state==STATE,
#          date == return_date_ymd) %>%
#   mutate(red_diff = `RED SNAPPER_kept` - `RED SNAPPER_claim`) %>%
#   distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
#   drop_na(red_diff) %>%
#   filter(between(red_diff,-10,-4.001) | 
#            between(red_diff,4.001,10)) %>%
#   nrow() /
#   n_total_rs
# 
# #disagree
# p.disagree_total_rs <- tidy_all_matches_docks %>%
#   filter(nbAnglers == PARTY,
#          state==STATE,
#          date == return_date_ymd) %>%
#   mutate(red_diff = `RED SNAPPER_kept` - `RED SNAPPER_claim`) %>%
#   distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
#   filter(between(red_diff,-100,-10.0001) |
#            between(red_diff,10.0001,100)) %>%
#   nrow() /
#   n_total_rs

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

#agree
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

#close
p.close_total_num_species <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  mutate(spec_caught_diff = reported_species_kept_cls - 
           reported_species_claim_mrip) %>%
  distinct(departdate,returndate,date_time_mrip,
           .keep_all = T) %>% 
  filter(spec_caught_diff %in% c(-1,1)) %>%
  nrow() /
  n_total_num_species
#far
p.far_total_num_species <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  mutate(spec_caught_diff = reported_species_kept_cls - 
           reported_species_claim_mrip) %>%
  distinct(departdate,returndate,date_time_mrip,
           .keep_all = T) %>% 
  filter(spec_caught_diff %in% c(-2,2)) %>%
  nrow() /
  n_total_num_species

#disagree
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

#agree
p.agree_total_num_species_rel <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  mutate(spec_rel_diff = reported_species_released_cls - reported_species_release_mrip) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  filter(spec_rel_diff %in% c(0)) %>%
  nrow() /
  n_total_num_species_rel

#close
p.close_total_num_species_rel <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state==STATE,
         date == return_date_ymd) %>%
  mutate(spec_rel_diff = reported_species_released_cls - reported_species_release_mrip) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T) %>% #remove duplicates
  filter(spec_rel_diff %in% c(-1,1)) %>%
  nrow()/
  n_total_num_species_rel
#far
p.far_total_num_species_rel <- tidy_all_matches_docks %>%
  filter(nbAnglers == PARTY,
         state == STATE,
         date == return_date_ymd) %>%
  mutate(spec_rel_diff = reported_species_released_cls - reported_species_release_mrip) %>%
  distinct(departdate, returndate, date_time_mrip, .keep_all = T) %>% #remove duplicates
  filter(spec_rel_diff %in% c(-2,2)) %>%
  nrow() /
  n_total_num_species_rel
#disagree
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
  filter(abs(total_kept_cls - total_claim_mrip) < 5,#too few obs, make total kept be "close"
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,strat_id,INTSITE,.keep_all = T)  %>% 
  drop_na(diff_cls_mrip_anglers) %>% 
  nrow()

#agree
p.agree_num_anglers <- tidy_all_matches_docks %>%
  filter(abs(total_kept_cls - total_claim_mrip) < 5,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,strat_id,INTSITE,.keep_all = T)  %>% 
  drop_na(diff_cls_mrip_anglers) %>%
  filter(diff_cls_mrip_anglers %in% c(0)) %>% 
  nrow() / 
  n_num_anglers

p.close_num_anglers <- tidy_all_matches_docks %>%
  filter(abs(total_kept_cls - total_claim_mrip) < 5,
         state==STATE,
         date == return_date_ymd) %>%
  distinct(departdate,returndate,date_time_mrip,strat_id,INTSITE,.keep_all = T)  %>% 
  drop_na(diff_cls_mrip_anglers) %>%
  filter(diff_cls_mrip_anglers %in% c(-1,1)) %>% 
  nrow()/ 
  n_num_anglers


p.disagree_num_anglers <- tidy_all_matches_docks %>%
  filter(abs(total_kept_cls - total_claim_mrip) < 5,
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
  filter(abs(total_kept_cls - total_claim_mrip) < 5,
         PARTY == nbAnglers,
         state==STATE,
         abs(`RED SNAPPER_claim` - `RED SNAPPER_kept`) <= 1) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T)  %>% 
  drop_na(date_diff) %>% 
  nrow() #139 cases           

p.agree_date <- tidy_all_matches_docks %>%
  filter(abs(total_kept_cls - total_claim_mrip) < 5,
         PARTY == nbAnglers,
         state==STATE,
         abs(`RED SNAPPER_claim` - `RED SNAPPER_kept`) <= 1) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T)  %>% 
  drop_na(date_diff) %>% 
  filter(between(date_diff,-0.15,0.15)) %>%
  nrow() /
  n_total_date
#close
p.close_date <- tidy_all_matches_docks %>%
  filter(abs(total_kept_cls - total_claim_mrip) < 5,
         PARTY == nbAnglers,
         state==STATE,
         abs(`RED SNAPPER_claim` - `RED SNAPPER_kept`) <= 1) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T)  %>% 
  drop_na(date_diff) %>%
  filter(between(date_diff,-1.5, -0.15001) |
           between(date_diff,0.15001,1.5)) %>%
  nrow() /
  n_total_date
#far
p.far_date <- tidy_all_matches_docks %>%
  filter(abs(total_kept_cls - total_claim_mrip) < 5,
         PARTY == nbAnglers,
         state==STATE,
         abs(`RED SNAPPER_claim` - `RED SNAPPER_kept`) <= 1) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T)  %>% 
  drop_na(date_diff) %>%
  filter(between(date_diff,-5,-1.5001) |
          between(date_diff, 1.5001, 5)) %>%
  nrow() /
  n_total_date
#further
p.further_date <- tidy_all_matches_docks %>%
  filter(abs(total_kept_cls - total_claim_mrip) < 5,
         PARTY == nbAnglers,
         state==STATE,
         abs(`RED SNAPPER_claim` - `RED SNAPPER_kept`) <= 1) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T)  %>% 
  drop_na(date_diff) %>% 
  filter(between(date_diff,-15, -5.001) |
           between(date_diff, 5.001, 15)) %>%
  nrow() /
  n_total_date
#disagree
p.disagree_date <- tidy_all_matches_docks %>%
  filter(abs(total_kept_cls - total_claim_mrip) < 5,
         PARTY == nbAnglers,
         state==STATE,
         abs(`RED SNAPPER_claim` - `RED SNAPPER_kept`) <= 1) %>%
  distinct(departdate,returndate,date_time_mrip,.keep_all = T)  %>% 
  drop_na(date_diff) %>% 
  filter(between(date_diff,-60, -15.001) |
           between(date_diff, 15.001, 60)) %>%
  nrow() /
  n_total_date

#wave_year

