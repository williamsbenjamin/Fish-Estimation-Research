
#Assume the population has been simulated

#packages needed 
library(dplyr)
library(lubridate)
library(geosphere)
library(survey)
library(haven)
library(truncdist)
#functions needed
neg_to_zero <- function(x){
  if(x < 0){ x = 0}
  x
}

##############
# REPLICATE THE SAMPLES
# n TIMES
##############

t_yp_est <- c()
t_yc_est <- c()
t_y2_est <- c()
t_ydiff_est <- c()
t_ynew_est <- c()
t_yp_se <- c()
t_yc_se <- c()
t_y2_se <- c()
t_ydiff_se <- c()
t_ynew_se <- c()
n1_hat_total <- c()
n1_hat_se <- c()
fp_rate <- c()
fn_rate <- c()
tp_rate <- c()
mm_rate <- c()

for(jj in 1:2){
  
  ###################################
  #Take a sample of 200 PSUs
  ###################################
  
  samp1_psu <- nest_pop %>% 
    sample_n(size = 200,replace = F,weight = (prob)) %>% 
    select(psu_id)
  
  s_pop <- tibble()
  
  for(i in 1:nrow(samp1_psu)){
    a <- filter(nest_pop,psu_id == samp1_psu$psu_id[i]) %>% 
      sample_n(1)
    s_pop <- bind_rows(s_pop,
                       a)
  }
  
  ##################################
  #Simulate the self-reports
  ##################################
  
  # In 2017 actual mrip data, we have about 100 true matches
  # Which is approx 7% of the sample (n2)
  # So make n1 be 7% of N
  
  n1 <- round(nrow(pop) * 0.07)  
  
  # 40% of the CLS sampled trips actually made a report
  
  cls_ids_cls <- cls17 %>% 
    select(CLS_ID) 
  
  self_gen <- s_pop %>% 
    unnest() %>% 
    filter(CLS == 1) %>% 
    filter(CLS_ID %in% cls17$CLS_ID) %>% 
    select(cls_party : true_match_id)
  
  additional_self_reps <- n1 - nrow(self_gen)
  self_reports <- sample_n(tbl = cls_ids_cls,
                           size = additional_self_reps,
                           replace = T
  )
  
  self_reports <- self_gen %>% 
    sample_n(additional_self_reps,
             replace = T) %>% 
    mutate(true_match_id = 0) %>% 
    mutate(CLS_ID = self_reports$CLS_ID) %>% 
    mutate(cls_part = round(cls_party + rnorm(additional_self_reps,
                                              0,0.5)),
           cls_harvest = round(cls_harvest + rnorm(additional_self_reps,
                                                   0,10)),
           cls_release = round(cls_release + rnorm(additional_self_reps,
                                                   0,20)),
           cls_red_snapper_harvest = round(cls_red_snapper_harvest +
                                             rnorm(additional_self_reps,
                                                   0,2)),
           species_caught_cls = round(species_caught_cls +
                                        rnorm(additional_self_reps,
                                              0,4)),
           species_released_cls = round(species_released_cls +
                                          rnorm(additional_self_reps,0,6)),
           date_cls = date_cls + rnorm(additional_self_reps,0,20),
           d_cls = day(date_cls),
           CLS_LAT = CLS_LAT + rnorm(additional_self_reps,0,3),
           CLS_LONG = CLS_LONG + rnorm(additional_self_reps,0,5)) %>% 
    mutate(cls_party = if_else(cls_part <=0,
                               cls_party,cls_part),
           cls_harvest = sapply(cls_harvest, neg_to_zero),
           cls_release = sapply(cls_release, neg_to_zero),
           cls_red_snapper_harvest = sapply(cls_red_snapper_harvest,
                                            neg_to_zero),
           species_caught_cls = sapply(species_caught_cls,
                                       neg_to_zero),
           species_released_cls = sapply(species_released_cls,
                                         neg_to_zero)) %>% 
    select(-cls_part) %>% 
    bind_rows(self_gen) %>% 
    mutate(trip_id = 1:n())
  ######################################
  #Prep Data for Record Linkage
  #######################################
  
  self_reports <- self_reports %>% 
    select(-id) %>% 
    rename(true_match_id_cls = true_match_id)
  
  sample_pop <- s_pop %>%
    unnest() %>%
    select(-c(cls_party:CLS_LONG)) %>% 
    rename(true_match_id_mrip = true_match_id)
  
  tidy_all_matches_docks <- self_reports %>% 
    drop_na(CLS_ID) %>% 
    inner_join(sample_pop, by = "CLS_ID")
  
  ###########################################
  #Block on CLS_ID then join
  ############################################
  
  tidy_all_matches_docks <- tidy_all_matches_docks %>% 
    mutate(diff_total_catch = total_harvest - cls_harvest,
           diff_total_release = total_release - cls_release,
           diff_red_snapper = red_snapper_harvest - cls_red_snapper_harvest,
           km_difference = distHaversine(
             as.matrix(tibble(CLS_LONG, CLS_LAT)),
             as.matrix(tibble(SITE_LONG, SITE_LAT))) / 1000,
           diff_spec_caught = species_caught_mrip - species_caught_cls,
           diff_spec_rel = species_released_mrip - species_released_cls,
           diff_num_anglers = party - cls_party,
           diff_date = date - date_cls)
  
  ################################
  # Linking Variable Distributions
  ################################
  ####
  # Total Catch
  ####
  n_match <- tidy_all_matches_docks %>%
    filter(true_match_id_cls == true_match_id_mrip) %>%
    distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>% 
    nrow()
  #agree
  p.agree_total_catch <- tidy_all_matches_docks %>%
    filter(true_match_id_cls == true_match_id_mrip) %>%
    distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>% 
    filter(!is.na(diff_total_catch)) %>% 
    filter(between(diff_total_catch,-0.99999,0.99999)) %>%
    nrow() / 
    n_match
  
  #close
  p.close_total_catch <- tidy_all_matches_docks %>%
    filter(true_match_id_cls == true_match_id_mrip) %>%
    distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T)  %>% #remove duplicates
    filter(between(diff_total_catch,-5.99999,-1) |
             between(diff_total_catch,1,5.99999)) %>%
    nrow() /
    n_match
  
  #far
  p.far_total_catch <- tidy_all_matches_docks %>%
    filter(true_match_id_cls == true_match_id_mrip) %>%
    distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T)  %>% 
    filter(!is.na(diff_total_catch)) %>% 
    filter(between(diff_total_catch,-15.99999,-6) |
             between(diff_total_catch,6,15.99999)) %>%
    nrow() /
    n_match
  
  #disagree
  p.disagree_total_catch <- tidy_all_matches_docks %>%
    filter(true_match_id_cls == true_match_id_mrip) %>%
    distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T)  %>% 
    filter(!is.na(diff_total_catch)) %>% 
    filter(between(diff_total_catch,-3000,-16) |
             between(diff_total_catch,16,9999)) %>%
    nrow() /
    n_match
  
  ####
  # Total Release
  ####
  
  n_total.release <- tidy_all_matches_docks %>%
    filter(true_match_id_cls == true_match_id_mrip) %>%
    distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>% 
    nrow()
  
  #agree
  p.agree_total_release <- tidy_all_matches_docks %>%
    filter(true_match_id_cls == true_match_id_mrip) %>%
    distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>% 
    filter(between(diff_total_release,-0.9999,0.9999) ) %>%
    nrow() /
    n_total.release
  
  #close
  p.close_total_release <- tidy_all_matches_docks %>%
    filter(true_match_id_cls == true_match_id_mrip) %>%
    distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>%
    filter(between(diff_total_release,-4.9999,-1) | 
             between(diff_total_release,1,4.9999)) %>%
    nrow() /
    n_total.release
  
  #far
  p.far_total_release <- tidy_all_matches_docks %>%
    filter(true_match_id_cls == true_match_id_mrip) %>%
    distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>%
    filter(between(diff_total_release,-10.9999,-5) |
             between(diff_total_release,5,10.9999)) %>%
    nrow() /
    n_total.release
  
  #disagree
  p.disagree_total_release <- tidy_all_matches_docks %>%
    filter(true_match_id_cls == true_match_id_mrip) %>%
    distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>%
    filter(between(diff_total_release,-3000,-11) |
             between(diff_total_release,11,300)) %>%
    nrow() /
    n_total.release
  
  #####
  # KM Distance
  #####
  
  n_total_distance <- tidy_all_matches_docks %>%
    filter(true_match_id_cls == true_match_id_mrip) %>%
    distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>%
    filter(km_difference != "NA") %>%
    nrow()
  
  #agree
  p.agree_total_distance <- tidy_all_matches_docks %>%
    filter(true_match_id_cls == true_match_id_mrip) %>%
    distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>%
    filter(km_difference != "NA") %>%
    filter(between(km_difference,0,15)) %>%
    nrow() /
    n_total_distance
  
  #close
  p.close_total_distance <- tidy_all_matches_docks %>%
    filter(true_match_id_cls == true_match_id_mrip) %>%
    distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>%
    filter(km_difference != "NA") %>%
    filter(between(km_difference,15.0001,40)) %>%
    nrow() /
    n_total_distance
  
  #disagree
  p.disagree_total_distance <- tidy_all_matches_docks %>%
    filter(true_match_id_cls == true_match_id_mrip) %>%
    distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>%
    filter(km_difference != "NA") %>%
    filter(between(km_difference,40.0001,4000)) %>%
    nrow() /
    n_total_distance
  
  #Red Snapper Catch Difference
  
  n_total_rs <- tidy_all_matches_docks %>%
    filter(true_match_id_cls == true_match_id_mrip) %>%
    distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>%
    nrow()
  #agree
  p.agree_total_rs <- tidy_all_matches_docks %>%
    filter(true_match_id_cls == true_match_id_mrip) %>%
    distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>% 
    filter(between(diff_red_snapper,-0.01,0.01)) %>%
    nrow() /
    n_total_rs
  
  #close
  p.disagree_total_rs <- tidy_all_matches_docks %>%
    filter(true_match_id_cls == true_match_id_mrip) %>%
    distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>%
    filter(between(diff_red_snapper,-10,-0.1001) |
             between(diff_red_snapper,0.1001,10)) %>%
    nrow() /
    n_total_rs
  
  
  ######
  # Number Species Caught Difference
  ######
  
  n_total_num_species <- tidy_all_matches_docks %>%
    filter(true_match_id_cls == true_match_id_mrip) %>%
    distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>%
    nrow()
  
  p.agree_total_num_species <- tidy_all_matches_docks %>%
    filter(true_match_id_cls == true_match_id_mrip) %>%
    distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>% 
    filter(diff_spec_caught %in% c(0)) %>%
    nrow() /
    n_total_num_species
  
  #close
  p.close_total_num_species <- tidy_all_matches_docks %>%
    filter(true_match_id_cls == true_match_id_mrip) %>%
    distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>% 
    filter(diff_spec_caught %in% c(-1,1)) %>%
    nrow() /
    n_total_num_species
  #far
  p.far_total_num_species <- tidy_all_matches_docks %>%
    filter(true_match_id_cls == true_match_id_mrip) %>%
    distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>% 
    filter(diff_spec_caught %in% c(-2,2)) %>%
    nrow() /
    n_total_num_species
  
  #disagree
  p.disagree_total_num_species <- tidy_all_matches_docks %>%
    filter(true_match_id_cls == true_match_id_mrip) %>%
    distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>% #remove duplicates
    filter(diff_spec_caught %in% c(-10:-3,3:10)) %>%
    nrow() /
    n_total_num_species
  
  #####
  # Number Species Released Difference
  #####
  
  n_total_num_species_rel <- tidy_all_matches_docks %>%
    filter(true_match_id_cls == true_match_id_mrip) %>%
    distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>% 
    nrow()
  
  p.agree_total_num_species_rel <- tidy_all_matches_docks %>%
    filter(true_match_id_cls == true_match_id_mrip) %>%
    distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>% 
    filter(diff_spec_rel %in% c(0)) %>%
    nrow() /
    n_total_num_species_rel
  
  #close
  p.close_total_num_species_rel <- tidy_all_matches_docks %>%
    filter(true_match_id_cls == true_match_id_mrip) %>%
    distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>%  #remove duplicates
    filter(diff_spec_rel %in% c(-1,1)) %>%
    nrow()/
    n_total_num_species_rel
  #far
  p.far_total_num_species_rel <- tidy_all_matches_docks %>%
    filter(true_match_id_cls == true_match_id_mrip) %>%
    distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>%  #remove duplicates
    filter(diff_spec_rel %in% c(-2,2)) %>%
    nrow() /
    n_total_num_species_rel
  #disagree
  p.disagree_total_num_spec_rel <- tidy_all_matches_docks %>%
    filter(true_match_id_cls == true_match_id_mrip) %>%
    distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>%  #remove duplicates
    filter(diff_spec_rel %in% c(-10:-3,3:10)) %>%
    nrow()/
    n_total_num_species_rel
  
  #####
  # Number of anglers
  #####
  n_num_anglers <- tidy_all_matches_docks %>%
    filter(true_match_id_cls == true_match_id_mrip) %>%
    distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T)  %>% 
    nrow()
  
  p.agree_num_anglers <- tidy_all_matches_docks %>%
    filter(true_match_id_cls == true_match_id_mrip) %>%
    distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>% 
    filter(diff_num_anglers %in% c(0)) %>% 
    nrow() / 
    n_num_anglers
  
  p.close_num_anglers <- tidy_all_matches_docks %>%
    filter(true_match_id_cls == true_match_id_mrip) %>%
    distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>% 
    filter(diff_num_anglers %in% c(-1,1)) %>% 
    nrow()/ 
    n_num_anglers
  
  
  p.disagree_num_anglers <- tidy_all_matches_docks %>%
    filter(true_match_id_cls == true_match_id_mrip) %>%
    distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>%
    filter(diff_num_anglers %in% c(-15:-2,2:15)) %>% 
    nrow()/ 
    n_num_anglers
  
  #####
  # Date
  #####
  
  n_total_date <-  tidy_all_matches_docks %>%
    filter(true_match_id_cls == true_match_id_mrip) %>%
    distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>% 
    nrow() 
  
  p.agree_date <-  tidy_all_matches_docks %>%
    filter(true_match_id_cls == true_match_id_mrip) %>%
    distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>%
    filter(between(diff_date,-0.15,0.15)) %>% 
    nrow() /
    n_total_date
  
  #close
  p.close_date <-  tidy_all_matches_docks %>%
    filter(true_match_id_cls == true_match_id_mrip) %>%
    distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T)%>%
    filter(between(diff_date,-1, -0.15001) |
             between(diff_date,0.15001,1)) %>%
    nrow() /
    n_total_date
  #far
  p.disagree_date <-  tidy_all_matches_docks %>%
    filter(true_match_id_cls == true_match_id_mrip) %>%
    distinct(true_match_id_mrip,true_match_id_cls,.keep_all = T) %>%
    filter(between(diff_date,-10,-1.0001) |
             between(diff_date, 1.0001, 10)) %>%
    nrow() /
    n_total_date
  
  
  ##############################
  # Record Linkage Algorithm
  ##############################
  
  
  #Total number of species reportedly caught
  score_species_caught <- c()
  for(s in 1:nrow(tidy_all_matches_docks)) {
    if (is.na(tidy_all_matches_docks$species_caught_cls[s]) |
        is.na(tidy_all_matches_docks$species_caught_mrip[s])) {
      score_species_caught[s] <- NA
    } else if (tidy_all_matches_docks$species_caught_cls[s] == #agree
               tidy_all_matches_docks$species_caught_mrip[s]) {
      score_species_caught[s] <-
        -log(
          sum(
            tidy_all_matches_docks$species_caught_mrip ==
              tidy_all_matches_docks$species_caught_cls[s]
          ) /
            nrow(tidy_all_matches_docks)
        )
    } else if (abs( #close
      tidy_all_matches_docks$species_caught_cls[s] -
      tidy_all_matches_docks$species_caught_mrip[s]
    ) %in% c(1)) {
      score_species_caught[s] <-
        -log(sum(
          abs(
            tidy_all_matches_docks$species_caught_cls[s] -
              tidy_all_matches_docks$species_caught_mrip
          ) %in% c(1)
        ) /
          nrow(tidy_all_matches_docks)) + log(p.close_total_num_species)
    } else if (abs( #far
      tidy_all_matches_docks$species_caught_cls[s] -
      tidy_all_matches_docks$species_caught_mrip[s]
    ) %in% c(2)) {
      score_species_caught[s] <-
        -log(sum(
          abs(
            tidy_all_matches_docks$species_caught_cls[s] -
              tidy_all_matches_docks$species_caught_mrip
          ) %in% c(2)
        ) /
          nrow(tidy_all_matches_docks)) + log(p.far_total_num_species)
    } else #disagree
      (score_species_caught[s] <- log(p.disagree_total_num_species))
  }
  
  #Total number of species reportedly released
  
  score_species_released <- c()
  for(s in 1:nrow(tidy_all_matches_docks)) {
    if (is.na(tidy_all_matches_docks$species_released_cls[s]) |
        is.na(tidy_all_matches_docks$species_released_mrip[s])) {
      score_species_released[s] <- NA
    } else if (tidy_all_matches_docks$species_released_cls[s] == #agree
               tidy_all_matches_docks$species_released_mrip[s]) {
      score_species_released[s] <-
        -log(
          sum(
            tidy_all_matches_docks$species_released_mrip ==
              tidy_all_matches_docks$species_released_cls[s]
          ) /
            nrow(tidy_all_matches_docks)
        )
    } else if (abs(
      tidy_all_matches_docks$species_released_cls[s] - #close
      tidy_all_matches_docks$species_released_mrip[s]
    ) %in% c(1)) {
      score_species_released[s] <-
        -log(sum(
          abs(
            tidy_all_matches_docks$species_released_cls[s] -
              tidy_all_matches_docks$species_released_mrip
          ) %in% c(1)
        ) /
          nrow(tidy_all_matches_docks)) + log(p.close_total_num_species_rel)
    } else if (abs(
      tidy_all_matches_docks$species_released_cls[s] - #far
      tidy_all_matches_docks$species_released_mrip[s]
    ) %in% c(2)) {
      score_species_released[s] <-
        -log(sum(
          abs(
            tidy_all_matches_docks$species_released_cls[s] -
              tidy_all_matches_docks$species_released_mrip
          ) %in% c(2)
        ) /
          nrow(tidy_all_matches_docks)) + log(p.far_total_num_species_rel)
    } else
      (score_species_released[s] <- log(p.disagree_total_num_spec_rel))
  }
  
  #Difference in Total Catch
  
  score_diff_total_catch <- c()
  for(s in 1:nrow(tidy_all_matches_docks)) {
    if (is.na(tidy_all_matches_docks$cls_harvest[s]) |
        is.na(tidy_all_matches_docks$total_harvest[s])) {
      score_diff_total_catch[s] <- NA
    } else if (between(tidy_all_matches_docks$diff_total_catch[s],-0.9999,0.9999)) {
      score_diff_total_catch[s] <-
        -log(
          sum(
            between(
              (tidy_all_matches_docks$total_harvest -
                 tidy_all_matches_docks$cls_harvest[s]),-0.9999,0.9999),
            na.rm = T
          ) /
            nrow(tidy_all_matches_docks)
        )
    } else if (between(tidy_all_matches_docks$diff_total_catch[s],-5.9999,-1) |
               between(tidy_all_matches_docks$diff_total_catch[s],1,5.9999)) {
      score_diff_total_catch[s] <-
        -log(
          sum(
            between(
              abs(tidy_all_matches_docks$cls_harvest[s] -
                    tidy_all_matches_docks$total_harvest),1,5.9999),
            na.rm = T
          ) /
            nrow(tidy_all_matches_docks)) + log(p.close_total_catch)
    } else if (between(tidy_all_matches_docks$diff_total_catch[s],-15.9999,-6) |
               between(tidy_all_matches_docks$diff_total_catch[s],6,15.9999)) {
      score_diff_total_catch[s] <-
        -log(
          sum(
            between(
              abs(tidy_all_matches_docks$cls_harvest[s] -
                    tidy_all_matches_docks$total_harvest),6,15.9999),
            na.rm = T
          ) /
            nrow(tidy_all_matches_docks)) + log(p.far_total_catch)
    }  else
      (score_diff_total_catch[s] <- log(p.disagree_total_catch))
  }
  
  #Difference in Total Release
  
  score_diff_total_release <- c()
  for(s in 1:nrow(tidy_all_matches_docks)) {
    if (is.na(tidy_all_matches_docks$cls_harvest[s]) |
        is.na(tidy_all_matches_docks$total_harvest[s])) {
      score_diff_total_release[s] <- NA
    } else if (between(tidy_all_matches_docks$diff_total_release[s],
                       -0.9999,0.9999)) {
      score_diff_total_release[s] <-
        -log(
          sum(
            between(
              (tidy_all_matches_docks$total_release -
                 tidy_all_matches_docks$cls_release[s]),-0.9999,0.9999),
            na.rm = T
          ) /
            nrow(tidy_all_matches_docks)
        )
    } else if (between(tidy_all_matches_docks$diff_total_release[s],
                       -4.9999,-1) |
               between(tidy_all_matches_docks$diff_total_release[s],
                       1,4.9999)) {
      score_diff_total_release[s] <-
        -log(
          sum(
            between(abs(tidy_all_matches_docks$cls_release[s] -
                          tidy_all_matches_docks$total_release),1,4.9999),
            na.rm = T
          ) /
            nrow(tidy_all_matches_docks)) + log(p.close_total_release)
    } else if (between(tidy_all_matches_docks$diff_total_release[s],
                       -10.9999,-5) |
               between(tidy_all_matches_docks$diff_total_release[s],
                       5,10.9999)){
      score_diff_total_release[s] <-
        -log(
          sum(
            between(abs(tidy_all_matches_docks$cls_release[s] -
                          tidy_all_matches_docks$total_release),5,10.9999),
            na.rm = T
          ) /
            nrow(tidy_all_matches_docks)) + log(p.far_total_release)
    } else
      (score_diff_total_release[s] <- log(p.disagree_total_release)) #disagree
  }
  
  #Difference in Red Snapper Caught
  
  score_diff_red_snapper_caught <- c()
  for(s in 1:nrow(tidy_all_matches_docks)) {
    if (is.na(tidy_all_matches_docks$cls_red_snapper_harvest[s]) |
        is.na(tidy_all_matches_docks$red_snapper_harvest[s])) {
      score_diff_red_snapper_caught[s] <- NA
    } #agree
    else if (between(
      (tidy_all_matches_docks$cls_red_snapper_harvest[s] - 
       tidy_all_matches_docks$red_snapper_harvest[s]),-0.1,0.1)) {
      score_diff_red_snapper_caught[s] <-
        -log(
          sum(
            between((tidy_all_matches_docks$red_snapper_harvest -
                       tidy_all_matches_docks$cls_red_snapper_harvest[s]),-0.1,0.1),
            na.rm = T
          ) /
            nrow(tidy_all_matches_docks)
        ) 
    } else
      (score_diff_red_snapper_caught[s] <- log(p.disagree_total_rs))
  }
  
  
  #Difference in Number of Anglers
  score_diff_anglers <- c()
  for(s in 1:nrow(tidy_all_matches_docks)) {
    if (is.na(tidy_all_matches_docks$party[s]) |
        is.na(tidy_all_matches_docks$cls_party[s])) {
      score_diff_anglers[s] <- NA
    } else if (tidy_all_matches_docks$cls_party[s] == #agree
               tidy_all_matches_docks$party[s]) {
      score_diff_anglers[s] <-
        -log(
          sum(
            tidy_all_matches_docks$party ==
              tidy_all_matches_docks$cls_party[s],na.rm = T
          ) /
            nrow(tidy_all_matches_docks)
        )
    } else if (abs(tidy_all_matches_docks$cls_party[s] - #close
                   tidy_all_matches_docks$party[s]) %in% c(1)) {
      score_diff_anglers[s] <-
        -log(sum(
          abs(
            tidy_all_matches_docks$cls_party[s] -
              tidy_all_matches_docks$party
          ) %in% c(1),na.rm = T
        ) /
          nrow(tidy_all_matches_docks)) + log(p.close_num_anglers)
    } else #disagree
      (score_diff_anglers[s] <- log(p.disagree_num_anglers))
  }
  
  #Difference in Reporting Date
  
  score_diff_date <- c()
  for(s in 1:nrow(tidy_all_matches_docks)) {
    if (is.na(tidy_all_matches_docks$date[s]) |
        is.na(tidy_all_matches_docks$date_cls[s])) {
      score_diff_date[s] <- NA
    }else if (between(tidy_all_matches_docks$diff_date[s],-0.15,0.15)) { #agree
      score_diff_date[s] <-
        -log(
          (sum(
            between(
              (difftime(tidy_all_matches_docks$date_cls[s],
                        tidy_all_matches_docks$date,units = "days")), 
              -0.15, 0.15),na.rm = T
          )
          ) /
            nrow(tidy_all_matches_docks))
    } else if (between(tidy_all_matches_docks$diff_date[s],-1,-0.15001) |
               between(tidy_all_matches_docks$diff_date[s],0.15001,1)) { #close
      score_diff_date[s] <-
        -log(
          (sum(
            between(
              (abs(difftime(tidy_all_matches_docks$date_cls[s],
                            tidy_all_matches_docks$date, units = "days"))), 
              0.15001, 1),na.rm = T
          )
          ) /
            nrow(tidy_all_matches_docks)) + log(p.close_date)
    } else #disagree
      (score_diff_date[s] <- log(p.disagree_date))
  }
  
  #Difference in Reporting Distance
  
  score_diff_report_distance <- c()
  for(s in 1:nrow(tidy_all_matches_docks)) {
    if(is.na(tidy_all_matches_docks$km_difference[s])){
      score_diff_report_distance[s] <- NA
    } #agree
    else if (between(tidy_all_matches_docks$km_difference[s],-100, 15)) {
      score_diff_report_distance[s] <-
        -log((sum(between((distHaversine(
          as.matrix(
            tibble(
              tidy_all_matches_docks$CLS_LONG[s],
              tidy_all_matches_docks$CLS_LAT[s]
            )
          ),
          as.matrix(
            tibble(tidy_all_matches_docks$SITE_LONG,
                   tidy_all_matches_docks$SITE_LAT)
          )
        ) / 1000),-100, 15
        ),na.rm = T)) / nrow(tidy_all_matches_docks))
    } else if (between(tidy_all_matches_docks$km_difference[s], 15.0001, 40)) {
      score_diff_report_distance[s] <- #close
        -log((sum(between((distHaversine(
          as.matrix(
            tibble(
              tidy_all_matches_docks$CLS_LONG[s],
              tidy_all_matches_docks$CLS_LAT[s]
            )
          ),
          as.matrix(
            tibble(tidy_all_matches_docks$SITE_LONG,
                   tidy_all_matches_docks$SITE_LAT)
          )
        ) / 1000), 15.0001, 40
        ),na.rm = T)) / nrow(tidy_all_matches_docks)) + log(p.close_total_distance)
    } else #disagree
      (score_diff_report_distance[s] <- log(p.disagree_total_distance))
  }
  
  #Add blocking variables as linking variables
  #Difference in CLS_ID
  score_cls_id <- c()
  for(s in 1:nrow(tidy_all_matches_docks)) {
    score_cls_id[s] <-
      -log(
        sum(
          tidy_all_matches_docks$CLS_ID ==
            tidy_all_matches_docks$CLS_ID[s],na.rm = T
        ) /
          nrow(tidy_all_matches_docks)
      )
  }
  
  tidy_all_matches_docks_rl <- tidy_all_matches_docks %>%
    mutate(
      score_diff_anglers = score_diff_anglers,
      score_diff_date = score_diff_date,
      score_diff_red_snapper_caught = score_diff_red_snapper_caught,
      score_diff_report_distance = score_diff_report_distance,
      score_diff_total_catch = score_diff_total_catch,
      score_diff_total_release = score_diff_total_release,
      score_species_caught = score_species_caught,
      score_species_released = score_species_released,
      score_cls_id = score_cls_id
    ) %>%
    mutate(
      rl_score = rowSums(
        select(.,
               score_diff_anglers,
               score_diff_date,
               score_diff_red_snapper_caught,
               score_diff_report_distance,
               score_diff_total_catch,
               score_diff_total_release,
               score_species_caught,
               score_species_released,
               score_cls_id
        ),na.rm = T))
  
  
  ######################
  # Matching
  ######################
  
  #need to filter it so that only one cls report 
  #is matched with an mrip trip
  
  #####
  # First Round
  #####
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
  
  mrip_dates_one_per_first <- one_per_first %>% 
    pull(id)
  mrip_tripids_one_per_first <- one_per_first %>% 
    pull(trip_id)
  
  ####
  # Second Round
  ####
  
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
  
  sec_d <- matched_second %>%
    pull(id)
  first_trips <- one_per_first %>% pull(trip_id)
  sec_trips <- matched_second %>% pull(trip_id)
  
  ######
  # Need a third round
  ######
  
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
  
  ##########
  # Determine Links using cutoff that 
  # Gets closest to the true number of
  # matches in sample
  tidy_all_matches_one_per_1 <- matched_second %>% 
    bind_rows(one_per_first) %>% 
    bind_rows(matched_third) %>% 
    distinct()
  
  tidy_all_matches_one_per <- tidy_all_matches_one_per_1 %>% 
    group_by(id) %>% 
    sample_n(1) %>% 
    ungroup()
  
  true_match_s <- nrow(filter(sample_pop,true_match_id_mrip != 0))
  cut <- c(5:25)
  cut_tib <- tibble()
  links <- c()
  for(i in 1:length(cut)){
    links[i] <- nrow(filter(tidy_all_matches_one_per,rl_score > cut[i]))
  }
  
  m <- abs(links - true_match_s)
  cutoff.w <- which(m == min(m))
  cutoff <- cut[cutoff.w]
  ######## 
  # Put CLS info onto sample for linked trips
  ########
  matches <- tidy_all_matches_one_per %>% 
    filter(rl_score > cutoff) %>% 
    mutate(match = 1)
  
  match_ids <- pull(matches,id)
  
  non_matches <- sample_pop %>% 
    filter(!id %in% match_ids)
  
  matched_samp <- non_matches %>% 
    bind_rows(matches)
  
  #Prep for estimation
  matched_samp <- matched_samp %>% 
    mutate(
      cls_red_snapper_harvest = if_else(is.na(cls_red_snapper_harvest),
                                        0,cls_red_snapper_harvest),
      delta_c = red_snapper_harvest -
        cls_red_snapper_harvest,
      match = if_else(is.na(match),
                      0,match),
      non_reporter_mrip_claim = if_else(match == 0,
                                        red_snapper_harvest,0))
  
  
  ########
  # Make Estimates
  ########
  
  s_design <- svydesign(id =~psu_id,
                        strat = ~strat_int,
                        prob = ~prob,
                        nest = T,
                        data = matched_samp)
  options(survey.lonely.psu = "adjust") 
  
  tyc <- svyratio(~red_snapper_harvest,
                  ~cls_red_snapper_harvest,
                  design=s_design)
  
  tyc_se <- as.numeric(
    predict(tyc,
            total = sum(self_reports$cls_red_snapper_harvest))$se)
  
  tyc_total <- as.numeric(
    predict(tyc,
            total = sum(self_reports$cls_red_snapper_harvest))$total)
  
  ty2 <- svyratio(~delta_c,
                  ~match,
                  design = s_design,
                  na.rm = T)
  ty2_se <- as.numeric(
    predict(ty2,total = n1)$se)
  
  ty2_total <- predict(ty2,total = n1)[[1]] + 
    sum(self_reports$cls_red_snapper_harvest,na.rm=T)
  ty2_total <- as.numeric(ty2_total)
  
  
  typ <- svyratio(~red_snapper_harvest,
                  ~match,
                  design = s_design,
                  na.rm = T)
  typ_se <- as.numeric(
    predict(typ,total = n1,na.rm=T)$se)
  
  typ_total <- as.numeric(
    predict(typ,total = n1,na.rm=T)$total)
  
  
  tydiff <- svytotal(~delta_c,
                     design = s_design,na.rm=T)
  tydiff_se <- as.numeric(SE(tydiff))
  
  tydiff_total <- tydiff[[1]] + sum(self_reports$cls_red_snapper_harvest,
                                    na.rm=T)
  
  tynew.nr <- svytotal(~non_reporter_mrip_claim,
                       design = s_design,
                       na.rm=T)
  tynew_se <- as.numeric(SE(tynew.nr))
  
  tynew_total <- tynew.nr[[1]] + 
    sum(self_reports$cls_red_snapper_harvest,na.rm=T)
  
  n1hat <- svytotal(~match,
                    s_design)
  n1hat_total <- n1hat[[1]]
  n1hat_se <- SE(n1hat)
  
  ########
  # Get FP & FN rates
  ########
  ex_m <- tidy_all_matches_one_per %>% 
    mutate(match = if_else(rl_score > cutoff,
                           1,0)) %>% 
    mutate(
      match_status = case_when( 
        match == 1 & true_match_id_mrip == 0 ~ "false-pos",
        match == 0 & true_match_id_mrip != 0 &
          true_match_id_cls == true_match_id_mrip ~ "false-neg",
        match == 0 & true_match_id_mrip != 0 &
          true_match_id_cls != true_match_id_mrip ~ "false-neg",
        match == 1 & true_match_id_mrip != 0 &
          true_match_id_cls != true_match_id_mrip ~ "mismatch",
        match == 0 & true_match_id_mrip == 0 ~ "true-neg",
        match == 1 & true_match_id_mrip != 0 &
          true_match_id_cls == true_match_id_mrip ~ "true-pos"
      )
    ) 
  
  rates <- ex_m %>% 
    summarize(fp.rate = sum(match_status == "false-pos" &
                              match == 1) / 
                sum(match == 1),
              fn.rate = sum(match_status == "false-neg" & 
                              match == 0) / 
                sum(match == 0),
              tp.rate = sum(match_status == "true-pos" &
                              match == 1) / 
                sum(match == 1),
              mm.rate = sum(match_status == "mismatch" &
                              match == 1) / 
                sum(match == 1)
    ) 
  
  fn.rate <- rates$fn.rate
  fp.rate <- rates$fp.rate
  tp.rate <- rates$tp.rate
  mm.rate <- rates$mm.rate
  
  #### Updates
  t_yp_est[jj] <- typ_total
  t_yc_est[jj] <- tyc_total
  t_y2_est[jj] <- ty2_total
  t_ydiff_est[jj] <- tydiff_total
  t_ynew_est[jj] <- tynew_total
  t_yp_se[jj] <- typ_se
  t_yc_se[jj] <- tyc_se
  t_y2_se[jj] <- ty2_se
  t_ydiff_se[jj] <- tydiff_se
  t_ynew_se[jj] <- tynew_se
  n1_hat_total[jj] <- n1hat_total
  n1_hat_se[jj] <- n1hat_se
  fp_rate[jj] <- fp.rate
  fn_rate[jj] <- fn.rate
  tp_rate[jj] <- tp.rate
  mm_rate[jj] <- mm.rate
  ######
}

sim_results <- tibble(
  t_yp_est = t_yp_est,
  t_yc_est = t_yc_est,
  t_y2_est = t_y2_est,
  t_ydiff_est = t_ydiff_est,
  t_ynew_est = t_ynew_est,
  t_yp_se = t_yp_se,
  t_yc_se = t_yc_se,
  t_y2_se = t_y2_se,
  t_ydiff_se = t_ydiff_se,
  t_ynew_se = t_ynew_se,
  n1_hat_total = n1_hat_total,
  n1_hat_se = n1_hat_se,
  fp_rate = fp_rate,
  fn_rate = fn_rate,
  tp_rate = tp_rate,
  mm_rate = mm_rate
)

#write.csv(sim_results,"sim_results.csv")