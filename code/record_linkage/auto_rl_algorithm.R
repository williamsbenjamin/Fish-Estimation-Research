library(tidyverse)
library(lubridate)
library(geosphere)

# Data : All Possible Matches
# The data already has variables for the difference between
# the variables in cls and in mrip

tidy_all_matches_docks <- read_csv("data/all_possible_matches_16_17.csv",
                                   col_types = cols(estimated_returndate = col_datetime(),
                                                    psu_id = col_character()))
source("code/record_linkage/auto_linking_var_dists.R")

#########
#Ok now everything is clean
#########
###Use all possible matches dataset

#Maybe just do this for each linking variable, and have a different 
#score vector for each variable, then add those as columns to the dataset 
# then add them all up for each row to get the score


#Total number of species reportedly caught
score_species_caught <- c()
for(s in 1:nrow(tidy_all_matches_docks)) {
  if (tidy_all_matches_docks$reported_species_kept_cls[s] == #agree
      tidy_all_matches_docks$reported_species_claim_mrip[s]) {
    score_species_caught[s] <-
      -log(
        sum(
          tidy_all_matches_docks$reported_species_claim_mrip ==
            tidy_all_matches_docks$reported_species_kept_cls[s]
        ) /
          nrow(tidy_all_matches_docks)
      )
  } else if (abs( #close
    tidy_all_matches_docks$reported_species_kept_cls[s] -
    tidy_all_matches_docks$reported_species_claim_mrip[s]
  ) %in% c(1)) {
    score_species_caught[s] <-
      -log(sum(
        abs(
          tidy_all_matches_docks$reported_species_kept_cls[s] -
            tidy_all_matches_docks$reported_species_claim_mrip
        ) %in% c(1)
      ) /
        nrow(tidy_all_matches_docks)) + log(p.close_total_num_species)
  } else if (abs( #far
    tidy_all_matches_docks$reported_species_kept_cls[s] -
    tidy_all_matches_docks$reported_species_claim_mrip[s]
  ) %in% c(2)) {
    score_species_caught[s] <-
      -log(sum(
        abs(
          tidy_all_matches_docks$reported_species_kept_cls[s] -
            tidy_all_matches_docks$reported_species_claim_mrip
        ) %in% c(2)
      ) /
        nrow(tidy_all_matches_docks)) + log(p.far_total_num_species)
  } else #disagree
    (score_species_caught[s] <- log(p.disagree_total_num_species))
}

#Total number of species reportedly released

score_species_released <- c()
for(s in 1:nrow(tidy_all_matches_docks)) {
  if (tidy_all_matches_docks$reported_species_released_cls[s] == #agree
      tidy_all_matches_docks$reported_species_release_mrip[s]) {
    score_species_released[s] <-
      -log(
        sum(
          tidy_all_matches_docks$reported_species_release_mrip ==
            tidy_all_matches_docks$reported_species_released_cls[s]
        ) /
          nrow(tidy_all_matches_docks)
      )
  } else if (abs(
    tidy_all_matches_docks$reported_species_released_cls[s] - #close
    tidy_all_matches_docks$reported_species_release_mrip[s]
  ) %in% c(1)) {
    score_species_released[s] <-
      -log(sum(
        abs(
          tidy_all_matches_docks$reported_species_released_cls[s] -
            tidy_all_matches_docks$reported_species_release_mrip
        ) %in% c(1)
      ) /
        nrow(tidy_all_matches_docks)) + log(p.close_total_num_species_rel)
  } else if (abs(
    tidy_all_matches_docks$reported_species_released_cls[s] - #far
    tidy_all_matches_docks$reported_species_release_mrip[s]
  ) %in% c(2)) {
    score_species_released[s] <-
      -log(sum(
        abs(
          tidy_all_matches_docks$reported_species_released_cls[s] -
            tidy_all_matches_docks$reported_species_release_mrip
        ) %in% c(2)
      ) /
        nrow(tidy_all_matches_docks)) + log(p.far_total_num_species_rel)
  } else
    (score_species_released[s] <- log(p.disagree_total_num_spec_rel))
}

#Difference in Total Catch

score_diff_total_catch <- c()
for(s in 1:nrow(tidy_all_matches_docks)) {
  if (between(tidy_all_matches_docks$diff_total_catch[s],-0.9999,0.9999)) {
    score_diff_total_catch[s] <-
      -log(
        sum(
          between(
            (tidy_all_matches_docks$total_claim_mrip -
             tidy_all_matches_docks$total_kept_cls[s]),-0.9999,0.9999)
        ) /
          nrow(tidy_all_matches_docks)
      )
  } else if (between(tidy_all_matches_docks$diff_total_catch[s],-5.9999,-1) |
             between(tidy_all_matches_docks$diff_total_catch[s],1,5.9999)) {
    score_diff_total_catch[s] <-
      -log(
        sum(
         between(
          abs(tidy_all_matches_docks$total_kept_cls[s] -
            tidy_all_matches_docks$total_claim_mrip),1,5.9999)
      ) /
        nrow(tidy_all_matches_docks)) + log(p.close_total_catch)
  } else if (between(tidy_all_matches_docks$diff_total_catch[s],-15.9999,-6) |
             between(tidy_all_matches_docks$diff_total_catch[s],6,15.9999)) {
    score_diff_total_catch[s] <-
      -log(
        sum(
         between(
          abs(tidy_all_matches_docks$total_kept_cls[s] -
            tidy_all_matches_docks$total_claim_mrip),6,15.9999)
      ) /
        nrow(tidy_all_matches_docks)) + log(p.far_total_catch)
  } else if (between(tidy_all_matches_docks$diff_total_catch[s],-25.9999,-16) |
             between(tidy_all_matches_docks$diff_total_catch[s],16,25.9999)) {
    score_diff_total_catch[s] <-
      -log(
        sum(
        between(
          abs(tidy_all_matches_docks$total_kept_cls[s] -
            tidy_all_matches_docks$total_claim_mrip),16,25.9999)
      ) /
        nrow(tidy_all_matches_docks)) + log(p.farther_total_catch)
  } else
    (score_diff_total_catch[s] <- log(p.disagree_total_catch))
}

#Difference in Total Release

score_diff_total_release <- c()
for(s in 1:nrow(tidy_all_matches_docks)) {
  if (between(tidy_all_matches_docks$diff_total_release[s],-0.9999,0.9999)) {
    score_diff_total_release[s] <-
      -log(
        sum(
          between(
            (tidy_all_matches_docks$total_release_mrip -
            tidy_all_matches_docks$total_released_cls[s]),-0.9999,0.9999)
        ) /
          nrow(tidy_all_matches_docks)
      )
  } else if (between(tidy_all_matches_docks$diff_total_release[s],-4.9999,-1) |
                    between(tidy_all_matches_docks$diff_total_release[s],1,4.9999)) {
    score_diff_total_release[s] <-
      -log(
        sum(
        between(abs(tidy_all_matches_docks$total_released_cls[s] -
            tidy_all_matches_docks$total_release_mrip),1,4.9999)
      ) /
        nrow(tidy_all_matches_docks)) + log(p.close_total_release)
  } else if (between(tidy_all_matches_docks$diff_total_release[s],-10.9999,-5) |
              between(tidy_all_matches_docks$diff_total_release[s],5,10.9999)){
    score_diff_total_release[s] <-
      -log(
        sum(
        between(abs(tidy_all_matches_docks$total_released_cls[s] -
                    tidy_all_matches_docks$total_release_mrip),5,10.9999)
      ) /
        nrow(tidy_all_matches_docks)) + log(p.far_total_release)
  } else
    (score_diff_total_release[s] <- log(p.disagree_total_release)) #disagree
}

#Difference in Red Snapper Caught

# score_diff_red_snapper_caught <- c()
# for(s in 1:nrow(tidy_all_matches_docks)) {
#   if (is.na(tidy_all_matches_docks$`RED SNAPPER_kept`[s]) |
#       is.na(tidy_all_matches_docks$`RED SNAPPER_claim`[s])) {
#     score_diff_red_snapper_caught[s] <- NA
#   } #agree
#   else if (between(
#             (tidy_all_matches_docks$`RED SNAPPER_kept`[s] - 
#              tidy_all_matches_docks$`RED SNAPPER_claim`[s]),-0.1,0.1)) {
#     score_diff_red_snapper_caught[s] <-
#       -log(
#         sum(
#           between((tidy_all_matches_docks$`RED SNAPPER_claim` -
#             tidy_all_matches_docks$`RED SNAPPER_kept`[s]),-0.1,0.1),na.rm = T
#         ) /
#           nrow(tidy_all_matches_docks)
#       ) 
#   } else if (between(
#               (tidy_all_matches_docks$`RED SNAPPER_kept`[s] - 
#                tidy_all_matches_docks$`RED SNAPPER_claim`[s]),-4,-0.1001) |
#               between((tidy_all_matches_docks$`RED SNAPPER_kept`[s] - 
#                        tidy_all_matches_docks$`RED SNAPPER_claim`[s]),0.1001,4)) { #close
#     score_diff_red_snapper_caught[s] <-
#       -log(
#         sum(
#         between(abs(tidy_all_matches_docks$`RED SNAPPER_kept`[s] -
#                     tidy_all_matches_docks$`RED SNAPPER_claim`),0.1001,4),na.rm = T
#       ) /
#         nrow(tidy_all_matches_docks)) + log(p.close_total_rs)
#   } else if (between(
#                 (tidy_all_matches_docks$`RED SNAPPER_kept`[s] - 
#                  tidy_all_matches_docks$`RED SNAPPER_claim`[s]),-10,-4.001) |
#              between((tidy_all_matches_docks$`RED SNAPPER_kept`[s] - 
#                       tidy_all_matches_docks$`RED SNAPPER_claim`[s]),4.001,10)) { #far
#                 score_diff_red_snapper_caught[s] <-
#       -log(
#         sum(
#         between(abs(tidy_all_matches_docks$`RED SNAPPER_kept`[s] -
#                     tidy_all_matches_docks$`RED SNAPPER_claim`),4.001,10),na.rm=T
#       ) /
#         nrow(tidy_all_matches_docks)) + log(p.far_total_rs)
#   } else
#     (score_diff_red_snapper_caught[s] <- log(p.disagree_total_rs))
# }


#Difference in Number of Anglers
score_diff_anglers <- c()
for(s in 1:nrow(tidy_all_matches_docks)) {
  if (is.na(tidy_all_matches_docks$PARTY[s])) {
    score_diff_anglers[s] <- NA
  } else if (tidy_all_matches_docks$nbAnglers[s] == #agree
      tidy_all_matches_docks$PARTY[s]) {
    score_diff_anglers[s] <-
      -log(
        sum(
          tidy_all_matches_docks$PARTY ==
            tidy_all_matches_docks$nbAnglers[s]
        ) /
          nrow(tidy_all_matches_docks)
      )
  } else if (abs(tidy_all_matches_docks$nbAnglers[s] - #close
                 tidy_all_matches_docks$PARTY[s]) %in% c(1)) {
    score_diff_anglers[s] <-
      -log(sum(
        abs(
          tidy_all_matches_docks$nbAnglers[s] -
            tidy_all_matches_docks$PARTY
        ) %in% c(1)
      ) /
        nrow(tidy_all_matches_docks)) + log(p.close_num_anglers)
  } else #disagree
    (score_diff_anglers[s] <- log(p.disagree_num_anglers))
}

#Difference in Reporting Date

score_diff_date <- c()
for(s in 1:nrow(tidy_all_matches_docks)) {
  if (between(tidy_all_matches_docks$date_diff[s],-0.15,0.15)) { #agree
    score_diff_date[s] <-
      -log(
        (sum(
          between(
          (difftime(tidy_all_matches_docks$returndate[s],
            tidy_all_matches_docks$date_time_mrip,units = "days")), 
          -0.15, 0.15)
          )
        ) /
          nrow(tidy_all_matches_docks))
  } else if (between(tidy_all_matches_docks$date_diff[s],-1.5,-0.15001) |
             between(tidy_all_matches_docks$date_diff[s],0.15001,1.5)) { #close
    score_diff_date[s] <-
      -log(
        (sum(
          between(
            (abs(difftime(tidy_all_matches_docks$returndate[s],
               tidy_all_matches_docks$date_time_mrip, units = "days"))), 
            0.15001, 1.5)
          )
        ) /
        nrow(tidy_all_matches_docks)) + log(p.close_date)
  } else if (between(tidy_all_matches_docks$date_diff[s],-5,-1.5001) |
             between(tidy_all_matches_docks$date_diff[s], 1.5001, 5)) { #far
    score_diff_date[s] <-
      -log(
        (sum(
          between(
            (abs(difftime(tidy_all_matches_docks$returndate[s],
                tidy_all_matches_docks$date_time_mrip, units = "days"))), 
             1.5001, 5)
          )
        ) /
        nrow(tidy_all_matches_docks)) + log(p.far_date)
  } else if (between(tidy_all_matches_docks$date_diff[s],-15, -5.001) |
             between(tidy_all_matches_docks$date_diff[s], 5.001, 15)) { #further
    score_diff_date[s] <-
      -log(
        (sum(
          between(
            (abs(difftime(tidy_all_matches_docks$returndate[s],
                   tidy_all_matches_docks$date_time_mrip,units = "days"))), 
             5.001, 15)
        )
        ) /
          nrow(tidy_all_matches_docks)) + log(p.further_date)
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
      ))) / nrow(tidy_all_matches_docks))
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
      ))) / nrow(tidy_all_matches_docks)) + log(p.close_total_distance)
  } else if (between(tidy_all_matches_docks$km_difference[s], 40.0001, 75)) {
    score_diff_report_distance[s] <- #far
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
      ) / 1000), 40.0001, 75
      ))) / nrow(tidy_all_matches_docks)) + log(p.far_total_distance)
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
            tidy_all_matches_docks$CLS_ID[s]
        ) /
          nrow(tidy_all_matches_docks)
      )
}


tidy_all_matches_docks_rl <- tidy_all_matches_docks %>%
  mutate(
    score_diff_anglers = score_diff_anglers,
    score_diff_date = score_diff_date,
    #score_diff_red_snapper_caught = score_diff_red_snapper_caught,
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
        #score_diff_red_snapper_caught,
        score_diff_report_distance,
        score_diff_total_catch,
        score_diff_total_release,
        score_species_caught,
        score_species_released,
        score_cls_id
      ),na.rm = T))

#linking variable agreement patterns
tidy_all_matches_docks_rl <- tidy_all_matches_docks_rl %>%
  mutate(spec_caught_diff = reported_species_kept_cls - 
           reported_species_claim_mrip) %>% 
  mutate(spec_rel_diff = reported_species_released_cls - 
           reported_species_release_mrip) %>%
  mutate(tot_catch_pattern = case_when(
    between(diff_total_catch,-0.9999,0.9999) ~ "agree",
    between(diff_total_catch,-5.9999,-1) |
      between(diff_total_catch,1,5.9999) ~ "close",
    between(diff_total_catch,-15.9999,-6) |
      between(diff_total_catch,6,15.9999) ~ "far",
    between(diff_total_catch,-25.9999,-16) |
      between(diff_total_catch,16,25.9999) ~ "farther",
    between(diff_total_catch,-3000,-26) |
      between(diff_total_catch,26,9006) ~ "disagree")) %>%
  mutate(tot_rel_pattern = case_when(
    between(diff_total_release,-0.9999,0.9999) ~ "agree",
    between(diff_total_release,-4.9999,-1) | 
      between(diff_total_release,1,4.9999) ~ "close",
    between(diff_total_release,-10.9999,-5) |
      between(diff_total_release,5,10.9999) ~ "far",
    between(diff_total_release,-90000,-11) |
      between(diff_total_release,11,90000) ~ "disagree")) %>%
  mutate(km_diff_pattern = case_when(
    between(km_difference,0,15) ~ "agree",
    between(km_difference,15.0001,40) ~ "close",
    between(km_difference,40.0001,75) ~ "far",
    between(km_difference,75.0001,4000) ~ "disagree"))%>%
  mutate(spec_caught_pattern = case_when(
    (spec_caught_diff %in% c(0)) ~ "agree",
    (spec_caught_diff %in% c(-1,1)) ~ "close",
    (spec_caught_diff %in% c(-2,2)) ~ "far",
    (spec_caught_diff %in% c(-20:-3,3:20)) ~ "disagree")) %>%
  mutate(spec_rel_pattern = case_when(
    (spec_rel_diff %in% c(0)) ~ "agree",
    (spec_rel_diff %in% c(-1,1)) ~ "close",
    (spec_rel_diff %in% c(-2,2)) ~ "far",
    (spec_rel_diff %in% c(-10:-3,3:10)) ~ "disagree")) %>%
  mutate(angler_pattern = case_when(
    (diff_cls_mrip_anglers %in% c(0)) ~ "agree",
    (diff_cls_mrip_anglers %in% c(-1,1)) ~ "close",
    (diff_cls_mrip_anglers %in% c(-55:-2,2:55)) ~ "disagree")) %>%
  mutate(date_pattern = case_when(
    between(date_diff,-0.15,0.15) ~ "agree",
    between(date_diff,-1.5, -0.15001) |
      between(date_diff,0.15001,1.5) ~ "close",
    between(date_diff,-5,-1.5001) |
      between(date_diff, 1.5001, 5) ~ "far",
    between(date_diff,-15, -5.001) |
      between(date_diff, 5.001, 15) ~ "farther",
    between(date_diff,-60000, -15.001) |
      between(date_diff, 15.001, 60000) ~ "disagree"))

write_csv(tidy_all_matches_docks_rl,"data/tidy_all_matches_docks_rl.csv")
