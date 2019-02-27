library(tidyverse)
library(lubridate)
library(geosphere)

# Data : All Possible Matches
# The data already has variables for the difference between
# the variables in cls and in mrip

source("code/simulations/auto_linking_var_dists_sim.R")



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
#write_csv(tidy_all_matches_docks_rl,"data/tidy_all_matches_docks_rl.csv")
