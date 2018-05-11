library(tidyverse)
library(lubridate)
library(geosphere)

# Data : All Possible Matches
# The data already has variables for the difference between
# the variables in cls and in mrip

tidy_all_matches_docks <- read_csv("data/all_possible_matches_16_17.csv")

source("code/auto_linking_var_dists.R")

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
  if (tidy_all_matches_docks$reported_species_kept_cls[s] == #exact
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
  ) %in% c(1, 2)) {
    score_species_caught[s] <-
      -log(sum(
        abs(
          tidy_all_matches_docks$reported_species_kept_cls[s] -
            tidy_all_matches_docks$reported_species_claim_mrip
        ) %in% c(1, 2)
      ) /
        nrow(tidy_all_matches_docks)) + log(p.close_total_num_species)
  } else #disagree
    (score_species_caught[s] <- log(p.disagree_total_num_species))
}

#Total number of species reportedly released

score_species_released <- c()
for(s in 1:nrow(tidy_all_matches_docks)) {
  if (tidy_all_matches_docks$reported_species_released_cls[s] ==
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
    tidy_all_matches_docks$reported_species_released_cls[s] -
    tidy_all_matches_docks$reported_species_release_mrip[s]
  ) %in% c(1, 2)) {
    score_species_released[s] <-
      -log(sum(
        abs(
          tidy_all_matches_docks$reported_species_released_cls[s] -
            tidy_all_matches_docks$reported_species_release_mrip
        ) %in% c(1, 2)
      ) /
        nrow(tidy_all_matches_docks)) + log(p.close_total_num_species_rel)
  } else
    (score_species_released[s] <- log(p.disagree_total_num_spec_rel))
}

#Difference in Total Catch

score_diff_total_catch <- c()
for(s in 1:nrow(tidy_all_matches_docks)) {
  if (tidy_all_matches_docks$total_kept_cls[s] ==
      tidy_all_matches_docks$total_claim_mrip[s]) {
    score_diff_total_catch[s] <-
      -log(
        sum(
          tidy_all_matches_docks$total_claim_mrip ==
            tidy_all_matches_docks$total_kept_cls[s]
        ) /
          nrow(tidy_all_matches_docks)
      )
  } else if (abs(
    tidy_all_matches_docks$total_kept_cls[s] -
    tidy_all_matches_docks$total_claim_mrip[s]
  ) %in% c(1:4)) {
    score_diff_total_catch[s] <-
      -log(sum(
        abs(
          tidy_all_matches_docks$total_kept_cls[s] -
            tidy_all_matches_docks$total_claim_mrip
        ) %in% c(1:4)
      ) /
        nrow(tidy_all_matches_docks)) + log(p.close_total_catch)
  } else
    (score_diff_total_catch[s] <- log(p.disagree_total_catch))
}

#Difference in Total Release

score_diff_total_release <- c()
for(s in 1:nrow(tidy_all_matches_docks)) {
  if (tidy_all_matches_docks$total_released_cls[s] ==
      tidy_all_matches_docks$total_release_mrip[s]) {
    score_diff_total_release[s] <-
      -log(
        sum(
          tidy_all_matches_docks$total_release_mrip ==
            tidy_all_matches_docks$total_released_cls[s]
        ) /
          nrow(tidy_all_matches_docks)
      )
  } else if (abs(
    tidy_all_matches_docks$total_released_cls[s] -
    tidy_all_matches_docks$total_release_mrip[s]
  ) %in% c(1:4)) {
    score_diff_total_release[s] <-
      -log(sum(
        abs(
          tidy_all_matches_docks$total_released_cls[s] -
            tidy_all_matches_docks$total_release_mrip
        ) %in% c(1:4)
      ) /
        nrow(tidy_all_matches_docks)) + log(p.close_total_release)
  } else
    (score_diff_total_release[s] <- log(p.disagree_total_release))
}

#Difference in Red Snapper Caught

score_diff_red_snapper_caught <- c()
for(s in 1:nrow(tidy_all_matches_docks)) {
  if (is.na(tidy_all_matches_docks$`RED SNAPPER_kept`[s])) {
    score_diff_red_snapper_caught[s] <- NA
  }
  else if (tidy_all_matches_docks$`RED SNAPPER_kept`[s] ==
           tidy_all_matches_docks$`RED SNAPPER_claim`[s]) {
    score_diff_red_snapper_caught[s] <-
      -log(
        sum(
          tidy_all_matches_docks$`RED SNAPPER_claim` ==
            tidy_all_matches_docks$`RED SNAPPER_kept`[s]
        ) /
          nrow(tidy_all_matches_docks)
      )
  } else if (abs(
    tidy_all_matches_docks$`RED SNAPPER_kept`[s] -
    tidy_all_matches_docks$`RED SNAPPER_claim`[s]
  ) %in% c(1:5)) {
    score_diff_red_snapper_caught[s] <-
      -log(sum(
        abs(
          tidy_all_matches_docks$`RED SNAPPER_kept`[s] -
            tidy_all_matches_docks$`RED SNAPPER_claim`
        ) %in% c(1:5)
      ) /
        nrow(tidy_all_matches_docks)) + log(p.close_total_rs)
  } else
    (score_diff_red_snapper_caught[s] <- log(p.disagree_total_rs))
}


#Difference in Number of Anglers
score_diff_anglers <- c()
for(s in 1:nrow(tidy_all_matches_docks)) {
  if (tidy_all_matches_docks$nbAnglers[s] ==
      tidy_all_matches_docks$PARTY[s]) {
    score_diff_anglers[s] <-
      -log(
        sum(
          tidy_all_matches_docks$PARTY ==
            tidy_all_matches_docks$nbAnglers[s]
        ) /
          nrow(tidy_all_matches_docks)
      )
  } else if (abs(tidy_all_matches_docks$nbAnglers[s] -
                 tidy_all_matches_docks$PARTY[s]) %in% c(1)) {
    score_diff_anglers[s] <-
      -log(sum(
        abs(
          tidy_all_matches_docks$nbAnglers[s] -
            tidy_all_matches_docks$PARTY
        ) %in% c(1)
      ) /
        nrow(tidy_all_matches_docks)) + log(p.close_num_anglers)
  } else
    (score_diff_anglers[s] <- log(p.disagree_num_anglers))
}

#Difference in Reporting Date

score_diff_date <- c()
for(s in 1:nrow(tidy_all_matches_docks)) {
  if (abs(
    as.numeric(
      tidy_all_matches_docks$return_date_ymd[s] -
      tidy_all_matches_docks$date[s]
    )
  ) == 0) {
    score_diff_date[s] <-
      -log(
        sum(
          tidy_all_matches_docks$date ==
            tidy_all_matches_docks$return_date_ymd[s]
        ) /
          nrow(tidy_all_matches_docks)
      )
  } else if (as.numeric(
      tidy_all_matches_docks$return_date_ymd[s] -
      tidy_all_matches_docks$date[s]
    ) %in% c(-2,-1,1,2)) {
    score_diff_date[s] <-
      -log(sum(abs(
        as.numeric(
          tidy_all_matches_docks$return_date_ymd[s] -
            tidy_all_matches_docks$date
        )
      ) %in% c(-2,-1,1,2)) /
        nrow(tidy_all_matches_docks)) + log(p.close_date)
  } else
    (score_diff_date[s] <- log(p.disagree_date))
}

#Difference in Reporting Distance

score_diff_report_distance <- c()
for(s in 1:nrow(tidy_all_matches_docks)) {
  if(is.na(tidy_all_matches_docks$km_difference[s])){
    score_diff_report_distance[s] <- NA
  }
  else if (between(tidy_all_matches_docks$km_difference[s],-100, 25)) {
    score_diff_report_distance[s] <-
      -log((sum(between((distHaversine(
        as.matrix(
          tibble(
            tidy_all_matches_docks$longitude[s],
            tidy_all_matches_docks$latitude[s]
          )
        ),
        as.matrix(
          tibble(tidy_all_matches_docks$SITE_LONG,
                 tidy_all_matches_docks$SITE_LAT)
        )
      ) / 1000),-100, 25
      ))) / nrow(tidy_all_matches_docks))
  } else if (between(tidy_all_matches_docks$km_difference[s], 25.01, 70)) {
    score_diff_report_distance[s] <-
      -log((sum(between((distHaversine(
        as.matrix(
          tibble(
            tidy_all_matches_docks$longitude[s],
            tidy_all_matches_docks$latitude[s]
          )
        ),
        as.matrix(
          tibble(tidy_all_matches_docks$SITE_LONG,
                 tidy_all_matches_docks$SITE_LAT)
        )
      ) / 1000), 25.01, 70
      ))) / nrow(tidy_all_matches_docks)) + log(p.close_total_distance)
  } else
    (score_diff_report_distance[s] <- log(p.disagree_total_distance))
}


##Add scores onto tidy_all_matches_docks

tidy_all_matches_docks_rl <- tidy_all_matches_docks %>%
  mutate(
    score_diff_anglers = score_diff_anglers,
    score_diff_date = score_diff_date,
    score_diff_red_snapper_caught = score_diff_red_snapper_caught,
    score_diff_report_distance = score_diff_report_distance,
    score_diff_total_catch = score_diff_total_catch,
    score_diff_total_release = score_diff_total_release,
    score_species_caught = score_species_caught,
    score_species_released = score_species_released
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
        score_species_released
      ),na.rm = T))
write_csv(tidy_all_matches_docks_rl,"data/tidy_all_matches_docks_rl.csv")
