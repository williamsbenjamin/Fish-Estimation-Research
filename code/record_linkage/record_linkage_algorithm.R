library(tidyverse)
library(lubridate)

# use auto_rl_algorithm.R for Record Linkage

tidy_all_matches_docks <- read_csv("data/all_possible_matches_16_17.csv")

#take the cls only and mrip only datasets
#for the actual record linkage 
#Do a little wrangling of them too
mrip_tidy2_16 <- read_csv("data/mrip_tidy2_16.csv")
mrip_tidy2_17 <- read_csv("data/mrip_tidy2_17.csv")
cls_tidy2_17 <- read_csv("data/cls_tidy2_full.csv") #includes 2016 and 2017

#Get dataset of the mrip trips with a cls id
mrip_with_clsid <- bind_rows(mrip_tidy2_16,
                             mrip_tidy2_17)

mrip_with_clsid <- mrip_with_clsid %>%
  filter(CLS_ID != "NA")

nrow(mrip_with_clsid)

cool <- c(17:102,104:105)
mrip_with_clsid[,cool] = apply(mrip_with_clsid[,cool],2,function(x)as.numeric(x))  
is.zero.na <- function(x){
  x == 0 | is.na(x)
}
mrip_with_clsid <- mrip_with_clsid %>% 
 bind_cols(
  select(mrip_with_clsid,ends_with("claim")) %>%
    transmute(total_claim_mrip = rowSums(.,na.rm = T),
              reported_species_claim_mrip = rowSums(!is.zero.na(.)))
) %>%
  bind_cols(
    select(mrip_with_clsid,ends_with("release")) %>%
      transmute(total_release_mrip = rowSums(.,na.rm = T),
                reported_species_release_mrip = rowSums(!is.zero.na(.)))
  ) 

#Add on lat and long to mrip
dock_info <- read_csv("data/dock_locations.csv")
dock_location <- dock_info %>% 
  select(SITE_EXTERNAL_ID,
         SITE_LAT,
         SITE_LONG,
         STATE,
         STATUS) %>%
  filter(STATUS != "Retired")
#Reformat Lat and Long
dock_location$SITE_LAT <- gsub(":", ".",
                               gsub(".", "",
                                    (
                                      str_replace(dock_location$SITE_LAT, "\t", "")
                                    ),
                                    fixed = T),
                               fixed = T)
dock_location$SITE_LONG <- gsub(":", ".",
                                gsub(".", "",
                                     (
                                       str_replace(dock_location$SITE_LONG, "\t", "")
                                     ),
                                     fixed = T),
                                fixed = T)
#All the longitudes need to be changed to negative
#Negative longitude is everything west of england
dock_location$SITE_LONG <- paste0("-",dock_location$SITE_LONG,sep="")
dock_location$SITE_LAT <- as.numeric(dock_location$SITE_LAT)
dock_location$SITE_LONG <- as.numeric(dock_location$SITE_LONG)
#filter out the sites that are seen in our mrip data
docks_for_joins <- dock_location %>%
  filter(STATE %in% c("FL","AL","TX","LA","MS")) %>%
  filter(SITE_EXTERNAL_ID %in% unique(mrip_with_clsid$INTSITE)) 

mrip_with_clsid <- mrip_with_clsid%>%
  left_join(docks_for_joins,
            by=c("INTSITE"="SITE_EXTERNAL_ID"))

#CLS ONLY for 2016 and 2017
cls_17_only <- cls_tidy2_17
nrow(cls_17_only)

colz <- c(22:109)
cls_17_only[,colz] = apply(cls_17_only[,colz],2,function(x)as.numeric(x))  
is.zero.na <- function(x){
  x == 0 | is.na(x)
}

cls_17_only <- cls_17_only %>%
  bind_cols(
select(cls_17_only,ends_with("kept")) %>%
    transmute(total_kept_cls = rowSums(.,na.rm = T),
              reported_species_kept_cls = rowSums(!is.zero.na(.)))
) %>%
bind_cols(
    select(cls_17_only,ends_with("released")) %>%
      transmute(total_released_cls = rowSums(.,na.rm = T),
                reported_species_released_cls = rowSums(!is.zero.na(.)))
  ) 

#########
#Ok now everything is clean
#########
###Use all possible matches dataset

#Maybe just do this for each linking variable, and have a different 
#score vector for each variable, then add those as columns to the dataset (not necessary)
# then add them all up for each row to get the score


#Total number of species reportedly caught
score_species_caught <- c()
for(s in 1:nrow(tidy_all_matches_docks)) {
  if (tidy_all_matches_docks$reported_species_kept_cls[s] == #exact
      tidy_all_matches_docks$reported_species_claim_mrip[s]) {
    score_species_caught[s] <-
      -log(
        sum(
          mrip_with_clsid$reported_species_claim_mrip ==
            tidy_all_matches_docks$reported_species_kept_cls[s]
        ) /
          nrow(mrip_with_clsid)
      )
  } else if (abs( #close
    tidy_all_matches_docks$reported_species_kept_cls[s] -
    tidy_all_matches_docks$reported_species_claim_mrip[s]
  ) %in% c(1, 2)) {
    score_species_caught[s] <-
      -log(sum(
        abs(
          tidy_all_matches_docks$reported_species_kept_cls[s] -
            mrip_with_clsid$reported_species_claim_mrip
        ) %in% c(1, 2)
      ) /
        nrow(mrip_with_clsid)) + log(0.4604651)
  } else #disagree
    (score_species_caught[s] <- log(0.1906977))
}

#Total number of species reportedly released

score_species_released <- c()
for(s in 1:nrow(tidy_all_matches_docks)) {
  if (tidy_all_matches_docks$reported_species_released_cls[s] ==
      tidy_all_matches_docks$reported_species_release_mrip[s]) {
    score_species_released[s] <-
      -log(
        sum(
          mrip_with_clsid$reported_species_release_mrip ==
            tidy_all_matches_docks$reported_species_released_cls[s]
        ) /
          nrow(mrip_with_clsid)
      )
  } else if (abs(
    tidy_all_matches_docks$reported_species_released_cls[s] -
    tidy_all_matches_docks$reported_species_release_mrip[s]
  ) %in% c(1, 2)) {
    score_species_released[s] <-
      -log(sum(
        abs(
          tidy_all_matches_docks$reported_species_released_cls[s] -
            mrip_with_clsid$reported_species_release_mrip
        ) %in% c(1, 2)
      ) /
        nrow(mrip_with_clsid)) + log(0.5627907)
  } else
    (score_species_released[s] <- log(0.1162791))
}

#Difference in Total Catch

score_diff_total_catch <- c()
for(s in 1:nrow(tidy_all_matches_docks)) {
  if (tidy_all_matches_docks$total_kept_cls[s] ==
      tidy_all_matches_docks$total_claim_mrip[s]) {
    score_diff_total_catch[s] <-
      -log(
        sum(
          mrip_with_clsid$total_claim_mrip ==
            tidy_all_matches_docks$total_kept_cls[s]
        ) /
          nrow(mrip_with_clsid)
      )
  } else if (abs(
    tidy_all_matches_docks$total_kept_cls[s] -
    tidy_all_matches_docks$total_claim_mrip[s]
  ) %in% c(1:4)) {
    score_diff_total_catch[s] <-
      -log(sum(
        abs(
          tidy_all_matches_docks$total_kept_cls[s] -
            mrip_with_clsid$total_claim_mrip
        ) %in% c(1:4)
      ) /
        nrow(mrip_with_clsid)) + log(0.3534884)
  } else
    (score_diff_total_catch[s] <- log(0.5255814))
}

#Difference in Total Release

score_diff_total_release <- c()
for(s in 1:nrow(tidy_all_matches_docks)) {
  if (tidy_all_matches_docks$total_released_cls[s] ==
      tidy_all_matches_docks$total_release_mrip[s]) {
    score_diff_total_release[s] <-
      -log(
        sum(
          mrip_with_clsid$total_release_mrip ==
            tidy_all_matches_docks$total_released_cls[s]
        ) /
          nrow(mrip_with_clsid)
      )
  } else if (abs(
    tidy_all_matches_docks$total_released_cls[s] -
    tidy_all_matches_docks$total_release_mrip[s]
  ) %in% c(1:4)) {
    score_diff_total_release[s] <-
      -log(sum(
        abs(
          tidy_all_matches_docks$total_released_cls[s] -
            mrip_with_clsid$total_release_mrip
        ) %in% c(1:4)
      ) /
        nrow(mrip_with_clsid)) + log(0.3116279)
  } else
    (score_diff_total_release[s] <- log(0.5953488))
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
          mrip_with_clsid$`RED SNAPPER_claim` ==
            tidy_all_matches_docks$`RED SNAPPER_kept`[s]
        ) /
          nrow(mrip_with_clsid)
      )
  } else if (abs(
    tidy_all_matches_docks$`RED SNAPPER_kept`[s] -
    tidy_all_matches_docks$`RED SNAPPER_claim`[s]
  ) %in% c(1:5)) {
    score_diff_red_snapper_caught[s] <-
      -log(sum(
        abs(
          tidy_all_matches_docks$`RED SNAPPER_kept`[s] -
            mrip_with_clsid$`RED SNAPPER_claim`
        ) %in% c(1:5)
      ) /
        nrow(mrip_with_clsid)) + log(0.1358025)
  } else
    (score_diff_red_snapper_caught[s] <- log(0.09876543))
}


#Difference in Number of Anglers
score_diff_anglers <- c()
for(s in 1:nrow(tidy_all_matches_docks)) {
  if (tidy_all_matches_docks$nbAnglers[s] ==
      tidy_all_matches_docks$PARTY[s]) {
    score_diff_anglers[s] <-
      -log(
        sum(
          mrip_with_clsid$PARTY ==
            tidy_all_matches_docks$nbAnglers[s]
        ) /
          nrow(mrip_with_clsid)
      )
  } else if (abs(tidy_all_matches_docks$nbAnglers[s] -
                 tidy_all_matches_docks$PARTY[s]) %in% c(1)) {
    score_diff_anglers[s] <-
      -log(sum(
        abs(
          tidy_all_matches_docks$nbAnglers[s] -
            mrip_with_clsid$PARTY
        ) %in% c(1)
      ) /
        nrow(mrip_with_clsid)) + log(0.1428571)
  } else
    (score_diff_anglers[s] <- log(0.1142857))
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
          mrip_with_clsid$date ==
            tidy_all_matches_docks$return_date_ymd[s]
        ) /
          nrow(mrip_with_clsid)
      )
  } else if (as.numeric(
      tidy_all_matches_docks$return_date_ymd[s] -
      tidy_all_matches_docks$date[s]
    ) %in% c(-2,-1,1,2)) {
    score_diff_date[s] <-
      -log(sum(abs(
        as.numeric(
          tidy_all_matches_docks$return_date_ymd[s] -
            mrip_with_clsid$date
        )
      ) %in% c(-2,-1,1,2)) /
        nrow(mrip_with_clsid)) + log(0.06451613)
  } else
    (score_diff_date[s] <- log(.516129))
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
          tibble(mrip_with_clsid$SITE_LONG,
                 mrip_with_clsid$SITE_LAT)
        )
      ) / 1000),-100, 25
      ))) / nrow(mrip_with_clsid))
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
          tibble(mrip_with_clsid$SITE_LONG,
                 mrip_with_clsid$SITE_LAT)
        )
      ) / 1000), 25.01, 70
      ))) / nrow(mrip_with_clsid)) + log(0.6325581)
  } else
    (score_diff_report_distance[s] <- log(0.1860465))
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
