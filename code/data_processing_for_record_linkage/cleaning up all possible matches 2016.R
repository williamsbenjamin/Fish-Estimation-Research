library(tidyverse)
library(geosphere)
library(ggthemes)
library(lubridate)
tidy_all_matches <- read_csv("data/2016_all_possible_matches_mrip_cls.csv",
                             col_names = T,
                             cols(
                               tripID = col_double(),
                               estimated_returndate = col_datetime(),
                               psu_id = col_character()
                             )
                    )
tidy_all_matches <- tidy_all_matches %>% 
  select(-wave_year.y) %>% 
  rename(wave_year = wave_year.x)
#make all catch and release data numeric
#cols = the columns with claim, release, kept, released variables
cols <- c(22:106,136:217)
tidy_all_matches[,cols] = apply(tidy_all_matches[,cols],2,function(x)as.numeric(x))  

#add on location of docks from mrip as lat,long
#in order to compare with lat/long from cls

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
#these will be joined onto tidy_all_matches
docks_for_joins <- dock_location %>%
  filter(STATE %in% c("FL","AL","MS")) %>%
  filter(SITE_EXTERNAL_ID %in% unique(tidy_all_matches$INTSITE)) 


#this makes tidy matches have more rows because some docks 
#have same identification code
#join on the dock sites to tidy_all_matches 
#in tidy matches use INTSITE 
#in dock_location use SITE_EXTERNAL_ID
tidy_all_matches_docks <- tidy_all_matches %>%
  left_join(docks_for_joins,
            by=c("INTSITE"="SITE_EXTERNAL_ID"))

lat.long <- tidy_all_matches_docks %>%
  select(latitude,longitude,
         SITE_LAT,SITE_LONG)


#km_difference is the distance in km between the report location and 
#the location of the dock for each potentially matched trip

tidy_all_matches_docks <- tidy_all_matches_docks %>% 
  mutate(CLS_LAT = if_else(is.na(CLS_SITE_LAT),
                                latitude,as.numeric(CLS_SITE_LAT))) %>% 
  mutate(CLS_LONG = if_else(is.na(CLS_SITE_LONG),
                                 longitude,as.numeric(CLS_SITE_LONG))) %>% 
  select(-CLS_SITE_LAT,-CLS_SITE_LONG)

tidy_all_matches_docks <- tidy_all_matches_docks %>%
  mutate(km_difference = 
           distHaversine(as.matrix(tibble(CLS_LONG, CLS_LAT)),
                         as.matrix(tibble(SITE_LONG, SITE_LAT))) / 1000)

# tidy_all_matches_docks <- tidy_all_matches_docks %>%
#   mutate(km_difference = 
#            distHaversine(as.matrix(tibble(longitude, latitude)),
#                          as.matrix(tibble(SITE_LONG, SITE_LAT))) / 1000)

#diff_number_anglers is the difference in the number of anglers 
#in CLS minus number in MRIP
tidy_all_matches_docks <- tidy_all_matches_docks %>%
   mutate(diff_cls_mrip_anglers = nbAnglers - PARTY)

#total_kept_cls = total fish caught on cls trip
#total_released_cls = total fish released on cls trip
#total_claim_mrip = total fish claim on mrip trip
#total_release_mrip = total fish release on mrip trip
#reported_species_kept_cls = number of reported species kept on cls trip
#reported_species_kept_cls = number of reported species kept on cls trip
#reported_species_kept_cls = number of reported species kept on cls trip

#need for species reported
is.zero.na <- function(x){
  x == 0 | is.na(x)
}


tidy_all_matches_docks <- tidy_all_matches_docks %>%
  select(-starts_with("mrip_non_cls")) %>% 
  bind_cols(
    select(tidy_all_matches_docks,ends_with("kept")) %>%
      transmute(total_kept_cls = rowSums(.,na.rm = T),
                reported_species_kept_cls = rowSums(!is.zero.na(.)))
  ) %>%
  bind_cols(
    select(tidy_all_matches_docks,ends_with("released")) %>%
      transmute(total_released_cls = rowSums(.,na.rm = T),
                reported_species_released_cls = rowSums(!is.zero.na(.)))
  ) %>%
  bind_cols(
    select(tidy_all_matches_docks,ends_with("claim")) %>%
      transmute(total_claim_mrip = rowSums(.,na.rm = T),
                reported_species_claim_mrip = rowSums(!is.zero.na(.)))
  ) %>%
  bind_cols(
    select(tidy_all_matches_docks,ends_with("release")) %>%
      transmute(total_release_mrip = rowSums(.,na.rm = T),
                reported_species_release_mrip = rowSums(!is.zero.na(.)))
  ) 

#difference in total catch per trip CLS - MRIP
#diff_total_catch = total_kept_cls - total_claim_mrip

#difference in total release per trip CLS - MRIP
#diff_total_release = total_released_cls - total_release_mrip

tidy_all_matches_docks <- tidy_all_matches_docks  %>%
  mutate(diff_total_catch = total_kept_cls - total_claim_mrip,
         diff_total_release = total_released_cls -total_release_mrip)

#Difference in date 
tidy_all_matches_docks <- tidy_all_matches_docks %>%
  mutate(returndate = if_else(is.na(estimated_returndate),
                              returndate,estimated_returndate))
tidy_all_matches_docks <- tidy_all_matches_docks %>%
  mutate(date_diff = (returndate - date_time_mrip)/(60*60*24))
write_csv(tidy_all_matches_docks,"data/tidy_all_matches_docks_2016.csv")

