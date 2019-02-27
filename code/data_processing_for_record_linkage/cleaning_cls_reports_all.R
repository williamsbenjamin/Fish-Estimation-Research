#Clean up cls reports
library(tidyverse)
library(lubridate)

#species info
spec_table <- read_sas("data/species_table.sas7bdat")
spec_table <- spec_table %>%
  mutate(SP_CODE = as.character(SP_CODE)) %>%
  select(SP_CODE,COMMON) %>%
  rename("speciesCode" = "SP_CODE")

#estimated landings 2017
#est_time <- read_sas("data/estimated_landing.sas7bdat")
est_time17 <- read_sas("data/cls_estimated_20181015.sas7bdat")
est_time17 <- est_time17 %>% 
  mutate(estimated_returndate = GPS_RETURNTIME_ADJ)

est_time16 <- read_sas("data/cls_estimated2016_20181015.sas7bdat")
est_time16 <- est_time16 %>% 
  mutate(estimated_returndate = GPS_RETURNTIME_ADJ)

est_time <- bind_rows(est_time16,
                      est_time17)

cls_data <- read_sas("data/clsdata_20180403.sas7bdat")
cls_data <- cls_data %>% #try this
  distinct(speciesCode,
           returndate,
           departdate,
           kept,
           releasedAlive,
           releasedDead,
           TID,USCG,
           captainName,
           permit,
           isCharterTrip,
           nbPassengers,
           nbAnglers,
           nbCrew,
           region,
           firstTarget,
           secondTarget,
           depthMin,
           depthMax,
           depthPrimary,
           hours,
           state,
           county,
           Name,
           gallons,
           pricegallons,
           .keep_all = TRUE)

tid_cls <- read_sas("data/cls2tid_20170905.sas7bdat")
tid_cls$TID <- as.integer(tid_cls$TID)
cls_data <- cls_data %>%
  left_join(tid_cls)

cls_data <- cls_data %>% 
    filter(!CLS_ID %in% 
           c(4,5,48,81,87,117,118,
             146,184,193,222,226,
             248,39,70,73,92,99,
             239,51,80,89,93,126,
             129,161,180,203,41,
             104,117,116,125,129,
             131,132,149,150,151,
             152,165,166,167,168,
             169,170,171,172,173,
             174,175,176,177,178,
             179,180,181,216))  %>% 
  filter(CLS_ID > 0)

cls_data <- cls_data %>%
  filter(!TID %in% c(512517,
                     512518,
                     512146,
                     512494,
                     512220,
                     512220,
                     512146,
                     512494,
                     512517,
                     512518,
                     512516,
                     512068,
                     512249,
                     512260,
                     512295,
                     512298,
                     512306,
                     512340,
                     512498,
                     513709,
                     520278))
cls_data<- cls_data %>% 
  filter(state %in% c("FL","AL","MS"))

trips <- cls_data %>% 
  distinct(tripID) %>% 
  pull(tripID)
nrow(cls_data %>%
       distinct(tripID,.keep_all = T) %>%
       filter(year(reportdate) == 2017) %>%
       filter(year(returndate) == 2017)) #should be 6308

cls_data <- cls_data %>% 
  select(-Activation_Date,-Deactivation_Date)

cls_data <- cls_data %>%
  mutate(speciesCode = as.character(speciesCode)) %>%
  left_join(spec_table,by="speciesCode")

#rename the common name of species to be speciesCode, works with later code
cls_data <- cls_data %>%
  select(-speciesCode) %>%
  rename(speciesCode=COMMON)

#call kept = released alive + kept
#released = released dead
#then drop released alive, released dead
cls_data <- cls_data %>%
  mutate(kept = as.numeric(releasedDead) + as.numeric(kept)) %>%
  mutate(released = releasedAlive) %>% 
  select(-releasedAlive,-releasedDead)


####testing this
cls_dat_species_spec <- cls_data %>%
  select(-gallons,-pricegallons,-depthMin,
         -depthMax,-depthPrimary,-ID_1,
         -ID,-TID,-USCG,-captainName,
         -permit,-latitude,-longitude,
         isCharterTrip,-nbPassengers,
         -nbAnglers,-nbCrew, -region,
         -firstTarget,-secondTarget,-hours,
         -state, -county, -Name, -departdate,
         -returndate, -reportdate,-isCharterTrip,
         -CLS_ID) %>% 
  gather(key,val, -c(speciesCode,tripID)) %>% 
  #need to name by common name maybe?
  unite(key2,speciesCode,key,sep="_") %>% 
  #rather than species code, can come back and do that, just replace speciescode with common
  spread(key2,val) 

cls_dat_trips_spec <- cls_data %>%
  select(tripID,TID,USCG,captainName,
         permit,latitude,longitude,
         isCharterTrip,nbPassengers,
         nbAnglers,nbCrew, region,
         firstTarget,secondTarget,hours,
         state, county, Name, departdate,
         returndate, reportdate,isCharterTrip,CLS_ID) %>%
  distinct()


cls_trips <- inner_join(cls_dat_trips_spec,
                         cls_dat_species_spec,
                        by = "tripID")


cls_trips <- cls_trips %>% left_join(
  est_time %>% 
    select(tripID,TID,departdate, 
           returndate, estimated_returndate, GPS_INTSITE_RETURN
    )
)

cls_trips <- cls_trips %>% 
  filter(isCharterTrip == "Y")

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
  filter(SITE_EXTERNAL_ID %in% unique(cls_trips$GPS_INTSITE_RETURN)) 


cls_trips <- cls_trips %>%
  mutate(GPS_INTSITE_RETURN = as.integer(GPS_INTSITE_RETURN)) %>% 
  left_join(docks_for_joins,
            by=c("GPS_INTSITE_RETURN"="SITE_EXTERNAL_ID"))

cls_trips <- cls_trips %>% 
  rename(CLS_SITE_LAT = SITE_LAT) %>% 
  rename(CLS_SITE_LONG = SITE_LONG) %>% 
  select(-STATE,-STATUS)

nrow(cls_trips %>%
       distinct(tripID,.keep_all = T) %>%
       filter(year(reportdate) == 2017) %>%
       filter(year(returndate) == 2017))
#should be 6259

write_csv(cls_trips,"data/cls_tidy_full.csv") #all reports
