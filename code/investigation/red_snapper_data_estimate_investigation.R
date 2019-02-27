spec_table <- read_sas("data/species_table.sas7bdat")
spec_table <- spec_table %>%
  mutate(SP_CODE = as.character(SP_CODE)) %>%
  select(SP_CODE,COMMON) %>%
  rename("speciesCode" = "SP_CODE")

#estimated landings 2017
est_time <- read_sas("data/estimated_landing.sas7bdat")


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
           returndate, estimated_returndate
    )
)

cls_trips <- cls_trips %>% 
  filter(isCharterTrip == "Y")
tidy_cls <- cls_trips
tidy_cls$departdate <- ymd_hms(tidy_cls$departdate)
tidy_cls$returndate <- ymd_hms(tidy_cls$returndate)
tidy_cls$report_date <- ymd_hms(tidy_cls$reportdate)
tidy_cls$estimated_returndate <- ymd_hms(tidy_cls$estimated_returndate)
tidy_cls$return_date_ymd <- vapply(str_split(tidy_cls$returndate," "),"[","",1)
tidy_cls_17 <- tidy_cls %>%
  filter(year(returndate) == 2017 &
           year(reportdate) != 2016)
nrow(tidy_cls_17)

dm <- read_csv("data/mrip_dm_17_all_sites.csv")

##get mrip

mrip_catch <- read_sas("data/catch_2017_cls.sas7bdat")
mrip_trip <- read_sas("data/trip_2017_cls.sas7bdat")
mrip_trip <- mrip_trip %>% 
  mutate(PRT_CODE = as.character(PRT_CODE))

m_catch <-
  mrip_catch %>% 
  select(-tot_len,-wgt_ab1,
         -tot_len_b1,-wgt_b1,-tot_len_a,-wgt_a,
         -tot_cat,-tot_len,-landing,
         -MODE_FX,-AREA_X,-month,-fl_reg,-alt_flag,
         -arx_method,-kod,-SUB_REG,-region) %>%
  unite("fish_spec",c(common,SP_CODE),sep="/") %>%
  unite("info",c(strat_id,psu_id,YEAR,ST,
                 ID_CODE,WAVE,
                 var_id,
                 CLS,CLS_ID,wp_int,wp_catch),sep="/") %>%
  gather(key,val, -c(fish_spec,info)) %>%
  unite(key2,fish_spec,key,sep="_")%>%
  spread(key2,val) %>%
  separate(info, into = c("strat_id","psu_id","YEAR","ST",
                          "ID_CODE","WAVE",
                          "var_id",
                          "CLS","CLS_ID","wp_int","wp_catch"),
           sep="/")

#Fix 2 CLS_IDs per some PRT_CODES, some were NA
cls_2_prt <- mrip_trip %>% 
  distinct(PRT_CODE,CLS_ID) %>%
  drop_na(CLS_ID)
mrip_trip <- mrip_trip %>% 
  select(-CLS_ID) %>% 
  left_join(cls_2_prt)

#maybe remove
m_trip <- mrip_trip %>% 
  select(ID_CODE,
         prim1_common,
         PARTY,
         INTSITE,
         TIME,
         PRT_CODE,
         CLS_ID) %>%
  mutate(ID_CODE = as.character(ID_CODE),
         prim1_common = as.character(prim1_common),
         PARTY = as.numeric(PARTY),
         INTSITE = as.numeric(INTSITE),
         TIME = as.numeric(TIME),
         PRT_CODE = as.character(PRT_CODE),
         CLS_ID)


#join the mrip datasets
mrip_ct <- m_catch %>%
  left_join(m_trip, by = "ID_CODE") %>%
  distinct() %>% 
  mutate(CLS_ID =  CLS_ID.x) %>% 
  select(-CLS_ID.x,-CLS_ID.y)

mrip_ct <- mrip_ct %>%
  mutate(
    date = ymd(
      str_sub(
        ID_CODE,
        6,
        13
      )
    )
  )
#format time
mrip_ct <- mrip_ct %>%
  mutate(TIME = if_else(TIME < 1000,
                        paste("0",TIME,sep = ""),as.character(TIME))) %>% 
  mutate(TIME = if_else(TIME == "015",
                        "0015",as.character(TIME)))


mrip_ct <- mrip_ct %>%
  mutate(TIME = paste0(str_sub(TIME,1,2),
                       ":",
                       str_sub(TIME,3,4))) 

#  mutate(TIME = if_else(TIME == "NA:NA",
#                        "12:00",TIME))
#date_time_mrip has date and time of interview
mrip_ct <- mrip_ct %>%
  mutate(date_time_mrip =ymd_hm(paste0(mrip_ct$date,
                                       " ",
                                       mrip_ct$TIME)
  )
  ) 

mrip_ct <- mrip_ct %>%
  select(1:10,starts_with("RED SNAP"),1182:1188,-YEAR,-ST,-WAVE,-var_id,-CLS,-date) 


# mrip_ct <- mrip_ct %>%
#   select(1:10,starts_with("RED SNAP"),1182:1188) %>% 
#   group_by(PRT_CODE) %>%
#   mutate(`RED SNAPPER_claim`=sum(`RED SNAPPER/8835360107_CLAIM`,na.rm=T),
#          `RED SNAPPER_claim_unadjusted`=sum(`RED SNAPPER/8835360107_CLAIM_UNADJ`,na.rm=T),
#          `RED SNAPPER_harvest`=sum(`RED SNAPPER/8835360107_HARVEST`,na.rm=T),
#          `RED SNAPPER_harvest_unadjusted`=sum(`RED SNAPPER/8835360107_HARVEST_UNADJ`,na.rm=T),
#          `RED SNAPPER_release`=sum(`RED SNAPPER/8835360107_RELEASE`,na.rm=T),
#          `RED SNAPPER_release_unadjusted`=sum(`RED SNAPPER/8835360107_RELEASE_UNADJ`,na.rm=T)
#          ) %>%
#   ungroup() %>%
#   select(-ends_with("unadj"),
#          -`RED SNAPPER/8835360107_CLAIM`,
#          -`RED SNAPPER/8835360107_HARVEST`,
#          -`RED SNAPPER/8835360107_RELEASE`,
#          -ID_CODE) %>%
#   distinct() %>%
#   mutate(psu_id = as.character(psu_id))
# 
# #only keep the first interview per trip done by MRIP
# mrip_ct <- mrip_ct %>%
#   #group_by(psu_id,wp_int,CLS_ID,PARTY,INTSITE) %>%
#   group_by(PRT_CODE) %>%
#   filter(date_time_mrip == min(date_time_mrip)) %>%
#   ungroup()
# 
# nrow(mrip_ct)
# 
# ####
mrip_j <- mrip_ct %>%
  select(PRT_CODE,starts_with("RED SN"))
cls_j <- cls_trips %>%
  select(tripID,starts_with("RED S"),nbAnglers)
examine <- dm %>% 
  mutate(PRT_CODE = as.character(PRT_CODE)) %>% 
  select(wp_int,wp_catch,PARTY,date_time_mrip,returndate,PRT_CODE,reported,tripID) %>% 
  left_join(cls_j,by = "tripID") %>% 
  left_join(mrip_j,by = "PRT_CODE")


write_csv(examine,"data/red_snapper_investigation.csv")

##deeper
mrip_dmatch_17 <- read_sas("data/tidy_naive_20180912.sas7bdat")


mrip_rs_d <- mrip_ct %>% 
  #select(PRT_CODE,ID_CODE,wp_int,wp_catch,PARTY,starts_with("RED SN")) %>% 
  filter(PRT_CODE %in% (examine %>% filter(reported == 1) %>% pull(PRT_CODE)))

#Add on base psu weights
w_data <- read_sas("data/weights_2016_2017_gulf_block.sas7bdat")

mrip_rs_d <- mrip_rs_d %>% 
  left_join(
    select(w_data,ID_CODE,w_psu,w_int)
           )

mrip_rs_d <- mrip_rs_d %>% 
  mutate(ID_CODE = paste0(ID_CODE,"a"),
         PRT_CODE = paste0(PRT_CODE,"a"),
         psu_id = paste0(psu_id,"a"))
write_csv(mrip_rs_d,"data/red_snapper_investigation_mrip_raw_9_27.csv")
