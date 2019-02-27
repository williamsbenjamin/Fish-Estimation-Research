library(tidyverse)
library(lubridate)
library(haven)

##################
#####
#Read in MRIP catch and trip data sets
#########

mrip_catch <- read_sas("data/catch_2016_cls.sas7bdat")
mrip_trip <- read_sas("data/trip_2016_cls.sas7bdat")
mrip_trip <- mrip_trip %>% 
  mutate(PRT_CODE = as.character(PRT_CODE))

###################

##I will be interested in CLAIM + HARVEST

#I will title claim to be the sum of 
#unadjusted claim and unadjusted harvest because
#I have already written all the code...


# mrip_catch <- mrip_catch %>% 
#   mutate(CLAIM_UNADJ = CLAIM_UNADJ + HARVEST_UNADJ)
# mrip_catch <- mrip_catch %>% 
#   mutate(CLAIM_UNADJ = CLAIM_UNADJ + HARVEST_UNADJ)


###get one ID_CODE per row
#One interview per row with columns being 
##trip and species specific
#start with mrip_catch
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
  unite(key2,fish_spec,key,sep="_") %>%
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
         PRT_CODE = as.character(PRT_CODE))


#join these mrip datasets

# mrip_ct <- m_catch %>%
#   left_join(m_trip, by = "ID_CODE") %>%
#   distinct() %>% 
#   mutate(CLS_ID = if_else(CLS_ID.x=="NA",
#                           as.numeric(CLS_ID.y),
#                           as.numeric(CLS_ID.x))) %>% 
#   select(-CLS_ID.x,-CLS_ID.y)
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
                        paste("0",TIME,sep = ""),
                        as.character(TIME)))

mrip_ct <- mrip_ct %>%
  mutate(TIME = paste0(str_sub(TIME,1,2),
                       ":",
                       str_sub(TIME,3,4))) 


mrip_ct <- mrip_ct %>% 
  mutate(date_time_mrip =ymd_hm(paste0(mrip_ct$date,
                                       " ",
                                       mrip_ct$TIME)
  )
  ) 


w_data <- read_sas("data/weights_2016_2017_gulf_block.sas7bdat")
w_data$ID_CODE <- as.character(w_data$ID_CODE)
mrip_ct <- mrip_ct %>% 
  left_join(
  select(w_data,ID_CODE,w_psu,w_int)
)

mrip_ct %>% 
  filter(PRT_CODE == 1571820160702012) %>% 
  select(PRT_CODE,ID_CODE,PARTY,wp_int,w_psu,w_int,
         starts_with("RED SN"),date_time_mrip) %>% 
  mutate(PRT_CODE = paste0(PRT_CODE,"a")) %>% 
  mutate(ID_CODE = paste0(ID_CODE,"a")) %>% 
  write_csv("data/ex1.csv")

mrip_ct %>% 
  filter(PRT_CODE == 1571820161023007) %>% 
  select(PRT_CODE,ID_CODE,PARTY,wp_int,w_psu,w_int,
         starts_with("RED SN"),date_time_mrip) %>% 
  mutate(PRT_CODE = paste0(PRT_CODE,"a")) %>% 
  mutate(ID_CODE = paste0(ID_CODE,"a")) %>% 
  write_csv("data/ex2.csv")


