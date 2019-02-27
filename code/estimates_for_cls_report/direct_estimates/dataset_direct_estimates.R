library(tidyverse)
library(lubridate)
library(haven)
mrip_dmatch_17 <- read_sas("data/tidy_naive_20181005.sas7bdat")

mrip_dmatch_17 <- mrip_dmatch_17 %>% 
  mutate(time = str_sub(returndate,12)) %>% 
  unite(return_date_time,return_date_ymd,time,sep = " ",remove = F) %>% 
  mutate(time_mrip = str_sub(date_time_mrip,12,19)) %>% 
  mutate(date_mrip = str_sub(date_time_mrip,0,10)) %>% 
  unite(date_time_mrip, date_mrip,time_mrip,sep = " ", remove = T) %>% 
  mutate(date_time_mrip = ymd_hms(date_time_mrip)) %>% 
  mutate(return_date_time = ymd_hms(return_date_time)) %>% 
  mutate(psu_id = as.character(psu_id)) %>% 
  mutate(return_date_ymd = ymd(return_date_ymd))

mrip_tidy2_17 <- read_csv("data/mrip_tidy2_17.csv",
                          col_types=cols(psu_id = col_character()))

#CLS from 2016 and 2017
cls_tidy2_17 <- read_csv("data/cls_tidy2_17.csv") #includes 2016 and 2017

#Get dataset of the mrip trips with a cls id
mrip_all <- mrip_tidy2_17

mrip_all <- mrip_all %>%
  bind_cols(select(mrip_all, ends_with("claim")) %>%
              transmute(total_claim_mrip = rowSums(., na.rm = T))) %>%
  bind_cols(select(mrip_all, ends_with("release")) %>%
              transmute(total_release_mrip = rowSums(., na.rm = T)))
colz <- c(22:106)
cls_tidy2_17[,colz] = apply(cls_tidy2_17[,colz],2,function(x)as.numeric(x))  

cls_all <- cls_tidy2_17 %>%
  bind_cols(
    select(cls_tidy2_17,ends_with("kept")) %>%
      transmute(total_kept_cls = rowSums(.,na.rm = T)
      )
  ) %>%
  bind_cols(
    select(cls_tidy2_17,ends_with("released")) %>%
      transmute(total_released_cls = rowSums(.,na.rm = T)
      )
  )

mrip_dmatch_17 <- mrip_dmatch_17 %>% 
  mutate(reported = 1)

cls_dm_full <- cls_all %>% 
  left_join(
    select(mrip_dmatch_17,tripID,CLS_ID,
           return_date_ymd,reported,date_time_mrip)
  ) 

#join self-reported matches with mrip data 
mrip_full <- mrip_all %>% 
  left_join(cls_dm_full, by = c("CLS_ID","date_time_mrip"))

mrip_full <- mrip_full %>%
  mutate(reported = if_else(is.na(reported)
                            ,0,1))
mrip_full$total_kept_cls[is.na(mrip_full$total_kept_cls)] <- 0
mrip_full <- mrip_full %>% 
  mutate(y_star = total_kept_cls) %>%
  mutate(delta = total_claim_mrip - total_kept_cls) %>%
  mutate(non_reporter_mrip_claim = if_else(reported == 0,
                                           total_claim_mrip,0))
mrip_cls_all <- mrip_full #mrip_full contains all mrip and cls info
write_csv(mrip_cls_all,"data/mrip_dm_17_all_sites.csv")
