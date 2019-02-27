### Get C*/C For Red Snapper and Vermillion Snapper
library(tidyverse)
library(ggthemes)
library(lubridate)
library(survey)
library(haven)
tidy_all_matches_one_per <- read_csv("data/tidy_all_matches_rl_one_per.csv",
                                     col_types=cols(psu_id = col_character()))
##RED SNAPPER

#mrip from 2016 and 2017
mrip_tidy2_16 <- read_csv("data/mrip_tidy2_16.csv",
                          col_types=cols(psu_id = col_character()))
mrip_tidy2_17 <- read_csv("data/mrip_tidy2_17.csv",
                          col_types=cols(psu_id = col_character()))
#CLS from 2016 and 2017
cls_tidy2_17 <- read_csv("data/cls_tidy2_17.csv") #includes 2016 and 2017

#Get dataset of the mrip trips with a cls id
mrip_all <- bind_rows(mrip_tidy2_16,
                      mrip_tidy2_17)
is.zero.na <- function(x){
  x == 0 | is.na(x)
}

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


#add on cls info to mrip_all
mrip_non_rep <- mrip_all %>% #take out the matches from mrip_all
  anti_join(tidy_all_matches_one_per,by="date_time_mrip") 

mrip_full <- bind_rows(tidy_all_matches_one_per,
                       mrip_non_rep)

mrip_full <- mrip_full %>%
  mutate(reported = if_else(is.na(total_kept_cls)
                            ,0,1))
mrip_full$total_kept_cls[is.na(mrip_full$total_kept_cls)] <- 0
mrip_full$`RED SNAPPER_kept`[is.na(mrip_full$`RED SNAPPER_kept`)] <- 0
mrip_full <- mrip_full %>% 
  mutate(y_star = total_kept_cls) %>% 
  mutate(delta_rs = `RED SNAPPER_claim`- `RED SNAPPER_kept`) %>%
  mutate(non_reporter_mrip_claim = if_else(reported == 0,
                                           total_claim_mrip,0))
mrip_cls_all <- mrip_full #mrip_full contains all mrip and cls info

mrip_all_16 <- filter(mrip_cls_all,year(date)==2016)
cls_16 <- cls_all %>% filter(year(return_date_ymd)==2016)
mrip_all_17 <- filter(mrip_cls_all,year(date)==2017)
cls_17 <- cls_all %>% 
  filter(year(return_date_ymd)==2017)

desi17_all <- svydesign(id=~psu_id,
                          weights=~wp_int,
                          strata = ~strat_id,
                          nest=T,
                          data=mrip_all_17)
options(survey.lonely.psu = "adjust") 
ty2.r17_all <- svyratio(~delta_rs,
                        ~reported,
                        design=desi17_all,
                        na.rm=T)
predict(ty2.r17_all,total = nrow(cls_17))
predict(ty2.r17_all,total = nrow(cls_17))[[1]] + sum(cls_17$`RED SNAPPER_kept`,na.rm=T)

##RED SNAPPER C*/C = 0.6



##Vermillion SNAPPER


tidy_all_matches_one_per <- read_csv("data/tidy_all_matches_rl_one_per.csv",
                                     col_types=cols(psu_id = col_character()))

#mrip from 2016 and 2017
mrip_tidy2_16 <- read_csv("data/mrip_tidy2_16.csv",
                          col_types=cols(psu_id = col_character()))
mrip_tidy2_17 <- read_csv("data/mrip_tidy2_17.csv",
                          col_types=cols(psu_id = col_character()))
#CLS from 2016 and 2017
cls_tidy2_17 <- read_csv("data/cls_tidy2_17.csv") #includes 2016 and 2017

#Get dataset of the mrip trips with a cls id
mrip_all <- bind_rows(mrip_tidy2_16,
                      mrip_tidy2_17)
is.zero.na <- function(x){
  x == 0 | is.na(x)
}

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


#add on cls info to mrip_all
mrip_non_rep <- mrip_all %>% #take out the matches from mrip_all
  anti_join(tidy_all_matches_one_per,by="date_time_mrip") 

mrip_full <- bind_rows(tidy_all_matches_one_per,
                       mrip_non_rep)

mrip_full <- mrip_full %>%
  mutate(reported = if_else(is.na(total_kept_cls)
                            ,0,1))
mrip_full$total_kept_cls[is.na(mrip_full$total_kept_cls)] <- 0
mrip_full$`VERMILION SNAPPER_kept`[is.na(mrip_full$`VERMILION SNAPPER_kept`)] <- 0
mrip_full <- mrip_full %>% 
  mutate(y_star = total_kept_cls) %>% 
  mutate(delta_rs = `VERMILION SNAPPER_claim`- `VERMILION SNAPPER_kept`) %>%
  mutate(non_reporter_mrip_claim = if_else(reported == 0,
                                           total_claim_mrip,0))
mrip_cls_all <- mrip_full #mrip_full contains all mrip and cls info

mrip_all_16 <- filter(mrip_cls_all,year(date)==2016)
cls_16 <- cls_all %>% filter(year(return_date_ymd)==2016)
mrip_all_17 <- filter(mrip_cls_all,year(date)==2017)
cls_17 <- cls_all %>% 
  filter(year(return_date_ymd)==2017)

desi17_all <- svydesign(id=~psu_id,
                        weights=~wp_int,
                        strata = ~strat_id,
                        nest=T,
                        data=mrip_all_17)
options(survey.lonely.psu = "adjust") 
ty2.r17_all <- svyratio(~delta_rs,
                        ~reported,
                        design=desi17_all,
                        na.rm=T)
predict(ty2.r17_all,total = nrow(cls_17))
predict(ty2.r17_all,total = nrow(cls_17))[[1]] + sum(cls_17$`VERMILION SNAPPER_kept`,na.rm=T)

#Vermillion Snapper C*/C = 0.3
