## Source code to estimate total
## using t_y2, public docks only
## with matches made via record-
## linkage. The function below
## gives estimate with the 
## parameter being: cutoff score

library(tidyverse)
library(lubridate)
library(survey)
library(haven)

#matches
tidy_all_matches_one_per <- read_csv("data/tidy_all_matches_rl_one_per.csv",
                                     col_types=cols(psu_id = col_character()))
#mrip from 2016 and 2017
mrip_tidy2_16 <- read_csv("data/mrip_tidy2_16.csv",
                          col_types=cols(psu_id = col_character()))
mrip_tidy2_17 <- read_csv("data/mrip_tidy2_17.csv",
                          col_types=cols(psu_id = col_character()))
#CLS from 2016 and 2017
cls_tidy2_17 <- read_csv("data/cls_tidy2_full.csv") #includes 2016 and 2017

#public/private - only look at public trips for now
cls_private_public_all <- read_sas("data/cls_pp.sas7bdat")
cls_private <- cls_private_public_all %>% 
  filter(Public_id2 %in% c(0,-1)) %>% 
  filter(Public_id == 0)

cls_tidy2_17 <- cls_tidy2_17 %>% 
  anti_join(cls_private)


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
colz <- c(23:106)
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


cls_16 <- cls_all %>% 
  filter(year(return_date_ymd)==2016)
cls_17 <- cls_all %>% 
  filter(year(return_date_ymd)==2017)

ty2_cutoff_estimates <- function(score){
  tidy_all_score_gt_3 <- tidy_all_matches_one_per %>%
    filter(rl_score > score)
  
  mrip_non_rep2 <- mrip_all %>% #take out the matches from mrip_all
    anti_join(tidy_all_score_gt_3,by="date_time_mrip") 
  
  mrip_full2 <- bind_rows(tidy_all_score_gt_3,
                          mrip_non_rep2)
  mrip_full2 <- mrip_full2 %>%
    mutate(reported = if_else(is.na(total_kept_cls)
                              ,0,1))
  
  mrip_full2$total_kept_cls[is.na(mrip_full2$total_kept_cls)] <- 0
  
  mrip_full2 <- mrip_full2 %>% 
    mutate(y_star = total_kept_cls) %>%
    mutate(delta = total_claim_mrip - total_kept_cls) %>%
    mutate(non_reporter_mrip_claim = if_else(reported == 0,
                                             total_claim_mrip,0))
  
  mrip_cls_all2 <- mrip_full2 #mrip_full contains all mrip and cls info
  
  mrip_all_16_2 <- filter(mrip_cls_all2,year(date)==2016)
  
  mrip_all_17_2 <- filter(mrip_cls_all2,year(date)==2017)
 
   #2016 
  des16_all_2 <- svydesign(id=~psu_id,
                           weights=~wp_int,
                           strata=~strat_id,
                           nest=T,
                           data=mrip_all_16_2)
  options(survey.lonely.psu = "adjust") 
  #centers the stratum with only 1 psu
  #at population 
  #mean for variance estimation
  ty2.r_2 <- svyratio(~delta,~reported,design=des16_all_2,na.rm=T)
  ty2_2016_estimate <- predict(ty2.r_2,total = nrow(cls_16))[[1]] + sum(cls_16$total_kept_cls,na.rm=T)
  ty2_2016_se <- predict(ty2.r_2,total = nrow(cls_16))[[2]]
  #2017
  desi17_all_2 <- svydesign(id=~psu_id,
                            weights=~wp_int,
                            strata = ~strat_id,
                            nest=T,
                            data=mrip_all_17_2)
  options(survey.lonely.psu = "adjust") 
  ty2.r17_all_2 <- svyratio(~delta,
                            ~reported,
                            design=desi17_all_2,
                            na.rm=T)
  ty2_2017_estimate <- predict(ty2.r17_all_2,total = nrow(cls_17))[[1]] + sum(cls_17$total_kept_cls,na.rm=T)
  ty2_2017_se <- predict(ty2.r17_all_2,total = nrow(cls_17))[[2]]
  est_n1_16 <- tidy_all_score_gt_3 %>% 
                  filter(YEAR == 2016) %>% 
                  pull(wp_int) %>% 
                  sum()
  est_n1_17 <- tidy_all_score_gt_3 %>% 
                  filter(YEAR == 2017) %>% 
                  pull(wp_int) %>% 
                  sum()
  return(list(estimate_2017 = as.numeric(ty2_2017_estimate),
              se_2017 = as.numeric(ty2_2017_se),
              se_2016 = as.numeric(ty2_2016_se),
              estimate_2016 = as.numeric(ty2_2016_estimate),
              n1_hat_16 = est_n1_16,
              n1_hat_17 = est_n1_17))
}
