library(tidyverse)
library(ggthemes)
library(lubridate)
library(survey)
library(haven)
#matches
tidy_all_matches_one_per <- read_csv("data/tidy_all_matches_rl_one_per.csv")
#mrip from 2016 and 2017
mrip_tidy2_16 <- read_csv("data/mrip_tidy2_16.csv")
mrip_tidy2_17 <- read_csv("data/mrip_tidy2_17.csv")
#CLS from 2016 and 2017
cls_tidy2_17 <- read_csv("data/cls_tidy2_17.csv") #includes 2016 and 2017

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
mrip_full <- mrip_full %>% 
  mutate(y_star = total_kept_cls) %>%
  mutate(delta = total_claim_mrip - total_kept_cls) %>%
  mutate(non_reporter_mrip_claim = if_else(reported == 0,
                                           total_claim_mrip,0))
mrip_cls_all <- mrip_full #mrip_full contains all mrip and cls info

mrip_all_16 <- filter(mrip_cls_all,year(date)==2016)
cls_16 <- cls_all %>% filter(year(return_date_ymd)==2016)
mrip_all_17 <- filter(mrip_cls_all,year(date)==2017)
cls_17 <- cls_all %>% 
  filter(year(return_date_ymd)==2017)


####USING ALL RECORD LINKAGE ROWS
#2016 

des16_all <- svydesign(id=~psu_id,
                       weights=~wp_int,
                       strata=~strat_id,
                       nest=T,
                       data=mrip_all_16)
options(survey.lonely.psu = "adjust") 
#centers the stratum with only 1 psu
#at population 
#mean for variance estimation

tyc_all <- svyratio(~total_claim_mrip,~total_kept_cls,
                    design=des16_all,na.rm=T)
predict(tyc_all,total=sum(cls_16$total_kept_cls,na.rm=T))

ty2.r <- svyratio(~delta,~reported,design=des16_all,na.rm=T)
predict(ty2.r,total = nrow(cls_16))
predict(ty2.r,total = nrow(cls_16))[[1]] + sum(cls_16$total_kept_cls,na.rm=T)

typ <- svyratio(~total_claim_mrip,~reported,design=des16_all,na.rm=T)
predict(typ,total = nrow(cls_16),na.rm=T)

tydiff.d <- svytotal(~delta,design=des16_all,na.rm=T)
tydiff.d
tydiff.d[[1]] + sum(cls_16$total_kept_cls,na.rm=T)

tynew.nr <- svytotal(~non_reporter_mrip_claim,design=des16_all)
tynew.nr
tynew.nr[[1]] + sum(cls_16$total_kept_cls,na.rm=T)

#2017
desi17_all <- svydesign(id=~psu_id,
                    weights=~wp_int,
                    strata = ~strat_id,
                    nest=T,
                    data=mrip_all_17)
options(survey.lonely.psu = "adjust") 
#centers the stratum with only 1 psu
#at population 
#mean for variance estimation
tyc17_all <- svyratio(~total_claim_mrip,
                      ~total_kept_cls,
                      design=desi17_all,
                      na.rm=T)

predict(tyc17_all,total=sum(cls_17$total_kept_cls,na.rm=T))

ty2.r17_all <- svyratio(~delta,
                        ~reported,
                        design=desi17_all,
                        na.rm=T)
predict(ty2.r17_all,total = nrow(cls_17))
predict(ty2.r17_all,total = nrow(cls_17))[[1]] + sum(cls_17$total_kept_cls,na.rm=T)

typ.17 <- svyratio(~total_claim_mrip,~reported,design=desi17_all,na.rm=T)
predict(typ.17,total = nrow(cls_17),na.rm=T)

tydiff.d.17 <- svytotal(~delta,design=desi17_all,na.rm=T)
tydiff.d.17
tydiff.d.17[[1]] + sum(cls_17$total_kept_cls,na.rm=T)

tynew.nr.17 <- svytotal(~non_reporter_mrip_claim,design=desi17_all,na.rm=T)
tynew.nr.17
tynew.nr.17[[1]] + sum(cls_17$total_kept_cls,na.rm=T)


#CUTOFF THE RECORD LINKAGE ROWS AT 4
#basically need to redo the step where I combine mrip and cls


tidy_all_score_gt_3 <- tidy_all_matches_one_per %>%
  filter(rl_score > 4)
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
cls_16_2 <- cls_all %>% 
  filter(year(return_date_ymd)==2016)
mrip_all_17_2 <- filter(mrip_cls_all2,year(date)==2017)
cls_17_2 <- cls_all %>% 
  filter(year(return_date_ymd)==2017)


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

tyc_all_2 <- svyratio(~total_claim_mrip,
                      ~total_kept_cls,
                      design=des16_all_2,
                      na.rm=T)
predict(tyc_all_2,total=sum(cls_16$total_kept_cls,na.rm=T))

ty2.r_2 <- svyratio(~delta,~reported,design=des16_all_2,na.rm=T)
predict(ty2.r_2,total = nrow(cls_16))
predict(ty2.r_2,total = nrow(cls_16))[[1]] + sum(cls_16$total_kept_cls,na.rm=T)

typ.2 <- svyratio(~total_claim_mrip,~reported,design=des16_all_2,na.rm=T)
predict(typ.2,total = nrow(cls_16),na.rm=T)

tydiff.d.2 <- svytotal(~delta,design=des16_all_2,na.rm=T)
tydiff.d.2
tydiff.d.2[[1]] + sum(cls_16$total_kept_cls,na.rm=T)

tynew.nr.2 <- svytotal(~non_reporter_mrip_claim,design=des16_all_2,na.rm=T)
tynew.nr.2
tynew.nr.2[[1]] + sum(cls_16$total_kept_cls,na.rm=T)


#2017
desi17_all_2 <- svydesign(id=~psu_id,
                        weights=~wp_int,
                        strata = ~strat_id,
                        nest=T,
                        data=mrip_all_17_2)
options(survey.lonely.psu = "adjust") 
#centers the stratum with only 1 psu
#at population 
#mean for variance estimation
tyc17_all_2 <- svyratio(~total_claim_mrip,
                      ~total_kept_cls,
                      design=desi17_all_2)

predict(tyc17_all_2,total=sum(cls_17$total_kept_cls,na.rm=T))

ty2.r17_all_2 <- svyratio(~delta,
                        ~reported,
                        design=desi17_all_2,
                        na.rm=T)
predict(ty2.r17_all_2,total = nrow(cls_17))
predict(ty2.r17_all_2,total = nrow(cls_17))[[1]] + sum(cls_17$total_kept_cls,na.rm=T)

typ.2.17 <- svyratio(~total_claim_mrip,~reported,design=desi17_all_2,na.rm=T)
predict(typ.2.17,total = nrow(cls_17),na.rm=T)

tydiff.d.2.17 <- svytotal(~delta,design=desi17_all_2,na.rm=T)
tydiff.d.2.17
tydiff.d.2.17[[1]] + sum(cls_17$total_kept_cls,na.rm=T)

tynew.nr.2.17 <- svytotal(~non_reporter_mrip_claim,design=desi17_all_2,na.rm=T)
tynew.nr.2.17
tynew.nr.2.17[[1]] + sum(cls_17$total_kept_cls,na.rm=T)

