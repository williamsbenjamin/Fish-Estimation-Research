library(tidyverse)
library(survey)
library(lubridate)
#naive linking and estimating

#naive matching

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


#add on reporting indicator to mrip_all
#cls_for_join
cls_for_join <- cls_all %>%
  drop_na(CLS_ID) %>% 
  mutate(cls_date = paste(return_date_ymd, ",", CLS_ID))
#all data
mrip_cls_all <- mrip_all %>%
  filter(CLS_ID %in% cls_all$CLS_ID) %>%
  mutate(cls_date = paste(date, ",", CLS_ID)) %>%
  left_join(cls_for_join,
            by = "cls_date")

mrip_cls_all <- mrip_cls_all %>% 
  mutate(reported = if_else(is.na(total_kept_cls)
                            ,0,1))
mrip_cls_all$total_kept_cls[is.na(mrip_cls_all$total_kept_cls)] <- 0

mrip_cls_all <- mrip_cls_all %>%
  mutate(y_star = total_kept_cls) %>%
  mutate(delta = total_claim_mrip - total_kept_cls) %>%
  mutate(non_reporter_mrip_claim = if_else(reported == 0,
                                           total_claim_mrip,0))

mrip_all_16 <- filter(mrip_cls_all,year(date)==2016)
cls_16 <- cls_all %>% 
  filter(year(return_date_ymd)==2016)


mrip_all_17 <- filter(mrip_cls_all,year(date)==2017)
cls_17 <- cls_all %>%
  filter(year(return_date_ymd)==2017)

###Just keep one trip-intercept combo
### This isn't a good matching procedure
### I'm just showing an example of naive matching!
mrip_all_16 <- mrip_all_16 %>% 
  distinct(return_date_ymd,date,CLS_ID.x,cls_date,.keep_all = T)
###

#2016
desi <- svydesign(id=~0,
                    weights=~wp_int,
                    strata = ~strat_id,
                    data=mrip_all_16)
options(survey.lonely.psu = "adjust") 
#centers the stratum with only 1 psu
#at population 
#mean for variance estimation
tyc <- svyratio(~total_claim_mrip,~total_kept_cls,design=desi,na.rm=T)
predict(tyc,total=sum(cls_16$total_kept_cls,na.rm=T))

ty2.r <- svyratio(~delta,~reported,design=desi,na.rm=T)
predict(ty2.r,total = nrow(cls_16))
predict(ty2.r,total = nrow(cls_16))[[1]] + sum(cls_16$total_kept_cls,na.rm=T)

typ <- svyratio(~total_claim_mrip,~reported,design=desi,na.rm=T)
predict(typ,total = nrow(cls_16),na.rm=T)


tydiff.d <- svytotal(~delta,design=desi,na.rm=T)
tydiff.d
tydiff.d[[1]] + sum(cls_16$total_kept_cls,na.rm=T)

tynew.nr <- svytotal(~non_reporter_mrip_claim,design=desi,na.rm=T)
tynew.nr
tynew.nr[[1]] + sum(cls_16$total_kept_cls,na.rm=T)
###As above for 2016 ... 
mrip_all_17 <- mrip_all_17 %>% 
  distinct(return_date_ymd,date,CLS_ID.x,cls_date,.keep_all = T)
###

#2017
desi17 <- svydesign(id=~0,
                  weights=~wp_int,
                  strata = ~strat_id,
                  data=mrip_all_17)
options(survey.lonely.psu = "adjust") 
#centers the stratum with only 1 psu
#at population 
#mean for variance estimation
tyc17 <- svyratio(~total_claim_mrip,~total_kept_cls,design=desi17,na.rm=T)
predict(tyc17,total=sum(cls_17$total_kept_cls,na.rm=T))

ty2.r17 <- svyratio(~delta,~reported,design=desi17,na.rm=T)
predict(ty2.r17,total = nrow(cls_17))
predict(ty2.r17,total = nrow(cls_17))[[1]] + sum(cls_17$total_kept_cls,na.rm=T)

typ.17 <- svyratio(~total_claim_mrip,~reported,design=desi17,na.rm=T)
predict(typ.17,total = nrow(cls_17),na.rm=T)

tydiff.d.17 <- svytotal(~delta,design=desi17,na.rm=T)
tydiff.d.17
tydiff.d.17[[1]] + sum(cls_17$total_kept_cls,na.rm=T)

tynew.nr.17 <- svytotal(~non_reporter_mrip_claim,design=desi17,na.rm=T)
tynew.nr.17
tynew.nr.17[[1]] + sum(cls_17$total_kept_cls,na.rm=T)



