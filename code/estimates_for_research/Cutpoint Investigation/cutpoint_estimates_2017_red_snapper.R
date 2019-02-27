library(tidyverse)
library(ggthemes)
library(lubridate)
library(survey)
library(haven)

#Define RL cutpoints 
source("code/source_code/second_step_mediating_datasets_for_rl_multiple_cutpoints.R")
#look at both private and pulic
#cls_17 <- read_csv("data/cls_17_all_sites.csv")

#make species specific variables for estimation

mrip_all_17_a$`RED SNAPPER_kept`[is.na(mrip_all_17_a$`RED SNAPPER_kept`)] <- 0
mrip_all_17_a <- mrip_all_17_a %>% 
  mutate(delta_rs = `RED SNAPPER_claim` - `RED SNAPPER_kept`) %>%
  mutate(non_reporter_mrip_claim_rs = if_else(reported == 0,
                                           `RED SNAPPER_claim`,as.numeric(0)))

####USING Cutpoint a
#2017
desi17_all_a <- svydesign(id = ~psu_id,
                    weights = ~w_int,
                    strata = ~strat_id,
                    nest=T,
                    data=mrip_all_17_a)
options(survey.lonely.psu = "adjust") 

#centers the stratum with only 1 psu
#at population 
#mean for variance estimation
tyc17_rs_a <- svyratio(~`RED SNAPPER_claim`,
                      ~`RED SNAPPER_kept`,
                      design=desi17_all_a,
                      na.rm=T)
#tyc
tyc_17_rs_se_a <- as.numeric(predict(tyc17_rs_a,
                                   total=sum(cls_17$`RED SNAPPER_kept`,na.rm=T))$se)

tyc_17_rs_total_a <- as.numeric(predict(tyc17_rs_a,
                                       total=sum(cls_17$`RED SNAPPER_kept`,na.rm=T))$total)
#ty2
ty2.r17_rs_a <- svyratio(~delta_rs,
                        ~reported,
                        design=desi17_all_a,
                        na.rm=T)
ty2_17_rs_se_a <- as.numeric(predict(ty2.r17_rs_a,
                                   total = nrow(cls_17))$se)

ty2_17_rs_total_a <- predict(ty2.r17_rs_a,total = nrow(cls_17))[[1]] + sum(cls_17$`RED SNAPPER_kept`,na.rm=T)
ty2_17_rs_total_a <- as.numeric(ty2_17_rs_total_a)

#tydiff
tydiff.d.17_rs_a <- svytotal(~delta_rs,
                        design=desi17_all_a,
                        na.rm=T)
tydiff_17_rs_se_a <- as.numeric(SE(tydiff.d.17_rs_a))

tydiff_17_rs_total_a <- tydiff.d.17_rs_a[[1]] + sum(cls_17$`RED SNAPPER_kept`,na.rm=T)

#tynew
tynew.nr.17_rs_a <- svytotal(~non_reporter_mrip_claim_rs,
                        design=desi17_all_a,
                        na.rm=T)
tynew_17_rs_se_a <- as.numeric(SE(tynew.nr.17_rs_a))

tynew_17_rs_total_a <- tynew.nr.17_rs_a[[1]] + sum(cls_17$`RED SNAPPER_kept`,na.rm=T)



#Number of Matches
#cutpoint a
number_matches <- mrip_all_17_a %>% 
  filter(reported == 1) %>% 
  nrow()

#tyc interval
#Harvest
#tyc_17_rs_total_a
#PSE
#tyc_17_rs_se_a / tyc_17_rs_total_a

#ty2 interval
#Harvest
#ty2_17_rs_total_a
#PSE
ty2_pse <- ty2_17_rs_se_a / ty2_17_rs_total_a

#tydiff interval
#Harvest
#tydiff_17_rs_total_a
#PSE
#tydiff_17_rs_se_a / tydiff_17_rs_total_a



