library(tidyverse)
library(ggthemes)
library(lubridate)
library(survey)
library(haven)

#look at both private and pulic
mrip_all_17 <- read_csv("data/mrip_all_17_all_sites.csv")
cls_17 <- read_csv("data/cls_17_all_sites.csv")

#make species specific variables for estimation

mrip_all_17$`RED SNAPPER_kept`[is.na(mrip_all_17$`RED SNAPPER_kept`)] <- 0
mrip_all_17 <- mrip_all_17 %>% 
  mutate(delta_rs = `RED SNAPPER_claim` - `RED SNAPPER_kept`) %>%
  mutate(non_reporter_mrip_claim_rs = if_else(reported == 0,
                                           `RED SNAPPER_claim`,as.integer(0)))


####USING ALL RECORD LINKAGE ROWS
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
tyc17_rs <- svyratio(~`RED SNAPPER_claim`,
                      ~`RED SNAPPER_kept`,
                      design=desi17_all,
                      na.rm=T)

tyc_17_rs_se <- as.numeric(predict(tyc17_rs,
                                   total=sum(cls_17$`RED SNAPPER_kept`,na.rm=T))$se)
#tyc_17_rs_se
tyc_17_rs_total <- as.numeric(predict(tyc17_rs,
                                       total=sum(cls_17$`RED SNAPPER_kept`,na.rm=T))$total)
#tyc_17_rs_total

ty2.r17_rs <- svyratio(~delta_rs,
                        ~reported,
                        design=desi17_all,
                        na.rm=T)
ty2_17_rs_se <- as.numeric(predict(ty2.r17_rs,
                                   total = nrow(cls_17))$se)
#ty2_17_rs_se

ty2_17_rs_total <- predict(ty2.r17_rs,total = nrow(cls_17))[[1]] + sum(cls_17$`RED SNAPPER_kept`,na.rm=T)
ty2_17_rs_total <- as.numeric(ty2_17_rs_total)
#ty2_17_rs_total


tydiff.d.17_rs <- svytotal(~delta_rs,
                        design=desi17_all,
                        na.rm=T)
tydiff_17_rs_se <- as.numeric(SE(tydiff.d.17_rs))
#tydiff_17_all_se
tydiff_17_rs_total <- tydiff.d.17_rs[[1]] + sum(cls_17$`RED SNAPPER_kept`,na.rm=T)
#tydiff_17_all_total

tynew.nr.17_rs <- svytotal(~non_reporter_mrip_claim_rs,
                        design=desi17_all,
                        na.rm=T)
tynew_17_rs_se <- as.numeric(SE(tynew.nr.17_rs))
#tynew_17_rs_se
tynew_17_rs_total <- tynew.nr.17_rs[[1]] + sum(cls_17$`RED SNAPPER_kept`,na.rm=T)
#tynew_17_rs_total

# USING RECORD LINKAGE ROWS WITH CUTOFF AT 15

mrip_all_17_2 <- read_csv("data/mrip_all_17_cutoff_all_sites.csv")

#make species specific variables for estimation

mrip_all_17_2$`RED SNAPPER_kept`[is.na(mrip_all_17_2$`RED SNAPPER_kept`)] <- 0
mrip_all_17_2 <- mrip_all_17_2 %>% 
  mutate(delta_rs = `RED SNAPPER_claim` - `RED SNAPPER_kept`) %>%
  mutate(non_reporter_mrip_claim_rs = if_else(reported == 0,
                                              `RED SNAPPER_claim`,as.integer(0)))

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
tyc17_rs_cut <- svyratio(~`RED SNAPPER_claim`,
                      ~`RED SNAPPER_kept`,
                      design = desi17_all_2,
                      na.rm = T)

tyc_17_cut_rs_se <- as.numeric(predict(tyc17_rs_cut,
                                    total=sum(cls_17$`RED SNAPPER_kept`,na.rm=T))$se)
#tyc_17_cut_se
tyc_17_cut_rs_total <- as.numeric(predict(tyc17_rs_cut,
                                       total=sum(cls_17$`RED SNAPPER_kept`,na.rm=T))$total)
#tyc_17_cut_total
ty2.r17_rs_cut <- svyratio(~delta_rs,
                        ~reported,
                        design=desi17_all_2,
                        na.rm=T)
ty2_17_cut_rs_se <- as.numeric(predict(ty2.r17_rs_cut,
                                       total = nrow(cls_17))$se)
#ty2_17_cut_rs_se
ty2_17_cut_rs_total <- predict(ty2.r17_rs_cut,total = nrow(cls_17))[[1]] + sum(cls_17$`RED SNAPPER_kept`,na.rm=T)
ty2_17_cut_rs_total <- as.numeric(ty2_17_cut_rs_total)
#ty2_17_cut_rs_total

tydiff.d.2.17_rs <- svytotal(~delta_rs,
                             design=desi17_all_2,
                             na.rm=T)
tydiff_17_cut_rs_se <- as.numeric(SE(tydiff.d.2.17_rs))
#tydiff_17_cut_rs_se
tydiff_17_cut_rs_total <- tydiff.d.2.17_rs[[1]] + sum(cls_17$`RED SNAPPER_kept`,na.rm=T)
#tydiff_17_cut_total

tynew.nr.2.17_rs <- svytotal(~non_reporter_mrip_claim_rs,
                             design=desi17_all_2,
                             na.rm=T)
tynew_17_cut_rs_se <- as.numeric(SE(tynew.nr.2.17_rs))
#tynew_17_cut_se
tynew_17_cut_total <- tynew.nr.2.17_rs[[1]] + sum(cls_17$`RED SNAPPER_kept`,na.rm=T)
#tynew_17_cut_total

