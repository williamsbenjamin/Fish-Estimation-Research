library(tidyverse)
library(ggthemes)
library(lubridate)
library(survey)
library(haven)

#look at both private and pulic
mrip_all_17 <- read_csv("data/mrip_all_17_all_sites.csv",
                        col_types = cols(psu_id = col_character()))
cls_17 <- read_csv("data/cls_17_all_sites.csv")

#make species specific variables for estimation

mrip_all_17$`VERMILION SNAPPER_kept`[is.na(mrip_all_17$`VERMILION SNAPPER_kept`)] <- 0
mrip_all_17 <- mrip_all_17 %>% 
  mutate(delta_vs = `VERMILION SNAPPER_claim` - `VERMILION SNAPPER_kept`) %>%
  mutate(non_reporter_mrip_claim_vs = if_else(reported == 0,
                                           `VERMILION SNAPPER_claim`,as.integer(0)))


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
tyc17_vs <- svyratio(~`VERMILION SNAPPER_claim`,
                      ~`VERMILION SNAPPER_kept`,
                      design=desi17_all,
                      na.rm=T)

tyc_17_vs_se <- as.numeric(predict(tyc17_vs,
                                   total=sum(cls_17$`VERMILION SNAPPER_kept`,na.rm=T))$se)
#tyc_17_vs_se
tyc_17_vs_total <- as.numeric(predict(tyc17_vs,
                                       total=sum(cls_17$`VERMILION SNAPPER_kept`,na.rm=T))$total)
#tyc_17_vs_total

ty2.r17_vs <- svyratio(~delta_vs,
                        ~reported,
                        design=desi17_all,
                        na.rm=T)
ty2_17_vs_se <- as.numeric(predict(ty2.r17_vs,
                                   total = nrow(cls_17))$se)
#ty2_17_vs_se

ty2_17_vs_total <- predict(ty2.r17_vs,total = nrow(cls_17))[[1]] + sum(cls_17$`VERMILION SNAPPER_kept`,na.rm=T)
ty2_17_vs_total <- as.numeric(ty2_17_vs_total)
#ty2_17_vs_total


tydiff.d.17_vs <- svytotal(~delta_vs,
                        design=desi17_all,
                        na.rm=T)
tydiff_17_vs_se <- as.numeric(SE(tydiff.d.17_vs))
#tydiff_17_vs_se
tydiff_17_vs_total <- tydiff.d.17_vs[[1]] + sum(cls_17$`VERMILION SNAPPER_kept`,na.rm=T)
#tydiff_17_vs_total

tynew.nr.17_vs <- svytotal(~non_reporter_mrip_claim_vs,
                        design=desi17_all,
                        na.rm=T)
tynew_17_vs_se <- as.numeric(SE(tynew.nr.17_vs))
#tynew_17_vs_se
tynew_17_vs_total <- tynew.nr.17_vs[[1]] + sum(cls_17$`VERMILION SNAPPER_kept`,na.rm=T)
#tynew_17_vs_total

##
# Record Linkage with cutoff at 13.5
##

mrip_all_17_1 <- read_csv("data/mrip_all_17_cutoff135_all_sites.csv",
                          col_types = cols(psu_id = col_character()))

#make species specific variables for estimation

mrip_all_17_1$`VERMILION SNAPPER_kept`[is.na(mrip_all_17_1$`VERMILION SNAPPER_kept`)] <- 0
mrip_all_17_1 <- mrip_all_17_1 %>% 
  mutate(delta_vs = `VERMILION SNAPPER_claim` - `VERMILION SNAPPER_kept`) %>%
  mutate(non_reporter_mrip_claim_vs = if_else(reported == 0,
                                              `VERMILION SNAPPER_claim`,as.integer(0)))

#2017
desi17_all_1 <- svydesign(id=~psu_id,
                          weights=~wp_int,
                          strata = ~strat_id,
                          nest=T,
                          data=mrip_all_17_1)
options(survey.lonely.psu = "adjust") 
#centers the stratum with only 1 psu
#at population 
#mean for variance estimation
tyc17_vs_cut_13 <- svyratio(~`VERMILION SNAPPER_claim`,
                         ~`VERMILION SNAPPER_kept`,
                         design = desi17_all_1,
                         na.rm = T)

tyc_17_cut_13_vs_se <- as.numeric(predict(tyc17_vs_cut_13,
                                       total=sum(cls_17$`VERMILION SNAPPER_kept`,na.rm=T))$se)
#tyc_17_cut_13_vs_se
tyc_17_cut_13_vs_total <- as.numeric(predict(tyc17_vs_cut_13,
                                          total=sum(cls_17$`VERMILION SNAPPER_kept`,na.rm=T))$total)
#tyc_17_cut_13_vs_total
ty2.r17_vs_cut_13 <- svyratio(~delta_vs,
                           ~reported,
                           design=desi17_all_1,
                           na.rm=T)
ty2_17_cut_13_vs_se <- as.numeric(predict(ty2.r17_vs_cut_13,
                                       total = nrow(cls_17))$se)
#ty2_17_cut_13_vs_se
ty2_17_cut_13_vs_total <- predict(ty2.r17_vs_cut_13,total = nrow(cls_17))[[1]] + sum(cls_17$`VERMILION SNAPPER_kept`,na.rm=T)
ty2_17_cut_13_vs_total <- as.numeric(ty2_17_cut_13_vs_total)
#ty2_17_cut_13_vs_total

tydiff.17_cut_13_vs <- svytotal(~delta_vs,
                             design=desi17_all_1,
                             na.rm=T)
tydiff_17_cut_13_vs_se <- as.numeric(SE(tydiff.17_cut_13_vs))
#tydiff_17_cut_13_vs_se
tydiff_17_cut_13_vs_total <- tydiff.17_cut_13_vs[[1]] + sum(cls_17$`VERMILION SNAPPER_kept`,na.rm=T)
#tydiff_17_cut_13_vs_total

tynew.nr.17_cut_13_vs <- svytotal(~non_reporter_mrip_claim_vs,
                             design=desi17_all_1,
                             na.rm=T)
tynew_17_cut_13_vs_se <- as.numeric(SE(tynew.nr.17_cut_13_vs))
#tynew_17_cut_13_vs_se
tynew_17_cut_13_vs_total <- tynew.nr.17_cut_13_vs[[1]] + sum(cls_17$`VERMILION SNAPPER_kept`,na.rm=T)
#tynew_17_cut_13_rs_total

#ty_hat for vs
tyvs <- svydesign(~psu_id,
                  weights = ~wp_int,
                  strata = ~strat_id,
                  nest = T,
                  data = mrip_all_17_1,
                  na.rm = T)
options(survey.lonely.psu = "adjust") 

ty_hat_vs <- svytotal(~`VERMILION SNAPPER_claim`,
                      design = tyvs)
#ty*_hat
ty_star_hat_vs <- svytotal(~`VERMILION SNAPPER_kept`,
                           design = tyvs)

#t_y*
ty_star_vs <- sum(cls_17$`VERMILION SNAPPER_kept`,na.rm=T)

#n_1 and n_1_hat
n_1 <- nrow(cls_17)

n_1_hat <- svytotal(~reported,
                    design = tyvs)

#Florida Only
mrip_all_17_1 <- read_csv("data/mrip_all_17_cutoff13_all_sites.csv",
                          col_types = cols(psu_id = col_character()))
mrip_all_17_1$`VERMILION SNAPPER_kept`[is.na(mrip_all_17_1$`VERMILION SNAPPER_kept`)] <- 0
mrip_all_17_1 <- mrip_all_17_1 %>% 
  mutate(delta_vs = `VERMILION SNAPPER_claim` - `VERMILION SNAPPER_kept`) %>%
  mutate(non_reporter_mrip_claim_vs = if_else(reported == 0,
                                              `VERMILION SNAPPER_claim`,as.integer(0)))

mrip_all_17_1_fl <- mrip_all_17_1 %>% 
  mutate(VERMILION_snapper_claim_fl = if_else(ST == 12,
                                        `VERMILION SNAPPER_claim`,as.integer(0)),
         VERMILION_snapper_kept_fl = if_else(ST == 12,
                                       `VERMILION SNAPPER_kept`,0)) %>% 
  mutate(delta_vs_fl = VERMILION_snapper_claim_fl - VERMILION_snapper_kept_fl) %>% 
  mutate(reported_fl = if_else(ST == 12,
                               reported,as.integer(0)))

cls_17 <- read_csv("data/cls_17_all_sites.csv")
cls_17_fl <- cls_17 %>%
  filter(state == "FL")

desi17_all_1_fl <- svydesign(id = ~psu_id,
                             weights = ~wp_int,
                             strata = ~strat_id,
                             nest=T,
                             data=mrip_all_17_1_fl)
options(survey.lonely.psu = "adjust") 

#centers the stratum with only 1 psu
#at population 
#mean for variance estimation
tyc17_vs_cut_13_fl <- svyratio(~VERMILION_snapper_claim_fl,
                               ~VERMILION_snapper_kept_fl,
                               design = desi17_all_1_fl,
                               na.rm = T)

tyc_17_cut_13_vs_fl_se <- as.numeric(predict(tyc17_vs_cut_13_fl,
                                             total=sum(cls_17_fl$`VERMILION SNAPPER_kept`,na.rm=T))$se)
#tyc_17_cut_13_vs_se
tyc_17_cut_13_vs_fl_total <- as.numeric(predict(tyc17_vs_cut_13_fl,
                                                total=sum(cls_17_fl$`VERMILION SNAPPER_kept`,na.rm=T))$total)
#tyc_17_cut_13_vs_total
ty2.r17_vs_cut_13_fl <- svyratio(~delta_vs_fl,
                                 ~reported_fl,
                                 design=desi17_all_1_fl,
                                 na.rm=T)
ty2_17_cut_13_vs_fl_se <- as.numeric(predict(ty2.r17_vs_cut_13_fl,
                                             total = nrow(cls_17_fl))$se)
#ty2_17_cut_13_vs_se
ty2_17_cut_13_vs_fl_total <- predict(ty2.r17_vs_cut_13_fl,total = nrow(cls_17_fl))[[1]] + sum(cls_17_fl$`VERMILION SNAPPER_kept`,na.rm=T)
ty2_17_cut_13_vs_fl_total <- as.numeric(ty2_17_cut_13_vs_fl_total)
#ty2_17_cut_13_vs_total

tydiff.17_cut_13_vs_fl <- svytotal(~delta_vs_fl,
                                   design=desi17_all_1_fl,
                                   na.rm=T)
tydiff_17_cut_13_vs_fl_se <- as.numeric(SE(tydiff.17_cut_13_vs_fl))
#tydiff_17_cut_13_vs_se
tydiff_17_cut_13_vs_fl_total <- tydiff.17_cut_13_vs_fl[[1]] + sum(cls_17_fl$`VERMILION SNAPPER_kept`,na.rm=T)
#tydiff_17_cut_13_vs_total

##
# USING RECORD LINKAGE ROWS WITH CUTOFF AT 15
##

mrip_all_17_2 <- read_csv("data/mrip_all_17_cutoff15_all_sites.csv",
                          col_types = cols(psu_id = col_character()))

#make species specific variables for estimation

mrip_all_17_2$`VERMILION SNAPPER_kept`[is.na(mrip_all_17_2$`VERMILION SNAPPER_kept`)] <- 0
mrip_all_17_2 <- mrip_all_17_2 %>% 
  mutate(delta_vs = `VERMILION SNAPPER_claim` - `VERMILION SNAPPER_kept`) %>%
  mutate(non_reporter_mrip_claim_vs = if_else(reported == 0,
                                              `VERMILION SNAPPER_claim`,as.integer(0)))

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
tyc17_vs_cut_15 <- svyratio(~`VERMILION SNAPPER_claim`,
                      ~`VERMILION SNAPPER_kept`,
                      design = desi17_all_2,
                      na.rm = T)

tyc_17_cut_15_vs_se <- as.numeric(predict(tyc17_vs_cut_15,
                                    total=sum(cls_17$`VERMILION SNAPPER_kept`,na.rm=T))$se)
#tyc_17_cut_15_vs_se
tyc_17_cut_15_vs_total <- as.numeric(predict(tyc17_vs_cut_15,
                                       total=sum(cls_17$`VERMILION SNAPPER_kept`,na.rm=T))$total)
#tyc_17_cut_15_vs_total
ty2.r17_vs_cut_15 <- svyratio(~delta_vs,
                        ~reported,
                        design=desi17_all_2,
                        na.rm=T)
ty2_17_cut_15_vs_se <- as.numeric(predict(ty2.r17_vs_cut_15,
                                       total = nrow(cls_17))$se)
#ty2_17_cut_15_vs_se
ty2_17_cut_15_vs_total <- predict(ty2.r17_vs_cut_15,total = nrow(cls_17))[[1]] + sum(cls_17$`VERMILION SNAPPER_kept`,na.rm=T)
ty2_17_cut_15_vs_total <- as.numeric(ty2_17_cut_15_vs_total)
#ty2_17_cut_15_vs_total

tydiff.17_cut_15_vs <- svytotal(~delta_vs,
                             design=desi17_all_2,
                             na.rm=T)
tydiff_17_cut_15_vs_se <- as.numeric(SE(tydiff.17_cut_15_vs))
#tydiff_17_cut_15_vs_se
tydiff_17_cut_15_vs_total <- tydiff.17_cut_15_vs[[1]] + sum(cls_17$`VERMILION SNAPPER_kept`,na.rm=T)
#tydiff_17_cut_15_vs_total

tynew.nr.17_cut_15_vs <- svytotal(~non_reporter_mrip_claim_vs,
                             design=desi17_all_2,
                             na.rm=T)
tynew_17_cut_15_vs_se <- as.numeric(SE(tynew.nr.17_cut_15_vs))
#tynew_17_cut_15_vs_se
tynew_17_cut_15_vs_total <- tynew.nr.17_cut_15_vs[[1]] + sum(cls_17$`VERMILION SNAPPER_kept`,na.rm=T)
#tynew_17_cut_15_vs_total

