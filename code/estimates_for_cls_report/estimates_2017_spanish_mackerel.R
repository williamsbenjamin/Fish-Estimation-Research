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

mrip_all_17$`SPANISH MACKEREL_kept`[is.na(mrip_all_17$`SPANISH MACKEREL_kept`)] <- 0
mrip_all_17 <- mrip_all_17 %>% 
  mutate(delta_sm = `SPANISH MACKEREL_claim` - `SPANISH MACKEREL_kept`) %>%
  mutate(non_reporter_mrip_claim_sm = if_else(reported == 0,
                                           `SPANISH MACKEREL_claim`,as.integer(0)))


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
tyc17_sm <- svyratio(~`SPANISH MACKEREL_claim`,
                      ~`SPANISH MACKEREL_kept`,
                      design=desi17_all,
                      na.rm=T)

tyc_17_sm_se <- as.numeric(predict(tyc17_sm,
                                   total=sum(cls_17$`SPANISH MACKEREL_kept`,na.rm=T))$se)
#tyc_17_sm_se
tyc_17_sm_total <- as.numeric(predict(tyc17_sm,
                                       total=sum(cls_17$`SPANISH MACKEREL_kept`,na.rm=T))$total)
#tyc_17_sm_total

ty2.r17_sm <- svyratio(~delta_sm,
                        ~reported,
                        design=desi17_all,
                        na.rm=T)
ty2_17_sm_se <- as.numeric(predict(ty2.r17_sm,
                                   total = nrow(cls_17))$se)
#ty2_17_sm_se

ty2_17_sm_total <- predict(ty2.r17_sm,total = nrow(cls_17))[[1]] + sum(cls_17$`SPANISH MACKEREL_kept`,na.rm=T)
ty2_17_sm_total <- as.numeric(ty2_17_sm_total)
#ty2_17_sm_total


tydiff.d.17_sm <- svytotal(~delta_sm,
                        design=desi17_all,
                        na.rm=T)
tydiff_17_sm_se <- as.numeric(SE(tydiff.d.17_sm))
#tydiff_17_sm_se
tydiff_17_sm_total <- tydiff.d.17_sm[[1]] + sum(cls_17$`SPANISH MACKEREL_kept`,na.rm=T)
#tydiff_17_sm_total

tynew.nr.17_sm <- svytotal(~non_reporter_mrip_claim_sm,
                        design=desi17_all,
                        na.rm=T)
tynew_17_sm_se <- as.numeric(SE(tynew.nr.17_sm))
#tynew_17_sm_se
tynew_17_sm_total <- tynew.nr.17_sm[[1]] + sum(cls_17$`SPANISH MACKEREL_kept`,na.rm=T)
#tynew_17_sm_total

##
# Record Linkage with cutoff at 13.5
##

mrip_all_17_1 <- read_csv("data/mrip_all_17_cutoff135_all_sites.csv",
                          col_types = cols(psu_id = col_character()))

#make species specific variables for estimation

mrip_all_17_1$`SPANISH MACKEREL_kept`[is.na(mrip_all_17_1$`SPANISH MACKEREL_kept`)] <- 0
mrip_all_17_1 <- mrip_all_17_1 %>% 
  mutate(delta_sm = `SPANISH MACKEREL_claim` - `SPANISH MACKEREL_kept`) %>%
  mutate(non_reporter_mrip_claim_sm = if_else(reported == 0,
                                              `SPANISH MACKEREL_claim`,as.integer(0)))

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
tyc17_sm_cut_13 <- svyratio(~`SPANISH MACKEREL_claim`,
                         ~`SPANISH MACKEREL_kept`,
                         design = desi17_all_1,
                         na.rm = T)

tyc_17_cut_13_sm_se <- as.numeric(predict(tyc17_sm_cut_13,
                                       total=sum(cls_17$`SPANISH MACKEREL_kept`,na.rm=T))$se)
#tyc_17_cut_13_sm_se
tyc_17_cut_13_sm_total <- as.numeric(predict(tyc17_sm_cut_13,
                                          total=sum(cls_17$`SPANISH MACKEREL_kept`,na.rm=T))$total)
#tyc_17_cut_13_sm_total
ty2.r17_sm_cut_13 <- svyratio(~delta_sm,
                           ~reported,
                           design=desi17_all_1,
                           na.rm=T)
ty2_17_cut_13_sm_se <- as.numeric(predict(ty2.r17_sm_cut_13,
                                       total = nrow(cls_17))$se)
#ty2_17_cut_13_sm_se
ty2_17_cut_13_sm_total <- predict(ty2.r17_sm_cut_13,total = nrow(cls_17))[[1]] + sum(cls_17$`SPANISH MACKEREL_kept`,na.rm=T)
ty2_17_cut_13_sm_total <- as.numeric(ty2_17_cut_13_sm_total)
#ty2_17_cut_13_sm_total

tydiff.17_cut_13_sm <- svytotal(~delta_sm,
                             design=desi17_all_1,
                             na.rm=T)
tydiff_17_cut_13_sm_se <- as.numeric(SE(tydiff.17_cut_13_sm))
#tydiff_17_cut_13_sm_se
tydiff_17_cut_13_sm_total <- tydiff.17_cut_13_sm[[1]] + sum(cls_17$`SPANISH MACKEREL_kept`,na.rm=T)
#tydiff_17_cut_13_sm_total

tynew.nr.17_cut_13_sm <- svytotal(~non_reporter_mrip_claim_sm,
                             design=desi17_all_1,
                             na.rm=T)
tynew_17_cut_13_sm_se <- as.numeric(SE(tynew.nr.17_cut_13_sm))
#tynew_17_cut_13_sm_se
tynew_17_cut_13_sm_total <- tynew.nr.17_cut_13_sm[[1]] + sum(cls_17$`SPANISH MACKEREL_kept`,na.rm=T)
#tynew_17_cut_13_sm_total

#ty_hat for sm
tysm <- svydesign(~psu_id,
                  weights = ~wp_int,
                  strata = ~strat_id,
                  nest = T,
                  data = mrip_all_17_1,
                  na.rm = T)
options(survey.lonely.psu = "adjust") 

ty_hat_sm <- svytotal(~`SPANISH MACKEREL_claim`,
                      design = tysm)
#ty*_hat
ty_star_hat_sm <- svytotal(~`SPANISH MACKEREL_kept`,
                           design = tysm)

#t_y*
ty_star_sm <- sum(cls_17$`SPANISH MACKEREL_kept`,na.rm=T)

#n_1 and n_1_hat
n_1 <- nrow(cls_17)

n_1_hat <- svytotal(~reported,
                    design = tyrp)

#Florida Only
mrip_all_17_1 <- read_csv("data/mrip_all_17_cutoff13_all_sites.csv",
                          col_types = cols(psu_id = col_character()))
mrip_all_17_1$`SPANISH MACKEREL_kept`[is.na(mrip_all_17_1$`SPANISH MACKEREL_kept`)] <- 0
mrip_all_17_1 <- mrip_all_17_1 %>% 
  mutate(delta_sm= `SPANISH MACKEREL_claim` - `SPANISH MACKEREL_kept`) %>%
  mutate(non_reporter_mrip_claim_sm= if_else(reported == 0,
                                              `SPANISH MACKEREL_claim`,as.integer(0)))

mrip_all_17_1_fl <- mrip_all_17_1 %>% 
  mutate(spanish_mackerel_claim_fl = if_else(ST == 12,
                                              `SPANISH MACKEREL_claim`,as.integer(0)),
         spanish_mackerel_kept_fl = if_else(ST == 12,
                                             `SPANISH MACKEREL_kept`,0)) %>% 
  mutate(delta_sm_fl = spanish_mackerel_claim_fl - spanish_mackerel_kept_fl) %>% 
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
tyc17_sm_cut_13_fl <- svyratio(~spanish_mackerel_claim_fl,
                               ~spanish_mackerel_kept_fl,
                               design = desi17_all_1_fl,
                               na.rm = T)

tyc_17_cut_13_sm_fl_se <- as.numeric(predict(tyc17_sm_cut_13_fl,
                                             total=sum(cls_17_fl$`SPANISH MACKEREL_kept`,na.rm=T))$se)
#tyc_17_cut_13_sm_se
tyc_17_cut_13_sm_fl_total <- as.numeric(predict(tyc17_sm_cut_13_fl,
                                                total=sum(cls_17_fl$`SPANISH MACKEREL_kept`,na.rm=T))$total)
#tyc_17_cut_13_sm_total
ty2.r17_sm_cut_13_fl <- svyratio(~delta_sm_fl,
                                 ~reported_fl,
                                 design=desi17_all_1_fl,
                                 na.rm=T)
ty2_17_cut_13_sm_fl_se <- as.numeric(predict(ty2.r17_sm_cut_13_fl,
                                             total = nrow(cls_17_fl))$se)
#ty2_17_cut_13_sm_se
ty2_17_cut_13_sm_fl_total <- predict(ty2.r17_sm_cut_13_fl,total = nrow(cls_17_fl))[[1]] + sum(cls_17_fl$`SPANISH MACKEREL_kept`,na.rm=T)
ty2_17_cut_13_sm_fl_total <- as.numeric(ty2_17_cut_13_sm_fl_total)
#ty2_17_cut_13_sm_total

tydiff.17_cut_13_sm_fl <- svytotal(~delta_sm_fl,
                                   design=desi17_all_1_fl,
                                   na.rm=T)
tydiff_17_cut_13_sm_fl_se <- as.numeric(SE(tydiff.17_cut_13_sm_fl))
#tydiff_17_cut_13_sm_se
tydiff_17_cut_13_sm_fl_total <- tydiff.17_cut_13_sm_fl[[1]] + sum(cls_17_fl$`SPANISH MACKEREL_kept`,na.rm=T)
#tydiff_17_cut_13_sm_total

##
# USING RECORD LINKAGE ROWS WITH CUTOFF AT 15
##

mrip_all_17_2 <- read_csv("data/mrip_all_17_cutoff15_all_sites.csv",
                          col_types = cols(psu_id = col_character()))

#make species specific variables for estimation

mrip_all_17_2$`SPANISH MACKEREL_kept`[is.na(mrip_all_17_2$`SPANISH MACKEREL_kept`)] <- 0
mrip_all_17_2 <- mrip_all_17_2 %>% 
  mutate(delta_sm = `SPANISH MACKEREL_claim` - `SPANISH MACKEREL_kept`) %>%
  mutate(non_reporter_mrip_claim_sm = if_else(reported == 0,
                                              `SPANISH MACKEREL_claim`,as.integer(0)))

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
tyc17_sm_cut_15 <- svyratio(~`SPANISH MACKEREL_claim`,
                      ~`SPANISH MACKEREL_kept`,
                      design = desi17_all_2,
                      na.rm = T)

tyc_17_cut_15_sm_se <- as.numeric(predict(tyc17_sm_cut_15,
                                    total=sum(cls_17$`SPANISH MACKEREL_kept`,na.rm=T))$se)
#tyc_17_cut_15_sm_se
tyc_17_cut_15_sm_total <- as.numeric(predict(tyc17_sm_cut_15,
                                       total=sum(cls_17$`SPANISH MACKEREL_kept`,na.rm=T))$total)
#tyc_17_cut_15_sm_total
ty2.r17_sm_cut_15 <- svyratio(~delta_sm,
                        ~reported,
                        design=desi17_all_2,
                        na.rm=T)
ty2_17_cut_15_sm_se <- as.numeric(predict(ty2.r17_sm_cut_15,
                                       total = nrow(cls_17))$se)
#ty2_17_cut_15_sm_se
ty2_17_cut_15_sm_total <- predict(ty2.r17_sm_cut_15,total = nrow(cls_17))[[1]] + sum(cls_17$`SPANISH MACKEREL_kept`,na.rm=T)
ty2_17_cut_15_sm_total <- as.numeric(ty2_17_cut_15_sm_total)
#ty2_17_cut_15_sm_total

tydiff.17_cut_15_sm <- svytotal(~delta_sm,
                             design=desi17_all_2,
                             na.rm=T)
tydiff_17_cut_15_sm_se <- as.numeric(SE(tydiff.17_cut_15_sm))
#tydiff_17_cut_15_sm_se
tydiff_17_cut_15_sm_total <- tydiff.17_cut_15_sm[[1]] + sum(cls_17$`SPANISH MACKEREL_kept`,na.rm=T)
#tydiff_17_cut_15_sm_total

tynew.nr.17_cut_15_sm <- svytotal(~non_reporter_mrip_claim_sm,
                             design=desi17_all_2,
                             na.rm=T)
tynew_17_cut_15_sm_se <- as.numeric(SE(tynew.nr.17_cut_15_sm))
#tynew_17_cut_15_sm_se
tynew_17_cut_15_sm_total <- tynew.nr.17_cut_15_sm[[1]] + sum(cls_17$`SPANISH MACKEREL_kept`,na.rm=T)
#tynew_17_cut_15_sm_total

