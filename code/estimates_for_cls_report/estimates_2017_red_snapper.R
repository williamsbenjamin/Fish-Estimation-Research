library(tidyverse)
library(ggthemes)
library(lubridate)
library(survey)
library(haven)

#look at both private and pulic
cls_17 <- read_csv("data/cls_17_all_sites.csv")

##
# Record Linkage with cutoff at 12
##

mrip_all_17_cut <- read_csv("data/mrip_all_17_cutoff12_all_sites.csv",
                          col_types = cols(psu_id = col_character()))

#make species specific variables for estimation

mrip_all_17_cut$`RED SNAPPER_kept`[is.na(mrip_all_17_cut$`RED SNAPPER_kept`)] <- 0
mrip_all_17_cut <- mrip_all_17_cut %>% 
  mutate(delta_rs = `RED SNAPPER_claim` - `RED SNAPPER_kept`) %>%
  mutate(non_reporter_mrip_claim_rs = if_else(reported == 0,
                                              `RED SNAPPER_claim`,
                                              as.double(0)))


#2017
desi17_all_1 <- svydesign(id = ~psu_id,
                          weights = ~w_int,
                          strata = ~strat_id,
                          nest=T,
                          data=mrip_all_17_cut)
options(survey.lonely.psu = "adjust") 

tyc17_rs_cut_13 <- svyratio(~`RED SNAPPER_claim`,
                            ~`RED SNAPPER_kept`,
                            design = desi17_all_1,
                            na.rm = T)

tyc_17_cut_13_rs_se <- as.numeric(predict(tyc17_rs_cut_13,
                                          total=sum(cls_17$`RED SNAPPER_kept`,na.rm=T))$se)
#tyc_17_cut_13_rs_se
tyc_17_cut_13_rs_total <- as.numeric(predict(tyc17_rs_cut_13,
                                             total=sum(cls_17$`RED SNAPPER_kept`,na.rm=T))$total)
#tyc_17_cut_13_rs_total
ty2.r17_rs_cut_13 <- svyratio(~delta_rs,
                              ~reported,
                              design=desi17_all_1,
                              na.rm=T)
ty2_17_cut_13_rs_se <- as.numeric(predict(ty2.r17_rs_cut_13,
                                          total = nrow(cls_17))$se)
#ty2_17_cut_13_rs_se
ty2_17_cut_13_rs_total <- predict(ty2.r17_rs_cut_13,total = nrow(cls_17))[[1]] + sum(cls_17$`RED SNAPPER_kept`,na.rm=T)
ty2_17_cut_13_rs_total <- as.numeric(ty2_17_cut_13_rs_total)
#ty2_17_cut_13_rs_total

tydiff.17_cut_13_rs <- svytotal(~delta_rs,
                                design=desi17_all_1,
                                na.rm=T)
tydiff_17_cut_13_rs_se <- as.numeric(SE(tydiff.17_cut_13_rs))
#tydiff_17_cut_13_rs_se
tydiff_17_cut_13_rs_total <- tydiff.17_cut_13_rs[[1]] + sum(cls_17$`RED SNAPPER_kept`,na.rm=T)
#tydiff_17_cut_13_rs_total

tynew.nr.17_cut_13_rs <- svytotal(~non_reporter_mrip_claim_rs,
                                  design=desi17_all_1,
                                  na.rm=T)
tynew_17_cut_13_rs_se <- as.numeric(SE(tynew.nr.17_cut_13_rs))
#tynew_17_cut_13_rs_se
tynew_17_cut_13_rs_total <- tynew.nr.17_cut_13_rs[[1]] + sum(cls_17$`RED SNAPPER_kept`,na.rm=T)
#tynew_17_cut_13_rs_total

#
##
# Record Linkage with cutoff at 13.5
##

mrip_all_17_1 <- read_csv("data/mrip_all_17_cutoff135_all_sites.csv",
                          col_types = cols(psu_id = col_character()))

#make species specific variables for estimation

mrip_all_17_1$`RED SNAPPER_kept`[is.na(mrip_all_17_1$`RED SNAPPER_kept`)] <- 0
mrip_all_17_1 <- mrip_all_17_1 %>% 
  mutate(delta_rs = `RED SNAPPER_claim` - `RED SNAPPER_kept`) %>%
  mutate(non_reporter_mrip_claim_rs = if_else(reported == 0,
                                              `RED SNAPPER_claim`,as.integer(0)))


#2017
desi17_all_1 <- svydesign(id = ~psu_id,
                          weights = ~wp_int,
                          strata = ~strat_id,
                          nest=T,
                          data=mrip_all_17_1)
options(survey.lonely.psu = "adjust") 
#centers the stratum with only 1 psu
#at population 
#mean for variance estimation
tyc17_rs_cut_13 <- svyratio(~`RED SNAPPER_claim`,
                         ~`RED SNAPPER_kept`,
                         design = desi17_all_1,
                         na.rm = T)

tyc_17_cut_13_rs_se <- as.numeric(predict(tyc17_rs_cut_13,
                                       total=sum(cls_17$`RED SNAPPER_kept`,na.rm=T))$se)
#tyc_17_cut_13_rs_se
tyc_17_cut_13_rs_total <- as.numeric(predict(tyc17_rs_cut_13,
                                          total=sum(cls_17$`RED SNAPPER_kept`,na.rm=T))$total)
#tyc_17_cut_13_rs_total
ty2.r17_rs_cut_13 <- svyratio(~delta_rs,
                           ~reported,
                           design=desi17_all_1,
                           na.rm=T)
ty2_17_cut_13_rs_se <- as.numeric(predict(ty2.r17_rs_cut_13,
                                       total = nrow(cls_17))$se)
#ty2_17_cut_13_rs_se
ty2_17_cut_13_rs_total <- predict(ty2.r17_rs_cut_13,total = nrow(cls_17))[[1]] + sum(cls_17$`RED SNAPPER_kept`,na.rm=T)
ty2_17_cut_13_rs_total <- as.numeric(ty2_17_cut_13_rs_total)
#ty2_17_cut_13_rs_total

tydiff.17_cut_13_rs <- svytotal(~delta_rs,
                             design=desi17_all_1,
                             na.rm=T)
tydiff_17_cut_13_rs_se <- as.numeric(SE(tydiff.17_cut_13_rs))
#tydiff_17_cut_13_rs_se
tydiff_17_cut_13_rs_total <- tydiff.17_cut_13_rs[[1]] + sum(cls_17$`RED SNAPPER_kept`,na.rm=T)
#tydiff_17_cut_13_rs_total

tynew.nr.17_cut_13_rs <- svytotal(~non_reporter_mrip_claim_rs,
                             design=desi17_all_1,
                             na.rm=T)
tynew_17_cut_13_rs_se <- as.numeric(SE(tynew.nr.17_cut_13_rs))
#tynew_17_cut_13_rs_se
tynew_17_cut_13_rs_total <- tynew.nr.17_cut_13_rs[[1]] + sum(cls_17$`RED SNAPPER_kept`,na.rm=T)
#tynew_17_cut_13_rs_total

#ty_hat for rs

tyrs <- svydesign(~psu_id,
                  weights = ~wp_int,
                  strata = ~strat_id,
                  nest = T,
                  data = mrip_all_17_1)

options(survey.lonely.psu = "adjust") 

ty_hat_rs <- svytotal(~`RED SNAPPER_claim`,
                      design = tyrs)
#ty*_hat
ty_star_hat_rs <- svytotal(~`RED SNAPPER_kept`,
                           design = tyrs)

#t_y*
ty_star_rs <- sum(cls_17$`RED SNAPPER_kept`,na.rm=T)

#n_1 and n_1_hat
n_1 <- nrow(cls_17)

n_1_hat <- svytotal(~reported,
                    design = tyrs)
#matches
mrip_all_17_1 %>% 
  filter(reported == 1) %>% 
  nrow()

#Florida Only
mrip_all_17_1 <- read_csv("data/mrip_all_17_cutoff13_all_sites.csv",
                          col_types = cols(psu_id = col_character()))
mrip_all_17_1$`RED SNAPPER_kept`[is.na(mrip_all_17_1$`RED SNAPPER_kept`)] <- 0
mrip_all_17_1 <- mrip_all_17_1 %>% 
  mutate(delta_rs = `RED SNAPPER_claim` - `RED SNAPPER_kept`) %>%
  mutate(non_reporter_mrip_claim_rs = if_else(reported == 0,
                                              `RED SNAPPER_claim`,as.integer(0)))

mrip_all_17_1_fl <- mrip_all_17_1 %>% 
  mutate(red_snapper_claim_fl = if_else(ST == 12,
                                        `RED SNAPPER_claim`,as.integer(0)),
         red_snapper_kept_fl = if_else(ST == 12,
                                       `RED SNAPPER_kept`,0)) %>% 
  mutate(delta_rs_fl = red_snapper_claim_fl - red_snapper_kept_fl) %>% 
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
tyc17_rs_cut_13_fl <- svyratio(~red_snapper_claim_fl,
                            ~red_snapper_kept_fl,
                            design = desi17_all_1_fl,
                            na.rm = T)

tyc_17_cut_13_rs_fl_se <- as.numeric(predict(tyc17_rs_cut_13_fl,
                                          total=sum(cls_17_fl$`RED SNAPPER_kept`,na.rm=T))$se)
#tyc_17_cut_13_rs_se
tyc_17_cut_13_rs_fl_total <- as.numeric(predict(tyc17_rs_cut_13_fl,
                                             total=sum(cls_17_fl$`RED SNAPPER_kept`,na.rm=T))$total)
#tyc_17_cut_13_rs_total
ty2.r17_rs_cut_13_fl <- svyratio(~delta_rs_fl,
                                ~reported_fl,
                                design=desi17_all_1_fl,
                                na.rm=T)
ty2_17_cut_13_rs_fl_se <- as.numeric(predict(ty2.r17_rs_cut_13_fl,
                                          total = nrow(cls_17_fl))$se)
#ty2_17_cut_13_rs_se
ty2_17_cut_13_rs_fl_total <- predict(ty2.r17_rs_cut_13_fl,total = nrow(cls_17_fl))[[1]] + sum(cls_17_fl$`RED SNAPPER_kept`,na.rm=T)
ty2_17_cut_13_rs_fl_total <- as.numeric(ty2_17_cut_13_rs_fl_total)
#ty2_17_cut_13_rs_total

tydiff.17_cut_13_rs_fl <- svytotal(~delta_rs_fl,
                                design=desi17_all_1_fl,
                                na.rm=T)
tydiff_17_cut_13_rs_fl_se <- as.numeric(SE(tydiff.17_cut_13_rs_fl))
#tydiff_17_cut_13_rs_se
tydiff_17_cut_13_rs_fl_total <- tydiff.17_cut_13_rs_fl[[1]] + sum(cls_17_fl$`RED SNAPPER_kept`,na.rm=T)
#tydiff_17_cut_13_rs_total

##
# USING RECORD LINKAGE ROWS WITH CUTOFF AT 15
##

mrip_all_17_2 <- read_csv("data/mrip_all_17_cutoff15_all_sites.csv",
                          col_types = cols(psu_id = col_character()))

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
tyc17_rs_cut_15 <- svyratio(~`RED SNAPPER_claim`,
                      ~`RED SNAPPER_kept`,
                      design = desi17_all_2,
                      na.rm = T)

tyc_17_cut_15_rs_se <- as.numeric(predict(tyc17_rs_cut_15,
                                    total=sum(cls_17$`RED SNAPPER_kept`,na.rm=T))$se)
#tyc_17_cut_15_rs_se
tyc_17_cut_15_rs_total <- as.numeric(predict(tyc17_rs_cut_15,
                                       total=sum(cls_17$`RED SNAPPER_kept`,na.rm=T))$total)
#tyc_17_cut_15_rs_total
ty2.r17_rs_cut_15 <- svyratio(~delta_rs,
                        ~reported,
                        design=desi17_all_2,
                        na.rm=T)
ty2_17_cut_15_rs_se <- as.numeric(predict(ty2.r17_rs_cut_15,
                                       total = nrow(cls_17))$se)
#ty2_17_cut_15_rs_se
ty2_17_cut_15_rs_total <- predict(ty2.r17_rs_cut_15,total = nrow(cls_17))[[1]] + sum(cls_17$`RED SNAPPER_kept`,na.rm=T)
ty2_17_cut_15_rs_total <- as.numeric(ty2_17_cut_15_rs_total)
#ty2_17_cut_15_rs_total

tydiff.17_cut_15_rs <- svytotal(~delta_rs,
                             design=desi17_all_2,
                             na.rm=T)
tydiff_17_cut_15_rs_se <- as.numeric(SE(tydiff.17_cut_15_rs))
#tydiff_17_cut_15_rs_se
tydiff_17_cut_15_rs_total <- tydiff.17_cut_15_rs[[1]] + sum(cls_17$`RED SNAPPER_kept`,na.rm=T)
#tydiff_17_cut_15_rs_total

tynew.nr.17_cut_15_rs <- svytotal(~non_reporter_mrip_claim_rs,
                             design=desi17_all_2,
                             na.rm=T)
tynew_17_cut_15_rs_se <- as.numeric(SE(tynew.nr.17_cut_15_rs))
#tynew_17_cut_15_rs_se
tynew_17_cut_15_rs_total <- tynew.nr.17_cut_15_rs[[1]] + sum(cls_17$`RED SNAPPER_kept`,na.rm=T)
#tynew_17_cut_15_rs_total

#All RL Rows

#make species specific variables for estimation
mrip_all_17 <- read_csv("data/mrip_all_17_all_sites.csv",
                        col_types = cols(psu_id = col_character()))

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

