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

mrip_all_17$`WHITE GRUNT_kept`[is.na(mrip_all_17$`WHITE GRUNT_kept`)] <- 0
mrip_all_17 <- mrip_all_17 %>% 
  mutate(delta_wg = `WHITE GRUNT_claim` - `WHITE GRUNT_kept`) %>%
  mutate(non_reporter_mrip_claim_wg = if_else(reported == 0,
                                           `WHITE GRUNT_claim`,as.integer(0)))


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
tyc17_wg <- svyratio(~`WHITE GRUNT_claim`,
                      ~`WHITE GRUNT_kept`,
                      design=desi17_all,
                      na.rm=T)

tyc_17_wg_se <- as.numeric(predict(tyc17_wg,
                                   total=sum(cls_17$`WHITE GRUNT_kept`,na.rm=T))$se)
#tyc_17_wg_se
tyc_17_wg_total <- as.numeric(predict(tyc17_wg,
                                       total=sum(cls_17$`WHITE GRUNT_kept`,na.rm=T))$total)
#tyc_17_wg_total

ty2.r17_wg <- svyratio(~delta_wg,
                        ~reported,
                        design=desi17_all,
                        na.rm=T)
ty2_17_wg_se <- as.numeric(predict(ty2.r17_wg,
                                   total = nrow(cls_17))$se)
#ty2_17_wg_se

ty2_17_wg_total <- predict(ty2.r17_wg,total = nrow(cls_17))[[1]] + sum(cls_17$`WHITE GRUNT_kept`,na.rm=T)
ty2_17_wg_total <- as.numeric(ty2_17_wg_total)
#ty2_17_wg_total


tydiff.d.17_wg <- svytotal(~delta_wg,
                        design=desi17_all,
                        na.rm=T)
tydiff_17_wg_se <- as.numeric(SE(tydiff.d.17_wg))
#tydiff_17_wg_se
tydiff_17_wg_total <- tydiff.d.17_wg[[1]] + sum(cls_17$`WHITE GRUNT_kept`,na.rm=T)
#tydiff_17_wg_total

tynew.nr.17_wg <- svytotal(~non_reporter_mrip_claim_wg,
                        design=desi17_all,
                        na.rm=T)
tynew_17_wg_se <- as.numeric(SE(tynew.nr.17_wg))
#tynew_17_wg_se
tynew_17_wg_total <- tynew.nr.17_wg[[1]] + sum(cls_17$`WHITE GRUNT_kept`,na.rm=T)
#tynew_17_wg_total

##
# Record Linkage with cutoff at 13
##

mrip_all_17_1 <- read_csv("data/mrip_all_17_cutoff13_all_sites.csv",
                          col_types = cols(psu_id = col_character()))

#make species specific variables for estimation

mrip_all_17_1$`WHITE GRUNT_kept`[is.na(mrip_all_17_1$`WHITE GRUNT_kept`)] <- 0
mrip_all_17_1 <- mrip_all_17_1 %>% 
  mutate(delta_wg = `WHITE GRUNT_claim` - `WHITE GRUNT_kept`) %>%
  mutate(non_reporter_mrip_claim_wg = if_else(reported == 0,
                                              `WHITE GRUNT_claim`,as.integer(0)))

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
tyc17_wg_cut_13 <- svyratio(~`WHITE GRUNT_claim`,
                         ~`WHITE GRUNT_kept`,
                         design = desi17_all_1,
                         na.rm = T)

tyc_17_cut_13_wg_se <- as.numeric(predict(tyc17_wg_cut_13,
                                       total=sum(cls_17$`WHITE GRUNT_kept`,na.rm=T))$se)
#tyc_17_cut_13_wg_se
tyc_17_cut_13_wg_total <- as.numeric(predict(tyc17_wg_cut_13,
                                          total=sum(cls_17$`WHITE GRUNT_kept`,na.rm=T))$total)
#tyc_17_cut_13_wg_total
ty2.r17_wg_cut_13 <- svyratio(~delta_wg,
                           ~reported,
                           design=desi17_all_1,
                           na.rm=T)
ty2_17_cut_13_wg_se <- as.numeric(predict(ty2.r17_wg_cut_13,
                                       total = nrow(cls_17))$se)
#ty2_17_cut_13_wg_se
ty2_17_cut_13_wg_total <- predict(ty2.r17_wg_cut_13,total = nrow(cls_17))[[1]] + sum(cls_17$`WHITE GRUNT_kept`,na.rm=T)
ty2_17_cut_13_wg_total <- as.numeric(ty2_17_cut_13_wg_total)
#ty2_17_cut_13_wg_total

tydiff.17_cut_13_wg <- svytotal(~delta_wg,
                             design=desi17_all_1,
                             na.rm=T)
tydiff_17_cut_13_wg_se <- as.numeric(SE(tydiff.17_cut_13_wg))
#tydiff_17_cut_13_wg_se
tydiff_17_cut_13_wg_total <- tydiff.17_cut_13_wg[[1]] + sum(cls_17$`WHITE GRUNT_kept`,na.rm=T)
#tydiff_17_cut_13_wg_total

tynew.nr.17_cut_13_wg <- svytotal(~non_reporter_mrip_claim_wg,
                             design=desi17_all_1,
                             na.rm=T)
tynew_17_cut_13_wg_se <- as.numeric(SE(tynew.nr.17_cut_13_wg))
#tynew_17_cut_13_wg_se
tynew_17_cut_13_wg_total <- tynew.nr.17_cut_13_wg[[1]] + sum(cls_17$`WHITE GRUNT_kept`,na.rm=T)
#tynew_17_cut_13_wg_total

##Estimate pieces of the estimators

#ty_hat for wg
tywg <- svydesign(~psu_id,
                  weights = ~wp_int,
                  strata = ~strat_id,
                  nest = T,
                  data = mrip_all_17_1)
options(survey.lonely.psu = "adjust") 

ty_hat_wg <- svytotal(~`WHITE GRUNT_claim`,
                      design = tywg)
#ty*_hat
ty_star_hat_wg <- svytotal(~`WHITE GRUNT_kept`,
                           design = tywg)

#t_y*
ty_star_wg <- sum(cls_17$`WHITE GRUNT_kept`,na.rm=T)

#n_1 and n_1_hat
n_1 <- nrow(cls_17)

n_1_hat <- svytotal(~reported,
                    design = tyrp)

#Florida Only
mrip_all_17_1 <- read_csv("data/mrip_all_17_cutoff13_all_sites.csv",
                          col_types = cols(psu_id = col_character()))
mrip_all_17_1$`WHITE GRUNT_kept`[is.na(mrip_all_17_1$`WHITE GRUNT_kept`)] <- 0
mrip_all_17_1 <- mrip_all_17_1 %>% 
  mutate(delta_wg= `WHITE GRUNT_claim` - `WHITE GRUNT_kept`) %>%
  mutate(non_reporter_mrip_claim_wg= if_else(reported == 0,
                                             `WHITE GRUNT_claim`,as.integer(0)))

mrip_all_17_1_fl <- mrip_all_17_1 %>% 
  mutate(spanish_mackerel_claim_fl = if_else(ST == 12,
                                             `WHITE GRUNT_claim`,as.integer(0)),
         spanish_mackerel_kept_fl = if_else(ST == 12,
                                            `WHITE GRUNT_kept`,0)) %>% 
  mutate(delta_wg_fl = spanish_mackerel_claim_fl - spanish_mackerel_kept_fl) %>% 
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
tyc17_wg_cut_13_fl <- svyratio(~spanish_mackerel_claim_fl,
                               ~spanish_mackerel_kept_fl,
                               design = desi17_all_1_fl,
                               na.rm = T)

tyc_17_cut_13_wg_fl_se <- as.numeric(predict(tyc17_wg_cut_13_fl,
                                             total=sum(cls_17_fl$`WHITE GRUNT_kept`,na.rm=T))$se)
#tyc_17_cut_13_wg_se
tyc_17_cut_13_wg_fl_total <- as.numeric(predict(tyc17_wg_cut_13_fl,
                                                total=sum(cls_17_fl$`WHITE GRUNT_kept`,na.rm=T))$total)
#tyc_17_cut_13_wg_total
ty2.r17_wg_cut_13_fl <- svyratio(~delta_wg_fl,
                                 ~reported_fl,
                                 design=desi17_all_1_fl,
                                 na.rm=T)
ty2_17_cut_13_wg_fl_se <- as.numeric(predict(ty2.r17_wg_cut_13_fl,
                                             total = nrow(cls_17_fl))$se)
#ty2_17_cut_13_wg_se
ty2_17_cut_13_wg_fl_total <- predict(ty2.r17_wg_cut_13_fl,total = nrow(cls_17_fl))[[1]] + sum(cls_17_fl$`WHITE GRUNT_kept`,na.rm=T)
ty2_17_cut_13_wg_fl_total <- as.numeric(ty2_17_cut_13_wg_fl_total)
#ty2_17_cut_13_wg_total

tydiff.17_cut_13_wg_fl <- svytotal(~delta_wg_fl,
                                   design=desi17_all_1_fl,
                                   na.rm=T)
tydiff_17_cut_13_wg_fl_se <- as.numeric(SE(tydiff.17_cut_13_wg_fl))
#tydiff_17_cut_13_wg_se
tydiff_17_cut_13_wg_fl_total <- tydiff.17_cut_13_wg_fl[[1]] + sum(cls_17_fl$`WHITE GRUNT_kept`,na.rm=T)
#tydiff_17_cut_13_wg_total

##Estimate pieces of the estimators

#ty_hat for wg
tywg <- svydesign(~psu_id,
                  weights = ~wp_int,
                  strata = ~strat_id,
                  nest = T,
                  data = mrip_all_17_1_fl)
options(survey.lonely.psu = "adjust") 

ty_hat_wg <- svytotal(~`WHITE GRUNT_claim`,
                      design = tywg)
#ty*_hat
ty_star_hat_wg <- svytotal(~`WHITE GRUNT_kept`,
                           design = tywg)

#t_y*
ty_star_wg <- sum(cls_17$`WHITE GRUNT_kept`,na.rm=T)

#n_1 and n_1_hat
n_1 <- nrow(cls_17)

n_1_hat <- svytotal(~reported,
                    design = tywg)


##
# USING RECORD LINKAGE ROWS WITH CUTOFF AT 15
##

mrip_all_17_2 <- read_csv("data/mrip_all_17_cutoff15_all_sites.csv",
                          col_types = cols(psu_id = col_character()))

#make species specific variables for estimation

mrip_all_17_2$`WHITE GRUNT_kept`[is.na(mrip_all_17_2$`WHITE GRUNT_kept`)] <- 0
mrip_all_17_2 <- mrip_all_17_2 %>% 
  mutate(delta_wg = `WHITE GRUNT_claim` - `WHITE GRUNT_kept`) %>%
  mutate(non_reporter_mrip_claim_wg = if_else(reported == 0,
                                              `WHITE GRUNT_claim`,as.integer(0)))

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
tyc17_wg_cut_15 <- svyratio(~`WHITE GRUNT_claim`,
                      ~`WHITE GRUNT_kept`,
                      design = desi17_all_2,
                      na.rm = T)

tyc_17_cut_15_wg_se <- as.numeric(predict(tyc17_wg_cut_15,
                                    total=sum(cls_17$`WHITE GRUNT_kept`,na.rm=T))$se)
#tyc_17_cut_15_wg_se
tyc_17_cut_15_wg_total <- as.numeric(predict(tyc17_wg_cut_15,
                                       total=sum(cls_17$`WHITE GRUNT_kept`,na.rm=T))$total)
#tyc_17_cut_15_wg_total
ty2.r17_wg_cut_15 <- svyratio(~delta_wg,
                        ~reported,
                        design=desi17_all_2,
                        na.rm=T)
ty2_17_cut_15_wg_se <- as.numeric(predict(ty2.r17_wg_cut_15,
                                       total = nrow(cls_17))$se)
#ty2_17_cut_15_wg_se
ty2_17_cut_15_wg_total <- predict(ty2.r17_wg_cut_15,total = nrow(cls_17))[[1]] + sum(cls_17$`WHITE GRUNT_kept`,na.rm=T)
ty2_17_cut_15_wg_total <- as.numeric(ty2_17_cut_15_wg_total)
#ty2_17_cut_15_wg_total

tydiff.17_cut_15_wg <- svytotal(~delta_wg,
                             design=desi17_all_2,
                             na.rm=T)
tydiff_17_cut_15_wg_se <- as.numeric(SE(tydiff.17_cut_15_wg))
#tydiff_17_cut_15_wg_se
tydiff_17_cut_15_wg_total <- tydiff.17_cut_15_wg[[1]] + sum(cls_17$`WHITE GRUNT_kept`,na.rm=T)
#tydiff_17_cut_15_wg_total

tynew.nr.17_cut_15_wg <- svytotal(~non_reporter_mrip_claim_wg,
                             design=desi17_all_2,
                             na.rm=T)
tynew_17_cut_15_wg_se <- as.numeric(SE(tynew.nr.17_cut_15_wg))
#tynew_17_cut_15_wg_se
tynew_17_cut_15_wg_total <- tynew.nr.17_cut_15_wg[[1]] + sum(cls_17$`WHITE GRUNT_kept`,na.rm=T)
#tynew_17_cut_15_wg_total

