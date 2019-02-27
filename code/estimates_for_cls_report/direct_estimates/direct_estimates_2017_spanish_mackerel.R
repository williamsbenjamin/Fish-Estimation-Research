library(tidyverse)
library(ggthemes)
library(lubridate)
library(survey)
library(haven)

#look at both private and pulic
mrip_all_17 <- read_csv("data/mrip_dm_17_all_sites.csv",
                        col_types = cols(psu_id = col_character()))
cls_17 <- read_csv("data/cls_17_all_sites.csv")

#make species specific variables for estimation

mrip_all_17$`SPANISH MACKEREL_kept`[is.na(mrip_all_17$`SPANISH MACKEREL_kept`)] <- 0
mrip_all_17 <- mrip_all_17 %>% 
  mutate(delta_sm = `SPANISH MACKEREL_claim` - `SPANISH MACKEREL_kept`) 

#2017
desi17_all <- svydesign(id=~psu_id,
                    weights=~w_int,
                    strata = ~strat_id,
                    nest=T,
                    data=mrip_all_17)
options(survey.lonely.psu = "adjust") 


ty2.r17_sm <- svyratio(~delta_sm,
                        ~reported,
                        design=desi17_all,
                        na.rm=T)
ty2_17_sm_se <- as.numeric(predict(ty2.r17_sm,
                                   total = nrow(cls_17))$se)

ty2_17_sm_total <- predict(ty2.r17_sm,total = nrow(cls_17))[[1]] + sum(cls_17$`SPANISH MACKEREL_kept`,na.rm=T)
ty2_17_sm_total <- as.numeric(ty2_17_sm_total)

#ty_hat for sm
tysm <- svydesign(~psu_id,
                  weights = ~w_int,
                  strata = ~strat_id,
                  nest = T,
                  data = mrip_all_17,
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
                    design = tysm)
ty_star_sm + (n_1/n_1_hat)*(ty_hat_sm - ty_star_hat_sm)

######
#Florida Only
######
mrip_all_17_1 <- read_csv("data/mrip_dm_17_all_sites.csv",
                          col_types = cols(psu_id = col_character()))
mrip_all_17_1$`SPANISH MACKEREL_kept`[is.na(mrip_all_17_1$`SPANISH MACKEREL_kept`)] <- 0
mrip_all_17_1 <- mrip_all_17_1 %>% 
  mutate(delta_sm= `SPANISH MACKEREL_claim` - `SPANISH MACKEREL_kept`) %>%
  mutate(non_reporter_mrip_claim_sm= if_else(reported == 0,
                                              `SPANISH MACKEREL_claim`,as.numeric(0)))

mrip_all_17_1_fl <- mrip_all_17_1 %>% 
  mutate(spanish_mackerel_claim_fl = if_else(ST == 12,
                                              `SPANISH MACKEREL_claim`,as.numeric(0)),
         spanish_mackerel_kept_fl = if_else(ST == 12,
                                             `SPANISH MACKEREL_kept`,as.numeric(0))) %>% 
  mutate(delta_sm_fl = spanish_mackerel_claim_fl - spanish_mackerel_kept_fl) %>% 
  mutate(reported_fl = if_else(ST == 12,
                               reported,as.integer(0)))

cls_17 <- read_csv("data/cls_17_all_sites.csv")
cls_17_fl <- cls_17 %>%
  filter(state == "FL")

desi17_all_1_fl <- svydesign(id = ~psu_id,
                             weights = ~w_int,
                             strata = ~strat_id,
                             nest=T,
                             data=mrip_all_17_1_fl)
options(survey.lonely.psu = "adjust") 

ty2.r17_sm_cut_13_fl <- svyratio(~delta_sm_fl,
                                 ~reported_fl,
                                 design=desi17_all_1_fl,
                                 na.rm=T)
ty2_17_cut_13_sm_fl_se <- as.numeric(predict(ty2.r17_sm_cut_13_fl,
                                             total = nrow(cls_17_fl))$se)
#ty2_17_cut_13_sm_se
ty2_17_cut_13_sm_fl_total <- predict(ty2.r17_sm_cut_13_fl,total = nrow(cls_17_fl))[[1]] + sum(cls_17_fl$`SPANISH MACKEREL_kept`,na.rm=T)
ty2_17_cut_13_sm_fl_total <- as.numeric(ty2_17_cut_13_sm_fl_total)

#ty_hat for sm
tysm <- svydesign(~psu_id,
                  weights = ~w_int,
                  strata = ~strat_id,
                  nest = T,
                  data = mrip_all_17_1_fl,
                  na.rm = T)
options(survey.lonely.psu = "adjust") 

ty_hat_sm <- svytotal(~spanish_mackerel_claim_fl,
                      design = tysm)
#ty*_hat
ty_star_hat_sm <- svytotal(~spanish_mackerel_kept_fl,
                           design = tysm)

#t_y*
ty_star_sm <- sum(cls_17_fl$`SPANISH MACKEREL_kept`,na.rm=T)

#n_1 and n_1_hat
n_1 <- nrow(cls_17_fl)

n_1_hat <- svytotal(~reported_fl,
                    design = tysm)
ty_star_sm + (n_1/n_1_hat)*(ty_hat_sm - ty_star_hat_sm)
#########
## Alabama
#########
mrip_all_17_2 <- read_csv("data/mrip_dm_17_all_sites.csv",
                          col_types = cols(psu_id = col_character()))
mrip_all_17_2$`SPANISH MACKEREL_kept`[is.na(mrip_all_17_2$`SPANISH MACKEREL_kept`)] <- 0
mrip_all_17_2 <- mrip_all_17_2 %>% 
  mutate(delta_sm= `SPANISH MACKEREL_claim` - `SPANISH MACKEREL_kept`) %>%
  mutate(non_reporter_mrip_claim_sm= if_else(reported == 0,
                                             `SPANISH MACKEREL_claim`,as.integer(0)))

mrip_all_17_2_al <- mrip_all_17_2 %>% 
  mutate(spanish_mackerel_claim_al = if_else(ST == 1,
                                             `SPANISH MACKEREL_claim`,as.numeric(0)),
         spanish_mackerel_kept_al = if_else(ST == 1,
                                            `SPANISH MACKEREL_kept`,as.numeric(0))) %>% 
  mutate(delta_sm_al = spanish_mackerel_claim_al - spanish_mackerel_kept_al) %>% 
  mutate(reported_al = if_else(ST == 1,
                               reported,as.integer(0)))

cls_17 <- read_csv("data/cls_17_all_sites.csv")
cls_17_al <- cls_17 %>%
  filter(state == "AL")

desi17_all_1_al <- svydesign(id = ~psu_id,
                             weights = ~w_int,
                             strata = ~strat_id,
                             nest=T,
                             data=mrip_all_17_2_al)
options(survey.lonely.psu = "adjust") 

ty2.r17_sm_cut_13_al <- svyratio(~delta_sm_al,
                                 ~reported_al,
                                 design=desi17_all_1_al,
                                 na.rm=T)
ty2_17_cut_13_sm_al_se <- as.numeric(predict(ty2.r17_sm_cut_13_al,
                                             total = nrow(cls_17_al))$se)
#ty2_17_cut_13_sm_se
ty2_17_cut_13_sm_al_total <- predict(ty2.r17_sm_cut_13_al,total = nrow(cls_17_al))[[1]] + sum(cls_17_al$`SPANISH MACKEREL_kept`,na.rm=T)
ty2_17_cut_13_sm_al_total <- as.numeric(ty2_17_cut_13_sm_al_total)

#ty_hat for sm
tysm <- svydesign(~psu_id,
                  weights = ~wp_int,
                  strata = ~strat_id,
                  nest = T,
                  data = mrip_all_17_2_al,
                  na.rm = T)
options(survey.lonely.psu = "adjust") 

ty_hat_sm <- svytotal(~spanish_mackerel_claim_al,
                      design = tysm)
#ty*_hat
ty_star_hat_sm <- svytotal(~spanish_mackerel_kept_al,
                           design = tysm)

#t_y*
ty_star_sm <- sum(cls_17_al$`SPANISH MACKEREL_kept`,na.rm=T)

#n_1 and n_1_hat
n_1 <- nrow(cls_17_al)

n_1_hat <- svytotal(~reported_al,
                    design = tysm)
ty_star_sm + (n_1/n_1_hat)*(ty_hat_sm - ty_star_hat_sm)
