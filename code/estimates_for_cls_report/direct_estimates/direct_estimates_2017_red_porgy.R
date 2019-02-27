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

mrip_all_17$`RED PORGY_kept`[is.na(mrip_all_17$`RED PORGY_kept`)] <- 0
mrip_all_17 <- mrip_all_17 %>% 
  mutate(delta_rp = `RED PORGY_claim` - `RED PORGY_kept`) 
#2017
desi17_all <- svydesign(id=~psu_id,
                    weights=~w_int,
                    strata = ~strat_id,
                    nest=T,
                    data=mrip_all_17)
options(survey.lonely.psu = "adjust") 

ty2.r17_rp <- svyratio(~delta_rp,
                        ~reported,
                        design=desi17_all,
                        na.rm=T)
ty2_17_rp_se <- as.numeric(predict(ty2.r17_rp,
                                   total = nrow(cls_17))$se)
#ty2_17_rp_se

ty2_17_rp_total <- predict(ty2.r17_rp,total = nrow(cls_17))[[1]] + sum(cls_17$`RED PORGY_kept`,na.rm=T)
ty2_17_rp_total <- as.numeric(ty2_17_rp_total)

tyrs <- svydesign(~psu_id,
                  weights = ~w_int,
                  strata = ~strat_id,
                  nest = T,
                  data = mrip_all_17)

options(survey.lonely.psu = "adjust") 


ty_hat_rs <- svytotal(~`RED PORGY_claim`,
                      design = tyrs)
#ty*_hat
ty_star_hat_rs <- svytotal(~`RED PORGY_kept`,
                           design = tyrs)

#t_y*
ty_star_rs <- sum(cls_17$`RED PORGY_kept`,na.rm=T)

#n_1 and n_1_hat
n_1 <- nrow(cls_17)

n_1_hat <- svytotal(~reported,
                    design = tyrs)
ty_star_rs + (n_1/n_1_hat)*(ty_hat_rs - ty_star_hat_rs)
#ty2_17_rp_total

######
#Florida Only
######
mrip_all_17_1 <- read_csv("data/mrip_dm_17_all_sites.csv",
                          col_types = cols(psu_id = col_character()))

mrip_all_17_1$`RED PORGY_kept`[is.na(mrip_all_17_1$`RED PORGY_kept`)] <- 0
mrip_all_17_1 <- mrip_all_17_1 %>% 
  mutate(delta_rp = `RED PORGY_claim` - `RED PORGY_kept`) 

mrip_all_17_1_fl <- mrip_all_17_1 %>% 
  mutate(red_porgy_claim_fl = if_else(ST == 12,
                                      `RED PORGY_claim`,as.integer(0)),
         red_porgy_kept_fl = if_else(ST == 12,
                                     `RED PORGY_kept`,0)) %>% 
  mutate(delta_rp_fl = red_porgy_claim_fl - red_porgy_kept_fl) %>% 
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

ty2.r17_rp_cut_13_fl <- svyratio(~delta_rp_fl,
                                 ~reported_fl,
                                 design=desi17_all_1_fl,
                                 na.rm=T)
ty2_17_cut_13_rp_fl_se <- as.numeric(predict(ty2.r17_rp_cut_13_fl,
                                             total = nrow(cls_17_fl))$se)
#ty2_17_cut_13_rp_se
ty2_17_cut_13_rp_fl_total <- predict(ty2.r17_rp_cut_13_fl,total = nrow(cls_17_fl))[[1]] + sum(cls_17_fl$`RED PORGY_kept`,na.rm=T)
ty2_17_cut_13_rp_fl_total <- as.numeric(ty2_17_cut_13_rp_fl_total)

tyrs <- svydesign(~psu_id,
                  weights = ~w_int,
                  strata = ~strat_id,
                  nest = T,
                  data = mrip_all_17_1_fl)

options(survey.lonely.psu = "adjust") 

ty_hat_rs <- svytotal(~red_porgy_claim_fl,
                      design = tyrs)
#ty*_hat
ty_star_hat_rs <- svytotal(~red_porgy_kept_fl,
                           design = tyrs)

#t_y*
ty_star_rs <- sum(cls_17_fl$`RED PORGY_kept`,na.rm=T)

#n_1 and n_1_hat
n_1 <- nrow(cls_17_fl)

n_1_hat <- svytotal(~reported_fl,
                    design = tyrs)
ty_star_rs + (n_1/n_1_hat)*(ty_hat_rs - ty_star_hat_rs)
#########
# ALABAMA
##########
mrip_all_17_2 <- read_csv("data/mrip_dm_17_all_sites.csv",
                          col_types = cols(psu_id = col_character()))

mrip_all_17_2$`RED PORGY_kept`[is.na(mrip_all_17_2$`RED PORGY_kept`)] <- 0
mrip_all_17_2 <- mrip_all_17_2 %>% 
  mutate(delta_rp = `RED PORGY_claim` - `RED PORGY_kept`) 

mrip_all_17_2_al <- mrip_all_17_2 %>% 
  mutate(red_porgy_claim_al = if_else(ST == 1,
                                      `RED PORGY_claim`,as.integer(0)),
         red_porgy_kept_al = if_else(ST == 1,
                                     `RED PORGY_kept`,0)) %>% 
  mutate(delta_rp_al = red_porgy_claim_al - red_porgy_kept_al) %>% 
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

ty2.r17_rp_cut_13_al <- svyratio(~delta_rp_al,
                                 ~reported_al,
                                 design=desi17_all_1_al,
                                 na.rm=T)
ty2_17_cut_13_rp_al_se <- as.numeric(predict(ty2.r17_rp_cut_13_al,
                                             total = nrow(cls_17_al))$se)
#ty2_17_cut_13_rp_se
ty2_17_cut_13_rp_al_total <- predict(ty2.r17_rp_cut_13_al,total = nrow(cls_17_al))[[1]] + sum(cls_17_al$`RED PORGY_kept`,na.rm=T)
ty2_17_cut_13_rp_al_total <- as.numeric(ty2_17_cut_13_rp_al_total)


tyrs <- svydesign(~psu_id,
                  weights = ~w_int,
                  strata = ~strat_id,
                  nest = T,
                  data = mrip_all_17_2_al)

options(survey.lonely.psu = "adjust") 


ty_hat_rs <- svytotal(~red_porgy_claim_al,
                      design = tyrs)
#ty*_hat
ty_star_hat_rs <- svytotal(~red_porgy_kept_al,
                           design = tyrs)

#t_y*
ty_star_rs <- sum(cls_17_al$`RED PORGY_kept`,na.rm=T)

#n_1 and n_1_hat
n_1 <- nrow(cls_17_al)

n_1_hat <- svytotal(~reported_al,
                    design = tyrs)
ty_star_rs + (n_1/n_1_hat)*(ty_hat_rs - ty_star_hat_rs)
