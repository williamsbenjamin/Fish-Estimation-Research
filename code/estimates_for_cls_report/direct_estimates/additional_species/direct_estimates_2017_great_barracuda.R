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
cls_17 <- cls_17 %>%
  filter(state == "FL")

mrip_all_17$`GREAT BARRACUDA_kept`[is.na(mrip_all_17$`GREAT BARRACUDA_kept`)] <- 0
mrip_all_17 <- mrip_all_17 %>% 
  mutate(`GREAT BARRACUDA_kept` = as.numeric(`GREAT BARRACUDA_kept`)) %>% 
  mutate(black_grouper_claim_fl = if_else(ST == 12,
                                            `GREAT BARRACUDA_claim`,as.integer(0)),
         black_grouper_kept_fl = if_else(ST == 12,
                                             `GREAT BARRACUDA_kept`,0)) %>% 
  mutate(delta_rs = black_grouper_claim_fl - black_grouper_kept_fl) %>% 
  mutate(reported = if_else(ST == 12,
                            reported,as.integer(0)))

#2017
desi17_all <- svydesign(id=~psu_id,
                    weights=~w_int,
                    strata = ~strat_id,
                    nest=T,
                    data=mrip_all_17)

options(survey.lonely.psu = "adjust") 

#tyc_17_rs_total

ty2.r17_rs <- svyratio(~delta_rs,
                        ~reported,
                        design=desi17_all,
                        na.rm=T)
ty2_17_rs_se <- as.numeric(predict(ty2.r17_rs,
                                   total = nrow(cls_17))$se)
#ty2_17_rs_se

ty2_17_rs_total <- predict(ty2.r17_rs,total = nrow(cls_17))[[1]] +
  sum(cls_17$`GREAT BARRACUDA_kept`,na.rm=T)
ty2_17_rs_total <- as.numeric(ty2_17_rs_total)

#FLORIDA
#Florida Only
mrip_all_17_1 <- read_csv("data/mrip_dm_17_all_sites.csv",
                          col_types = cols(psu_id = col_character()))
mrip_all_17_1$`GREAT BARRACUDA_kept`[is.na(mrip_all_17_1$`GREAT BARRACUDA_kept`)] <- 0
mrip_all_17_1 <- mrip_all_17_1 %>% 
  mutate(delta_rs = `GREAT BARRACUDA_claim` - `GREAT BARRACUDA_kept`)
mrip_all_17_1_fl <- mrip_all_17_1 %>% 
  mutate(red_snapper_claim_fl = if_else(ST == 12,
                                        `GREAT BARRACUDA_claim`,as.integer(0)),
         red_snapper_kept_fl = if_else(ST == 12,
                                       `GREAT BARRACUDA_kept`,as.numeric(0))) %>% 
  mutate(delta_rs_fl = red_snapper_claim_fl - red_snapper_kept_fl) %>% 
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
ty2.r17_rs_cut_13_fl <- svyratio(~delta_rs_fl,
                                 ~reported_fl,
                                 design=desi17_all_1_fl,
                                 na.rm=T)
ty2_17_cut_13_rs_fl_se <- as.numeric(predict(ty2.r17_rs_cut_13_fl,
                                             total = nrow(cls_17_fl))$se)

ty2_17_cut_13_rs_fl_total <- predict(ty2.r17_rs_cut_13_fl,total = nrow(cls_17_fl))[[1]] + sum(cls_17_fl$`GREAT BARRACUDA_kept`,na.rm=T)
ty2_17_cut_13_rs_fl_total <- as.numeric(ty2_17_cut_13_rs_fl_total)

############
#ALABAMA
############

mrip_all_17_2 <- read_csv("data/mrip_dm_17_all_sites.csv",
                          col_types = cols(psu_id = col_character()))
mrip_all_17_2$`GREAT BARRACUDA_kept`[is.na(mrip_all_17_2$`GREAT BARRACUDA_kept`)] <- 0
mrip_all_17_2 <- mrip_all_17_2 %>% 
  mutate(delta_rs = `GREAT BARRACUDA_claim` - `GREAT BARRACUDA_kept`)
mrip_all_17_2_al <- mrip_all_17_2 %>% 
  mutate(red_snapper_claim_al = if_else(ST == 1,
                                        `GREAT BARRACUDA_claim`,as.integer(0)),
         red_snapper_kept_al = if_else(ST == 1,
                                       `GREAT BARRACUDA_kept`,as.numeric(0))) %>% 
  mutate(delta_rs_al = red_snapper_claim_al - red_snapper_kept_al) %>% 
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
ty2.r17_rs_cut_13_al <- svyratio(~delta_rs_al,
                                 ~reported_al,
                                 design=desi17_all_1_al,
                                 na.rm=T)
ty2_17_cut_13_rs_al_se <- as.numeric(predict(ty2.r17_rs_cut_13_al,
                                             total = nrow(cls_17_al))$se)
ty2_17_cut_13_rs_al_total <- predict(ty2.r17_rs_cut_13_al,total = nrow(cls_17_al))[[1]] + sum(cls_17_al$`GREAT BARRACUDA_kept`,na.rm=T)
ty2_17_cut_13_rs_al_total <- as.numeric(ty2_17_cut_13_rs_al_total)
