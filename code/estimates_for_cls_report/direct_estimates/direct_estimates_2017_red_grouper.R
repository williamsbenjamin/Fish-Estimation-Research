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

mrip_all_17$`RED GROUPER_kept`[is.na(mrip_all_17$`RED GROUPER_kept`)] <- 0
mrip_all_17 <- mrip_all_17 %>% 
  mutate(delta_rg = `RED GROUPER_claim` - `RED GROUPER_kept`) 

#2017
desi17_all <- svydesign(id=~psu_id,
                    weights=~w_int,
                    strata = ~strat_id,
                    nest=T,
                    data=mrip_all_17)
options(survey.lonely.psu = "adjust")

ty2.r17_rg <- svyratio(~delta_rg,
                        ~reported,
                        design=desi17_all,
                        na.rm=T)
ty2_17_rg_se <- as.numeric(predict(ty2.r17_rg,
                                   total = nrow(cls_17))$se)
#ty2_17_rg_se

ty2_17_rg_total <- predict(ty2.r17_rg,total = nrow(cls_17))[[1]] + 
  sum(cls_17$`RED GROUPER_kept`,na.rm=T)
ty2_17_rg_total <- as.numeric(ty2_17_rg_total)

########
#Florida Only
#######
mrip_all_17_1 <- read_csv("data/mrip_dm_17_all_sites.csv",
                          col_types = cols(psu_id = col_character()))

mrip_all_17_1$`RED GROUPER_kept`[is.na(mrip_all_17_1$`RED GROUPER_kept`)] <- 0
mrip_all_17_1 <- mrip_all_17_1 %>% 
  mutate(delta_rg = `RED GROUPER_claim` - `RED GROUPER_kept`) 

mrip_all_17_1_fl <- mrip_all_17_1 %>% 
  mutate(red_GROUPER_claim_fl = if_else(ST == 12,
                                      `RED GROUPER_claim`,as.integer(0)),
         red_GROUPER_kept_fl = if_else(ST == 12,
                                     `RED GROUPER_kept`,0)) %>% 
  mutate(delta_rg_fl = red_GROUPER_claim_fl - red_GROUPER_kept_fl) %>% 
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

ty2.r17_rg_cut_13_fl <- svyratio(~delta_rg_fl,
                                 ~reported_fl,
                                 design=desi17_all_1_fl,
                                 na.rm=T)
ty2_17_cut_13_rg_fl_se <- as.numeric(predict(ty2.r17_rg_cut_13_fl,
                                             total = nrow(cls_17_fl))$se)
#ty2_17_cut_13_rg_se
ty2_17_cut_13_rg_fl_total <- predict(ty2.r17_rg_cut_13_fl,total = nrow(cls_17_fl))[[1]] + sum(cls_17_fl$`RED GROUPER_kept`,na.rm=T)
ty2_17_cut_13_rg_fl_total <- as.numeric(ty2_17_cut_13_rg_fl_total)

#######
##ALABAMA
#######
mrip_all_17_2 <- read_csv("data/mrip_dm_17_all_sites.csv",
                          col_types = cols(psu_id = col_character()))

mrip_all_17_2$`RED GROUPER_kept`[is.na(mrip_all_17_2$`RED GROUPER_kept`)] <- 0
mrip_all_17_2 <- mrip_all_17_2 %>% 
  mutate(delta_rg = `RED GROUPER_claim` - `RED GROUPER_kept`) 

mrip_all_17_2_al <- mrip_all_17_2 %>% 
  mutate(red_GROUPER_claim_al = if_else(ST == 1,
                                        `RED GROUPER_claim`,as.integer(0)),
         red_GROUPER_kept_al = if_else(ST == 1,
                                       `RED GROUPER_kept`,0)) %>% 
  mutate(delta_rg_al = red_GROUPER_claim_al - red_GROUPER_kept_al) %>% 
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

ty2.r17_rg_cut_13_al <- svyratio(~delta_rg_al,
                                 ~reported_al,
                                 design=desi17_all_1_al,
                                 na.rm=T)
ty2_17_cut_13_rg_al_se <- as.numeric(predict(ty2.r17_rg_cut_13_al,
                                             total = nrow(cls_17_al))$se)
#ty2_17_cut_13_rg_se
ty2_17_cut_13_rg_al_total <- predict(ty2.r17_rg_cut_13_al,total = nrow(cls_17_al))[[1]] + sum(cls_17_al$`RED GROUPER_kept`,na.rm=T)
ty2_17_cut_13_rg_al_total <- as.numeric(ty2_17_cut_13_rg_al_total)

