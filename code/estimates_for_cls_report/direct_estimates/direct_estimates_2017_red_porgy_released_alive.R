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

mrip_all_17$`RED PORGY_released`[is.na(mrip_all_17$`RED PORGY_released`)] <- 0
mrip_all_17 <- mrip_all_17 %>% 
  mutate(delta_rp = `RED PORGY_release` - `RED PORGY_released`) 

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

ty2_17_rp_total <- predict(ty2.r17_rp,total = nrow(cls_17))[[1]] + 
  sum(cls_17$`RED PORGY_released`,na.rm=T)
ty2_17_rp_total <- as.numeric(ty2_17_rp_total)
#ty2_17_rp_total

######
#Florida Only
######
mrip_all_17_1 <- read_csv("data/mrip_dm_17_all_sites.csv",
                          col_types = cols(psu_id = col_character()))

mrip_all_17_1$`RED PORGY_released`[is.na(mrip_all_17_1$`RED PORGY_released`)] <- 0
mrip_all_17_1 <- mrip_all_17_1 %>% 
  mutate(delta_rp = `RED PORGY_release` - `RED PORGY_released`) 

mrip_all_17_1_fl <- mrip_all_17_1 %>% 
  mutate(red_porgy_release_fl = if_else(ST == 12,
                                      `RED PORGY_release`,as.numeric(0)),
         red_porgy_released_fl = if_else(ST == 12,
                                     `RED PORGY_released`,as.numeric(0))) %>% 
  mutate(delta_rp_fl = red_porgy_release_fl - red_porgy_released_fl) %>% 
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
ty2_17_cut_13_rp_fl_total <- predict(ty2.r17_rp_cut_13_fl,total = nrow(cls_17_fl))[[1]] + sum(cls_17_fl$`RED PORGY_released`,na.rm=T)
ty2_17_cut_13_rp_fl_total <- as.numeric(ty2_17_cut_13_rp_fl_total)


#########
# ALABAMA
##########
mrip_all_17_2 <- read_csv("data/mrip_dm_17_all_sites.csv",
                          col_types = cols(psu_id = col_character()))

mrip_all_17_2$`RED PORGY_released`[is.na(mrip_all_17_2$`RED PORGY_released`)] <- 0
mrip_all_17_2 <- mrip_all_17_2 %>% 
  mutate(delta_rp = `RED PORGY_release` - `RED PORGY_released`) 

mrip_all_17_2_al <- mrip_all_17_2 %>% 
  mutate(red_porgy_release_al = if_else(ST == 1,
                                      `RED PORGY_release`,as.numeric(0)),
         red_porgy_released_al = if_else(ST == 1,
                                     `RED PORGY_released`,0)) %>% 
  mutate(delta_rp_al = red_porgy_release_al - red_porgy_released_al) %>% 
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
ty2_17_cut_13_rp_al_total <- predict(ty2.r17_rp_cut_13_al,total = nrow(cls_17_al))[[1]] + sum(cls_17_al$`RED PORGY_released`,na.rm=T)
ty2_17_cut_13_rp_al_total <- as.numeric(ty2_17_cut_13_rp_al_total)
