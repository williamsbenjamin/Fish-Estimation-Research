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

mrip_all_17$`VERMILION SNAPPER_released`[is.na(mrip_all_17$`VERMILION SNAPPER_released`)] <- 0
mrip_all_17 <- mrip_all_17 %>% 
  mutate(delta_vs = `VERMILION SNAPPER_release` - `VERMILION SNAPPER_released`)


#2017
desi17_all <- svydesign(id=~psu_id,
                    weights=~w_int,
                    strata = ~strat_id,
                    nest=T,
                    data=mrip_all_17)
options(survey.lonely.psu = "adjust") 

ty2.r17_vs <- svyratio(~delta_vs,
                        ~reported,
                        design=desi17_all,
                        na.rm=T)
ty2_17_vs_se <- as.numeric(predict(ty2.r17_vs,
                                   total = nrow(cls_17))$se)


ty2_17_vs_total <- predict(ty2.r17_vs,total = nrow(cls_17))[[1]] + sum(cls_17$`VERMILION SNAPPER_released`,na.rm=T)
ty2_17_vs_total <- as.numeric(ty2_17_vs_total)
ty2_17_vs_total
ty2_17_vs_se / ty2_17_vs_total


########
#Florida Only
#########
mrip_all_17_1 <- read_csv("data/mrip_dm_17_all_sites.csv",
                          col_types = cols(psu_id = col_character()))
mrip_all_17_1$`VERMILION SNAPPER_released`[is.na(mrip_all_17_1$`VERMILION SNAPPER_released`)] <- 0
mrip_all_17_1 <- mrip_all_17_1 %>% 
  mutate(delta_vs = `VERMILION SNAPPER_release` - `VERMILION SNAPPER_released`) %>%
  mutate(non_reporter_mrip_claim_vs = if_else(reported == 0,
                                              `VERMILION SNAPPER_release`,as.numeric(0)))

mrip_all_17_1_fl <- mrip_all_17_1 %>% 
  mutate(VERMILION_snapper_release_fl = if_else(ST == 12,
                                        `VERMILION SNAPPER_release`,as.numeric(0)),
         VERMILION_snapper_released_fl = if_else(ST == 12,
                                       `VERMILION SNAPPER_released`,as.numeric(0))) %>% 
  mutate(delta_vs_fl = VERMILION_snapper_release_fl - VERMILION_snapper_released_fl) %>% 
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

ty2.r17_vs_cut_13_fl <- svyratio(~delta_vs_fl,
                                 ~reported_fl,
                                 design=desi17_all_1_fl,
                                 na.rm=T)
ty2_17_cut_13_vs_fl_se <- as.numeric(predict(ty2.r17_vs_cut_13_fl,
                                             total = nrow(cls_17_fl))$se)
ty2_17_cut_13_vs_fl_total <- predict(ty2.r17_vs_cut_13_fl,total = nrow(cls_17_fl))[[1]] + 
  sum(cls_17_fl$`VERMILION SNAPPER_released`,na.rm=T)
ty2_17_cut_13_vs_fl_total <- as.numeric(ty2_17_cut_13_vs_fl_total)

##########
##ALABAMA
##########
mrip_all_17_2 <- read_csv("data/mrip_dm_17_all_sites.csv",
                          col_types = cols(psu_id = col_character()))
mrip_all_17_2$`VERMILION SNAPPER_released`[is.na(mrip_all_17_2$`VERMILION SNAPPER_released`)] <- 0
mrip_all_17_2 <- mrip_all_17_2 %>% 
  mutate(delta_vs = `VERMILION SNAPPER_release` - `VERMILION SNAPPER_released`) %>%
  mutate(non_reporter_mrip_claim_vs = if_else(reported == 0,
                                              `VERMILION SNAPPER_release`,as.numeric(0)))

mrip_all_17_2_al <- mrip_all_17_2 %>% 
  mutate(VERMILION_snapper_release_al = if_else(ST == 1,
                                              `VERMILION SNAPPER_release`,as.numeric(0)),
         VERMILION_snapper_released_al = if_else(ST == 1,
                                             `VERMILION SNAPPER_released`,as.numeric(0))) %>% 
  mutate(delta_vs_al = VERMILION_snapper_release_al - VERMILION_snapper_released_al) %>% 
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

ty2.r17_vs_cut_13_al <- svyratio(~delta_vs_al,
                                 ~reported_al,
                                 design=desi17_all_1_al,
                                 na.rm=T)
ty2_17_cut_13_vs_al_se <- as.numeric(predict(ty2.r17_vs_cut_13_al,
                                             total = nrow(cls_17_al))$se)
ty2_17_cut_13_vs_al_total <- predict(ty2.r17_vs_cut_13_al,total = nrow(cls_17_al))[[1]] + sum(cls_17_al$`VERMILION SNAPPER_released`,na.rm=T)
ty2_17_cut_13_vs_al_total <- as.numeric(ty2_17_cut_13_vs_al_total)
