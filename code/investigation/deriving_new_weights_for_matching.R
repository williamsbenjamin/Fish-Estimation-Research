library(tidyverse)
library(ggthemes)
library(lubridate)
library(survey)
library(haven)

source("code/source_code/mediating_datasets_for_making_data_sets_with_cutpoints.R")
#use all sites 
cls_17 <- read_csv("data/cls_17_all_sites.csv")
cutpoint <- 10
#Define RL cutpoints 
source("code/source_code/second_step_mediating_datasets_for_rl_multiple_cutpoints.R")

#make species specific variables for estimation

mrip_all_17_a$`RED SNAPPER_kept`[is.na(mrip_all_17_a$`RED SNAPPER_kept`)] <- 0
mrip_all_17_a <- mrip_all_17_a %>% 
  mutate(delta_rs = `RED SNAPPER_claim` - `RED SNAPPER_kept`) %>%
  mutate(non_reporter_mrip_claim_rs = if_else(reported == 0,
                                           `RED SNAPPER_claim`,as.numeric(0)))

####USING Cutpoint a
#2017
desi17_all_a <- svydesign(id = ~psu_id,
                    weights = ~w_int,
                    strata = ~strat_id,
                    nest=T,
                    data=mrip_all_17_a)
options(survey.lonely.psu = "adjust") 

#ty2
ty2.r17_rs_a <- svyratio(~delta_rs,
                        ~reported,
                        design=desi17_all_a,
                        na.rm=T)
ty2_17_rs_se_a <- as.numeric(predict(ty2.r17_rs_a,
                                   total = nrow(cls_17))$se)

ty2_17_rs_total_a <- predict(ty2.r17_rs_a,total = nrow(cls_17))[[1]] + sum(cls_17$`RED SNAPPER_kept`,na.rm=T)
ty2_17_rs_total_a <- as.numeric(ty2_17_rs_total_a)

##
tyrs <- svydesign(~psu_id,
                  weights = ~w_int,
                  strata = ~strat_id,
                  nest = T,
                  data = mrip_all_17_a)

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
ty_star_rs + (n_1/n_1_hat)*(ty_hat_rs - ty_star_hat_rs)



###
tidy_mrip <- read_csv("data/mrip_tidy2_17.csv")
tidy_mrip %>% 
  filter(CLS == "Y") %>% 
  nrow()
tidy_mrip <- tidy_mrip %>% 
  mutate(CLS_1 = if_else(is.na(CLS),
                       "N",CLS)) %>% 
  mutate(rep = if_else(CLS_1 =="Y",
                       1,0))

ty_m <- svydesign(~psu_id,
                  weights = ~w_int,
                  strata = ~strat_id,
                  nest = T,
                  data = tidy_mrip)
n1h <- svytotal(~rep,
                ty_m)
