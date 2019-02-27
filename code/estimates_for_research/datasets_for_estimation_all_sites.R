
## Code to make full dataset for estimation
## Makes a dataset of the mrip intercepts and appends
## CLS information if the trip also reported

## Leaves private trips in cls reports

library(tidyverse)
library(lubridate)
library(survey)
library(haven)

#matches
tidy_all_matches_one_per <- read_csv("data/tidy_all_matches_rl_one_per.csv",
                                     col_types=cols(psu_id = col_character()))
#mrip from 2016 and 2017
mrip_tidy2_16 <- read_csv("data/mrip_tidy2_16.csv",
                          col_types=cols(psu_id = col_character()))
mrip_tidy2_17 <- read_csv("data/mrip_tidy2_17.csv",
                          col_types=cols(psu_id = col_character()))
#CLS from 2016 and 2017
cls_tidy2_17 <- read_csv("data/cls_tidy2_full.csv") #includes 2016 and 2017


#Get dataset of the mrip trips with a cls id
mrip_all <- bind_rows(mrip_tidy2_16,
                      mrip_tidy2_17)
is.zero.na <- function(x){
  x == 0 | is.na(x)
}

mrip_all <- mrip_all %>%
  select(-mrip_non_cls_claim,
         -mrip_non_cls_release) %>% 
  bind_cols(select(mrip_all, ends_with("claim")) %>%
              transmute(total_claim_mrip = rowSums(., na.rm = T))) %>%
  bind_cols(select(mrip_all, ends_with("release")) %>%
              transmute(total_release_mrip = rowSums(., na.rm = T)))
colz <- c(22:106)
cls_tidy2_17[,colz] = apply(cls_tidy2_17[,colz],2,function(x)as.numeric(x))  

cls_all <- cls_tidy2_17 %>%
  bind_cols(
    select(cls_tidy2_17,ends_with("kept")) %>%
      transmute(total_kept_cls = rowSums(.,na.rm = T)
      )
  ) %>%
  bind_cols(
    select(cls_tidy2_17,ends_with("released")) %>%
      transmute(total_released_cls = rowSums(.,na.rm = T)
      )
  )


#add on cls info to mrip_all
mrip_non_rep <- mrip_all %>% #take out the matches from mrip_all
  anti_join(tidy_all_matches_one_per,
            by=c("PRT_CODE")) 

mrip_full <- bind_rows(tidy_all_matches_one_per,
                       mrip_non_rep)

mrip_full <- mrip_full %>%
  mutate(reported = if_else(is.na(total_kept_cls)
                            ,0,1))
mrip_full$total_kept_cls[is.na(mrip_full$total_kept_cls)] <- 0
mrip_full <- mrip_full %>% 
  mutate(y_star = total_kept_cls) %>%
  mutate(delta = total_claim_mrip - total_kept_cls) %>%
  mutate(non_reporter_mrip_claim = if_else(reported == 0,
                                           total_claim_mrip,0))
mrip_cls_all <- mrip_full #mrip_full contains all mrip and cls info

mrip_all_16 <- filter(mrip_cls_all,year(date)==2016)

cls_16 <- cls_all %>% 
  filter(year(return_date_ymd)==2016)

mrip_all_17 <- filter(mrip_cls_all,year(date)==2017)

cls_17 <- cls_all %>% 
  filter(year(return_date_ymd)==2017 &
           year(reportdate) != 2016)

write_csv(mrip_all_16,"data/mrip_all_16_all_sites.csv")
write_csv(mrip_all_17,"data/mrip_all_17_all_sites.csv")
write_csv(cls_16,"data/cls_16_all_sites.csv")
write_csv(cls_17,"data/cls_17_all_sites.csv")

## Record Linkage with Cut point of 12

tidy_all_score_gt_3 <- tidy_all_matches_one_per %>%
  filter(rl_score > 12)

mrip_non_rep2 <- mrip_all %>% #take out the matches from mrip_all
  anti_join(tidy_all_score_gt_3,
            by=c("PRT_CODE")) 

mrip_full2 <- bind_rows(tidy_all_score_gt_3,
                        mrip_non_rep2)
mrip_full2 <- mrip_full2 %>%
  mutate(reported = if_else(is.na(total_kept_cls)
                            ,0,1))

mrip_full2$total_kept_cls[is.na(mrip_full2$total_kept_cls)] <- 0

mrip_full2 <- mrip_full2 %>% 
  mutate(y_star = total_kept_cls) %>%
  mutate(delta = total_claim_mrip - total_kept_cls) %>%
  mutate(non_reporter_mrip_claim = if_else(reported == 0,
                                           total_claim_mrip,0))

mrip_cls_all2 <- mrip_full2 #mrip_full contains all mrip and cls info

mrip_all_16_2 <- filter(mrip_cls_all2,year(date)==2016)

mrip_all_17_2 <- filter(mrip_cls_all2,year(date)==2017)


write_csv(mrip_all_16_2,"data/mrip_all_16_cutoff12_all_sites.csv")
write_csv(mrip_all_17_2,"data/mrip_all_17_cutoff12_all_sites.csv")

## Record Linkage with Cut point of 13.5

tidy_all_score_gt_3 <- tidy_all_matches_one_per %>%
  filter(rl_score > 13.5)

mrip_non_rep2 <- mrip_all %>% #take out the matches from mrip_all
  anti_join(tidy_all_score_gt_3,by="date_time_mrip") 

mrip_full2 <- bind_rows(tidy_all_score_gt_3,
                        mrip_non_rep2)
mrip_full2 <- mrip_full2 %>%
  mutate(reported = if_else(is.na(total_kept_cls)
                            ,0,1))

mrip_full2$total_kept_cls[is.na(mrip_full2$total_kept_cls)] <- 0

mrip_full2 <- mrip_full2 %>% 
  mutate(y_star = total_kept_cls) %>%
  mutate(delta = total_claim_mrip - total_kept_cls) %>%
  mutate(non_reporter_mrip_claim = if_else(reported == 0,
                                           total_claim_mrip,0))

mrip_cls_all2 <- mrip_full2 #mrip_full contains all mrip and cls info

mrip_all_16_2 <- filter(mrip_cls_all2,year(date)==2016)

mrip_all_17_2 <- filter(mrip_cls_all2,year(date)==2017)


write_csv(mrip_all_16_2,"data/mrip_all_16_cutoff135_all_sites.csv")
write_csv(mrip_all_17_2,"data/mrip_all_17_cutoff135_all_sites.csv")

