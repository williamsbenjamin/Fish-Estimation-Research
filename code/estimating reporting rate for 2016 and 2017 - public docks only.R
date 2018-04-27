library(tidyverse)
library(lubridate)
mrip16 <- read_csv("data/mrip_tidy2_16.csv")
mrip17 <- read_csv("data/mrip_tidy2_17.csv")
#public/private - only look at public trips for now
cls_tidy2_17 <- read_csv("data/cls_tidy2_17.csv")
cls_private_public_all <- read_sas("data/cls_pp.sas7bdat")
cls_private <- cls_private_public_all %>% 
  filter(Public_id2 %in% c(0,-1)) %>% 
  filter(Public_id == 0)

cls <- cls_tidy2_17 %>% 
  anti_join(cls_private)

matches <- read_csv("data/tidy_all_matches_rl_one_per.csv")
matches_16 <- matches %>% filter(year(return_date_ymd)==2016)
matches_17 <- matches %>% filter(year(return_date_ymd)==2017)
cls_16 <- cls %>% filter(year(return_date_ymd)==2016)
cls_16 <- cls %>% filter(year(return_date_ymd)==2017)

m_16 <- nrow(matches_16)
m_17 <- nrow(matches_17)

n1_16 <- nrow(cls_16)
n1_17 <- nrow(cls_17)

n2_16 <- nrow(mrip_16)
n2_17 <- nrow(mrip_17)

N_16 <- (n1_16 * n2_16) / m_16
N_17 <- (n1_17 * n2_17) / m_17

p1_16 <- n1_16 / N_16
p1_17 <- n1_17 / N_17

p1_16
p1_17
