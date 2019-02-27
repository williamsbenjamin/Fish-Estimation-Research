##Estimating reporting rate, use all sites

library(tidyverse)

mrip16 <- read_csv("data/mrip_all_16_all_sites.csv")
mrip17 <- read_csv("data/mrip_all_17_all_sites.csv")
cls_16 <- read_csv("data/cls_16_all_sites.csv")
cls_17 <- read_csv("data/cls_17_all_sites.csv")

###ALL RL Rows
#With Weights
mrip_16 <- mrip16 %>% 
  mutate(m = wp_int*reported)

mrip_17 <- mrip17 %>% 
  mutate(m = wp_int*reported)

rep_rate_16 <- sum(mrip_16$m) / sum(mrip_16$wp_int)
rep_rate_17 <- sum(mrip_17$m) / sum(mrip_17$wp_int)

rep_rate_16
rep_rate_17

## In our data, calculate : number of matches / sample size
## Not using weights

matches <- read_csv("data/tidy_all_matches_rl_one_per.csv")

matches_16 <- matches %>% filter(year(return_date_ymd)==2016)
matches_17 <- matches %>% filter(year(return_date_ymd)==2017)

m_16 <- nrow(matches_16)
m_17 <- nrow(matches_17)

n2_16 <- nrow(mrip16)
n2_17 <- nrow(mrip17)

p1_16 <- m_16 / n2_16
p1_17 <- m_17 / n2_17

p1_16
p1_17

#RL Score > 16
#UNWEIGHTED
matches_rl <- matches %>% 
  filter(rl_score > 16)
matches_16_rl <- matches_rl %>% filter(year(return_date_ymd)==2016)
matches_17_rl <- matches_rl %>% filter(year(return_date_ymd)==2017)

m_16_rl <- nrow(matches_16_rl)
m_17_rl <- nrow(matches_17_rl)

n2_16_rl <- nrow(mrip16)
n2_17_rl <- nrow(mrip17)

p1_16_rl <- m_16_rl / n2_16_rl
p1_17_rl <- m_17_rl / n2_17_rl

p1_16_rl
p1_17_rl

#weighted
dat17_cutof16 <- read_csv("data/mrip_all_17_cutoff16_all_sites.csv")
dat17_cutof16_w <- dat17_cutof16 %>% 
  mutate(m = wp_int*reported)
rep_rate_17_cutof16 <- sum(dat17_cutof16_w$m) / 
                              sum(dat17_cutof16_w$wp_int)

rep_rate_17_cutof16
#Direct Matching
#Unweighted
matches_dm_17 <- 55

n2_17_dm <- nrow(mrip17)

p1_17_dm <- matches_dm_17 / n2_17_dm

p1_17_dm

# WEIGHTED
mrip_17_dm_w <- read_csv("data/mrip_dm_17_all_sites.csv")
mrip_17_dm_w <- mrip_17_dm_w %>% 
              mutate(m = wp_int*reported)
rep_rate_17_dm <- sum(mrip_17_dm_w$m) / sum(mrip_17_dm_w$wp_int)

rep_rate_17_dm

#naive matching (updated direct matching) 
mrip17_naive <- read_csv("data/mrip_dm_17_all_sites.csv")
mrip17_naive_w <- mrip17_naive %>% 
  mutate(m = wp_int*reported)
rep_rate_17_naive_ben <- sum(mrip17_naive_w$m) / sum(mrip17_naive_w$wp_int)
rep_rate_17_naive_ben


#naive matching (updated direct matching) 
#AL
mrip17_naive <- read_csv("data/mrip_dm_17_all_sites.csv")
mrip17_naive_w_al <- mrip17_naive %>% 
  filter(ST == 1) %>% 
  mutate(m = wp_int*reported)
rep_rate_17_naive_ben_al <- sum(mrip17_naive_w_al$m) / 
                                sum(mrip17_naive_w_al$wp_int)
rep_rate_17_naive_ben_al

#FL
mrip17_naive <- read_csv("data/mrip_dm_17_all_sites.csv")
mrip17_naive_w_fl <- mrip17_naive %>% 
  filter(ST == 12) %>% 
  mutate(m = wp_int*reported)
rep_rate_17_naive_ben_fl <- sum(mrip17_naive_w_fl$m) / 
  sum(mrip17_naive_w_fl$wp_int)
rep_rate_17_naive_ben_fl

