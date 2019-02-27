library(tidyverse)
library(lubridate)


cls17 <- read_csv("data/cls_17_all_sites.csv")


cls17 %>% 
  filter(year(reportdate) == 2017) %>% 
  nrow()
cls17 %>% 
  filter(year(returndate) == 2017) %>% 
  nrow()
#number of angler trips - CLS
cls17 %>% 
  pull(nbAnglers) %>% 
  sum()

mrip17 <- read_csv("data/mrip_all_17_all_sites.csv")
mrip17 %>% 
  mutate(ang_w = wp_int*PARTY) %>% 
  pull(ang_w) %>% 
  sum()
#1238267