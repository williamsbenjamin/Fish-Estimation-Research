library(tidyverse)
library(haven)
#CLS from 2016 and 2017
cls_tidy2_17 <- read_csv("data/cls_tidy2_17.csv") #includes 2016 and 2017

nrow(cls_tidy2_17)

cls_private_public_all <- read_sas("data/cls_pp.sas7bdat")

cls_private <- cls_private_public_all %>% 
  filter(Public_id2 %in% c(0,-1)) %>% 
  filter(Public_id == 0)

cls_public_all <- cls_tidy2_17 %>% 
  anti_join(cls_private)

write_csv(cls_public_all,"data/cls_public_all.csv")
