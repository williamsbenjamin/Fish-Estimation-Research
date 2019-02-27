library(tidyverse)
library(lubridate)
library(haven)
##########################################################

mrip_tidy <- read_csv("data/mrip_tidy2_17.csv")
mrip_tidy$ID_CODE <- as.character(mrip_tidy$ID_CODE)
mrip_tidy <- mrip_tidy %>% 
  mutate(PRT_CODE = paste0(PRT_CODE,"a")) %>% 
  mutate(ID_CODE = paste0(ID_CODE,"a"))

# mrip_tidy %>% 
#   select(PRT_CODE,PARTY,date,ST,starts_with("RED S"),
#          starts_with("VERM"), starts_with("KING"),
#          starts_with("RED P"), starts_with("RED G"),
#          starts_with("WHITE G")) %>% 
#   write_csv("code/miscellaneous/dataset_for_class.csv")


write_csv(mrip_tidy,"code/miscellaneous/tidy_mrip_weights_10_2_2018.csv")






