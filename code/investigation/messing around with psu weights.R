library(tidyverse)
library(haven)

w_data <- read_sas("data/weights_2016_2017_gulf_block.sas7bdat")

w_data %>% 
  count(psu)

w_data %>% 
  count(psu,w_psu) %>% 
  View()

w_data %>% 
  group_by(psu) %>% 
  summarise(w_psu = n_distinct(w_psu),
            w_i = n_distinct(w_int)) %>% 
  View()

w_data %>% 
  mutate(w_psu = as.character(w_psu),
         w_int = as.character(w_int)) %>% 
  unite(ws,c(w_psu,w_int)) %>% 
  group_by(psu) %>% 
  summarise(w_psu = n_distinct(ws)) %>% 
  View()

w_data %>% 
  group_by(psu) %>% 
  summarise(w_psu = n_distinct(w_psu)) %>% 
  count(w_psu)

mrip <- read_csv("data/mrip_tidy2_17.csv",
                 col_types = cols(psu_id = col_character()))

w_data %>% 
  group_by(psu,site,block_start) %>% 
  summarise(ws = n_distinct(w_psu)) %>% 
  View("ww")


w_data %>% 
  select(psu,site,w_psu,block_start,ST) %>% 
  View("w")

mrip %>% 
  select(psu_id,wp_int,INTSITE,ST) %>% 
  View()

w_data %>% 
  group_by(ID_CODE) %>% 
  summarise(n_d = n_distinct(w_int)) %>% 
  arrange(n_d)
