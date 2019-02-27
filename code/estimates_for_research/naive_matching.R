library(tidyverse)
library(lubridate)


cls17 <- read_csv("data/cls_17_all_sites.csv")
mrip17 <- read_csv("data/mrip_tidy2_17.csv")

mrip17 %>% 
  pull(date_time_mrip) %>% 
  .[duplicated(.)]
nrow(mrip17)
length(unique(mrip17$date_time_mrip))
mrip17 %>% 
  distinct(date_time_mrip,wp_int) %>% 
  nrow()

cls17 %>% 
  filter(year(reportdate) == 2017) %>% 
  nrow()

naive_17 <- mrip17 %>% 
  left_join(cls17, by = c("CLS_ID" = "CLS_ID",
                          "date" = "return_date_ymd"))

nrow(naive_17)
names(naive_17)
naive_17 %>% 
  filter(!is.na(reportdate)) %>% 
  nrow() #147 naive matches before deduplicating

naive_17 %>% 
  mutate(date_diff = returndate - date_time_mrip) %>% 
  group_by(date_time_mrip,wp_int) %>% 
  top_n(n=-1,date_diff) %>% 
  ungroup() %>% 
  group_by(tripID) %>% 
  top_n(n=-1,date_diff) %>% 
  nrow()

naive_trips <- naive_17 %>% 
  mutate(date_diff = returndate - date_time_mrip) %>% 
  group_by(date_time_mrip,wp_int) %>% 
  top_n(n=-1,date_diff) %>% 
  ungroup() %>% 
  group_by(tripID) %>% 
  top_n(n=-1,date_diff) %>% 
  pull(tripID)

naive_matches_ben_17 <- naive_17 %>% 
  mutate(date_diff = returndate - date_time_mrip) %>% 
  group_by(date_time_mrip,wp_int) %>% 
  top_n(n=-1,date_diff) %>% 
  ungroup() %>% 
  group_by(tripID) %>% 
  top_n(n=-1,date_diff) 

naive_matches_ben_17 <- mrip17 %>% 
  left_join(naive_matches_ben_17) %>% 
  mutate(reported = if_else(!is.na(tripID),
                            1,0))
write_csv(naive_matches_ben_17,"data/naive_matches_17_ben.csv")

length(naive_trips)
length(unique(naive_trips))

tidy_naive <- read_sas("data/tidy_naive5.sas7bdat")
tidy_naive$tripID

naive_trips %in% tidy_naive$tripID
