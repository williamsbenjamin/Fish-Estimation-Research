library(tidyverse)
library(ggthemes)
library(lubridate)
#get some frequency stats from the data

mrip_16 <- read_csv("mrip_tidy_16.csv")
mrip_17 <- read_csv("mrip_tidy_17.csv")
cls2tid <- read_csv("cls_2_tid_2-26-2018.csv")
cls <- read_csv("cls_tidy_17.csv")

unique(cls2tid$CLS_ID)
unique(mrip_16$CLS_ID)
unique(mrip_17$CLS_ID)
unique(cls$CLS_ID)

#number of reports per CLS_ID in CLS data in 2016
cls %>%
  mutate(yr =year(return_date_ymd)) %>%
  filter(yr == 2016) %>%
  count(TID,sort=T) %>% 
  left_join(cls2tid,by=c("TID"="Serial_Number"))%>%
  View("number of reports per CLS boats 2016")

cls %>%
  mutate(yr =year(return_date_ymd)) %>%
  filter(yr == 2016) %>%
  count(TID,sort=T) %>%
  mutate(TID=reorder(TID,n)) %>%
  ggplot(aes(TID,n)) +
  geom_bar(stat="identity") +
  theme_economist()

#number of reports per CLS_ID in CLS data in 2017
cls %>%
  mutate(yr =year(return_date_ymd)) %>%
  filter(yr == 2017) %>%
  count(TID,sort=T) %>% 
  left_join(cls2tid,by=c("TID"="Serial_Number")) %>%
  View("number of reports per CLS boats 2017")

cls %>%
  mutate(yr =year(return_date_ymd)) %>%
  filter(yr == 2017) %>%
  count(TID,sort=T) %>%
  mutate(TID=reorder(TID,n)) %>%
  ggplot(aes(TID,n)) +
  geom_bar(stat="identity") +
  theme_economist()

#trips that reported in 2016
#cls boats that were sampled in 2016
#were there boats that reported that are not 
#in cls2tid dataset

#The CLS boats that reported in 2016
unique_cls_rep_16 <- cls %>%
  mutate(yr =year(return_date_ymd)) %>%
  filter(yr == 2016) %>%
  select(TID) %>%
  unique()

#how many reports have an associated CLS_ID
inner_join(unique_cls_rep_16,
          cls2tid,
          by=c("TID"="Serial_Number")) %>%
  View()

#CLS boats caught in APAIS in 2016
unique_cls_in_mrip16 <- mrip_16 %>%
  filter(CLS_ID != "NA") %>%
  select(CLS_ID) %>%
  unique()
#How many CLS_IDs are associated with a valid TID
inner_join(unique_cls_in_mrip16,
          cls2tid)



#The CLS boats that reported in 2016
unique_cls_rep_16 <- cls %>%
  mutate(yr =year(return_date_ymd)) %>%
  filter(yr == 2016) %>%
  select(TID) %>%
  unique()

#how many reports have an associated CLS_ID
inner_join(unique_cls_rep_16,
           cls2tid,
           by=c("TID"="Serial_Number")) %>%
  View()

#CLS boats caught in APAIS in 2017
unique_cls_in_mrip16 <- mrip_16 %>%
  filter(CLS_ID != "NA") %>%
  select(CLS_ID) %>%
  unique()

#How many CLS_IDs are associated with a valid TID
inner_join(unique_cls_in_mrip16,
           cls2tid) %>% View()

#the CLS_IDs of the boats who self reported and have a given CLS_ID number
cls_ids_in_self_reps16 <- inner_join(unique_cls_rep_16,
                                     cls2tid,
                                     by=c("TID"="Serial_Number")) %>%
  select(CLS_ID) 

#boats caught in MRIP who have a CLS_ID number currently acknowledged by Gregg
cls_ids_in_mrip16 <- inner_join(unique_cls_in_mrip16,
                                cls2tid) %>%
  select(CLS_ID)

#The boats with a valid CLS_ID that were self reported and caught by MRIP
cls_ids_in_both_selfreps_mrip_16 <- cls_ids_in_self_reps16$CLS_ID[
  cls_ids_in_self_reps16$CLS_ID %in% cls_ids_in_mrip16$CLS_ID
  ]

#The boats with a valid CLS_ID that were self-reported and not caught
cls_ids_only_in_selfreps_16 <- cls_ids_in_self_reps16$CLS_ID[
  !cls_ids_in_self_reps16$CLS_ID %in% cls_ids_in_mrip16$CLS_ID
  ]



#####2017
#CLS boats caught in APAIS in 2017
unique_cls_in_mrip17 <- mrip_17 %>%
  filter(CLS_ID != "NA") %>%
  select(CLS_ID) %>%
  unique()
#How many CLS_IDs are associated with a valid TID
inner_join(unique_cls_in_mrip17,
           cls2tid) %>% View()

#the CLS_IDs of the boats who self reported and have a given CLS_ID number
cls_ids_in_self_reps17 <- inner_join(unique_cls_rep_17,
           cls2tid,
           by=c("TID"="Serial_Number")) %>%
  select(CLS_ID) 

#boats caught in MRIP who have a CLS_ID number currently acknowledged by Gregg
cls_ids_in_mrip17 <- inner_join(unique_cls_in_mrip17,
                              cls2tid) %>%
  select(CLS_ID)

#The boats with a valid CLS_ID that were self reported and caught by MRIP
cls_ids_in_both_selfreps_mrip_17 <- cls_ids_in_self_reps17$CLS_ID[
  cls_ids_in_self_reps17$CLS_ID %in% cls_ids_in_mrip17$CLS_ID
]

#The boats with a valid CLS_ID that were self-reported and not caught
cls_ids_only_in_selfreps_17 <- cls_ids_in_self_reps17$CLS_ID[
  !cls_ids_in_self_reps17$CLS_ID %in% cls_ids_in_mrip17$CLS_ID
  ]


#total reports 2016
cls %>%
  mutate(yr = year(return_date_ymd)) %>%
  filter(yr == 2016) %>%
  mutate(in_samp = if_else(CLS_ID %in% cls_ids_in_both_selfreps_mrip_16,
                           "Yes","No"),
         out_sample = if_else(CLS_ID %in% cls_ids_only_in_selfreps_16,
                              "Yes","No")
  ) %>%
  filter(in_samp == "Yes" || out_sample == "Yes") %>%
  group_by(CLS_ID) %>%
  mutate(samp = if_else(in_samp == "Yes","Yes","No"),
         n=n()) %>%
  select(CLS_ID,n,samp) %>%
  distinct() %>%
  ggplot(aes(CLS_ID,n)) +
  geom_col() +
  facet_grid(factor(samp)~.) +
  theme_economist() +
  ylab("Number of Reports") +
  ggtitle("# of reports, broken down by if report was seen in APAIS 2016")

#total reports 2017
cls %>%
  mutate(yr = year(return_date_ymd)) %>%
  filter(yr == 2017) %>%
  mutate(in_samp = if_else(CLS_ID %in% cls_ids_in_both_selfreps_mrip_17,
                           "Yes","No"),
         out_sample = if_else(CLS_ID %in% cls_ids_only_in_selfreps_17,
                              "Yes","No")
  ) %>%
  filter(in_samp == "Yes" || out_sample == "Yes") %>%
  group_by(CLS_ID) %>%
  mutate(samp = if_else(in_samp == "Yes","Yes","No"),
         n=n()) %>%
  select(CLS_ID,n,samp) %>%
  distinct() %>%
  ggplot(aes(CLS_ID,n)) +
  geom_col() +
  facet_grid(factor(samp)~.) +
  theme_economist() +
  ylab("Number of Reports") +
  ggtitle("# of reports, broken down by if report was seen in APAIS 2017")
