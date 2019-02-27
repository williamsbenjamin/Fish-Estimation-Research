
#I don't think I need this anymore?...

library(tidyverse)
library(ggthemes)
tidy_all_matches_docks <- read_csv("data/all_possible_matches_16_17.csv")

#histogram of distances between cls reports and docks
tidy_all_matches_docks %>%
  ggplot(aes(km_difference)) +
  #geom_line(aes(y = ..density..), stat = 'density',size=2)+
  geom_histogram(aes(y=..density..),bins = 50,fill="royalblue4") +
  theme_fivethirtyeight() +
  labs(title="Distance between CLS Reporting Location and Site Location for all Possible Matches")+
  theme(plot.title = element_text(size=24))


#Get Frequencies, 19804 total matches
tidy_all_matches_docks %>%
  filter(km_difference != "NA") %>%
  nrow() 
tidy_all_matches_docks %>%
  filter(km_difference != "NA") %>%
  filter(between(km_difference,-100,25)) %>%
  nrow()

tidy_all_matches_docks %>%
  filter(km_difference != "NA") %>%
  filter(between(km_difference,25.01,70)) %>%
  nrow()

tidy_all_matches_docks %>%
  filter(km_difference != "NA") %>%
  filter(between(km_difference,70.01,200)) %>%
  nrow()

tidy_all_matches_docks %>%
  filter(km_difference != "NA") %>%
  filter(between(km_difference,200.01,1000)) %>%
  nrow()

tidy_all_matches_docks %>%
  filter(km_difference != "NA") %>%
  filter(between(km_difference,1000.01,100000)) %>%
  nrow()

#histogram of the difference in the number of anglers, CLS-MRIP
tidy_all_matches_docks %>%
  ggplot(aes(diff_cls_mrip_anglers)) +
  geom_histogram(aes(y=..density..),bins=10,fill="royalblue4") +
  theme_fivethirtyeight() +
  labs(title="Difference in Number of Anglers (CLS-MRIP) for all Possible Matches")+
  theme(plot.title = element_text(size=24))

#Get frequencies, 137074 cases
tidy_all_matches_docks %>%
  filter(diff_cls_mrip_anglers != "NA") %>%
  nrow()

tidy_all_matches_docks %>%
  filter(diff_cls_mrip_anglers %in% c(0)) %>% 
  nrow()

tidy_all_matches_docks %>%
  filter(diff_cls_mrip_anglers %in% c(-1,1)) %>% 
  nrow()

tidy_all_matches_docks %>%
  filter(diff_cls_mrip_anglers %in% c(-2,2)) %>% 
  nrow()

tidy_all_matches_docks %>%
  filter(diff_cls_mrip_anglers %in% c(-3,3)) %>% 
  nrow()

tidy_all_matches_docks %>%
  filter(diff_cls_mrip_anglers %in% c(-33:-4,4:163)) %>% 
  nrow()

#histogram of difference in total catch per trip CLS - MRIP
#diff_total_catch
tidy_all_matches_docks %>%
  ggplot(aes(diff_total_catch)) +
  geom_histogram(aes(y=..density..),bins=40,fill="royalblue4") +
  theme_fivethirtyeight() +
  labs(title="Difference in Total Catch (CLS-MRIP) for all Possible Matches")+
  theme(plot.title = element_text(size=24))

#Get Frequenices for non-matches, 137074 total non-matches
tidy_all_matches_docks %>%
  filter(diff_total_catch %in% c(0)) %>%
  nrow()

tidy_all_matches_docks %>%
  filter(diff_total_catch %in% c(-4:-1,1:4)) %>%
  nrow()

tidy_all_matches_docks %>%
  filter(diff_total_catch %in% c(-15:-5,5:15)) %>%
  nrow()

tidy_all_matches_docks %>%
  filter(diff_total_catch %in% c(-30:-16,16:30)) %>%
  nrow()

tidy_all_matches_docks %>%
  filter(diff_total_catch %in% c(-3000:-31,31:3000)) %>%
  nrow()


#histogram of difference in total release per trip CLS - MRIP
#diff_total_release
tidy_all_matches_docks %>%
  ggplot(aes(diff_total_release)) +
  geom_histogram(aes(y=..density..),bins=40,fill="royalblue4") + 
  theme_fivethirtyeight() +
  labs(title="Difference in Total Release (CLS-MRIP) for all Possible Matches")+
  theme(plot.title = element_text(size=24))
#Get frequencies, total cases = 137074
tidy_all_matches_docks %>%
  filter(diff_total_release %in% c(0)) %>%
  nrow()

tidy_all_matches_docks %>%
  filter(diff_total_release %in% c(-4:-1,1:4)) %>%
  nrow()

tidy_all_matches_docks %>%
  filter(diff_total_release %in% c(-15:-5,5:15)) %>%
  nrow()

tidy_all_matches_docks %>%
  filter(diff_total_release %in% c(-30:-16,16:30)) %>%
  nrow()

tidy_all_matches_docks %>%
  filter(diff_total_release %in% c(-3000:-31,31:3000)) %>%
  nrow()

###Difference in Red Snapper Caught (101087)
tidy_all_matches_docks %>%
  mutate(red_diff = `RED SNAPPER_kept` - `RED SNAPPER_claim`) %>%
  ggplot(aes(red_diff))+
  geom_histogram(bins=10)+
  theme_economist()

tidy_all_matches_docks %>%
  mutate(red_diff = `RED SNAPPER_kept` - `RED SNAPPER_claim`) %>%
  filter(red_diff != "NA" ) %>% 
  nrow()

tidy_all_matches_docks %>%
  mutate(red_diff = `RED SNAPPER_kept` - `RED SNAPPER_claim`) %>%
  filter(red_diff != "NA" ) %>%
  filter(red_diff %in% c(0)) %>%
  nrow()

tidy_all_matches_docks %>%
  mutate(red_diff = `RED SNAPPER_kept` - `RED SNAPPER_claim`) %>%
  filter(red_diff != "NA" ) %>%
  filter(red_diff %in% c(-5:-1,1:5)) %>%
  nrow()

tidy_all_matches_docks %>%
  mutate(red_diff = `RED SNAPPER_kept` - `RED SNAPPER_claim`) %>%
  filter(red_diff %in% c(-15:-6,6:15)) %>%
  nrow()

tidy_all_matches_docks %>%
  mutate(red_diff = `RED SNAPPER_kept` - `RED SNAPPER_claim`) %>%
  filter(red_diff %in% c(-100:-16,16:500)) %>%
  nrow()


#Difference in Reporting/Sampling Date
tidy_all_matches_docks %>%
  mutate(date_diff=as.numeric(return_date_ymd - date)) %>%
  ggplot(aes(date_diff)) +
  geom_histogram(bins=10) +
  theme_economist()

#Get Frequencies, 137074 cases 
tidy_all_matches_docks %>%
  mutate(date_diff=as.numeric(return_date_ymd - date)) %>%
  filter(date_diff != "NA") %>% 
  nrow()

tidy_all_matches_docks %>%
  mutate(date_diff=as.numeric(return_date_ymd - date)) %>%
  filter(date_diff %in% c(0)) %>% 
  nrow()

tidy_all_matches_docks %>%
  mutate(date_diff=as.numeric(return_date_ymd - date)) %>%
  filter(date_diff %in% c(-5:-1,1:5)) %>% 
  nrow()

tidy_all_matches_docks %>%
  mutate(date_diff=as.numeric(return_date_ymd - date)) %>%
  filter(date_diff %in% c(-10:-6,6:10)) %>% 
  nrow()
  
tidy_all_matches_docks %>%
  mutate(date_diff=as.numeric(return_date_ymd - date)) %>%
  filter(date_diff %in% c(-20:-11,11:20)) %>% 
  nrow()

tidy_all_matches_docks %>%
  mutate(date_diff=as.numeric(return_date_ymd - date)) %>%
  filter(date_diff %in% c(-30:-21,21:30)) %>% 
  nrow()


tidy_all_matches_docks %>%
  mutate(date_diff=as.numeric(return_date_ymd - date)) %>%
  filter(date_diff %in% c(-100:-31,31:100)) %>% 
  nrow()
