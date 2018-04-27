library(tidyverse)
library(ggthemes)
tidy_all_matches_docks <- read_csv("tidy_all_matches_docks_2017.csv")


#histogram of distances between cls reports and docks
tidy_all_matches_docks %>%
  ggplot(aes(km_difference)) +
  #geom_line(aes(y = ..density..), stat = 'density',size=2)+
  geom_histogram(aes(y=..density..),bins = 50,fill="royalblue4") +
  theme_fivethirtyeight() +
  labs(title="Distance between CLS Reporting Location and Site Location for all Possible Matches")+
  theme(plot.title = element_text(size=24))

#histogram of the difference in the number of anglers, CLS-MRIP
tidy_all_matches_docks %>%
  ggplot(aes(diff_cls_mrip_anglers)) +
  geom_histogram(aes(y=..density..),bins=10,fill="royalblue4") +
  theme_fivethirtyeight() +
  labs(title="Difference in Number of Anglers (CLS-MRIP) for all Possible Matches")+
  theme(plot.title = element_text(size=24))


#histogram of difference in total catch per trip CLS - MRIP
#diff_total_catch
tidy_all_matches_docks %>%
  ggplot(aes(diff_total_catch)) +
  geom_histogram(aes(y=..density..),bins=40,fill="royalblue4") +
  theme_fivethirtyeight() +
  labs(title="Difference in Total Catch (CLS-MRIP) for all Possible Matches")+
  theme(plot.title = element_text(size=24))




#histogram of difference in total release per trip CLS - MRIP
#diff_total_release
tidy_all_matches_docks %>%
  ggplot(aes(diff_total_release)) +
  geom_histogram(aes(y=..density..),bins=40,fill="royalblue4") + 
  theme_fivethirtyeight() +
  labs(title="Difference in Total Release (CLS-MRIP) for all Possible Matches")+
  theme(plot.title = element_text(size=24))

