
### Calculate proportion of trips caught in sample
### who could have reported, i.e. have a device


library(tidyverse)

# mrip16 <- read_csv("data/mrip_tidy2_16.csv")
mrip17 <- read_csv("data/mrip_tidy2_17.csv")
cls2tid <- read_sas("data/cls2tid_20170905.sas7bdat")
cls2tid$CLS_ID <- as.numeric(cls2tid$CLS_ID)
cls_tid_dates <- cls2tid %>% 
  select(CLS_ID,Deactivation_Date) %>% 
  filter(!is.na(CLS_ID)) %>% 
  filter(!is.na(Deactivation_Date))

mrip_with_activations <- mrip17 %>% 
  left_join(cls_tid_dates)

mrip_with_activations %>% 
  select(date,Deactivation_Date) %>% 
  View()

mrip_with_activations <- mrip_with_activations %>% 
  mutate(deactive_remove = if_else(date - Deactivation_Date > 0,
                                   1,0))

possible_reporters_17 <- mrip_with_activations %>% 
           filter(!is.na(CLS)) %>% 
           filter(is.na(deactive_remove) | deactive_remove == 0) %>% 
           nrow()

#prop_possible_16 <- possible_reporters_16 / nrow(mrip16)
prop_possible_17 <- possible_reporters_17 / nrow(mrip_with_activations)

#prop_possible_16
prop_possible_17


#weighted
possible_reporters_17_w <- mrip_with_activations %>% 
  filter(!is.na(CLS)) %>% 
  filter(is.na(deactive_remove) | deactive_remove == 0)
sum(possible_reporters_17_w$wp_int) / 
  sum(mrip_with_activations$wp_int)

#Broken Down by State - AL
mrip_with_activations_al <- mrip_with_activations %>% 
  filter(ST == 1)
possible_reporters_17_al_w <- mrip_with_activations_al %>% 
  filter(!is.na(CLS)) %>% 
  filter(is.na(deactive_remove) | deactive_remove == 0) 
sum(possible_reporters_17_al_w$wp_int) / 
  sum(mrip_with_activations_al$wp_int)

#FL
mrip_with_activations_fl <- mrip_with_activations %>% 
  filter(ST == 12)

possible_reporters_17_fl_w <- mrip_with_activations_fl %>% 
  filter(!is.na(CLS)) %>% 
  filter(is.na(deactive_remove) | deactive_remove == 0)

sum(possible_reporters_17_fl_w$wp_int) / 
  sum(mrip_with_activations_fl$wp_int)

#messing around
# mrip17 %>% 
#   filter(CLS_ID %in% c(6,21,22,29,30,45,53,
#                        61,76,78,82,101,102,
#                        112,113,114,128,135,
#                        136,137,141,153,160,
#                        187,198,206,212,215,
#                        240,243,244,252,253,
#                        257,263,267)) %>% 
#   pull(wp_int) %>% 
#   sum()
# 
# mrip17 %>% 
#   filter(!is.na(CLS)) %>% 
#   pull(wp_int) %>% 
#   sum()
# 
# 
# max(mrip17$PARTY)
# cls2tid <- read_sas("data/cls2tid_20170905.sas7bdat")
# cls2tid$CLS_ID <- as.numeric(cls2tid$CLS_ID)
# mrip17 %>% 
#   left_join(cls2tid) %>% 
#   filter(TID %in% c(512517,
#                              512518,
#                              512146,
#                              512494,
#                              512220,
#                              512220,
#                              512146,
#                              512494,
#                              512517,
#                              512518,
#                              512516,
#                              512068,
#                              512249,
#                              512260,
#                              512295,
#                              512298,
#                              512306,
#                              512340,
#                              512498,
#                              513709,
#                              520278,
#                              502576))
# 
# bad_tids <- cls2tid %>% 
#   filter(TID %in% c(512517,
#                     512518,
#                     512146,
#                     512494,
#                     512220,
#                     512220,
#                     512146,
#                     512494,
#                     512517,
#                     512518,
#                     512516,
#                     512068,
#                     512249,
#                     512260,
#                     512295,
#                     512298,
#                     512306,
#                     512340,
#                     512498,
#                     513709,
#                     520278,
#                     502576))
# bad_tids <- bad_tids %>% 
#   filter(!is.na(CLS_ID))
# 
# mrip17 %>% 
#   filter(CLS_ID %in% bad_tids$CLS_ID) %>% 
#   pull(PARTY)
