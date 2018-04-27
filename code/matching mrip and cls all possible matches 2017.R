library(tidyverse)
library(lubridate)

tidy_cls <- read_csv("data/cls_tidy_full.csv")
tidy_mrip <- read_csv("data/mrip_tidy_17.csv",
                      col_types=cols(psu_id = col_character()))

tid_cls <- read_csv("data/cls_2_tid_2016.csv") #the 2016 name is meaningless
tid_cls <- tid_cls %>%
  rename(TID = Serial_Number)
tidy_cls <- tidy_cls %>%
  left_join(tid_cls)
#format the dates
tidy_cls$departdate <- ymd_hms(tidy_cls$departdate)
tidy_cls$returndate <- ymd_hms(tidy_cls$returndate)
tidy_cls$report_date <- ymd_hms(tidy_cls$reportdate)

tidy_cls$return_date_ymd <- vapply(str_split(tidy_cls$returndate," "),"[","",1)

tidy_mrip$date  <- ymd(tidy_mrip$date)

################
# sum up claim and release for the species seen in mrip that are not seen in CLS.
# that way we keep the info as an extra column, but reduce the total number of columns to match on
################

#the species reported in CLS are :
cls_species <- tidy_cls %>% 
  select(ends_with("kept")) %>%
  names() %>%
  str_sub(end=-6)

#Sandbar Shark and NA are the 2 species reported in CLS that are not reported in MRIP

# Want to sum up claim and release for the species seen in mrip that are not seen in CLS.
# In mrip, the species unknown, atlantic thread herring and menhaden genus 
# have 0s for both claim and release so remove them

#non cls species, summed up for claim
mrip_non_cls_claim_tot <- tidy_mrip %>%
  select(-starts_with("UNKNOWN"),
         -starts_with("ATLANTIC THRE"), #remove the species with no info
         -starts_with("MENHADEN GENUS")
  ) %>%
  select(-(map(cls_species,    #select only the species seen in mrip alone
               starts_with,
               vars = colnames(.)) %>%
             unlist())) %>%
  select(ends_with("claim")) %>% #only keep variables having to do with claim
  mutate(mrip_non_cls_claim = rowSums(.,na.rm = T)) %>%
  select(mrip_non_cls_claim)

#non cls species, summed up for release
mrip_non_cls_release_tot <- tidy_mrip %>%
  select(-starts_with("UNKNOWN"),
         -starts_with("ATLANTIC THRE"), #remove the species with no info
         -starts_with("MENHADEN GENUS")
  ) %>%
  select(-(map(cls_species,    #select only the species seen in mrip alone
               starts_with,
               vars = colnames(.)) %>%
             unlist())) %>%
  select(ends_with("release")) %>% #only keep variables having to do with release
  mutate(mrip_non_cls_release = rowSums(.,na.rm = T)) %>%
  select(mrip_non_cls_release) 

#add on non cls species claim and release to tidy_mrip, remove the individual species
tidyer_mrip <- tidy_mrip %>%
  select(-starts_with("UNKNOWN"),
         -starts_with("ATLANTIC THRE"), #remove the species with no info
         -starts_with("MENHADEN GENUS")
  ) %>%
  select(1:16,(map(cls_species,    #select only the species seen in mrip alone
                   starts_with,
                   vars = colnames(.)) %>%
                 unlist())) %>% 
  bind_cols(mrip_non_cls_claim_tot) %>%
  bind_cols(mrip_non_cls_release_tot)

tidy_cls <- tidy_cls %>%
  mutate(
    wave = case_when(
      month(return_date_ymd) %in% c(1,2) ~ 1,
      month(return_date_ymd) %in% c(3,4) ~ 2,
      month(return_date_ymd) %in% c(5,6) ~ 3,
      month(return_date_ymd) %in% c(7,8) ~ 4,
      month(return_date_ymd) %in% c(9,10) ~ 5,
      month(return_date_ymd) %in% c(11,12) ~ 6
    )
  ) %>% 
  mutate(wave_year = paste0(wave,"_",year(return_date_ymd))
  )
#make wave_year variable for MRIP
tidyer_mrip <- tidyer_mrip %>%
  mutate(wave_year = paste0(WAVE,"_",year(date))
  ) 
write_csv(tidyer_mrip,"data/mrip_tidy2_17.csv") #updte tidy_mrip_17

#I want a dataset of all possible matches

#here is a full join of the datasets
#includes cls only and mrip only values
fully_everything <- tidy_cls %>%
  drop_na(CLS_ID) %>%
  inner_join(tidyer_mrip,by = c("CLS_ID","wave_year"))

all_possible_matches <- fully_everything #%>%
  #drop_na(date) %>%
  #drop_na(return_date_ymd)

write_csv(all_possible_matches,"data/2017_all_possible_matches_mrip_cls.csv")

